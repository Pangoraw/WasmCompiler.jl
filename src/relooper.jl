import Core.Compiler: DomTree, dominates, IRCode, CFG, BasicBlock

"""
    Relooper(exprs, ir)

This struct contains the state needed to transform the `goto` style control flow
of Julia SSAIR to WebAssembly valid control flow using `Loop`, `Block` and `If`
constructs.

The implementation is based on the paper:

> Beyond Relooper, Recursive Generation of WebAssembly \\
> ICFP 2022, Norman Ramsey

TODOs:
   - [x] test and support ireducible control flow using node-splitting.
   - [ ] exception handling with `:enter` and `:leave`.

The core API to use the relooper is to call `reloop(exprs, ir)` which will return
a re-looped single list of instructions.
"""
struct Relooper
    # We already have emitted the code for each block content
    exprs::Vector{Vector{Inst}}
    ir::IRCode
    cfg::CFG
    domtree::DomTree
    order::Vector{Int}
    context::Vector{Int}
end

_copy(cfg) = CFG(copy(cfg.blocks), copy(cfg.index))

function Relooper(exprs, ir::IRCode)
    exprs = copy(exprs)
    exprs, cfg, domtree = reduce!(exprs, _copy(ir.cfg))
    Relooper(
        exprs,
        ir,
        cfg,
        domtree,
        reverse_postorder(domtree.dfs_tree),
        Int[],
    )
end

function reverse_postorder(dfs)
    postorder = dfs.to_post # bb -> order
    rev_postorder = similar(postorder)
    # post_order -> bb
    for (i, b) in enumerate(sortperm(postorder; rev=true))
        rev_postorder[b] = i
    end
    rev_postorder
end

function brindex(relooper::Relooper, l)
    idx = findlast(==(l), relooper.context)
    isnothing(idx) && error("failed to find block $l in context $(relooper.context)")
    length(relooper.context) - idx
end

# N.B. A block is a merge node if it is where control flow merges.
# That means it is entered by multiple control-flow edges, _except_
# back edges don't count.  There must be multiple paths that enter the
# block _without_ passing through the block itself.
# Merge nodes are positioned after other dominated blocks which are not
# merge nodes such that each ancestor in the dominator tree is able to
# jump to a merge node.
#
# Consider the following CFG:
#
#   A
#  ↙ ↘
# B   C
#  ↘ ↙
#   D
#
# Here D is a merge node, immediately dominated by A. It will result in the
# following wasm control flow.
#
# block
#   A
#   if
#     B
#   else
#     C
#   end
#   D
# end
function ismergenode(relooper::Relooper, bidx)
    block = relooper.cfg.blocks[bidx]
    count(b -> relooper.order[b] < relooper.order[bidx], block.preds) >= 2
end

# N.B. A block is a loop header if any edge flows backward to it.
# Self-loop also count. A loop header if represented using the `Loop`
# Wasm construct.
#
# In the following control flow graph, B is a loop header since the
# reverse post-order is the following [A:1, B:2, C:3] and therefore
# the edge from C to B is backward (order[C] >= order[B]).
#
# A
# ↓
# B←┐
# ↓ |
# C-┘
#
function isloopheader(relooper::Relooper, bidx)
    block = relooper.cfg.blocks[bidx]
    any(b -> relooper.order[b] >= relooper.order[bidx], block.preds)
end

function dobranch(relooper::Relooper, source, target)
    # Jumping backward means that the block is dominated by the loop header
    # therefore the target is present in the context and the jump can be
    # represented with a `br` instruction.
    if !(relooper.order[target] > relooper.order[source]) # isbackward
        i = brindex(relooper, target)
        return br(i)
    end

    # The target is a merge node, it means that it is positioned after
    # the source block and is present in the context to jump to with a
    # `br` instruction.
    if ismergenode(relooper, target) # ismergelabel
        i = brindex(relooper, target)
        return br(i)
    end

    # Otherwise, we can simply inline the code for the target block in-place.
    @debug "succ" source target
    donode!(relooper, target)
end

isreturn(relooper, bidx) = relooper.ir.stmts[relooper.cfg.blocks[bidx].stmts.stop][:inst] isa Core.ReturnNode
isunreachable(relooper, bidx) = isreturn(relooper, bidx) &&
                                !isdefined(relooper.ir.stmts[relooper.cfg.blocks[bidx].stmts.stop][:inst], :val)

function getsuccs(relooper, bidx)
    ir = relooper.ir
    block = relooper.cfg.blocks[bidx]
    @assert length(block.succs) <= 2

    gotoifnot = ir.stmts[block.stmts.stop][:inst]
    if length(block.succs) == 1
        return only(block.succs), nothing
    end

    if gotoifnot isa Core.GotoNode
        gotoifnot = ir.stmts[block.stmts.stop - 1][:inst]::Core.GotoIfNot
    end

    falsedest = gotoifnot.dest

    truedest = block.succs[1] == falsedest ?
        last(block.succs) : first(block.succs)

    if !(truedest in relooper.cfg.blocks[bidx].succs)
        return Tuple(relooper.cfg.blocks[bidx].succs...)
    end

    return truedest, falsedest
end

function nestwithin!(relooper::Relooper, bidx, mergenodes)
    if isempty(mergenodes)
        if isreturn(relooper, bidx)
            if isunreachable(relooper, bidx)
                push!(relooper.exprs[bidx], unreachable())
            else
                push!(relooper.exprs[bidx], return_())
            end
            return relooper.exprs[bidx]
        end

        truedest, falsedest = getsuccs(relooper,bidx)

        if isnothing(falsedest)
            push!(relooper.exprs[bidx], dobranch(relooper, bidx, truedest))
        else
            push!(relooper.context, -1)
            push!(relooper.exprs[bidx],
                If(
                    FuncType([], []),
                    Inst[dobranch(relooper, bidx, truedest)],
                    Inst[dobranch(relooper, bidx, falsedest)],
                ),
            )
            @assert -1 == pop!(relooper.context)
        end
        return relooper.exprs[bidx]
    end

    # y_n has a higher reverse postorder indexing
    # which means that it should be placed *after* other
    # mergenodes. We therefore push it in the context before
    # other mergenodes.
    (ys..., y_n) = mergenodes

    push!(relooper.context, y_n)
    codeforx = Block(FuncType([], []), nestwithin!(relooper, bidx, ys))
    @assert y_n == pop!(relooper.context)
    Inst[
        codeforx,
        donode!(relooper, y_n),
    ]
end

function donode!(relooper::Relooper, bidx)
    (; ir, domtree) = relooper

    # When placing a block, we also place all successors in the
    # dominator tree.
    toplace = sort(domtree.nodes[bidx].children, by=b -> relooper.order[b])

    # Out of the immediately dominated blocks, merge nodes have a
    # special handling since they are placed after.
    mergenodes = filter(b -> ismergenode(relooper, b), toplace)

    # Very verbose
    # @debug "placing" bidx toplace mergenodes

    if isloopheader(relooper, bidx)
        block = ir.cfg.blocks[bidx]

        # Each loop must have a single entry point, otherwise the CFG
        # is not reducible. So all edges must either be forward or the
        # current block must dominate the predecessor for a backward edge.
        @assert all(b -> relooper.order[b] < relooper.order[bidx] ||
                         dominates(relooper.domtree, bidx, b),
                    block.preds) "CFG is not reducible"

        push!(relooper.context, bidx)
        codeforx = nestwithin!(relooper, bidx, mergenodes)
        @assert bidx == pop!(relooper.context)
        Loop(FuncType([], []), codeforx)
    else
        push!(relooper.context, -1)
        codeforx = nestwithin!(relooper, bidx, mergenodes)
        @assert -1 == pop!(relooper.context)
        Block(FuncType([], []), codeforx)
    end
end

reloop!(relooper::Relooper) = donode!(relooper, 1)

"""
    reloop(exprs::Vector{Vector{Inst}}, ir::IRCode)::Vector{Inst}

Consumes the given expressions (one for each block in `ir.cfg.blocks`) and return_
a single expression list which contains the WebAssembly structured control flow constructs
such as `Block`, `If` and `Loop` and which uses `br` instructions instead of arbitrary goto jumps.
"""
function reloop(exprs, ir)
    relooper = Relooper(exprs, ir)
    Inst[
        reloop!(relooper),
        unreachable(), # CF will go through a ReturnNode
    ]
end

# --- Irreducible CFG ---

const BBNumber = Int

struct SuperNode
    head::BBNumber
    nodes::Vector{BBNumber}
end

"""
    SuperGraph(cfg)

State needed to perform the node splitting algorithm from

> Compilers - Principles, Techniques, and Tools \\
> Aho et al., sec 9.7.5.

with the implementation described in

> Beyond Relooper, Recursive Generation of WebAssembly \\
> ICFP 2022, Norman Ramsey
"""
mutable struct SuperGraph
    exprs::Vector{Vector{Inst}}
    domtree::DomTree
    const cfg::CFG
    const nodes::Vector{SuperNode}
end
function SuperGraph(exprs, cfg::CFG)
    n_blocks = length(cfg.blocks)
    domtree = Core.Compiler.construct_domtree(cfg.blocks)
    SuperGraph(exprs, domtree, cfg, [
        SuperNode(b, BBNumber[b])
        for b in 1:n_blocks
    ])
end

"""
    reduce!(exprs, cfg::CFG)

Transforms an irreducible in a reducible CFG by performing
node-splitting. See [`SuperGraph`](@ref).
"""
function reduce!(exprs, cfg)
    sg = SuperGraph(exprs, cfg)
    while length(sg.nodes) != 1
        while merge!(sg) end
        split!(sg)
    end
    verifycfg(cfg)
    return sg.exprs, sg.cfg, sg.domtree
end

"control-flow predecessors of the super-node U in the super-graph sg"
predecessors(sg, U) =
    setdiff(sg.cfg.blocks[U.head].preds, U.nodes)

function merge!(sg)
    for u in eachindex(sg.nodes)
        for v in eachindex(sg.nodes)
            u == v && continue

            U = sg.nodes[u]
            V = sg.nodes[v]

            dominates(sg.domtree, V.head, U.head) || continue

            preds = predecessors(sg, U)
            if !isempty(preds) && preds ⊆ V.nodes
                deleteat!(sg.nodes, u)
                union!(V.nodes, U.nodes)
                return true
            end
        end
    end
    return false
end

function split!(sg)
    for i in eachindex(sg.nodes)
        X = sg.nodes[i]

        preds = predecessors(sg, X)
        Ws = filter(W -> !isdisjoint(W.nodes, preds), sg.nodes)
        isempty(Ws) && continue

        # Create Xᵢ foreach Wᵢ ∈ Ws
        for Wᵢ in @view Ws[begin+1:end]
            n_blocks = length(sg.cfg.blocks)

            map_block(x) = x ∈ X.nodes ? findfirst(==(x), X.nodes) + n_blocks : x
            Xᵢ = SuperNode(
                map_block(X.head),
                map(map_block, X.nodes),
            )

            get_block = Base.Fix1(getindex, sg.cfg.blocks)
            new_blocks = [
                BasicBlock(
                    b.stmts,
                    map(map_block, b.preds) ∩ (Xᵢ.nodes ∪ Wᵢ.nodes),
                    map(map_block, b.succs) ∩ (Xᵢ.nodes ∪ Wᵢ.nodes),
                )
                for b in map(get_block, X.nodes)
            ]
            new_exprs = Vector{Inst}[
                copy(sg.exprs[i])
                for b in map(get_block, X.nodes)
            ]

            for W in Wᵢ.nodes
                W_block = sg.cfg.blocks[W]
                union!(W_block.preds, map(map_block, W_block.preds))
                map!(map_block, W_block.succs, W_block.succs)
            end

            for x in X.nodes
                x_block = sg.cfg.blocks[x]
                setdiff!(x_block.preds, Wᵢ.nodes)
            end

            append!(sg.exprs, new_exprs)
            append!(sg.cfg.blocks, new_blocks)
            append!(sg.cfg.index, map(i -> sg.cfg.index[i], X.nodes))
            push!(sg.nodes, Xᵢ)
        end

        # NOTE: do not rebuild the CFG everytime
        sg.domtree = Core.Compiler.construct_domtree(sg.cfg.blocks)
        return true
    end
    return false
end

# Verify that we have not broken the CFG
function verifycfg(cfg)
   for (i, b) in enumerate(cfg.blocks)
       for p in b.preds
           @assert i ∈ cfg.blocks[p].succs p => i
       end
       for s in b.succs
           @assert i ∈ cfg.blocks[s].preds i => s
       end
   end
end
