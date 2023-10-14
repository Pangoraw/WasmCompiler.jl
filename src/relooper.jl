import Core.Compiler: DomTree, dominates, IRCode

"""
    Relooper(exprs, ir)

This struct contains the state needed to transform the `goto` style control flow
of Julia SSAIR to WebAssembly valid control flow using `Loop`, `Block` and `If`
constructs.

The implementation is based on the paper:

> Beyond Relooper, Recursive Generation of WebAssembly \\
> ICFP 2022, Norman Ramsey

TODOs:
   - [ ] test and support ireducible control flow.
   - [ ] exception handling with `:enter` and `:leave`.

"""
struct Relooper
    # We already have emitted the code for each block content
    exprs::Vector{Vector{Inst}}
    ir::IRCode
    domtree::DomTree
    order::Vector{Int}
    context::Vector{Int}
end

function Relooper(exprs, ir::IRCode)
    domtree = Core.Compiler.construct_domtree(ir.cfg.blocks)
    Relooper(
        exprs,
        ir,
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
    block = relooper.ir.cfg.blocks[bidx]
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
    block = relooper.ir.cfg.blocks[bidx]
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

isreturn(relooper, bidx) = relooper.ir.stmts[relooper.ir.cfg.blocks[bidx].stmts.stop][:inst] isa Core.ReturnNode
isunreachable(relooper, bidx) = isreturn(relooper, bidx) && !isdefined(relooper.ir.stmts[relooper.ir.cfg.blocks[bidx].stmts.stop][:inst], :val)

function getsuccs(relooper, bidx)
    ir = relooper.ir
    block = ir.cfg.blocks[bidx]
    @assert length(block.succs) <= 2

    gotoifnot = ir.stmts[block.stmts.stop][:inst]
    if length(block.succs) == 1
        return gotoifnot isa Core.GotoNode ?
            gotoifnot.label : bidx + 1, nothing
    end

    if gotoifnot isa Core.GotoNode
        gotoifnot = ir.stmts[block.stmts.stop - 1][:inst]::Core.GotoIfNot
    end

    falsedest = gotoifnot.dest

    truedest = block.succs[1] == falsedest ?
        last(block.succs) : first(block.succs)

    truedest, falsedest
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
struct SuperGraph
    domtree::DomTree
    cfg::Core.Compiler.CFG
    nodes::Vector{SuperNode}
end
function SuperGraph(cfg::Core.Compiler.CFG)
    n_blocks = length(cfg.blocks)
    domtree = Core.Compiler.construct_domtree(cfg.blocks)
    SuperGraph(domtree, cfg, [
        SuperNode(b, BBNumber[b])
        for b in 1:n_blocks
    ])
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
        # for Wᵢ in @view Ws[begin+1:end]
        #     n_blocks = length(sg.cfg.blocks)
        #     new_nodes = n_blocks+1:n_blocks+1+length(X.nodes)
        #     for (x, x′) in zip(X.nodes, new_nodes)
        #         b = sg.cfg.blocks[x]
        #         new_b = Core.Compiler.BasicBlock(
        #             b.stmts,
        #             map(p -> new_nodes[p], b.preds),
        #             map(p -> new_nodes[p], b.succs),
        #         )
        #     end
        #     Xᵢ = SuperNode()
        # end
    end
end
