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
    ir::Core.Compiler.IRCode
    idoms::Vector{Int}
    order::Vector{Int}
    context::Vector{Int}
end

function Relooper(exprs, ir::Core.Compiler.IRCode)
    domtree = Core.Compiler.construct_domtree(ir.cfg.blocks)
    Relooper(
        exprs,
        ir,
        domtree.idoms_bb,
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

function brindex(relooper::Relooper, l, i=0)
    i == length(relooper.context) && error("failed to find block $l in context $(relooper.context)")
    relooper.context[end-i] == l ?
        i : brindex(relooper, l, i+1)
end

# N.B. A block is a merge node if it is where control flow merges.
# That means it is entered by multiple control-flow edges, _except_
# back edges don't count.  There must be multiple paths that enter the
# block _without_ passing through the block itself.
function ismergenode(relooper::Relooper, bidx)
    block = relooper.ir.cfg.blocks[bidx]
    count(b -> relooper.order[b] < relooper.order[bidx], block.preds) >= 2
end

function isloopheader(relooper::Relooper, bidx)
    block = relooper.ir.cfg.blocks[bidx]
    any(b -> relooper.order[b] >= relooper.order[bidx], block.preds)
end

function dobranch(relooper::Relooper, source, target)
    if !(relooper.order[target] > relooper.order[source]) # isbackward
        i = brindex(relooper, target)
        br(i)
    elseif ismergenode(relooper, target) # ismergelabel
        i = brindex(relooper, target)
        br(i)
    else
        @debug "succ" source target
        donode!(relooper, target)
    end
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
    (; ir, idoms) = relooper
    (; cfg) = ir
    (; stmts, preds, succs) = cfg.blocks[bidx]

    toplace = sort(findall(==(bidx), idoms),
                   by=b -> relooper.order[b])
    mnodes = filter(b -> ismergenode(relooper, b), toplace)
  
    # Very verbose
    # @debug "placing" bidx toplace mnodes

    if isloopheader(relooper, bidx)
        push!(relooper.context, bidx)
        codeforx = nestwithin!(relooper, bidx, mnodes)
        @assert bidx == pop!(relooper.context)
        Loop(FuncType([], []), codeforx)
    else
        push!(relooper.context, -1)
        codeforx = nestwithin!(relooper, bidx, mnodes)
        @assert -1 == pop!(relooper.context)
        Block(FuncType([], []), codeforx)
    end
end

reloop!(relooper::Relooper) = donode!(relooper, 1)
