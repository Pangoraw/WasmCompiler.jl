"""
    make_tees!(func::Func)::Func

Merge subsequent `local.set` and `local.get` instructions to the
same local in a single `local.tee` instruction.
"""
make_tees!(func) = (_make_tees!(func.inst); func)

function _make_tees!(instlist)
    i = firstindex(instlist)
    while i <= lastindex(instlist)
        inst = instlist[i]
        i += 1

        if inst isa If
            _make_tees!(inst.trueinst)
            _make_tees!(inst.falseinst)
            continue
        elseif inst isa Try
            _make_tees!(inst.inst)
            foreach(c -> _make_tees!(c.inst),
                    inst.catches)
            continue
        elseif inst isa Union{Block,Loop,TryDelegate}
            _make_tees!(inst.inst)
            continue
        end

        inst isa local_set || continue

        i > lastindex(instlist) && continue

        nextinst = instlist[i]
        nextinst isa local_get || continue

        inst.n == nextinst.n || continue

        instlist[i - 1] = local_tee(inst.n)
        instlist[i] = nop()

        i += 1
    end

    instlist
end

function remove_return!(f)
    isempty(f.inst) && return f
    if length(f.inst) >= 2 &&
        f.inst[end] isa unreachable &&
        f.inst[end-1] isa return_
        pop!(f.inst); pop!(f.inst)
    elseif f.inst[end] isa return_
        pop!(f.inst)
    end
    f
end

"""
    remove_unused!(func::Func)::Func

Remove locals who have no corresponding `local.get`.
"""
function remove_unused!(func)
    uses = zeros(UInt32, length(func.locals))
    nargs = length(func.fntype.params)

    foreach(func) do inst
        inst isa local_get || return
        uses[inst.n] += 1
    end

    unused = setdiff(findall(iszero, uses), 1:nargs) # we don't want to remove params
    newindices = map(n -> n - count(<(n), unused), 1:length(func.locals))
    deleteat!(func.locals, unused)

    map!(func) do inst
        if inst isa local_set || inst isa local_tee || inst isa local_get
            inst.n in unused && return inst isa local_set ? drop() : nop()
            newn = newindices[inst.n]
            return typeof(inst)(newn)
        end
        inst
    end

    func
end

"""
    remove_nops!(func::Func)::Func

Remove `nop` instructions from the function body.
"""
function remove_nops!(func)
    filter!(inst -> !(inst isa nop), func)
end

"""
    merge_blocks!(func::Func)::Func

Remove blocks wich are not targeted by any branch instructions.
"""
function merge_blocks!(func)
    _explore_blocks!(func.inst, Int[])
    func
end

const Branch = Union{br,br_if}

function _renumber_brs!(inst, deleted)
    if inst isa Branch
        inst.label > deleted && return typeof(inst)(inst.label - 1)
        inst.label == deleted && error("invalid branch")
        return inst
    elseif inst isa Union{Block,Loop}
        map!(inst -> _renumber_brs!(inst, deleted + 1), inst.inst, inst.inst)
    elseif inst isa If
        map!(inst -> _renumber_brs!(inst, deleted + 1), inst.trueinst, inst.trueinst)
        map!(inst -> _renumber_brs!(inst, deleted + 1), inst.falseinst, inst.falseinst)
    end
    return inst
end

function _explore_blocks!(expr, stack)
    i = firstindex(expr)

    while i <= length(expr)
        inst = expr[i]

        if inst isa Branch
            stack[end-inst.label] += 1
        elseif inst isa Loop 
            push!(stack, 0)
            _explore_blocks!(inst.inst, stack)
            pop!(stack)
        elseif inst isa If
            push!(stack, 0)
            _explore_blocks!(inst.falseinst, stack)
            _explore_blocks!(inst.trueinst, stack)
            pop!(stack)
        elseif inst isa Block
            push!(stack, 0)
            _explore_blocks!(inst.inst, stack)
            count = pop!(stack)

            if count == 0 && inst.fntype == voidtype
                deleteat!(expr, i)
                for newinst in inst.inst
                    insert!(expr, i, _renumber_brs!(newinst, 0))
                    i += 1
                end
            end
        end

        i += 1
    end

    expr
end

"""
    remove_useless_branches!(func::Func)::Func

Remove useless `br` instructions at the end of a block.

Example:

```wat
(block
  (block
    (br 1)))
```

to:

```wat
(block
  (block))
```
"""
function remove_useless_branches!(func)
    _remove_useless_branches!(func.inst)
    func
end

function _remove_useless_branches!(expr)
    # TODO: make it generalize to any stack level, not just the first
    isempty(expr) && return expr

    inst = last(expr)
    if inst isa If &&
        !isempty(inst.trueinst) &&
        !isempty(inst.falseinst) &&
        last(inst.trueinst) == br(1) &&
        last(inst.falseinst) == br(1)

        pop!(inst.trueinst)
        pop!(inst.falseinst)
    end

    level = 1
    while inst isa Block && !isempty(inst.inst)
        if last(inst.inst) == br(level)
            pop!(inst.inst)
        elseif last(inst.inst) isa Block
            level += 1
            inst = last(inst.inst)
        else
            break
        end
    end

    for (i, inst) in enumerate(expr)
        if inst == br(0)
            deleteat!(expr, i:lastindex(expr))
            break
        end

        if inst == unreachable()
            deleteat!(expr, i+1:lastindex(expr))
            break
        end

        if inst isa Block
            _remove_useless_branches!(inst.inst)
        elseif inst isa Loop
            for newinst in inst.inst
                if newinst isa If
                    _remove_useless_branches!(newinst.trueinst)
                    _remove_useless_branches!(newinst.falseinst)
                elseif newinst isa Block
                    _remove_useless_branches!(newinst.inst)
                end
            end
        elseif inst isa Try
            for newinst in inst.inst
                if newinst isa If
                    _remove_useless_branches!(newinst.trueinst)
                    _remove_useless_branches!(newinst.falseinst)
                elseif newinst isa Block
                    _remove_useless_branches!(newinst.inst)
                end
            end
            for c in inst.catches
                for newinst in c.inst
                    if newinst isa If
                        _remove_useless_branches!(newinst.trueinst)
                        _remove_useless_branches!(newinst.falseinst)
                    elseif newinst isa Block
                        _remove_useless_branches!(newinst.inst)
                    end
                end
            end
        elseif inst isa If
            _remove_useless_branches!(inst.trueinst)
            _remove_useless_branches!(inst.falseinst)
        end
    end

    expr
end

"""
    leak_ifs!(func::Func)::Func

Replace to branches of ifs assigning to the same variable to
a an assignment from the value of the if.

## Example

Tranforms this construct

```wat
(if (local.get \$cond)
    (then (local.set \$x (i32.const 1)))
    (else (local.set \$x (i32.const 2))))
```

into

```wat
(local.set \$x
    (if (local.get \$cond)
        (then (i32.const 1))
        (else (i32.const 2))))
```
"""
function leak_ifs!(func)
    _leak_ifs!(func.inst, func.locals)
    func
end

function _leak_ifs!(expr, locals)
    i = firstindex(expr) - 1
    while i < lastindex(expr)
        i += 1
        inst = expr[i]

        if inst isa Union{Loop,Block,TryDelegate}
            _leak_ifs!(inst.inst, locals)
        elseif inst isa Try
            _leak_ifs!(inst.inst, locals)
            foreach(c -> _leak_ifs!(c.inst, locals),
                    inst.catches)
        elseif inst isa If
            _leak_ifs!(inst.trueinst, locals)
            _leak_ifs!(inst.falseinst, locals)
        end

        inst isa If || continue

        isempty(inst.fntype.results) || continue
        isempty(inst.trueinst) && continue
        isempty(inst.falseinst) && continue

        if last(inst.trueinst) isa local_set &&
            last(inst.falseinst) isa local_set &&
            last(inst.trueinst).n == last(inst.falseinst).n

            (; n) = pop!(inst.trueinst)
            pop!(inst.falseinst)
            push!(inst.fntype.results, locals[n])

            i += 1

            insert!(
                expr,
                i,
                local_set(n),
            )
        end
    end
    expr
end

_type_score(::WasmInt32) = 1
_type_score(::WasmInt64) = 2
_type_score(::WasmFloat32) = 3
_type_score(::WasmFloat64) = 4
_type_score(::WasmVector128) = 5
_type_score(r::StructRef) = 100 + 10r.typeidx + r.null
_type_score(r::ArrayRef) = 200 + 10r.typeidx + r.null
_type_score(::StringRef) = 300

function sort_locals!(func)
    (; locals) = func
    nparams = length(func.fntype.params)
    perm = nparams .+ sortperm(@view locals[begin+nparams:end]; by=_type_score)
    prepend!(perm, 1:nparams)
    permute!(locals, perm)
    rev = invperm(perm)
    map!(func) do inst
        inst isa Union{local_set,local_get,local_tee} || return inst
        return typeof(inst)(rev[inst.n])
    end
    func
end

"""
    collapse_branches!(f::Func)

Collapse if/else constructs when one branch only has a `br` instruction
to a block with a `br_if`.

```
(if (cond)
    (then (...))
    (else (br 3)))
```
to 
```
(br_if 2 (i32.eqz (cond)))
(block
    (...))
```
"""
collapse_branches!(f::Func) = (_collapse_branches!(f.inst); f)

function _collapse_branches!(expr)
    for (i, inst) in enumerate(expr)
        if inst isa Block || inst isa Loop || inst isa TryDelegate
            _collapse_branches!(inst.inst)
            continue
        elseif inst isa Try
            _collapse_branches!(inst.inst)
            foreach(c -> _collapse_branches!(c.inst),
                    inst.catches)
            continue
        end

        inst isa If || continue

        _collapse_branches!(inst.trueinst)
        _collapse_branches!(inst.falseinst)

        length(inst.falseinst) == 1 || continue
        branch = only(inst.falseinst)
        branch isa br || continue

        if iszero(branch.label)
            empty!(inst.falseinst)
            continue
        end

        expr[i] = Block(inst.fntype, inst.trueinst)
        insert!(expr, i, br_if(branch.label - 1))
        insert!(expr, i, i32_eqz()) # negate condition
    end
    expr
end

function unused_functions(wmod)
    num_imports = count(imp -> imp isa FuncImport, wmod.imports)
    edges = Pair{Int,Int}[]
    for (i, f) in enumerate(wmod.funcs)
        foreach(f.inst) do inst
            if inst isa WC.call
                push!(edges, num_imports + i => inst.func)
            end
        end
    end

    entries = filter(exp -> exp isa FuncExport, wmod.exports)
    called = BitSet()

    function dfs(fi)
        fi ∈ called && return
        push!(called, fi)

        succs = [s for (p, s) in edges if p == fi]
        foreach(dfs, succs)
    end

    for ent in entries
        dfs(ent.func)
    end
    push!(called, wmod.start)

    called
end
