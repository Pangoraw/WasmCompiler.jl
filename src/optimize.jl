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
        elseif inst isa Union{Block,Loop}
            _make_tees!(inst.inst)
            continue
        end

        inst isa local_set || continue

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
    isempty(f.inst) && return
    if f.inst[end] isa unreachable &&
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
    i = firstindex(expr)
    while i <= lastindex(expr)
        inst = expr[i]

        inst isa If || continue
        isempty(inst.fntype.results) && continue
        isempty(inst.trueinst) && continue
        isempty(inst.falseinst) && continue

        if last(inst.trueinst) isa local_set &&
            last(inst.falseinst) isa local_set &&
            last(inst.trueinst).n == last(inst.falseinst).n

            (; n) = pop!(inst.trueinst)
            pop!(inst.falseinst)
            push!(inst.fntype.results, locals[n])

            insert!(
                expr,
                i,
                local_set(n),
            )

            i += 1
        end

        i += 1
    end
    expr
end
