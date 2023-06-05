make_tees!(func) = (_make_tees!(func.inst); func)

function _make_tees!(instlist)

    i = firstindex(instlist)
    while i < lastindex(instlist)
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

        deleteat!(instlist, i)
        instlist[i - 1] = local_tee(inst.n)
    end

    instlist
end

function remove_unused!(func)
    uses = zeros(UInt32, length(func.locals))
    nargs = length(func.fntype.params)

    foreach(func) do inst
        inst isa local_get || return
        uses[inst.n + 1] += 1
    end

    unused = setdiff(findall(iszero, uses), 1:nargs)
    deleteat!(func.locals, unused)

    map!(func) do inst
        if inst isa local_set || inst isa local_tee || inst isa local_get
            inst.n + 1 in unused && return inst isa local_set ? drop() : nop()
            return typeof(inst)(inst.n - count(<(inst.n + 1), unused))
        end
        inst
    end

    func
end

function merge_blocks!(func)
    i = firstindex(func.inst)

    while i <= length(func.inst)
        inst = func.inst[i]

        if inst isa Block && inst.fntype == voidtype
            # NOTE: this is wrong if block has br insts
            deleteat!(func.inst, i)
            for newinst in inst.inst
                insert!(func.inst, i, newinst)
                i += 1
            end
        end

        i += 1
    end

    func
end

