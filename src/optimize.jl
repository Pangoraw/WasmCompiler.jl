function make_tees!(func)

    i = firstindex(func.inst)
    while i < lastindex(func.inst)
        inst = func.inst[i]
        i += 1

        inst isa local_set || continue

        nextinst = func.inst[i]
        nextinst isa local_get || continue

        inst.n == nextinst.n || continue

        deleteat!(func.inst, i)
        func.inst[i - 1] = local_tee(inst.n)
    end

    func
end

function remove_unused!(func)
    uses = zeros(UInt32, length(func.locals))
    nargs = length(func.fntype.params)

    foreach(func) do inst
        inst isa local_get || return
        uses[inst.n + 1] += 1
    end

    unused = setdiff(findall(==(0), uses), 1:nargs)
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
