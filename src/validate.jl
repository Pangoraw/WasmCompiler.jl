struct FnValidator
    mod::WModule
    func::Func

    fntype::FuncType
    stack::Vector{ValType}
    block_types::Vector{FuncType}
end

struct ValidationError <: Exception
    msg::String
end

showerror(io::IO, ve::ValidationError) = print(io, ve.msg)

function validate_fn(mod::WModule, func::Func)
    val = FnValidator(mod, func, func.fntype, ValType[], FuncType[])

    for inst in func.inst
        validate_inst(val, inst)
    end

    @show val.stack
end

function validate_inst(val, inst)
    if inst isa unreachable
        if !isempty(val.stack)
            @show val.stack
            throw(ValidationError("unreachable with values on the stack"))
        end
        return
    end

    if inst isa select
        if length(val.stack) < 3
            throw(ValidationError("not enough values for select"))
        end

        cond = pop!(val.stack)
        if cond !== i32
            throw(ValidationError("condition for select is of type $cond"))
        end

        t1, t2 = pop!(val.stack), pop!(val.stack)
        if t1 != t2
            throw(ValidationError("invalid select with two types $t1 and $t2"))
        end

        push!(val.stack, t1)

        return
    end

    fntype = inst_func_type(val, inst)

    WC._printwasm(stdout,inst)
    print(" ")
    WC._printwasm(stdout, fntype)
    println(" ", val.stack)

    if length(val.stack) < length(fntype.params)
        throw(ValidationError("invalid inst $inst"))
    end

    stack_values = reverse(ValType[pop!(val.stack) for _ in fntype.params])

    if stack_values != fntype.params
        throw(ValidationError("expected $(fntype.params) but got $stack_values on the stack for $inst"))
    end

    if inst isa If
        push!(
            val.block_types,
            inst.fntype,
        )

        append!(val.stack, stack_values[begin:end-1])

        for cinst in inst.trueinst
            validate_inst(val, cinst)
        end

        if last(val.stack, length(inst.fntype.results)) != inst.fntype.results
            throw(ValidationError("invalid if true branch"))
        end
        for _ in 1:length(inst.fntype.results) pop!(val.stack) end

        append!(val.stack, stack_values[begin:end-1])

        for cinst in inst.falseinst
            validate_inst(val, cinst)
        end

        if last(val.stack, length(inst.fntype.results)) != inst.fntype.results
            throw(ValidationError("invalid if false branch"))
        end
        for _ in 1:length(inst.fntype.results) pop!(val.stack) end

        pop!(val.block_types)
    end

    if inst isa Block || inst isa Loop
        push!(
            val.block_types,
            inst.fntype,
        )
        append!(val.stack, stack_values)

        for cinst in inst.inst
            validate_inst(val, cinst)
        end

        pop!(val.block_types)
    end

    append!(val.stack, fntype.results)
end

inst_func_type(_, ::i32_const) = FuncType([], [i32])
inst_func_type(_, ::i64_const) = FuncType([], [i64])
inst_func_type(_, ::f32_const) = FuncType([], [f32])
inst_func_type(_, ::f64_const) = FuncType([], [f64])

inst_func_type(_, ::i32_trunc_f32_u) = FuncType([f32], [i32])
inst_func_type(_, ::i32_trunc_f32_s) = FuncType([f32], [i32])

inst_func_type(_, ::nop) = FuncType([], [])

inst_func_type(val, lg::local_get) = FuncType([], [val.func.locals[lg.n]])
inst_func_type(val, lt::local_tee) = FuncType([val.func.locals[lt.n]], [val.func.locals[lt.n]])
inst_func_type(val, ls::local_set) = FuncType([val.func.locals[ls.n]], [])
inst_func_type(val, ::return_) = FuncType(copy(val.func.fntype.results), [])

inst_func_type(val, b::br) = val.block_types[end-b.label]
function inst_func_type(val, b::br_if)
    bt = val.block_types[end-b.label]
    FuncType([bt.params..., i32], copy(bt.results))
end

inst_func_type(_, b::Block) = copy(b.fntype)
inst_func_type(_, b::Loop) = copy(b.fntype)
inst_func_type(_, b::If) = FuncType([b.fntype.params..., i32], copy(b.fntype.results))
