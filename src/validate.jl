mutable struct FnValidator
    mod::Module
    func::Func

    fntype::FuncType

    type_unreachable::Bool
    stack::Vector{ValType}
    block_types::Vector{FuncType}
end

struct ValidationError <: Exception
    msg::String
end

Base.showerror(io::IO, ve::ValidationError) = print(io, ve.msg)

function validate(mod)
    foreach(fn -> validate_fn(mod, fn), mod.funcs)
    mod
end

function validate_fn(mod::Module, func::Func)
    val = FnValidator(mod, func, func.fntype, false, ValType[], FuncType[])

    for inst in func.inst
        validate_inst(val, inst)
    end

    if !val.type_unreachable && val.stack != func.fntype.results
        name = func.name
        throw(ValidationError("$(name): type mismatch: invalid return value, expected $(func.fntype.results), got $(val.stack)"))
    end
end

function validate_inst(val, inst)
    val.type_unreachable && return

    if inst isa unreachable
        if !isempty(val.stack)
            throw(ValidationError("unreachable with values on the stack"))
        end

        val.type_unreachable = true

        return
    end

    if inst isa drop
        if isempty(val.stack)
            throw(ValidationError("type mismatch: not enough values to drop"))
        end

        pop!(val.stack)

        return
    end

    if inst isa select
        if length(val.stack) < 3
            throw(ValidationError("type mismatch: not enough values for select"))
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

    # Validate that module has a memory
    if inst isa Union{i32_load, i64_load, f32_load, f64_load,
                      i32_load8_s, i32_load8_u, i32_load16_s, i32_load16_u,
                      i64_load8_s, i64_load8_u, i64_load16_s, i64_load16_u,
                      i64_load32_s, i64_load32_u,
                      i32_store, i64_store, f32_store, f64_store,
                      i32_store8, i32_store16, i64_store8, i64_store16, i64_store32,
                      memory_copy, memory_grow}
        if isempty(val.mod.mems) && !any(imp -> imp isa MemImport, val.mod.imports)
            inst_ = sprint(WC._printwasm, inst)
            throw(ValidationError("$inst_ requires a memory"))
        end

        if !(inst isa memory_copy || inst isa memory_grow)
            if inst.memarg.offset < 0
                throw(ValidationError("invalid offset $(inst.memarg.offset)"))
            end            
        end
    end

    fntype = inst_func_type(val, inst)

    # WC._printwasm(stdout, inst)
    # print(" ")
    # WC._printwasm(stdout, fntype)
    # println(" ", val.stack)

    if length(val.stack) < length(fntype.params)
        throw(ValidationError("type mismatch: expected $(fntype.params) but got $(val.stack) on the stack for $inst"))
    end

    stack_values = reverse(ValType[pop!(val.stack) for _ in fntype.params])

    if stack_values != fntype.params
        throw(ValidationError("type mismatch: expected $(fntype.params) but got $stack_values on the stack for $inst"))
    end

    if inst isa global_set && !global_type(val.mod, inst.n).mut
        throw(ValidationError("setting value to immutable global $(inst.n)"))
    end

    if inst isa If
        prev_stack = copy(val.stack)
        empty!(val.stack)

        push!(
            val.block_types,
            inst.fntype,
        )

        append!(val.stack, stack_values[begin:end-1])

        for cinst in inst.trueinst
            validate_inst(val, cinst)
        end

        if val.type_unreachable
            empty!(val.stack)
        else
            if last(val.stack, length(inst.fntype.results)) != inst.fntype.results
                throw(ValidationError("type mismatch: invalid if true branch"))
            end
            for _ in 1:length(inst.fntype.results)
                pop!(val.stack)
            end
        end
        val.type_unreachable = false

        if !isempty(val.stack)
            throw(ValidationError("type mismatch: if"))
        end

        append!(val.stack, stack_values[begin:end-1])

        for cinst in inst.falseinst
            validate_inst(val, cinst)
        end

        if val.type_unreachable
            empty!(val.stack)
        else
            if last(val.stack, length(inst.fntype.results)) != inst.fntype.results
                throw(ValidationError("type mismatch: invalid if false branch"))
            end
            for _ in 1:length(inst.fntype.results)
                pop!(val.stack)
            end
        end
        val.type_unreachable = false

        if !isempty(val.stack)
            throw(ValidationError("type mismatch: if"))
        end

        append!(val.stack, prev_stack)

        pop!(val.block_types)
    end

    if inst isa Block || inst isa Loop
        prev_stack = copy(val.stack)
        empty!(val.stack)

        push!(
            val.block_types,
            inst.fntype,
        )
        append!(val.stack, stack_values)

        for cinst in inst.inst
            validate_inst(val, cinst)
        end

        if val.type_unreachable
            empty!(val.stack)
        else
            if last(val.stack, length(inst.fntype.results)) != inst.fntype.results
                throw(ValidationError("$(val.func.name): type mismatch: invalid"))
            end

            for _ in 1:length(inst.fntype.results)
                pop!(val.stack)
            end
        end
        val.type_unreachable = false

        if !isempty(val.stack)
            throw(ValidationError("$(val.func.name): type mismatch: block"))
        end

        append!(val.stack, prev_stack)

        pop!(val.block_types)
    end

    if inst isa return_ || inst isa br
        val.type_unreachable = true
    else
        val.type_unreachable = false
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
inst_func_type(val, gg::global_get) = FuncType([], [global_type(val.mod, gg.n).type])
inst_func_type(val, gs::global_set) = FuncType([global_type(val.mod, gs.n).type], [])
inst_func_type(val, ::return_) = FuncType(copy(val.func.fntype.results), [])

function inst_func_type(val, bt::br_table)
    bt = val.block_types[end-bt.default]
    FuncType([bt.params..., i32], [])
end

inst_func_type(val, b::br) = FuncType(copy(val.block_types[end-b.label].results), [])
function inst_func_type(val, b::br_if)
    bt = val.block_types[end-b.label]
    FuncType([bt.results..., i32], [bt.results...])
end

inst_func_type(_, ::v128_store) = FuncType([i32,v128], [])
inst_func_type(_, ::v128_load) = FuncType([i32], [v128])

inst_func_type(_, ::v128bin) = FuncType([v128, v128], [v128])
inst_func_type(_, s::v128replace_lane) = if s.lane <= Lanes.i32
    FuncType([v128,i32], [v128])
elseif s.lane == Lanes.f32
    FuncType([v128,f32], [v128])
elseif s.lane == Lanes.i64
    FuncType([v128,i64], [v128])
elseif s.lane == Lanes.f64
    FuncType([v128,f64], [v128])
end

inst_func_type(_, s::v128splat) = if s.lane <= Lanes.i32
    FuncType([i32], [v128])
elseif s.lane == Lanes.f32
    FuncType([f32], [v128])
elseif s.lane == Lanes.i64
    FuncType([i64], [v128])
elseif s.lane == Lanes.f64
    FuncType([f64], [v128])
end

inst_func_type(_, ::i32_load8_s) = FuncType([i32], [i32])
inst_func_type(_, ::i32_load8_u) = FuncType([i32], [i32])
inst_func_type(_, ::i32_load16_s) = FuncType([i32], [i32])
inst_func_type(_, ::i32_load16_u) = FuncType([i32], [i32])

inst_func_type(_, ::i64_load8_s) = FuncType([i32], [i64])
inst_func_type(_, ::i64_load8_u) = FuncType([i32], [i64])
inst_func_type(_, ::i64_load16_s) = FuncType([i32], [i64])
inst_func_type(_, ::i64_load16_u) = FuncType([i32], [i64])
inst_func_type(_, ::i64_load32_s) = FuncType([i32], [i64])
inst_func_type(_, ::i64_load32_u) = FuncType([i32], [i64])

inst_func_type(_, ::i32_store8) = FuncType([i32,i32], [])
inst_func_type(_, ::i32_store16) = FuncType([i32,i32], [])
inst_func_type(_, ::i64_store8) = FuncType([i32,i64], [])
inst_func_type(_, ::i64_store16) = FuncType([i32,i64], [])
inst_func_type(_, ::i64_store32) = FuncType([i32,i64], [])

inst_func_type(_, ::i32_extend8_s) = FuncType([i32], [i32])
inst_func_type(_, ::i32_extend16_s) = FuncType([i32], [i32])

inst_func_type(_, ::i32_wrap_i64) = FuncType([i64], [i32])
inst_func_type(_, ::f32_demote_f64) = FuncType([f64], [f32])
inst_func_type(_, ::f64_promote_f32) = FuncType([f32], [f64])

inst_func_type(_, ::i64_extend8_s) = FuncType([i64], [i64])
inst_func_type(_, ::i64_extend16_s) = FuncType([i64], [i64])
inst_func_type(_, ::i64_extend32_s) = FuncType([i64], [i64])
inst_func_type(_, ::i64_extend_i32_s) = FuncType([i32], [i64])
inst_func_type(_, ::i64_extend_i32_u) = FuncType([i32], [i64])

inst_func_type(_, ::i32_reinterpret_f32) = FuncType([f32], [i32])
inst_func_type(_, ::i64_reinterpret_f64) = FuncType([f64], [i64])
inst_func_type(_, ::f32_reinterpret_i32) = FuncType([i32], [f32])
inst_func_type(_, ::f64_reinterpret_i64) = FuncType([i64], [f64])

inst_func_type(_, ::f32_convert_i64_s) = FuncType([i64], [f32])
inst_func_type(_, ::f32_convert_i64_u) = FuncType([i64], [f32])
inst_func_type(_, ::f32_convert_i32_s) = FuncType([i32], [f32])
inst_func_type(_, ::f32_convert_i32_u) = FuncType([i32], [f32])

inst_func_type(_, t::throw_) = FuncType([], [])

inst_func_type(_, ::f64_convert_i64_s) = FuncType([i64], [f64])
inst_func_type(_, ::f64_convert_i64_u) = FuncType([i64], [f64])
inst_func_type(_, ::f64_convert_i32_s) = FuncType([i32], [f64])
inst_func_type(_, ::f64_convert_i32_u) = FuncType([i32], [f64])

inst_func_type(val, c::call) = copy(get_function_type(val.mod, c.func))
function inst_func_type(val, c::call_indirect)
    t = resolve_type(val.mod.types, c.typeidx)
    FuncType([i32, t.params...], copy(t.results))
end

inst_func_type(_, ::memory_grow) = FuncType([i32], [i32])

inst_func_type(_, b::Block) = copy(b.fntype)
inst_func_type(_, b::Loop) = copy(b.fntype)
inst_func_type(_, b::If) = FuncType([b.fntype.params..., i32], copy(b.fntype.results))
