module Interpreter

using ..WasmCompiler
using ..WasmCompiler: GlobalType, Module, MemoryType, ValType, FuncType, Lanes, MathOperators
using ..WasmCompiler:
    i32_const, f32_const, f64_const, f32_lt, local_get, local_set, local_tee,
    i32_eq, i32_ne, i32_lt_s, i32_lt_u, i32_le_s, i32_le_u, i32_gt_s, i32_gt_u, i32_ge_s, i32_ge_u,
    i64_eq, i64_ne, i64_lt_s, i64_lt_u, i64_le_s, i64_le_u, i64_gt_s, i64_gt_u, i64_ge_s, i64_ge_u,
    i32_eqz, i64_eqz, f64_lt, i32_sub,
    i32_add, i32_div_s, i32_div_u, i64_div_u, i64_div_s,
    i32_rem_s, i32_rem_u, i64_rem_s, i64_rem_u,
    i64_const, i64_add, i64_sub, i64_mul, i64_eq,
    i32_and, i32_or, i32_xor,
    i64_and, i64_or, i64_xor,
    i32_extend8_s, i32_extend16_s, i32_wrap_i64,
    i64_extend8_s, i64_extend16_s, i64_extend32_s,
    i32_popcnt, i32_clz, i32_ctz, i64_popcnt, i64_clz, i64_ctz,
    i32_rotl, i32_rotr, i64_rotl, i64_rotr,
    i32_shr_s, i32_shr_u, i32_shl, i64_shr_s, i64_shr_u, i64_shl,
    Block, If, Loop, i32_mul,
    f64_sqrt, f32_sqrt,
    i32_load, i64_load, f32_load, f64_load,
    i32_load8_s, i32_load8_u, i32_load16_s, i32_load16_u,
    i64_load8_s, i64_load8_u, i64_load16_s, i64_load16_u,
    i64_load32_s, i64_load32_u,
    i32_store, i64_store, f32_store, f64_store,
    i32_store8, i32_store16, i64_store8, i64_store16, i64_store32,
    f32_neg, f32_add, f32_sub, f32_mul, f32_div, f32_nearest,
    f64_neg, f64_add, f64_sub, f64_mul, f64_div, f64_nearest,
    f32_ne, f32_eq, f32_gt, f32_ge, f32_le, f32_lt, 
    f64_ne, f64_eq, f64_gt, f64_ge, f64_le, f64_lt, 
    f32_demote_f64, f64_promote_f32,
    i32_trunc_f32_s, i32_trunc_f32_u, f32_reinterpret_i32,
    i64_trunc_f64_s, i64_trunc_f64_u, f64_reinterpret_i64,
    i64_extend_i32_s, i64_extend_i32_u,
    f32_convert_i32_u, f32_convert_i64_u,
    f32_convert_i32_s, f32_convert_i64_s,
    f64_convert_i32_s, f64_convert_i64_s,
    f64_convert_i32_u, f64_convert_i64_u,
    call,
    memory_grow, memory_copy, memory_fill,
    v128_const, v128bin,
    global_set, global_get,
    select, br, br_if, br_table, nop, unreachable, return_, drop,
    i32, i64, f32, f64, v128

const PAGE_SIZE = 65536

struct Memory <: AbstractVector{UInt8}
    type::MemoryType
    buf::Vector{UInt8}
end
Memory(type::MemoryType) = Memory(type, zeros(UInt8, PAGE_SIZE * type.min))

Base.size(m::Memory) = size(m.buf)
Base.getindex(m::Memory, idx...) = getindex(m.buf, idx...)
Base.setindex!(m::Memory, val, idx...) = setindex!(m.buf, val, idx...)

mutable struct Global{T}
    const type::GlobalType
    val::T
end

function Base.setindex!(g::Global{T}, v::T) where {T}
    @assert g.type.mut
    g.val = v
end
Base.getindex(g::Global) = g.val

struct Instance
    mod::Module

    imported_funcs::Vector{Any}
    mems::Vector{Memory}
    globals::Vector{Global}
end

struct FuncRef
    inst::Instance
    idx::UInt32
end

function (fr::FuncRef)(args...)
    ft = WC.get_function_type(fr.inst.mod, fr.idx)
    results = invoke(fr.inst, fr.idx, collect(args))
    isempty(ft.results) && return nothing
    return last(results, length(ft.results))
end

function exports(instance)
    exports = Dict{Symbol,Any}()
    for exp in instance.mod.exports
        if exp isa WC.FuncExport
            exports[Symbol(exp.name)] = FuncRef(instance, exp.func)
        elseif exp isa WC.MemExport
            exports[Symbol(exp.name)] = instance.mems[exp.mem]
        elseif exp isa WC.GlobalExport
            exports[Symbol(exp.name)] = instance.globals[exp.globalidx]
        end
    end
    NamedTuple(exports)
end

function instantiate(module_, imports=(;))
    mems = if any(imp -> imp isa WC.MemImport, module_.imports)
        mem_import = module_.imports[findfirst(imp -> imp isa WC.MemImport, module_.imports)]
        Memory[imports[mem_import.mod_name][mem_import.name]]
    else
        map(m -> Memory(m.type), module_.mems)
    end

    imported_funcs = map(imp -> imports[imp.mod_name][imp.name],
                         filter(imp -> imp isa WC.FuncImport, module_.imports))

    globals = Global[]
    inst = Instance(module_, imported_funcs, mems, globals)

    for global_ in module_.globals
        T = jltype(global_.type.type)
        frame = CallFrame()
        interpret(inst, frame, global_.init)
        init = only(frame.value_stack)::T
        push!(globals, Global{T}(global_.type, init))
    end

    inst
end

abstract type Trap <: Exception end

struct OutOfBoundsError <: Trap
    addr::Int
    mem_length::Int
end

Base.showerror(io::IO, e::OutOfBoundsError) = print(io, "trap: address $(e.addr) is out of bound for memory of size $(e.mem_length)")

struct UnreachableReached <: Trap end

Base.showerror(io::IO, ::UnreachableReached) = print(io, "trap: unreachable")

function check_inbounds(mem, ptr)
    if ptr < 0 || ptr >= length(mem)
        throw(OutOfBoundsError(ptr, length(mem)))
    end
end

struct CallFrame
    fntype::Union{Nothing,FuncType}
    locals::Vector{Any}
    value_stack::Vector{Any}
end
CallFrame() = CallFrame(nothing, Any[], Any[])

function interpret(instance, frame, expr)
    expr_stack = Vector{WasmCompiler.Inst}[expr]
    stack_ptr_stack = Int[0]
    pc_stack = Int[lastindex(expr)+1]

    pc = firstindex(expr)
    function pop_label_stack!(label)
        local stack_ptr
        for _ in 0:label
            pc = pop!(pc_stack)
            expr = pop!(expr_stack)
            stack_ptr = pop!(stack_ptr_stack)
        end
        pc > lastindex(expr) && return
        inst = expr[pc]
        if inst isa WC.ContainerInst
            num_to_keep = inst isa Loop ? length(inst.fntype.params) : length(inst.fntype.results)
            splice!(frame.value_stack,
                    stack_ptr+1:lastindex(frame.value_stack)-num_to_keep)
        end
        # br to loop is to beginning of loop
        expr[pc] isa Loop && (pc -= 1)
        nothing
    end
    function push_label_stack!(inst)
        push!(expr_stack, expr)
        push!(pc_stack, pc)
        push!(stack_ptr_stack, lastindex(frame.value_stack))
        pc = 0
        expr = inst
    end

    while pc <= lastindex(expr)
        inst = expr[pc]

        if inst isa i32_const
            push!(frame.value_stack, inst.val)
        elseif inst isa i64_const
            push!(frame.value_stack, inst.val)
        elseif inst isa f32_const
            push!(frame.value_stack, inst.val)
        elseif inst isa f64_const
            push!(frame.value_stack, inst.val)
        elseif inst isa f32_lt
            b, a = pop!(frame.value_stack), pop!(frame.value_stack)
            push!(frame.value_stack, Int32(a::Float32 < b::Float32))
        elseif inst isa f64_lt
            b, a = pop!(frame.value_stack), pop!(frame.value_stack)
            push!(frame.value_stack, Int32(a::Float64 < b::Float64))
        elseif inst isa f32_sqrt
            push!(frame.value_stack, sqrt(pop!(frame.value_stack)::Float32))
        elseif inst isa f64_sqrt
            push!(frame.value_stack, sqrt(pop!(frame.value_stack)::Float64))
        elseif inst isa i32_eq
            push!(frame.value_stack, Int32(pop!(frame.value_stack)::Int32 == pop!(frame.value_stack)::Int32))
        elseif inst isa i32_ne
            push!(frame.value_stack, Int32(pop!(frame.value_stack)::Int32 != pop!(frame.value_stack)::Int32))
        elseif inst isa i32_eqz
            push!(frame.value_stack, Int32(pop!(frame.value_stack)::Int32 == Int32(0)))
        elseif inst isa i32_le_s
            b, a = pop!(frame.value_stack)::Int32, pop!(frame.value_stack)::Int32
            push!(frame.value_stack, Int32(a <= b))
        elseif inst isa i32_le_u
            b, a = pop!(frame.value_stack)::Int32, pop!(frame.value_stack)::Int32
            push!(frame.value_stack, Int32(reinterpret(UInt32, a) <= reinterpret(UInt32, b)))
        elseif inst isa i32_lt_s
            b, a = pop!(frame.value_stack)::Int32, pop!(frame.value_stack)::Int32
            push!(frame.value_stack, Int32(a < b))
        elseif inst isa i32_lt_u
            b, a = pop!(frame.value_stack)::Int32, pop!(frame.value_stack)::Int32
            push!(frame.value_stack, Int32(reinterpret(UInt32, a) < reinterpret(UInt32, b)))
        elseif inst isa i32_gt_s
            b, a = pop!(frame.value_stack)::Int32, pop!(frame.value_stack)::Int32
            push!(frame.value_stack, Int32(a > b))
        elseif inst isa i32_gt_u
            b, a = pop!(frame.value_stack)::Int32, pop!(frame.value_stack)::Int32
            push!(frame.value_stack, Int32(reinterpret(UInt32, a) > reinterpret(UInt32, b)))
        elseif inst isa i32_ge_s
            b, a = pop!(frame.value_stack)::Int32, pop!(frame.value_stack)::Int32
            push!(frame.value_stack, Int32(a >= b))
        elseif inst isa i32_ge_u
            b, a = pop!(frame.value_stack)::Int32, pop!(frame.value_stack)::Int32
            push!(frame.value_stack, Int32(reinterpret(UInt32, a) >= reinterpret(UInt32, b)))
        elseif inst isa i64_eq
            push!(frame.value_stack, Int32(pop!(frame.value_stack)::Int64 === pop!(frame.value_stack)::Int64))
        elseif inst isa i64_ne
            push!(frame.value_stack, Int32(pop!(frame.value_stack)::Int64 !== pop!(frame.value_stack)::Int64))
        elseif inst isa i64_eqz
            push!(frame.value_stack, Int32(pop!(frame.value_stack)::Int64 === Int64(0)))
        elseif inst isa i64_le_s
            b, a = pop!(frame.value_stack)::Int64, pop!(frame.value_stack)::Int64
            push!(frame.value_stack, Int32(a <= b))
        elseif inst isa i64_le_u
            b, a = pop!(frame.value_stack)::Int64, pop!(frame.value_stack)::Int64
            push!(frame.value_stack, Int32(reinterpret(UInt64, a) <= reinterpret(UInt64, b)))
        elseif inst isa i64_lt_s
            b, a = pop!(frame.value_stack)::Int64, pop!(frame.value_stack)::Int64
            push!(frame.value_stack, Int32(a < b))
        elseif inst isa i64_lt_u
            b, a = pop!(frame.value_stack)::Int64, pop!(frame.value_stack)::Int64
            push!(frame.value_stack, Int32(reinterpret(UInt64, a) < reinterpret(UInt64, b)))
        elseif inst isa i64_gt_s
            b, a = pop!(frame.value_stack)::Int64, pop!(frame.value_stack)::Int64
            push!(frame.value_stack, Int32(a > b))
        elseif inst isa i64_gt_u
            b, a = pop!(frame.value_stack)::Int64, pop!(frame.value_stack)::Int64
            push!(frame.value_stack, Int32(reinterpret(UInt64, a) > reinterpret(UInt64, b)))
        elseif inst isa i64_ge_s
            b, a = pop!(frame.value_stack)::Int64, pop!(frame.value_stack)::Int64
            push!(frame.value_stack, Int32(a >= b))
        elseif inst isa i64_ge_u
            b, a = pop!(frame.value_stack)::Int64, pop!(frame.value_stack)::Int64
            push!(frame.value_stack, Int32(reinterpret(UInt64, a) >= reinterpret(UInt64, b)))
        elseif inst isa local_get
            push!(frame.value_stack, frame.locals[inst.n])
        elseif inst isa local_set
            frame.locals[inst.n] = pop!(frame.value_stack)
        elseif inst isa local_tee
            frame.locals[inst.n] = last(frame.value_stack)
        elseif inst isa global_set
            val = pop!(frame.value_stack)
            g = instance.globals[inst.n]
            @assert g.type.mut
            g.val = val
        elseif inst isa global_get
            g = instance.globals[inst.n]
            push!(frame.value_stack, g.val)
        elseif inst isa i32_add
            push!(frame.value_stack, pop!(frame.value_stack)::Int32 + pop!(frame.value_stack)::Int32)
        elseif inst isa i32_sub
            b, a = pop!(frame.value_stack)::Int32, pop!(frame.value_stack)::Int32
            push!(frame.value_stack, a - b)
        elseif inst isa i32_mul
            push!(frame.value_stack, pop!(frame.value_stack)::Int32 * pop!(frame.value_stack)::Int32)
        elseif inst isa i32_div_s
            b, a = pop!(frame.value_stack)::Int32, pop!(frame.value_stack)::Int32
            push!(frame.value_stack, div(a, b))
        elseif inst isa i32_div_u
            b, a = pop!(frame.value_stack)::Int32, pop!(frame.value_stack)::Int32
            push!(frame.value_stack, reinterpret(Int32, div(reinterpret(UInt32, a), reinterpret(UInt32, b))))
        elseif inst isa i32_rem_s
            b, a = pop!(frame.value_stack)::Int32, pop!(frame.value_stack)::Int32
            if b === zero(Int32)
                push!(frame.value_stack, zero(Int32))
            else
                push!(frame.value_stack, rem(a, b))
            end
        elseif inst isa i32_rem_u
            b, a = pop!(frame.value_stack)::Int32, pop!(frame.value_stack)::Int32
            if b === zero(Int32)
                push!(frame.value_stack, zero(Int32))
            else
                push!(frame.value_stack, reinterpret(Int32, rem(reinterpret(UInt32, a), reinterpret(UInt32, b))))
            end
        elseif inst isa i32_and
            push!(frame.value_stack, pop!(frame.value_stack)::Int32 & pop!(frame.value_stack)::Int32)
        elseif inst isa i32_or
            push!(frame.value_stack, pop!(frame.value_stack)::Int32 | pop!(frame.value_stack)::Int32)
        elseif inst isa i32_xor
            push!(frame.value_stack, xor(pop!(frame.value_stack)::Int32, pop!(frame.value_stack)::Int32))
        elseif inst isa i32_shl
            b, a = pop!(frame.value_stack)::Int32, pop!(frame.value_stack)::Int32
            push!(frame.value_stack, a << mod(b, Int32(32)))
        elseif inst isa i32_shr_s
            b, a = pop!(frame.value_stack)::Int32, pop!(frame.value_stack)::Int32
            push!(frame.value_stack, a >> mod(b, Int32(32)))
        elseif inst isa i32_shr_u
            b, a = pop!(frame.value_stack)::Int32, pop!(frame.value_stack)::Int32
            push!(frame.value_stack, a >>> mod(b, Int32(32)))
        elseif inst isa i32_rotl
            b, a = pop!(frame.value_stack)::Int32, pop!(frame.value_stack)::Int32
            push!(frame.value_stack, bitrotate(a, mod(b, Int32(32))))
        elseif inst isa i32_rotr
            b, a = pop!(frame.value_stack)::Int32, pop!(frame.value_stack)::Int32
            push!(frame.value_stack, bitrotate(a, -mod(b, Int32(32))))
        elseif inst isa i32_extend8_s
            x = pop!(frame.value_stack)::Int32 % Int8
            push!(frame.value_stack, Int32(x))
        elseif inst isa i32_extend16_s
            x = pop!(frame.value_stack)::Int32 % Int16
            push!(frame.value_stack, Int32(x))
        elseif inst isa i32_wrap_i64
            x = pop!(frame.value_stack)::Int64
            push!(frame.value_stack, x % Int32)
        elseif inst isa f32_demote_f64
            push!(frame.value_stack, Float32(pop!(frame.value_stack)::Float64))
        elseif inst isa f64_promote_f32
            push!(frame.value_stack, Float64(pop!(frame.value_stack)::Float32))
        elseif inst isa i64_extend8_s
            x = pop!(frame.value_stack)::Int64 % Int8
            push!(frame.value_stack, Int64(x))
        elseif inst isa i64_extend16_s
            x = pop!(frame.value_stack)::Int64 % Int16
            push!(frame.value_stack, Int64(x))
        elseif inst isa i64_extend32_s
            x = pop!(frame.value_stack)::Int64 % Int32
            push!(frame.value_stack, Int64(x))
        elseif inst isa i64_extend_i32_s
            push!(frame.value_stack, Int64(pop!(frame.value_stack)::Int32))
        elseif inst isa i64_extend_i32_u
            push!(frame.value_stack, reinterpret(Int64, UInt64(reinterpret(UInt32, pop!(frame.value_stack)::Int32))))
        elseif inst isa i64_add
            push!(frame.value_stack, pop!(frame.value_stack)::Int64 + pop!(frame.value_stack)::Int64)
        elseif inst isa i64_sub
            b, a = pop!(frame.value_stack)::Int64, pop!(frame.value_stack)::Int64
            push!(frame.value_stack, a - b)
        elseif inst isa i64_mul
            push!(frame.value_stack, pop!(frame.value_stack)::Int64 * pop!(frame.value_stack)::Int64)
        elseif inst isa i64_div_s
            b, a = pop!(frame.value_stack)::Int64, pop!(frame.value_stack)::Int64
            push!(frame.value_stack, div(a, b))
        elseif inst isa i64_div_u
            b, a = pop!(frame.value_stack)::Int64, pop!(frame.value_stack)::Int64
            push!(frame.value_stack, reinterpret(Int64, div(reinterpret(UInt64, a), reinterpret(UInt64, b))))
        elseif inst isa i64_rem_s
            b, a = pop!(frame.value_stack)::Int64, pop!(frame.value_stack)::Int64
            push!(frame.value_stack, rem(a, b))
        elseif inst isa i64_rem_u
            b, a = pop!(frame.value_stack)::Int64, pop!(frame.value_stack)::Int64
            push!(frame.value_stack, reinterpret(Int64, rem(reinterpret(UInt64, a), reinterpret(UInt64, b))))
        elseif inst isa i64_eq
            push!(frame.value_stack, Int32(pop!(frame.value_stack)::Int64 == pop!(frame.value_stack)::Int64))
        elseif inst isa i64_and
            push!(frame.value_stack, pop!(frame.value_stack)::Int64 & pop!(frame.value_stack)::Int64)
        elseif inst isa i64_or
            push!(frame.value_stack, pop!(frame.value_stack)::Int64 | pop!(frame.value_stack)::Int64)
        elseif inst isa i64_xor
            push!(frame.value_stack, xor(pop!(frame.value_stack)::Int64, pop!(frame.value_stack)::Int64))
        elseif inst isa i64_shl
            b, a = pop!(frame.value_stack)::Int64, pop!(frame.value_stack)::Int64
            push!(frame.value_stack, a << mod(b, 64))
        elseif inst isa i64_shr_s
            b, a = pop!(frame.value_stack)::Int64, pop!(frame.value_stack)::Int64
            push!(frame.value_stack, a >> mod(b, 64))
        elseif inst isa i64_shr_u
            b, a = pop!(frame.value_stack)::Int64, pop!(frame.value_stack)::Int64
            push!(frame.value_stack, a >>> mod(b, 64))
       elseif inst isa i32_popcnt
            push!(frame.value_stack, Int32(Base.count_ones(pop!(frame.value_stack)::Int32)))
        elseif inst isa i32_clz
            push!(frame.value_stack, Int32(Base.leading_zeros(pop!(frame.value_stack)::Int32)))
        elseif inst isa i32_ctz
            push!(frame.value_stack, Int32(Base.trailing_zeros(pop!(frame.value_stack)::Int32)))
        elseif inst isa i64_popcnt
            push!(frame.value_stack, Int64(Base.count_ones(pop!(frame.value_stack)::Int64)))
        elseif inst isa i64_clz
            push!(frame.value_stack, Int64(Base.leading_zeros(pop!(frame.value_stack)::Int64)))
        elseif inst isa i64_ctz
            push!(frame.value_stack, Int64(Base.trailing_zeros(pop!(frame.value_stack)::Int64)))
         elseif inst isa i64_rotl
            b, a = pop!(frame.value_stack)::Int64, pop!(frame.value_stack)::Int64
            push!(frame.value_stack, bitrotate(a, mod(b, 64)))
        elseif inst isa i64_rotr
            b, a = pop!(frame.value_stack)::Int64, pop!(frame.value_stack)::Int64
            push!(frame.value_stack, bitrotate(a, -mod(b, 64)))
        elseif inst isa f32_convert_i32_u
            push!(frame.value_stack, Float32(reinterpret(UInt32, pop!(frame.value_stack)::Int32)))
        elseif inst isa f32_convert_i32_s
            push!(frame.value_stack, Float32(pop!(frame.value_stack)::Int32))
        elseif inst isa f32_convert_i64_u
            push!(frame.value_stack, Float32(reinterpret(UInt64, pop!(frame.value_stack)::Int64)))
        elseif inst isa f32_convert_i64_s
            push!(frame.value_stack, Float32(pop!(frame.value_stack)::Int64))
        elseif inst isa f64_convert_i32_s
            push!(frame.value_stack, Float64(pop!(frame.value_stack)::Int32))
        elseif inst isa f64_convert_i32_u
            push!(frame.value_stack, Float64(reinterpret(UInt32, pop!(frame.value_stack)::Int32)))
        elseif inst isa f64_convert_i64_s
            push!(frame.value_stack, Float64(pop!(frame.value_stack)::Int64))
        elseif inst isa f64_convert_i64_u
            push!(frame.value_stack, Float64(reinterpret(UInt64, pop!(frame.value_stack)::Int64)))
        elseif inst isa f32_neg
            push!(frame.value_stack, -pop!(frame.value_stack)::Float32)
        elseif inst isa f64_neg
            push!(frame.value_stack, -pop!(frame.value_stack)::Float64)
        elseif inst isa f32_add
            push!(frame.value_stack, pop!(frame.value_stack)::Float32 + pop!(frame.value_stack)::Float32)
        elseif inst isa f32_sub
            b, a = pop!(frame.value_stack)::Float32, pop!(frame.value_stack)::Float32
            push!(frame.value_stack, a - b)
        elseif inst isa f32_mul
            b, a = pop!(frame.value_stack)::Float32, pop!(frame.value_stack)::Float32
            push!(frame.value_stack, a * b)
        elseif inst isa f32_div
            b, a = pop!(frame.value_stack)::Float32, pop!(frame.value_stack)::Float32
            push!(frame.value_stack, a / b)
        elseif inst isa f32_nearest
            push!(frame.value_stack, round(pop!(frame.value_stack)::Float32))
        elseif inst isa f32_eq
            b, a = pop!(frame.value_stack)::Float32, pop!(frame.value_stack)::Float32
            push!(frame.value_stack, Int32(a == b))
        elseif inst isa f32_ne
            b, a = pop!(frame.value_stack)::Float32, pop!(frame.value_stack)::Float32
            push!(frame.value_stack, Int32(a != b))
        elseif inst isa f32_lt
            b, a = pop!(frame.value_stack)::Float32, pop!(frame.value_stack)::Float32
            push!(frame.value_stack, Int32(a < b))
        elseif inst isa f32_le
            b, a = pop!(frame.value_stack)::Float32, pop!(frame.value_stack)::Float32
            push!(frame.value_stack, Int32(a <= b))
        elseif inst isa f32_gt
            b, a = pop!(frame.value_stack)::Float32, pop!(frame.value_stack)::Float32
            push!(frame.value_stack, Int32(a > b))
        elseif inst isa f32_ge
            b, a = pop!(frame.value_stack)::Float32, pop!(frame.value_stack)::Float32
            push!(frame.value_stack, Int32(a >= b))
        elseif inst isa f32_reinterpret_i32
            x = pop!(frame.value_stack)::Int32
            push!(frame.value_stack, reinterpret(Float32, x))
        elseif inst isa f64_add
            push!(frame.value_stack, pop!(frame.value_stack)::Float64 + pop!(frame.value_stack)::Float64)
        elseif inst isa f64_sub
            b, a = pop!(frame.value_stack)::Float64, pop!(frame.value_stack)::Float64
            push!(frame.value_stack, a - b)
        elseif inst isa f64_mul
            b, a = pop!(frame.value_stack)::Float64, pop!(frame.value_stack)::Float64
            push!(frame.value_stack, a * b)
        elseif inst isa f64_div
            b, a = pop!(frame.value_stack)::Float64, pop!(frame.value_stack)::Float64
            push!(frame.value_stack, a / b)
        elseif inst isa f64_nearest
            push!(frame.value_stack, round(pop!(frame.value_stack)::Float64))
        elseif inst isa f64_eq
            b, a = pop!(frame.value_stack)::Float64, pop!(frame.value_stack)::Float64
            push!(frame.value_stack, Int32(a == b))
        elseif inst isa f64_ne
            b, a = pop!(frame.value_stack)::Float64, pop!(frame.value_stack)::Float64
            push!(frame.value_stack, Int32(a != b))
        elseif inst isa f64_lt
            b, a = pop!(frame.value_stack)::Float64, pop!(frame.value_stack)::Float64
            push!(frame.value_stack, Int32(a < b))
        elseif inst isa f64_le
            b, a = pop!(frame.value_stack)::Float64, pop!(frame.value_stack)::Float64
            push!(frame.value_stack, Int32(a <= b))
        elseif inst isa f64_gt
            b, a = pop!(frame.value_stack)::Float64, pop!(frame.value_stack)::Float64
            push!(frame.value_stack, Int32(a > b))
        elseif inst isa f64_ge
            b, a = pop!(frame.value_stack)::Float64, pop!(frame.value_stack)::Float64
            push!(frame.value_stack, Int32(a >= b))
        elseif inst isa f64_reinterpret_i64
            x = pop!(frame.value_stack)::Int64
            push!(frame.value_stack, reinterpret(Float64, x))
        elseif inst isa i64_trunc_f64_s
            push!(frame.value_stack, Int64(floor(pop!(frame.value_stack)::Float64)))
        elseif inst isa i32_load
            ptr = pop!(frame.value_stack)::Int32 + inst.memarg.offset
            check_inbounds(instance.mems[1], ptr)
            push!(frame.value_stack, reinterpret(Int32, instance.mems[1].buf[1+ptr:sizeof(Int32)+ptr])[1])
        elseif inst isa i64_load
            ptr = pop!(frame.value_stack)::Int32 + inst.memarg.offset
            check_inbounds(instance.mems[1], ptr)
            push!(frame.value_stack, reinterpret(Int64, instance.mems[1].buf[1+ptr:sizeof(Int64)+ptr])[1])
        elseif inst isa f32_load
            ptr = pop!(frame.value_stack)::Int32 + inst.memarg.offset
            check_inbounds(instance.mems[1], ptr)
            push!(frame.value_stack, reinterpret(Float32, instance.mems[1].buf[1+ptr:sizeof(Float32)+ptr])[1])
        elseif inst isa f64_load
            ptr = pop!(frame.value_stack)::Int32 + inst.memarg.offset
            check_inbounds(instance.mems[1], ptr)
            push!(frame.value_stack, reinterpret(Float64, instance.mems[1].buf[1+ptr:sizeof(Float64)+ptr])[1])
        elseif inst isa i32_load8_s
            ptr = pop!(frame.value_stack)::Int32 + inst.memarg.offset
            check_inbounds(instance.mems[1], ptr)
            push!(frame.value_stack, Int32(reinterpret(Int8, instance.mems[1].buf[1 + ptr])))
        elseif inst isa i32_load8_u
            ptr = pop!(frame.value_stack)::Int32 + inst.memarg.offset
            check_inbounds(instance.mems[1], ptr)
            push!(frame.value_stack, Int32(instance.mems[1][1 + ptr]))
        elseif inst isa i32_load16_s
            ptr = pop!(frame.value_stack)::Int32 + inst.memarg.offset
            check_inbounds(instance.mems[1], ptr)
            push!(frame.value_stack, Int32(reinterpret(Int16, @view instance.mems[1].buf[1+ptr:sizeof(Int16)+ptr])[1]))
        elseif inst isa i32_load16_u
            ptr = pop!(frame.value_stack)::Int32 + inst.memarg.offset
            check_inbounds(instance.mems[1], ptr)
            push!(frame.value_stack, Int32(reinterpret(UInt16, @view instance.mems[1].buf[1+ptr:sizeof(Int16)+ptr])[1]))
        elseif inst isa i64_load8_s
            ptr = pop!(frame.value_stack)::Int32 + inst.memarg.offset
            check_inbounds(instance.mems[1], ptr)
            push!(frame.value_stack, Int64(reinterpret(Int8, instance.mems[1][1 + ptr])))
        elseif inst isa i64_load8_u
            ptr = pop!(frame.value_stack)::Int32 + inst.memarg.offset
            check_inbounds(instance.mems[1], ptr)
            push!(frame.value_stack, Int64(instance.mems[1][1 + ptr]))
        elseif inst isa i64_load16_s
            ptr = pop!(frame.value_stack)::Int32 + inst.memarg.offset
            check_inbounds(instance.mems[1], ptr)
            push!(frame.value_stack, Int64(reinterpret(Int16, instance.mems[1+ptr:sizeof(Int16)+ptr])[1]))
        elseif inst isa i64_load16_u
            ptr = pop!(frame.value_stack)::Int32 + inst.memarg.offset
            check_inbounds(instance.mems[1], ptr)
            push!(frame.value_stack, Int64(reinterpret(UInt16, instance.mems[1+ptr:sizeof(UInt16)+ptr])[1]))
        elseif inst isa i64_load32_s
            ptr = pop!(frame.value_stack)::Int32 + inst.memarg.offset
            check_inbounds(instance.mems[1], ptr)
            push!(frame.value_stack, Int64(reinterpret(Int32, instance.mems[1+ptr:sizeof(Int32)+ptr])[1]))
        elseif inst isa i64_load32_u
            ptr = pop!(frame.value_stack)::Int32 + inst.memarg.offset
            check_inbounds(instance.mems[1], ptr)
            push!(frame.value_stack, Int64(reinterpret(UInt32, instance.mems[1+ptr:sizeof(UInt32)+ptr])[1]))
        elseif inst isa i32_store
            val = pop!(frame.value_stack)::Int32
            ptr = pop!(frame.value_stack)::Int32 + inst.memarg.offset
            check_inbounds(instance.mems[1], ptr)
            buf = instance.mems[1].buf
            reinterpret(Int32, @view buf[1+ptr:sizeof(Int32)+ptr])[1] = val
        elseif inst isa i64_store
            val = pop!(frame.value_stack)::Int64
            ptr = pop!(frame.value_stack)::Int32 + inst.memarg.offset
            check_inbounds(instance.mems[1], ptr)
            buf = instance.mems[1].buf
            reinterpret(Int64, @view buf[1+ptr:sizeof(Int64)+ptr])[1] = val
        elseif inst isa f32_store
            val = pop!(frame.value_stack)::Float32
            ptr = pop!(frame.value_stack)::Int32 + inst.memarg.offset
            check_inbounds(instance.mems[1], ptr)
            buf = instance.mems[1].buf
            reinterpret(Float32, @view buf[1+ptr:sizeof(Int64)+ptr])[1] = val
        elseif inst isa f64_store
            val = pop!(frame.value_stack)::Float64
            ptr = pop!(frame.value_stack)::Int32 + inst.memarg.offset
            check_inbounds(instance.mems[1], ptr)
            buf = instance.mems[1].buf
            reinterpret(Float64, @view buf[1+ptr:sizeof(Float64)+ptr])[1] = val
        elseif inst isa i32_store8
            val = pop!(frame.value_stack)::Int32
            ptr = pop!(frame.value_stack)::Int32 + inst.memarg.offset
            check_inbounds(instance.mems[1], ptr)
            mem = instance.mems[1]
            mem.buf[1 + ptr] = val
        elseif inst isa i32_store16
            val = pop!(frame.value_stack)::Int32
            ptr = pop!(frame.value_stack)::Int32 + inst.memarg.offset
            check_inbounds(instance.mems[1], ptr)
            mem = instance.mems[1]
            buf = instance.mems[1].buf
            reinterpret(Int16, @view buf[1+ptr:sizeof(Int16)+ptr])[1] = val % Int16
        elseif inst isa i64_store8
            val = pop!(frame.value_stack)::Int64
            ptr = pop!(frame.value_stack)::Int32 + inst.memarg.offset
            check_inbounds(instance.mems[1], ptr)
            mem = instance.mems[1]
            mem.buf[1 + ptr] = val
        elseif inst isa i64_store16
            val = pop!(frame.value_stack)::Int64
            ptr = pop!(frame.value_stack)::Int32 + inst.memarg.offset
            check_inbounds(instance.mems[1], ptr)
            buf = instance.mems[1].buf
            reinterpret(Int16, @view buf[1+ptr:sizeof(Int16)+ptr])[1] = val % Int16
        elseif inst isa i64_store32
            val = pop!(frame.value_stack)::Int64
            ptr = pop!(frame.value_stack)::Int32 + inst.memarg.offset
            check_inbounds(instance.mems[1], ptr)
            buf = instance.mems[1].buf
            reinterpret(Int32, @view buf[1+ptr:sizeof(Int32)+ptr])[1] = val % Int32
        elseif inst isa drop
            pop!(frame.value_stack)
        elseif inst isa select
            cond = pop!(frame.value_stack)::Int32
            b, a = pop!(frame.value_stack), pop!(frame.value_stack)
            if cond != Int32(0)
                push!(frame.value_stack, a)
            else
                push!(frame.value_stack, b)
            end
        elseif inst isa call
            ft = WC.get_function_type(instance.mod, inst.func)
            arguments = reverse([pop!(frame.value_stack) for _ in ft.params])
            results = invoke(instance, inst.func, arguments)
            append!(frame.value_stack, results)
        elseif inst isa v128_const
            push!(frame.value_stack, inst.val)
        elseif inst isa v128bin
            b = pop!(frame.value_stack)::NTuple{16,UInt8}
            a = pop!(frame.value_stack)::NTuple{16,UInt8}
            a, b = if inst.lane == Lanes.i8
                WC.i8x16(a), WC.i8x16(b)
            elseif inst.lane == Lanes.i16
                WC.i16x8(a), WC.i16x8(b)
            elseif inst.lane == Lanes.i32
                WC.i32x4(a), WC.i32x4(b)
            elseif inst.lane == Lanes.i64
                WC.i64x2(a), WC.i64x2(b)
            elseif inst.lane == Lanes.f32
                WC.f32x4(a), WC.f32x4(b)
            elseif inst.lane == Lanes.f64
                WC.f64x2(a), WC.f64x2(b)
            end

            res = if inst.op == MathOperators.add
                a .+ b
            elseif inst.op == MathOperators.sub
                a .- b
            elseif inst.op == MathOperators.mul
                a .* b
            elseif inst.op == MathOperators.div
                if inst.lane <= Lanes.i64
                    div.(a, b)
                else
                    a ./ b
                end
            end

            push!(frame.value_stack, if inst.lane == Lanes.i8
                WC.i8x16(res...)
            elseif inst.lane == Lanes.i16
                WC.i16x8(res...)
            elseif inst.lane == Lanes.i32
                WC.i32x4(res...)
            elseif inst.lane == Lanes.i64
                WC.i64x2(res...)
            elseif inst.lane == Lanes.f32
                WC.f32x4(res...)
            elseif inst.lane == Lanes.f64
                WC.f64x2(res...)
            end)
        elseif inst isa memory_grow
            mem = instance.mems[1]
            len = length(mem)
            n_pages = div(len, PAGE_SIZE)
            to_add = pop!(frame.value_stack)::Int32
            new_pages = n_pages + to_add
            if new_pages > mem.type.max
                push!(frame.value_stack, Int32(-1))
            else
                resize!(mem.buf, new_pages * PAGE_SIZE)
                fill!(@view(mem.buf[len+1:end]), 0x00)
                push!(frame.value_stack, Int32(1))
            end
        elseif inst isa memory_copy
            N, src, dest = pop!(frame.value_stack)::Int32, pop!(frame.value_stack)::Int32, pop!(frame.value_stack)::Int32
            buf = instance.mems[1].buf
            # TODO: inbounds?
            copyto!(buf, dest, buf, src, N)
        elseif inst isa memory_fill
            N, val, dest = pop!(frame.value_stack)::Int32, pop!(frame.value_stack)::Int32, pop!(frame.value_stack)::Int32
            0 <= val < 256 || error("invalid val")
            # TODO: inbounds?
            buf = instance.mems[1].buf
            fill!(view(buf, dest:dest+N-1), val)
        elseif inst isa If
            cond = pop!(frame.value_stack)::Int32

            push_label_stack!(!iszero(cond) ? inst.trueinst : inst.falseinst)
            #push!(pc_stack, pc)
            #push!(expr_stack, expr)

            #expr = !iszero(cond) ? inst.trueinst : inst.falseinst
            # pc = 0
        elseif inst isa Loop
            push_label_stack!(inst.inst)
            # push!(pc_stack, pc)
            # push!(expr_stack, expr)
            # pc = 0
            # expr = inst.inst
        elseif inst isa Block
            push_label_stack!(inst.inst)
            # push!(pc_stack, pc)
            # push!(expr_stack, expr)
            # pc = 0
            # expr = inst.inst
        elseif inst isa return_
            values = last(frame.value_stack, length(frame.fntype.results))
            empty!(frame.value_stack)
            append!(frame.value_stack, values)
            return
        elseif inst isa br_if
            cond = pop!(frame.value_stack)::Int32
            !iszero(cond) && pop_label_stack!(inst.label)
        elseif inst isa br
            pop_label_stack!(inst.label)
        elseif inst isa br_table
            idx = pop!(frame.value_stack)::Int32 + 1
            dest = if idx <= 0 || idx > length(inst.labels)
                inst.default
            else
                inst.labels[idx]
            end
            pop_label_stack!(dest)
        elseif inst isa nop
            # pass
        elseif inst isa unreachable
            throw(UnreachableReached())
        else
            error("unimplemented inst $inst")
        end

        # fallthrough
        while pc == lastindex(expr) && !isempty(pc_stack)
            expr = pop!(expr_stack)
            pc = pop!(pc_stack)
            pop!(stack_ptr_stack)
        end

        pc += 1
    end
end

jltype(valtype) = if valtype == i32
    Int32
elseif valtype == i64
    Int64
elseif valtype == f32
    Float32
elseif valtype == f64
    Float64
else
    error("invalid valtype $valtype")
end

function invoke(instance, idx, args)
    num_imports = count(imp -> imp isa WC.FuncImport, instance.mod.imports)

    if idx <= num_imports
        result = instance.imported_funcs[idx](args...)
        ft = WC.get_function_type(instance.mod, idx)
        if isempty(ft.results)
            return Any[]
        elseif length(ft.results) == 1
            return Any[result]
        else
            return result
        end
    end

    idx -= num_imports
    func = instance.mod.funcs[idx]

    frame = CallFrame(
        func.fntype,
        Any[zero(jltype(v)) for v in func.locals],
        Any[],
    )
    for (i, (T,arg)) in enumerate(zip(func.fntype.params, args))
        @assert arg isa jltype(T) "invalid argument #$i of type $(typeof(arg)), wanted $(T)"
        frame.locals[i] = arg
    end

    interpret(instance, frame, func.inst)

    results = last(frame.value_stack, length(func.fntype.results))
    results
end

export exports, instantiate

end # module Interpreter
