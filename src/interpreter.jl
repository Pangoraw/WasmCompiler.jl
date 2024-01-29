module Interpreter

using ..WasmCompiler
using ..WasmCompiler: GlobalType, Module, MemoryType, ValType, FuncType
using ..WasmCompiler:
    i32_const, f32_const, f64_const, f32_lt, local_get, local_set,
    i32_eq, i32_ne, i32_lt_s, i32_lt_u, i32_le_s, i32_le_u, i32_gt_s, i32_gt_u, i32_ge_s, i32_ge_u,
    i64_eq, i64_ne, i64_lt_s, i64_lt_u, i64_le_s, i64_le_u, i64_gt_s, i64_gt_u, i64_ge_s, i64_ge_u,
    i32_eqz, i64_eqz, f64_lt, i32_sub,
    i32_add, i32_div_s, i32_div_u, i64_div_u, i64_div_s,
    i32_rem_s, i32_rem_u, i64_rem_s, i64_rem_u,
    i64_const, i64_add, i64_sub, i64_mul, i64_eq,
    i32_and, i32_or, i32_xor,
    i64_and, i64_or, i64_xor,
    i32_extend8_s, i32_extend16_s,
    i64_extend8_s, i64_extend16_s, i64_extend32_s,
    i32_popcnt, i32_clz, i32_ctz, i64_popcnt, i64_clz, i64_ctz,
    i32_rotl, i32_rotr, i64_rotl, i64_rotr,
    i32_shr_s, i32_shr_u, i32_shl, i64_shr_s, i64_shr_u, i64_shl,
    Block, If, Loop, i32_mul,
    f64_convert_i64_s, f64_sqrt, f32_sqrt,
    f64_convert_i64_u,
    f32_load,
    f32_add,
    br, nop, unreachable, return_, drop,
    i32, i64, f32, f64, v128

const PAGE_SIZE = 65536

mutable struct Memory <: AbstractVector{UInt8}
    type::MemoryType
    buf::Vector{UInt8}
end
Memory(type::MemoryType) = Memory(type, zeros(UInt8, PAGE_SIZE * type.min))

Base.size(m::Memory) = size(m.buf)
Base.getindex(m::Memory, idx...) = getindex(m.buf, idx...)
Base.setindex!(m::Memory, val, idx...) = setindex!(m.buf, val, idx...)

mutable struct Global{T}
    type::GlobalType
    val::T
end

mutable struct Store
    mems::Vector{Memory}
end

struct Instance
    mod::Module

    mems::Vector{Memory}
    globals::Vector{Global}
end

function instantiate(module_)
    @assert isempty(module_.globals)

    mems = map(module_.mems) do m
        Memory(m.type) 
    end

    Instance(module_, mems, [])
end

mutable struct CallFrame
    fntype::Union{Nothing,FuncType}
    locals::Vector{Any}
    value_stack::Vector{Any}

    jmp_counter::Int # increment at each block
    jmp_target::Int  # reach jmp_counter when returning from interpret
end
CallFrame() = CallFrame(nothing, Any[], Any[], 0, 0)

function interpret(instance, frame, expr)
    frame.jmp_counter += 1
    current_stack = frame.jmp_counter

    for inst in expr
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
        elseif inst isa i64_extend8_s
            x = pop!(frame.value_stack)::Int64 % Int8
            push!(frame.value_stack, Int64(x))
        elseif inst isa i64_extend16_s
            x = pop!(frame.value_stack)::Int64 % Int16
            push!(frame.value_stack, Int64(x))
        elseif inst isa i64_extend32_s
            x = pop!(frame.value_stack)::Int64 % Int32
            push!(frame.value_stack, Int64(x))
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
            push!(frame.value_stack, bitrotate(a, (b % 64)))
        elseif inst isa i64_rotr
            b, a = pop!(frame.value_stack)::Int64, pop!(frame.value_stack)::Int64
            push!(frame.value_stack, bitrotate(a, -(b % 64)))
        elseif inst isa drop
            pop!(frame.value_stack)
        elseif inst isa f64_convert_i64_s
            push!(frame.value_stack, Float64(pop!(frame.value_stack)::Int64))
        elseif inst isa f64_convert_i64_u
            push!(frame.value_stack, Float64(reinterpret(UInt64, pop!(frame.value_stack)::Int64)))
        elseif inst isa f32_add
            push!(frame.value_stack, pop!(frame.value_stack)::Float32 + pop!(frame.value_stack)::Float32)
        elseif inst isa f32_load
            ptr = pop!(frame.value_stack)::Int32 + inst.memarg.offset
            push!(frame.value_stack, reinterpret(Float32, instance.mems[1])[1 + div(ptr, sizeof(Float32))])
        elseif inst isa If
            cond = pop!(frame.value_stack)::Int32

            if cond != zero(Int32)
                interpret(instance, frame, inst.trueinst)

                if current_stack > frame.jmp_target
                    frame.jmp_counter = current_stack - 1
                    return
                elseif current_stack == frame.jmp_target
                    # ok
                end
            else
                interpret(instance, frame, inst.falseinst)

                if current_stack > frame.jmp_target
                    frame.jmp_counter = current_stack - 1
                    return
                elseif current_stack == frame.jmp_target
                    # ok
                end
            end
        elseif inst isa Loop
            while true
                interpret(instance, frame, inst.inst)

                if current_stack > frame.jmp_target
                    frame.jmp_counter = current_stack - 1
                    return
                elseif current_stack == frame.jmp_target
                    continue
                else
                    break
                end
            end
        elseif inst isa Block

            interpret(instance, frame, inst.inst)

            if current_stack > frame.jmp_target
                frame.jmp_counter = current_stack - 1
                return
            elseif current_stack == frame.jmp_target
                # ok
            end

        elseif inst isa return_
            values = reverse([pop!(frame.value_stack) for _ in frame.fntype.results])
            empty!(frame.value_stack)
            append!(frame.value_stack, values)
            frame.jmp_target = -1
            return
        elseif inst isa br
            frame.jmp_target = current_stack - inst.label - 1
            return
        elseif inst isa nop
            # pass
        elseif inst isa unreachable
            throw(UnreachableReached())
        else
            error("unimplemented inst $inst")
        end
    end

    # Simulate (br 0)
    frame.jmp_target = current_stack - 1
    frame.jmp_counter = current_stack - 1

    return
end

struct UnreachableReached <: Exception end

Base.showerror(io::IO, ::UnreachableReached) = print(io, "trap: unreachable")

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

function invoke(instance, name, args)
    func = instance.mod.funcs[name]

    frame = CallFrame(
        func.fntype,
        Any[zero(jltype(v)) for v in func.locals],
        Any[], 0, 0,
    )
    for (i, (T,arg)) in enumerate(zip(func.fntype.params, args))
        @assert arg isa jltype(T) "invalid argument #$i of type $(typeof(arg)), wanted $(T)"
        frame.locals[i] = arg
    end
    interpret(instance, frame, func.inst)

    frame.value_stack
end

end # module Interpreter
