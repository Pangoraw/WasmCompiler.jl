module Interpreter

using ..WasmCompiler
using ..WasmCompiler: GlobalType, Module, MemoryType, ValType
using ..WasmCompiler:
    i32_const, f32_const, f64_const, f32_lt, local_get, local_set,
    i64_le_s, i64_lt_s, i32_eqz, f64_lt, i32_sub,
    i32_add, i64_const, i64_add, i64_mul, i64_eq,
    Block, If, Loop, i32_mul,
    f64_convert_i64_s, f64_sqrt, f32_sqrt,
    f64_convert_i64_u,
    br, nop, unreachable, return_, drop,
    i32, i64, f32, f64, v128

mutable struct Memory
    type::MemoryType
    buf::Vector{UInt8}
end

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
    @assert isempty(module_.mems)
    @assert isempty(module_.globals)
    Instance(module_, [], [])
end

mutable struct CallFrame
    locals::Vector{Any}
    value_stack::Vector{Any}

    jmp_counter::Int # increment at each block
    jmp_target::Int  # reach jmp_counter when returning from interpret
end
CallFrame() = CallFrame(Any[], Any[], 0, 0)

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
        elseif inst isa i64_le_s
            b, a = pop!(frame.value_stack), pop!(frame.value_stack)
            push!(frame.value_stack, Int32(a::Int64 <= b::Int64))
        elseif inst isa i64_lt_s
            b, a = pop!(frame.value_stack), pop!(frame.value_stack)
            push!(frame.value_stack, Int32(a::Int64 < b::Int64))
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
        elseif inst isa i32_eqz
            push!(frame.value_stack, Int32(pop!(frame.value_stack)::Int32 == 0))
        elseif inst isa local_get
            push!(frame.value_stack, frame.locals[inst.n])
        elseif inst isa local_set
            frame.locals[inst.n] = pop!(frame.value_stack)
        elseif inst isa i32_add
            push!(frame.value_stack, pop!(frame.value_stack)::Int32 + pop!(frame.value_stack)::Int32)
        elseif inst isa i32_sub
            push!(frame.value_stack, pop!(frame.value_stack)::Int32 - pop!(frame.value_stack)::Int32)
        elseif inst isa i32_mul
            push!(frame.value_stack, pop!(frame.value_stack)::Int32 * pop!(frame.value_stack)::Int32)
        elseif inst isa i64_add
            push!(frame.value_stack, pop!(frame.value_stack)::Int64 + pop!(frame.value_stack)::Int64)
        elseif inst isa i64_mul
            push!(frame.value_stack, pop!(frame.value_stack)::Int64 * pop!(frame.value_stack)::Int64)
        elseif inst isa i64_eq
            push!(frame.value_stack, Int32(pop!(frame.value_stack)::Int64 == pop!(frame.value_stack)::Int64))
        elseif inst isa drop
            pop!(frame.value_stack)
        elseif inst isa f64_convert_i64_s
            push!(frame.value_stack, Float64(pop!(frame.value_stack)::Int64))
        elseif inst isa f64_convert_i64_u
            push!(frame.value_stack, Float64(reinterpret(UInt64, pop!(frame.value_stack)::Int64)))
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
