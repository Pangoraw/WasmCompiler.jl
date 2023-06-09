const Index = UInt32

abstract type ValType end

abstract type WasmNumeric <: ValType end

struct WasmInt32 <: WasmNumeric end
struct WasmInt64 <: WasmNumeric end
struct WasmFloat32 <: WasmNumeric end
struct WasmFloat64 <: WasmNumeric end
struct WasmVector128 <: WasmNumeric end

Base.show(io::IO, ::WasmInt32) = print(io, "i32")
Base.show(io::IO, ::WasmInt64) = print(io, "i64")
Base.show(io::IO, ::WasmFloat32) = print(io, "f32")
Base.show(io::IO, ::WasmFloat64) = print(io, "f64")
Base.show(io::IO, ::WasmVector128) = print(io, "v128")

const i32 = WasmInt32()
const i64 = WasmInt64()
const f32 = WasmFloat32()
const f64 = WasmFloat64()
const v128 = WasmVector128()

abstract type WasmRef <: ValType end

struct FuncRef <: WasmRef end
struct ExternRef <: WasmRef end

struct ArrayRef <: WasmRef
    content::ValType
end

struct StructRef <: WasmRef
    null::Bool
    typeidx::Index
end

valtype(::Type{Bool}) = i32

valtype(::Type{Int32}) = i32
valtype(::Type{Int64}) = i64
valtype(::Type{Float32}) = f32
valtype(::Type{Float64}) = f64

abstract type Inst end

abstract type ContainerInst <: Inst end
abstract type TerminatorInst <: Inst end

struct Block <: ContainerInst
    fntype::FuncType
    inst::Vector{Inst}
end

struct If <: ContainerInst
    fntype::FuncType
    trueinst::Vector{Inst}
    falseinst::Vector{Inst}
end

struct Loop <: ContainerInst
    fntype::FuncType
    inst::Vector{Inst}
end

const Tag = UInt32

struct CatchBlock
    tag::Union{Nothing,Tag} # use nothing for catch_all
    inst::Vector{Inst}
end

struct Try <: ContainerInst
    fntype::FuncType
    inst::Vector{Inst}
    catches::Vector{CatchBlock}
end

struct global_get <: Inst
    n::Index
end
struct global_set <: Inst
    n::Index
end

struct local_get <: Inst
    n::Index
end
"local.tee sets but leaves the value on the stack"
struct local_tee <: Inst
    n::Index
end
struct local_set <: Inst
    n::Index
end

for (WT, T) in zip((f32, f64), (Float32, Float64))
    @eval struct $(Symbol(WT, "_const")) <: Inst
        val::$T
    end
    @eval struct $(Symbol(WT, "_load")) <: Inst end
    @eval struct $(Symbol(WT, "_store")) <: Inst end

    for f in ("abs", "neg", "sqrt", "ceil", "floor", "trunc", "nearest",
        "add", "sub", "mul", "div", "min", "max", "copysign",
        "eq", "ne", "lt", "gt", "le", "ge")
        @eval struct $(Symbol(WT, "_", f)) <: Inst end
    end
end

for (WT, T) in zip((i32, i64), (Int32, Int64))
    @eval struct $(Symbol(WT, "_const")) <: Inst
        val::$T
    end
    @eval struct $(Symbol(WT, "_load")) <: Inst end
    @eval struct $(Symbol(WT, "_store")) <: Inst end

    for f in ("add", "sub", "mul", "div_u", "div_s", "rem_u", "rem_s",
        "and", "or", "xor", "shl", "shr_u", "shr_s", "rotl", "rotr",
        "eq", "ne", "lt_u", "lt_s", "gt_u", "gt_s", "le_u", "le_s",
        "ge_u", "ge_s", "eqz", "clz", "ctz", "popcnt")
        @eval struct $(Symbol(WT, "_", f)) <: Inst end
    end
end

struct i32_load8_s <: Inst end
struct i32_load8_u <: Inst end
struct i64_load8_s <: Inst end
struct i64_load8_u <: Inst end

struct i32_load16_s <: Inst end
struct i32_load16_u <: Inst end
struct i64_load16_s <: Inst end
struct i64_load16_u <: Inst end

struct i32_store16 <: Inst end
struct i64_store16 <: Inst end

struct i64_store32 <: Inst end
struct i64_load32_s <: Inst end
struct i64_load32_u <: Inst end

struct i64_extend32_s <: Inst end
struct i64_extend_i32_s <: Inst end
struct i64_extend_i32_u <: Inst end

struct i32_reinterpret_f32 <: Inst end
struct i64_reinterpret_f64 <: Inst end
struct f32_reinterpret_i32 <: Inst end
struct f64_reinterpret_i64 <: Inst end

struct f64_promote_f32 <: Inst end

struct f32_convert_i32_s <: Inst end
struct f32_convert_i32_u <: Inst end
struct f32_convert_i64_s <: Inst end
struct f32_convert_i64_u <: Inst end

struct f64_convert_i64_s <: Inst end
struct f64_convert_i64_u <: Inst end
struct f64_convert_i32_s <: Inst end
struct f64_convert_i32_u <: Inst end

struct i32_wrap_i64 <: Inst end

struct drop <: Inst end
struct select <: Inst
    valtype::Union{ValType,Nothing}
end
select() = select(nothing)

struct nop <: Inst end
struct unreachable <: TerminatorInst end
struct br <: TerminatorInst
    label::Int
end
struct br_if <: Inst
    label::Inst
end
struct br_table <: Inst
    labels::Vector{Int}
    default::Index
end
struct br_on_cast <: Inst
    inputtype::Index
    casttype::Index
end

# Those are not keywords but still
struct throw_ <: Inst
    tag::Tag
end
struct rethrow_ <: Inst
    label::Index
end

struct return_ <: TerminatorInst end

struct call <: Inst
    func::Index
end

struct call_indirect <: Inst
    tableidx::Index
    typeidx::Index
end

struct struct_new <: Inst
    typeidx::Index
end
struct struct_get <: Inst
    typeidx::Index
    fieldidx::Index
end

struct array_new <: Inst
    typeidx::Index
end
struct array_len <: Inst end
struct array_get <: Inst
    typeidx::Index
end

struct ref_cast <: Inst
    typeidx::Index
end
struct ref_test <: Inst
    typeidx::Index
end

struct string_const <: Inst
    contents::String
end

## Utilities

function Base.map!(f, cont::Union{Func,Block,Loop})
    for i in eachindex(cont.inst)
        inst = cont.inst[i]
        cont.inst[i] = if inst isa Union{Block,Loop,If}
            map!(f, inst)
        else
            f(inst)
        end
    end
    cont
end
function Base.map!(f, if_::If)
    map!(f, if_.trueinst, if_.trueinst)
    map!(f, if_.falseinst, if_.falseinst)
    if_
end

function Base.foreach(f, cont::Union{Func,Block,Loop})
    for inst in cont.inst
        if inst isa Union{Block,Loop,If}
            foreach(f, inst)
        else
            f(inst)
        end
    end
end
function Base.foreach(f, if_::If)
    for inst in if_.trueinst
        if inst isa Union{Block,Loop,If}
            foreach(f, inst)
        else
            f(inst)
        end
    end
    for inst in if_.falseinst
        if inst isa Union{Block,Loop,If}
            foreach(f, inst)
        else
            f(inst)
        end
    end
end
