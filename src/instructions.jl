"""
Indices in instructions are one-based. therefore when saving for wasm,
it must be converted to zero-based.
"""
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
    null::Bool
    typeidx::Index
end

struct StructRef <: WasmRef
    null::Bool
    typeidx::Index
end

struct StringRef <: WasmRef end

valtype(::Type{Bool}) = i32
valtype(::Type{Int32}) = i32
valtype(::Type{UInt32}) = i32
valtype(::Type{Int64}) = i64
valtype(::Type{UInt64}) = i64
valtype(::Type{Float32}) = f32
valtype(::Type{Float64}) = f64
valtype(::Type{T}) where {T} =
    isprimitivetype(T) &&
    sizeof(T) <= 4 ? i32 :
    sizeof(T) <= 8 ? i64 :
    sizeof(T) <= 16 ? v128 :
    error("type $T cannot be represented in wasm")

abstract type WasmType end

struct FuncType <: WasmType
    params::Vector{ValType}
    results::Vector{ValType}
end

Base.:(==)(fntype1::FuncType, fntype2::FuncType) =
    fntype1.params == fntype2.params && fntype1.results == fntype2.results

struct MemArg
    align::UInt32
    offset::UInt32
end
MemArg() = MemArg(0, 0)

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
"`local.tee` is like `local.set` but leaves the value on the stack"
struct local_tee <: Inst
    n::Index
end
struct local_set <: Inst
    n::Index
end

abstract type UnaryInst <: Inst end
abstract type BinaryInst <: Inst end

for (WT, T) in zip((f32, f64), (Float32, Float64))
    @eval struct $(Symbol(WT, "_const")) <: Inst
        val::$T
    end

    load_s = Symbol(WT, "_load")
    @eval struct $load_s <: Inst
        memarg::MemArg
    end
    @eval $load_s() = $load_s(MemArg())

    store_s = Symbol(WT, "_store")
    @eval struct $store_s <: Inst
        memarg::MemArg
    end
    @eval $store_s() = $store_s(MemArg())

    for f in ("abs", "neg", "sqrt", "ceil", "floor", "trunc", "nearest")
        @eval struct $(Symbol(WT, "_", f)) <: UnaryInst end
    end

    for f in ("add", "sub", "mul", "div", "min", "max", "copysign",
        "eq", "ne", "lt", "gt", "le", "ge")
        @eval struct $(Symbol(WT, "_", f)) <: BinaryInst end
    end
end

for (WT, T) in zip((i32, i64), (Int32, Int64))
    @eval struct $(Symbol(WT, "_const")) <: Inst
        val::$T
    end

    load_s = Symbol(WT, "_load")
    @eval struct $load_s <: Inst
        memarg::MemArg
    end
    @eval $load_s() = $load_s(MemArg())

    store_s = Symbol(WT, "_store")
    @eval struct $store_s <: Inst
        memarg::MemArg
    end
    @eval $store_s() = $store_s(MemArg())

    for f in ("clz", "ctz", "popcnt", "eqz")
        @eval struct $(Symbol(WT, "_", f)) <: UnaryInst end
    end

    for f in ("add", "sub", "mul", "div_u", "div_s", "rem_u", "rem_s",
        "and", "or", "xor", "shl", "shr_u", "shr_s", "rotl", "rotr",
        "eq", "ne", "lt_u", "lt_s", "gt_u", "gt_s", "le_u", "le_s",
        "ge_u", "ge_s")
        @eval struct $(Symbol(WT, "_", f)) <: BinaryInst end
    end
end

struct i32_load8_s <: Inst
    memarg::MemArg
end
i32_load8_s() = i32_load8_s(MemArg())
struct i32_load8_u <: Inst
    memarg::MemArg
end
i32_load8_u() = i32_load8_u(MemArg())
struct i64_load8_s <: Inst
    memarg::MemArg
end
i64_load8_s() = i64_load8_s(MemArg())
struct i64_load8_u <: Inst
    memarg::MemArg
end
i64_load8_u() = i64_load8_u(MemArg())

struct i32_load16_s <: Inst
    memarg::MemArg
end
i32_load16_s() = i32_load16_s(MemArg())
struct i32_load16_u <: Inst
    memarg::MemArg
end
i32_load16_u() = i32_load16_u(MemArg())
struct i64_load16_s <: Inst
    memarg::MemArg
end
i64_load16_s() = i64_load16_s(MemArg())
struct i64_load16_u <: Inst
    memarg::MemArg
end
i64_load16_u() = i64_load16_u(MemArg())

struct i32_store16 <: Inst
    memarg::MemArg
end
i32_store16() = i32_store16(MemArg())
struct i64_store16 <: Inst
    memarg::MemArg
end
i64_store16() = i64_store16(MemArg())

struct i64_store32 <: Inst
    memarg::MemArg
end
i64_store32() = i64_store32(MemArg())
struct i64_load32_s <: Inst
    memarg::MemArg
end
i64_load32_s() = i64_load32_s(MemArg())
struct i64_load32_u <: Inst
    memarg::MemArg
end
i64_load32_u() = i64_load32_u(MemArg())

struct i32_extend8_s <: UnaryInst end
struct i32_extend16_s <: UnaryInst end

struct i64_extend8_s <: UnaryInst end
struct i64_extend16_s <: UnaryInst end
struct i64_extend32_s <: UnaryInst end
struct i64_extend_i32_s <: UnaryInst end
struct i64_extend_i32_u <: UnaryInst end

struct i32_reinterpret_f32 <: UnaryInst end
struct i64_reinterpret_f64 <: UnaryInst end
struct f32_reinterpret_i32 <: UnaryInst end
struct f64_reinterpret_i64 <: UnaryInst end

struct f64_promote_f32 <: UnaryInst end

struct f32_convert_i32_s <: UnaryInst end
struct f32_convert_i32_u <: UnaryInst end
struct f32_convert_i64_s <: UnaryInst end
struct f32_convert_i64_u <: UnaryInst end

struct f64_convert_i64_s <: UnaryInst end
struct f64_convert_i64_u <: UnaryInst end
struct f64_convert_i32_s <: UnaryInst end
struct f64_convert_i32_u <: UnaryInst end

struct i32_trunc_f32_s <: UnaryInst end
struct i32_trunc_f32_u <: UnaryInst end
struct i32_trunc_f64_s <: UnaryInst end
struct i32_trunc_f64_u <: UnaryInst end

struct i64_trunc_f32_s <: UnaryInst end
struct i64_trunc_f32_u <: UnaryInst end
struct i64_trunc_f64_s <: UnaryInst end
struct i64_trunc_f64_u <: UnaryInst end

struct i32_wrap_i64 <: UnaryInst end
struct f32_demote_f64 <: UnaryInst end

struct v128_store <: Inst
    memarg::MemArg
end
v128_store() = v128_store(MemArg())
struct v128_load <: Inst
    memarg::MemArg
end
v128_load() = v128_load(MemArg())

enum_values(E) = E.(Int(typemin(E)):Int(typemax(E)))

module CmpOperators
    @enum CmpOperator eq ne lt gt le ge
    needs_sign(op) = op > ne
end
using .CmpOperators: CmpOperator

module MathOperators
    @enum MathOperator add sub mul div
end
using .MathOperators: MathOperator

module Lanes
    @enum Lane i8 i16 i32 i64 f32 f64
    is_integer(lane::Lane) = lane <= i64
    count(lane) =
        is_integer(lane) ?
            128 ÷ (2 ^ (3 + Int(lane))) :
            lane == f32 ? 4 : 2
end
using .Lanes: Lane

struct v128cmp <: Inst
    cmp::CmpOperator
    lane::Lane
    signed::Bool # for ints
end
v128cmp(cmp, lane) = v128cmp(cmp, lane, false)

struct v128splat <: UnaryInst
    lane::Lane
end

for lane in enum_values(Lane)
    name = Symbol(lane, "x", Lanes.count(lane), "_splat")
    @eval $name() = v128splat($lane)
end

struct v128bin <: BinaryInst
    lane::Lane
    op::MathOperator
end

for (op, lane) in Iterators.product(enum_values(MathOperator),
                                    enum_values(Lane))
    name = Symbol(lane, "x", Lanes.count(lane), "_", op)
    @eval $name() = v128bin($lane,  $op)
end

struct v128all_true <: UnaryInst
    lane::Lane
end
struct v128bitmask <: UnaryInst
    lane::Lane
end

struct elem_drop <: Inst
    elem::Index
end
struct table_init <: Inst
    elem::Index
    table::Index
end
struct table_copy <: Inst
    src::Index
    dst::Index
end
struct table_size <: Inst
    idx::Index
end
struct table_grow <: Inst
    idx::Index
end
struct table_fill <: Inst
    idx::Index
end

struct memory_copy <: Inst end
struct memory_fill <: Inst end

struct drop <: Inst end
struct select <: Inst
    valtype::Union{ValType,Nothing}
end
select() = select(nothing)

struct nop <: Inst end
struct unreachable <: TerminatorInst end
struct br <: TerminatorInst
    label::Index
end
struct br_if <: Inst
    label::Index
end
struct br_table <: Inst
    labels::Vector{Int}
    default::Index
end
struct br_on_cast <: Inst
    label::Index
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

struct ref_null <: Inst
    typeidx::Index
end
struct ref_as_non_null <: Inst end
struct ref_cast <: Inst
    typeidx::Index
end
struct ref_test <: Inst
    typeidx::Index
end

struct string_const <: Inst
    contents::String
end

