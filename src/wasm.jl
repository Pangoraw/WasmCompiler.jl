import Core.Compiler: widenconst, PiNode, PhiNode, ReturnNode,
    GotoIfNot, GotoNode, SSAValue, UpsilonNode, PhiCNode

@enum ValType i32 i64 f32 f64 v128 funcref externref

valtype(::Type{Bool}) = i32

valtype(::Type{Int32}) = i32
valtype(::Type{Int64}) = i64
valtype(::Type{Float32}) = f32
valtype(::Type{Float64}) = f64

const Index = UInt32

abstract type Inst end

abstract type ContainerInst <: Inst end
abstract type TerminatorInst <: Inst end

abstract type WasmType end

struct FuncType <: WasmType
    params::Vector{ValType}
    results::Vector{ValType}
end

struct StructField
    type::ValType
    name::Union{Nothing,String}
    mut::Bool
end

struct StructType <: WasmType
    name::Union{Nothing,String}
    fields::Vector{StructField}
end

struct RecursiveZone <: WasmType
    structs::Vector{StructType}
end

function jl_to_struct(T)
    mut = ismutabletype(T)
    StructType(nameof(T) |> string, [
        StructField(valtype(FT), FN isa Symbol ? string(FN) : nothing, mut)
        for (FT, FN) in zip(fieldtypes(T), fieldnames(T))
    ])
end

const voidtype = FuncType([], [])

struct MemoryType <: WasmType
    min::UInt32
    max::UInt32
end

struct TableType <: WasmType
    min::UInt32
    max::UInt32
    reftype::ValType
end

struct GlobalType <: WasmType
    mut::Bool
    type::ValType
end

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

    for f in ("add", "sub", "mul", "div_u", "div_s", "rem_u", "rem_s",
        "and", "or", "xor", "shl", "shr_u", "shr_s", "rotl", "rotr",
        "eq", "ne", "lt_u", "lt_s", "gt_u", "gt_s", "le_u", "le_s",
        "ge_u", "ge_s", "eqz", "clz", "ctz", "popcnt")
        @eval struct $(Symbol(WT, "_", f)) <: Inst end
    end
end

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

struct Func
    name::Union{Nothing,String}
    fntype::FuncType
    locals::Vector{ValType}
    inst::Vector{Inst}
end

abstract type Import end

struct FuncImport <: Import
    module_name::String
    name::String
    id::String
    fntype::FuncType
end

abstract type Export end

struct FuncExport <: Export
    name::String
    func::Index
end

struct Table
    type::TableType
end
struct Mem
    type::MemoryType
end
struct Global
    type::GlobalType
    init::Vector{Inst}
end

abstract type ElemMode end
struct ElemModePassive <: ElemMode end
struct ElemModeActive <: ElemMode
    table::Index
    offset::Vector{Inst}
end
struct ElemModeDeclarative <: ElemMode end

struct Elem
    type::ValType
    init::Vector{Vector{Inst}}
    mode::ElemMode
end

abstract type DataMode end
struct DataModePassive <: DataMode end
struct DataModeActive <: DataMode
    memory::Index
    offset::Vector{Inst}
end

struct Data
    init::Vector{UInt8}
    mode::DataMode
end

mutable struct WModule
    types::Vector{WasmType}
    funcs::Vector{Func}
    tables::Vector{Table}
    mems::Vector{Mem}
    globals::Vector{Global}
    elems::Vector{Elem}
    datas::Vector{Data}
    start::Union{Nothing,Index}
    imports::Vector{Import}
    exports::Vector{Export}
end
WModule(func::Func) = WModule(
        [], [func], [], [],
        [], [], [], nothing,
        [], [FuncExport(func.name::String, 1)],
    )


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

struct Intrinsic
    int::Bool
    s32::Inst
    s64::Inst
end

const INTRINSICS = Dict(
    Base.eq_int  => Intrinsic(true, i32_eq(), i64_eq()),
    Base.ne_int  => Intrinsic(true, i32_ne(), i64_ne()),
    Base.slt_int => Intrinsic(true, i32_lt_s(), i64_lt_s()),
    Base.ult_int => Intrinsic(true, i32_lt_u(), i64_lt_u()),
    Base.sle_int => Intrinsic(true, i32_le_s(), i64_le_s()),
    Base.ule_int => Intrinsic(true, i32_le_u(), i64_le_u()),
    Base.mul_int => Intrinsic(true, i32_mul(), i64_mul()),
    Base.add_int => Intrinsic(true, i32_add(), i64_add()),
    Base.sub_int => Intrinsic(true, i32_sub(), i64_sub()),
    Base.sdiv_int => Intrinsic(true, i32_div_s(), i64_div_s()),
    Base.udiv_int => Intrinsic(true, i32_div_u(), i64_div_u()),
    Base.srem_int => Intrinsic(true, i32_rem_s(), i64_rem_s()),
    Base.urem_int => Intrinsic(true, i32_rem_u(), i64_rem_u()),
    Base.and_int => Intrinsic(true, i32_and(), i64_and()),
    Base.or_int => Intrinsic(true, i32_or(), i64_or()),
    Base.xor_int => Intrinsic(true, i32_xor(), i64_xor()),
    # TODO: sh fns needs special handling as you can specify the shift with a different type
    Base.lshr_int => Intrinsic(true, i32_shr_u(), i64_shr_u()),
    Base.ashr_int => Intrinsic(true, i32_shr_s(), i64_shr_s()),
    Base.shl_int => Intrinsic(true, i32_shl(), i64_shl()),
    Base.mul_float => Intrinsic(false, f32_mul(), f64_mul()),
    Base.add_float => Intrinsic(false, f32_add(), f64_add()),
    Base.sub_float => Intrinsic(false, f32_sub(), f64_sub()),
    Base.neg_float => Intrinsic(false, f32_neg(), f64_neg()),
    Base.div_float => Intrinsic(false, f32_div(), f64_div()),
    Base.eq_float => Intrinsic(false, f32_eq(), f64_eq()),
    Base.ne_float => Intrinsic(false, f32_ne(), f64_ne()),
    Base.lt_float => Intrinsic(false, f32_lt(), f64_lt()),
    Base.le_float => Intrinsic(false, f32_le(), f64_le()),
    Base.abs_float => Intrinsic(false, f32_abs(), f64_abs()),
    Base.copysign_float => Intrinsic(false, f32_copysign(), f64_copysign()),
    Base.ceil_llvm => Intrinsic(false, f32_ceil(), f64_ceil()),
    Base.floor_llvm => Intrinsic(false, f32_floor(), f64_floor()),
    Base.trunc_llvm => Intrinsic(false, f32_trunc(), f64_trunc()),
    Base.rint_llvm => Intrinsic(false, f32_nearest(), f64_nearest()),
)

function resolve_arg(inst, n)
    @assert Meta.isexpr(inst, :call)
    arg = inst.args[n]
    arg isa GlobalRef && isconst(arg) ?
        getfield(arg.mod, arg.name) : error("invalid call $inst")
end

function emit_codes(ir, nargs; debug=false)
    exprs = Vector{Inst}[]

    # TODO: handle first arg properly
    locals = ValType[
        valtype(argtype)
        for argtype in ir.argtypes[begin+1:begin+nargs]
    ]

    ssa_to_local = fill(-1, length(ir.stmts))

    function getlocal!(val)
        val isa Core.Argument && return val.n - 2
        val isa SSAValue || error("invalid value $val")
        loc = ssa_to_local[val.id]
        if loc == -1
            push!(locals, irtype(val))
            loc = ssa_to_local[val.id] = length(locals) - 1
        end
        loc
    end

    function jltype(val)
        if val isa Core.Argument
            ir.argtypes[val.n]
        elseif val isa Core.SSAValue
            widenconst(ir.stmts[val.id][:type])
        elseif val isa Core.Const
            typeof(val.val)
        else
            typeof(val)
        end
    end

    irtype(val) = valtype(jltype(val))

    function emit_val(val)
        if val isa Core.Argument
            # One indexing + Skip first arg
            local_get(val.n - 2)
        elseif val isa Core.SSAValue
            loc = getlocal!(val)
            local_get(loc)
        elseif val isa Int32 || val isa Bool
            i32_const(val)
        elseif val isa Int64
            i64_const(val)
        elseif val isa Float32
            f32_const(val)
        elseif val isa Float64
            f64_const(val)
        elseif val isa String
            string_const(val)
        elseif isnothing(val)
            nop()
        else
            error("invalid value $val @ $(typeof(val))")
        end
    end

    for (sidx, stmt) in enumerate(ir.stmts)
        inst = stmt[:inst] 
        inst isa PhiCNode || continue

        ssa = SSAValue(sidx)

        phic_loc = getlocal!(ssa)

        for ups_ssa in inst.values
            @assert ups_ssa isa SSAValue
            @assert ir.stmts[ups_ssa.id][:inst] isa UpsilonNode
            ssa_to_local[ups_ssa.id] = phic_loc
        end
    end

    for (bidx, b) in enumerate(ir.cfg.blocks)
        push!(exprs, debug ?
            Inst[i64_const(bidx), drop()] : Inst[])

        for sidx in b.stmts
            stmt = ir.stmts[sidx]
            inst = stmt[:inst]

            ssa = SSAValue(sidx)

            if Meta.isexpr(inst, :call)
                f = inst.args[1]
                if f isa GlobalRef && isconst(f)
                    f = getfield(f.mod, f.name)
                end
                if f === Base.sext_int
                    typ = resolve_arg(inst, 2)
                    arg = inst.args[3]
                    push!(exprs[bidx], emit_val(arg))
                    if typ == Int64 && irtype(arg) == i32
                        push!(exprs[bidx], i64_extend_i32_s())
                        push!(exprs[bidx], local_set(getlocal!(ssa)))
                        continue
                    else
                        throw("unsupported sext_int $(inst)")
                    end
                elseif f === Base.fpext
                    typ = resolve_arg(inst, 2)
                    arg = inst.args[3]
                    push!(exprs[bidx], emit_val(arg))
                    argtype = irtype(arg)
                    if typ == Float64 && argtype == f32
                        push!(exprs[bidx], f64_promote_f32())
                    elseif typ == Float64 && argtype == f64
                        # pass
                    else
                        error("invalid fpext $inst")
                    end
                    push!(exprs[bidx], local_set(getlocal!(ssa)))
                    continue
                elseif f === Base.bitcast
                    typ = resolve_arg(inst, 2)
                    arg = inst.args[3]
                    argtype = irtype(arg)
                    if typ == Int32 && argtype == f32
                        push!(exprs[bidx], i32_reinterpret_f32())
                    elseif typ == Int64 && argtype == f64
                        push!(exprs[bidx], i64_reinterpret_f64())
                    elseif typ == Float32 && argtype == i32
                        push!(exprs[bidx], f32_reinterpret_i32())
                    elseif typ == Float64 && argtype == i64
                        push!(exprs[bidx], f64_reinterpret_i64())
                    else
                        error("invalid bitcast $inst (argtype = $argtype)")
                    end
                    push!(exprs[bidx], local_set(getlocal!(ssa)))
                    continue
                elseif f === Base.trunc_int
                    typ = resolve_arg(inst, 2)
                    arg = inst.args[3]
                    push!(exprs[bidx], emit_val(arg))
                    argtype = irtype(arg)
                    if typ == Int32 && argtype == i64
                        push!(exprs[bidx], i32_wrap_i64())
                    elseif typ == Int32 && argtype == i32
                        # pass
                    else
                        error("invalid trunc_int $inst")
                    end
                    push!(exprs[bidx], local_set(getlocal!(ssa)))
                    continue
                elseif f === Base.sitofp
                    typ = resolve_arg(inst, 2)
                    arg = inst.args[3]
                    push!(exprs[bidx], emit_val(arg))
                    argtype = jltype(arg)
                    if typ == Float32
                        if argtype == Int32
                            push!(exprs[bidx], f32_convert_i32_s()) 
                        elseif argtype == UInt32
                            push!(exprs[bidx], f32_convert_i32_u())
                        elseif argtype == Int64
                            push!(exprs[bidx], f32_convert_i64_s()) 
                        elseif argtype == UInt64
                            push!(exprs[bidx], f32_convert_i64_u())
                        else
                            error("unsupported conversion $inst")
                        end
                    elseif typ == Float64
                        if argtype == Int32
                            push!(exprs[bidx], f64_convert_i32_s()) 
                        elseif argtype == UInt32
                            push!(exprs[bidx], f64_convert_i32_u())
                        elseif argtype == Int64
                            push!(exprs[bidx], f64_convert_i64_s()) 
                        elseif argtype == UInt64
                            push!(exprs[bidx], f64_convert_i64_u())
                        else
                            error("unsupported conversion $inst")
                        end
                    end
                    push!(exprs[bidx], local_set(getlocal!(ssa)))
                    continue
                elseif f === Core.ifelse
                    push!(exprs[bidx], emit_val(inst.args[3]))
                    push!(exprs[bidx], emit_val(inst.args[4]))
                    push!(exprs[bidx], emit_val(inst.args[2]))
                    push!(exprs[bidx], select())
                    push!(exprs[bidx], local_set(getlocal!(ssa)))
                    continue
                elseif f === Base.neg_int
                    arg = inst.args[2]
                    typ = jltype(arg)
                    if typ == Bool
                        push!(exprs[bidx], emit_val(arg))
                    elseif typ == Int32 || typ == UInt32
                        push!(exprs[bidx], i32_const(0), emit_val(arg), i32_sub())
                    elseif typ == Int64 || typ == UInt64
                        push!(exprs[bidx], i64_const(0), emit_val(arg), i64_sub())
                    else
                        error("invalid neg_int $inst")
                    end
                    push!(exprs[bidx], local_set(getlocal!(ssa)))
                    continue
                end
                for arg in inst.args[begin+1:end]
                    push!(exprs[bidx], emit_val(arg))
                end
                if haskey(INTRINSICS, f)
                    intr = INTRINSICS[f]
                    newinst = if intr.int
                        if all(arg -> irtype(arg) == i32, inst.args[begin+1:end])
                            intr.s32
                        elseif all(arg -> irtype(arg) == i64, inst.args[begin+1:end])
                            intr.s64
                        else
                            error("unsupported call $inst")
                        end
                    else
                        if all(arg -> irtype(arg) == f32, inst.args[begin+1:end])
                            intr.s32 
                        elseif all(arg -> irtype(arg) == f64, inst.args[begin+1:end])
                            intr.s64
                        else
                            error("unsupported call $inst")
                        end
                    end
                    push!(exprs[bidx], newinst)
                elseif f === Base.not_int
                    typ = jltype(inst.args[begin+1])
                    if typ == Bool
                        push!(exprs[bidx], i32_eqz())
                    elseif typ == Int32 || typ == UInt32
                        push!(exprs[bidx], i32_const(-1), i32_xor())
                    elseif typ == Int64 || typ == UInt64
                        push!(exprs[bidx], i64_const(-1), i64_xor())
                    else
                        error("invalid not_int")
                    end
                elseif f === Base.:(===)
                    if all(arg -> irtype(arg) == i32, inst.args[begin+1:end])
                        push!(exprs[bidx], i32_eq())
                    elseif all(arg -> irtype(arg) == i64, inst.args[begin+1:end])
                        push!(exprs[bidx], i64_eq())
                    elseif all(arg -> irtype(arg) == f32, inst.args[begin+1:end])
                        push!(exprs[bidx], f32_eq())
                    elseif all(arg -> irtype(arg) == f64, inst.args[begin+1:end])
                        push!(exprs[bidx], f64_eq())
                    else
                        error("invalid sub_int")
                    end
                else
                    error("Cannot handle call to $f")
                end

                loc = getlocal!(ssa)
                push!(exprs[bidx], local_set(loc))
            elseif inst isa PiNode
                loc = getlocal!(ssa)
                push!(exprs[bidx], emit_val(inst.val))
                push!(exprs[bidx], local_set(loc))
            elseif inst isa GotoIfNot 
                push!(exprs[bidx], emit_val(inst.cond))
            elseif inst isa ReturnNode
                if !isdefined(inst, :val)
                    continue
                end
                push!(exprs[bidx], emit_val(inst.val))
                # push!(exprs[bidx], return_())
            elseif inst isa GotoNode 
                # pass
            elseif inst isa PhiCNode
                # handled by assigning a local to each PhiCNode at the start of emission
            elseif inst isa UpsilonNode
                if !isdefined(inst, :val)
                    continue # skip upsnode
                end
                loc = getlocal!(ssa)
                push!(exprs[bidx], emit_val(inst.val))
                push!(exprs[bidx], local_set(loc))
            elseif inst isa PhiNode
                # handled on incoming blocks
            elseif Meta.isexpr(inst, :invoke)
                f = inst.args[2]
                f = f isa GlobalRef && isconst(f) ? getfield(f.mod, f.name) : f
                # TODO: use the exception handling proposal
                if f === Core.throw_inexacterror ||
                    f === Core.throw ||
                    f === Base.Math.throw_complex_domainerror

                    push!(exprs[bidx], unreachable())
                    continue
                end
                for arg in inst.args[begin+2:end]
                    push!(exprs[bidx], emit_val(arg))
                end
                @warn "invoke is currently emulated" inst
                push!(exprs[bidx], call(0))
                loc = getlocal!(ssa)
                push!(exprs[bidx], local_set(loc))
            else
                @warn "Unhandled instruction" inst typeof(inst)
            end
        end

        for tgt in b.succs
            tgt = ir.cfg.blocks[tgt]
            for tgt_sidx in tgt.stmts
                tgt_inst = ir.stmts[tgt_sidx][:inst]
                tgt_inst isa Core.PhiNode || continue

                validx = findfirst(==(bidx), tgt_inst.edges)
                isnothing(validx) && continue
                push!(exprs[bidx], emit_val(tgt_inst.values[validx]))
                philoc = getlocal!(SSAValue(tgt_sidx))
                push!(exprs[bidx], local_set(philoc))
            end
        end
    end

    exprs, locals
end

function emit_func(f, types; optimize=false, debug=false)
    ir, rt = Base.code_ircode(f, types) |> only

    nargs = length(types.parameters)
    exprs, locals = emit_codes(ir, nargs; debug)

    relooper = Relooper(exprs, ir)

    expr = Inst[
        reloop!(relooper),
        unreachable(), # CF will go through a ReturnNode
    ]

    functype = FuncType(
        locals[begin:nargs],
        [valtype(rt)],
    )

    name = replace(nameof(f) |> string, "#" => "_")
    if startswith(name, "_")
        name = "jl" * name
    end
    f = Func(
        name,
        functype,
        locals,
        expr,
    )

    # _printwasm(stdout, f)

    if optimize
        f |> make_tees! # |> remove_unused!
    else
        f
    end
end

function unreachable_end!(func)
    if last(func.inst) isa Union{Block,Loop,If} &&
        !isempty(func.fntype.results) && isempty(last(func.inst).fntype.results)

        push!(func.inst, unreachable())
    end
    func
end

function towasm(io::IO, mod; opt=0, enable_gc=false, enable_reference_types=false)
    args = String[]
    enable_gc && push!(args, "--enable-gc")
    enable_reference_types && push!(args, "--enable-reference-types")
    wat = sprint(_printwasm, mod)
    run(pipeline(IOBuffer(wat),
        `wat2wasm $(args...) - --output="-"`,
        `wasm-opt -O$opt $(args...) - --output="-"`,
        io,
    ))
    io
end
towasm(f::String, mod; kwargs...) = open(io -> towasm(io, mod; kwargs...), f; write=true)

function towasm(mod; kwargs...)
    io = IOBuffer()
    towasm(io, mod; kwargs...)
    take!(io)
end

