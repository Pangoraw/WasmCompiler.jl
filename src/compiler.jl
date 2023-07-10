import Core.Compiler: widenconst, PiNode, PhiNode, ReturnNode,
    GotoIfNot, GotoNode, SSAValue, UpsilonNode, PhiCNode

"""
    RuntimeModule()::WModule

Returns a `WModule` which can be merged with `bootstrap.wat` using `wasm-merge`.
"""
RuntimeModule() =
    WModule(
        [
            RecursiveZone([
                StructType("jl-value-t", nothing, [ # index 1
                    StructField(StructRef(true, 2), "jl-value-type", false),
                ]),
                StructType("jl-datatype-t", 1, [ # index 2
                    StructField(StructRef(true, 2), "jl-value-type", false),
                    StructField(StructRef(true, 3), "name", true),
                    StructField(StructRef(true, 2), "super", true),
                ]),
                StructType("jl-typename-t", 1, [ # index 3
                    StructField(StructRef(true, 2), "jl-value-type", false),
                    StructField(StructRef(false, 6), "name", false),
                ]),
                StructType("jl-string-t", 1, [ # index 4
                    StructField(StructRef(true, 2), "jl-value-type", false),
                    StructField(StringRef(), "str", false),
                ]),
                ArrayType("jl-values-t", true, jl_value_t), # index 5
                StructType("jl-simplevector-t", 1, [ # index 6
                    StructField(StructRef(true, 2), "jl-value-type", false),
                    StructField(ArrayRef(false, 5), "values", false),
                ]),
                StructType("jl-symbol-t", 1, [ # index 7
                    StructField(StructRef(true, 2), "jl-value-type", false),
                    StructField(i32, "hash", false),
                    StructField(StructRef(false, 3), "str", false),
                ])
            ]),
      ],
      [Func("jl_init", FuncType([], []), [], [])],
      [], [], [], [], [], jl_init,
      [
          FuncImport("bootstrap", "jl_symbol", "jl-symbol", FuncType([StringRef()], [jl_symbol_t])),
          FuncImport("bootstrap", "jl_new_datatype", "jl-new-datatype", FuncType([jl_symbol_t, i32, i32], [jl_datatype_t])),
          FuncImport("bootstrap", "jl_isa", "jl-isa", FuncType([jl_value_t, jl_datatype_t], [i32])),
          FuncImport("bootstrap", "jl_box_int32", "jl-box-int32", FuncType([i32], [jl_value_t])),
          FuncImport("bootstrap", "jl_box_int64", "jl-box-int64", FuncType([i64], [jl_value_t])),
          FuncImport("bootstrap", "jl_box_float32", "jl-box-float32", FuncType([f32], [jl_value_t])),
          FuncImport("bootstrap", "jl_box_float64", "jl-box-float64", FuncType([f64], [jl_value_t])),
          FuncImport("bootstrap", "jl_unbox_int32", "jl-unbox-int32", FuncType([jl_value_t], [i32])),
          FuncImport("bootstrap", "jl_unbox_int64", "jl-unbox-int64", FuncType([jl_value_t], [i64])),
          FuncImport("bootstrap", "jl_unbox_float32", "jl-unbox-float32", FuncType([jl_value_t], [f32])),
          FuncImport("bootstrap", "jl_unbox_float64", "jl-unbox-float64", FuncType([jl_value_t], [f64])),
          GlobalImport("bootstrap", "jl_exception", "jl-exception", GlobalType(true, jl_value_t)),
          TagImport("bootstrap", "jl_exception_tag", "jl-exception-tag", voidtype),
      ], [],
  )

# Useful indices in RuntimeModule

const jl_symbol = 1
const jl_new_datatype = 2
const jl_isa = 3
const jl_box_int32 = 4
const jl_box_int64 = 5
const jl_box_float32 = 6
const jl_box_float64 = 7
const jl_unbox_int32 = 8
const jl_unbox_int64 = 9
const jl_unbox_float32 = 10
const jl_unbox_float64 = 11

const jl_init = 12

const jl_value_t = StructRef(false, 1)
const jl_datatype_t = StructRef(false, 2)
const jl_typename_t = StructRef(false, 3)
const jl_string_t = StructRef(false, 4)
const jl_simplevector_t = StructRef(false, 6)
const jl_symbol_t = StructRef(false, 7)

struct CodegenContext
    mod::WModule
    type_dict::Dict
    func_dict::Dict
    datatype_dict::Dict
    optimize::Bool
    debug::Bool
end
CodegenContext(module_=RuntimeModule(); debug=false, optimize=false) =
    CodegenContext(module_, Dict(), Dict(), Dict(), optimize, debug)

function emit_datatype!(ctx, @nospecialize(typ))
    haskey(ctx.datatype_dict, typ) && return ctx.datatype_dict[typ]
    push!(ctx.mod.globals, Global(string(nameof(typ), "-type"), GlobalType(true, StructRef(true, 2)), [ref_null(2)]))
    init = ctx.mod.funcs[1]
    global_idx = length(ctx.mod.globals)
    push!(
        init.inst,
        string_const(string(nameof(typ))),
        call(jl_symbol),
        i32_const(typ.hash),
        i32_const(typ.flags),
        call(jl_new_datatype),
        global_set(global_idx),
    )
    ctx.datatype_dict[typ] = global_idx
    return global_idx
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
    Base.ctlz_int => Intrinsic(true, i32_clz(), i64_clz()),
    Base.cttz_int => Intrinsic(true, i32_ctz(), i64_ctz()),
    Base.checked_srem_int => Intrinsic(true, i32_rem_s(), i64_rem_s()),
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

function struct_idx!(ctx, @nospecialize(typ))
    isconcretetype(typ) || return 1
    get!(ctx.type_dict, typ) do
        @assert isconcretetype(typ) typ
        mut = ismutabletype(typ)
        push!(ctx.mod.types, StructType(string(nameof(typ)), 1,
          append!([StructField(StructRef(false, 2), "jl-value-type", false)], [
            StructField(isnumeric(FT) ? valtype(FT) : jl_value_t, string(FN), mut)
            for (FT, FN) in zip(fieldtypes(typ), fieldnames(typ))
        ])))
        emit_datatype!(ctx, typ)
        num_types(ctx.mod)
    end
end

function resolve_arg(inst, n)
    @assert Meta.isexpr(inst, :call) || Meta.isexpr(inst, :new)
    arg = inst.args[n]
    arg isa GlobalRef && isconst(arg) ?
        getfield(arg.mod, arg.name) : arg
end

function convert_val!(inst, from, to)
    if from == to
        push!(inst, nop())
    elseif from isa StructRef && to == jl_value_t
        push!(inst, ref_cast(jl_value_t.typeidx))
    elseif from == jl_value_t && to == i32
        push!(inst, call(jl_unbox_int32))
    elseif from == jl_value_t && to == i64
        push!(inst, call(jl_unbox_int64))
    elseif from == jl_value_t && to == f32
        push!(inst, call(jl_unbox_float32))
    elseif from == jl_value_t && to == f64
        push!(inst, call(jl_unbox_float64))
    elseif from == i32 && to == jl_value_t
        push!(inst, call(jl_box_int32), ref_cast(jl_value_t.typeidx))
    elseif from == i64 && to == jl_value_t
        push!(inst, call(jl_box_int64), ref_cast(jl_value_t.typeidx))
    elseif from == f32 && to == jl_value_t
        push!(inst, call(jl_box_float32), ref_cast(jl_value_t.typeidx))
    elseif from == f64 && to == jl_value_t
        push!(inst, call(jl_box_float64), ref_cast(jl_value_t.typeidx))
    else
        error("cannot convert value from $from to $to")
    end
end

isnumeric(@nospecialize typ) = isprimitivetype(typ) && sizeof(typ) <= sizeof(Int64)

function emit_codes(ctx, ir, rt, nargs)
    (; debug) = ctx
    exprs = Vector{Inst}[]

    types = Tuple{map(widenconst, ir.argtypes[begin:nargs+1])...}

    # TODO: handle first arg properly
    locals = ValType[
        isnumeric(argtype) ? valtype(argtype) : StructRef(false, struct_idx!(ctx, argtype))
        for argtype in map(widenconst, @view ir.argtypes[begin+1:begin+nargs])
    ]

    ssa_to_local = fill(-1, length(ir.stmts))

    function getlocal!(val)
        val isa Core.Argument && return val.n - 1
        val isa SSAValue || error("invalid value $val")
        loc = ssa_to_local[val.id]
        if loc == -1
            local_type = irtype(val)
            push!(locals, local_type)
            loc = ssa_to_local[val.id] = length(locals)
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

    function irtype(val)
        typ = jltype(val)
        isnumeric(typ) ? valtype(typ) : jl_value_t
    end

    function emit_val(val)
        if val isa Core.SSAValue || val isa Core.Argument
            local_get(getlocal!(val))
        elseif val isa Int32 || val isa Bool
            i32_const(val)
        elseif val isa UInt32
            i32_const(reinterpret(Int32, val))
        elseif val isa Int64
            i64_const(val)
        elseif val isa UInt64
            i64_const(reinterpret(Int64, val))
        elseif val isa Float32
            f32_const(val)
        elseif val isa Float64
            f64_const(val)
        elseif val isa String
            string_const(val)
        elseif isnothing(val)
            nop()
        elseif val isa GlobalRef && isconst(val)
            emit_val(getproperty(val.mod, val.name))
        else
            throw(CompilationError(types, "invalid value $val @ $(typeof(val))"))
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

        block_terminator = nothing

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
                        throw(CompilationError(types, "unsupported sext_int $(inst) @ $(irtype(arg))"))
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
                        throw(CompilationError(types, "invalid fpext $inst"))
                    end
                    push!(exprs[bidx], local_set(getlocal!(ssa)))
                    continue
                elseif f === Base.bitcast
                    typ = resolve_arg(inst, 2)
                    arg = inst.args[3]
                    argtype = irtype(arg)
                    push!(exprs[bidx], emit_val(arg))
                    if valtype(typ) == argtype
                        # pass
                    elseif typ in (Int32, UInt32) && argtype == f32
                        push!(exprs[bidx], i32_reinterpret_f32())
                    elseif typ in (Int64, UInt64) && argtype == f64
                        push!(exprs[bidx], i64_reinterpret_f64())
                    elseif typ == Float32 && argtype == i32
                        push!(exprs[bidx], f32_reinterpret_i32())
                    elseif typ == Float64 && argtype == i64
                        push!(exprs[bidx], f64_reinterpret_i64())
                    else
                        throw(CompilationError(types, "invalid bitcast $inst (argtype = $argtype)"))
                    end
                    push!(exprs[bidx], local_set(getlocal!(ssa)))
                    continue
                elseif f === Base.fptrunc
                    typ = resolve_arg(inst, 2)
                    arg = inst.args[3]
                    push!(exprs[bidx], emit_val(arg))
                    argtype = irtype(arg)
                    if typ == Float32 && argtype == f64
                        push!(exprs[bidx], f32_demote_f64())
                    else
                        throw(CompilationError(types, "unsupported fptrunc $inst"))
                    end
                    push!(exprs[bidx], local_set(getlocal!(ssa)))
                    continue
                elseif f === Base.trunc_int
                    typ = resolve_arg(inst, 2)
                    arg = inst.args[3]
                    push!(exprs[bidx], emit_val(arg))
                    argtype = irtype(arg)
                    if typ in (Int32, UInt32) && argtype == i64
                        push!(exprs[bidx], i32_wrap_i64())
                    elseif typ in (Int32, UInt32) && argtype == i32
                        # pass
                    else
                        throw(CompilationError(types, "invalid trunc_int $inst"))
                    end
                    push!(exprs[bidx], local_set(getlocal!(ssa)))
                    continue
                elseif f === Base.zext_int
                    typ = resolve_arg(inst, 2)
                    arg = inst.args[3]
                    push!(exprs[bidx], emit_val(arg))
                    argtype = jltype(arg)
                    if typ in (Int64, UInt64)
                        if argtype == Int32 || argtype == Bool
                            push!(exprs[bidx], i64_extend_i32_s())
                        elseif argtype == UInt32
                            push!(exprs[bidx], i64_extend_i32_u())
                        else
                            throw(CompilationError(types, "invalid zext_int $inst"))
                        end
                    elseif typ in (Int32, UInt32)
                        # pass
                    else
                        throw(CompilationError(types, "invalid zext_int $inst"))
                    end
                    push!(exprs[bidx], local_set(getlocal!(ssa)))
                    continue
                elseif f === Base.fptosi
                    typ = resolve_arg(inst, 2)
                    arg = inst.args[3]
                    push!(exprs[bidx], emit_val(arg))
                    argtype = jltype(arg)
                    if typ == Int32
                        if argtype == Float32
                            push!(exprs[bidx], i32_trunc_f32_s())
                        elseif argtype == Float64
                            push!(exprs[bidx], i32_trunc_f64_s())
                        else
                            throw(CompilationError(types, "invalid fptosi $inst"))
                        end
                    elseif typ == Int64
                        if argtype == Float32
                            push!(exprs[bidx], i64_trunc_f32_s())
                        elseif argtype == Float64
                            push!(exprs[bidx], i64_trunc_f64_s())
                        else
                            throw(CompilationError(types, "invalid fptosi $inst"))
                        end
                    else
                        throw(CompilationError(types, "invalid fptosi $inst"))
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
                            throw(CompilationError(types, "unsupported conversion $inst"))
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
                            throw(CompilationError(types, "unsupported conversion $inst"))
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
                        throw(CompilationError(types, "invalid neg_int $inst"))
                    end
                    push!(exprs[bidx], local_set(getlocal!(ssa)))
                    continue
                elseif f === Core.Intrinsics.have_fma
                    push!(exprs[bidx], i32_const(0), local_set(getlocal!(ssa)))
                    continue
                elseif f === Base.flipsign_int
                    argtype = irtype(inst.args[2])
                    push!(exprs[bidx], emit_val(inst.args[3]))
                    if argtype == i32
                        push!(
                            exprs[bidx],
                            i32_const(0),
                            emit_val(inst.args[2]),
                            i32_sub(),
                            emit_val(inst.args[2]),
                            i32_const(0),
                            i32_ge_u(),
                            select(),
                            local_set(getlocal!(ssa)),
                        )
                    elseif argtype == i64
                        push!(
                            exprs[bidx],
                            i64_const(0),
                            emit_val(inst.args[2]),
                            i64_sub(),
                            emit_val(inst.args[2]),
                            i64_const(0),
                            i64_ge_u(),
                            select(),
                            local_set(getlocal!(ssa)),
                        )
                    else
                        throw(CompilationError(types, "invalid flipsign_int $inst"))
                    end
                    continue
                elseif f === Base.arrayref
                    idxtype = irtype(inst.args[4])
                    idxtype in (i32, i64) || throw(CompilationError(types, "invalid index type $idxtype in $inst"))
                    push!(
                        exprs[bidx],
                        emit_val(inst.args[3]),
                        emit_val(inst.args[4]),
                        idxtype == i32 ? nop() : i32_wrap_i64(),
                        array_get(5 - 1),
                        local_set(getlocal!(ssa)),
                    )
                    continue
                elseif f === Base.getfield
                    field = inst.args[3]
                    typ = jltype(inst.args[2])
                    fieldidx = if field isa QuoteNode && field.value isa Symbol
                        field = field.value
                        argtype = irtype(inst.args[2])
                        findfirst(==(field), fieldnames(typ))
                    elseif field isa Int
                        field
                    else
                        throw(CompilationError(types, "invalid getfield $inst"))
                    end
                    structidx = struct_idx!(ctx, typ)
                    push!(
                        exprs[bidx],
                        emit_val(inst.args[2]),
                        ref_cast(structidx),
                        struct_get(structidx, fieldidx),
                        local_set(getlocal!(ssa)),
                    )
                    continue
                end
                for arg in inst.args[begin+1:end]
                    if arg isa GlobalRef && isconst(arg)
                        arg = getproperty(arg.mod, arg.name) 
                    end
                    if arg isa DataType
                        push!(exprs[bidx], global_get(emit_datatype!(ctx, arg)))
                        continue
                    end
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
                            throw(CompilationError(types, "unsupported call $inst"))
                        end
                    else
                        if all(arg -> irtype(arg) == f32, inst.args[begin+1:end])
                            intr.s32 
                        elseif all(arg -> irtype(arg) == f64, inst.args[begin+1:end])
                            intr.s64
                        else
                            throw(CompilationError(types, "unsupported call $inst"))
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
                        throw(CompilationError(types, "invalid not_int"))
                    end
                elseif f === Base.muladd_float || f === Base.fma_float
                    if all(arg -> irtype(arg) == f32, inst.args[begin+1:end])
                        push!(exprs[bidx], f32_mul(), f32_add())
                    elseif all(arg -> irtype(arg) == f64, inst.args[begin+1:end])
                        push!(exprs[bidx], f64_mul(), f64_add())
                    else
                        throw(CompilationError(types, "invalid muladd_float"))
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
                        throw(CompilationError(types, "invalid sub_int"))
                    end
                elseif f === Base.ashr_int || f === Base.lshr_int
                    argtype = irtype(inst.args[2])
                    if argtype == i32
                        offtype = irtype(inst.args[3])
                        if offtype == i64
                            push!(exprs[bidx], i32_wrap_i64())
                        end
                        push!(exprs[bidx], f === Base.ashr_int ? i32_shr_s() : i32_shr_u())
                    elseif argtype == i64
                        push!(exprs[bidx], f === Base.ashr_int ? i64_shr_s() : i64_shr_u())
                    else
                        throw(CompilationError(types, "invalid shl_int $inst"))
                    end
                elseif f === Core.tuple
                    push!(exprs[bidx], array_new(1))
                    push!(exprs[bidx], local_set(getlocal!(ssa)))
                elseif f === Base.shl_int
                    argtype = irtype(inst.args[2])
                    if argtype == i32
                        offtype = irtype(inst.args[3])
                        if offtype == i64
                            push!(exprs[bidx], i32_wrap_i64())
                        end
                        push!(exprs[bidx], i32_shl())
                    elseif argtype == i64
                        push!(exprs[bidx], i64_shl())
                    else
                        throw(CompilationError(types, "invalid shl_int $inst"))
                    end
                elseif f === Core.convert
                    push!(exprs[bidx], drop(), drop())
                    push!(exprs[bidx], emit_val(inst.args[3]))
                elseif f === Base.isa
                    typ = resolve_arg(inst, 3)
                    push!(exprs[bidx], global_get(emit_datatype!(ctx, typ)))
                    push!(exprs[bidx], call(jl_isa))
                elseif f === Core.throw
                    push!(exprs[bidx], emit_val(inst.args[2]))
                    push!(exprs[bidx], drop(), throw_(0))
                    continue
                else
                    throw(CompilationError(types, "Cannot handle call to $f @ $inst"))
                end

                loc = getlocal!(ssa)
                push!(exprs[bidx], local_set(loc))
            elseif inst isa PiNode
                loc = getlocal!(ssa)
                push!(exprs[bidx], emit_val(inst.val))
                push!(exprs[bidx], local_set(loc))
            elseif inst isa GotoIfNot 
                block_terminator = emit_val(inst.cond)
            elseif inst isa ReturnNode
                if !isdefined(inst, :val)
                    continue
                end
                rt_type = isnumeric(rt) ? valtype(rt) : jl_value_t
                push!(
                    exprs[bidx],
                    emit_val(inst.val),
                )
                convert_val!(exprs[bidx], irtype(inst.val), rt_type)
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
                mi = inst.args[1]
                haskey(ctx.func_dict, mi.specTypes) || emit_func!(ctx, mi.specTypes)
                funcidx = ctx.func_dict[mi.specTypes]
                push!(exprs[bidx], call(funcidx))
                typ = jltype(ssa)
                typ <: Union{} && (push!(exprs[bidx], drop(), unreachable()); continue)
                loc = getlocal!(ssa)
                push!(exprs[bidx], local_set(loc))
            elseif isnothing(inst)
                push!(exprs[bidx], nop())
            elseif inst isa GlobalRef
                push!(emit_val(inst), local_set(getlocal!(ssa)))
            elseif Meta.isexpr(inst, :new)
                typ = resolve_arg(inst, 1)
                push!(exprs[bidx], global_get(emit_datatype!(ctx, typ)))
                for arg in inst.args[begin+1:end]
                    push!(exprs[bidx], emit_val(arg))
                end
                structidx = struct_idx!(ctx, typ)
                push!(
                    exprs[bidx],
                    struct_new(structidx), # $jl-XXX
                    ref_cast(1), # $jl-value-t
                    local_set(getlocal!(ssa)),
                )
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

        !isnothing(block_terminator) && push!(exprs[bidx], block_terminator)
    end

    exprs, locals
end

struct CompilationError <: Exception
    types
    msg
end

function Base.showerror(io::IO, err::CompilationError)
    println(io, "compiling ")
    Base.show_tuple_as_call(io, :f, err.types)
    print(io, ": ", err.msg)
end

emit_func(f, types; optimize=false, debug=false) = emit_func!(CodegenContext(; optimize, debug), f, types)
emit_func!(mod::WModule, f, types; kwargs...) = emit_func!(CodegenContext(mod; kwargs...), f, types)
emit_func!(ctx, f, types) = emit_func!(ctx, Tuple{typeof(f), types.parameters...})

function emit_func!(ctx, types)
    ircodes = Base.code_ircode_by_type(types)
    if isempty(ircodes)
        error("could not find methods for type $types")
    elseif length(ircodes) >= 2
        error("types $types is ambiguous")
    end
    ir, rt = ircodes |> only

    num_func_imports = count(imp -> imp isa FuncImport, ctx.mod.imports)
    func_idx = ctx.func_dict[types] = num_func_imports + length(ctx.func_dict) + 1

    nargs = length(types.parameters) - 1
    exprs, locals = try
        emit_codes(ctx, ir, rt, nargs)
    catch err
        err isa CompilationError && rethrow()
        # display(ir)
        throw(CompilationError(types, err))
    end

    relooper = Relooper(exprs, ir)

    @debug "relooping" c = sprint(Base.show_tuple_as_call, :ok, types)
    content = reloop!(relooper)
    expr = Inst[
        content,
        unreachable(), # CF will go through a ReturnNode
    ]

    functype = FuncType(
        locals[begin:nargs],
        rt <: Union{} ?
          [] :
          [isnumeric(rt) ? valtype(rt) : StructRef(false, struct_idx!(ctx, rt))],
    )

    t = types.parameters[1]
    name = isdefined(t, :instance) ? nameof(t.instance) |> string : sprint(show, t)
    name = replace(name, "#" => "_")
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

    f = if ctx.optimize
        f |>
            make_tees! |>
            remove_unused! |>
            remove_nops! |>
            merge_blocks! |>
            remove_return!
    else
        f
    end

    pushfirst!(ctx.mod.funcs, f)
    if !isnothing(ctx.mod.start) && ctx.mod.start > num_func_imports
        if func_idx == ctx.mod.start
            push!(ctx.mod.exports, FuncExport(name, func_idx))
        end
        ctx.mod.start += 1
    end

    f
end
