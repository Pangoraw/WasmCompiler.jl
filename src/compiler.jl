import Core.Compiler: widenconst, PiNode, PhiNode, ReturnNode,
    GotoIfNot, GotoNode, SSAValue, UpsilonNode, PhiCNode

struct CodegenContext
    mod::WModule
    type_dict::Dict
    func_dict::Dict
    optimize::Bool
    debug::Bool
end
CodegenContext(module_=WModule([], [], [], [], [], [], [], nothing, [], []); debug=false, optimize=false) =
    CodegenContext(module_, Dict(), Dict(), optimize, debug)

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
        getfield(arg.mod, arg.name) : arg
end

function emit_codes(ctx, ir, nargs)
    (; debug) = ctx
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

    function emit_boxed(typ) # from valtype(typ) -> (ref $jl-value-t)
        if typ == Int32
            call("jl-box-Int32")
        elseif typ == UInt32
            call("jl-box-UInt32")
        elseif typ == Int64
            call("jl-box-Int64")
        elseif typ == Float32
            call("jl-box-Float32")
        elseif typ == Float64
            call("jl-box-Float64")
        else
            nop()
        end
    end

    function emit_val(val)
        if val isa Core.SSAValue || val isa Core.Argument
            local_get(getlocal!(val))
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
                    if typ == Int32 && argtype == f32
                        push!(exprs[bidx], i32_reinterpret_f32())
                    elseif typ == Int64 && argtype == f64
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
                    if typ == Int32 && argtype == i64
                        push!(exprs[bidx], i32_wrap_i64())
                    elseif typ == Int32 && argtype == i32
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
                    if typ == Int64
                        if argtype == Int32 || argtype == Bool
                            push!(exprs[bidx], i64_extend_i32_s())
                        elseif argtype == UInt32
                            push!(exprs[bidx], i64_extend_i32_u())
                        else
                            throw(CompilationError(types, "invalid zext_int $inst"))
                        end
                    elseif typ == Int32
                        # pass
                    else
                        throw(CompilationError(types, "invalid zext_int $inst"))
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
                end
                for arg in inst.args[begin+1:end]
                    (arg isa DataType || arg isa GlobalRef) && error("invalid arg $inst") 
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
                elseif f === Base.muladd_float
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
                else
                    throw(CompilationError(types, "Cannot handle call to $f"))
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
                mi = inst.args[1]
                haskey(ctx.func_dict, mi.specTypes) || emit_func!(ctx, mi.specTypes)
                funcidx = ctx.func_dict[mi.specTypes]
                push!(exprs[bidx], call(funcidx - 1))
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

struct CompilationError <: Exception
    types
    msg
end

Base.showerror(io::IO, err::CompilationError) = println(io, "compiling $(err.types): $(err.msg)")

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

    ctx.func_dict[types] = length(ctx.func_dict) + 1

    nargs = length(types.parameters) - 1
    exprs, locals = try
        emit_codes(ctx, ir, nargs)
    catch err
        err isa CompilationError && rethrow()
        throw(CompilationError(types, err))
    end

    relooper = Relooper(exprs, ir)

    expr = Inst[
        reloop!(relooper),
        unreachable(), # CF will go through a ReturnNode
    ]

    functype = FuncType(
        locals[begin:nargs],
        [valtype(rt)],
    )

    t = types.parameters[1]
    name = replace(nameof(t.instance) |> string, "#" => "_")
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
        f |> make_tees! # |> remove_unused!
    else
        f
    end

    pushfirst!(ctx.mod.funcs, f)

    f
end
