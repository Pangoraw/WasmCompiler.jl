@enum ValType i32 i64 f32 f64 v128 funcref externref

valtype(::Type{Bool}) = i32

valtype(::Type{Int32}) = i32
valtype(::Type{Int64}) = i64
valtype(::Type{Float32}) = f32
valtype(::Type{Float64}) = f64

const Index = UInt32

abstract type Inst end

abstract type WasmType end

struct FuncType <: WasmType
    params::Vector{ValType}
    results::Vector{ValType}
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

struct Block <: Inst
    fntype::FuncType
    inst::Vector{Inst}
end

struct If <: Inst
    fntype::FuncType
    trueinst::Vector{Inst}
    falseinst::Vector{Inst}
end

struct Loop <: Inst
    fntype::FuncType
    inst::Vector{Inst}
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

struct i32_wrap_i64 <: Inst end

struct drop <: Inst end
struct select <: Inst
    valtype::Union{ValType,Nothing}
end
select() = select(nothing)

struct nop <: Inst end
struct unreachable <: Inst end
struct br <: Inst
    label::Int
end
struct br_if <: Inst
    label::Inst
end
struct br_table <: Inst
    labels::Vector{Int}
    default::Index
end

struct return_ <: Inst end

struct call <: Inst
    func::Index
end

struct call_indirect <: Inst
    tableidx::Index
    typeidx::Index
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

function Base.map!(f, cont::Union{Func,Block,If,Loop})
    map!(f, cont.inst, cont.inst)
    cont
end
Base.map(f, cont::Union{Func,Block,If,Loop}) = (map(f, cont.inst); cont) 
Base.foreach(f, cont::Union{Func,Block,If,Loop}) = foreach(f, cont.inst)

function _printwasm(io::IO, mod::WModule)
    println(io, "(module")
    indent = 2

    for type in mod.types
        print(io, "  "^indent, "(type)")
        println(io)
    end

    ctx = IOContext(io, :indent => indent)
    for func in mod.funcs
        _printwasm(ctx, func)
        println(io)
        println(io)
    end

    if !isnothing(mod.start)
        print(io, "  "^indent, "(start ")
        func = mod.funcs[mod.start]
        if !isnothing(func.name)
            print(io, "\$", func.name)
        else
            print(io, mod.start - 1)
        end
        println(io, ")")
    end

    for imp in mod.imports
        print(io, "  "^indent, "(import \"$(imp.module_name)\" \"$(imp.name)\"")
        if imp isa FuncImport
            print(io, "(func \$", imp.id)
            _printwasm(io, imp.fntype)
            print(io, ")")
        else
            error("cannot handle import $imp")
        end
        println(io, ")")
    end

    println(io)
    for exp in mod.exports
        print(io, "  "^indent, "(export ")
        if exp isa FuncExport
            func = mod.funcs[exp.func]
            print(io, "\"$(exp.name)\" ", "(func ")
            if !isnothing(func.name)
                print(io, '$', func.name)
            else
                print(io, exp.func - 1)
            end
            print(io, ")")
        else
            error("cannot handle export $exp")
        end
        print(io, ")")
    end
    print(io, ")")
end

function _printwasm(io::IO, fntype::FuncType)
    for param in fntype.params
        print(io, "(param ", param, ") ")
    end
    for result in fntype.results
        print(io, "(result ", result, ") ")
    end
    println(io)
end

_printwasm(io::IO, ::return_) = print(io, "  "^get(io, :indent, 2), "return")

_printwasm(io::IO, ::i32_lt_s) = print(io, "  "^get(io, :indent, 2), "i32.lt_s")
_printwasm(io::IO, ::i32_lt_u) = print(io, "  "^get(io, :indent, 2), "i32.lt_u")
_printwasm(io::IO, ::i64_lt_s) = print(io, "  "^get(io, :indent, 2), "i64.lt_s")
_printwasm(io::IO, ::i64_lt_u) = print(io, "  "^get(io, :indent, 2), "i64.lt_u")
_printwasm(io::IO, ::i32_le_s) = print(io, "  "^get(io, :indent, 2), "i32.le_s")
_printwasm(io::IO, ::i32_le_u) = print(io, "  "^get(io, :indent, 2), "i32.le_u")
_printwasm(io::IO, ::i64_le_s) = print(io, "  "^get(io, :indent, 2), "i64.le_s")
_printwasm(io::IO, ::i64_le_u) = print(io, "  "^get(io, :indent, 2), "i64.le_u")

_printwasm(io::IO, ::i64_extend_i32_s) = print(io, "  "^get(io, :indent, 2), "i64.extend_i32_s")
_printwasm(io::IO, ::i64_extend_i32_u) = print(io, "  "^get(io, :indent, 2), "i64.extend_i32_u")

function _printwasm(io::IO, inst::Inst)
    name = string(nameof(typeof(inst)))
    print(io, "  "^get(io, :indent, 2), replace(name, "_" => "."))
    for f in fieldnames(typeof(inst))
        print(io, " ", getfield(inst, f))
    end
end

function _printwasm(io::IO, f::Func)
    indent = get(io, :indent, 0)::Int
    print(io, "  "^indent, "(func ")
    if !isnothing(f.name)
        print(io, '$', f.name, " ")
    end
    _printwasm(io, f.fntype)
    for loc in Iterators.drop(f.locals, length(f.fntype.params))
        println(io, "  "^(indent+2), "(local ", loc, ")")
    end
    ctx = IOContext(io, :indent => indent + 2)
    _printwasm(ctx, f.inst)
    print(io, "  "^indent, ")")
end

function _printwasm(io::IO, loop::Loop)
    indent = get(io, :indent, 2)
    print(io, "  "^indent, "loop ")
    _printwasm(io, loop.fntype)
    ctx = IOContext(io, :indent => indent + 2)
    _printwasm(ctx, loop.inst)
    print(io, "  "^indent, "end")
end

function _printwasm(io::IO, i::If)
    indent = get(io, :indent, 2)
    print(io, "  "^indent, "if ")
    _printwasm(io, i.fntype)
    ctx = IOContext(io, :indent => indent + 2)
    _printwasm(ctx, i.trueinst)
    println(io, "  "^indent, "else")
    _printwasm(ctx, i.falseinst)
    print(io, "  "^indent, "end")
end

function _printwasm(io::IO, expr::Vector{Inst})
    ctx = IOContext(io, :indent => get(io, :indent, 0))
    for inst in expr
        _printwasm(ctx, inst)
        println(io)
    end
end

function _printwasm(io::IO, b::Block)
    indent = get(io, :indent, 2)
    print(io, "  "^indent, "block ")
    _printwasm(io, b.fntype)
    ctx = IOContext(io, :indent => indent + 2)
    _printwasm(ctx, b.inst)
    print(io, "  "^indent, "end")
end

printw(mod::WModule) = _printwasm(stdout, mod)

ft = FuncType([i32, i32], [i32])
f = Func(
    "add",
    ft,
    ValType[],
    [
        local_get(0),
        local_get(1),
        If(
            FuncType([], [i32]),
            [i32_const(32)],
            [i32_const(0)],
        ),
        i32_add(),
    ])

mod = WModule(
    [], [f],
    [], [], [], [], [],
    nothing,
    [], [FuncExport("add", 1)],
)

printw(mod)

struct Relooper
    # We already have emitted the code for each block content
    exprs::Vector{Vector{Inst}}
    ir::Core.Compiler.IRCode
    idoms::Vector{Int}
    order::Vector{Int}
end

Relooper(ir::Core.Compiler.IRCode) =
    Relooper(
        [[i32_const(i)] for i in 1:length(ir.cfg.blocks)],
        ir, Core.Compiler.naive_idoms(ir.cfg.blocks, false),
        reverse_postorder(ir.cfg.blocks),
    )

function reverse_postorder(blocks)
    dfs = Core.Compiler.DFS(blocks, false)
    reverse(dfs.to_post) # is this correct?
end

function reloop!(relooper, bidx=1)
    (; ir, idoms) = relooper
    (; cfg) = ir
    (; stmts, preds, succs) = cfg.blocks[bidx]

    hasbackedge = any(>=(bidx), preds)
    toplace = findall(==(bidx), idoms)
    toplace = sort(collect(toplace), by=b -> relooper.order[b])

    # @info "Placing $bidx" toplace

    if length(toplace) == 0
        if length(succs) == 0
            return
        end

        @assert length(succs) == 1 "not supported $succs"
        push!(
            relooper.exprs[bidx],
            br(0),
        )
    elseif length(toplace) == 1
        type = hasbackedge ? Loop : Block
        toplace = only(toplace)
        push!(
            relooper.exprs[bidx],
            type(voidtype, relooper.exprs[toplace]),
        )
        reloop!(relooper, toplace)
    elseif length(succs) == 2 && !hasbackedge
        gotoifnot = ir.stmts[stmts.stop][:inst]
        if gotoifnot isa Core.GotoNode
            gotoifnot = ir.stmts[stmts.stop - 1][:inst]::Core.GotoIfNot
        end
        falsedest = gotoifnot.dest
        truedest = setdiff(succs, falsedest) |> only

        push!(
            relooper.exprs[bidx],
            If(voidtype,
               relooper.exprs[truedest],
               relooper.exprs[falsedest],
            )
        )

        for b in toplace
            b ∈ (truedest, falsedest) && continue
            push!(
                relooper.exprs[bidx],
                Block(
                    voidtype,
                    relooper.exprs[b],
                )
            )
        end

        for b in toplace
            reloop!(relooper, b)
        end
    elseif length(succs) == 2 && hasbackedge
        gotoifnot = ir.stmts[stmts.stop][:inst]
        if gotoifnot isa Core.GotoNode
            gotoifnot = ir.stmts[stmts.stop - 1][:inst]::Core.GotoIfNot
        end
        falsedest = gotoifnot.dest
        truedest = setdiff(succs, falsedest) |> only

        push!(
            relooper.exprs[bidx],
            Loop(voidtype,
                Inst[
                    If(voidtype,
                        relooper.exprs[truedest],
                        relooper.exprs[falsedest],
                    )
                ]
            )
        )
    
        for b in toplace
            reloop!(relooper, b)
        end
    else
        error("cannot place block $bidx (toplace = $(toplace))")
    end
end

function emit_codes(ir, nargs)
    exprs = Vector{Inst}[]

    # TODO: handle first arg properly
    locals = ValType[
        valtype(argtype)
        for argtype in ir.argtypes[begin+1:begin+nargs]
    ]

    ssa_to_local = fill(-1, length(ir.stmts))

    function irtype(val)
        jltype = if val isa Core.Argument
            ir.argtypes[val.n]
        elseif val isa Core.SSAValue
            ir.stmts[val.id][:type]
        else
            typeof(val)
        end
        valtype(jltype)
    end

    function emit_val(val)
        if val isa Core.Argument
            # One indexing + Skip first arg
            local_get(val.n - 2)
        elseif val isa Core.SSAValue
            loc = ssa_to_local[val.id]
            if loc == -1
                type = ir.stmts[val.id][:type]
                push!(locals, valtype(type))
                loc = ssa_to_local[val.id] = length(locals) - 1
            end
            local_get(loc)
        elseif val isa Int32
            i32_const(val)
        elseif val isa Int64
            i64_const(val)
        elseif val isa Float32
            f32_const(val)
        elseif val isa Float64
            f64_const(val)
        elseif isnothing(val)
            nop()
        else
            error("invalid value $val @ $(typeof(val))")
        end
    end

    for (bidx, b) in enumerate(ir.cfg.blocks)
        push!(exprs, Inst[])

        for sidx in b.stmts
            stmt = ir.stmts[sidx]
            inst = stmt[:inst]

            if Meta.isexpr(inst, :call)
                f = inst.args[1]
                if f isa GlobalRef && isconst(f)
                    f = getfield(f.mod, f.name)
                end
                if f === Base.sext_int
                    typ = inst.args[2]
                    if typ isa GlobalRef && isconst(typ)
                        typ = getfield(typ.mod, typ.name)
                    end
                    arg = inst.args[3]
                    push!(exprs[bidx], emit_val(arg))
                    if typ === Int64 && irtype(arg) == i32
                        push!(exprs[bidx], i64_extend_i32_s())
                        continue
                    else
                        throw("unsupported sext_int $(inst)")
                    end
                end
                for arg in inst.args[begin+1:end]
                    push!(exprs[bidx], emit_val(arg))
                end
                if f === Base.slt_int
                    if all(arg -> irtype(arg) == i32, inst.args[begin+1:end])
                        push!(exprs[bidx], i32_lt_s())
                    elseif all(arg -> irtype(arg) == i64, inst.args[begin+1:end])
                        push!(exprs[bidx], i64_lt_s())
                    else
                        error("invalid slt_int")
                    end
                elseif f === Base.sle_int
                    if all(arg -> irtype(arg) == i32, inst.args[begin+1:end])
                        push!(exprs[bidx], i32_le_s())
                    elseif all(arg -> irtype(arg) == i64, inst.args[begin+1:end])
                        push!(exprs[bidx], i64_le_s())
                    else
                        error("invalid sle_int")
                    end
                elseif f === Base.not_int
                    if all(arg -> irtype(arg) == i32, inst.args[begin+1:end])
                        push!(exprs[bidx], i32_const(-1), i32_xor())
                    elseif all(arg -> irtype(arg) == i64, inst.args[begin+1:end])
                        push!(exprs[bidx], i64_const(-1), i64_xor())
                    else
                        error("invalid not_int")
                    end
                elseif f === Base.sub_int
                    if all(arg -> irtype(arg) == i32, inst.args[begin+1:end])
                        push!(exprs[bidx], i32_sub())
                    elseif all(arg -> irtype(arg) == i64, inst.args[begin+1:end])
                        push!(exprs[bidx], i64_sub())
                    else
                        error("invalid sub_int")
                    end
                elseif f === Base.add_int
                    if all(arg -> irtype(arg) == i32, inst.args[begin+1:end])
                        push!(exprs[bidx], i32_add())
                    elseif all(arg -> irtype(arg) == i64, inst.args[begin+1:end])
                        push!(exprs[bidx], i64_add())
                    else
                        error("invalid sub_int")
                    end 
                elseif f === Base.mul_int
                    if all(arg -> irtype(arg) == i32, inst.args[begin+1:end])
                        push!(exprs[bidx], i32_mul())
                    elseif all(arg -> irtype(arg) == i64, inst.args[begin+1:end])
                        push!(exprs[bidx], i64_mul())
                    else
                        error("invalid sub_int")
                    end
                elseif f === Base.:(===)
                    if all(arg -> irtype(arg) == i32, inst.args[begin+1:end])
                        push!(exprs[bidx], i32_eq())
                    elseif all(arg -> irtype(arg) == i64, inst.args[begin+1:end])
                        push!(exprs[bidx], i64_eq())
                    else
                        error("invalid sub_int")
                    end
                else
                    error("Cannot handle call to $f")
                end

                loc = ssa_to_local[sidx]
                if loc == -1
                    push!(locals, valtype(stmt[:type]))
                    loc = ssa_to_local[sidx] = length(locals) - 1
                end

                push!(exprs[bidx], local_set(loc))
            elseif inst isa Core.GotoIfNot 
                push!(exprs[bidx], emit_val(inst.cond))
            elseif inst isa Core.ReturnNode
                push!(exprs[bidx], emit_val(inst.val))
                if bidx != length(ir.cfg.blocks)
                    push!(exprs[bidx], return_())
                end
            end

            # TODO: Emit phi nodes
        end
    end

    relooper = Relooper(
        exprs,
        ir,
        Core.Compiler.naive_idoms(ir.cfg.blocks, false),
        reverse_postorder(ir.cfg.blocks),
    )
    reloop!(relooper)

    # _printwasm(stdout, exprs[1])

    first(exprs), locals
end

function emit_func(f, types)
    ir, rt = Base.code_ircode(f, types) |> only

    nargs = length(types.parameters)
    expr, locals = emit_codes(ir, nargs)

    functype = FuncType(
        locals[begin:nargs],
        [valtype(rt)],
    )

    f = Func(
        nameof(f) |> string,
        functype,
        locals,
        expr,
    )

    # _printwasm(stdout, f)

    merge_blocks!(f)
end

function towasm(io::IO, mod; enable_gc=false, enable_reference_types=false)
    args = String[]
    enable_gc && push!(args, "--enable-gc")
    asargs = copy(args)
    enable_reference_types && push!(asargs, "--enable-reference-types")
    inputio = IOBuffer()
    _printwasm(inputio, mod)
    run(pipeline(inputio, `wat-desugar -f $(args...)`, `wasm-as -g -v web $(asargs...)`, io))
end
towasm(f::String, mod; kwargs...) = open(io -> towasm(io, mod; kwargs...), f; write=true)
