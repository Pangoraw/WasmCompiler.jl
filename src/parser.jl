struct Token
    pos::Int
    val::Any
end

function lex(io::IO)
    tokens = Token[]

@label start

    while !eof(io)
        c = peek(io, Char)

        if c == '('
            read(io, Char)

            if peek(io, Char) == ';'
                level = 1

                p = position(io)

                read(io, Char)

                while !eof(io)
                    c = read(io, Char)
                    if c == '('
                        if peek(io, Char) == ';'
                            level += 1
                        end
                    elseif c == ';'
                        if peek(io, Char) == ')'
                            level -= 1

                            if level == 0
                                read(io, Char)
                                # push!(tokens, BlockComment())
                                @goto start
                            end
                        end
                    end
                end

                seek(io, p)
            end

            push!(tokens, Token(position(io), '('))
        elseif symstart(c)
            pos = position(io)
            sym = read_sym(io)
            tok = if sym === :nan && peek(io, Char) == ':'
                read(io, Char)
                N = read_number(io)
                N = if N <= typemax(UInt32)
                    reinterpret(Float32,
                                reinterpret(UInt32, NaN32) | UInt32(N))
                elseif N <= typemax(UInt64)
                    reinterpret(Float64,
                                reinterpret(UInt64, NaN64) | UInt64(N))
                end
            else
                sym
            end
            push!(tokens, Token(pos, tok))
        elseif numberstart(c)
            push!(tokens, Token(position(io), read_number(io)))
        elseif c == '"'
            push!(tokens, Token(position(io), read_str(io)))
        elseif c == ')'
            read(io, Char)
            push!(tokens, Token(position(io), ')'))
        elseif c == '='
            read(io, Char)
            push!(tokens, Token(position(io), '='))
        elseif c == ';'
            read(io, Char)
            if peek(io, Char) == ';'
                while !eof(io)
                    c = read(io, UInt8)
                    if c == UInt8('\n') || c == UInt8('\r')
                        break
                    end
                end
                c = peek(io, Char)
            end
        elseif c == '\t' || c == ' ' || c == '\n' || c == '\r'
            read(io, Char)
        else
            # println(String(read(io)))
            error("invalid char $c $((UInt32(c))) at $(position(io))")
        end
    end

    tokens
end

struct FuncContext
    mod::Union{Module,Nothing}
    named_globals::Dict{Symbol,Int}
    named_functions::Dict{Symbol,Int}
    named_types::Dict{Symbol,Int}
    named_locals::Dict{Symbol,Int}
    labels::Vector{Union{Nothing,Symbol}}
end
FuncContext(mod=nothing) = FuncContext(mod, Dict{Symbol,Int}(), Dict{Symbol,Int}(), Dict{Symbol,Int}(), Dict{Symbol,Int}(), Symbol[])

symstart(c) = validchar(c) && c ∉ '0':'9' && c != '-'
validchar(c) = c ∈ (('a':'z') ∪ ('A':'Z') ∪ ('0':'9')) || c == '_' || c == '.' || c == '$' || c == '-' || c == '!' || c == '>'
numberstart(c) = c ∈ '0':'9' || c == '-'

function read_sym(io::IO)
    s = ""
    c = peek(io, Char)
    while !eof(io) && validchar(c)
        s *= read(io, Char)
        c = peek(io, Char)
    end
    Symbol(s)
end

function read_str(io::IO)
    vec = UInt8[]
    @assert read(io, UInt8) == UInt8('"')
    c = peek(io, UInt8)
    while !eof(io) && c != UInt8('"')
        read(io, UInt8)

        if c == UInt8('\\')
            eof(io) && break
            c = read(io,UInt8)
            if c in UInt8('0'):UInt8('f')
                eof(io) && break
                cs = string(Char(c), Char(read(io, UInt8)))
                c = parse(UInt8, cs; base=16)
            end
        end
        push!(vec, c)

        c = peek(io, UInt8)
    end
    !eof(io) && read(io, UInt8)
    String(vec)
end

function read_number(io::IO)
    neg = peek(io, Char) == '-'
    neg && read(io, Char)

    s = ""

    nan = false
    if peek(io, Char) == 'n' && read_sym(io) === :nan && read(io, Char) == ':'
        nan = true
    end

    base = 10
    if peek(io, Char) == '0'
        s *= read(io, Char)
        if peek(io, Char) == 'x'
            s *= read(io, Char)
            base = 16
        elseif peek(io, Char) == 'b'
            s *= read(io, Char)
            base = 2
        end
    end

    decimal = 0

    n = 0
    c = peek(io, Char)
    while !eof(io) && (numberstart(c) || (base == 16 && (c ∈ ('a':'f') ∪ ('A':'F'))) || c == '.' || c == '_')
        s *= c
        if c == '.'
            decimal > 0 && error("invalid token '.' at $(position(io))")
            decimal = 1
            read(io, Char)
            c = peek(io, Char)
            continue
        elseif c == '_'
            read(io, Char)
            c = peek(io, Char)
            continue
        end
        d = parse(Int,c;base)
        if decimal > 0
            n = n + d * Float64(base)^-decimal
            decimal += 1
        else
            n = base * n + d
        end
        read(io, Char)
        eof(io) && break
        c = peek(io, Char)
    end

    if peek(io, Char) == 'p'
        s *= read(io, Char)
        peek(io, Char) in ('+', '-') || error("invalid number $s")
        s *= read(io, Char)
        c = peek(io, Char)
        while c in '0':'9'
            s *= read(io, Char)
            c = peek(io, Char)
        end
        return parse(Float64, s)
    end
    
    n = nan ? reinterpret(float(typeof(n)), n) : n
    neg ? -n : n
end

function parse_sexpr(tokens, level=0)
    result = []

    while !isempty(tokens)
        (; pos, val) = popfirst!(tokens)

        if val === '('
            push!(result, parse_sexpr(tokens, level + 1))
        elseif val === ')'
            if level == 0
                error("invalid closing paren at 0x$(string(pos; base=16))")
            end
            return result
        else
            push!(result, val)
        end
    end

    level == 0 || error("invalid expression")

    result
end

issexpr(val::Vector{Any}, head) = length(val) >= 1 && first(val) === head
issexpr(_, _) = false

function parse_valtype(ctx, ex)
    if ex === :i32
        i32
    elseif ex === :i64
        i64
    elseif ex === :f32
        f32
    elseif ex === :f64
        f64
    elseif ex === :v128
        v128
    elseif ex === :externref
        ExternRef(false)
    elseif ex === :eqref
        EqRef(false)
    elseif ex isa Vector && first(ex) === :ref
        popfirst!(ex)
        s = popfirst!(ex)
        null = s === :null

        # resolve type here
        t = _resolve_type(ctx, isempty(ex) ? s : popfirst!(ex))
        StructRef(null,t)
    else
        error("unknown valtype $(ex)")
    end
end

function parse_functype!(args, ctx)
    local fntype = nothing

    if length(args) >= 1 && issexpr(first(args), :type)
        fntype = ctx.mod.types[_resolve_type(ctx, last(popfirst!(args)))]
    end

    params = ValType[]
    while length(args) >= 1 && issexpr(first(args), :param)
        _, p... = popfirst!(args)

        valtypes = if length(p) > 0 && first(p) isa Symbol && startswith(string(first(p)), '$')
            locname = first(p)
            ctx.named_locals[locname] = length(params) + 1
            @assert length(p) == 2
            ValType[parse_valtype(ctx, last(p))]
        else
            parse_valtype.((ctx,), p)
        end

        append!(params, valtypes)
    end

    results = ValType[]
    while length(args) >= 1 && issexpr(first(args), :result)
        _, p... = popfirst!(args)
        append!(results, parse_valtype.((ctx,), p))
    end

    newfntype = FuncType(params, results)

    if fntype !== nothing
        if isempty(params) && isempty(results)
            return fntype
        else
            @assert newfntype == fntype
        end
    end

    return newfntype
end

function parse_struct_type!(args, ctx)
    local structtype = StructType(nothing, nothing, [])

    if !isempty(args)
        if issexpr(first(args), :sub)
            args = only(args); popfirst!(args)
            sub = popfirst!(args)
        else
            sub = nothing
        end

        while !isempty(args)
            popfirst!(args)
            isempty(args) && break
            f = popfirst!(args)
            if !issexpr(f, :field)
                error("invalid struct")
            end
            popfirst!(f)

            while !isempty(f)
                mut = false
                if first(f) === :mut
                    popfirst!(f)
                    mut = true
                elseif first(f) isa Symbol && startswith(string(first(f)), "\$")
                    name = popfirst!(f)
                end
                t = parse_valtype(ctx, popfirst!(f))
                push!(structtype.fields, StructField(t, nothing, mut))
            end
        end
    end

    return structtype
end

_resolve_local(ctx, idx) = idx isa Int ? idx + 1 : ctx.named_locals[idx]
_resolve_label(ctx, idx) = idx isa Int ? idx : length(ctx.labels)-findlast(==(idx), ctx.labels)
_resolve_func(ctx, idx) = get(() -> idx+1, ctx.named_functions, idx)
_resolve_type(ctx, idx) = get(() -> idx+1, ctx.named_types, idx)
_resolve_global(ctx, idx) = get(() -> idx + 1, ctx.named_globals, idx)

# for wat
function make_inst_linear!(inst, args, ctx)

    while !isempty(args)
        if first(args) in (:end, :else)
            break
        end
        
        head = popfirst!(args)
        head =  replace(string(head), '.' => '_') |> Symbol

        if head === :i32_const
            val = popfirst!(args)
    
            if val > typemax(Int32)
                val = reinterpret(Int32, UInt32(val))
            end
    
            push!(inst, i32_const(val))
        elseif head === :i64_const
            push!(inst, i64_const(popfirst!(args)))
        elseif head === :local_set
            push!(inst, local_set(_resolve_local(ctx, popfirst!(args))))
        elseif head === :local_tee
            push!(inst, local_tee(_resolve_local(ctx, popfirst!(args))))
        elseif head === :local_get
            push!(inst, local_get(_resolve_local(ctx, popfirst!(args))))
        elseif head === :global_set
            push!(inst, global_set(_resolve_global(ctx, popfirst!(args))))
        elseif head === :global_get
            push!(inst, global_get(_resolve_global(ctx, popfirst!(args))))
        elseif head === :call_indirect
            typ = _resolve_type(ctx, popfirst!(args)[end])
            push!(inst, call_indirect(typ))
        elseif head === :call
            func = _resolve_func(ctx, popfirst!(args))
            push!(inst, call(func))
        elseif head === :struct_new
            strty = _resolve_type(ctx, popfirst!(args))
            push!(inst, struct_new(strty))
        elseif head === :br_if || head === :br
            dest = _resolve_label(ctx, popfirst!(args))
            push!(inst, getproperty(WC, head)(_resolve_label(ctx, dest)))
        elseif head === :br_table
            labels = Int[]
    
            while first(args) isa Symbol || first(args) isa Int
                label = popfirst!(args)
                push!(
                    labels,
                    _resolve_label(ctx, label),
                )
            end
    
            default = pop!(labels)
            push!(inst, br_table(labels, default))

        elseif head === :return
            make_inst_linear!(inst, args, ctx)
            push!(inst, return_())
        elseif head === :if
            fntype = parse_functype!(args, ctx)
      
            trueinst = Inst[]
            make_inst_linear!(trueinst, args, ctx)

            terminator = popfirst!(args)
            
            falseinst = Inst[]
            if terminator === :else
                make_inst_linear!(falseinst, args, ctx)
                terminator = popfirst!(args)
            end
            @assert terminator === :end "invalid if"
    
            push!(inst, If(fntype, trueinst, falseinst))
        elseif head === :loop
            fntype = parse_functype!(args, ctx)
            newinst = Inst[]
            make_inst_linear!(newinst, args, ctx)
            @assert popfirst!(args) === :end "invalid loop"
            push!(inst, Loop(fntype, newinst))
        elseif head === :block
            fntype = parse_functype!(args, ctx)
            newinst = Inst[]
            make_inst_linear!(newinst, args, ctx)
            @assert popfirst!(args) === :end "invalid block"
            push!(inst, Block(fntype, newinst))
        else
            T = getproperty(WC, head)
            @assert T <: Inst "invalid head $head $T"
            push!(inst, T())
        end
    end
end

function parse_memarg!(args)
    align = 0 
    offset = 0

    if !isempty(args) && first(args) === :offset
        @assert length(args) >= 3
        @assert args[2] === '='
        popfirst!(args); popfirst!(args)

        offset = popfirst!(args)::Int
    end

    if !isempty(args) && first(args) === :align
        @assert length(args) >= 3
        @assert args[2] === '='
        popfirst!(args); popfirst!(args)

        align = popfirst!(args)::Int
    end

    MemArg(align, offset)
end

# for wast
function make_inst!(inst, ex, ctx)
    if ex isa Symbol
        head = ex
        args = []
    else
        if length(ex) == 0
            return
        end

        if first(ex) isa Vector{Any}
            while !isempty(ex)
                e = popfirst!(ex)
                make_inst!(inst, e, ctx)
            end
            return
        end

        head = popfirst!(ex)
        args = ex
        head = replace(string(head), '.' => '_') |> Symbol
    end

    if head === :i32_const
        val = popfirst!(args)

        if val > typemax(Int32)
            val = reinterpret(Int32, UInt32(val))
        end

        push!(inst, i32_const(val))
    elseif head === :i64_const
        val = popfirst!(args)
        make_inst!(inst, args, ctx)
        push!(inst, i64_const(val))
    elseif head === :f32_const
        val = popfirst!(args)
        make_inst!(inst, args, ctx)
        push!(inst, f32_const(val))
    elseif head === :f64_const
        val = popfirst!(args)
        make_inst!(inst, args, ctx)
        push!(inst, f64_const(val))
    elseif head === :v128_const
        typ = popfirst!(args)
        val = if typ === :i32x4
            if !(length(args) >= 4 && all(x->isa(x,Integer), first(args, 4)))
                error("invalid v128.const")
            end
            i32x4(
              popfirst!(args),
              popfirst!(args),
              popfirst!(args),
              popfirst!(args),
            )
        else
            error("invalid token $typ in v128.const")
        end
        make_inst!(inst, args, ctx)
        push!(inst, v128_const(val))
    elseif head === :local_set
        loc = _resolve_local(ctx, popfirst!(args))
        make_inst!(inst, args, ctx)
        push!(inst, local_set(loc))
    elseif head === :local_tee
        loc = _resolve_local(ctx, popfirst!(args))
        make_inst!(inst, args, ctx)
        push!(inst, local_tee(loc))
    elseif head === :local_get
        loc = _resolve_local(ctx, popfirst!(args))
        make_inst!(inst, args, ctx)
        push!(inst, local_get(loc))
    elseif head === :global_set
        global_ = _resolve_global(ctx, popfirst!(args))
        make_inst!(inst, args, ctx)
        push!(inst, global_set(global_))
    elseif head === :global_get
        global_ = _resolve_global(ctx, popfirst!(args))
        make_inst!(inst, args, ctx)
        push!(inst, global_get(global_))
    elseif head in [:i32_load, :i64_load, :f32_load, :f64_load,
                    :i32_load8_s, :i32_load8_u, :i32_load16_s, :i32_load16_u,
                    :i64_load8_s, :i64_load8_u, :i64_load16_s, :i64_load16_u,
                    :i64_load32_s, :i64_load32_u,
                    :i32_store, :i64_store, :f32_store, :f64_store,
                    :i32_store8, :i32_store16, :i64_store8, :i64_store16, :i64_store32]
        memarg = parse_memarg!(args)
        make_inst!(inst, args, ctx)
        push!(inst, getproperty(WC, head)(memarg))
    elseif head in [:i8x16_add, :i16x8_add, :i32x4_add, :i64x2_add, :f32x4_add, :f64x2_add,
                    :i8x16_sub, :i16x8_sub, :i32x4_sub, :i64x2_sub, :f32x4_sub, :f64x2_sub,
                    :i8x16_mul, :i16x8_mul, :i32x4_mul, :i64x2_mul, :f32x4_mul, :f64x2_mul,
                    :i8x16_div, :i16x8_div, :i32x4_div, :i64x2_div, :f32x4_div, :f64x2_div]
        shead = string(head)
        lane = getproperty(Lanes, Symbol(first(split(shead, "x"))))
        op = getproperty(MathOperators, Symbol(last(split(shead, "_"))))
        make_inst!(inst, args, ctx)
        push!(inst, v128bin(lane, op))
    elseif head === :call_indirect
        typ = _resolve_type(ctx, popfirst!(args)[end])
        make_inst!(inst, args, ctx)
        push!(inst, call_indirect(typ))
    elseif head === :call
        func = _resolve_func(ctx, popfirst!(args))
        make_inst!(inst, args, ctx)
        push!(inst, call(func))
    elseif head === :struct_new
        strty = _resolve_type(ctx, popfirst!(args))
        make_inst!(inst, args, ctx)
        push!(inst, struct_new(strty))
    elseif head === :struct_get
        strty = _resolve_type(ctx, popfirst!(args))
        fieldidx = popfirst!(args)
        make_inst!(inst, args, ctx)
        push!(inst, struct_get(strty, fieldidx + 1))
    elseif head === :br_if || head === :br
        dest = _resolve_label(ctx, popfirst!(args))
        make_inst!(inst, args, ctx)
        push!(inst, getproperty(WC, head)(dest))
    elseif head === :br_table
        labels = Int[]

        while !isempty(args) && ((first(args) isa Symbol && first(args) !== :end) || first(args) isa Int)
            label = popfirst!(args)
            push!(
                labels,
                _resolve_label(ctx, label),
            )
        end

        default = pop!(labels)
        make_inst!(inst, args, ctx)
        push!(inst, br_table(labels, default))
    elseif head === :return
        make_inst!(inst, args, ctx)
        push!(inst, return_())
    elseif head === :if
        if length(args) >= 1 && first(args) isa Symbol && startswith(string(first(args)), '$')
            push!(ctx.labels, popfirst!(args))
        else
            push!(ctx.labels, nothing)
        end

        fntype = parse_functype!(args, ctx)
        while length(args) >= 1 && (!issexpr(first(args), :then) && !issexpr(first(args), :else))
            # Cond
            make_inst!(inst, popfirst!(args), ctx)
        end

        trueinst = Inst[]
        if !issexpr(first(args), :then)
            error("invalid if (nodo)")
        end
        make_inst!(trueinst, popfirst!(args)[begin+1:end], ctx)

        falseinst = Inst[]
        if !isempty(args)
            if !issexpr(first(args), :else)
                error("invalid if (nofalse)")
            end
            make_inst!(falseinst, popfirst!(args)[begin+1:end], ctx)
        end

        pop!(ctx.labels)
        push!(inst, If(fntype, trueinst, falseinst))
    elseif head === :loop
        if length(args) >= 1 && first(args) isa Symbol && startswith(string(first(args)), '$')
            push!(ctx.labels, popfirst!(args))
        else
            push!(ctx.labels, nothing)
        end
        fntype = parse_functype!(args, ctx)
        newinst = Inst[]
        make_inst!(newinst, args, ctx)
        pop!(ctx.labels)
        push!(inst, Loop(fntype, newinst))
    elseif head === :block
        if length(args) >= 1 && first(args) isa Symbol && startswith(string(first(args)), '$')
            push!(ctx.labels, popfirst!(args))
        else
            push!(ctx.labels, nothing)
        end
        fntype = parse_functype!(args, ctx)
        newinst = Inst[]
        make_inst!(newinst, args, ctx)
        pop!(ctx.labels)
        push!(inst, Block(fntype, newinst))
    elseif head === :ref_extern
        push!(inst, i32_const(popfirst!(args)))
    elseif isdefined(WC, head) && getproperty(WC, head) isa Type && getproperty(WC, head) <: Inst
        make_inst!(inst, args, ctx)
        push!(inst, getproperty(WC, head)())
    else
        if startswith(string(head), '$')
            error("invalid label $head")
        end
        error("invalid head $head")
    end
end

function make_module!(mod, exprs)
    func_exprs = Any[]
    export_exprs = Any[]
    imports_exprs = Any[]

    num_function_imports = 0

    named_functions = Dict{Symbol,Int}()
    named_globals = Dict{Symbol,Int}()
    named_types = Dict{Symbol,Int}()

    for ex in exprs
        if !(ex isa Vector{Any})
            error("invalid token $ex")
        end

        head, args... = ex
        if head === :func
            name = length(args) >= 1 && first(args) isa Symbol && startswith(string(args[1]), '$') ?
                first(args) : nothing

            push!(func_exprs, ex)
            if name !== nothing
                named_functions[name] = length(func_exprs) + num_function_imports
            end
        elseif head === :global
            name = length(args) >= 1 && first(args) isa Symbol && startswith(string(first(args)), '$') ?
                string(popfirst!(args))[begin+1:end] : nothing

            if isempty(args)
                error("invalid global")
            end

            ctx = FuncContext(mod)
            mut, type = if first(args) isa Vector{Any} && first(args)[1] === :mut
                _, a = popfirst!(args)
                true, parse_valtype(ctx, a)
            else
                false, parse_valtype(ctx, popfirst!(args))
            end

            inst = Inst[]
            make_inst!(inst, args, ctx)

            push!(mod.globals, Global(name, GlobalType(mut, type), inst))
            named_globals[Symbol('$', name)] = length(mod.globals)
        elseif head === :memory
            if !isempty(args) && issexpr(first(args), :data)
                min = 1
                max = typemax(UInt32)
                push!(mod.datas, Data(Vector{UInt8}(last(popfirst!(args))), DataModePassive()))
            else
                min = popfirst!(args)
                max = isempty(args) ? typemax(UInt32) : only(args)
            end
            push!(mod.mems, Mem(MemoryType(min, max)))
        elseif head === :data
            ctx = FuncContext(mod)
            inst = Inst[]
            make_inst!(inst, popfirst!(args), ctx)
            mode = DataModeActive(0, inst)
            data = Data(Vector{UInt8}(popfirst!(args)), mode)
            push!(mod.datas, data)
        elseif head === :export
            push!(export_exprs, args)
        elseif head === :type
            name = nothing
            if first(args) isa Symbol
                name = popfirst!(args)
            end

            if length(args) >= 1 && issexpr(first(args), :func)
                rest = only(args)
                popfirst!(rest)
                t = parse_functype!(rest, FuncContext(mod))
            elseif length(args) >= 1 && (issexpr(first(args), :struct) || issexpr(first(args), :sub))
                rest = only(args)
                t = parse_struct_type!(rest, FuncContext(mod))
            else
                rest = args
                t = parse_functype!(rest, FuncContext(mod))
            end

            push!(mod.types, t)

            if name !== nothing
                named_types[name] = length(mod.types)
            end
        end
    end

    for (func_idx, ex) in enumerate(func_exprs)
        head, args... = ex

        name = length(args) >= 1 && first(args) isa Symbol && startswith(string(args[1]), '$') ?
            string(popfirst!(args))[begin+1:end] : nothing

            if length(args) >= 1 && issexpr(first(args), :export)
            _, name = popfirst!(args)
            push!(mod.exports,
                FuncExport(name, func_idx + num_function_imports)
            )
        end

        ctx = FuncContext(
            mod,
            named_globals,
            named_functions,
            named_types,
            Dict{Symbol,Int}(),
            Symbol[],
        )

        fntype = parse_functype!(args, ctx)

        locals = copy(fntype.params)
        while length(args) >= 1 && issexpr(first(args), :local)
            _, p... = popfirst!(args)

            valtypes = if length(p) > 0 && first(p) isa Symbol && startswith(string(first(p)), '$')
                locname = first(p)
                ctx.named_locals[locname] = length(locals) + 1
                @assert length(p) == 2
                ValType[parse_valtype(ctx, last(p))]
            else
                parse_valtype.((ctx,), p)
            end

            append!(locals, valtypes)
        end

        inst = Inst[]

        if !any(a -> a isa Vector{Any}, args)
            make_inst_linear!(inst, args, ctx)
        else
            while !isempty(args)
                make_inst!(inst, args, ctx)
            end
        end

        push!(
            mod.funcs,
            Func(name, fntype, locals, inst)
        )

    end

    for args in export_exprs
        ctx = FuncContext(
            mod,
            named_globals,
            named_functions,
            named_types,
            Dict{Symbol,Int}(),
            Symbol[],
        )
        name = popfirst!(args)::String
        exp = if issexpr(only(args), :func)
            fname = last(only(args))
            FuncExport(name, _resolve_func(ctx, fname))
        elseif issexpr(only(args), :memory)
            MemExport(name, 1+last(only(args)))
        elseif issexpr(only(args), :global)
            gname = last(only(args))
            GlobalExport(name, _resolve_global(ctx, gname))
        else
            error("cannot export $args")
        end
        push!(mod.exports, exp)
    end

end

function parse_wast(io::IO)
    sexpr = parse_sexpr(lex(io))

    results = []

    for ex in sexpr
        if !(ex isa Vector{Any})
            error("invalid token $ex")
        end

        if isempty(ex)
            continue
        end

        head, args... = ex

        if head === :module
            if length(args) >= 1 && first(args) === :quote
                popfirst!(args)

                args = join(args,) |> IOBuffer |> lex |> parse_sexpr
            end

            mod = Module()
            make_module!(mod, args)
            push!(results, mod)
        elseif head === :assert_return
            inst = Inst[]
            ctx = FuncContext(nothing)

            if length(args) >= 2
                make_inst!(inst, args[2:end], ctx)
            end

            inv, name, fargs... = args[1]

            vargs = Inst[]
            make_inst!(vargs, fargs, ctx)

            push!(results, [:assert_return, Any[inv,name,vargs], inst])
        elseif head === :assert_invalid
            mod = Module()
            make_module!(mod, args[1][begin+1:end])
            push!(
                results,
                Any[:assert_invalid, mod, args[end]]
            )
            # try validate(mod)
            # catch e
            #     e isa ValidationError || rethrow()
            # end
        else
            push!(results, ex)
        end
    end

    results
end
