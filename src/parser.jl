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
            push!(tokens, Token(position(io), read_sym(io)))
        elseif numberstart(c)
            push!(tokens, Token(position(io), read_number(io)))
        elseif c == '"'
            push!(tokens, Token(position(io), read_str(io)))
        elseif c == ')'
            read(io, Char)
            push!(tokens, Token(position(io), ')'))
        elseif c == ';'
            read(io, Char)
            if peek(io, Char) == ';'
                while !eof(io)
                    c = read(io, UInt8)
                    if c == UInt8('\n') || c == UInt8('\r')
                        break
                    end
                end
            end
        elseif c == '\t' || c == ' ' || c == '\n' || c == '\r'
            read(io, Char)
        else
            @show tokens
            println(String(read(io)))
            error("invalid char $c $((UInt32(c))) at $(position(io))")
        end
    end

    tokens
end

struct FuncContext
    named_globals::Dict{Symbol,Int}
    named_functions::Dict{Symbol,Int}
    named_types::Dict{Symbol,Int}
    named_locals::Dict{Symbol,Int}
    labels::Vector{Symbol}
end
FuncContext() = FuncContext(Dict{Symbol,Int}(), Dict{Symbol,Int}(), Dict{Symbol,Int}(), Dict{Symbol,Int}(), Symbol[])

symstart(c) = validchar(c) && c ∉ '0':'9' && c != '-'
validchar(c) = c ∈ (('a':'z') ∪ ('A':'Z') ∪ ('0':'9')) || c == '_' || c == '.' || c == '$' || c == '-'
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
    s = ""
    @assert read(io, Char) == '"'
    c = peek(io, Char)
    while !eof(io) && c != '"'
        read(io, Char)

        if c == '\\'
            eof(io) && break
            c = read(io,Char)
            if c == '0'
                eof(io) && break
                c = Char(parse(UInt8, read(io, Char); base=16))
            end
            s *= c
        else
            s *= c
        end

        c = peek(io, Char)
    end
    !eof(io) && read(io, Char)
    s
end

function read_number(io::IO)
    neg = peek(io, Char) == '-'
    neg && read(io, Char)

    base = 10
    if peek(io, Char) == '0'
        read(io, Char)
        if peek(io, Char) == 'x'
            read(io, Char)
            base = 16
        elseif peek(io, Char) == 'b'
            read(io, Char)
            base = 2
        end
    end

    n = 0
    c = peek(io, Char)
    while !eof(io) && (numberstart(c) || (base == 16 && (c ∈ ('a':'f') ∪ ('A':'F'))) || c == '.')
        if c == '.'
            error("unimplemented")
        end
        n = base * n + parse(Int,c;base)
        read(io, Char)
        eof(io) && break
        c = peek(io, Char)
    end
    neg ? -n : n
end

function parse_sexpr(tokens)
    result = []

    while !isempty(tokens)
        (; pos, val) = popfirst!(tokens)

        if val === '('
            push!(result, parse_sexpr(tokens))
        elseif val === ')'
            break
        else
            push!(result, val)
        end
    end

    result
end

issexpr(val::Vector{Any}, head) = length(val) >= 1 && first(val) === head

function parse_valtype(ex)
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
    else
        error("unknown valtype $(ex)")
    end
end

function parse_functype!(args, ctx)
    params = ValType[]
    while length(args) >= 1 && issexpr(first(args), :param)
        _, p... = popfirst!(args)

        valtypes = if length(p) > 0 && first(p) isa Symbol && startswith(string(first(p)), '$')
            name = first(p)
            ctx.named_locals[name] = length(params)
            @assert length(p) == 2
            ValType[parse_valtype(last(p))]
        else
            parse_valtype.(p)
        end

        append!(params, valtypes)
    end
    results = ValType[]
    while length(args) >= 1 && issexpr(first(args), :result)
        _, p... = popfirst!(args)
        append!(results, parse_valtype.(p))
    end

    FuncType(params, results)
end

_resolve_local(ctx, idx) = idx isa Int ? idx + 1 : ctx.named_locals[idx]
_resolve_label(ctx, idx) = idx isa Int ? idx : error()
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
            @assert length(args) == 1
            push!(inst, local_get(_resolve_local(ctx, popfirst!(args))))
        elseif head === :global_set
            push!(inst, global_set(_resolve_global(ctx, popfirst!(args))))
        elseif head === :call_indirect
            typ = _resolve_type(ctx, popfirst!(args)[end])
            push!(inst, call_indirect(typ))
        elseif head === :call
            func = _resolve_func(ctx, popfirst!(args))
            push!(inst, call(func))
        elseif head === :br_if || head === :br
            dest = popfirst!(args)
            push!(inst, getproperty(WC, head)(_resolve_label(ctx, dest)))
        elseif head === :br_table
            labels = Int[]
    
            while first(args) isa Symbol || first(args) isa Int
                push!(
                    labels,
                    _resolve_label(ctx, popfirst!(args)),
                )
            end
    
            default = pop!(labels)
            push!(inst, br_table(labels, default))

        elseif head === :return
            make_inst!(inst, args, ctx)
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
            @assert T <: Inst "invalid head $head"
            push!(inst, T())
            error("invalid head $head")
        end        
    end
end

# for wast
function make_inst!(inst, ex, ctx)
    if length(ex) == 0
        return
    end
    if first(ex) isa Vector{Any}
        for ex2 in ex
            make_inst!(inst, ex2, ctx)
        end
        return
    end

    head, args... = ex
    head =  replace(string(head), '.' => '_') |> Symbol
    if head === :i32_const
        val = only(args)

        if val > typemax(Int32)
            val = reinterpret(Int32, UInt32(val))
        end

        push!(inst, i32_const(val))
    elseif head === :i64_const
        push!(inst, i64_const(only(args)))
    elseif head === :local_set
        @assert length(args) == 2
        make_inst!(inst, last(args), ctx)
        push!(inst, local_set(_resolve_local(ctx, first(args))))
    elseif head === :local_tee
        make_inst!(inst, args, ctx)
        push!(inst, local_tee(_resolve_local(ctx, first(args))))
    elseif head === :local_get
        @assert length(args) == 1
        push!(inst, local_get(_resolve_local(ctx, first(args))))
    elseif head === :global_set
        @assert length(args) == 1
        push!(inst, global_set(_resolve_global(ctx, first(args))))
    elseif head === :select
        @assert length(args) == 3
        make_inst!(inst, args[1], ctx)
        make_inst!(inst, args[2], ctx)
        make_inst!(inst, args[3], ctx)
        push!(inst, select())
    elseif head ∈ (:i32_eqz, :i32_eq, :i32_ne, :i32_add, :i32_sub, :i32_div_s, :i32_div_u, :i32_lt_s, :i32_lt_u,
                   :i32_le_s, :i32_le_u, :i32_gt_u, :i32_gt_s, :i32_ge_s, :i32_ge_u, :i32_rem_s, :i32_rem_u,
                   :i32_and, :i32_or, :i32_xor, :i32_shl, :i32_shr_u, :i32_shr_s, :i32_rotl, :i32_rotr, :i32_clz,
                   :i32_ctz, :i32_popcnt, :i32_extend8_s, :i32_extend8_u, :i32_extend16_u, :i32_extend16_s,
                   :i32_gt_s, :i32_mul,
                   :i64_eq, :i64_add, :i64_sub, :i64_le_s, :i64_gt_s, :i64_mul, )
        make_inst!(inst, args, ctx)
        push!(inst, getproperty(WC, head)())
    elseif head ∈ (:unreachable, :nop, :drop)
        @assert isempty(args)
        push!(inst, getproperty(WC, head)())
    elseif head === :call_indirect
        typ = _resolve_type(ctx, popfirst!(args)[end])
        make_inst!(inst, args, ctx)
        push!(inst, call_indirect(typ))
    elseif head === :call
        func = _resolve_func(ctx, popfirst!(args))
        make_inst!(inst, args, ctx)
        push!(inst, call(func))
    elseif head === :br_if || head === :br
        dest = popfirst!(args)
        make_inst!(inst, args, ctx)
        push!(inst, getproperty(WC, head)(dest))
    elseif head === :br_table
        labels = Int[]

        while first(args) isa Symbol || first(args) isa Int
            push!(
                labels,
                _resolve_label(ctx, popfirst!(args)),
            )
        end

        default = pop!(labels)
        make_inst!(inst, args, ctx)
        push!(inst, br_table(labels, default))
    elseif head === :return
        make_inst!(inst, args, ctx)
        push!(inst, return_())
    elseif head === :if
        fntype = parse_functype!(args, ctx)
        if length(args) >= 1 && (!issexpr(first(args), :then) && !issexpr(first(args), :else))
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

        push!(inst, If(fntype, trueinst, falseinst))
    elseif head === :loop
        fntype = parse_functype!(args, ctx)
        if length(args) <= 1
            error("invalid if")
        end
        newinst = Inst[]
        make_inst!(newinst, args, ctx)
        push!(inst, Loop(fntype, newinst))
    elseif head === :block
        fntype = parse_functype!(args, ctx)
        if length(args) <= 1
            error("invalid if")
        end
        newinst = Inst[]
        make_inst!(newinst, args, ctx)
        push!(inst, Block(fntype, newinst))
    else
        error("invalid head $head")
    end
end

function make_module!(mod, exprs)
    func_exprs = Any[]
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

            if name !== nothing
                named_functions[name] = length(func_exprs) + num_function_imports
            end
            push!(func_exprs, ex)
        elseif head === :type
            name, rest... = args
            _, rest... = rest

            t = parse_functype!(rest, FuncContext())
            push!(mod.types, t)

            named_types[name] = length(mod.types)
        end
    end

    for (func_idx, ex) in enumerate(func_exprs)
        head, args... = ex
        if head === :func
            name = length(args) >= 1 && first(args) isa Symbol && startswith(string(args[1]), '$') ?
                string(popfirst!(args))[begin+1:end] : nothing

            if length(args) >= 1 && issexpr(first(args), :export)
                _, name = popfirst!(args)
                push!(mod.exports,
                    FuncExport(name, func_idx + num_function_imports)
                )
            end

            ctx = FuncContext(
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
                    name = first(p)
                    ctx.named_locals[name] = length(locals)
                    @assert length(p) == 2
                    ValType[parse_valtype(last(p))]
                else
                    parse_valtype.(p)
                end

                append!(locals, parse_valtype.(p))
            end

            inst = Inst[]

            while !isempty(args)
                ex = popfirst!(args)
                if !(ex isa Vector{Any})
                    error("invalid expr $ex")
                end
                make_inst!(inst, ex, ctx)
            end

            push!(
                mod.funcs,
                Func(name, fntype, locals, inst)
            )
        end

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
            make_inst!(inst, args[2], FuncContext())

            push!(results, [:assert_return, args[1], inst])
        elseif head === :assert_invalid
            mod = Module()
            make_module!(mod, args[1][begin+1:end])
            @show WC.Wat(mod)
            try validate(mod)
            catch e
                e isa ValidationError || rethrow()
            end
        else
            push!(results, ex)
        end
    end

    results
end
