const INDENT_S = " "
const INDENT_INC = 2

function format_data_init(io::IO, bytes::Vector{UInt8})
    for b in bytes
        c = Char(b)
        r = repr(c)
        r = r[nextind(r,begin):prevind(r,end)]
        r = replace(r, "\\x" => "\\")
        if startswith(r, '\\') && length(r) == 2 && last(r) in '0':'9'
            r = '\\' * lpad(r[begin+1:end], 2, '0')
        end
        write(io, r)
    end
end

function _printwasm(io::IO, mod::WModule)
    print(io, "(")
    _printkw(io, "module")
    indent = INDENT_INC

    for type in mod.types
        println(io)
        if type isa StructType
            ctx = IOContext(io, :indent => indent, :mod => mod)
            _printwasm(ctx, type)
        elseif type isa RecursiveZone
            print(io, INDENT_S^indent, "(")
            _printkw(io, "rec"); println(io)
            ctx = IOContext(io, :indent => indent + INDENT_INC, :mod => mod)
            for struct_ in type.structs
                _printwasm(ctx, struct_) 
            end
            println(io, INDENT_S^indent, ")")
        else
            error("don't know how to export $type")
        end
    end

    !isempty(mod.types) && println(io)

    for mem in mod.mems
        println(io)
        print(io, INDENT_S^indent, '(')
        _printkw(io, "memory")
        print(io, ' ', mem.type.min, ' ', mem.type.max)
        print(io, ')')
    end

    !isempty(mod.mems) && println(io)

    for data in mod.datas
        println(io)
        print(io, INDENT_S^indent, '(')
        _printkw(io, "data")
        print(io, ' ')
        if data.mode isa DataModeActive
            print(io, data.mode.memory, " (")
            _printwasm(io, data.mode.offset) 
            print(io, ") ")
        end
        # show(io, String(copy(data.init)))
        print(io, '"')
        format_data_init(io, data.init)
        print(io, '"')
        print(io, ')')
    end

    !isempty(mod.datas) && println(io)

    ctx = IOContext(io, :indent => indent, :mod => mod)
    for func in mod.funcs
        println(io)
        _printwasm(ctx, func)
        println(io)
    end

    if !isnothing(mod.start)
        println(io)
        print(io, INDENT_S^indent, "(")
        _printkw(io, "start")
        print(io, " ")
        start = mod.start - count(imp -> imp isa FuncImport, mod.imports)
        name = start <= 0 ?
            filter(imp -> imp isa FuncImport, mod.imports)[mod.start].id :
            mod.funcs[start].name
        if !isnothing(name)
            print_sigil(io, name)
        else
            print(io, mod.start - 1)
        end
        println(io, ")")
        println(io)
    end

    ctx = IOContext(io, :mod => mod, :ctx => indent + INDENT_INC)
    for global_ in mod.globals
        print(io, INDENT_S^indent, "(")
        _printkw(io, "global")
        print(io, ' ')
        !isnothing(global_.name) && (print_sigil(io, global_.name); print(io, ' '))
        global_.type.mut && (print(io, '('); _printkw(io, "mut"); print(io, ' '))
        _printwasm(ctx, global_.type.type)
        global_.type.mut && print(io, ')')
        print(io, " (")
        _printwasm(ctx, global_.init)
        println(io, "))")
    end

    !isempty(mod.globals) && println(io)

    ctx = IOContext(io, :mod => mod)
    for imp in mod.imports
        print(io, INDENT_S^indent, "(")
        _printkw(io, "import")
        print(io, " \"$(imp.module_name)\" \"$(imp.name)\" ")
        if imp isa FuncImport
            print(io, "(")
            _printkw(io, "func")
            print(io, ' ')
            print_sigil(io, imp.id)
            !isnothing(imp.id) && print(io, ' ')
            _printwasm(ctx, imp.fntype)
            print(io, ")")
        elseif imp isa GlobalImport
            print(io, "(")
            _printkw(io, "global")
            print(io, ' ')
            print_sigil(io, imp.id)
            print(io, ' ')
            imp.type.mut && (print(io, '('); _printkw(io, "mut"); print(io, ' '))
            _printwasm(ctx, imp.type.type)
            imp.type.mut && print(io, ')')
            print(io, ")")
        elseif imp isa TagImport
            print(io, "(")
            _printkw(io, "tag")
            print(io, ' ')
            !isnothing(imp.id) && (print_sigil(io, imp.id); print(io, ' '))
            _printwasm(io, imp.fntype)
            print(io, ')')
        elseif imp isa MemImport
            print(io, "(")
            _printkw(io, "memory")
            print(io, ' ')
            !isnothing(imp.id) && (print_sigil(io, imp.id); print(io, ' '))
            print(io, imp.mem.type.min, " ", imp.mem.type.max)
            print(io, ")")
        else
            error("cannot handle import $imp")
        end
        println(io, ")")
    end

    !isempty(mod.imports) && println(io)

    for exp in mod.exports
        print(io, INDENT_S^indent, "(")
        _printkw(io, "export")
        print(io, " ")
        if exp isa FuncExport
            print(io, "\"$(exp.name)\" ", "(")
            _printkw(io, "func")
            print(io, " ")
            print_funcidx(ctx, exp.func)
            print(io, ")")
        elseif exp isa MemExport
            print(io, "\"$(exp.name)\" ", "(")
            _printkw(io, "memory")
            print(io, ' ', exp.mem - 1)
            print(io, ')')
        else
            error("cannot handle export $exp")
        end
        println(io, ")")
    end
    print(io, ")")
end

function print_sigil(io::IO, sigil)
    isnothing(sigil) && return
    get(io, :color, false) ?
      printstyled(io, '$', sigil; color=:blue) :
      print(io, '$', sigil)
end

function _find_type(mod::WModule, typeidx)
    s = 0
    for type in Iterators.flatten(
        Iterators.map(t -> t isa RecursiveZone ? t.structs : (t,), mod.types))

        s += 1

        s == typeidx && return type
    end
    return nothing
end

function print_typeidx(io::IO, typeidx)
    mod = get(io, :mod, nothing)

    if isnothing(mod)
        print(io, typeidx - 1)
        return
    end

    type = _find_type(mod, typeidx)

    if isnothing(type) || isnothing(type.name)
        print(io, typeidx - 1)
    elseif type isa GCType
        print_sigil(io, type.name)
    else
        error("dont know how to print type $type")
    end
    return
end

function print_funcidx(io::IO, funcidx)
    mod = get(io, :mod, nothing)
    if isnothing(mod)
        print(io, funcidx - 1)
        return
    end

    num_func_imports = count(imp -> imp isa FuncImport, mod.imports)
    name = if funcidx <= num_func_imports
        func_imports = filter(imp -> imp isa FuncImport, mod.imports)
        func_imports[funcidx].id
    else
        func = mod.funcs[funcidx - num_func_imports]
        func.name
    end
    if isnothing(name)
        print(io, funcidx - 1)
        return
    end

    print_sigil(io, name)
end

function _printwasm(io::IO, arraytype::ArrayType)
    indent = get(io, :indent, 0)
    print(io, INDENT_S^indent, "(")
    _printkw(io, "type")
    !isnothing(arraytype.name) && (print(io, ' '); print_sigil(io, arraytype.name))
    print(io, " (")
    _printkw(io, "array")
    print(io, ' ')

    if arraytype.mut
        print(io, "(")
        _printkw(io, "mut")
        print(io, ' ')
    end

    _printwasm(io, arraytype.content)

    arraytype.mut && print(io, ')')
    println(io, "))")
end

function _printwasm(io::IO, structtype::StructType)
    indent = get(io, :indent, 0)
    print(io, INDENT_S^indent, "(")
    _printkw(io, "type")

    !isnothing(structtype.name) && (print(io, ' '); print_sigil(io, structtype.name))
    println(io)
    indent += INDENT_INC

    if !isnothing(structtype.subidx)
        print(io, INDENT_S^indent, "(")
        _printkw(io, "sub")
        print(io, ' ')
        print_typeidx(io, structtype.subidx)
        indent += INDENT_INC
        println(io)
    end

    print(io, INDENT_S^indent, "("); _printkw(io, "struct")
    indent += INDENT_INC

    for field in structtype.fields
        println(io)
        print(io, INDENT_S^indent, "("); _printkw(io, "field ")
        !isnothing(field.name) && (print_sigil(io, field.name); print(io, ' '))
        field.mut && (print(io, "("); _printkw(io, "mut"); print(io, " "))
        _printwasm(io, field.type)
        field.mut && print(io, ")")
        print(io, ")")
    end

    !isnothing(structtype.subidx) && print(io, ')')

    print(io, "))")
    println(io)
end

_printwasm(io::IO, val::ValType) = show(io, val)
_printwasm(io::IO, ::StringRef) = (print(io, '('); _printkw(io, "ref"); print(io, " "); _printkw(io, "string"); print(io, ')'))
function _printwasm(io::IO, ref::StructRef)
    print(io, "(")
    _printkw(io, "ref"); print(io, " ")
    ref.null && (_printkw(io, "null"); print(io, " "))
    print_typeidx(io, ref.typeidx)
    print(io, ")")
end
function _printwasm(io::IO, arr::ArrayRef)
    print(io, "(")
    _printkw(io, "ref")
    print(io, " ")
    arr.null && (_printkw(io, "null"); print(io, ' '))
    print_typeidx(io, arr.typeidx)
    print(io, ")")
end

function _printwasm(io::IO, fntype::FuncType)
    compact = get(io, :compact, true)
    if compact
        if length(fntype.params) > 0
          print(io, "("); _printkw(io, "param")
          for param in fntype.params
              print(io, " ")
              _printwasm(io, param)
          end
          print(io, ")")
        end

        if length(fntype.results) > 0
          print(io, " ("); _printkw(io, "result")
          for result in fntype.results
              print(io, " ")
              _printwasm(io, result)
          end
          print(io, ")")
        end
    else
        for param in fntype.params
            print(io, "(")
            _printkw(io, "param")
            print(io, " ")
            _printwasm(io, param)
            print(io, ") ")
        end
        for result in fntype.results
            print(io, "(")
            _printkw(io, "result")
            print(io, " ")
            _printwasm(io, result)
            print(io, ") ")
        end
    end
end

_printkw(io::IO, kw) = get(io, :color, false) ?  printstyled(io, kw; color=:red) : print(io, kw)
function _printinst(io::IO, s)
    print(io, INDENT_S^get(io, :indent, INDENT_INC))
    get(io, :color, false) ?
        printstyled(io, s; color=:magenta) : print(io, s)
end

function print_tagidx(io::IO, idx)
    wmod = get(io, :mod, nothing)
    if isnothing(wmod)
        print(io, idx)
        return
    end

    name = nothing
    for imp in wmod.imports
        imp isa TagImport || continue
        idx -= 1
        if iszero(idx)
            name = imp.name
            break
        end
    end

    if isnothing(name)
        name = wmod.tags[idx].name
    end

    if isnothing(name)
        print(io, idx)
        return
    end

    print_sigil(io, name)
end

function print_globalidx(io::IO, idx)
    wmod = get(io, :mod, nothing)
    if isnothing(wmod)
        print(io, idx - 1)
        return
    end

    name = nothing
    for imp in wmod.imports
        imp isa GlobalImport || continue
        idx -= 1
        if iszero(idx)
            name = imp.name
            break
        end
    end

    if isnothing(name)
        name = wmod.globals[idx].name
    end

    if isnothing(name)
        print(io, idx - 1)
        return
    end
    print_sigil(io, name)
end

_printwasm(io::IO, g::global_get) = (_printinst(io, "global.get"); print(io, ' '); print_globalidx(io, g.n))
_printwasm(io::IO, g::global_set) = (_printinst(io, "global.set"); print(io, ' '); print_globalidx(io, g.n))
_printwasm(io::IO, ::return_) = _printinst(io, "return")
_printwasm(io::IO, t::throw_) = (_printinst(io, "throw"); print(io, ' '); print_tagidx(io, t.tag))
_printwasm(io::IO, rt::rethrow_) = (_printinst(io, "rethrow"); print(io, " ", rt.label))
_printwasm(io::IO, s::string_const) = (_printinst(io, "string.const"); print(io, " \"", s.contents, "\""))
_printwasm(io::IO, c::call) = (_printinst(io, "call"); print(io, ' '); print_funcidx(io, c.func))

_printwasm(io::IO, ls::local_set) = (_printinst(io, "local.set"), print(io, ' ', ls.n - 1))
_printwasm(io::IO, lt::local_tee) = (_printinst(io, "local.tee"), print(io, ' ', lt.n - 1))
_printwasm(io::IO, lg::local_get) = (_printinst(io, "local.get"), print(io, ' ', lg.n - 1))

function _printwasm(io::IO, cmp::v128cmp)
    inst = string(cmp.lane, "x", Lanes.count(cmp.lane), ".", cmp.cmp)
    if Operators.needs_sign(cmp.cmp)
        inst *= "_" * (cmp.signed ? "_s" : "_u")
    end
    _printinst(io, inst)
end
_printwasm(io::IO, b::v128bitmask) = _printinst(io, string(b.lane, "x", Lanes.count(b.lane), ".bitmask"))
_printwasm(io::IO, b::v128bin) = _printinst(io, string(b.lane, "x", Lanes.count(b.lane), b.op))
_printwasm(io::IO, b::v128all_true) = _printinst(io, string(b.lane, "x", Lanes.count(b.lane), ".all_true"))

_printwasm(io::IO, r::Union{ref_cast,ref_null}) = (_printinst(io, replace(string(nameof(typeof(r))), "_" => ".")); print(io, ' '); print_typeidx(io, r.typeidx))
_printwasm(io::IO, sn::struct_new) = (_printinst(io, "struct.new"); print(io, ' '); print_typeidx(io, sn.typeidx))
function _printwasm(io::IO, sg::struct_get)
    _printinst(io, "struct.get")
    print(io, ' ')
    print_typeidx(io, sg.typeidx)
    print(io, ' ')
    mod = get(io, :mod, nothing)
    if isnothing(mod)
        print(io, sg.fieldidx-1)
        return
    end
    typ = _find_type(mod, sg.typeidx)
    field = typ.fields[sg.fieldidx]
    if isnothing(field.name)
        print(io, sg.fieldidx-1)
    else
        print_sigil(io, field.name)
    end
end

function _printwasm(io::IO, inst::Inst)
    prefixes = ("i32", "i64", "f32", "f64",
                "v128", "ref", "struct", "string")

    name = string(nameof(typeof(inst)))
    prefidx = findfirst(t -> startswith(name, string(t) * '_'), prefixes)
    name = if !isnothing(prefidx)
        offset = length(prefixes[prefidx])-1
        name[begin:begin+offset] * '.' * name[begin+offset+2:end]
    elseif startswith(name, "br_")
        name
    else
        replace(name, "_" => ".")
    end
    _printinst(io, name)
    for f in fieldnames(typeof(inst))
        fieldval = getfield(inst, f)
        isnothing(fieldval) && continue
        if fieldval isa MemArg
            !iszero(fieldval.offset) && print(io, " offset=", fieldval.offset)
            !iszero(fieldval.align) && print(io, " align=", fieldval.align)
        else
            print(io, " ", fieldval)
        end
    end
end

function _printwasm(io::IO, instop::InstOperands)
    indent = get(io, :indent, INDENT_INC)
    print(io, INDENT_S^indent, '(')
    if instop.inst isa Union{Block,Loop}
        wmod = get(io, :mod, nothing)
        func = get(io, :func, nothing)
        instops = sexpr(wmod, func, instop.inst.inst)
        _printkw(io, instop.inst isa Block ? "block" : "loop")
        _printwasm(io, instop.inst.fntype)
        ctx = IOContext(io, :indent => indent + INDENT_INC)
        for instop in instops
            println(io)
            _printwasm(ctx, instop)
        end
        print(io, ')')
        return
    end
    if instop.inst isa If
        wmod = get(io, :mod, nothing)
        func = get(io, :func, nothing)
        trueinstops = sexpr(wmod, func, instop.inst.trueinst)
        falseinstops = sexpr(wmod, func, instop.inst.falseinst)
        _printkw(io, "if")
        if instop.inst.fntype != voidtype
            _printwasm(io, instop.inst.fntype)
        end
        println(io)
        ctx = IOContext(io, :indent => indent + INDENT_INC)
        _printwasm(ctx, instop.operands[1])
        println(io)
        print(io, INDENT_S^(indent+INDENT_INC), "(")
        _printkw(io, "then")
        print(io, " ")
        ctx = IOContext(io, :indent => indent + 2INDENT_INC)
        for op in trueinstops
            println(io)
            _printwasm(ctx, op)
        end
        print(io, ")")
        if !isempty(falseinstops)
            println(io)
            print(io, INDENT_S^(indent+INDENT_INC), "(")
            _printkw(io, "else")
            print(io, " ")
            for op in falseinstops
                println(io)
                _printwasm(ctx, op)
            end
            print(io, ")")
        end
        print(io, ")")
        return
    end
    ctx = IOContext(io, :indent => 0)
    _printwasm(ctx, instop.inst)
    ctx = IOContext(io, :indent => indent + INDENT_INC)
    for op in instop.operands
        println(io)
        _printwasm(ctx, op)
    end
    print(io, ')')
end

function _printwasm(io::IO, f::Func)
    indent = get(io, :indent, 0)::Int
    print(io, INDENT_S^indent, "(")
    _printkw(io, "func")
    print(io, " ")
    if !isnothing(f.name)
        print_sigil(io, f.name)
        print(io, ' ')
    end
    _printwasm(io, f.fntype)
    println(io)
    compact = get(io, :compact, true)
    if compact
        if length(f.locals) > length(f.fntype.params)
            print(io, INDENT_S^(indent+INDENT_INC), "(")
            _printkw(io, "local")
            for loc in Iterators.drop(f.locals, length(f.fntype.params))
                print(io, " ")
                _printwasm(io, loc)
            end
            println(io, ")")
        end
    else
        for loc in Iterators.drop(f.locals, length(f.fntype.params))
            print(io, INDENT_S^(indent+INDENT_INC), "(")
            _printkw(io, "local")
            println(io, " ")
            _printwasm(io, loc)
            print(io, ")")
        end
    end
    ctx = IOContext(io, :func => f,
                        :indent => indent + INDENT_INC)
    print_sexpr = get(io, :print_sexpr, false)
    wmod = get(io, :mod, nothing)
    if isnothing(wmod) || !print_sexpr
        _printwasm(ctx, f.inst)
        println(io)
    else
        for instop in sexpr(wmod, f, f.inst)
            _printwasm(ctx, instop)
            println(io)
        end
    end
    print(io, INDENT_S^indent, ")")
end

function _printwasm(io::IO, loop::Loop)
    indent = get(io, :indent, INDENT_INC)
    print(io, INDENT_S^indent)
    _printkw(io, "loop")
    print(io, " ")
    _printwasm(io, loop.fntype)
    println(io)
    ctx = IOContext(io, :indent => indent + INDENT_INC)
    _printwasm(ctx, loop.inst)
    println(io)
    print(io, INDENT_S^indent)
    _printkw(io, "end")
end

function _printwasm(io::IO, i::If)
    indent = get(io, :indent, INDENT_INC)
    print(io, INDENT_S^indent)
    _printkw(io, "if")
    _printwasm(io, i.fntype)
    println(io)
    ctx = IOContext(io, :indent => indent + INDENT_INC)
    _printwasm(ctx, i.trueinst)
    println(io)
    print(io, INDENT_S^indent)
    if !isempty(i.falseinst)
      _printkw(io, "else")
      println(io)
      _printwasm(ctx, i.falseinst)
      println(io)
      print(io, INDENT_S^indent)
    end
    _printkw(io, "end")
end

function _printwasm(io::IO, try_::Try)
    indent = get(io, :indent, INDENT_INC)
    print(io, INDENT_S^indent)
    _printkw(io, "try")
    println(io)
    _printwasm(io, try_.fntype)
    println(io)
    ctx = IOContext(io, :indent => indent + INDENT_INC)
    _printwasm(ctx, try_.inst)
    for cblock in try_.catches
        print(io, INDENT_S^indent)
        _printkw(io, isnothing(cblock.tag) ? "catch_all" : "catch")
        println(io)
        _printwasm(ctx, cblock.inst)
    end
    println(io)
    print(io, INDENT_S^indent)
    _printkw(io, "end")
end

function _printwasm(io::IO, expr::Vector{Inst})
    ctx = IOContext(io, :indent => get(io, :indent, 0))
    for (i, inst) in enumerate(expr)
        _printwasm(ctx, inst)
        i != lastindex(expr) && println(io)
    end
end

function _printwasm(io::IO, b::Block)
    indent = get(io, :indent, INDENT_INC)
    print(io, INDENT_S^indent)
    _printkw(io, "block")
    print(io, " ")
    _printwasm(io, b.fntype)
    println(io)
    ctx = IOContext(io, :indent => indent + INDENT_INC)
    _printwasm(ctx, b.inst)
    println(io)
    print(io, INDENT_S^indent)
    _printkw(io, "end")
end
