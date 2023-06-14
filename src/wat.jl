const INDENT_S = " "
const INDENT_INC = 2

function _printwasm(io::IO, mod::WModule)
    print(io, "(")
    _printkw(io, "module")
    println(io)
    indent = INDENT_INC

    for type in mod.types
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

    println(io)

    ctx = IOContext(io, :indent => indent, :mod => mod)
    for func in mod.funcs
        _printwasm(ctx, func)
        println(io)
        println(io)
    end

    if !isnothing(mod.start)
        print(io, INDENT_S^indent, "(")
        _printkw(io, "start")
        print(io, " ")
        func = mod.funcs[mod.start]
        if !isnothing(func.name)
            print_sigil(io, func.name)
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
        _printwasm(io, global_.type.type)
        global_.type.mut && print(io, ')')
        print(io, " (")
        _printwasm(ctx, global_.init)
        print(io, "))")
    end

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
            _printwasm(io, imp.type)
            print(io, ')')
        else
            error("cannot handle import $imp")
        end
        println(io, ")")
    end

    println(io)
    for exp in mod.exports
        print(io, INDENT_S^indent, "(")
        _printkw(io, "export")
        print(io, " ")
        if exp isa FuncExport
            func = mod.funcs[exp.func]
            print(io, "\"$(exp.name)\" ", "(")
            _printkw(io, "func")
            print(io, " ")
            if !isnothing(func.name)
                print_sigil(io, func.name)
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

function print_sigil(io::IO, sigil)
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

    func = mod.funcs[funcidx]
    if isnothing(func.name)
        print(io, funcidx - 1)
        return
    end

    print_sigil(io, func.name)
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
    print(io, INDENT_S^indent, "("); _printkw(io, "struct")
    indent += INDENT_INC

    if !isnothing(structtype.subidx)
        println(io)
        print(io, INDENT_S^indent, "(")
        _printkw(io, "sub")
        print(io, ' ')
        print_typeidx(io, structtype.subidx)
        indent += INDENT_INC
    end

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
_printwasm(io::IO, ::StringRef) = (print(io, '('); _printkw(io, "ref"); print(io, " "); _printkw(io, "string"))
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

_printwasm(io::IO, ::return_) = _printinst(io, "return")
_printwasm(io::IO, t::throw_) = (_printinst(io, "throw"); print(io, " ", t.tag))
_printwasm(io::IO, rt::rethrow_) = (_printinst(io, "rethrow"); print(io, " ", rt.label))
_printwasm(io::IO, s::string_const) = (_printinst(io, "string.const"); print(io, " \"", s.contents, "\""))
_printwasm(io::IO, c::call) = (_printinst(io, "call"); print(io, ' '); print_funcidx(io, c.func + 1))

_printwasm(io::IO, r::ref_cast) = (_printinst(io, "ref.cast"); print(io, ' '); print_typeidx(io, r.typeidx + 1))
_printwasm(io::IO, sn::struct_new) = (_printinst(io, "struct.new"); print(io, ' '); print_typeidx(io, sn.typeidx + 1))
function _printwasm(io::IO, sg::struct_get)
    _printinst(io, "struct.get")
    print(io, ' ')
    print_typeidx(io, sg.typeidx + 1)
    print(io, ' ')
    mod = get(io, :mod, nothing)
    if isnothing(mod)
        print(io, sg.fieldidx)
        return
    end
    typ = _find_type(mod, sg.typeidx + 1)
    field = typ.fields[sg.fieldidx + 1]
    if isnothing(field.name)
        print(io, sg.fieldidx)
    else
        print_sigil(io, field.name)
    end
end

function _printwasm(io::IO, inst::Inst)
    prefixes = ("i32", "i64", "f32", "f64", "ref", "struct", "string")

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
        print(io, " ", fieldval)
    end
end

function _printwasm(io::IO, instop::InstOperands)
    indent = get(io, :indent, INDENT_INC)
    print(io, INDENT_S^indent, '(')
    if instop.inst isa Block
        wmod = get(io, :mod, nothing)
        instops = sexpr(wmod, instop.inst.inst)
        _printkw(io, "block")
        _printwasm(io, instop.inst.fntype)
        ctx = IOContext(io, :indent => indent + INDENT_INC)
        for instop in instops
            println(io)
            _printwasm(ctx, instop)
        end
        print(io, ')')
        println(io)
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
            _printwasm(ctx, loc)
            print(io, ")")
        end
    end
    ctx = IOContext(io, :indent => indent + INDENT_INC)
    print_sexpr = get(io, :print_sexpr, false)
    wmod = get(io, :mod, nothing)
    if isnothing(wmod) || !print_sexpr
        _printwasm(ctx, f.inst)
    else
        for instop in sexpr(wmod, f.inst)
            _printwasm(ctx, instop)
        end
    end
    println(io)
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
    print(io, " ")
    _printwasm(io, i.fntype)
    println(io)
    ctx = IOContext(io, :indent => indent + INDENT_INC)
    _printwasm(ctx, i.trueinst)
    println(io)
    print(io, INDENT_S^indent)
    _printkw(io, "else")
    println(io)
    _printwasm(ctx, i.falseinst)
    println(io)
    print(io, INDENT_S^indent)
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
