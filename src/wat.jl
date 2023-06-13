const INDENT_S = " "
const INDENT_INC = 2

function _printwasm(io::IO, mod::WModule)
    print(io, "(")
    _printkw(io, "module")
    println(io)
    indent = INDENT_INC

    for type in mod.types
        if type isa StructType
            _printwasm(io, type)
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
    end

    for imp in mod.imports
        print(io, INDENT_S^indent, "(")
        _printkw(io, "import")
        print(io, " \"$(imp.module_name)\" \"$(imp.name)\"")
        if imp isa FuncImport
            print(io, "(")
            _printkw(io, "func")
            print_sigil(io, imp.id)
            _printwasm(io, imp.fntype)
            print(io, ")")
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

function print_typeidx(io::IO, typeidx)
    mod = get(io, :mod, nothing)

    if isnothing(mod)
        print(io, typeidx - 1)
        return
    end

    s = 0
    for type in Iterators.flatten(
        Iterators.map(t -> t isa RecursiveZone ? t.structs : (t,), mod.types))

        s += 1

        if s == typeidx
            type isa StructType || error("don't know how to print $type")
            if isnothing(type.name)
                print(io, typeidx - 1)
            else
                print_sigil(io, type.name)
            end
            return
        end
    end
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

function _printwasm(io::IO, structtype::StructType)
    indent = get(io, :indent, 0)
    print(io, INDENT_S^indent, "(")
    _printkw(io, "type")

    !isnothing(structtype.name) && (print(io, ' '); print_sigil(io, structtype.name))
    println(io)
    indent += INDENT_INC
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

    print(io, "))")
    println(io)
end

_printwasm(io::IO, val::ValType) = show(io, val)
function _printwasm(io::IO, ref::StructRef)
    print(io, "(")
    _printkw(io, "ref"); print(io, " ")
    ref.null && (_printkw(io, "null"); print(io, " "))
    print_typeidx(io, ref.typeidx)
    print(io, ")")
end
function _printwasm(io::IO, arr::ArrayRef)
    print(io, "(")
    _printkw(io, "array")
    print(io, " ")
    arr.mut && (print(io, "("); _printkw(io, "mut"); print(io, " "))
    _printwasm(io, arr.content)
    arr.mut && print(io, ")")
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
    println(io)
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

function _printwasm(io::IO, inst::Inst)
    prefixes = ("i32", "i64", "f32", "f64", "ref", "struct", "string")

    name = string(nameof(typeof(inst)))
    prefidx = findfirst(t -> startswith(name, string(t) * '_'), prefixes)
    name = if !isnothing(prefidx)
        name[begin:begin+length(prefixes[prefidx])-1] * '.' * name[begin+4:end]
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
    ctx = IOContext(io, :indent => indent + INDENT_INC)
    _printwasm(ctx, f.inst)
    print(io, INDENT_S^indent, ")")
end

function _printwasm(io::IO, loop::Loop)
    indent = get(io, :indent, INDENT_INC)
    print(io, INDENT_S^indent)
    _printkw(io, "loop")
    print(io, " ")
    _printwasm(io, loop.fntype)
    ctx = IOContext(io, :indent => indent + INDENT_INC)
    _printwasm(ctx, loop.inst)
    print(io, INDENT_S^indent)
    _printkw(io, "end")
end

function _printwasm(io::IO, i::If)
    indent = get(io, :indent, INDENT_INC)
    print(io, INDENT_S^indent)
    _printkw(io, "if")
    print(io, " ")
    _printwasm(io, i.fntype)
    ctx = IOContext(io, :indent => indent + INDENT_INC)
    _printwasm(ctx, i.trueinst)
    print(io, INDENT_S^indent)
    _printkw(io, "else")
    println(io)
    _printwasm(ctx, i.falseinst)
    print(io, INDENT_S^indent)
    _printkw(io, "end")
end

function _printwasm(io::IO, try_::Try)
    indent = get(io, :indent, INDENT_INC)
    print(io, INDENT_S^indent)
    _printkw(io, "try")
    println(io)
    _printwasm(io, try_.fntype)
    ctx = IOContext(io, :indent => indent + INDENT_INC)
    _printwasm(ctx, try_.inst)
    for cblock in try_.catches
        print(io, INDENT_S^indent)
        _printkw(io, isnothing(cblock.tag) ? "catch_all" : "catch")
        println(io)
        _printwasm(ctx, cblock.inst)
    end
    print(io, INDENT_S^indent)
    _printkw(io, "end")
end

function _printwasm(io::IO, expr::Vector{Inst})
    ctx = IOContext(io, :indent => get(io, :indent, 0))
    for inst in expr
        _printwasm(ctx, inst)
        println(io)
    end
end

function _printwasm(io::IO, b::Block)
    indent = get(io, :indent, INDENT_INC)
    print(io, INDENT_S^indent)
    _printkw(io, "block")
    print(io, " ")
    _printwasm(io, b.fntype)
    ctx = IOContext(io, :indent => indent + INDENT_INC)
    _printwasm(ctx, b.inst)
    print(io, INDENT_S^indent)
    _printkw(io, "end")
end
