module WasmCompiler

include("./binaryen.jl")
include("./instructions.jl")
include("./wasm.jl")
include("./compiler.jl")
include("./relooper.jl")
include("./sexpr.jl")
include("./wat.jl")
include("./write.jl")
include("./read.jl")
include("./optimize.jl")

struct Wat
    obj::Any
    sexpr::Bool
end
Wat(obj) = Wat(obj, false)

Base.show(io::IO, wat::Wat) = WasmCompiler._printwasm(IOContext(io, :print_sexpr => wat.sexpr), wat.obj)

macro code_wasm(exprs...)
    opts..., ex = exprs
    @assert Meta.isexpr(ex, :call)
    @assert all(opt -> Meta.isexpr(opt, :(=)), opts)

    args = esc(Expr(:tuple, ex.args[begin+1:end]...))
    f = esc(ex.args[1])

    dopts = Dict{Symbol,Any}()
    for opt in opts
        key, val = opt.args
        dopts[key] = val
    end

    print_sexpr = get(dopts, :sexpr, false)
    optimize = get(dopts, :optimize, false)
    wmod = get(dopts, :mod, false)
    if wmod !== false
        quote
            types = Tuple{map(Core.Typeof, $(args))...}
            module_ = $(wmod !== :runtime) ?
                WasmCompiler.WModule() :
                WasmCompiler.RuntimeModule()
            WasmCompiler.emit_func!(module_, $f, types; optimize=$optimize !== false)
            if applicable(nameof, $f)
                WasmCompiler.export!(module_, string(nameof($f)), 1)
            end
            if $optimize === :binaryen
                module_ = WasmCompiler.optimize(module_)
            end
            Wat(module_, $(print_sexpr))
        end
    else
        quote
            types = Tuple{map(Core.Typeof, $(args))...}
            func = WasmCompiler.emit_func($f, types; optimize=$optimize)
            Wat(func, $(print_sexpr))
        end
    end
end

export @code_wasm

end # module WasmCompiler
