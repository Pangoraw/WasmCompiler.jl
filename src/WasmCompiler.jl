module WasmCompiler

include("./instructions.jl")
include("./wasm.jl")
include("./compiler.jl")
include("./relooper.jl")
include("./sexpr.jl")
include("./wat.jl")
include("./optimize.jl")

struct Wat
    obj
end

Base.show(io::IO, wat::Wat) = WasmCompiler._printwasm(io, wat.obj)

macro code_wasm(opts, ex=nothing)
    if isnothing(ex)
        ex, opts = opts, nothing
    end
    @assert Meta.isexpr(ex, :call)

    args = esc(Expr(:tuple, ex.args[begin+1:end]...))
    f = esc(ex.args[1])

    if !isnothing(opts)
        @assert Meta.isexpr(opts, :(=)) "invalid option $opts"
        @assert opts.args == [:mod, true] "invalid option $opts"
        quote
            types = Tuple{map(Core.Typeof, $(args))...}
            module_ = WasmCompiler.RuntimeModule()
            WasmCompiler.emit_func!(module_, $f, types)
            Wat(module_)
        end
    else
        quote
            types = Tuple{map(Core.Typeof, $(args))...}
            func = WasmCompiler.emit_func($f, types)
            Wat(func)
        end
    end
end

export @code_wasm

end # module WasmCompiler
