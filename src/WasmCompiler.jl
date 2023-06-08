module WasmCompiler

include("./instructions.jl")
include("./wasm.jl")
include("./compiler.jl")
include("./relooper.jl")
include("./wat.jl")
include("./optimize.jl")

struct Wat
    obj
end

Base.show(io::IO, wat::Wat) = WasmCompiler._printwasm(io, wat.obj)

macro code_wasm(ex)
    @assert Meta.isexpr(ex, :call)

    args = esc(Expr(:tuple, ex.args[begin+1:end]...))
    f = esc(ex.args[1])

    quote
        types = Tuple{map(Core.Typeof, $(args))...}
        func = WasmCompiler.emit_func($f, types)
        Wat(func)
    end
end

export @code_wasm

end # module WasmCompiler
