module WasmCompiler

include("./utils.jl")
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
include("./validate.jl")
include("./parser.jl")
include("./interpreter.jl")
include("./builder.jl")

struct Wat
    obj::Any
    sexpr::Bool
end
Wat(obj) = Wat(obj, false)

Base.show(io::IO, wat::Wat) = WasmCompiler._printwasm(IOContext(io, :print_sexpr => wat.sexpr), wat.obj)

"""
    @code_wasm f(args...)
    @code_wasm [optimize=false mod=false sexpr=false debug=false] f(args...)

Returns the WebAssembly form of the called function.

## Example

```julia
julia> @code_wasm optimize=true floor(2.)
(func $floor (param f64) (result f64)
  local.get 0
  f64.floor
)

julia> @code_wasm optimize=true sexpr=true sqrt(1f0)
(module
  (func $sqrt (param f32) (result f32)
    (if
      (f32.lt
        (local.get 0)
        (f32.const 0.0))
      (then
        (unreachable))
      (else
        (return
          (f32.sqrt
            (local.get 0)))))
    (unreachable)
  )
  (export "sqrt" (func $sqrt))
)
```
"""
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
    debug = get(dopts, :debug, false)
    validate = get(dopts, :validate, false)
    if wmod !== false || !(optimize isa Bool) || print_sexpr
        quote
            types = Tuple{map(Core.Typeof, $(args))...}
            module_ = $(wmod) === :runtime ?
                WasmCompiler.RuntimeModule() :
                $(wmod) === :malloc ?
                WasmCompiler.MallocModule() :
                WasmCompiler.Module()
            num_funcs = length(module_.funcs)
            WasmCompiler.emit_func!(module_, $f, types;
                                    debug=$(debug),
                                    mode=$(wmod) === :malloc ? $(WasmCompiler.Malloc) :
                                                               $(WasmCompiler.GCProposal))
            if applicable(nameof, $f)
                WasmCompiler.export!(module_, string(nameof($f)), num_funcs + 1)
            end

            if $(esc(validate)) !== false
                WC.validate(module_)
            end

            if $(esc(optimize)) !== false
                lvl = !($(esc(optimize)) isa Int) ? 1 : $(esc(optimize))
                module_ = WasmCompiler.optimize!(module_, lvl)
            end

            if $(esc(optimize)) === :binaryen
                module_ = WasmCompiler.optimize(module_; debug=$debug)
            end

            Wat(module_, $(print_sexpr))
        end
    else
        quote
            types = Tuple{map(Core.Typeof, $(args))...}
            func = WasmCompiler.emit_func($f, types; debug=$debug)
            if $(esc(optimize)) !== false
                WasmCompiler.optimize_func!(func, $(esc(optimize)) === true ? 1 : $(esc(optimize)))
            end
            Wat(func, $(print_sexpr))
        end
    end
end

"""
    wat"(module)"

Parses the web assembly text content and returns a parsed module.

```julia-repl
julia> wat"(module)" |> WasmCompiler.wast
"(module)"
```
"""
macro wat_str(s)
    quote
        WC.parse_wast(IOBuffer($s))[1]::Module
    end
end 

wat(obj) = sprint(show, Wat(obj))
wast(obj::Module) = sprint(show, Wat(obj, true))

const WC = @__MODULE__

export @code_wasm, @wat_str, WC

end # module WasmCompiler
