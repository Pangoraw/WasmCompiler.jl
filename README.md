# WasmCompiler.jl

This is essentially a rewrite of [Charlotte.jl](https://github.com/MikeInnes/Charlotte.jl) (a lot has changed since 2018, both in WebAssembly and Julia land) targeting the new WebAssembly proposals ([gc](https://github.com/WebAssembly/gc),[exceptions](https://github.com/WebAssembly/exception-handling),...).

## Example

```julia
julia> using WasmCompiler

julia> relu(x) = ifelse(x < zero(x), zero(x), x)
relu (generic function with 1 method)

julia> @code_wasm relu(1f0)
(func $relu (param f32) (result f32)
  (local i32 f32)
  block 
    local.get 0
    f32.const 0.0
    f32.lt
    local.set 1
    f32.const 0.0
    local.get 0
    local.get 1
    select
    local.set 2
    local.get 2
    nop
    return
  end
  unreachable
)

julia> @code_wasm optimize=true relu(1f0)
(func $relu (param f32) (result f32)
  (local i32)
  local.get 0
  f32.const 0.0
  f32.lt
  local.set 1
  f32.const 0.0
  local.get 0
  local.get 1
  select
)

```

## References

 - https://webassembly.github.io/spec/core/
 - https://developer.mozilla.org/en-US/docs/WebAssembly/Reference
