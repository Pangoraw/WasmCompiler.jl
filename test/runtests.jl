import WasmCompiler as WC
import Wasmtime
using Test

@testset "add" begin
    function add(a, b)
        a + b
    end

    f = WC.emit_func(add, Tuple{Int32,Int32})
    mod = WC.WModule(
        [], [f], [], [],
        [], [], [], nothing,
        [], [WC.FuncExport("add", 1)],
    )

    wat = sprint(WC._printwasm, mod)
    wasm = Wasmtime.wat2wasm(wat)

    engine = Wasmtime.WasmEngine()
    store = Wasmtime.WasmStore(engine)
    wmodule = Wasmtime.WasmModule(store, wasm)
    instance = Wasmtime.WasmInstance(store, wmodule)

    wadd = Wasmtime.exports(instance).add

    for _ in 1:10
        x, y = rand(Int32, 2)
        @test only(wadd(x, y)) == Wasmtime.WasmInt32(add(x, y))
    end
end

@testset "conds" begin
    function func(a, b)
        x = if a >= Int32(2)
            b + a
        else
            b + Int32(2)
        end
        return x
    end

    f = WC.emit_func(func, Tuple{Int32,Int32}; optimize=true)
    mod = WC.WModule(f)

    wat = sprint(WC._printwasm, mod)
    wasm = Wasmtime.wat2wasm(wat)

    engine = Wasmtime.WasmEngine()
    store = Wasmtime.WasmStore(engine)
    wmodule = Wasmtime.WasmModule(store, wasm)
    instance = Wasmtime.WasmInstance(store, wmodule)

    wfunc = Wasmtime.exports(instance).func

    for _ in 1:10
        x, y = rand(Int32, 2)
        @test only(wfunc(x, y)) == Wasmtime.WasmInt32(func(x, y))
    end
end

@testset "WAT: Instructions" begin
    insts = WC.Inst[
        WC.i32_load(),
        WC.i32_store(),
        WC.i32_reinterpret_f32(),
        WC.f32_reinterpret_i32(),
    ]

    io = IOBuffer()
    WC._printwasm(io, insts)
    wat = String(take!(io))

    @test occursin("i32.load", wat)
    @test occursin("i32.store", wat)
    @test occursin("i32.reinterpret_f32", wat)
    @test occursin("f32.reinterpret_i32", wat)
end

fac(n) = iszero(n) ? one(n) : fac(n-one(n)) * n

@testset "Recursive call" begin
    f = WC.emit_func(fac, Tuple{Int32}; optimize=true)
    mod = WC.WModule(f)

    wat = sprint(WC._printwasm, mod)
    wasm = Wasmtime.wat2wasm(wat)

    engine = Wasmtime.WasmEngine()
    store = Wasmtime.WasmStore(engine)
    wmodule = Wasmtime.WasmModule(store, wasm)
    instance = Wasmtime.WasmInstance(store, wmodule)

    wfac = Wasmtime.exports(instance).fac

    for x in Int32(0):Int32(10)
        @test convert(Int32, only(wfac(x))) == fac(x)
    end
end

@noinline g(a, b) = a < 1 ? f(a, b) : b
f(a, b) = g(a - 1, b)

@testset "mutually recursive functions" begin
    mod = WC.WModule()

    f = WC.emit_func!(mod, fac, Tuple{Int32,Int32}; optimize=true)
    export!(mod, "f", findfirst(f -> f.name == "f", mod.funcs))

    wat = sprint(WC._printwasm, mod)
    wasm = Wasmtime.wat2wasm(wat)

    engine = Wasmtime.WasmEngine()
    store = Wasmtime.WasmStore(engine)
    wmodule = Wasmtime.WasmModule(store, wasm)
    instance = Wasmtime.WasmInstance(store, wmodule)

    wf = Wasmtime.exports(instance).f

    x = Int32(10)
    @test f(x) == wf(x)
end
