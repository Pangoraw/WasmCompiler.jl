import WasmCompiler as WC
using WasmCompiler: Func, i32, FuncType, local_set, local_get, i32_const, drop
using WasmCompiler: @code_wasm

import Wasmtime
import Wasmtime: WasmEngine, WasmStore, WasmModule, WasmInstance
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

    wasm = WC.wasm(mod) |> Wasmtime.WasmByteVec

    engine = WasmEngine()
    store = WasmStore(engine)
    wmodule = WasmModule(store, wasm)
    instance = WasmInstance(store, wmodule)

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

    wasm = WC.wasm(mod) |> Wasmtime.WasmByteVec

    engine = WasmEngine()
    store = WasmStore(engine)
    wmodule = WasmModule(store, wasm)
    instance = WasmInstance(store, wmodule)

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
    mod = WC.WModule()

    WC.emit_func!(mod, fac, Tuple{Int32}; optimize=true)
    WC.export!(mod, "fac", findfirst(f -> f.name == "fac", mod.funcs))

    wasm = WC.wasm(mod) |> Wasmtime.WasmByteVec

    engine = WasmEngine()
    store = WasmStore(engine)
    wmodule = WasmModule(store, wasm)
    instance = WasmInstance(store, wmodule)

    wfac = Wasmtime.exports(instance).fac

    for x in Int32(0):Int32(10)
        @test convert(Int32, only(wfac(x))) == fac(x)
    end
end

@noinline g(a, b) = a < 1 ? f(a, b) : b
f(a, b) = g(a - 1, b)

@testset "Mutually recursive functions" begin
    mod = WC.WModule()

    WC.emit_func!(mod, f, Tuple{Int32,Int32}; optimize=true)
    WC.export!(mod, "f", findfirst(f -> f.name == "f", mod.funcs))

    wasm = WC.wasm(mod) |> Wasmtime.WasmByteVec

    engine = Wasmtime.WasmEngine()
    store = Wasmtime.WasmStore(engine)
    wmodule = Wasmtime.WasmModule(store, wasm)
    instance = Wasmtime.WasmInstance(store, wmodule)

    wf = Wasmtime.exports(instance).f

    x, y = Int32(10), Int32(42)
    @test f(x, y) == convert(Int32, wf(x, y) |> only)
end

@testset "Opt: Remove unused" begin
    func = Func("f", FuncType([], []), [i32, i32], [
        i32_const(2),
        local_set(2),
        i32_const(1),
        local_set(1),
        local_get(1),
        drop(),
        local_get(2),
    ])

    WC.make_tees!(func)
    WC.remove_unused!(func)

    @test length(func.locals) == 1
end

include("./pow.jl")
