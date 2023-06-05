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

    f = WC.emit_func(func, Tuple{Int32,Int32})
    mod = WC.WModule(
        [], [f], [], [],
        [], [], [], nothing,
        [], [WC.FuncExport("func", 1)],
    )

    wat = sprint(WC._printwasm, mod)
    println(wat)

    wasm = Wasmtime.wat2wasm(wat)

    #=
    wasm = Wasmtime.wat"""
    (module
    (func $func (param i32) (param i32) (result i32)
            (local i32)
            (local i32)
            (local i32)
            (local i32)
        i32.const 2
        local.get 0
        i64.le_s

        if
            local.get 1
            local.get 0
            i32.add
            local.set 5
        else
            local.get 1
            i32.const 2
            i32.add
            local.set 5
        end

        local.get 5
    )


    (export "func" (func $func)))
    """
    =#

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

