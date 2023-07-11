function pow(x, n)
    s = one(x)
    for _ in 1:n
        s *= x
    end
    s
end

@testset "Export pow" begin
    wat = @code_wasm mod=true pow(Int32(2), Int32(3))
    wmod = wat.obj

    wasm = WC.wasm(wmod) |> Wasmtime.WasmByteVec

    engine = WasmEngine()
    store = WasmtimeStore(engine)
    wmodule = WasmtimeModule(engine, wasm)
    instance = WasmtimeInstance(store, wmodule)

    wpow = Wasmtime.exports(instance).pow

    for (x, n) in [(3, 4), (0, 1), (1, 0), (2, 8)]
        @test convert(Int32, only(wpow(Int32(x), Int32(n)))) == Int32(x ^ n)
    end
end
