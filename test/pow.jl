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

    inst = WC.Interpreter.instantiate(wmod)
    
    for p in [(3, 4), (0, 1), (1, 0), (2, 8)]
        x, n = Int32.(p)
        interp_result = WC.Interpreter.invoke(inst, 1, Any[x, n]) |> only
        expected_result = pow(x, n)
        @test convert(Int32, only(wpow(x, n))) == expected_result
        @test interp_result == expected_result
    end
end

@testset "Optimize ratio" begin
    mod_opt = (@code_wasm mod=true optimize=true pow(Int32(1), Int32(1))).obj
    mod_unopt = (@code_wasm mod=true optimize=false pow(Int32(1), Int32(1))).obj

    size_opt = length(WC.wasm(mod_opt; names=false, producers=false))
    size_unopt = length(WC.wasm(mod_unopt))
    size_opt2 = length(WC.wasm(WC.optimize(mod_unopt); names=false, producers=false))

    ratio = (size_unopt - size_opt) / size_opt
    ratio2 = (size_unopt - size_opt2) / size_opt2
    @test ratio >= 0.

    print("Unopt(pow)  ")
    printstyled(size_unopt, "B", color=:cyan)
    println()
    print("Opt(pow)    ")
    printstyled(size_opt, "B ", color=:cyan)
    printstyled(ratio > 0 ? "+" : "",
                round(100ratio; digits=0),
                "%";
                color=ratio > 0 ? :green : :red)
    println()
    print("Byen(pow)   ")
    printstyled(size_opt2, "B ", color=:cyan)
    printstyled(ratio2 > 0 ? "+" : "",
                round(100ratio2; digits=0),
                "%";
                color=ratio2 > 0 ? :green : :red)
    println()
end
