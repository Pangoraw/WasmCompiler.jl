function f_try_catch(x)
    try
        sqrt(x)
    catch
        2x + 2
    end
end

@testset "trycatch" begin
    (; obj) = @code_wasm optimize=false mod=:runtime f_try_catch(-1f0)
    w = WC.wasm(obj)

    p = launch()
    instantiate!(p, w)

    @test call(p, "f_try_catch", -1f0) |> Float32 == f_try_catch(-1f0)
    @test call(p, "f_try_catch", 1f0) |> Float32 ==  f_try_catch(1f0)

    exit(p)
    # wait(p)
end
