using WasmCompiler: WModule, i32, local_get, f32_add, i32_add, drop, unreachable

@testset "validation" begin
    f = WC.Func("f",
        WC.FuncType([i32,i32], [i32]),
        [i32,i32],
        WC.Inst[
            local_get(1),
            local_get(2),
            f32_add(),
        ]
    )
    mod = WC.WModule(f)

    @test_throws "f32_add" WC.validate(mod)

    empty!(f.inst)
    append!(f.inst, WC.Inst[
        local_get(1),
        local_get(2),
        i32_add(),
        drop(),
    ])

    @test_throws "i32" WC.validate(mod)

    empty!(f.inst)
    append!(f.inst, WC.Inst[
        drop(),
    ])

    @test_throws "drop" WC.validate(mod)

    empty!(f.inst)
    append!(f.inst, WC.Inst[
        unreachable(),
    ])

    @test (WC.validate(mod); true)
end
