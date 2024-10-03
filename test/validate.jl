using WebAssemblyToolkit: i32, local_get, f32_add, i32_add, drop, unreachable

@testset "validation" begin
    f = WAT.Func("f",
        WAT.FuncType([i32, i32], [i32]),
        [i32, i32],
        WAT.Inst[
            local_get(1),
            local_get(2),
            f32_add(),
        ]
    )
    mod = WAT.Module(f)

    @test_throws "type mismatch" WAT.validate(mod)

    empty!(f.inst)
    append!(f.inst, WAT.Inst[
        local_get(1),
        local_get(2),
        i32_add(),
        drop(),
    ])

    @test_throws "i32" WAT.validate(mod)

    empty!(f.inst)
    append!(f.inst, WAT.Inst[
        drop(),
    ])

    @test_throws "drop" WAT.validate(mod)

    empty!(f.inst)
    append!(f.inst, WAT.Inst[
        unreachable(),
    ])

    @test (WAT.validate(mod); true)
end
