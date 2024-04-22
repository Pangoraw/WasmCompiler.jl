using WasmCompiler, Test

const testsuite_dir = joinpath(@__DIR__, "testsuite")

@testset "spectest: $(basename(p))" for p in filter!(Returns(true), readdir(testsuite_dir; join=true))
    sexprs = open(WC.parse_wast, p)

    module_ = nothing
    inst = nothing

    for ex in sexprs
        if ex isa WC.Module
            module_ = ex

            @test begin
                WC.validate(module_)
                true
            end

            @test begin
                inst = WC.Interpreter.instantiate(module_)
                true
            end
            continue
        end

        head, args... = ex
        if head === :assert_invalid
            mod, msg = args
            wat = WC.Wat(mod)

            @testset let wat = wat
                @test try WC.validate(mod); false catch e; true end
                @test_throws msg WC.validate(mod)
            end
        elseif head === :assert_return
            module_ === nothing && continue
            inst === nothing && continue

            inv, expected = args
            _, name, fargs = inv

            @test inv[1] === :invoke

            func_idx = findfirst(f -> f.name === name, module_.funcs)

            vargs = map(i -> i.val, fargs)
            exp = map(i -> i.val, expected)

            wat = WC.Wat(inst.mod)

            @testset let fn=name, vargs=vargs
                @test WC.Interpreter.invoke(
                    inst, func_idx, vargs
                    ) == exp skip = contains(name, "call_indirect") || contains(name, "memory.grow")
            end
        end

    end


end
