using WasmCompiler, Test

@testset "spectest: $p"  for p in ["i32.wast"]
    sexprs = open(WC.parse_wast, p)

    module_ = nothing
    inst = nothing

    for ex in sexprs
        if ex isa WC.Module
            module_ = ex
            inst = WC.Interpreter.instantiate(module_)
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
            inv, expected = args
            _, name, fargs = inv

            @test inv[1] === :invoke

            func_idx = findfirst(f -> f.name === name, module_.funcs)

            vargs = map(i -> i.val, fargs)
            exp = map(i -> i.val, expected)

            @test WC.Interpreter.invoke(
                inst, func_idx, vargs
            ) == exp
        end

    end


end
