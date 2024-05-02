using WasmCompiler, Test

const testsuite_dir = joinpath(@__DIR__, "testsuite")
const filters = ["binary", "bulk", "call_indirect", "const", "conversions",
                 "custom", "data", "elem", "endian", "exports", "f32", "f64", "float_",
                 "func", "global", "imports", "int_lite", "left-to-right", "linking",
                 "local_tee", "memory", "names", "nop", "ref_is_null", "ref_null", "select",
                 "simd_", "stack", "start", "table", "token", "unreachable", "unreachable-invalid",
                 "unreached-invalid", "unreached-valid", "unwind", "utf8"]

@testset "spectest: $(basename(p))" for p in filter!(p -> endswith(p, ".wast") && all(f -> !contains(p, f),filters), readdir(testsuite_dir; join=true))
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
                skip = contains(name, "call_indirect") || contains(name, "extern")
                if any(isnan, exp)
                    @test isequal(WC.Interpreter.invoke(inst, func_idx, vargs), exp) skip=skip
                else
                    @test WC.Interpreter.invoke(inst, func_idx, vargs) == exp skip=skip
                end
            end
        end

    end


end
