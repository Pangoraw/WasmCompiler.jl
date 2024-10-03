@testset "parser" begin
    input = "(module)"

    output = WAT.wast(WAT.parse_wast(IOBuffer(input))[1])

    @test input == output

    input = "(module) )"
    @test_throws "invalid" WAT.wast(WAT.parse_wast(IOBuffer(input))[1])
end
