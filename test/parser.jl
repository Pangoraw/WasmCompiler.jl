@testset "parser" begin
    input = "(module)"

    output = WC.wast(WC.parse_wast(IOBuffer(input))[1])

    @test input == output
end