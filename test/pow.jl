function pow(x, n)
    s = one(x)
    for _ in 1:n
        s *= x
    end
    s
end

@testset "Export pow" begin
    wat = @code_wasm mod=true sexpr=true pow(Int32(2), Int32(3))
    WC.towasm(wat.obj; opt=0)
end
