module WasmCompiler

include("./wasm.jl")

struct A
    x::Int
    y::Int
end

wrepr(::Type{Int32}) = i32
wrepr(::Type{Int64}) = i64
wrepr(::Type{Float32}) = f32
wrepr(::Type{Float64}) = f64
wrepr(_) = ref

function generate_type(io::IO, T)
    mut = ismutabletype(T)

    println(io, "(type \$", nameof(T))
    println(io, "  (sub \$any")
    println(io, "    (struct")
    for (fname, fT) in zip(fieldnames(T), fieldtypes(T))
        println(io, "      (field \$", fname, " ", wrepr(fT), ")") 
    end
    print(io, ")))")
end

generate_type(stdout, A)

end # module WasmCompiler
