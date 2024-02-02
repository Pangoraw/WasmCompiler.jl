"""
    i64x2(x₁::Int64, x₂::Int64)

Creates a simd v128 vector from two Int64.
"""
i64x2(x₁, x₂) = Tuple(reinterpret(UInt8, Int64[x₁,x₂]))
"""
    i64x2(v)::Tuple{Int64,Int64}

Interprets the byte values in the simd vector as two Int64.
"""
function i64x2(v)
    x₁, x₂ = 0, 0
    for i in 1:sizeof(Float64)
        x₁ |= Int64(v[i]) << (8 * (i-1))
        x₂ |= Int64(v[i+sizeof(Float64)]) << (8 * (i-1))
    end
    (x₁, x₂)
end

"""
    f64x2(x₁::Float64, x₂::Float64)

Creates a simd v128 vector from two Float64.
"""
f64x2(x₁, x₂) = Tuple(reinterpret(UInt8, Float64[x₁, x₂]))
"""
    f64x2(v)::Tuple{Float64,Float64}

Interprets the byte values in the simd vector as two Float64.
"""
function f64x2(v)
    x₁, x₂ = i64x2(v)
    (reinterpret(Float64,x₁),
     reinterpret(Float64,x₂))
end

"""
    i32x4(x₁::Int32, x₂::Int32, x₃::Int32, x₄::Int32)

Creates a simd v128 vector from four Int32.
"""
i32x4(x₁, x₂, x₃, x₄) = Tuple(reinterpret(UInt8, Int32[x₁, x₂, x₃, x₄]))
"""
    i32x4(v)::Tuple{Int32,Int32,Int32,Int32}

Interprets the byte values in the simd vector as four Int32.
"""
function i32x4(v)
    x₁, x₂, x₃, x₄ = zeros(Int32, 4)
    for i in 1:sizeof(Float32)
        x₁ |= Int32(v[i]) << (8 * (i-1))
        x₂ |= Int32(v[i+1sizeof(Float32)]) << (8 * (i-1))
        x₃ |= Int32(v[i+2sizeof(Float32)]) << (8 * (i-1))
        x₄ |= Int32(v[i+3sizeof(Float32)]) << (8 * (i-1))
    end
    (x₁,x₂,x₃,x₄)
end

"""
    f32x4(x₁::Float32, x₂::Float32, x₃::Float32, x₄::Float32)

Creates a simd v128 vector from four Float32.
"""
f32x4(x₁,x₂,x₃,x₄) = Tuple(reinterpret(UInt8, Float32[x₁,x₂,x₃,x₄]))
"""
    f32x4(v)::Tuple{Float32,Float32,Float32,Float32}

Interprets the byte values in the simd vector as four Float32.
"""
function f32x4(v)
    x₁, x₂, x₃, x₄ = i32x4(v)
    Tuple(reinterpret(Float32,x) for x in (x₁,x₂,x₃,x₄))
end

"""
    i16x8(x₁, x₂, x₃, x₄, x₅, x₆, x₇, x₈)

Creates a simd v128 vector from eight Int16.
"""
i16x8(x::Vararg{Int16,8}) = Tuple(reinterpret(UInt8, Int16[x...]))
"""
   i16x8(v)::NTuple{8,Int16}

Interprets the byte values in the simd vector as eight Int16.
"""
function i16x8(v)
    Tuple(reinterpret(Int16, UInt8[v...]))
end