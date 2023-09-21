primitive type WasmArray{T,N,S} <: AbstractArray{T,N} 32 end

function WasmArray{T,N,S}(x::Int32) where {T,N,S}
    Base.bitcast(WasmArray{T,N,S}, x)
end

primitive type Vec{N,T} 128 end

Base.Int32(x::WasmArray) = Base.bitcast(Int32, x)

Base.size(::WasmArray{T,N,S}) where {T,N,S} = S
@inline Base.setindex!(x::WasmArray{T}, v, i) where {T} =
    (T_store(Int32(x) + Int32(i - one(Int32)) * Int32(sizeof(T)), convert(T, v)); v)
@inline Base.getindex(x::WasmArray{T}, i, other...) where {T} = T_load(T, Int32(x) + Int32(i - one(Int32)) * Int32(sizeof(T)))::T

@inline Base.getindex(x::WasmArray{T}, i, other...) where {T<:Vec}= 
    _wasmcall(T, (WC.v128_load(),), Int32(x) + Int32(i - one(Int32)) * Int32(16))
# T_load(WasmCompiler.WasmVector128, Int32(x) + Int32(i - one(Int32)) * Int32(16))::WasmCompiler.WasmVector128


const f32x4 = Vec{4,Float32}

Vec(f::Float32) = _wasmcall(f32x4, (WC.f32x4_splat(),), f)
Vec(i::Int32) = _wasmcall(i32x4, (WC.i32x4_splat(),), i)

import Base: +, -, *, /
a::f32x4 + b::f32x4 = _wasmcall(f32x4, (WC.f32x4_add(),), a, b)
a::f32x4 - b::f32x4 = _wasmcall(f32x4, (WC.f32x4_sub(),), a, b)
a::f32x4 * b::f32x4 = _wasmcall(f32x4, (WC.f32x4_mul(),), a, b)
a::f32x4 / b::f32x4 = _wasmcall(f32x4, (WC.f32x4_div(),), a, b)

@inline Base.sum(::Vec{N,T}) where {N,T} = convert(T, 42)

function rmsnorm(o, x, weight)
    ss = zero(eltype(o))

    T = eltype(x)

    bT = 8sizeof(T) 

    # -- Vector size
    # .. Float32 => 4,
    # .. Float64 => 8
    N = 128 ÷ bT

    ℓ = length(x)

    if ℓ % N == 0
        vec = WasmArray{Vec{N,T},1,(ℓ ÷ N,)}(Int32(x))
        for i in eachindex(vec)
            v = vec[i]
            ss += sum(v * v)
        end
    else
        for i in eachindex(x)
            el = x[i]
            ss += el * el
        end
    end

    ss /= length(x)
    ss += eps(ss)

    ss = one(ss) / sqrt(ss)

    for j in eachindex(o)
        o = weight[j] .* (ss .* x[j])
    end
end

function softmax(x)
    T = eltype(x)
    max_val = typemin(T)

    for j in eachindex(x)
        el = x[j]
        max_val = ifelse(el > max_val, el, max_val)
    end

    bT = 8sizeof(T) # Float32 => 4, Float64 => 8
    N = 128 ÷ bT
    ℓ = length(x)

    s = zero(T)

    for j in eachindex(x)
        el = exp(x[j] - max_val)
        x[j] = el
        s += el
    end

    if ℓ % N == 0
        vec = WasmArray{Vec{N,T},1,(ℓ ÷ N,)}(Int32(x))
        svec = Vec(s)
        for i in eachindex(vec)
            el = vec[i]
            vec[i] = el / svec
        end
    else
        for j in eachindex(x)
            el = x[j]
            x[j] = el / s
        end
    end

    nothing
end

function matmul(xout, x, w)
    # x (n,) @ W (n,d) -> xout (d,)
    @inbounds for i in axes(w, 2)
        val = zero(eltype(xout))
        for j in axes(w, 1)
            val += w[j, i] * x[j]
        end
        xout[i] = val
    end
end

@noinline function _wasmcall(t, insts, args...)
    Main.unknown()::t
end

@noinline function T_load(t, x::Int32)
    Main.unknown()::t
end

@noinline function T_store(x::Int32, v)
    Main.unknown2(v)
    nothing
end

n, d = 4, 20
x = WasmArray{Float32,1,(n,)}(Int32(0));
nothing
