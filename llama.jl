primitive type WasmArray{T,N,S} <: AbstractArray{T,N} 32 end

function WasmArray{T,N,S}(x::Int32) where {T,N,S}
    Base.bitcast(WasmArray{T,N,S}, x)
end

Base.Int32(x::WasmArray) = Base.bitcast(Int32, x)

Base.size(::WasmArray{T,N,S}) where {T,N,S} = S
@inline Base.setindex!(x::WasmArray{T}, v::T, i) where {T} = T_store(Int32(x) * Int32(i) * Int32(sizeof(T)), v)
@inline Base.getindex(x::WasmArray{T}, i) where {T} = T_load(Int32(x) * Int32(i) * Int32(sizeof(T)))::T

function rmsnorm(o, x, weight)
    ss = zero(eltype(o))

    for i in eachindex(x)
        el = x[i]
        ss += sum(el .* el)
    end

    ss /= length(x)
    ss += eps(ss)

    ss = one(ss) / sqrt(ss)

    for j in eachindex(o)
        o = weight[j] .* (ss .* x[j])
    end
end

function softmax(x)
    max_val = typemin(eltype(x))
    for j in eachindex(x)
        el = x[j]
        max_val = ifelse(el > max_val, el, max_val)
    end

    s = zero(eltype(x))
    for j in eachindex(x)
        el = exp(x[j] - max_val)
        x[j] = el
        s += sum(el)
    end

    for j in eachindex(x)
        el = x[j]
        x[j] = el / s
    end

    nothing
end

function matmul(xout, x, w)
    # x (n,) @ W (n,d) -> xout (d,)
    @inbounds for i in axes(w,2)
        val = zero(eltype(xout))
        for j in axes(w,1)
            val += w[j,i] * x[j]
        end
        xout[i] = val
    end
end

@noinline function T_load(x::Int32)
    Main.unknown()::Float32
end

@noinline function T_store(x::Int32, v::Float32)
    Main.unknown2()::Nothing
end
