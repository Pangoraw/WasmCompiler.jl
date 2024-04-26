module Runtime

# For conditionals
i32_to_bool(v::Int32) = !iszero(v)

f32_abs(a::Float32) = abs(a)
f64_abs(a::Float64) = abs(a)
f32_neg(a::Float32) = -a
f64_neg(a::Float64) = -a
f32_sqrt(a::Float32) = sqrt(a)
f64_sqrt(a::Float64) = sqrt(a)
f32_ceil(a::Float32) = ceil(a)
f64_ceil(a::Float64) = ceil(a)
f32_floor(a::Float32) = floor(a)
f64_floor(a::Float64) = floor(a)
f32_trunc(a::Float32) = trunc(a)
f64_trunc(a::Float64) = trunc(a)
f32_nearest(a::Float32) = round(a, RoundNearest)
f64_nearest(a::Float64) = round(a, RoundNearest)

f32_add(a::Float32, b::Float32) = a + b
f64_add(a::Float64, b::Float64) = a + b
f32_sub(a::Float32, b::Float32) = a - b
f64_sub(a::Float64, b::Float64) = a - b
f32_mul(a::Float32, b::Float32) = a * b
f64_mul(a::Float64, b::Float64) = a * b
f32_div(a::Float32, b::Float32) = a / b
f64_div(a::Float64, b::Float64) = a / b
f32_min(a::Float32, b::Float32) = min(a, b)
f64_min(a::Float64, b::Float64) = min(a, b)
f32_max(a::Float32, b::Float32) = max(a, b)
f64_max(a::Float64, b::Float64) = max(a, b)
f32_copysign(a::Float32, b::Float32) = copysign(a, b)
f64_copysign(a::Float64, b::Float64) = copysign(a, b)

f32_eq(a::Float32, b::Float32) = Int32(a == b)
f64_eq(a::Float64, b::Float64) = Int32(a == b)
f32_ne(a::Float32, b::Float32) = Int32(a != b)
f64_ne(a::Float64, b::Float64) = Int32(a != b)
f32_lt(a::Float32, b::Float32) = Int32(a < b)
f64_lt(a::Float64, b::Float64) = Int32(a < b)
f32_le(a::Float32, b::Float32) = Int32(a <= b)
f64_le(a::Float64, b::Float64) = Int32(a <= b)
f32_gt(a::Float32, b::Float32) = Int32(a > b)
f64_gt(a::Float64, b::Float64) = Int32(a > b)
f32_ge(a::Float32, b::Float32) = Int32(a >= b)
f64_ge(a::Float64, b::Float64) = Int32(a >= b)

i32_clz(a::Int32) = Base.leading_zeros(a)
i64_clz(a::Int64) = Base.leading_zeros(a)
i32_ctz(a::Int32) = Base.trailing_zeros(a)
i64_ctz(a::Int64) = Base.trailing_zeros(a)
i32_popcnt(a::Int32) = Base.count_ones(a)
i64_popcnt(a::Int64) = Base.count_ones(a)
i32_eqz(a::Int32) = Int32(iszero(a))
i64_eqz(a::Int64) = Int32(iszero(a))

i32_add(a::Int32, b::Int32) = a + b
i64_add(a::Int64, b::Int64) = a + b
i32_sub(a::Int32, b::Int32) = a - b
i64_sub(a::Int64, b::Int64) = a - b
i32_mul(a::Int32, b::Int32) = a * b
i64_mul(a::Int64, b::Int64) = a * b
i32_div_u(a::Int32, b::Int32) = reinterpret(Int32, div(reinterpret(UInt32, a), reinterpret(UInt32, b)))
i64_div_u(a::Int64, b::Int64) = reinterpret(Int64, div(reinterpret(UInt64, a), reinterpret(UInt64, b)))
i32_div_s(a::Int32, b::Int32) = a ÷ b
i64_div_s(a::Int64, b::Int64) = a ÷ b
i32_rem_u(a::Int32, b::Int32) = iszero(b) ? b : reinterpret(Int32, rem(reinterpret(UInt32, a), reinterpret(UInt32, b)))
i64_rem_u(a::Int64, b::Int64) = iszero(b) ? b : reinterpret(Int64, rem(reinterpret(UInt64, a), reinterpret(UInt64, b)))
i32_rem_s(a::Int32, b::Int32) = iszero(b) ? b : rem(a, b)
i64_rem_s(a::Int64, b::Int64) = iszero(b) ? b : rem(a, b)
i32_and(a::Int32, b::Int32) = a & b
i64_and(a::Int64, b::Int64) = a & b
i32_or(a::Int32, b::Int32) = a | b
i64_or(a::Int64, b::Int64) = a | b
i32_xor(a::Int32, b::Int32) = a ⊻ b
i64_xor(a::Int64, b::Int64) = a ⊻ b
i32_shl(a::Int32, b::Int32) = a << mod(b, Int32(32))
i64_shl(a::Int64, b::Int64) = a << mod(b, Int64(64))
i32_shr_u(a::Int32, b::Int32) = a >>> mod(b, Int32(32))
i64_shr_u(a::Int64, b::Int64) = a >>> mod(b, Int64(64))
i32_shr_s(a::Int32, b::Int32) = a >> mod(b, Int32(32))
i64_shr_s(a::Int64, b::Int64) = a >> mod(b, Int64(64))
i32_rotl(a::Int32, b::Int32) = bitrotate(a, mod(b, Int32(32)))
i64_rotl(a::Int64, b::Int64) = bitrotate(a, mod(b, Int64(64)))
i32_rotr(a::Int32, b::Int32) = bitrotate(a, -mod(b, Int32(32)))
i64_rotr(a::Int64, b::Int64) = bitrotate(a, -mod(b, Int64(64)))

i32_eq(a::Int32, b::Int32)   = Int32(a == b)
i32_ne(a::Int32, b::Int32)   = Int32(a != b)
i32_lt_u(a::Int32, b::Int32) = Int32(reinterpret(UInt32, a) < reinterpret(UInt32, b))
i32_lt_s(a::Int32, b::Int32) = Int32(a < b)
i32_gt_u(a::Int32, b::Int32) = Int32(reinterpret(UInt32, a) > reinterpret(UInt32, b))
i32_gt_s(a::Int32, b::Int32) = Int32(a > b)
i32_le_u(a::Int32, b::Int32) = Int32(reinterpret(UInt32, a) <= reinterpret(UInt32, b))
i32_le_s(a::Int32, b::Int32) = Int32(a <= b)
i32_ge_u(a::Int32, b::Int32) = Int32(reinterpret(UInt32, a) >= reinterpret(UInt32, b))
i32_ge_s(a::Int32, b::Int32) = Int32(a >= b)
i64_eq(a::Int64, b::Int64)   = Int32(a == b)
i64_ne(a::Int64, b::Int64)   = Int32(a != b)
i64_lt_u(a::Int64, b::Int64) = Int32(reinterpret(UInt64, a) < reinterpret(UInt64, b))
i64_lt_s(a::Int64, b::Int64) = Int32(a < b)
i64_gt_u(a::Int64, b::Int64) = Int32(reinterpret(UInt64, a) > reinterpret(UInt64, b))
i64_gt_s(a::Int64, b::Int64) = Int32(a > b)
i64_le_u(a::Int64, b::Int64) = Int32(reinterpret(UInt64, a) <= reinterpret(UInt64, b))
i64_le_s(a::Int64, b::Int64) = Int32(a <= b)
i64_ge_u(a::Int64, b::Int64) = Int32(reinterpret(UInt64, a) >= reinterpret(UInt64, b))
i64_ge_s(a::Int64, b::Int64) = Int32(a >= b)

f32_convert_i32_u(a::Int32) = Float32(reinterpret(UInt32, a))
f32_convert_i32_s(a::Int32) = Float32(a)
f32_convert_i64_u(a::Int64) = Float32(reinterpret(UInt64, a))
f32_convert_i64_s(a::Int64) = Float32(a)
f64_convert_i32_s(a::Int32) = Float64(a)
f64_convert_i32_u(a::Int32) = Float64(reinterpret(UInt32, a))
f64_convert_i64_s(a::Int64) = Float64(a)
f64_convert_i64_u(a::Int64) = Float64(reinterpret(UInt64, a))

f32_reinterpret_i32(a::Int32) = reinterpret(Float32, a)
f64_reinterpret_i64(a::Int64) = reinterpret(Float64, a)

i64_trunc_f64_s(a::Float64) = Int64(floor(a))

f32_demote_f64(a::Float64) = Float32(a)
f64_promote_f32(a::Float32) = Float64(a)

i32_wrap_i64(a::Int64) = (a & typemax(UInt32)) % Int32

i32_extend8_s(a::Int32) = Int32(a % Int8)
i32_extend16_s(a::Int32) = Int32(a % Int16)

i64_extend8_s(a::Int64) = Int64(a % Int8)
i64_extend16_s(a::Int64) = Int64(a % Int16)
i64_extend32_s(a::Int64) = Int64(a % Int32)

i64_extend_i32_s(a::Int32) = Int64(a)
i64_extend_i32_u(a::Int32) = reinterpret(Int64, UInt64(reinterpret(UInt32, a)))

end # module Runtime
