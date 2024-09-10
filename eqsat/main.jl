using Metatheory
using Metatheory.Library
using TermInterface

using WasmCompiler

ex = WC.InstOperands(
    WC.i32_mul(),
    [WC.InstOperands(WC.i32_const(2), [], []),
        WC.InstOperands(WC.local_get(1), [], [])],
    [],
)

mkconst(i::Int32) = Expr(:call, :i32_const, i)
mkconst(i::Int64) = Expr(:call, :i64_const, i)
mkconst(f::Float32) = Expr(:call, :f32_const, f)
mkconst(f::Float64) = Expr(:call, :f64_const, f)

t = @theory a begin
    i32_mul(a, i32_const(2)) --> i32_shl(a, i32_const(1))

    i32_eq(a, i32_const($(Int32(0)))) --> i32_eqz(a)
    i32_eq(i32_const($(Int32(0))), a) --> i32_eqz(a)

    i64_eq(a, i64_const(0)) --> i64_eqz(a)
    i64_eq(i64_const(0), a) --> i64_eqz(a)
end

# Const prop
t = t ∪ @theory a b begin
    i32_add(i32_const(a::Int32), i32_const(b::Int32)) => mkconst(WC.Interpreter.Runtime.i32_add(a, b))
    i32_mul(i32_const(a::Int32), i32_const(b::Int32)) => mkconst(WC.Interpreter.Runtime.i32_mul(a, b))
    i32_sub(i32_const(a::Int32), i32_const(b::Int32)) => mkconst(WC.Interpreter.Runtime.i32_sub(a, b))
    i32_div(i32_const(a::Int32), i32_const(b::Int32)) => mkconst(WC.Interpreter.Runtime.i32_div(a, b))

    i64_add(i64_const(a::Int64), i64_const(b::Int64)) => mkconst(WC.Interpreter.Runtime.i64_add(a, b))
    i64_sub(i64_const(a::Int64), i64_const(b::Int64)) => mkconst(WC.Interpreter.Runtime.i64_sub(a, b))
    i64_mul(i64_const(a::Int64), i64_const(b::Int64)) => mkconst(WC.Interpreter.Runtime.i64_mul(a, b))
    i64_div(i64_const(a::Int64), i64_const(b::Int64)) => mkconst(WC.Interpreter.Runtime.i64_div(a, b))
end

t = t ∪ @commutative_monoid (i32_add) i32_const(Int32(0))
t = t ∪ @commutative_monoid (i32_mul) i32_const(Int32(1))

t = t ∪ @commutative_monoid (i64_add) i64_const(0)
t = t ∪ @commutative_monoid (i64_mul) i64_const(1)

t = t ∪ @commutative_monoid (f32_add) f32_const(0f0)
t = t ∪ @commutative_monoid (f32_mul) f32_const(1f0)
t = t ∪ @commutative_monoid (f64_add) f64_const(0.0)
t = t ∪ @commutative_monoid (f64_mul) f64_const(1.0)

function toexpr(iop)
    iop.inst isa WC.i32_const && return Expr(:call, :i32_const, iop.inst.val)
    iop.inst isa WC.local_get && return Expr(:call, :local_get, iop.inst.n)
    Expr(:call, nameof(typeof(iop.inst)), toexpr.(iop.operands)...)
end

function cost_function(n, g)
    exprhead(n) === :call || return 0
    operation(n) === :i32_mul ? (return 2) : (return 1)
end

g = EGraph(toexpr(ex))
saturate!(g, t)
extract!(g, cost_function)
