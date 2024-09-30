module ConstantPropagation

using ..WasmCompiler: InstOperands, f32_const, f64_const, i32_const, i64_const, select
using ..WasmCompiler.Interpreter: Runtime

const ConstInst = Union{f32_const,f64_const,i32_const,i64_const}
unwrap(ci::ConstInst) = ci.val

mkconst(f::Float32) = f32_const(f)
mkconst(f::Float64) = f64_const(f)
mkconst(i::Int32) = i32_const(i)
mkconst(i::Int64) = i64_const(i)

function const_prop!(ops)
    for (i, op) in enumerate(ops)
        const_prop!(op.operands)
        foreach(const_prop!, op.blocks)
        if all(cop -> cop.inst isa ConstInst, op.operands)
            inst_name = nameof(typeof(op.inst))
            if isdefined(Runtime, inst_name)
                impl = getproperty(Runtime, inst_name)
                val = impl((unwrap(cop.inst) for cop in op.operands)...)
                ops[i] = InstOperands(mkconst(val), [])
            elseif op.inst isa select
                a, b, cond = op.operands
                ops[i] = iszero(unwrap(cond)) ? b : a
            end
        end
    end
    ops
end

end # module ConstantPropagation
