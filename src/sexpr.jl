takes(_, _, ::Union{i32_const,i64_const,f32_const,f64_const,local_tee,local_get,global_get,unreachable}) = 0
takes(_, _, ::UnaryInst) = 1
takes(_, _, ::BinaryInst) = 2
takes(_, _, block::Block) = length(block.fntype.params)
takes(_, func, ::return_) = length(func.fntype.results)

produces(_, _, ::Union{unreachable,drop,nop,local_set,global_set}) = 0
produces(_, _, inst) = 1
produces(_, func, ::return_) = length(func.fntype.results)
produces(_, _, block::Union{If,Loop,Block}) = length(block.fntype.results)

struct InstOperands
    inst::Inst
    operands::Vector{InstOperands}
end

sexpr(wmod, func, expr=func.inst) = sexpr!(wmod, func, Inst[deepcopy(inst) for inst in expr])
function sexpr!(wmod, func, expr::Vector{Inst})
    out = InstOperands[]

    while !isempty(expr)
        inst = popfirst!(expr)
        to_take = takes(wmod, func, inst)

        operands = InstOperands[]

        taken = 0
        while taken < to_take
            op = pop!(out)
            prod = produces(wmod, func, op)
            iszero(prod) && error("invalid stack order")
            taken += prod

            pushfirst!(operands, op)
        end
        taken == to_take || error("could not handle values on stack")
        push!(out, InstOperands(inst, operands))
    end

    out
end
