takes(_, ::Union{i32_const,i64_const,f32_const,f64_const,local_tee,local_get,global_get,unreachable}) = 0
takes(_, inst) = 1
takes(_, block::Block) = length(block.fntype.params)

produces(_, ::Union{drop,nop,local_set,global_set}) = 0
produces(_, inst) = 1
produces(_, block::Block) = length(block.fntype.results)

struct InstOperands
    inst::Inst
    operands::Vector{InstOperands}
end

sexpr(wmod, expr) = sexpr!(wmod, Inst[deepcopy(inst) for inst in expr])
function sexpr!(wmod, expr::Vector{Inst})
    out = InstOperands[]

    while !isempty(expr)
        inst = popfirst!(expr)
        to_take = takes(wmod, inst)

        operands = InstOperands[]

        taken = 0
        while taken < to_take
            op = pop!(out)
            prod = produces(wmod, op)
            iszero(prod) && error("invalid stack order")
            taken += prod

            pushfirst!(operands, op)
        end
        taken == to_take || error("could not handle values on stack")
        push!(out, InstOperands(inst, operands))
    end

    out
end
