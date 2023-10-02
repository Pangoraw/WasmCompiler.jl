takes(_, _, ::Union{i32_const,i64_const,f32_const,f64_const,local_get,global_get,unreachable,string_const,nop,br}) = 0
takes(_, _, ::UnaryInst) = 1
takes(_, _, ::Union{local_set,local_tee,global_set,drop,ref_cast,struct_get}) = 1
takes(_, _, ::Union{i32_store,i64_store,f32_store,f64_store,v128_store}) = 2
takes(_, _, ::Union{i32_load, i64_load, f32_load, f64_load,
                    i32_load8_s, i32_load8_u, i32_load16_s, i32_load16_u,
                    i64_load8_s, i64_load8_u, i64_load16_s, i64_load16_u,
                    i64_load32_s, i64_load32_u,v128_load}) = 1
takes(_, _, ::BinaryInst) = 2
takes(_, _, ::v128cmp) = 2
function takes(wmod, _, (; tag)::throw_)
    for imp in wmod.imports
        imp isa TagImport || continue
        tag -= 1
        iszero(tag) && return length(imp.fntype.params)
    end

    length(wmod.tags[tag].fntype.params)
end
takes(wmod, _, c::call) = length(get_function_type(wmod, c.func).params)
takes(_, _, block::Union{Block,Loop}) = length(block.fntype.params)
takes(_, _, if_::If) = length(if_.fntype.params) + 1
takes(_, func, ::return_) = length(func.fntype.results)
takes(_, _, ::select) = 3

produces(_, _, ::Union{unreachable,drop,nop,local_set,global_set,return_,nop}) = 0
produces(_, _, ::Union{i32_load, i64_load, f32_load, f64_load,
                       i32_load8_s, i32_load8_u, i32_load16_s, i32_load16_u,
                       i64_load8_s, i64_load8_u, i64_load16_s, i64_load16_u,
                       i64_load32_s, i64_load32_u,v128_load}) = 1
produces(_, _, ::Union{i32_store,i64_store,f32_store,f64_store}) = 0
produces(_, _, inst) = 1
produces(wmod, _, c::call) = length(get_function_type(wmod, c.func).results)
produces(_, _, block::Union{If,Loop,Block}) = length(block.fntype.results)

"""
    InstOperands(::Inst, operands::Vector{InstOperands})

This struct represent a node in sexpr notation similarly to Binaryen IR which does not
rely on the implicit stack.
"""
struct InstOperands
    inst::Inst
    operands::Vector{InstOperands}
end

sexpr(wmod, func, expr=func.inst) = sexpr!(wmod, func, Inst[deepcopy(inst) for inst in expr])
function sexpr!(wmod, func, expr::Vector{Inst})
    out = InstOperands[]

    while !isempty(expr)
        inst = popfirst!(expr)
        inst isa nop && continue
        to_take = takes(wmod, func, inst)

        operands = InstOperands[]

        taken = 0
        while taken < to_take
            isempty(out) && error("stack is empty for expr ($(sprint(_printwasm, inst; context=(:mod => wmod, :indent => 0))))")
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

function emit!(out, op)
    foldl(emit!, op.operands; init=out)
    push!(out, op.inst)
end
flatten(ops::Vector{InstOperands}) = foldl(emit!, ops; init=Inst[])
