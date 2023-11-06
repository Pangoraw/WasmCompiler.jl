takes(_, _, _, ::Union{i32_const,i64_const,f32_const,f64_const,local_get,global_get,unreachable,string_const,nop,ref_null}) = 0
takes(_, _, _, ::UnaryInst) = 1
takes(_, _, _, ::Union{local_set,local_tee,global_set,drop,ref_cast,struct_get,ref_test}) = 1
takes(_, _, _, ::Union{i32_store,i64_store,f32_store,f64_store,v128_store,struct_set,array_get}) = 2
takes(_, _, _, ::Union{i32_load, i64_load, f32_load, f64_load,
                       i32_load8_s, i32_load8_u, i32_load16_s, i32_load16_u,
                       i64_load8_s, i64_load8_u, i64_load16_s, i64_load16_u,
                       i64_load32_s, i64_load32_u,v128_load}) = 1
takes(_, _, _, ::Union{ref_as_non_null}) = 1
takes(_, _, _, ::memory_copy) = 3
takes(_, _, _, ::BinaryInst) = 2
takes(_, _, _, ::v128cmp) = 2
takes(_, _, ctx, b::Union{br,br_table,br_if}) = length(ctx[end-b.label].params)
takes(wmod, _, _, ::array_new) = 2 # elty, length
takes(wmod, _, _, sn::struct_new) = length(_find_type(wmod, sn.typeidx).fields)
function takes(wmod, _, _, (; tag)::throw_)
    for imp in wmod.imports
        imp isa TagImport || continue
        tag -= 1
        iszero(tag) && return length(imp.fntype.params)
    end

    length(wmod.tags[tag].fntype.params)
end
takes(wmod, _, _, c::call) = length(get_function_type(wmod, c.func).params)
takes(_, _, _, block::Union{Block,Loop,Try}) = length(block.fntype.params)
takes(_, _, _, if_::If) = length(if_.fntype.params) + 1
takes(_, func, _, ::return_) = length(func.fntype.results)
takes(_, _, _, ::select) = 3

produces(_, _, ::Union{unreachable,drop,nop,local_set,global_set,return_}) = 0
produces(_, _, ::Union{i32_store,i64_store,f32_store,f64_store}) = 0
produces(_, _, inst::Inst) = 1
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
    blocks::Vector{Vector{InstOperands}} # for if, blocks, loops...
end

sexpr(wmod, func, expr=func.inst, ctx=FuncType[]) = sexpr!(wmod, func, expr, ctx)
function sexpr!(wmod, func, expr::Vector{Inst}, ctx)
    out = InstOperands[]

    while !isempty(expr)
        inst = popfirst!(expr)
        inst isa nop && continue
        to_take = takes(wmod, func, ctx, inst)

        operands = InstOperands[]

        taken = 0
        while taken < to_take
            isempty(out) && error("stack is empty for expr ($(sprint(_printwasm, inst; context=(:mod => wmod, :indent => 0)))) taken $taken/$to_take params")
            op = pop!(out)
            prod = produces(wmod, func, op.inst)
            if iszero(prod)
                error("invalid stack order")
            end
            taken += prod

            pushfirst!(operands, op)
        end
        taken > to_take && error("could not handle values on stack")

        # empty form of inst
        inst, blocks = if inst isa Union{Block,Loop}
            push!(ctx, inst.fntype)
            T = typeof(inst)
            inst, blocks = T(copy(inst.fntype), Inst[]), [sexpr(wmod, func, inst.inst)]
            pop!(ctx, inst.fntype)
            inst, blocks
        elseif inst isa If
            push!(ctx, inst.fntype)
            inst, blocks = (
                If(copy(inst.fntype), Inst[], Inst[]), 
                Vector{InstOperands}[sexpr(wmod, func, inst.trueinst), sexpr(wmod, func, inst.falseinst)],
            )
            pop!(ctx, inst.fntype)
            inst, blocks
        else
            inst, Vector{InstOperands}[]
        end

        push!(out, InstOperands(inst, operands, blocks))
    end

    out
end

function emit!(out, op)
    foldl(emit!, op.operands; init=out)
    if op.inst isa Union{Loop,Block}
        T = typeof(op.inst)
        push!(
            T(op.inst.fntype, flatten(only(op.blocks)))
        )
    elseif op.inst isa If
        push!(
            If(op.inst.fntype, flatten(op.blocks[1]), flatten(op.blocks[2]))
        )
    else
        push!(out, op.inst)
    end
end
flatten(ops::Vector{InstOperands}) = foldl(emit!, ops; init=Inst[])
