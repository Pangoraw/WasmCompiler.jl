takes(_, _, _, ::Union{i32_const,i64_const,f32_const,f64_const,local_get,global_get,unreachable,string_const,nop,ref_null}) = 0
takes(_, _, _, ::Union{local_set,local_tee,global_set,drop,ref_cast,struct_get,ref_test}) = 1
takes(_, _, _, ::Union{i32_store,i64_store,f32_store,f64_store,v128_store,struct_set,array_get}) = 2
takes(_, _, _, ::Union{i32_load, i64_load, f32_load, f64_load,
                       i32_load8_s, i32_load8_u, i32_load16_s, i32_load16_u,
                       i64_load8_s, i64_load8_u, i64_load16_s, i64_load16_u,
                       i64_load32_s, i64_load32_u,v128_load}) = 1
takes(_, _, _, ::Union{ref_as_non_null}) = 1
takes(
    mod, func, ctx, inst
) = length(inst_func_type(FnValidator(mod, func, func.fntype, false, [], ctx), inst).params)
takes(_, _, ctx, b::Union{br,br_table}) = length(ctx[end-b.label].params)
takes(_, _, ctx, b::br_if) = 1 + length(ctx[end-b.label].params)
takes(wmod, _, _, ::array_new) = 2 # elty, length
takes(wmod, _, _, sn::struct_new) = length(_find_type(wmod, sn.typeidx).fields)
function takes(wmod, _, _, (; tag)::throw_)
    for imp in wmod.imports
        imp isa TagImport || continue
        tag -= 1
        iszero(tag) && return length(imp.fntype.params)
    end

    error("invalid tag $tag")
    # length(wmod.tags[tag].fntype.params)
end
takes(wmod, _, _, c::call) = length(get_function_type(wmod, c.func).params)
takes(_, _, _, block::Union{Block,Loop,Try,TryTable}) = length(block.fntype.params)
takes(_, _, _, if_::If) = length(if_.fntype.params) + 1
takes(_, func, _, ::return_) = length(func.fntype.results)
takes(_, _, _, ::select) = 3

produces(
    mod, func, ctx, inst
) = length(inst_func_type(FnValidator(mod, func, func.fntype, false, [], ctx), inst).results)
produces(_, _, _, ::Union{unreachable,drop,nop,local_set,global_set,return_}) = 0
produces(_, _, _, ::Union{i32_store,i64_store,f32_store,f64_store}) = 0
produces(wmod, _, _, c::call) = length(get_function_type(wmod, c.func).results)
produces(_, _, _, block::Union{If,Loop,Block,TryTable}) = length(block.fntype.results)

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

sexpr(wmod, func, expr=func.inst, ctx=FuncType[]) = sexpr!(wmod, func, copy(expr), ctx)
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
            prod = produces(wmod, func, ctx, op.inst)
            if iszero(prod)
                error("invalid stack order")
            end
            taken += prod

            pushfirst!(operands, op)
        end
        taken != to_take && error("could not handle values on stack")

        # empty form of inst
        inst, blocks = if inst isa Union{Block,Loop}
            push!(ctx, inst.fntype)
            T = typeof(inst)
            inst, blocks = T(copy(inst.fntype), Inst[]), [sexpr(wmod, func, inst.inst, ctx)]
            pop!(ctx)
            inst, blocks
        elseif inst isa TryTable
            push!(ctx, inst.fntype)
            T = typeof(inst)
            inst, blocks = T(copy(inst.fntype), Inst[], inst.handlers, inst.catch_all, inst.catch_all_ref), [sexpr(wmod, func, inst.inst, ctx)]
            pop!(ctx)
            inst, blocks
        elseif inst isa If
            push!(ctx, inst.fntype)
            inst, blocks = (
                If(copy(inst.fntype), Inst[], Inst[]), 
                Vector{InstOperands}[sexpr(wmod, func, inst.trueinst, ctx), sexpr(wmod, func, inst.falseinst, ctx)],
            )
            pop!(ctx)
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
            out,
            T(op.inst.fntype, flatten(only(op.blocks)))
        )
    elseif op.inst isa If
        push!(
            out,
            If(op.inst.fntype, flatten(op.blocks[1]), flatten(op.blocks[2]))
        )
    else
        push!(out, op.inst)
    end
end
flatten(ops::Vector{InstOperands}; init=Inst[]) = foldl(emit!, ops; init)

# --- Analysis

zero_inst(valtype) = if valtype == i32
    i32_const(0)
elseif valtype == i64
    i64_const(0)
elseif valtype == f32
    f32_const(0f0)
elseif valtype == f64
    f64_const(0.)
else
    error("valtype")
end

mutable struct OpDeps
    calls::BitSet
    locals::BitSet
    globals::BitSet
    effectful::Bool
end
OpDeps() = OpDeps(BitSet(), BitSet(), BitSet(), false)

function analyse(op, deps=OpDeps())
    op.inst isa Union{local_get,local_tee} && push!(deps.locals, op.inst.n)
    op.inst isa global_get && push!(deps.globals, op.inst.n)
    deps.effectful |= op.inst isa Union{global_set,local_set,call}
    foreach(Base.Fix2(analyse, deps), op.operands)
    deps
end

function inline_ssa_values!(wmod, func)
    ops = sexpr(wmod, func)

    nparams = length(func.fntype.params)
    local_values = [InstOperands(i <= nparams ? unreachable() : zero_inst(loc), [], [])
                    for (i, loc) in enumerate(func.locals)]
    local_reads = zeros(UInt, length(func.locals))

    function explore(op)
        if op.inst isa local_set || op.inst isa local_tee
            local_values[op.inst.n] = only(op.operands)
        end
        if op.inst isa local_get
            local_reads[op.inst.n] += 1
        end
        foreach(explore, op.operands)
    end

    foreach(explore, ops)

    movable = map(1:length(func.locals)) do loc
        loc > nparams && local_reads[loc] == 1
    end

    locals = 1:length(func.locals)
    live = falses(length(func.locals))
    moved = falses(length(func.locals))
    deps = Dict(
        i => analyse(local_values[i])
        for i in 1:length(func.locals)
        if movable[i]
    )

    function explore2(op)
        if (op.inst isa local_set || op.inst isa local_tee)
            loc = op.inst.n
            if movable[loc]
                live[loc] = true
            end

            for (i, d) in deps
                movable[i] || continue
                live[i] || continue

                if loc ∈ d.locals
                    live[i] = false
                end
            end
        end

        if op.inst isa local_get && movable[op.inst.n] && live[op.inst.n]
            loc = op.inst.n
            newop = local_values[loc]
            live[loc] = false
            moved[loc] = true
            return newop
        end

        map!(explore2, op.operands, op.operands)

        op
    end

    map!(explore2, ops, ops)

    nmoved = count(moved)
    nmoved > 0 && @debug "moved $(nmoved) locals" name=func.name

    function explore3(op)
        if op.inst isa local_set || op.inst isa local_get
            loc = op.inst.n
            if moved[loc]
                return InstOperands(nop(), [], [])
            end
        end

        map!(explore3, op.operands, op.operands)
        return op
    end

    map!(explore3, ops, ops)

    empty!(func.inst)
    flatten(ops; init=func.inst)

    remove_unused!(func)
end
