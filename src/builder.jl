module Builder

using ..WebAssemblyToolkit

resolve(mod, Inst, arg) = arg
resolve(mod, ::Type{WAT.call}, arg::QuoteNode) = begin
    name = String(arg.value)
    count(exp -> exp isa WAT.FuncExport, mod.exports) +
        findfirst(f -> f.name == name, mod.funcs)
end

function fromexpr(mod, ex)
    if Meta.isexpr(ex, :call)
        Inst = getproperty(WebAssemblyToolkit, ex.args[1])
        @assert Inst <: WebAssemblyToolkit.Inst
        length(ex.args) == 1 && return Inst()

        args = Any[]
        for i in 1:fieldcount(Inst)
            arg = resolve(mod, Inst, ex.args[1 + i])
            push!(args, arg)
        end

        operands = WebAssemblyToolkit.InstOperands[]
        for arg in ex.args[2 + fieldcount(Inst):end]
            push!(operands, fromexpr(mod, arg))
        end

        return WebAssemblyToolkit.InstOperands(Inst(args...), operands, [])
    end
end

function toexpr(iop)
    if iop.inst isa WebAssemblyToolkit.Block
        return Expr(:block, iop.inst.fntype, (toexpr(op) for op in iop.blocks[1])...)
    end

    if  iop.inst isa WebAssemblyToolkit.Loop
        return Expr(:while, :true, toexpr(WebAssemblyToolkit.InstOperands(WebAssemblyToolkit.Block(iop.inst.fntype, []), [], [iop.blocks[1]])))
    end

    if iop.inst isa WebAssemblyToolkit.If
        return Expr(:if,
            toexpr(iop.operands |> only),
            toexpr(WAT.InstOperands(WAT.Block(iop.inst.fntype, []), [], [iop.blocks[1]])),
            toexpr(WAT.InstOperands(WAT.Block(iop.inst.fntype, []), [], [iop.blocks[2]])))
    end

    T = typeof(iop.inst)
    n = nameof(T)
    Expr(:call, n,
        (getproperty(iop.inst, f) for f in fieldnames(T))...,
        (toexpr(arg) for arg in iop.operands)...,
    )
end

end # module Builder
