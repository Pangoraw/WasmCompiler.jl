module Builder

using ..WasmCompiler

resolve(mod, Inst, arg) = arg
resolve(mod, ::Type{WasmCompiler.call}, arg::QuoteNode) = begin
    name = String(arg.value)
    count(exp -> exp isa WasmCompiler.FuncExport, mod.exports) +
        findfirst(f -> f.name == name, mod.funcs)
end

function fromexpr!(mod, inst, ex)
    if Meta.isexpr(ex, :call)
        Inst = getproperty(WasmCompiler, ex.args[1])
        @assert Inst <: WasmCompiler.Inst
        length(ex.args) == 1 && return Inst()

        args = Any[]
        for i in 1:fieldcount(Inst)
            arg = resolve(mod, Inst, ex.args[1 + i])
            push!(args, arg)
        end

        for arg in ex.args[2 + fieldcount(Inst):end]
            fromexpr!(mod, inst, arg)
        end

        push!(inst, Inst(args...))
    end
end

function toexpr(iop)
    if iop.inst isa WasmCompiler.Block
        return Expr(:block, iop.inst.fntype, (toexpr(op) for op in iop.blocks[1])...)
    end

    if  iop.inst isa WasmCompiler.Loop
        return Expr(:while, :true, toexpr(WasmCompiler.InstOperands(WasmCompiler.Block(iop.inst.fntype, []), [], [iop.blocks[1]])))
    end

    if iop.inst isa WasmCompiler.If
        return Expr(:if,
            toexpr(iop.operands |> only),
            toexpr(WasmCompiler.InstOperands(WasmCompiler.Block(iop.inst.fntype, []), [], [iop.blocks[1]])),
            toexpr(WasmCompiler.InstOperands(WasmCompiler.Block(iop.inst.fntype, []), [], [iop.blocks[2]])))
    end

    T = typeof(iop.inst)
    n = nameof(T)
    Expr(:call, n,
        (getproperty(iop.inst, f) for f in fieldnames(T))...,
        (toexpr(arg) for arg in iop.operands)...,
    )
end

end # module Builder
