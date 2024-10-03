import WebAssemblyToolkit as WAT
using WebAssemblyToolkit:
    Func, i32, i64, FuncType, Inst, local_set,
    local_get, i32_const, i64_const, drop,
    br, Block, If, nop,
    @code_wasm

import Wasmtime
using Wasmtime: WasmEngine, WasmStore, WasmModule, WasmInstance
using Wasmtime: WasmtimeStore, WasmtimeModule, WasmtimeInstance
using Wasmtime: exports
using Test

include("utils.jl")
include("try-catch.jl")
include("validate.jl")
include("parser.jl")
include("spec.jl")

@testset "add" begin
    function add(a, b)
        a + b
    end

    f = WAT.emit_func(add, Tuple{Int32,Int32})
    mod = WAT.Module(f)

    wasm = WAT.wasm(mod) |> Wasmtime.WasmByteVec

    engine = WasmEngine()
    store = WasmStore(engine)
    wmodule = WasmModule(store, wasm)
    instance = WasmInstance(store, wmodule)

    wadd = Wasmtime.exports(instance).add

    for _ in 1:10
        x, y = rand(Int32, 2)
        @test only(wadd(x, y)) == Wasmtime.WasmInt32(add(x, y))
    end
end

function func(a, b)
    x = if a >= Int32(2)
        b + a
    else
        b + Int32(2)
    end
    return x
end

@testset "conds" begin
    f = WAT.emit_func(func, Tuple{Int32,Int32})
    mod = WAT.Module(f)
    WAT.optimize!(mod, 1)

    wasm = WAT.wasm(mod)
    wasm = wasm |> Wasmtime.WasmByteVec

    engine = WasmEngine()
    store = WasmtimeStore(engine)
    wmodule = WasmtimeModule(engine, wasm)
    instance = WasmtimeInstance(store, wmodule)

    wfunc = Wasmtime.exports(instance).func

    for _ in 1:10
        x, y = rand(Int32, 2)
        @test only(wfunc(x, y)) == func(x, y)
    end
end

@testset "max" begin
    (; obj) = @code_wasm optimize=true mod=true max(0f0, 1)
    mod = obj

    wasm = WAT.wasm(mod) |> Wasmtime.WasmByteVec

    engine = Wasmtime.WasmEngine()
    store = Wasmtime.WasmStore(engine)
    wmodule = Wasmtime.WasmModule(store, wasm)
    instance = Wasmtime.WasmInstance(store, wmodule)

    wmax = Wasmtime.exports(instance).max
    @test convert(Float32, only(wmax(NaN32, 1))) |> isnan
    @test convert(Float32, only(wmax(+0f0,  0))) == 0f0
end

@testset "sqrt" begin
    (; obj) = @code_wasm optimize=true mod=:malloc sqrt(1f0)
    num_imports = count(imp -> imp isa WAT.FuncImport, obj.imports)
    empty!(obj.imports) # remove malloc/free imports
    map!(obj.exports, obj.exports) do exp # renumber func exports
        exp isa WAT.FuncExport || return exp
        WAT.FuncExport(exp.name, exp.func - num_imports)
    end
    mod = obj

    wasm = WAT.wasm(mod) |> Wasmtime.WasmByteVec

    engine = Wasmtime.WasmEngine()
    store = Wasmtime.WasmStore(engine)
    wmodule = Wasmtime.WasmModule(store, wasm)
    instance = Wasmtime.WasmInstance(store, wmodule)

    wsqrt = Wasmtime.exports(instance).sqrt

    @test convert(Float32, wsqrt(NaN32) |> only) |> isnan
    @test_throws Exception convert(Float32, only(wsqrt(-1f0)))
end

@testset "WAT: Instructions" begin
    insts = WAT.Inst[
        WAT.i32_load(),
        WAT.i32_store(),
        WAT.i32_reinterpret_f32(),
        WAT.f32_reinterpret_i32(),
    ]

    io = IOBuffer()
    WAT._printwasm(io, insts)
    wat = String(take!(io))

    @test occursin("i32.load", wat)
    @test occursin("i32.store", wat)
    @test occursin("i32.reinterpret_f32", wat)
    @test occursin("f32.reinterpret_i32", wat)
end

fac(n) = iszero(n) ? one(n) : fac(n-one(n)) * n

@testset "Recursive call" begin
    mod = WAT.Module()

    WAT.emit_func!(mod, fac, Tuple{Int32})
    WAT.export!(mod, "fac", findfirst(f -> f.name == "fac", mod.funcs))
    WAT.optimize!(mod, 1)

    wasm = WAT.wasm(mod) |> Wasmtime.WasmByteVec

    engine = WasmEngine()
    store = WasmtimeStore(engine)
    wmodule = WasmtimeModule(engine, wasm)
    instance = WasmtimeInstance(store, wmodule)

    wfac = Wasmtime.exports(instance).fac

    for x in Int32(0):Int32(10)
        @test only(wfac(x)) == fac(x)
    end
end

@noinline g(a, b) = a < 1 ? f(a, b) : b
f(a, b) = g(a - 1, b)

@testset "Mutually recursive functions" begin
    mod = WAT.Module()

    WAT.emit_func!(mod, f, Tuple{Int32,Int32})
    WAT.export!(mod, "f", findfirst(f -> f.name == "f", mod.funcs))
    WAT.optimize!(mod, 1)

    wasm = WAT.wasm(mod) |> Wasmtime.WasmByteVec

    engine = Wasmtime.WasmEngine()
    store = Wasmtime.WasmStore(engine)
    wmodule = Wasmtime.WasmModule(store, wasm)
    instance = Wasmtime.WasmInstance(store, wmodule)

    wf = Wasmtime.exports(instance).f

    x, y = Int32(10), Int32(42)
    @test f(x, y) == convert(Int32, wf(x, y) |> only)
end

@testset "Opt: Remove unused" begin
    func = Func("f", FuncType([], []), [i32, i64], [
        i64_const(2),
        local_set(2),
        i32_const(1),
        local_set(1),
        local_get(1),
        drop(),
        local_get(2),
    ])

    WAT.make_tees!(func)
    WAT.remove_unused!(func)

    @test length(func.locals) == 1
    @test only(func.locals) == i64
end

@testset "Merge untargeted blocks" begin
    code = Inst[
        Block(FuncType([], []), Inst[
            i32_const(1),
            i32_const(2),
            drop(),
            drop(),
            Block(FuncType([], []), Inst[
                br(1),
            ])
        ]),
        nop(),
    ]

    code = WAT._explore_blocks!(code, Int[])
    num_blocks = 0
    foreach(code) do inst
        inst isa Block && (num_blocks += 1)
    end
    @test num_blocks == 1
    @test last(first(code).inst) == br(0)
end

@testset "Remove useless branches" begin
    code = Inst[
        Block(FuncType([], []), Inst[
            Block(FuncType([], []), Inst[
                i32_const(1),
                drop(),
                br(1),
            ])
        ]),
    ]

    f = Func("f", FuncType([], []), [], code)
    WAT.remove_useless_branches!(f)
    WAT.merge_blocks!(f)

    @test length(code) == 2
    @test first(code) == i32_const(1)
    @test last(code) == drop()

    code = Inst[
        i32_const(1),
        drop(),
        br(0),
        i32_const(2),
        drop(),
    ]
    WAT._remove_useless_branches!(code)
    @test length(code) == 2
    @test first(code) == i32_const(1)
    @test last(code) == drop() 
end

mutable struct X
    x::Int
    y::Int
end

function g!(x)
    x.y = 10x.x + x.y
end

@testset "Mutable structs on memory" begin
    x = X(1,2)

    (; obj) = @code_wasm optimize=false sexpr=true mod=:malloc g!(x)
    num_imports = count(imp -> imp isa WAT.FuncImport, obj.imports)
    empty!(obj.imports) # remove malloc/free imports
    map!(obj.exports, obj.exports) do exp # renumber func exports
        exp isa WAT.FuncExport || return exp
        WAT.FuncExport(exp.name, exp.func - num_imports)
    end

    code = WAT.wasm(obj)

    engine = WasmEngine()
    store = Wasmtime.WasmtimeStore(engine)
    module_ = Wasmtime.WasmtimeModule(engine, code)
    instance = Wasmtime.WasmtimeInstance(store, module_)

    memory = exports(instance).memory
    wg! = exports(instance).g!

    buf = reinterpret(Int, memory)
    addr = 12
    buf[addr:addr+1] .= (x.x, x.y)
    @test buf[addr] == 1
    @test buf[addr+1] == 2

    @test wg!(Int32((addr - 1) * sizeof(Int))) == g!(x)
    #                       ↑
    #                       pass addr as zero-based pointer
    #                       in the wasm memory (i32).
    @test buf[addr] == x.x
    @test buf[addr+1] == x.y
end

getfield(i) = Core.getfield((1,2,3,), i, true)

@testset "Dynamic getfield" begin
    (; obj) = @code_wasm optimize=false mod=:malloc getfield(1)
    num_imports = count(imp -> imp isa WAT.FuncImport, obj.imports)
    empty!(obj.imports) # remove malloc/free imports
    map!(obj.exports, obj.exports) do exp # renumber func exports
        exp isa WAT.FuncExport || return exp
        WAT.FuncExport(exp.name, exp.func - num_imports)
    end

    code = WAT.wasm(obj)

    engine = WasmEngine()
    store = Wasmtime.WasmtimeStore(engine)
    module_ = Wasmtime.WasmtimeModule(engine, code)
    instance = Wasmtime.WasmtimeInstance(store, module_)

    memory = exports(instance).memory
    buf = reinterpret(Int, memory)
    wgetfield = exports(instance).getfield

    @test buf[1] == 1
    @test buf[2] == 2
    @test buf[3] == 3

    @test_throws ErrorException wgetfield(0) == 0
    @test_throws ErrorException wgetfield(4) == 0

    @test wgetfield(1) == 1
    @test wgetfield(2) == 2
    @test wgetfield(3) == 3
end

f(i) = isvalid("hello", i)

@testset "isvalid(::String, ::Int)" begin
    (; obj) = @code_wasm mod=:malloc optimize=:binaryen f(1)
    wasm = WAT.wasm(obj) |> Wasmtime.WasmByteVec

    mem_base = Int32(0xff)
    malloc_called = false

    function jlmalloc(sz)
        malloc_called = true
        ptr = mem_base
        mem_base += Int32(sz)
        ptr
    end

    engine = WasmEngine()
    store = WasmStore(engine)
    wmodule = WasmModule(store, wasm)
    instance = WasmInstance(store, wmodule, [Wasmtime.WasmFunc(store, jlmalloc, Int32, (Int32,))])

    wf = exports(instance).f

    @test only(wf(1)) == Wasmtime.WasmInt32(Int32(1))
    @test only(wf(1000)) == Wasmtime.WasmInt32(Int32(0))
    @test !malloc_called 
end

# Irreducible control flow
# since the loop 3 - 4 - 5
# has multiple entry points (3 & 4).
# 1
# ↓\
# 2 3
# ↓/↑
# 4 |
# ↓\|
# 6 5
# ↓
# 9
function ir_cfg(cond1, cond2)
    s = 0

    cond1 && @goto loop2
@label loop1

    s += 1

@label loop2

    s *= 2

    cond2 && @goto loop1

    return s
end

@testset "Irreducible control flow" begin
    (; obj) = @code_wasm mod=true ir_cfg(true,false)

    code = WAT.wasm(obj)

    engine = WasmEngine()
    store = Wasmtime.WasmtimeStore(engine)
    module_ = Wasmtime.WasmtimeModule(engine, code)
    instance = Wasmtime.WasmtimeInstance(store, module_)

    wir_cfg = exports(instance).ir_cfg
    @test wir_cfg(Int32(1), Int32(0)) == ir_cfg(true,false)
end

include("./pow.jl")
