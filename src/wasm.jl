struct StructField
    type::ValType
    name::Union{Nothing,String}
    mut::Bool
end

abstract type GCType <: WasmType end

struct StructType <: GCType
    name::Union{Nothing,String}
    subidx::Union{Nothing,Index}
    fields::Vector{StructField}
end

struct ArrayType <: GCType
    name::Union{Nothing,String}
    mut::Bool
    content::ValType
end

struct RecursiveZone <: WasmType
    structs::Vector{GCType}
end

const voidtype = FuncType([], [])

struct MemoryType <: WasmType
    min::UInt32
    max::UInt32
end

struct TableType <: WasmType
    min::UInt32
    max::UInt32
    reftype::ValType
end

struct GlobalType <: WasmType
    mut::Bool
    type::ValType
end

struct Func
    name::Union{Nothing,String}
    fntype::FuncType
    locals::Vector{ValType}
    inst::Vector{Inst}
end

abstract type Import end

struct FuncImport <: Import
    module_name::String
    name::String
    id::String
    fntype::FuncType
end

struct GlobalImport <: Import
    module_name::String
    name::String
    id::String
    type::GlobalType
end

struct TagImport <: Import
    module_name::String
    name::String
    id::Union{Nothing,String}
    fntype::FuncType
end

abstract type Export end

struct FuncExport <: Export
    name::String
    func::Index
end

struct Table
    type::TableType
end
struct Mem
    type::MemoryType
end
struct Global
    name::Union{Nothing,String}
    type::GlobalType
    init::Vector{Inst}
end

abstract type ElemMode end
struct ElemModePassive <: ElemMode end
struct ElemModeActive <: ElemMode
    table::Index
    offset::Vector{Inst}
end
struct ElemModeDeclarative <: ElemMode end

struct Elem
    type::ValType
    init::Vector{Vector{Inst}}
    mode::ElemMode
end

abstract type DataMode end
struct DataModePassive <: DataMode end
struct DataModeActive <: DataMode
    memory::Index
    offset::Vector{Inst}
end

struct Data
    init::Vector{UInt8}
    mode::DataMode
end

mutable struct WModule
    types::Vector{WasmType}
    funcs::Vector{Func}
    tables::Vector{Table}
    mems::Vector{Mem}
    globals::Vector{Global}
    elems::Vector{Elem}
    datas::Vector{Data}
    start::Union{Nothing,Index}
    imports::Vector{Import}
    exports::Vector{Export}
end
WModule() = WModule([], [], [], [], [], [], [], nothing, [], [])
WModule(func::Func) = WModule(
        [], [func], [], [],
        [], [], [], nothing,
        [], [FuncExport(func.name::String, 1)],
    )

num_types(mod::WModule) = sum(mod.types) do typ
    typ isa StructType && return 1
    typ isa ArrayType && return 1
    typ isa RecursiveZone && return length(typ.structs)
    error("invalid type $typ")
end

function get_function_type(wmod, funcidx)
    n_imported = 0
    for funcimport in wmod.imports
        funcimport isa FuncImport || continue
        n_imported += 1

        n_imported == funcidx && return funcimport.fntype
    end

    funcidx -= n_imported
    wmod.funcs[funcidx].fntype
end

export!(mod, name, index) = push!(mod.exports, FuncExport(name, count(imp -> imp isa FuncImport, mod.imports) + index))

function make_bootstrap()
    bootstrap_file = joinpath(@__DIR__, "..", "bootstrap.wat") |> normpath
    bootstrap_output = joinpath(@__DIR__, "..", "bootstrap.wasm") |> normpath
    isfile(bootstrap_output) && return bootstrap_output
    run(`$(wasm_as()) --enable-gc --enable-strings --enable-exception-handling --enable-gc-nn-locals --enable-reference-types $bootstrap_file --output="$(bootstrap_output)"`)
    bootstrap_output
end

function towasm(io::IO, mod; opt=0)
    args = String["--enable-gc", "--enable-reference-types", "--enable-strings", "--enable-exception-handling"]
    wat = sprint(_printwasm, mod; context=(:mod => mod, :print_sexpr => true))
    bootstrap_file = make_bootstrap()
    run(pipeline(IOBuffer(wat),
        `$(wasm_as()) -g $(args) - --output="-"`,
        `$(wasm_merge()) -g $(args) $(bootstrap_file) bootstrap - module --output="-"`,
        `$(wasm_opt()) -g -O$opt $(args) - --output="-"`,
        io,
    ))
    io
end
towasm(f::String, mod; kwargs...) = open(io -> towasm(io, mod; kwargs...), f; write=true)

function towasm(mod; kwargs...)
    io = IOBuffer()
    towasm(io, mod; kwargs...)
    take!(io)
end

## Utilities

function Base.map!(f, cont::Union{Func,Block,Loop})
    map!(cont.inst, cont.inst) do inst
        if inst isa Union{Block,Loop,If}
            v = f(inst)
            map!(f, inst)
            v
        else
            f(inst)
        end
    end
    cont
end
function Base.map!(f, if_::If)
    map!(if_.trueinst, if_.trueinst) do inst
        if inst isa Union{Block,Loop,If}
            v = f(inst)
            map!(f, inst)
            v
        else
            f(inst)
        end
    end
    map!(if_.falseinst, if_.falseinst) do inst
        if inst isa Union{Block,Loop,If}
            v = f(inst)
            map!(f, inst)
            v
        else
            f(inst)
        end
    end
    if_
end

function Base.foreach(f, cont::Union{Func,Block,Loop})
    for inst in cont.inst
        if inst isa Union{Block,Loop,If}
            f(inst)
            foreach(f, inst)
        else
            f(inst)
        end
    end
end
function Base.foreach(f, if_::If)
    for inst in if_.trueinst
        if inst isa Union{Block,Loop,If}
            f(inst)
            foreach(f, inst)
        else
            f(inst)
        end
    end
    for inst in if_.falseinst
        if inst isa Union{Block,Loop,If}
            f(inst)
            foreach(f, inst)
        else
            f(inst)
        end
    end
end

function Base.filter!(f, cont::Union{Func,Block,Loop})
    filter!(cont.inst) do inst
        if inst isa Union{Block,Loop,If}
            v = f(inst)
            v && filter!(f, inst)
            v 
        else
            f(inst)
        end
    end
    cont
end
function Base.filter!(f, if_::If)
    filter!(if_.trueinst) do inst
        if inst isa Union{Block,Loop,If}
            v = f(inst)
            v && filter!(f, inst)
            v 
        else
            f(inst)
        end
    end
    filter!(if_.falseinst) do inst
        if inst isa Union{Block,Loop,If}
            filter!(f, inst)
            true
        else
            f(inst)
        end
    end
    if_
end
