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

struct Mem
    type::MemoryType
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

struct MemImport <: Import
    module_name::String
    name::String
    id::Union{Nothing,String}
    mem::Mem
end

struct FuncImport <: Import
    module_name::String
    name::String
    id::Union{Nothing,String}
    fntype::FuncType
end

struct GlobalImport <: Import
    module_name::String
    name::String
    id::Union{Nothing,String}
    type::GlobalType
end

struct TagImport <: Import
    module_name::String
    name::String
    id::Union{Nothing,String}
    fntype::FuncType
end

abstract type Export end

struct MemExport <: Export
    name::String
    mem::Index
end

struct FuncExport <: Export
    name::String
    func::Index
end

struct GlobalExport <: Export
    name::String
    globalidx::Index
end

struct TagExport <: Export
    name::String
    tagidx::Index
end

struct Table
    type::TableType
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

mutable struct Module
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
    strings::Vector{String}
end
Module() = Module([], [], [], [], [], [], [], nothing, [], [], [])
Module(func::Func) = Module(
        [], [func], [], [],
        [], [], [], nothing,
        [], [FuncExport(func.name::String, 1)],
        [],
    )

num_types(mod::Module) = sum(mod.types) do typ
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
    funcidx > length(wmod.funcs) && return nothing
    wmod.funcs[funcidx].fntype
end

export!(mod, name, index) =
    push!(mod.exports, FuncExport(name, count(imp -> imp isa FuncImport, mod.imports) + index))

function make_bootstrap()
    bootstrap_file = joinpath(@__DIR__, "..", "bootstrap.wast") |> normpath
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

function Base.map!(f, cont::Union{Func,Block,Loop,TryDelegate,TryTable})
    map!(cont.inst, cont.inst) do inst
        if inst isa ContainerInst
            v = f(inst)
            map!(f, inst)
            v
        else
            f(inst)
        end
    end
    cont
end
function Base.map!(f, try_::Try)
    map!(try_.inst, try_.inst) do inst
        if inst isa ContainerInst
            v = f(inst)
            map!(f, inst)
            v
        else
            f(inst)
        end
    end
    for c in try_.catches
        map!(c.inst, c.inst) do inst
            if inst isa ContainerInst
                v = f(inst)
                map!(f, inst)
                v
            else
                f(inst)
            end
        end
    end
    try_
end
function Base.map!(f, if_::If)
    map!(if_.trueinst, if_.trueinst) do inst
        if inst isa ContainerInst
            v = f(inst)
            map!(f, inst)
            v
        else
            f(inst)
        end
    end
    map!(if_.falseinst, if_.falseinst) do inst
        if inst isa ContainerInst
            v = f(inst)
            map!(f, inst)
            v
        else
            f(inst)
        end
    end
    if_
end

function Base.foreach(f, cont::Union{Func,Block,Loop,TryTable})
    for inst in cont.inst
        if inst isa ContainerInst
            f(inst)
            foreach(f, inst)
        else
            f(inst)
        end
    end
end
function Base.foreach(f, try_::Try)
    for inst in try_.inst
        if inst isa ContainerInst
            f(inst)
            foreach(f, inst)
        else
            f(inst)
        end
    end
    for c in try_.catches
        for inst in c.inst
            if inst isa ContainerInst
                f(inst)
                foreach(f, inst)
            else
                f(inst)
            end
        end
    end
    try_
end
function Base.foreach(f, if_::If)
    for inst in if_.trueinst
        if inst isa ContainerInst
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

function Base.filter!(f, cont::Union{Func,Block,Loop,TryDelegate,TryTable})
    filter!(cont.inst) do inst
        if inst isa ContainerInst
            v = f(inst)
            v && filter!(f, inst)
            v
        else
            f(inst)
        end
    end
    cont
end
function Base.filter!(f, try_::Try)
    filter!(try_.inst) do inst
        if inst isa ContainerInst
            v = f(inst)
            v && filter!(f, inst)
            v
        else
            f(inst)
        end
    end
    for c in try_.catches
        filter!(c.inst) do inst
            if inst isa ContainerInst
                v = f(inst)
                v && filter!(f, inst)
                v
            else
                f(inst)
            end
        end
    end
    try_
end
function Base.filter!(f, if_::If)
    filter!(if_.trueinst) do inst
        if inst isa ContainerInst
            v = f(inst)
            v && filter!(f, inst)
            v
        else
            f(inst)
        end
    end
    filter!(if_.falseinst) do inst
        if inst isa ContainerInst
            v = f(inst)
            v && filter!(f, inst)
            v
        else
            f(inst)
        end
    end
    if_
end
