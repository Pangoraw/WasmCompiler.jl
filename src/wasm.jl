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

function jl_to_struct(T)
    mut = ismutabletype(T)
    StructType(nameof(T) |> string, [
        StructField(valtype(FT), FN isa Symbol ? string(FN) : nothing, mut)
        for (FT, FN) in zip(fieldtypes(T), fieldnames(T))
    ])
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
    type::FuncType
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

export!(mod, name, index) = push!(mod.exports, FuncExport(name, index))

function towasm(io::IO, mod; opt=0, enable_gc=false, enable_reference_types=false)
    args = String[]
    enable_gc && push!(args, "--enable-gc")
    enable_reference_types && push!(args, "--enable-reference-types")
    wat = sprint(_printwasm, mod)
    run(pipeline(IOBuffer(wat),
        `wat2wasm $(args...) - --output="-"`,
        `wasm-opt -O$opt $(args...) - --output="-"`,
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
            map!(f, inst)
        else
            f(inst)
        end
    end
    cont
end
function Base.map!(f, if_::If)
    map!(if_.trueinst, if_.trueinst) do inst
        if inst isa Union{Block,Loop,If}
            map!(f, inst)
        else
            f(inst)
        end
    end
    map!(if_.falseinst, if_.falseinst) do inst
        if inst isa Union{Block,Loop,If}
            map!(f, inst)
        else
            f(inst)
        end
    end
    if_
end

function Base.foreach(f, cont::Union{Func,Block,Loop})
    for inst in cont.inst
        if inst isa Union{Block,Loop,If}
            foreach(f, inst)
        else
            f(inst)
        end
    end
end
function Base.foreach(f, if_::If)
    for inst in if_.trueinst
        if inst isa Union{Block,Loop,If}
            foreach(f, inst)
        else
            f(inst)
        end
    end
    for inst in if_.falseinst
        if inst isa Union{Block,Loop,If}
            foreach(f, inst)
        else
            f(inst)
        end
    end
end

function Base.filter!(f, cont::Union{Func,Block,Loop})
    filter!(cont.inst) do inst
        if inst isa Union{Block,Loop,If}
            filter!(f, inst)
            true
        else
            f(inst)
        end
    end
    cont
end
function Base.filter!(f, if_::If)
    filter!(if_.trueinst) do inst
        if inst isa Union{Block,Loop,If}
            filter!(f, inst)
            true
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
