import Core.Compiler: widenconst, PiNode, PhiNode, ReturnNode,
    GotoIfNot, GotoNode, SSAValue, UpsilonNode, PhiCNode

abstract type WasmType end

struct FuncType <: WasmType
    params::Vector{ValType}
    results::Vector{ValType}
end

struct StructField
    type::ValType
    name::Union{Nothing,String}
    mut::Bool
end

struct StructType <: WasmType
    name::Union{Nothing,String}
    fields::Vector{StructField}
end

struct RecursiveZone <: WasmType
    structs::Vector{StructType}
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
WModule(func::Func) = WModule(
        [], [func], [], [],
        [], [], [], nothing,
        [], [FuncExport(func.name::String, 1)],
    )

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

