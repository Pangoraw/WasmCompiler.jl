module LEB128
# Implementations from https://en.wikipedia.org/wiki/LEB128
function encode(io::IO, value::Signed)
    n = 0
    while true
        byte = UInt8(value & 0x7F)
        value = value >> 7
        if (value == 0 && (byte & 0x40) == 0) ||
           (value == -1 && (byte & 0x40) != 0)
            n += write(io, byte)
            break
        end
        n += write(io, byte | 0x80)
    end
    n
end

function encode(io::IO, value::Unsigned)
    n = 0
    while true
        byte = UInt8(value & 0x7F)
        value = value >> 7
        if value != 0 # More bytes to come
            byte |= 0x80
        end
        n += write(io, byte)

        value == 0 && break
    end
    n
end

decode(io::IO, T::Type{<:Signed}) = read_signed(T, io)
decode(io::IO, T) = read_unsigned(T, io)

# https://en.wikipedia.org/wiki/LEB128
function read_unsigned(T, io::IO)
    result = zero(T)
    shift = 0

    while true
        byte = read(io, UInt8)
        result |= T(byte & 0x7f) << shift

        if (byte & 0x80) == 0x00
            break
        end

        shift += 7
    end

    result
end

function read_signed(T, io::IO)
    result = zero(T)
    shift = 0

    while true
        byte = read(io, UInt8)
        result |= T(byte & 0x7f) << shift
        shift += 7

        if (byte & 0x80) == 0x00
            if shift < sizeof(T) && (byte & 0x40) != 0x00
                return result | (~0 << shift)
            end
            return result
        end
    end
end
end

const MAGIC = UInt8[0x00, 0x61, 0x73, 0x6D]
const WASM_VERSION = UInt8[0x01, 0x00, 0x00, 0x00]

wwrite(io::IO, a, b, args...) = wwrite(io, a) + wwrite(io, b) + sum(arg -> wwrite(io, arg), args; init=0)
wwrite(io::IO, x::UInt8) = write(io, x)
wwrite(io::IO, x::Integer) = LEB128.encode(io, x)
wwrite(io::IO, ::WasmInt32) = write(io, 0x7F)
wwrite(io::IO, ::WasmInt64) = write(io, 0x7E)
wwrite(io::IO, ::WasmFloat32) = write(io, 0x7D)
wwrite(io::IO, ::WasmFloat64) = write(io, 0x7C)
wwrite(io::IO, ::WasmVector128) = write(io, 0x7B)

function write_ref_type(io::IO, nr::WasmRef)
    types = Dict(
        NoFuncRef => 0x73,
        NoExternRef => 0x72,
        NoneRef => 0x71,
        FuncRef => 0x70,
        ExternRef => 0x6f,
        AnyRef => 0x6e,
        EqRef => 0x6d,
        I31Ref => 0x6c,
        StringRef => 0x67,
    )
    wwrite(io, types[typeof(nr)])
end
write_ref_type(io::IO, sr::Union{ArrayRef,StructRef}) = LEB128.encode(io, Int32(sr.typeidx - one(Int32)))

function wwrite(io::IO, nr::WasmRef)
    n = 0
    if nr.null
        n += wwrite(io, 0x63)
    else
        n += wwrite(io, 0x64)
    end
    n += write_ref_type(io, nr)
end

const opcodes = Dict{Any,UInt8}(
    unreachable => 0x00,
    nop => 0x01,
    return_ => 0x0F,
    drop => 0x1A,
    select => 0x1B,
    i32_eqz => 0x45,
    i32_eq => 0x46,
    i32_ne => 0x47,
    i32_lt_s => 0x48,
    i32_lt_u => 0x49,
    i32_gt_s => 0x4A,
    i32_gt_u => 0x4B,
    i32_le_s => 0x4C,
    i32_le_u => 0x4D,
    i32_ge_s => 0x4E,
    i32_ge_u => 0x4F,
    i64_eqz => 0x50,
    i64_eq => 0x51,
    i64_ne => 0x52,
    i64_lt_s => 0x53,
    i64_lt_u => 0x54,
    i64_gt_s => 0x55,
    i64_gt_u => 0x56,
    i64_le_s => 0x57,
    i64_le_u => 0x58,
    i64_ge_s => 0x59,
    i64_ge_u => 0x5A,
    f32_eq => 0x5B,
    f32_ne => 0x5C,
    f32_lt => 0x5D,
    f32_gt => 0x5E,
    f32_le => 0x5F,
    f32_ge => 0x60,
    f64_eq => 0x61,
    f64_ne => 0x62,
    f64_lt => 0x63,
    f64_gt => 0x64,
    f64_le => 0x65,
    f64_ge => 0x66,
    i32_clz => 0x67,
    i32_ctz => 0x68,
    i32_popcnt => 0x69,
    i32_add => 0x6A,
    i32_sub => 0x6B,
    i32_mul => 0x6C,
    i32_div_s => 0x6D,
    i32_div_u => 0x6E,
    i32_rem_s => 0x6F,
    i32_rem_u => 0x70,
    i32_and => 0x71,
    i32_or => 0x72,
    i32_xor => 0x73,
    i32_shl => 0x74,
    i32_shr_s => 0x75,
    i32_shr_u => 0x76,
    i32_rotl => 0x77,
    i32_rotr => 0x78,
    i64_clz => 0x79,
    i64_ctz => 0x7A,
    i64_popcnt => 0x7B,
    i64_add => 0x7C,
    i64_sub => 0x7D,
    i64_mul => 0x7E,
    i64_div_s => 0x7F,
    i64_div_u => 0x80,
    i64_rem_s => 0x81,
    i64_rem_u => 0x82,
    i64_and => 0x83,
    i64_or => 0x84,
    i64_xor => 0x85,
    i64_shl => 0x86,
    i64_shr_s => 0x87,
    i64_shr_u => 0x88,
    i64_rotl => 0x89,
    i64_rotr => 0x8A,
    f32_abs => 0x8B,
    f32_neg => 0x8C,
    f32_ceil => 0x8D,
    f32_floor => 0x8E,
    f32_trunc => 0x8F,
    f32_nearest => 0x90,
    f32_sqrt => 0x91,
    f32_add => 0x92,
    f32_sub => 0x93,
    f32_mul => 0x94,
    f32_div => 0x95,
    f32_min => 0x96,
    f32_max => 0x97,
    f32_copysign => 0x98,
    f64_abs => 0x99,
    f64_neg => 0x9A,
    f64_ceil => 0x9B,
    f64_floor => 0x9C,
    f64_trunc => 0x9D,
    f64_nearest => 0x9E,
    f64_sqrt => 0x9F,
    f64_add => 0xA0,
    f64_sub => 0xA1,
    f64_mul => 0xA2,
    f64_div => 0xA3,
    f64_min => 0xA4,
    f64_max => 0xA5,
    f64_copysign => 0xA6,
    i32_wrap_i64 => 0xA7,
    i32_trunc_f32_s => 0xA8,
    i32_trunc_f32_u => 0xA9,
    i32_trunc_f64_s => 0xAA,
    i32_trunc_f64_u => 0xAB,
    i64_extend_i32_s => 0xAC,
    i64_extend_i32_u => 0xAD,
    i64_trunc_f32_s => 0xAE,
    i64_trunc_f32_u => 0xAF,
    i64_trunc_f64_s => 0xB0,
    i64_trunc_f64_u => 0xB1,
    f32_convert_i32_s => 0xB2,
    f32_convert_i32_u => 0xB3,
    f32_convert_i64_s => 0xB4,
    f32_convert_i64_u => 0xB5,
    f32_demote_f64 => 0xB6,
    f64_convert_i32_s => 0xB7,
    f64_convert_i32_u => 0xB8,
    f64_convert_i64_s => 0xB9,
    f64_convert_i64_u => 0xBA,
    f64_promote_f32 => 0xBB,
    i32_reinterpret_f32 => 0xBC,
    i64_reinterpret_f64 => 0xBD,
    f32_reinterpret_i32 => 0xBE,
    f64_reinterpret_i64 => 0xBF,
    i32_extend8_s => 0xC0,
    i32_extend16_s => 0xC1,
    i64_extend8_s => 0xC2,
    i64_extend16_s => 0xC3,
    i64_extend32_s => 0xC4,
    ref_is_null => 0xd1,
    ref_eq => 0xd3,
    ref_as_non_null => 0xd4,
)

wwrite(io::IO, ::T) where {T<:Inst} = haskey(opcodes, T) ? write(io, opcodes[T]) : error("could not emit instruction $T")
wwrite(io::IO, lg::local_get) = wwrite(io, 0x20, lg.n - one(lg.n))
wwrite(io::IO, lg::local_set) = wwrite(io, 0x21, lg.n - one(lg.n))
wwrite(io::IO, lg::local_tee) = wwrite(io, 0x22, lg.n - one(lg.n))
wwrite(io::IO, lg::global_get) = wwrite(io, 0x23, lg.n - one(lg.n))
wwrite(io::IO, lg::global_set) = wwrite(io, 0x24, lg.n - one(lg.n))

wwrite(io::IO, c::i32_const) = wwrite(io, 0x41, c.val)
wwrite(io::IO, c::i64_const) = wwrite(io, 0x42, c.val)
wwrite(io::IO, c::f32_const) = write(io, 0x43, c.val)
wwrite(io::IO, c::f64_const) = write(io, 0x44, c.val)


const MemoryOp = Union{i32_load,i64_load,f32_load,f64_load,
    i32_load8_s,i32_load8_u,i32_load16_s,i32_load16_u,
    i64_load8_s,i64_load8_u,i64_load16_s,i64_load16_u,
    i64_load32_s,i64_load32_u,
    i32_store,i64_store,f32_store,f64_store}

function wwrite(io::IO, op::MemoryOp)
    ops = [i32_load, i64_load, f32_load, f64_load,
        i32_load8_s, i32_load8_u, i32_load16_s, i32_load16_u,
        i64_load8_s, i64_load8_u, i64_load16_s, i64_load16_u,
        i64_load32_s, i64_load32_u,
        i32_store, i64_store, f32_store, f64_store]
    wwrite(io, 0x27 + UInt8(findfirst(==(typeof(op)), ops)), op.memarg)
end


wwrite(io::IO, ::memory_copy) = wwrite(io, 0xfc, UInt32(10), 0x00, 0x00)

function wwrite(io::IO, c::v128_const)
    n = wwrite(io, 0xfd, UInt32(12))
    foreach(b -> write(io, b), c.val)
    n + 16
end
wwrite(io::IO, s::v128_load) = wwrite(io, 0xfd, UInt32(0), s.memarg)
wwrite(io::IO, s::v128_store) = wwrite(io, 0xfd, UInt32(11), s.memarg)

function wwrite(io::IO, cmp::v128cmp)
    tag = UInt32(
        0x23 +
        Int(cmp.lane) * 10 +
        Int(cmp.cmp)
    )
    wwrite(io, 0xfd, tag)
end
function wwrite(io::IO, splt::v128splat)
    tag = UInt32(
        0x0f +
        UInt32(splt.lane)
    )
    wwrite(io, 0xfd, tag)
end
function wwrite(io::IO, d::v128bin)
    @assert d.lane ∈ (Lanes.f32, Lanes.f64) "unimplemented $d"
    @assert d.op == MathOperators.div
    wwrite(io, 0xfd, UInt32(231 + 12 * (d.lane == Lanes.f64)))
end
wwrite(io::IO, bm::v128bitmask) = wwrite(io, 0xfd, UInt32(100 + 32 * Int(bm.lane)))

wwrite(io::IO, c::call) = wwrite(io, 0x10, c.func - one(c.func))

wwrite(io::IO, memarg::MemArg) = wwrite(io, memarg.align, memarg.offset)

wwrite(io::IO, rn::ref_null) = wwrite(io, 0xd0) + write_ref_type(io, NoneRef(true))

wwrite(io::IO, sn::struct_new) = wwrite(io, 0xfb, UInt32(0), sn.typeidx - one(Index))
wwrite(io::IO, sn::struct_get) = wwrite(io, 0xfb, UInt32(2), sn.typeidx - one(Index), sn.fieldidx - one(Index))
wwrite(io::IO, sn::struct_set) = wwrite(io, 0xfb, UInt32(5), sn.typeidx - one(Index), sn.fieldidx - one(Index))
wwrite(io::IO, an::array_new) = wwrite(io, 0xfb, UInt32(6), an.typeidx - one(Index))
wwrite(io::IO, an::array_get) = wwrite(io, 0xfb, UInt32(11), an.typeidx - one(Index))
wwrite(io::IO, an::array_set) = wwrite(io, 0xfb, UInt32(14), an.typeidx - one(Index))
wwrite(io::IO, ::array_len) = wwrite(io, 0xfb, UInt32(15))
wwrite(io::IO, rn::ref_test) = wwrite(io, 0xfb, UInt32(20)) + write_ref_type(io, rn.ref)
wwrite(io::IO, rn::ref_cast) = wwrite(io, 0xfb, UInt32(22)) + write_ref_type(io, rn.ref)
wwrite(io::IO, sc::string_const) = wwrite(io, 0xfb, UInt32(0x82), sc.stringidx - one(Index))

function write_block_type(io::IO, fntype::FuncType)
    fntype == voidtype && return write(io, 0x40)
    length(fntype.results) == 1 && return wwrite(io, only(fntype.results))
    fntypes = get(io, :fntypes, nothing)
    wwrite(io, findfirst(==(fntype), fntypes) - 1)
end

function wwrite(io::IO, block::Union{Loop,Block})
    n = write(io, block isa Loop ? 0x03 : 0x02)
    n += write_block_type(io, block.fntype)
    n += wwrite(io, block.inst)
    n += write(io, 0x0B)
end
function wwrite(io::IO, if_::If)
    n = write(io, 0x04)
    n += write_block_type(io, if_.fntype)
    n += wwrite(io, if_.trueinst)
    if !isempty(if_.falseinst)
        n += write(io, 0x05)
        n += wwrite(io, if_.falseinst)
    end
    n += write(io, 0x0B)
end
function wwrite(io::IO, try_::Try)
    n = write(io, 0x06)
    n += write_block_type(io, try_.fntype)
    n += wwrite(io, try_.inst)
    for c in try_.catches
        if isnothing(c.tag)
            n += wwrite(io, 0x19)
        else
            n += wwrite(io, 0x07, UInt32(c.tag - 1))
        end
        n += wwrite(io, c.inst)
    end
    n += wwrite(io, 0x0b)
    n
end
wwrite(io::IO, td::TryDelegate) = wwrite(io, 0x06, td.inst, 0x18, UInt32(td.label))

function wwrite(io::IO, tt::TryTable)
    n_handlers = length(tt.handlers) + (tt.catch_all !== nothing) + (tt.catch_all_ref !== nothing)
    n = wwrite(io, 0x1f, tt.fntype, UInt32(n_handlers))
    for h in tt.handlers
        n += wwrite(io, h.ref ? 0x01 : 0x00, h.tag - one(UInt32), h.label - one(UInt32))
    end
    if tt.catch_all !== nothing
        n += wwrite(io, 0x02, tt.catch_all - one(UInt32))
    end
    if tt.catch_all_ref !== nothing
        n += wwrite(io, 0x03, tt.catch_all_ref - one(UInt32))
    end
    n += wwrite(io, tt.inst, 0x0b)
    n
end

wwrite(io::IO, t::throw_) = wwrite(io, 0x08, t.tag - one(UInt32))
wwrite(io::IO, ::rethrow_) = wwrite(io, 0x09, UInt32(4))
wwrite(io::IO, t::throw_ref) = wwrite(io, 0x0a, t.tag - one(UInt32))
wwrite(io::IO, b::br) = wwrite(io, 0x0c, b.label)
wwrite(io::IO, b::br_if) = wwrite(io, 0x0d, b.label)
wwrite(io::IO, b::br_table) = wwrite(io, 0x0e, b.labels, b.default)

function wwrite(io::IO, expr::Vector{Inst})
    n = 0
    for inst in expr
        n += wwrite(io, inst)
    end
    n
end

wwrite(io::IO, s::String) = wwrite(io, Vector{UInt8}(s))
function wwrite(io::IO, a::Vector)
    n = wwrite(io, UInt32(length(a)))
    for el in a
        n += wwrite(io, el)
    end
    n
end

wwrite(io::IO, type::RecursiveZone) = wwrite(io, 0x4e, type.structs)
function wwrite(io::IO, type::StructType)
    n = 0
    if isnothing(type.subidx)
        n += wwrite(io, 0x50, UInt32(0))
    else
        n += wwrite(io, 0x50, UInt32(1), UInt32(type.subidx - 1))
    end
    n += wwrite(io, 0x5f, type.fields)
end

function wwrite(io::IO, sf::StructField)
    wwrite(io, sf.type, sf.mut ? 0x01 : 0x00)
end

wwrite(io::IO, at::ArrayType) = wwrite(io, 0x5e, StructField(at.content, nothing, at.mut))

function wwrite(io::IO, fntype::FuncType)
    n = write(io, 0x60)
    n += wwrite(io, fntype.params)
    n += wwrite(io, fntype.results)
    n
end

function wwrite(io::IO, data::Data)
    n = 0
    if data.mode isa DataModeActive
        mode = data.mode.memory == 0 ? UInt32(0) : UInt32(2)
        n += wwrite(io, mode)
        mode == 2 && (n += wwrite(io, UInt32(data.mode.memory)))
        n += wwrite(io, data.mode.offset, 0x0b)
    else
        @assert data.mode isa DataModePassive
        n += wwrite(io, UInt32(1))
    end
    n += wwrite(io, data.init)
    n
end

function wwrite(io::IO, glob::Global)
    n = wwrite(io, glob.type.type, glob.type.mut ? 0x01 : 0x00)
    n += wwrite(io, glob.init)
    n += wwrite(io, 0x0B)
    n
end

wwrite(path::String, wmod::Module) = open(io -> wwrite(io, wmod), path, "w")
function wwrite(io::IO, wmod::Module; names=true, producers=true)
    n = write(io, MAGIC)
    n += write(io, WASM_VERSION)

    # 1. Type Section
    @debug "Type section" pos = position(io)
    fntypes = copy(wmod.types)
    # fntypes = union!(fntypes, unique(map(f -> f.fntype, wmod.funcs)))
    for func in wmod.funcs
        !(func.fntype in fntypes) && push!(fntypes, func.fntype)
        foreach(func) do inst
            if inst isa ContainerInst && !(inst.fntype in fntypes)
                push!(fntypes, inst.fntype)
            end
        end
    end
    for imp in wmod.imports
        (imp isa FuncImport || imp isa TagImport) || continue
        if isnothing(findfirst(==(imp.fntype), fntypes))
            push!(fntypes, imp.fntype)
        end
    end
    # TODO: global code, data code
    sio = IOBuffer()
    wwrite(sio, fntypes)
    buf = take!(sio)
    n += wwrite(io, 0x01, buf)

    # To help resolve indices
    fntypes = flatten_types(fntypes)

    # 2. Import Section
    @debug "Import section" pos = position(io)
    if !isempty(wmod.imports)
        sio = IOBuffer()
        wwrite(sio, UInt32(length(wmod.imports)))
        for imp in wmod.imports
            wwrite(sio, imp.module_name, imp.name)
            if imp isa FuncImport
                wwrite(sio, 0x00, UInt32(findfirst(==(imp.fntype), fntypes)) - one(UInt32))
            elseif imp isa MemImport
                wwrite(sio, 0x02)
                memty = imp.mem.type
                if memty.max == typemax(memty.max)
                    wwrite(sio, 0x00)
                else
                    wwrite(sio, 0x01)
                end
                wwrite(sio, memty.min)
                memty.max != typemax(memty.max) && wwrite(sio, memty.max)
            elseif imp isa GlobalImport
                wwrite(sio, 0x03, imp.type.type, imp.type.mut ? 0x01 : 0x00)
            elseif imp isa TagImport
                wwrite(sio, 0x04, 0x00, UInt32(findfirst(==(imp.fntype), fntypes)) - one(UInt32))
            else
                error("cannot write import $(imp)")
            end
        end
        buf = take!(sio)
        n += wwrite(io, 0x02, buf)
    end

    # 3. Func Section
    @debug "Func section" pos = position(io)
    fntype_indices = map(f -> UInt32(findfirst(==(f.fntype), fntypes)) - one(UInt32), wmod.funcs)
    sio = IOBuffer()
    wwrite(sio, fntype_indices)
    buf = take!(sio)
    n += wwrite(io, 0x03, buf)

    # 5. Memory Section
    @debug "Memory section" pos = position(io)
    if !isempty(wmod.mems)
        sio = IOBuffer()
        wwrite(sio, UInt32(length(wmod.mems)))
        for mem in wmod.mems
            if mem.type.max == typemax(mem.type.max)
                wwrite(sio, 0x00, mem.type.min)
            else
                wwrite(sio, 0x01, mem.type.min, mem.type.max)
            end
        end
        buf = take!(sio)
        n += wwrite(io, 0x05, buf)
    end

    # 6. Global Section
    if !isempty(wmod.globals)
        @debug "Global section" pos = UInt32(position(io))
        sio = IOBuffer()
        wwrite(sio, wmod.globals)
        buf = take!(sio)
        n += wwrite(io, 0x06, buf)
    end

    # 14. String section
    if !isempty(wmod.strings)
        sio = IOBuffer()
        wwrite(sio, 0x00)
        LEB128.encode(sio, UInt32(length(wmod.strings)))
        for s in wmod.strings
            wwrite(sio, UInt32(sizeof(s)))
            write(sio, Vector{UInt8}(s))
        end
        buf = take!(sio)
        n += wwrite(io, 0x0e, buf)
    end

    # 7. Export Section
    if !isempty(wmod.exports)
        sio = IOBuffer()
        wwrite(sio, UInt32(length(wmod.exports)))
        for exp in wmod.exports
            if exp isa FuncExport
                wwrite(sio, exp.name, 0x00, exp.func - one(exp.func))
            elseif exp isa MemExport
                wwrite(sio, exp.name, 0x02, exp.mem - one(exp.mem))
            elseif exp isa GlobalExport
                wwrite(sio, exp.name, 0x03, exp.globalidx - one(exp.globalidx))
            elseif exp isa TagExport
                wwrite(sio, exp.name, 0x04, exp.tagidx - one(exp.tagidx))
            else
                error("unsupported export $exp")
            end
        end
        buf = take!(sio)
        n += wwrite(io, 0x07, buf)
    end

    # 8. Start Section
    if !isnothing(wmod.start)
        sio = IOBuffer()
        wwrite(sio, wmod.start - one(wmod.start))
        buf = take!(sio)
        n += wwrite(io, 0x08, buf)
    end

    # 12. Data Count Section
    if !isempty(wmod.datas)
        sio = IOBuffer()
        wwrite(sio, UInt32(length(wmod.datas)))
        buf = take!(sio)
        n += wwrite(io, 0x0C, buf)
    end

    # 10. Code Section
    sio = IOBuffer()
    wwrite(sio, UInt32(length(wmod.funcs)))
    for (; locals, fntype, inst) in wmod.funcs
        cio = IOBuffer() # code buffer
        nparams = length(fntype.params)

        n_local_groups = 0
        i = nparams + 1
        while i <= lastindex(locals)
            n = 0
            n_local_groups += 1
            while i + n <= lastindex(locals) && locals[i] == locals[i+n]
                n += 1
            end
            i += n
        end
        wwrite(cio, UInt32(n_local_groups))
        i = nparams + 1
        while i <= lastindex(locals)
            n = 1
            loc = locals[i]
            while i + n <= lastindex(locals) && loc == locals[i+n]
                n += 1
            end
            wwrite(cio, UInt32(n))
            wwrite(cio, loc)
            i += n
        end
        wwrite(IOContext(cio, :fntypes => fntypes), inst)
        wwrite(cio, 0x0B)
        buf = take!(cio)
        wwrite(sio, buf)
    end
    buf = take!(sio)
    n += write(io, 0x0A)
    n += wwrite(io, buf)

    # 11. Data Section
    if !isempty(wmod.datas)
        sio = IOBuffer()
        wwrite(sio, wmod.datas)
        buf = take!(sio)
        n += wwrite(io, 0x0B, buf)
    end

    # 0. Name Section
    if names
        sio = IOBuffer()
        wwrite(sio, "name")

        # 0.0 Module Name
        # ...
        # 0.1 Functions Name
        ssio = IOBuffer()
        named_functions = filter(((_, f),) -> !isnothing(f.name), collect(enumerate(wmod.funcs)))
        n_imported = count(imp -> imp isa FuncImport, wmod.imports)
        wwrite(ssio, UInt32(length(named_functions)))
        for (i, f) in named_functions
            wwrite(ssio, Index(n_imported + i - 1), UInt32(sizeof(f.name)))
            write(ssio, f.name)
        end
        buf = take!(ssio)
        wwrite(sio, 0x01, buf)
        # 0.2 Locals Name
        # ...
        # 0.4 Type names
        ssio = IOBuffer()
        named_types = filter(((_,ty),) -> ty isa StructType && !isnothing(ty.name), collect(enumerate(fntypes)))
        wwrite(ssio, UInt32(length(named_types)))
        for (i, ty) in named_types
            wwrite(ssio, Index(i - 1), UInt32(sizeof(ty.name)))
            write(ssio, ty.name)
        end
        buf = take!(ssio)
        wwrite(sio, 0x04, buf)

        buf = take!(sio)
        n += wwrite(io, 0x00, buf)
    end

    # 0. Producers Section
    if producers
        sio = IOBuffer()
        wwrite(sio, "producers")

        language_name = "julia"
        language_version = string(VERSION)
        tool_name = string(nameof(@__MODULE__)) * ".jl"
        tool_version = "v0.1.0"

        wwrite(sio, UInt32(2))
        wwrite(sio, "language", UInt32(1), language_name, language_version)
        wwrite(sio, "processed-by", UInt32(1), tool_name, tool_version)

        buf = take!(sio)
        n += wwrite(io, 0x00, UInt32(length(buf)))
        n += write(io, buf)
    end
end

function wasm(wmod; names=true, producers=true)
    io = IOBuffer()
    wwrite(io, wmod; names, producers)
    take!(io)
end
