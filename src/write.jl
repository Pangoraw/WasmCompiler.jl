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
end

const MAGIC = UInt8[0x00, 0x61, 0x73, 0x6D]
const VERSION = UInt8[0x01, 0x00, 0x00, 0x00]

wwrite(io::IO, args...) = sum(arg -> wwrite(io, arg), args)
wwrite(io::IO, x::Integer) = LEB128.encode(io, x)
wwrite(io::IO, ::WasmInt32) = write(io, 0x7F)
wwrite(io::IO, ::WasmInt64) = write(io, 0x7E)
wwrite(io::IO, ::WasmFloat32) = write(io, 0x7D)
wwrite(io::IO, ::WasmFloat64) = write(io, 0x7C)

const opcodes = Dict{Any,UInt8}(
    return_     => 0x0F,
    unreachable => 0x00,
    drop        => 0x1A,
    select      => 0x1B,
    i32_eqz     => 0x45,
    i32_eq      => 0x46,
    i32_ne      => 0x47,
    i32_lt_s    => 0x48,
    i32_lt_u    => 0x49,
    i32_gt_s    => 0x4a,
    i32_gt_u    => 0x4b,
    i32_le_s    => 0x4c,
    i32_le_u    => 0x4D,
    i32_ge_s    => 0x4e,
    i32_ge_u    => 0x4f,
    i64_eqz     => 0x50,
    i64_eq      => 0x51,
    i64_ne      => 0x52,
    i64_lt_s    => 0x53,
    i64_lt_u    => 0x54,
    i64_gt_s    => 0x55,
    i64_gt_u    => 0x56,
    i64_le_s    => 0x57,
    i64_le_u    => 0x58,
    i64_ge_s    => 0x59,
    i64_ge_u    => 0x5a,
    f32_eq      => 0x5B,
    f32_ne      => 0x5C,
    f32_lt      => 0x5D,
    f32_gt      => 0x5E,
    f32_le      => 0x5F,
    f32_ge      => 0x60,
    f64_eq      => 0x61,
    f64_ne      => 0x62,
    f64_lt      => 0x63,
    f64_gt      => 0x64,
    f64_le      => 0x65,
    f64_ge      => 0x66,
    i32_clz     => 0x67,
    i32_ctz     => 0x68,
    i32_popcnt  => 0x69,
    i32_add     => 0x6a,
    i32_sub     => 0x6b,
    i32_mul     => 0x6c,
    i32_div_s   => 0x6d,
    i32_div_u   => 0x6e,
    i32_rem_s   => 0x6f,
    i32_rem_u   => 0x70,
    i32_and     => 0x71,
    i32_or      => 0x72,
    i32_xor     => 0x73,
    i32_shl     => 0x74,
    i32_shr_s   => 0x75,
    i32_shr_u   => 0x76,
    i32_rotl    => 0x77,
    i32_rotr    => 0x78,
    i64_clz     => 0x79,
    i64_ctz     => 0x7a,
    i64_popcnt  => 0x7b,
    i64_add     => 0x7c,
    i64_sub     => 0x7d,
    i64_mul     => 0x7e,
    i64_div_s   => 0x7f,
    i64_div_u   => 0x80,
    i64_rem_s   => 0x81,
    i64_rem_u   => 0x82,
    i64_and     => 0x83,
    i64_or      => 0x84,
    i64_xor     => 0x85,
    i64_shl     => 0x86,
    i64_shr_s   => 0x87,
    i64_shr_u   => 0x88,
    i64_rotl    => 0x89,
    i64_rotr    => 0x8a,
)

wwrite(io::IO, ::T) where {T <: Inst} = haskey(opcodes, T) ? write(io, opcodes[T]) : error("could not emit instruction $T")
wwrite(io::IO, lg::local_get) = wwrite(io, 0x20, lg.n - one(lg.n))
wwrite(io::IO, lg::local_set) = wwrite(io, 0x21, lg.n - one(lg.n))
wwrite(io::IO, lg::local_tee) = wwrite(io, 0x22, lg.n - one(lg.n))
wwrite(io::IO, lg::global_get) = wwrite(io, 0x23, lg.n - one(lg.n))
wwrite(io::IO, lg::global_set) = wwrite(io, 0x24, lg.n - one(lg.n))

wwrite(io::IO, c::i32_const) = wwrite(io, 0x41, c.val)
wwrite(io::IO, c::i64_const) = wwrite(io, 0x42, c.val)
wwrite(io::IO, c::f32_const) = wwrite(io, 0x43, c.val)
wwrite(io::IO, c::f64_const) = wwrite(io, 0x44, c.val)

wwrite(io::IO, c::call) = wwrite(io, 0x10, c.func - one(c.func))
wwrite(io::IO, ::nop) = write(io, 0x01)
function wwrite(io::IO, block::Union{Loop,Block})
    n = write(io, block isa Loop ? 0x03 : 0x02)
    @assert block.fntype == voidtype
    n += write(io, 0x40) # voidtype
    n += wwrite(io, block.inst)
    n += write(io, 0x0B)
end
function wwrite(io::IO, if_::If)
    n = write(io, 0x04)
    @assert if_.fntype == voidtype
    n += write(io, 0x40) # voidtype
    n += wwrite(io, if_.trueinst)
    n += write(io, 0x05)
    n += wwrite(io, if_.falseinst)
    n += write(io, 0x0B)
end

wwrite(io::IO, b::br) = wwrite(io, 0x0C, b.label)

function wwrite(io::IO, expr::Vector{Inst})
    n = 0
    for inst in expr
        n += wwrite(io, inst)
    end
    n
end

function wwrite(io::IO, a::Vector)
    n = wwrite(io, UInt32(length(a)))
    for el in a
        n += wwrite(io, el)
    end
    n
end

function wwrite(io::IO, fntype::FuncType)
    write(io, 0x60)
    wwrite(io, fntype.params)
    wwrite(io, fntype.results)
end

function wwrite(io::IO, wmod::WModule)
    n = write(io, MAGIC)
    n += write(io, VERSION)
 
    # 1. Type Section
    fntypes::Vector{FuncType} = unique(map(f -> f.fntype, wmod.funcs))
    sio = IOBuffer()
    wwrite(sio, fntypes)
    buf = take!(sio)
    n += write(io, 0x01)
    n += wwrite(io, UInt32(length(buf)))
    n += write(io, buf)

    # 3. Func Section
    fntype_indices = map(f -> UInt32(findfirst(==(f.fntype), fntypes)) - one(UInt32), wmod.funcs)
    n += write(io, 0x03)
    sio = IOBuffer()
    wwrite(sio, fntype_indices)
    buf = take!(sio)
    n += wwrite(io, UInt32(length(buf)))
    n += write(io, buf)

    # 7. Export Section
    sio = IOBuffer()
    wwrite(sio, UInt32(length(wmod.exports)))
    for exp in wmod.exports
        if exp isa FuncExport
            wwrite(sio, UInt32(length(exp.name)))
            write(sio, exp.name)
            wwrite(sio, 0x00, exp.func - one(exp.func))
        else
            error("unsupported export $exp")
        end
    end
    buf = take!(sio)
    n += wwrite(io, 0x07, UInt32(length(buf)))
    n += write(io, buf)

    # 8. Start Section
    if !isnothing(wmod.start)
        sio = IOBuffer()
        wwrite(sio, wmod.start - one(wmod.start))
        buf = take!(sio)
        n += wwrite(io, 0x08, UInt32(length(buf)))
        n += write(io, buf)
    end

    # 10. Code Section
    sio = IOBuffer()
    wwrite(sio, UInt32(length(wmod.funcs)))
    for func in wmod.funcs
        cio = IOBuffer() # code buffer
        nparams = length(func.fntype.params)
        wwrite(cio, UInt32(length(func.locals) - nparams))
        for loc in Iterators.drop(func.locals, nparams)
            wwrite(cio, UInt32(1)) # TODO: can optimize by grouping
            wwrite(cio, loc)
        end
        wwrite(cio, func.inst)
        wwrite(cio, 0x0B)
        buf = take!(cio)
        wwrite(sio, UInt32(length(buf)))
        write(sio, buf)
    end
    buf = take!(sio)
    n += write(io, 0x0A)
    n += wwrite(io, UInt32(length(buf)))
    n += write(io, buf)

end
