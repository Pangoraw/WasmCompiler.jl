const reverse_opcodes = Dict{UInt8,Any}(v => k for (k, v) in opcodes)

function wread(io::IO, ::Type{ValType})
    x = read(io, UInt8)
    if x == 0x7F
        return i32
    elseif x == 0x7E
        return i64
    elseif x == 0x7D
        return f32
    elseif x == 0x7C
        return f64
    else
        error("unknown type byte $x")
    end
end

function wread(io::IO, ::Type{FuncType})
    tag = read(io, UInt8)
    @assert tag == 0x60 "invalid tag byte $tag"
    params = wread(io, Vector{ValType})
    results = wread(io, Vector{ValType})
    FuncType(params, results)
end

function wread(io::IO, T::Type{Vector})
    out = T()
    len = LEB128.decode(io, UInt32)
    for _ in 1:len
        push!(out, wread(io, eltype(out)))
    end
    out
end

function read_block_type(io::IO)
    if peek(io) == 0x40
        read(io, UInt8)
        return FuncType([], [])
    else
        error("unsupported block type")
    end
end

function read_inst(io::IO)
    tag = read(io, UInt8)
    if haskey(reverse_opcodes, tag)
        return reverse_opcodes[tag]()
    elseif tag == 0x02 || tag == 0x03
        T = tag == 0x02 ? Block : Loop
        fntype = read_block_type(io)
        inst = Inst[]
        read_inst_list!(io, inst)
        return T(fntype, inst)
    elseif tag == 0x04
        fntype = read_block_type(io)
        lookahead = peek(io)
        trueinst = Inst[]
        while lookahead != 0x0B && lookahead != 0x05 
            push!(trueinst, read_inst(io))
            lookahead = peek(io)
        end
        falseinst = Inst[]
        last = read(io, UInt8)
        if last == 0x05
            read_inst_list!(io, falseinst)
        else
            @assert last == 0x0B "invalid if inst"
        end
        return If(fntype, trueinst, falseinst)
    elseif tag == 0x0C
        return br(LEB128.decode(io, UInt32))
    elseif tag == 0x10
        return call(one(Index) + LEB128.decode(io, UInt32))
    elseif tag == 0x20
        return local_get(one(Index) + LEB128.decode(io, UInt32))
    elseif tag == 0x21
        return local_set(one(Index) + LEB128.decode(io, UInt32))
    elseif tag == 0x22
        return local_tee(one(Index) + LEB128.decode(io, UInt32))
    elseif tag == 0x23
        return global_get(one(Index) + LEB128.decode(io, UInt32))
    elseif tag == 0x24
        return global_set(one(Index) + LEB128.decode(io, UInt32))
    elseif tag == 0x41
        return i32_const(LEB128.decode(io, Int32))
    elseif tag == 0x42
        return i64_const(LEB128.decode(io, Int64))
    elseif tag == 0x43
        return f32_const(read(io, Float32))
    elseif tag == 0x44
        return f64_const(read(io, float64))
    else
        error("invalid instruction code $tag")
    end
end

function read_inst_list!(io::IO, inst)
    while peek(io) != 0x0B
        push!(inst, read_inst(io))
    end
    @assert read(io, UInt8) == 0x0B
    inst
end

function wread(io::IO)
    wmod = WModule()

    # Preamble
    @assert read(io, length(MAGIC)) == MAGIC
    @assert read(io, length(WASM_VERSION)) == WASM_VERSION

    local fntypes

    while !eof(io)
        sid = read(io, UInt8)
        section_length = LEB128.decode(io, UInt32)
        pos = position(io)

        if sid == 0x00
            # 0. Custom Section
            name_length = LEB128.decode(io, UInt32)
            name = String(read(io, name_length))
            if name == "name"
                subsection_id = read(io, UInt8)
                ss_length = LEB128.decode(io, UInt32)
                ss_pos = position(io)
                if subsection_id == 0x01
                    n_named_functions = LEB128.decode(io, UInt32)
                    n_imports = count(imp -> imp isa FuncImport, wmod.imports)
                    for _ in 1:n_named_functions
                        func_index = LEB128.decode(io, UInt32) - n_imported + 1
                        name_length = LEB128.decode(io, UInt32)
                        name = String(read(io, name_length))
                        (; fntype, locals, inst) = wmod.funcs[func_index]
                        wmod.funcs[func_index] = Func(name, fntype, locals, inst)
                    end
                    @assert position(io) == ss_pos + ss_length
                else
                    @debug "skipping name subsection" subsection_id
                    seek(io, ss_pos + ss_length)
                end
            else
                @debug "Skipping custom section" name
                seek(io, pos + section_length)
            end
        elseif sid == 0x01
            # 1. Type Section
            fntypes = wread(io, Vector{FuncType})
        elseif sid == 0x03
            # 3. Func Section
            n_funcs = LEB128.decode(io, UInt32)
            for _ in 1:n_funcs
                push!(
                    wmod.funcs,
                    Func(
                        nothing, fntypes[LEB128.decode(io, UInt32) + 1],
                        copy(fntypes[LEB128.decode(io, UInt32) + 1].params),
                        [],
                    )
                )
            end
        elseif sid == 0x07
            # 7. Export Section
            n_exports = LEB128.decode(io, UInt32)
            tag = read(io, UInt8)
            if tag == 0x00
                name_length = LEB128.decode(io, UInt32)
                name = String(read(io, name_length))
                index = one(Index) + LEB128.decode(io, UInt32)
                push!(wmod.exports, FuncExport(name, index))
            else
                error("unsupported export tag $tag")
            end
        elseif sid == 0x08
            # 8. Start Section
            wmod.start = one(UInt32) + LEB128.decode(io, UInt32)
        elseif sid == 0x0A
            # 10. Code Section
            n_funcs = LEB128.decode(io, UInt32)
            @assert n_funcs == length(wmod.funcs)
            for i in eachindex(wmod.funcs)
                (; fntype, locals, inst) = wmod.funcs[i]
                n_code_bytes = LEB128.decode(io, UInt32)
                code_pos = position(io)
                n_locals_pairs = LEB128.decode(io, UInt32)
                for _ in 1:n_locals_pairs
                    n_repeat = LEB128.decode(io, UInt32)
                    val = wread(io, ValType)
                    for _ in 1:n_repeat
                        push!(locals, val)
                    end
                end
                read_inst_list!(io, inst)
                @assert position(io) == code_pos + n_code_bytes "malformed code section (function $i)"
                wmod.funcs[i] = Func(nothing, fntype, locals, inst)
            end
        end

        @assert position(io) == pos + section_length
    end

    wmod
end