const reverse_opcodes = Dict{UInt8,Any}(v => k for (k, v) in opcodes)

function flatten_types(types)
    Iterators.flatten(map(t -> t isa RecursiveZone ? t.structs : (t,), types)) |> collect
end

function resolve_type(types, idx)
    all_types = flatten_types(types)
    all_types[idx]
end

function set_typename!(types, idx, name)
    s = 0

    while s < idx 
        s += 1
        ty = types[s]
        if ty isa RecursiveZone && s + length(ty.structs) >= idx
            return set_typename!(ty.structs, idx - s, name)
        elseif ty isa RecursiveZone
            idx -= length(ty.structs)
        end
    end

    ty = types[s]
    if ty isa StructType
        types[s] = StructType(name, ty.subidx, ty.fields)
    end

    return
end

function wread(io::IO, ::Type{ValType})
    x = peek(io)
    if x == 0x7f
        read(io, UInt8)
        return i32
    elseif x == 0x7e
        read(io, UInt8)
        return i64
    elseif x == 0x7d
        read(io, UInt8)
        return f32
    elseif x == 0x7c
        read(io, UInt8)
        return f64
    elseif x == 0x7b
        read(io, UInt8)
        return v128
    elseif x == 0x73
        read(io, UInt8)
        return NoFuncRef(false)
    elseif x == 0x72
        read(io, UInt8)
        return NoExternRef(false)
    elseif x == 0x71
        read(io, UInt8)
        return NoneRef(false)
    elseif x == 0x70
        read(io, UInt8)
        return FuncRef(false)
    elseif x == 0x6f
        read(io, UInt8)
        return ExternRef(false)
    elseif x == 0x6e
        read(io, UInt8)
        return AnyRef(false)
    elseif x == 0x6d
        read(io, UInt8)
        return EqRef(false)
    elseif x == 0x6c
        read(io, UInt8)
        return I31Ref(false)
    elseif x == 0x6b
        read(io, UInt8)
        return StructRef(false, 1)
    elseif x == 0x6a
        read(io, UInt8)
        return ArrayRef(false, 1)
    elseif x == 0x67
        read(io, UInt8)
        return StringRef(false)
    elseif x == 0x63
        read(io, UInt8)
        t = wread(io, ValType)
        return t isa StructRef ?
                StructRef(true, t.typeidx) :
                typeof(t)(true)
    elseif x == 0x64
        read(io, UInt8)
        return wread(io, ValType)
    else
        s = LEB128.decode(io, Int32)
        return StructRef(false, s+1)
    end
end

function wread(io::IO, ::Type{StructField})
    StructField(
        wread(io, ValType),
        nothing,
        LEB128.decode(io, UInt32) != 0x00,
    )
end

function wread(io::IO, ::Type{StructType})
    code = LEB128.decode(io, Int64)
    fields = wread(io, Vector{StructField})
    subidx = if code == -Int8(0x21)
        nothing
    elseif code == -Int8(0x30)
        @assert false "invalid sub"
    end
    StructType(nothing, subidx, fields)
end

function wread(io::IO, ::Type{FuncType})
    tag = read(io, UInt8)
    @assert tag == 0x60 "invalid tag byte $tag"
    params = wread(io, Vector{ValType})
    results = wread(io, Vector{ValType})
    FuncType(params, results)
end

function wread(io::IO, ::Type{T}) where {T <: Vector}
    out = T()
    len = LEB128.decode(io, UInt32)
    for _ in 1:len
        push!(out, wread(io, eltype(out)))
    end
    out
end

function read_block_type(io::IO)
    next = peek(io)
    if next == 0x40
        read(io, UInt8)
        return FuncType([], [])
    elseif next in 0x7b:0x7f
        return FuncType([], [wread(io, ValType)])
    else
        s = LEB128.decode(io, Int32)
        error("unsupported block type $(next) $s")
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
    elseif tag == 0x06
        fntype = read_block_type(io)
        inst = Inst[]

        lookahead = peek(io)
        inst = Inst[]
        while lookahead != 0x19 && lookahead != 0x18 && lookahead != 0x07
            push!(inst, read_inst(io))
            lookahead = peek(io)
        end

        if lookahead == 0x18
            read(io, UInt8)
            return TryDelegate(inst, LEB128.decode(io, UInt32))
        end

        cblocks = CatchBlock[]
        lookahead = read(io, UInt8)
        while lookahead == 0x07
            lookahead = peek(io)
            tagidx = LEB128.decode(io, UInt32)
            cinst = Inst[]
            while lookahead != 0x19 && lookahead != 0x07 && lookahead != 0x0b
                push!(inst, read_inst(io))
                lookahead = peek(io)
            end
            lookahead = read(io,UInt8)
            push!(cblocks, CatchBlock(tagidx, cinst))
        end

        if lookahead == 0x19
            call_inst = Inst[]
            read_inst_list!(io, call_inst)
            push!(cblocks, CatchBlock(nothing, call_inst))
        else
            @assert read(io, UInt8) == 0x0b
        end

        return Try(fntype, inst, cblocks)
    elseif tag == 0x08
        return throw_(one(Index) + LEB128.decode(io, UInt32))
    elseif tag == 0x09
        return rethrow_(one(Index) + LEB128.decode(io, UInt32))
    elseif tag == 0x0a
        return throw_ref(one(Index) + LEB128.decode(io, UInt32))
    elseif tag == 0x0C
        return br(LEB128.decode(io, UInt32))
    elseif tag == 0x0d
        return br_if(LEB128.decode(io, UInt32))
    elseif tag == 0x10
        return call(one(Index) + LEB128.decode(io, UInt32))
    elseif tag == 0x1f
        fntype = read_block_type(io)

        n_handlers = LEB128.decode(io, UInt32)
        handlers = Vector{TryTableHandler}()
        catch_all = nothing
        catch_all_ref = nothing
        for _ in 1:n_handlers
            tag = read(io, UInt8)
            if tag == 0x00 || tag == 0x01
                tag, label = LEB128.decode(io, UInt32), LEB128.decode(io, UInt32)
                push!(handlers, TryTableHandler(tag + one(UInt32), label + one(UInt32), tag == 0x01))
            elseif tag == 0x02
                @assert catch_all === nothing "multiple catch_all blocks"
                catch_all = LEB128.decode(io, UInt32) + one(UInt32)
            elseif tag == 0x03
                @assert catch_all_ref === nothing "multiple catch_all_ref blocks"
                catch_all_ref = LEB128.decode(io, UInt32) + one(UInt32)
            else
                tag = "0x" * string(tag; base=16)
                throw("unknown try_table handler $tag")
            end
        end
        inst = Vector{Inst}()
        read_inst_list!(io, inst)
        @assert n_handlers == length(handlers) + (catch_all !== nothing) + (catch_all_ref !== nothing)
        return TryTable(fntype, inst, handlers, catch_all, catch_all_ref)
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
    elseif tag in 0x28:0x39
        inst = [i32_load, i64_load, f32_load, f64_load,
                i32_load8_s, i32_load8_u, i32_load16_s, i32_load16_u,
                i64_load8_s, i64_load8_u, i64_load16_s, i64_load16_u,
                i64_load32_s, i64_load32_u,
                i32_store, i64_store, f32_store, f64_store][tag - 0x28 + 1]
        return inst(MemArg(LEB128.decode(io, UInt32), LEB128.decode(io, UInt32)))
    elseif tag == 0x41
        return i32_const(LEB128.decode(io, Int32))
    elseif tag == 0x42
        return i64_const(LEB128.decode(io, Int64))
    elseif tag == 0x43
        return f32_const(read(io, Float32))
    elseif tag == 0x44
        return f64_const(read(io, Float64))
    elseif tag == 0xd0
        return ref_null(wread(io, ValType))
    elseif tag == 0xd3
        return ref_eq()
    elseif tag == 0xfb # structs/array
        tag = LEB128.decode(io, UInt32)
        if tag == 0
            return struct_new(LEB128.decode(io, UInt32) + one(Index))
        elseif tag == 2
            return struct_get(LEB128.decode(io, UInt32) + one(Index),
                              LEB128.decode(io, UInt32) + one(Index))
        elseif tag == 5
            return struct_set(LEB128.decode(io, UInt32) + one(Index),
                              LEB128.decode(io, UInt32) + one(Index))
        elseif tag == 6
            return array_new(LEB128.decode(io, UInt32) + one(Index))
        elseif tag == 11
            return array_get(LEB128.decode(io, UInt32) + one(Index))
        elseif tag == 14
            return array_set(LEB128.decode(io, UInt32) + one(Index))
        elseif tag == 15
            return array_len()
        elseif tag == 20
            ty = wread(io, ValType)
            return ref_test(ty)
        elseif tag == 22
            ty = wread(io, ValType)
            return ref_cast(ty)
        elseif tag == 0x82
            return string_const(LEB128.decode(io, UInt32) + one(Index))
        else
            error("invalid instruction code table 0xfb $tag")
        end
    elseif tag == 0xfc # table/memory
        tag = LEB128.decode(io, UInt32)
        if tag == 10
            @assert read(io, 2) == UInt8[0x00, 0x00]
            return memory_copy()
        elseif tag == 11
            @assert read(io, UInt8) == 0x00
            return memory_fill()
        elseif tag == 12
            return table_init(LEB128.decode(io, UInt32), LEB128.decode(io, UInt32))
        elseif tag == 13
            return elem_drop(LEB128.decode(io, UInt32))
        elseif tag == 14
            return table_copy(LEB128.decode(io, UInt32), LEB128.decode(io, UInt32))
        elseif tag == 15
            return table_grow(LEB128.decode(io, UInt32))
        elseif tag == 16
            return table_size(LEB128.decode(io, UInt32))
        elseif tag == 17
            return table_fill(LEB128.decode(io, UInt32))
        else
            error("invalid instruction code table 0xfc $tag")
        end
    elseif tag == 0xfd # v128
        tag = LEB128.decode(io, UInt32)
        if tag == 0x00
            return v128_load(MemArg(LEB128.decode(io, UInt32), LEB128.decode(io, UInt32)))
        elseif tag == 0x0b
            return v128_store(MemArg(LEB128.decode(io, UInt32), LEB128.decode(io,UInt32)))
        elseif tag == 0x11
            return i32x4_splat()
        elseif tag == 0x1c
            return i32x4_replace_lane(LEB128.decode(io, UInt32))
        elseif tag in 0x23:0x40
            idx = (tag - 0x23) % 10
            op = idx < 2 ?
                CmpOperators.CmpOperator(idx) :
                CmpOperators.CmpOperator(2 + (idx - 2) ÷ 2)
            signed = CmpOperators.needs_sign(op) && (idx - 2) % 2 == 0
            lane = Lanes.Lane((tag - 0x23) ÷ 10)
            return v128cmp(op, lane, signed)
        elseif tag in 99:32:195
            return v128all_true(Lanes.Lane((tag - 99) ÷ 32))
        elseif tag in 100:32:196
            return v128bitmask(Lanes.Lane((tag - 100) ÷ 32))
        elseif tag in (0xe7,0xf3)
            return v128div(tag == 0xe7 ? Lanes.f32 : Lanes.f64)
        else
            tag = "0x" * string(tag; base=16)
            error("invalid instruction code v128 0xfd $tag")
        end
    else
        tag = "0x" * string(tag; base=16, pad=2)
        offset = "0x" * string(position(io); base=16)
        error("invalid instruction code $tag at $offset")
    end
end

function read_inst_list!(io::IO, inst)
    while peek(io) != 0x0B
        push!(inst, read_inst(io))
    end
    @assert read(io, UInt8) == 0x0B
    inst
end

function read_field_type(io::IO)
    StructField(wread(io, ValType),
                nothing,
                read(io, UInt8) == 0x01)
end

function read_comptype(io::IO)
    tag = peek(io)

    if tag == 0x5e
        read(io, UInt8)
        ft = read_field_type(io)
        return ArrayType(nothing, ft.mut, ft.type)
    end

    if tag == 0x5f
        read(io, UInt8)
        n_fields = LEB128.decode(io, UInt32)
        @debug "struct" n_fields 
        fields = StructField[
            read_field_type(io)
            for _ in 1:n_fields
        ]
        return StructType(nothing, nothing, fields)
    end

    @assert tag == 0x60 "invalid composite type of tag $tag"
    return wread(io, FuncType)
end

function read_type(io::IO)
    tag = peek(io)

    if tag == 0x4e # recursive types
        read(io, UInt8)
        n_subtypes = LEB128.decode(io, UInt32)
        subtypes = GCType[]
        @debug "rec" n_subtypes
        for _ in 1:n_subtypes
            push!(subtypes, read_subtype(io))
        end
        return RecursiveZone(subtypes)
    end

    return read_subtype(io)
end

function read_subtype(io)
    tag = peek(io)

    if tag == 0x50 # sub type
        read(io, UInt8)
        n_types = LEB128.decode(io, UInt32)
        types = UInt32[
            one(UInt32) + LEB128.decode(io, UInt32)
            for _ in 1:n_types
        ]
        @debug "sub" types
        ct = read_comptype(io)
        return StructType(nothing, get(types, 1, nothing), ct.fields)
    end

    if tag == 0x4f # sub final type
        read(io, UInt8)
        throw("unsupported final type")
    end

    return read_comptype(io)
end

wread(path::String) = open(wread, path)
function wread(io::IO)
    wmod = WModule()

    # Preamble
    magic = read(io, length(MAGIC))
    @assert magic  == MAGIC "got wrong MAGIC -> $magic"
    @assert read(io, length(WASM_VERSION)) == WASM_VERSION

    local fntypes = WasmType[]
    data_count = nothing

    while !eof(io)
        sid = read(io, UInt8)
        section_length = LEB128.decode(io, UInt32)
        pos = position(io)

        @debug "reading section" sid section_length

        if sid == 0x00
            # 0. Custom Section
            name_length = LEB128.decode(io, UInt32)
            name = String(read(io, name_length))
            if name == "name"
                @debug "Entering name section"
                subsection_id = read(io, UInt8)
                ss_length = LEB128.decode(io, UInt32)
                ss_pos = position(io)
                if subsection_id == 0x01
                    n_named_functions = LEB128.decode(io, UInt32)
                    n_imports = count(imp -> imp isa FuncImport, wmod.imports)
                    for _ in 1:n_named_functions
                        func_index = LEB128.decode(io, UInt32) + 1
                        name_length = LEB128.decode(io, UInt32)
                        name = String(read(io, name_length))
                        if func_index <= n_imports
                            fi = 1
                            func_index = findfirst(imp -> imp isa FuncImport &&
                                                          (fi+=1)-1 == func_index,
                                                   wmod.imports)
                            fimport = wmod.imports[func_index]
                            wmod.imports[func_index] = FuncImport(
                                fimport.module_name,
                                fimport.name, name, fimport.fntype)
                            continue
                        else
                            func_index -= n_imports
                            (; fntype, locals, inst) = wmod.funcs[func_index]
                            wmod.funcs[func_index] = Func(name, fntype, locals, inst)
                        end
                    end
                    @assert position(io) == ss_pos + ss_length
                elseif subsection_id == 0x04
                    n_named_types = LEB128.decode(io, UInt32)
                    for _ in 1:n_named_types
                        type_idx = LEB128.decode(io, UInt32) + 1
                        name_length = LEB128.decode(io, UInt32)
                        name = String(read(io, name_length))
                        set_typename!(fntypes, type_idx, name)
                    end
                else
                    @debug "skipping name subsection" subsection_id
                    seek(io, ss_pos + ss_length)
                end
            else
                @debug "Skipping custom section" name
            end

            seek(io, pos + section_length)
        elseif sid == 0x01
            # 1. Type Section
            n_types = Int(LEB128.decode(io, UInt32))
            @debug "Type section" n_types
            all_types = WasmType[read_type(io)
                                 for _ in 1:n_types]
            @debug "type section $(length(fntypes)) types" all_types = sum(t -> t isa RecursiveZone ?
                                                                           length(t.structs) : 1, all_types)
            fntypes = wmod.types = all_types
        elseif sid == 0x02
            # 2. Import Section
            n_imports = LEB128.decode(io, UInt32)
            @debug "imports" n_imports pos = UInt(position(io))
            for _ in 1:n_imports
                mod_length = LEB128.decode(io, UInt32)
                mod = String(read(io, mod_length))
                name_length = LEB128.decode(io, UInt32)
                name = String(read(io, name_length))
                tag = read(io, UInt8)
                if tag == 0x00
                    typeidx = LEB128.decode(io, UInt32)
                    fntype = resolve_type(fntypes, typeidx+1)
                    push!(wmod.imports, FuncImport(mod, name, nothing, fntype))
                elseif tag == 0x02
                    lim = read(io, UInt8)
                    @assert lim == 0x00 || lim == 0x01 "invalid memory type"
                    memtype = MemoryType(
                        LEB128.decode(io, UInt32),
                        lim == 0x00 ? typemax(UInt32) : LEB128.decode(io, UInt32)
                    )
                    push!(wmod.imports, MemImport(mod, name, nothing, Mem(memtype)))
                elseif tag == 0x03
                    ty = wread(io, ValType)
                    mut = read(io, UInt8)
                    @assert mut == 0x00 || mut == 0x01 "invalid mut for global"
                    mut = mut == 0x01
                    push!(wmod.imports, GlobalImport(mod, name, nothing,
                                                     GlobalType(mut, ty)))
                elseif tag == 0x04
                    @assert read(io, UInt8) == 0x00 "invalid tag import"
                    tagtype = LEB128.decode(io, UInt32) + one(UInt32)
                    type = resolve_type(fntypes, tagtype)
                    push!(wmod.imports, TagImport(mod, name, nothing, type))
                else
                    error("cannot import with tag $tag")
                end
            end
        elseif sid == 0x03
            # 3. Func Section
            n_funcs = LEB128.decode(io, UInt32)
            for _ in 1:n_funcs
                index = LEB128.decode(io, UInt32) + 1
                fntype = resolve_type(fntypes, index)
                push!(
                    wmod.funcs,
                    Func(
                        nothing, fntype,
                        copy(fntype.params),
                        [],
                    )
                )
            end
        elseif sid == 0x05
            # 5. Memory Section
            n_mems = LEB128.decode(io, UInt32)
            for _ in 1:n_mems
                has_max = read(io, UInt8) == 0x01
                memtype = has_max ?
                    MemoryType(LEB128.decode(io, UInt32), LEB128.decode(io, UInt32)) :
                    MemoryType(LEB128.decode(io, UInt32), typemax(UInt32))
                push!(wmod.mems, Mem(memtype))
            end
        elseif sid == 0x04
            # 4. Table Section
            n_tables = LEB128.decode(io, UInt32) 
            for _ in 1:n_tables
                reftype = read(io, UInt8)
                @assert reftype in (0x70, 0x6f) "invalid reftype $reftype"
                tt = TableType(
                    LEB128.decode(io, UInt32),
                    LEB128.decode(io, UInt32),
                    reftype == 0x70 ? FuncRef() : ExternRef()
                )
                push!(wmod.tables, Table(tt))
            end
        elseif sid == 0x06
            # 6. Global Section
            n_globals = LEB128.decode(io, UInt32)
            @debug "globals" n_globals
            for _ in 1:n_globals
                valtype = wread(io, ValType)
                mut = read(io, UInt8)
                @assert mut == 0x00 || mut == 0x01
                mut = mut == 0x01
                gt = GlobalType(mut, valtype)
                init = Inst[]
                read_inst_list!(io, init)
                g = Global(nothing, gt, init)
                push!(wmod.globals, g)
            end
        elseif sid == 0x07
            # 7. Export Section
            n_exports = LEB128.decode(io, UInt32)
            @debug "export section" n_exports
            for _ in 1:n_exports
                name_length = LEB128.decode(io, UInt32)
                name = String(read(io, name_length))
                tag = read(io, UInt8)
                index = one(Index) + LEB128.decode(io, UInt32)
                if tag == 0x00
                    push!(wmod.exports, FuncExport(name, index))
                elseif tag == 0x02
                    push!(wmod.exports, MemExport(name, index))
                elseif tag == 0x03
                    push!(wmod.exports, GlobalExport(name, index))
                elseif tag == 0x04
                    push!(wmod.exports, TagExport(name, index))
                else
                    error("unsupported export tag $tag for export named \"$name\"")
                end
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
        elseif sid == 0x0B
            # 11. Data Section
            n_data = LEB128.decode(io, UInt32)
            @assert isnothing(data_count) || data_count == n_data "malformed module (expected $data_count datas got $n_data instead)" 
            for _ in 1:n_data
                mode = LEB128.decode(io, UInt32)
                @assert mode in 0:2 "invalid data mode $mode"
                offset = Inst[]
                mem = 0
                mode == 2 && (mem = LEB128.decode(io, UInt32))
                if mode == 0 || mode == 2
                    read_inst_list!(io, offset) 
                end
                mode = mode == 1 ?
                  DataModePassive() :
                  DataModeActive(mem, offset)
                n_bytes = LEB128.decode(io, UInt32)
                bytes = read(io, n_bytes)
                push!(wmod.datas, Data(bytes, mode))
            end
        elseif sid == 0x0C
            # 12. Data Count section
            @assert isnothing(data_count) "multiple data count sections"
            data_count = LEB128.decode(io, UInt32)
        elseif sid == 0x0d
            # 13. Tag declarations section
            n_tags = LEB128.decode(io, UInt32)
            for _ in 1:n_tags
                attribute = read(io, UInt8)
                index = LEB128.decode(io, UInt32) + 1
                fntype = resolve_type(fntypes, index)
                push!(wmod.tags, fntype)
            end
        elseif sid == 0x0e
            # 14. String refs section
            @assert read(io, UInt8) == 0x00
            n_strings = LEB128.decode(io, UInt32)
            @debug "String section" n_strings
            for _ in 1:n_strings
                n_bytes = LEB128.decode(io, UInt32)
                bytes = read(io, n_bytes)
                s = String(bytes)
                push!(wmod.strings, s)
            end
        else
            throw("cannot read section $sid")
        end

        @assert position(io) == pos + section_length "failed to read section $sid (diff $(position(io) - (pos + section_length)))"
    end

    wmod
end
