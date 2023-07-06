using Pkg.Artifacts: @artifact_str

const binaryen_bin_dir = let p = artifact"binaryen"
    joinpath(p, first(readdir(p)), "bin")
end

function wasm_as()
    joinpath(binaryen_bin_dir, "wasm-as")
end

function wasm_opt()
    joinpath(binaryen_bin_dir, "wasm-opt")
end

function wasm_merge()
    joinpath(binaryen_bin_dir, "wasm-merge")
end

"""
    optimize(::WModule, level=1)::WModule

Runs the module through binaryen's optimizer.
"""
function optimize(wmod, level=1)
    args = String["--enable-gc", "--enable-reference-types", "--enable-strings", "--enable-exception-handling"]
    io_in = IOBuffer()
    wwrite(io_in, wmod)
    seek(io_in, 0)
    io_out = IOBuffer()
    run(pipeline(
        io_in,
        `$(wasm_opt()) -g $args -O$level - --output="-"`,
        io_out,
    ))
    seek(io_out, 0)
    wread(io_out)
end
