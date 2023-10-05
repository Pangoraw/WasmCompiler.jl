using Binaryen_jll: wasmopt

"""
    optimize(::WModule, level=1)::WModule

Runs the module through binaryen's optimizer.
"""
function optimize(wmod; level=1, debug=false)
    args = String["--enable-gc", "--enable-reference-types",
                  "--enable-strings", "--enable-exception-handling",
                  "--enable-simd", "--enable-bulk-memory"]
    debug && push!(args, "--debug")
    io_in = IOBuffer()
    wwrite(io_in, wmod)
    seek(io_in, 0)
    io_out = IOBuffer()
    run(pipeline(
        io_in,
        `$(wasmopt()) -g $args -O$level - --output="-"`,
        io_out,
    ))
    seek(io_out, 0)
    wread(io_out)
end
