using Binaryen_jll: wasmopt

"""
    optimize(::Module, level=1)::Module

Runs the module through binaryen's optimizer.
"""
function optimize(wmod; level=1, debug=false)
    args = String["--all-features"]
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
