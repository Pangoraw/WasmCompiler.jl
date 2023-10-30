using Deno_jll, JSON3, Base64, WasmCompiler

function send_command!(p, cmd)
    write(p, JSON3.write(cmd))
    write(p, '\n')
    JSON3.read(readuntil(p, '\n'))
end
instantiate!(p, code) = @assert send_command!(p, Dict(:header => "instantiate", :code => base64encode(code)))["result"] == "ok"
call(p, f, params...) = send_command!(p, Dict(:header => "call", :params => collect(params), :func => string(f)))["result"]
exit(p) = @assert send_command!(p, Dict(:header => "exit"))["result"] == "exit"

launch() =
    open(`$(deno()) run $(joinpath(@__DIR__, "run.ts"))`;
         write=true, read=true)
