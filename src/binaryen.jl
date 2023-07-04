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
