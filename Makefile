#wasm-as --enable-gc --enable-strings --enable-tail-call --enable-reference-types --enable-bulk-memory bootstrap.wat -o bootstrap.wasm

BINARYEN_FEATURES=--enable-gc --enable-strings --enable-exception-handling --enable-gc-nn-locals
WASM_AS=wasm-as $(BINARYEN_FEATURES) --enable-reference-types
WASM_MERGE=wasm-merge $(BINARYEN_FEATURES) --enable-reference-types
WAT_DESUGAR=wat-desugar $(BINARYEN_FEATURES)

bootstrap.wasm: bootstrap.wat
	$(WASM_AS) -g -o $@ $<

runtime.wasm: runtime.wat
	$(WASM_AS) -g -o $@ $<

merge.wasm: bootstrap.wasm runtime.wasm
	$(WASM_MERGE) bootstrap.wasm bootstrap runtime.wasm runtime -o $@

clean:
	rm bootstrap.wasm runtime.wasm merge.wasm
