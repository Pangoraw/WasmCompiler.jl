#wasm-as --enable-gc --enable-strings --enable-tail-call --enable-reference-types --enable-bulk-memory bootstrap.wast -o bootstrap.wasm

BINARYEN_FEATURES=--enable-gc --enable-strings --enable-exception-handling --enable-reference-types
WASM_AS=wasm-as $(BINARYEN_FEATURES) --enable-reference-types
WASM_MERGE=wasm-merge $(BINARYEN_FEATURES) --enable-reference-types
WAT_DESUGAR=wat-desugar $(BINARYEN_FEATURES)

bootstrap.wasm: bootstrap.wast
	$(WASM_AS) -g -o $@ $<

clean:
	rm bootstrap.wasm
