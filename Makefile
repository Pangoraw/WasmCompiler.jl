#wasm-as --enable-gc --enable-strings --enable-tail-call --enable-reference-types --enable-bulk-memory bootstrap.wat -o bootstrap.wasm

BINARYEN_FEATURES=--enable-gc --enable-strings
WASM_AS=wasm-as $(BINARYEN_FEATURES) --enable-reference-types
WAT_DESUGAR=wat-desugar $(BINARYEN_FEATURES)

bootstrap.wasm: bootstrap.wat
	$(WASM_AS) -g -v web -o $@ bootstrap.wat

test.wasm: test.wat
	$(WAT_DESUGAR) -f -o test.sexpr.wast $<
	$(WASM_AS) -g -v web -o $@ test.sexpr.wast

clean:
	rm bootstrap.wasm
