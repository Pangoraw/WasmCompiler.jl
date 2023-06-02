BINARYEN_FEATURES=--enable-gc  
WASM_AS=wasm-as $(BINARYEN_FEATURES) --enable-reference-types
WAT_DESUGAR=wat-desugar $(BINARYEN_FEATURES)

bootstrap.wasm: bootstrap.wat
	$(WAT_DESUGAR) -f -o bootstrap.sexpr.wast $<
	$(WASM_AS) -g -v web -o $@ bootstrap.sexpr.wast

clean:
	rm bootstrap.wasm