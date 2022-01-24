%.wasm: %.c
	emcc $< -o $@

run: wisp.wasm
	wasmtime $<
