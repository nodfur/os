default: clean wisp

%.wasm: %.c
	emcc $< -o $@

wisp: wisp.c
	$(CC) -g wisp.c -o wisp

clean:; rm -f wisp wisp.wasm

run: wisp
	./wisp

run-wasm: wisp.wasm
	wasmtime $<
