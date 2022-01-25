default: clean run

%.wasm: %.c
	emcc -g $< -o $@

wisp: wisp.c
	$(CC) -g wisp.c -o wisp

clean:; rm -f wisp wisp.wasm

run: wisp
	./wisp

run-wasm: wisp.wasm
	wasmtime $<

gdb-wasm: wisp.wasm
	gdb --args wasmtime run -g wisp.wasm
