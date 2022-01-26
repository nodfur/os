default: clean wisp wisp.wasm test

CFLAGS += -g -O0

wisp.wasm: wisp.c wisp-read.c
	emcc $(CFLAGS) $^ -o $@

wisp: wisp.c wisp-read.c
	$(CC) $(CFLAGS) $^ -o wisp

clean:; rm -f wisp *.wasm *.o

test: wisp
	./wisp test

run-wasm: wisp.wasm
	wasmtime $<

gdb-wasm: wisp.wasm
	gdb --args wasmtime run -g wisp.wasm
