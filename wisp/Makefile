default: clean wisp wisp.wasm test

CFLAGS += -g

SOURCES = wisp.c wisp-read.c wisp-eval.c

wisp.wasm: $(SOURCES)
	emcc $(CFLAGS) $^ -o $@

wisp: $(SOURCES)
	$(CC) $(CFLAGS) $^ -o wisp

clean:; rm -f wisp *.wasm *.o

test: wisp
	./wisp test

run-wasm: wisp.wasm
	wasmtime $<

gdb-wasm: wisp.wasm
	gdb --args wasmtime run -g wisp.wasm
