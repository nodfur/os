default: clean wisp wisp.wasm index.js test

CFLAGS += -g
EMCCFLAGS += -s WASM=1 -s EXPORTED_RUNTIME_METHODS=ccall,cwrap -lidbfs.js

SOURCES = wisp.c wisp-eval.c wisp-read.c wisp-dump.c

index.js: $(SOURCES)
	emcc $(CFLAGS) $^ -o $@ $(EMCCFLAGS)

wisp.wasm: $(SOURCES)
	emcc $(CFLAGS) $^ -o $@

wisp: $(SOURCES)
	$(CC) $(CFLAGS) $^ -o wisp

clean:; rm -f wisp *.wasm *.o index.js

test: wisp
	./wisp test

run-wasm: wisp.wasm
	wasmtime $<

gdb-wasm: wisp.wasm
	gdb --args wasmtime run -g wisp.wasm
