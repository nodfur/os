default: clean wisp dist/wisp.js esbuild test

CFLAGS += -g

EMCCFLAGS += -s WASM=1
EMCCFLAGS += -s EXPORTED_RUNTIME_METHODS=ccall,cwrap,FS,ENV,IDBFS -lidbfs.js
EMCCFLAGS += -s MODULARIZE=1 -s 'EXPORT_NAME="loadWisp"'

SOURCES = wisp.c wisp-eval.c wisp-read.c wisp-dump.c

dist/wisp.js: $(SOURCES)
	emcc $(CFLAGS) $^ -o $@ $(EMCCFLAGS)

esbuild:
	./build

wisp: $(SOURCES)
	$(CC) $(CFLAGS) $^ -o wisp

clean:; rm -rf wisp dist/*

test: wisp
	./wisp test
