default: wisp wasm/wisp.js dist/index.html

CFLAGS += -g

EMCCFLAGS += -s WASM=1
EMCCFLAGS += -s EXPORTED_RUNTIME_METHODS=ccall,cwrap,FS,ENV,IDBFS
EMCCFLAGS += -lidbfs.js
EMCCFLAGS += -s MODULARIZE=1 -s 'EXPORT_NAME="loadWisp"'

SOURCES = wisp.c wisp-eval.c wisp-read.c wisp-dump.c

wasm/wisp.js: $(SOURCES)
	emcc $(CFLAGS) $^ -o $@ $(EMCCFLAGS)

dist/index.html: repl.jsx index.html wasm/wisp.js wasm/wisp.wasm build
	./build

wisp: $(SOURCES)
	$(CC) $(CFLAGS) $^ -o wisp
	./wisp test

clean:; rm -rf wisp dist/* wasm/*

deploy:
	cp dist/* /restless/www/wisp/
