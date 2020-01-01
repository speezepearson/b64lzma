all: dist

dist: dist/index.html dist/bundle.js

dist/bundle.js: lib/lzma/lzma-d-min.js lib/lzma/lzma_worker-min.js build/elm.js src/Native/b64lzma.js src/Native/init.js
	mkdir -p dist
	cat $^ > "$@"

dist/index.html: src/Native/index.html
	mkdir -p dist
	cp "$<" "$@"

build/elm.js: src/*.elm src/*/*.elm
	elm make src/Main.elm --output="$@"
