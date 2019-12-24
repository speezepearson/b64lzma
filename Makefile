all: dist/index.html dist/main.js dist/lzma/lzma-d-min.js dist/lzma/lzma_worker-min.js

serve:
	python3 -m http.server --bind 127.0.0.1 34348

dist/lzma/%: lib/lzma/%
	mkdir -p dist/lzma
	cp "$<" "$@"

dist/index.html: src/index.html
	cp "$<" "$@"

dist/main.js: src/*.elm src/*/*.elm
	elm make src/Main.elm --output="$@"
