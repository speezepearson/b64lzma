all: bundle

bundle: dist/b64lzma.js dist/main.js dist/elm.js dist/lzma/lzma-d-min.js dist/lzma/lzma_worker-min.js
	rm dist/index.html
	make dist/index.html
	python3 bundle.py dist/index.html

dist/lzma/%: lib/lzma/%
	mkdir -p dist/lzma
	cp "$<" "$@"

dist/index.html: src/Native/index.html
	mkdir -p dist
	cp "$<" "$@"

dist/main.js: src/Native/main.js
	mkdir -p dist
	cp "$<" "$@"

dist/b64lzma.js: src/Native/b64lzma.js
	mkdir -p dist
	cp "$<" "$@"

dist/elm.js: src/*.elm src/*/*.elm
	elm make src/Main.elm --output="$@"
