all: dist/main.js

serve:
	python3 -m http.server --bind 127.0.0.1 34348

dist/main.js: src/Main.elm
	elm make "$<" --output="$@"
