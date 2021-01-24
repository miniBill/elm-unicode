all: src/Internal.elm

dist/UnicodeData.txt:
	mkdir -p $$(dirname $@)
	curl 'https://www.unicode.org/Public/UCD/latest/ucd/UnicodeData.txt' > $@

dist/Main.elm.js: src/Main.elm src/Unicode.elm
	mkdir -p $$(dirname $@)
	#elm-optimize-level-2 $^ --output=$@.fat.js
	#terser $@.fat.js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | terser --mangle --output=$@
	elm make $< --optimize --output $@

dist/index.js: src/index.js
	mkdir -p $$(dirname $@)
	cp $^ $@

src/Internal.elm: dist/index.js dist/Main.elm.js dist/UnicodeData.txt
	node $< < dist/UnicodeData.txt > $@
	elm-format --yes $@

.PHONY: clean
clean:
	rm -r dist
