all: ../src/Utf8.elm

dist/UnicodeData.txt:
	mkdir -p $$(dirname $@)
	curl 'https://www.unicode.org/Public/UCD/latest/ucd/UnicodeData.txt' > $@

dist/Main.elm.js: src/Main.elm src/Unicode.elm
	mkdir -p $$(dirname $@)
	elm make $< --optimize --output $@

dist/index.js: src/index.js
	mkdir -p $$(dirname $@)
	cp $^ $@

../src/Utf8.elm: dist/index.js dist/Main.elm.js dist/UnicodeData.txt
	node $< < dist/UnicodeData.txt > $@
	elm-format --yes $@

.PHONY: clean
clean:
	rm -r dist
