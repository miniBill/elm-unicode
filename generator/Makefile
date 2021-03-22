all: ../src/Unicode.elm ../docs.json

dist/UnicodeData.txt:
	mkdir -p $$(dirname $@)
	curl 'https://www.unicode.org/Public/UCD/latest/ucd/UnicodeData.txt' > $@

dist/Main.elm.js: src/Main.elm src/Unicode.elm
	mkdir -p $$(dirname $@)
	elm make $< --optimize --output $@

dist/index.js: src/index.js
	mkdir -p $$(dirname $@)
	cp $^ $@

../src/Unicode.elm: dist/index.js dist/Main.elm.js dist/UnicodeData.txt src/Unicode.elm
	node $< < dist/UnicodeData.txt > $@
	tail -n+2 src/Unicode.elm >> $@
	elm-format --yes $@

../docs.json: ../src/Unicode.elm
	(cd ..; elm make --docs=$@ src/Unicode.elm)

.PHONY: clean
clean:
	rm -rf dist elm-stuff ../elm-stuff
