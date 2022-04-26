all: ../src/Unicode.elm ../docs.json

dist/UnicodeData.txt:
	mkdir -p $$(dirname $@)
	curl 'https://www.unicode.org/Public/UCD/latest/ucd/UnicodeData.txt' > $@

dist/Unicode.elm.js: src/GenerateUnicode.elm src/Categories.elm
	mkdir -p $$(dirname $@)
	elm make $< --optimize --output $@

dist/Categories.elm.js: src/GenerateCategories.elm
	mkdir -p $$(dirname $@)
	elm make $< --optimize --output $@

dist/%.js: src/%.js
	mkdir -p $$(dirname $@)
	cp $^ $@

src/Categories.elm: dist/categories.js dist/Categories.elm.js dist/UnicodeData.txt
	node $< < dist/UnicodeData.txt > $@
	elm-format --yes $@

../src/Unicode.elm: dist/unicode.js dist/Unicode.elm.js dist/UnicodeData.txt
	node $< < dist/UnicodeData.txt > $@
	elm-format --yes $@

../docs.json: ../src/Unicode.elm
	(cd ..; elm make --docs=$@ src/Unicode.elm)

.PHONY: clean
clean:
	rm -rf dist elm-stuff ../elm-stuff
