all: src/Categories.elm src/Unicode.elm docs.json

dist/UnicodeData.txt:
	mkdir -p $(dir $@)
	curl 'https://www.unicode.org/Public/UCD/latest/ucd/UnicodeData.txt' > $@

codegen/helpers/Categories.elm: codegen/GenerateCategories.elm dist/UnicodeData.txt
	pnpm codegen run $< --output $(dir $@)
	elm-format --yes $@

src/Unicode.elm: dist/unicode.js dist/Unicode.elm.js dist/UnicodeData.txt
	pnpm codegen run $< --flags-from dist/UnicodeData.txt --output $(dir $@)
	elm-format --yes $@

docs.json: src/Unicode.elm
	elm make --docs=$@ $^

.PHONY: clean
clean:
	rm -rf dist elm-stuff
