all: src/Unicode.elm docs.json

dist/UnicodeData.txt:
	mkdir -p $(dir $@)
	curl 'https://www.unicode.org/Public/UCD/latest/ucd/UnicodeData.txt' > $@

codegen/Categories.elm: codegen/GenerateCategories.elm dist/UnicodeData.txt elm-stuff/yarn-run
	yarn elm-codegen run $< --output $(dir $@)
	elm-format --yes $@

src/Unicode.elm: codegen/GenerateUnicode.elm dist/UnicodeData.txt codegen/Categories.elm elm-stuff/yarn-run
	yarn elm-codegen run $< --flags-from dist/UnicodeData.txt --output $(dir $@)
	elm-format --yes $@

elm-stuff/yarn-run:
	yarn install

docs.json: src/Unicode.elm
	elm make --docs=$@

.PHONY: clean
clean:
	rm -rf dist elm-stuff
