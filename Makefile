all: src/Unicode.elm docs.json

dist/UnicodeData.txt:
	mkdir -p $(dir $@)
	curl 'https://www.unicode.org/Public/UCD/latest/ucd/UnicodeData.txt' > $@

codegen/Gen/Basics.elm: codegen/elm.codegen.json dist/yarn-run
	yarn elm-codegen install

codegen/Categories.elm: codegen/GenerateCategories.elm codegen/Gen/Basics.elm dist/UnicodeData.txt dist/yarn-run
	yarn elm-codegen run $< --output $(dir $@)
	elm-format --yes $@

src/Unicode.elm: codegen/GenerateUnicode.elm codegen/Gen/Basics.elm dist/UnicodeData.txt codegen/Categories.elm dist/yarn-run
	yarn elm-codegen run $< --flags-from dist/UnicodeData.txt --output $(dir $@)
	elm-format --yes $@

dist/yarn-run:
	mkdir -p $(dir $@)
	yarn install && touch $@

docs.json: src/Unicode.elm
	elm make --docs=$@

.PHONY: clean
clean:
	rm -rf dist elm-stuff codegen/elm-stuff node_modules
