all: src/Unicode.elm docs.json tests/TestData.elm

dist/UnicodeData.txt:
	mkdir -p $(dir $@)
	curl 'https://www.unicode.org/Public/UCD/latest/ucd/UnicodeData.txt' > $@

tests/TestData.elm: dist/UnicodeData.txt Makefile
	echo "module TestData exposing (testData)" > $@
	echo "" >> $@
	echo "testData : List Int" >> $@
	echo "testData =" >> $@
	echo -n "    [ 0x" >> $@
	grep -oE '^[^;]*' < $< | sed '2,$$s/^/    , 0x/' >> $@
	echo "    ]" >> $@
	elm-format --yes $@

codegen/Gen/Basics.elm: codegen/elm.codegen.json dist/yarn-run
	yarn elm-codegen install

codegen/Categories.elm: codegen/GenerateCategories.elm codegen/Gen/Basics.elm codegen/elm.json dist/UnicodeData.txt dist/yarn-run
	yarn elm-codegen run $< --output $(dir $@)
	elm-format --yes $@

src/Unicode.elm: codegen/GenerateUnicode.elm codegen/Gen/Basics.elm dist/UnicodeData.txt codegen/Categories.elm dist/yarn-run
	yarn elm-codegen run $< --flags-from dist/UnicodeData.txt --output $(dir $@) --debug
	elm-format --yes $@

dist/yarn-run: package.json yarn.lock
	mkdir -p $(dir $@)
	yarn install && touch $@

docs.json: src/Unicode.elm
	elm make --docs=$@

.PHONY: clean
clean:
	rm -rf dist elm-stuff codegen/elm-stuff node_modules codegen/Gen codegen/Categories.elm
