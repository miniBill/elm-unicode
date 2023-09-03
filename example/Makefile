.PHONY: all

all: dist/elm.min.js.gz dist/elm-old.min.js.gz

dist/%.gz: dist/%
	pigz -9 < $< > $@

dist/%.min.js: dist/%.js dist/yarn-run
	yarn uglifyjs $< --mangle --output $@

dist/%.intermediate.js: dist/%.js dist/yarn-run
	yarn uglifyjs $< --compress 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe' --output $@

dist/elm.js: src/Main.elm ../src/Unicode.elm dist/yarn-run
	yarn elm-optimize-level-2 $< --output $@

dist/elm-old.js: src/OldMain.elm ../src/OldUnicode.elm dist/yarn-run
	yarn elm-optimize-level-2 $< --output $@

dist/yarn-run: package.json yarn.lock
	mkdir -p $(dir $@)
	yarn install && touch $@
