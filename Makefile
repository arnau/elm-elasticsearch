docs:
	cd examples; elm make Parser.elm --output ../docs/parser.html
.PHONY: docs

clean:
	@rm -rf elm-stuff
.PHONY: clean
