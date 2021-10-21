all: src/Text/unicodeWidth.inc
	stack test

bench:
	stack bench

clean:
	stack clean

prof:
	stack build --profile

repl:
	stack ghci src/Text/DocLayout.hs --ghc-options=-XOverloadedStrings

src/Text/unicodeWidth.inc: EastAsianWidth.txt
	./update.hs > $@

unicodeVersion = latest

EastAsianWidth.txt: .FORCE
	curl https://www.unicode.org/Public/UCD/$(unicodeVersion)/ucd/$@ > $@

.PHONY: all clean bench repl prof .FORCE
