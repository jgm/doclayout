all: src/Text/unicodeWidth.inc
	cabal test --test-options='--hide-successes --ansi-tricks=false'

bench:
	cabal bench

clean:
	cabal clean

src/Text/unicodeWidth.inc: EastAsianWidth.txt
	./update.hs > $@

unicodeVersion = latest

EastAsianWidth.txt:
	curl https://www.unicode.org/Public/UCD/$(unicodeVersion)/ucd/$@ > $@

.PHONY: all clean bench .FORCE
