all:
	stack test

bench:
	stack bench

clean:
	stack clean

prof:
	stack build --profile

repl:
	stack ghci src/Text/DocLayout.hs --ghc-options=-XOverloadedStrings

.PHONY: all clean bench repl prof
