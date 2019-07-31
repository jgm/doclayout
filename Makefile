all:
	stack test

bench:
	stack bench

clean:
	stack clean

.PHONY: all clean bench
