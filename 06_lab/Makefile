.PHONY: all pack run
all:
	alex Lexer.x
	happy Parser.y
	ghc Main.hs -O3 -XBangPatterns

pack:
	zip ZIP -r Makefile Main.hs Solve.hs Lexer.x Parser.y

run:
	./Main
