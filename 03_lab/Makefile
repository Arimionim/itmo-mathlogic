.PHONY: all pack run
all:
	ghc Main.hs -O3 -XBangPatterns

alexHappy:
	alex Lexer.x
	happy Parser.y

pack:
	alex Lexer.x
	happy Parser.y
	zip ZIP -r Makefile Main.hs Parser.hs Grammar.hs Lexer.hs Check.hs Mmm.hs

run:
	./Main
