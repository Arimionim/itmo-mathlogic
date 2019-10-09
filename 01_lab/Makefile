.PHONY: all pack run
all:    main

pack:
	alex Lexer.x
	happy Parser.y
	zip ZIP -r Main.hs Parser.hs Grammar.hs Lexer.hs Makefile

run:
	./Main

main:
	ghc Main.hs -O3 -o Main

