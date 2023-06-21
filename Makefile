setup:
	stack install random ormolu

build:
	touch ranking.txt
	mkdir -p bin
	stack ghc -- typingGame.hs -o bin/typingGame

run: build
	./bin/typingGame rfc2914.txt
