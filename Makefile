build:
	stack ghc -- typingGame.hs -o bin/typingGame

run: build
	./bin/typingGame rfc2914.txt 
