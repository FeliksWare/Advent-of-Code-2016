.PHONY: all clean deps

all: main

main: main.hs
	ghc main.hs

deps:
	cabal install --lib utf8-string bytestring cryptohash-md5 containers --package-env .

clean:
	rm -f main main.hi main.o
	rm -f .ghc.environment.x86_64-linux-9.6.7
