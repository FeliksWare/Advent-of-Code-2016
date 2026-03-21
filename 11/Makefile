.PHONY: all clean

all: main

main: main.hs
	ghc -prof -fprof-auto -rtsopts -O2 main.hs

clean:
	rm main main.hi main.o
