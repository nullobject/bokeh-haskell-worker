.PHONY: run build clean

run:
	@cabal run tcp://127.0.0.1:6000

build:
	@cabal build

clean:
	@cabal clean
