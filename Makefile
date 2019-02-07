.PHONY: run build clean

run:
	@stack run tcp://127.0.0.1:6000

build:
	@stack build

clean:
	@stack clean
