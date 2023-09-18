.PHONY: test build

test:
	dune build @runtest

build:
	dune build @all

clean:
	dune clean
