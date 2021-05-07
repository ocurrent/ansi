.PHONY: test build

test:
	dune build @check

build:
	dune build @all

clean:
	dune clean
