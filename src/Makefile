.PHONY: default
default: build

# Install the opam packages other than 'dune'
.PHONY: setup
setup:
	dune build geometricon.opam
	opam install --deps-only ./geometricon.opam

.PHONY: build
build:
	dune build

.PHONY: test
test:
	dune build tests/run_tests.exe
	_build/default/tests/run_tests.exe

.PHONY: install
install:
	dune install

.PHONY: clean
clean:
	dune clean
	git clean -dfXq

.PHONY: fmt
.IGNORE: fmt
fmt:
	dune build @fmt
	dune promote