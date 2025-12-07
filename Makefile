.PHONY: example example-docker

SCHEME=chibi

example:
	COMPILE_SCHEME=${SCHEME} compile-scheme -I src/ -o example examples/simple.scm
	./example

example-docker:
	docker build --build-arg SCHEME=${SCHEME} --tag=example-${SCHEME} .
	docker run -v "${PWD}:/workdir" -w /workdir -t example-${SCHEME} sh -c "make SCHEME=${SCHEME} example"