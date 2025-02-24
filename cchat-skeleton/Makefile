all: *.erl lib/*.erl
	make -C lib
	erl -make

clean:
	rm -f *.beam *.dump

run_tests: all
	erl -noshell -eval "eunit:test(test_client), halt()"
