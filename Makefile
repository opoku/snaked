all:
	mkdir -p ebin
	(cd src && make)

clean:
	rm -fr ebin/*.beam

test: all
	@echo "Compiling tests..."
	@erlc -I src -o ebin test/*.erl
	@echo "Running EUnit tests..."
	@erl -pa ebin -noshell -eval 'eunit:test(game_logic_tests, [verbose])' -s init stop