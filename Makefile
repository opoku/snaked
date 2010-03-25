all:
	(cd src && erl -make)

clean:
	rm -fr ebin/*.beam