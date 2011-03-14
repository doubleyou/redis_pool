all: 
	rebar compile
	@escript release/build_rel.escript boot redis `pwd`/ebin

clean:
	rebar clean

test: all
	rebar eunit
