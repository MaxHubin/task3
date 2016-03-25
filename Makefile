REBAR = `which rebar`

all: deps compile

deps:
	@( $(REBAR) get-deps )

compile: clean
	@( $(REBAR) compile )

clean:
	@( $(REBAR) clean )

run:
	@( erl +P 100000  -mnesia dir '"./db"' -pa ebin deps/*/ebin -s webserver )

.PHONY: all deps compile clean run