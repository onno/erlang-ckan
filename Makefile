ERL ?= erl
APP := ckan

.PHONY: deps

all: deps
	@./rebar compile

deps:
	@./rebar get-deps

clean:
	@./rebar clean

distclean: clean
	@./rebar delete-deps

test: all
	@./rebar eunit suite=ckan_test

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'
