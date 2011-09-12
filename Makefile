
ERLIBS=$(shell couch-config --erl-libs-dir)
COUCHVER=$(shell couch-config --couch-version)
COUCH_PATH=$(ERLIBS)/couch-$(COUCHVER)
CONFDIR=$(shell couch-config --config-dir)

all: get-deps compile

get-deps:
	@./rebar get-deps

compile:
	@ERL_COMPILER_OPTIONS='[{i, "$(COUCH_PATH)/include"}]' ./rebar compile

clean:
	@./rebar clean

install: all
	@mkdir -p $(ERLIBS)/couch_es
	@cp -r ebin $(ERLIBS)/couch_es
	@cp couch_es.ini $(CONFDIR)
	@echo "\n\
couch_es has been installed in $(ERLIBS)/couch_es\n\
To launch it run the commandline:\n\
\n\
	$ ERL_CFLAGS=\"-pa $(ERLIBS)/couch_es\" couchdb -a $(CONFDIR)/couch_es.ini\n"

uninstall:
	@rm -rf $(ERLIBS)/couch_es
	@rm -rf $(CONFDIR)/couch_es.ini
