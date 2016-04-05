.SUFFIXES:

SHELL = /bin/bash
SCHEME = chibi-scheme -Ilib -Irapid-lib -Irapid-macros

all:
	cd tests && $(MAKE) $@

check: all
	cd tests && $(MAKE) $@

.PHONY: all check
