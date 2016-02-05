.SUFFIXES:

SHELL = /bin/bash
SCHEME = chibi-scheme -Ilib

all:
	cd tests && $(MAKE) $@

check: all
	cd tests && $(MAKE) $@

.PHONY: all check
