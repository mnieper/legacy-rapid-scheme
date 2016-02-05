SHELL = /bin/bash
SCHEME = chibi-scheme -Ilib

.SUFFIXES:

check: tests/tests.scm
	$(SCHEME) -Itests/lib $<

.PHONY: check
