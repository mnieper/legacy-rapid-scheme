TEST-RUNNER = chibi-scheme -I../lib -Ilib  
TESTS = *-tests.scm

all:

check: $(TESTS)

%-tests.scm: all
	OUTPUT=`$(TEST-RUNNER) $@`; if [ -n "$$OUTPUT" ]; then echo "$$OUTPUT"; exit 1; fi

.PHONY: all check
