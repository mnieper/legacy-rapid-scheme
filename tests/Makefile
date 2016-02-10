TEST-RUNNER = chibi-scheme -I../lib -Ilib  
TESTS = *-tests.scm

all:

check: $(TESTS)

%-tests.scm: all
	./testrunner.sh $@

clean:
	rm -f testrunner.sh

.PHONY: all check
