TEST-RUNNER = chibi-scheme -I../lib -Ilib  
SCHEME = ../bin/scheme
UNIT-TESTS = *-tests.scm
INTEGRATION-TESTS = check_simple

all:

check: $(UNIT-TESTS) $(INTEGRATION-TESTS)

check_simple: all
	$(SCHEME) ../examples/example.scm | grep -q "3"

%-tests.scm: all
	./testrunner.sh $@

clean:
	rm -f testrunner.sh

.PHONY: all check $(INTEGRATION_TESTS)

