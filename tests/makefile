
PLATFORM=$(shell uname)
ARCH=$(shell uname -m)

LD=ld
LDFLAGS=
LDLIBS=-lc

ifeq "$(PLATFORM)" "Darwin"
#  LDFLAGS += -macosx_version_min $(shell sw_vers -productVersion)
endif

TARGETS := simple0 simple1 simple1.5 simple2 simple3 simple4 simple5 complex0

.PHONY: test

test: tests $(TARGETS)
	/bin/bash tests

%.o: %.s
	as -o $@ $<

%.s: %.ml ../intml
	../intml -o $@ $<

%: %.o
	$(LD) $(LDFLAGS) -o $@ $< $(LDLIBS)

.PHONY: clean

clean:
	rm -f $(TARGETS) *.o *.s

