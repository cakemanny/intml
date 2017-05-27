SHELL = /bin/sh
.SUFFIXES:

PLATFORM=$(shell uname)
ARCH=$(shell uname -m)

LD=ld
LDFLAGS=
LDLIBS=-lc

ifeq "$(PLATFORM)" "Darwin"
  LDFLAGS += -macosx_version_min $(shell sw_vers -productVersion)
endif
ifeq "$(OS)" "Windows_NT"
  # on windows it was easier just to use the C compiler to link
  LD=gcc
  LDLIBS=
  SUFF=.exe
endif

TARGETS := simple0 simple1
ifneq "$(ARCH)" "armv7l"
  TARGETS += simple1.5
endif
TARGETS += simple2 simple3 simple4 simple5 complex0
TARGETS += lists0 lists1 lists2
TARGETS := $(addsuffix $(SUFF),$(TARGETS))

.PHONY: test

test: tests $(TARGETS)
	/bin/bash tests

# The rules below didn't seem to work in mingw32-make
%.exe: %.ml
	../../intml -o $(basename $<).s $<
	as -o $(basename $<).o $(basename $<).s
	$(LD) $(LDFLAGS) -o $@ $(basename $<).o ../../runtime.a $(LDLIBS)

%.o: %.s
	as -o $@ $<

%.s: %.ml ../../intml
	../../intml -o $@ $<

%: %.o
	$(LD) $(LDFLAGS) -o $@ $< ../../runtime.a $(LDLIBS)

.PHONY: clean

clean:
	rm -f $(TARGETS) *.o *.s

