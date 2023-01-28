.SUFFIXES:

SUFFIXES = .c .h .o .l .y .d

CC=gcc
CFLAGS=-std=gnu11 -g -Wall -fno-omit-frame-pointer
LDLIBS=
LDFLAGS=

RTCFLAGS=-std=gnu11 -Wall -fno-omit-frame-pointer

ifndef NDEBUG
  ifeq "$(shell uname)" "Darwin"
    # RTCFLAGS += -fsanitize=address
  endif
  ifneq "$(OS)" "Windows_NT"
    CFLAGS += -fsanitize=address
    LDFLAGS += -fsanitize=address
  endif
else
  CFLAGS += -O2 -march=native
endif

YACC=bison
YFLAGS=-d -v
LEX=flex
LFLAGS=

# Common files for both SRC and OSRC
BSRC=ast.c codegen.c symbols.c types_and_vars.c
# files that are edited by a hum
SRC=ast.h codegen.h grammar.y lexer.l symbols.h \
      types_and_vars.h runtime.c $(BSRC)
# deps for the final binary
OSRC=grammar.tab.c lex.yy.c $(BSRC)

TARGETS := intml runtime.a

all: $(TARGETS)

# Use default rule...
intml: $(OSRC:.c=.o)
	$(CC) $(LDFLAGS) -o $@ $^ $(LDLIBS)

grammar.tab.h grammar.tab.c: grammar.y
	$(YACC) $(YFLAGS) $<

lex.yy.c: lexer.l grammar.tab.h
	$(LEX) $(LFLAGS) $<

runtime.a: runtime.o
	$(AR) -r $@ $^

runtime.o: runtime.c
	$(CC) $(RTCFLAGS) $(CPPFLAGS) $(TARGET_ARCH) -c -o $@ $<

%.o: %.c
	$(CC) $(CFLAGS) $(CPPFLAGS) $(TARGET_ARCH) -c -o $@ $<

.PHONY: clean

clean:
	$(RM) $(TARGETS) intml.exe *.o lex.yy.c grammar.tab.{c,h} grammar.output *.d

%.d: %.c $(SRC)
	$(CC) $(CFLAGS) -MM $< > $@

ifneq "$(MAKECMDGOALS)" "clean"
  -include ${OSRC:.c=.d}
endif

