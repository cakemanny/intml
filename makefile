
CC=gcc
CFLAGS=-std=gnu11 -g -Wall -fno-omit-frame-pointer
LDLIBS=
LDFLAGS=

ifndef NDEBUG
  ifneq "$(OS)" "Windows_NT"
    CFLAGS += -fsanitize=address
    LDFLAGS += -fsanitize=address
  endif
endif

YACC=bison
YFLAGS=-d -v
LEX=flex
LFLAGS=

# Use default rule...
intml: grammar.tab.o lex.yy.o ast.o symbols.o types_and_vars.o codegen.o
	$(CC) $(LDFLAGS) -o $@ $^ $(LDLIBS)

grammar.tab.h grammar.tab.c: grammar.y
	$(YACC) $(YFLAGS) $<

lex.yy.c: lexer.l
	$(LEX) $(LFLAGS) $<

.PHONY: clean

clean:
	rm -f intml.exe *.o lex.yy.c grammar.tab.{c,h} grammar.output

