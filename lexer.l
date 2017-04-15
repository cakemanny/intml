%{
#include <stdlib.h>
#include <string.h>
#include "ast.h"
#include "symbols.h"
#include "grammar.tab.h"

extern int yylineno;

%}

INT     [1-9][0-9]*
ID      [a-z][[:alnum:]]*
TYPE    [A-Z][[:alnum:]]*
WS      [ \t\f\v\r]

%%

"(*"            {
                    /* swallow comments */
                    int c, next = 1;
                    while ((c = input()) != 0) {
                        if (c == '\n') {
                            ++yylineno;
                        } else if (c == '*') {
                            c = input();
                            if (c == ')') {
                                if (--next == 0)
                                    break;
                            } else {
                                unput(c);
                            }
                        } else if (c == '(') {
                            if ((c = input()) == '*')
                                ++next;
                            else
                                unput(c);
                        } else if (c == 0) {
                            return ERROR;
                        }
                    }
                }

 /* keywords */
let             { return LET; }
in              { return IN; }
type            { return TYPE; }
"()"            { return UNIT; }

 /* built in types*/
{INT}           { yylval.intVal = atoi(yytext); return INT; }

{ID}            { yylval.identifier = symbol(yytext); return ID; }
{TYPE}          { yylval.identifier = symbol(yytext); return TYPE; }
 /* punctuation */
"<="            { return LE; }
[-+*/=<=()]     { return yytext[0]; }

{WS}            ; /* Drop whitespace */
[\n]            { ++yylineno; }

%%


