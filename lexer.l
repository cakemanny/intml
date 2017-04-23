%{
#include <stdlib.h>
#include <string.h>
#include "ast.h"
#include "symbols.h"
#include "grammar.tab.h"

// This is already present in the output from flex 2.5.35 on Apple
// But not present in the 2.5.4 version on gnuwin32.sourceforge.net
int yylineno;

%}

INT     [1-9][0-9]*|0
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
if              { return IF; }
then            { return THEN; }
else            { return ELSE; }

 /* built in types*/
{INT}           { yylval.intval = atoi(yytext); return INT; }
"()"            { return UNIT; }

{ID}            { yylval.identifier = symbol(yytext); return ID; }
{TYPE}          { yylval.identifier = symbol(yytext); return TYPE; }
 /* punctuation */
"->"            { return ARROW; }
"<="            { return LE; }
[-+*/=<=():]    { return yytext[0]; }

{WS}            ; /* Drop whitespace */
[\n]            { ++yylineno; }

%%


