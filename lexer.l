%{
#include <stdlib.h>
#include <string.h>
#include "ast.h"
#include "symbols.h"
#include "grammar.tab.h"

// This is already present in the output from flex 2.5.35 on Apple
// But not present in the 2.5.4 version on gnuwin32.sourceforge.net
int yylineno;

#define MAXSTRLITLEN ((10 * 1024)-1)
char strbuf[MAXSTRLITLEN + 1];
char* strbuf_end = strbuf + MAXSTRLITLEN;

%}

INT     [1-9][0-9]*|0
ID      [a-z][[:alnum:]_]*
TYPE    [A-Z][[:alnum:]_]*
WS      [ \t\f\v\r]

%%

 /* multi-line comments */
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
                        } else if (c < 0) {
                            return ERROR;
                        }
                    }
                }

 /* strings */
\"              {
                    /* TODO: save line number here for any err messages */
                    char* s = strbuf;
                    int c;
                    while (s < strbuf_end && (c = input()) != 0) {
                        if (c == '"') {
                            break;
                        } else if (c == '\n') { // allow multi-line strings
                            *s++ = c;
                            ++yylineno;
                        } else if (c == '\\') {
                            c = input();
                            *s++ = (c == 'n') ? '\n'
                                 : (c == 'r') ? '\r'
                                 : (c == 't') ? '\t'
                                 : c;
                        } else {
                            *s++ = c;
                        }
                    }
                    *s = '\0'; // terminate string
                    if (s == strbuf_end) {
                        yylval.error_msg = "string literal too long";
                        return ERROR;
                    } else {
                        yylval.text = symbol(strbuf);
                        return STR_LIT;
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
 /* actually constructor */
{TYPE}          { yylval.identifier = symbol(yytext); return TYPE; }
 /* punctuation */
"->"            { return ARROW; }
"<="            { return LE; }
"[|"            { return VSTART; }  /* vectors (not arrays) */
"|]"            { return VEND; }
[-+*/=<=():\[\];]       { return yytext[0]; }

{WS}            ; /* Drop whitespace */
[\n]            { ++yylineno; }

.               { yylval.error_msg = strdup(yytext); return ERROR; }


%%


