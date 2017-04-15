%{
#include <stdio.h>
#include <string.h>
#include "ast.h"
#include "symbols.h"

extern int yylex();
static void yyerror(const char* msg);

int yylineno = 1;

%}

%union {
/* AST */
    DeclarationList*    declarations;
    Declaration*        declaration;
    Expr*               expr;
    ParamList*          params;

/* terminals */
    int                 intVal;
    Symbol*             identifier;
}

%token LET TYPE IN
%token UNIT
%token <intVal> INT
%token <identifier> ID
%token <error> ERROR

%type <declarations> program declarations
%type <declaration> declaration letdecl
%type <params> params
%type <expr> expr letexpr

%right LET
%right IN       /* should be precedence in bison v3 */
%left '='       /* right in assignments but left in expressions */
%nonassoc '<' LE
%left '+' '-'
%left '*' '/'
%left ID        /* function application */

%%

program:
    declarations                { $$ = reverse_declarations($1); }
  ;
declarations:
    declarations declaration    { $$ = add_declaration($1, $2); }
  | declaration                 { $$ = declaration_list($1); }
  ;
/* we will add types to this*/
declaration:
    letdecl                     { $$ = $1; }
  /*| typedecl */
  ;
letdecl:
    LET ID params '=' expr      { $$ = func($2, reverse_params($3), $5); }
  | LET ID '=' expr             { $$ = binding($2, $4); }
  ;
params:
    params ID           { $$ = add_param($1, $2); }
  | params UNIT         { $$ = add_param($1, symbol("()")); }
  | ID                  { $$ = param_list($1); }
  | UNIT                { $$ = param_list(symbol("()")); }
  ;
expr:
    letexpr             { $$ = $1; }
  | '(' expr ')'        { $$ = $2; }
  | expr '+' expr       { $$ = plus($1, $3); }
  | expr '-' expr       { $$ = minus($1, $3); }
  | expr '*' expr       { $$ = multiply($1, $3); }
  | expr '/' expr       { $$ = divide($1, $3); }
  | expr '=' expr       { $$ = equal($1, $3); }
  | expr '<' expr       { $$ = lessthan($1, $3); }
  | expr LE expr        { $$ = lessequal($1, $3); }
    /* in real ML this should be "expr expr" */
  | ID expr             { $$ = apply($1, $2); }
  | ID                  { $$ = var($1); }
  | UNIT                { $$ = unit_expr(); }
  | INT                 { $$ = intval($1); }
  ;

letexpr:
    LET ID params '=' expr IN expr
    {
      $$ = local_func($2, reverse_params($3), $5, $7);
    }
  | LET ID '=' expr IN expr
    {
      $$ = local_binding($2, $4, $6);
    }
  ;
%%

int yywrap() { return 1; }

void yyerror(const char* msg)
{
    fprintf(stderr, "error line:%d: %s\n", yylineno, msg);
}

int main(int argc, char* argv[])
{
    yyparse();
}

