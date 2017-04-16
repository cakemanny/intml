%{
#include <stdio.h>
#include <string.h>
#include "ast.h"
#include "symbols.h"

extern int yylex();
static void yyerror(const char* msg);

extern int yylineno;

DeclarationList* tree = NULL;

%}

%union {
/* AST */
    DeclarationList*    declarations;
    Declaration*        declaration;
    Expr*               expr;
    ParamList*          params;
    Param*              param;
    TypeExpr*           typexpr;

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
%type <declaration> declaration letdecl typedecl
%type <params> params
%type <param> param
%type <expr> expr letexpr exprterm
%type <typexpr> typexpr typeterm

%right LET      /* These are to make let bindings stick to top level if poss */
%right IN       /* should be precedence in bison v3 */
%right ARROW    /* function typexprs */
%left '='       /* right in assignments but left in expressions */
%nonassoc '<' LE
%left '+' '-'
%left '*' '/'
%left ID UNIT INT '('  /* function application */

%%

program:
    declarations                { tree = $$ = reverse_declarations($1); }
  ;
declarations:
    declarations declaration    { $$ = add_declaration($1, $2); }
  | declaration                 { $$ = declaration_list($1); }
  ;
/* we will add types to this*/
declaration:
    letdecl                     { $$ = $1; }
  | typedecl                    { $$ = $1; }
  ;
letdecl:
    LET ID '=' expr             { $$ = binding($2, $4); }
  | LET ID params '=' expr      { $$ = func($2, reverse_params($3), $5); }
  ;
params:
    params param                { $$ = add_param($1, $2); }
  | param                       { $$ = param_list($1); }
  ;
param:
    ID                      { $$ = param($1); }
  | '(' ID ':' typexpr ')'  { $$ = param_with_type($2, $4); }
  | UNIT                    { $$ = param(symbol("()")); }
  ;
expr:
    letexpr             { $$ = $1; }
  | expr '+' expr       { $$ = plus($1, $3); }
  | expr '-' expr       { $$ = minus($1, $3); }
  | expr '*' expr       { $$ = multiply($1, $3); }
  | expr '/' expr       { $$ = divide($1, $3); }
  | expr '=' expr       { $$ = equal($1, $3); }
  | expr '<' expr       { $$ = lessthan($1, $3); }
  | expr LE expr        { $$ = lessequal($1, $3); }
    /* in real ML this should be "expr expr" */
    /* but we need to disambiguate */
  | expr exprterm       { $$ = apply($1, $2); }
  | exprterm            { $$ = $1; }
  ;
exprterm:
    '(' expr ')'        { $$ = $2; }
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
typedecl:
    TYPE ID '=' typexpr     { $$ = type($2, $4); }
  ;
typexpr:
    /* should be typexpr -> typexpr  but use terminals on right to
     * clear up ambiguities in the grammar
     */
    typeterm ARROW typexpr  { $$ = typearrow($1, $3); }
  | typeterm                { $$ = $1; }
typeterm:
    '(' typexpr ')'         { $$ = $2; }
  | ID                      { $$ = typename($1); }
  ;
%%

int yywrap() { return 1; }

void yyerror(const char* msg)
{
    fprintf(stderr, "line:%d: error: %s\n", yylineno, msg);
}

int main(int argc, char* argv[])
{
    yyparse();
    if (tree) {
        print_tree(stdout, tree);
        printf("\n");

        // type check!
    }
}

