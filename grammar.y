%{
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "ast.h"
#include "symbols.h"
#include "types_and_vars.h"
#include "codegen.h"

extern int yylex();
static void yyerror(const char* msg);

/* Line number from flex lexer */
extern int yylineno;
/* Input file used by flex lexer */
extern FILE* yyin;

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
    Symbol              identifier;
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
    int debug = 0;
    char* inarg = NULL;
    char* outarg = NULL;
    for (int i = 1; i < argc; i++) {
        char* c = argv[i];
        if (strcmp(c, "-v") == 0) {
            debug = debug_type_checker = 1;
            break;
        } else if (i + 1 < argc && strcmp(c, "-o") == 0) {
            if (outarg) {
                fprintf(stderr, "intml: error: too many output files\n");
                exit(EXIT_FAILURE);
            }
            outarg = argv[++i];
        } else {
            if (inarg) {
                fprintf(stderr, "intml: error: too many input files\n");
                exit(EXIT_FAILURE);
            }
            inarg = c;
        }
    }
    // want to check we have input before potentially destroying an output file
    // which was supposed to be input
    if (inarg) {
        if (strcmp(inarg, "-") == 0) {
            yyin = stdin; // default anyway
        } else {
            if (!(yyin = fopen(inarg, "r"))) {
                perror(inarg);
                exit(EXIT_FAILURE);
            }
        }
    } else {
        fprintf(stderr, "intml: error: no input files\n");
        exit(EXIT_FAILURE);
    }
    if (outarg) {
        if (!(cgenout = fopen(outarg, "w"))) {
            perror(outarg);
            exit(EXIT_FAILURE);
        }
    } else {
        cgenout = stdout;
    }

    debug_type_checker = debug;
    debug_codegen = debug;


    yyparse();
    if (tree) {
        if (debug) {
            print_tree(stdout, tree);
            printf("\n");
        }
        // type check!
        type_check_tree(tree);
        // Check we have a main function <- entry point
        check_runtime_properties(tree);
        if (debug) {
            fprintf(stderr, "found enough components of a runnable program (e.g. main)\n");
        }

        {
            /* ideal target: */
            // High level tree optimization
            //optimize1(tree);
            // Transform to 3AC (Three address code)
            //ir_tree ir = transform();
            //optimize2(ir);
            // Generate code?
        }
        {
            /* idea for now */
            // Generate stack machine code
            codegen(tree);
        }
    }
}

