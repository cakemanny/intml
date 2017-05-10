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

static DeclarationList* tree = NULL;

%}

%union {
/* AST */
    DeclarationList*    declarations;
    Declaration*        declaration;
    Expr*               expr;
    ExprList*           exprs;
    Pattern*            pattern;
    PatternList*        patterns;
    TPat*               tpat;
    ParamList*          params;
    Param*              param;
    TypeExpr*           typexpr;
    TypeExprList*       types;
    Case*               kase;
    CaseList*           kases;
    Ctor*               ctor;
    CtorList*           ctors;

/* terminals */
    int                 intval;     // we should probably store these
                                    // as strings/symbols as well...
    _Bool               boolval;
    Symbol              identifier;
    Symbol              text;
    const char*         error_msg;
}

%token LET REC TYPE IN IF THEN ELSE MATCH WITH FUNCTION FUN
%token <intval> INT
%token <boolval> BOOL
%token <text>   STR_LIT
%token <identifier> ID CTORID
%token <error> ERROR
%token EOFTOK

%type <declarations> program declarations
%type <declaration> declaration letdecl
%type <pattern> pattern
%type <tpat> tpattern
%type <params> params
%type <param> param
%type <exprs> exprlist nonemptylist tuplexpr
%type <expr> expr letexpr exprterm
%type <typexpr> typexpr typeterm optionaltype
%type <types> typetuple
%type <kases> matchings
%type <kase> matching
%type <ctors> constructors
%type <ctor> constructor

%nonassoc LET IN TYPE EXTERNAL MATCH WITH FUN FUNCTION AND
%right ARROW    /* function typexprs */
%nonassoc '[' ']' VSTART VEND
%right ';'
%nonassoc IF THEN ELSE
%left ',' /* not really, but makes list-building easier */
%left '=' '|'   /* right in assignments but left in expressions */
%nonassoc '<' LE
%right '^' '@'
%nonassoc OF
%right CONS
%left '+' '-'
%left '*' '/'
/* function application -note, typecheck will stop strings and ints being
   functions, rather than the grammar */
%left ID CTORID INT BOOL STR_LIT '('

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
  | TYPE ID '=' typexpr         { $$ = type($2, $4); }
  | TYPE ID '=' constructors    { $$ = type_ctor($2, reverse_ctors($4)); }
  | EXTERNAL ID ':' typexpr '=' STR_LIT
    {
      $$ = externdecl($2, $4, $6);
    }
  ;
letdecl:
    LET pattern '=' expr        { $$ = binding($2, $4); }
  | LET ID params optionaltype '=' expr
    { $$ = func_w_type($2, reverse_params($3), $4, $6); }
  | LET REC ID params optionaltype '=' expr
    { $$ = recfunc_w_type($3, reverse_params($4), $5, $7); }
  ;
pattern:
    tpattern                    { $$ = tpat_to_pat($1); }
  ;
tpattern:
    ID                          { $$ = tpat_var($1); }
  | '(' pattern ')'             { $$ = tpat_pattern($2); }
  | '_'                         { $$ = tpat_discard(); }
  | tpattern CONS tpattern      { $$ = tpat_cons($1, $3); }
  | tpattern ',' tpattern       { $$ = tpat_tuple($1, $3); }
  ;
params:
    params param                { $$ = add_param($1, $2); }
  | param                       { $$ = param_list($1); }
  ;
param:
    ID                          { $$ = param($1); }
  | '(' ID ':' typexpr ')'      { $$ = param_with_type($2, $4); }
  | '(' ')'                     { $$ = param(symbol("()")); }
  ;
expr:
    letexpr                     { $$ = $1; }
  | IF expr THEN expr ELSE expr { $$ = ifexpr($2,$4,$6); }
  /* TODO add '^' string concatenation */
  | expr '+' expr               { $$ = plus($1, $3); }
  | expr '-' expr               { $$ = minus($1, $3); }
  | expr '*' expr               { $$ = multiply($1, $3); }
  | expr '/' expr               { $$ = divide($1, $3); }
  | expr '=' expr               { $$ = equal($1, $3); }
  | expr '<' expr               { $$ = lessthan($1, $3); }
  | expr LE expr                { $$ = lessequal($1, $3); }
    /* in real ML this should be "expr expr" */
    /* but we need to disambiguate */
  | expr exprterm               { $$ = apply($1, $2); }
  | exprterm                    { $$ = $1; }
  | MATCH expr WITH matchings   { $$ = match($2, reverse_cases($4)); }
  | tuplexpr %prec ';'          { $$ = tuple(reverse_list($1)); }
  ;
tuplexpr:
    expr ',' expr               { $$ = add_expr(add_expr(exprlist(), $1), $3); }
  | tuplexpr ',' expr           { $$ = add_expr($1, $3); }
  ;
exprterm:
    '(' expr ')'                { $$ = $2; }
  | '(' expr ':' typexpr ')'    { $$ = $2; $2->type = $4; }
  | '[' exprlist ']'            { $$ = list($2); }
  | VSTART exprlist VEND        { $$ = vector($2); }
  | ID                          { $$ = var($1); }
  | CTORID                      { $$ = var($1); }
  | '(' ')'                     { $$ = unit_expr(); }
  | INT                         { $$ = intval($1); }
  | STR_LIT                     { $$ = strval($1); }
  ;
letexpr:
    LET ID params optionaltype '=' expr IN expr
    {
      $$ = local_func_w_type($2, reverse_params($3), $4, $6, $8);
    }
  | LET REC ID params optionaltype '=' expr IN expr
    {
      $$ = local_recfunc_w_type($3, reverse_params($4), $5, $7, $9);
    }
  | LET pattern '=' expr IN expr
    {
      $$ = local_binding($2, $4, $6);
    }
  ;
optionaltype:
    /* empty */     { $$ = NULL; /* will become type constraint in TC*/ }
  | ':' typexpr     { $$ = $2; }
  ;
exprlist:
    /* empty */             { $$ = exprlist(); }
  | nonemptylist            { $$ = $1; }
  ;
nonemptylist:
    expr                    { $$ = add_expr(exprlist(), $1); }
  | expr ';' nonemptylist   { $$ = add_expr($3, $1); }
  ;
matchings:
    matchings '|' matching      { $$ = case_add($1, $3); }
  | '|' matching                { $$ = caselist($2); } /* optional first | */
  | matching                    { $$ = caselist($1); }
  ;
matching:
    pattern ARROW expr          { $$ = matchcase($1, $3); }
  ;
typexpr:
    typexpr ARROW typexpr       { $$ = typearrow($1, $3); }
  | typetuple                   { $$ = typetuple(reversed_types($1));  }
  | typeterm                    { $$ = $1; }
  ;
typeterm:
    '(' typexpr ')'             { $$ = $2; }
  | typeterm ID                 { $$ = typeconstructor($1, $2); }
  | ID                          { $$ = typename($1); }
  ;
typetuple:
    /* these need to be a list as (1,2,3) != ((1,2),3) and != (1,(2,3))   */
    typeterm '*' typeterm       { $$ = type_add(type_list($1), $3); }
  | typetuple '*' typeterm      { $$ = type_add($1, $3); }
  ;
constructors:
    constructor                     { $$ = ctor_list($1); }
  | constructors '|' constructor    { $$ = add_ctor($1, $3); }
  ;
constructor:
    CTORID                      { $$ = ctor_noarg($1); }
  | CTORID OF typexpr           { $$ = ctor_warg($1, $3); }
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
    int parse_only = 0;
    int stop_after_type_check = 0;
    char* inarg = NULL;
    char* outarg = NULL;
    for (int i = 1; i < argc; i++) {
        char* c = argv[i];
        if (strcmp(c, "-v") == 0) {
            debug = debug_type_checker = 1;
        } else if (strcmp(c, "-p") == 0) {
            parse_only = 1;
        } else if (strcmp(c, "-t") == 0) {
            stop_after_type_check = 1;
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
    if (!tree) {
        exit(EXIT_FAILURE);
    }
    if (debug || parse_only) {
        print_tree(stdout, tree);
        printf("\n");
    }
    if (parse_only)
        return 0;
    // type check!
    type_check_tree(tree);
    if (stop_after_type_check)
        return 0;

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

    if (cgenout != stdout)
        fclose(cgenout);
    if (yyin != stdin)
        fclose(yyin);
}

