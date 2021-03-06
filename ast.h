#ifndef __AST_H__
#define __AST_H__

#include "symbols.h"
#include <stdio.h> // FILE*

#define DECLARE_STRUCT(name)  struct name; typedef struct name name

// Pre-declare our struct types to allow recursive tree
DECLARE_STRUCT( DeclarationList );
DECLARE_STRUCT( Declaration     );
DECLARE_STRUCT( Func            );
DECLARE_STRUCT( Binding         );
DECLARE_STRUCT( Type            );
DECLARE_STRUCT( External        );
DECLARE_STRUCT( TypeCtor        );
DECLARE_STRUCT( Ctor            );
DECLARE_STRUCT( CtorList        );
DECLARE_STRUCT( ParamList       );
DECLARE_STRUCT( Param           );
DECLARE_STRUCT( Pattern         );
DECLARE_STRUCT( PatternList     );
DECLARE_STRUCT( TPat            );
DECLARE_STRUCT( Expr            );
DECLARE_STRUCT( FuncExpr        );
DECLARE_STRUCT( BindExpr        );
DECLARE_STRUCT( ExternExpr      );
DECLARE_STRUCT( ExprList        );
DECLARE_STRUCT( Case            );
DECLARE_STRUCT( CaseList        );

/*
 * Declare types so that they are immutable as this has some nice properties
 * We may do some interning of these at some point
 */
struct TypeExpr;
struct TypeExprList;
typedef const struct TypeExpr TypeExpr; /* can use "struct TypeExpr" for
                                           mutable version in the constructor */
typedef const struct TypeExprList TypeExprList;


/* define our tree */

struct DeclarationList {
    Declaration* declaration;
    DeclarationList* next;
};

struct Func {
    Symbol name;
    ParamList* params;
    TypeExpr* resulttype;
    Expr* body;

    TypeExpr* type;
};

struct Binding {
    Pattern* pattern;
    Expr* init;

    TypeExpr* type;
};

struct Type {
    Symbol name;
    TypeExpr* definition;
};

struct External {
    Symbol name;
    TypeExpr* type;
    Symbol external_name;
};

struct TypeCtor {
    Symbol name;
    CtorList* ctors;
};

struct Declaration {
    enum DeclTag {
        DECL_FUNC = 1,
        DECL_BIND,
        DECL_TYPE,
        DECL_EXTERN,
        DECL_RECFUNC,
        DECL_TYPECTOR
    }               tag;
    int             an_line;
    union {
        Func        func;
        Binding     binding;
        Type        type;
        External    ext;
        TypeCtor    ctor;
    };
};

// These could probably be just just a single node type

struct ParamList {
    Param* param;
    ParamList* next;
};

struct Param {
    Symbol name;
    /* Maybe be either declared or deduced */
    TypeExpr* type;
    int an_line;

    int var_id; // we misuse these in a closure list and tag them as var
};

struct CtorList {
    Ctor* ctor;
    CtorList* next;
};

struct Ctor {
    enum CtorTag {
        CTOR_NOARG,
        CTOR_WARG,
    } tag;
    int an_line;
    Symbol name;
    TypeExpr* typexpr;  /* Only for use when tag == CTOR_WARG */

    int ctor_id; // for use is codegen
};


// TODO: Consider embedding these directly
struct FuncExpr {
    Symbol name;
    ParamList* params;
    TypeExpr* resulttype;
    Expr* body;
    Expr* subexpr;

    /* The deduced type of the function - which is not the same and the
     * type of the expression itself or it's body
     * i.e. typeof(param1) -> typeof(param2) -> ... -> typeof(body)
     */
    TypeExpr* functype;
    int var_id;         // We add ID's to the named declarations
    int function_id;
};

struct BindExpr {
    Pattern* pattern;
    Expr* init;
    Expr* subexpr;
};

struct ExternExpr {
    Symbol name;
    TypeExpr* functype;
    Symbol external_name;
    Expr* subexpr;

    int var_id;
};

struct Expr {
    enum ExprTag {
        PLUS = 1,
        MINUS,
        MULTIPLY,
        DIVIDE,
        EQUAL,
        LESSTHAN,
        LESSEQUAL,
        APPLY,
        VAR,
        UNITVAL,
        INTVAL,
        STRVAL,
        FUNC_EXPR,
        RECFUNC_EXPR,
        BIND_EXPR,
        IF_EXPR,
        LIST,
        VECTOR,
        TUPLE,
        EXTERN_EXPR, /* not in the language - just for codegen */
        MATCH_EXPR,
        DIRECT_CALL, /* codgen only! */
    }               tag;
    int             an_line;
    /* We will want to be able to type all our expressions */
    TypeExpr*       type;
    union {
        struct { /* PLUS - APPLY */
            Expr*   left;
            Expr*   right;
        };
        struct {
            Symbol  var;        /* VAR */
            int     var_id; // tag variables as they are used in the program text
            int     enclosing_func_id; // tag function that closes the variable
        };
        int         intval;     /* INTVAL */
        Symbol      strval;     /* STRVAL */
        FuncExpr    func;       /* FUNC_EXPR, RECFUNC_EXPR */
        BindExpr    binding;    /* BIND_EXPR */
        struct { /* IF_EXPR */
            Expr*   condition;
            Expr*   btrue;      // true branch
            Expr*   bfalse;     // false branch
        };
        ExprList*   expr_list; /* LIST, VECTOR, TUPLE */
        ExternExpr  ext;        /* EXTERN_EXPR */
        struct { /* MATCH_EXPR */
            Expr* matchexpr;
            CaseList* cases;
        };
        struct { /* DIRECT_CALL */
            Symbol  funcname;
            int     dc_var_id;
            int     dc_closingfunc_id;
            ExprList*   args;
            int     dc_func_id;
        };
    };
};

struct ExprList {
    Expr* expr;
    ExprList* next;
};

struct TypeExpr {
    enum TypeExprTag {
        TYPE_NAME = 1,
        TYPE_ARROW,
        TYPE_CONSTRAINT,
        TYPE_TUPLE,
        TYPE_CONSTRUCTOR,
    } tag;
    int an_line;
    union {
        Symbol name;                /* TYPE_NAME */
        struct {                    /* TYPE_ARROW */
            TypeExpr* left;
            TypeExpr* right;
        };
        int constraint_id;          /* TYPE_CONSTRAINT */
        TypeExprList* type_list;    /* TYPE_TUPLE */
        struct {                    /* TYPE_CONSTRUCTOR */
            TypeExpr* param;
            Symbol constructor;
        };
    };
};

struct TypeExprList {
    TypeExpr* type;
    TypeExprList* next;
};

/*  */
struct TPat {
    enum TPatTag {
        TPAT_VAR = 1,
        TPAT_DISCARD,
        TPAT_CONS,
        TPAT_TUPLE,
        TPAT_PATTERN,
        TPAT_CTOR_NOARG,
        TPAT_CTOR_WARG,
        TPAT_INT,
        TPAT_STR,
        TPAT_NIL,
    } tag;
    int an_line;
    union {
        Symbol name; /* TPAT_VAR */
        struct {
            TPat* left;  /* TPAT_CONS, TPAT_TUPLE */
            TPat* right;
        };
        Pattern* pattern; /* TPAT_PATTERN */
        struct {
            Symbol ctor_name;
            TPat* ctor_arg;
        };
        int intval; /* TPAT_INT */
        Symbol strval; /* TPAT_STR */
    };
};

struct Pattern {
    enum PatternTag {
        PAT_VAR = 1,
        PAT_DISCARD,
        PAT_CONS,
        PAT_TUPLE,
        PAT_CTOR_NOARG,
        PAT_CTOR_WARG,
        PAT_INT,
        PAT_STR,
        PAT_NIL,
    } tag;
    int an_line;
    TypeExpr* type;
    union {
        struct {
            Symbol name; /* PAT_VAR */
            int var_id; // used for tagging in codgen
        };
        struct {
            Pattern* left;  /* PAT_CONS */
            Pattern* right;
        };
        PatternList* pat_list;  /* PAT_TUPLE*/
        struct {
            Symbol ctor_name; /* PAT_CTOR_WARG, PAT_CTOR_NOARG */
            Pattern* ctor_arg; /* PAT_CTOR_WARG */
        };
        int intval; /* PAT_INT */
        Symbol strval; /* PAT_STR */
    };
};

struct PatternList {
    Pattern* pattern;
    PatternList* next;
};

struct Case {
    Pattern* pattern;
    Expr* expr;
    int an_line;
};

struct CaseList {
    Case* kase;
    CaseList* next;
};

/*
 * Print the AST
 */
void print_tree(FILE* out, const DeclarationList* root);
/*
 * Print an expression
 */
void print_expr(FILE* out, const Expr* expr);
/*
 * Print out a declration
 */
void print_declaration(FILE* out, const Declaration* decl);
/*
 * Print a type expression tree
 */
void print_typexpr(FILE*, TypeExpr*);

/*
 * Print a pattern expression
 */
void print_pattern(FILE* out, const Pattern* pat);

/*
 * Write our own printf just to make printing types a bit easier
 * Only defines, simply %d %s and %T where %T prints a type
 */
#define tprintf(out, fmt, ...) tprintfx(out, fmt, tp_count(fmt), ##__VA_ARGS__)
/* the actual variadic print */
void tprintfx(FILE* out, const char* fmt, int nargs, ...);
/* helper for our tprintf macro to count the number of arguments */
int tp_count(const char* fmt);

/*----------------------------------------*\
 * Some constructors and manipulator fns  *
\*----------------------------------------*/

/*
 * Create a new declaration list from one node
 */
DeclarationList* declaration_list(Declaration* node);

/*
 * Returns a new list with node pushed onto the head
 */
DeclarationList* add_declaration(DeclarationList* list, Declaration* node);

/*
 * Returns a reverse declaration list. *Mutates the list*
 */
DeclarationList* reverse_declarations(DeclarationList* list);

/*
 * Create a global Func node
 */
Declaration* func(Symbol name, ParamList* params, Expr* body);

/*
 * Create a global function with an explicit type specified in the program
 */
Declaration* func_w_type(
        Symbol name, ParamList* params, TypeExpr* type, Expr* body);

/*
 * Create a recursive function which can refer to it's name within its body
 */
Declaration* recfunc_w_type(
        Symbol name, ParamList* params, TypeExpr* type, Expr* body);

/*
 * Create a global binding node
 */
Declaration* binding(Pattern* pattern, Expr* init);

/*
 * Create a type declaration node
 */
Declaration* type(Symbol name, TypeExpr* definition);

/*
 * Create an external declaration node
 */
Declaration* externdecl(Symbol name, TypeExpr* type, Symbol external_name);

/*
 * Create a type declaration of a constructed type
 */
Declaration* type_ctor(Symbol name, CtorList* ctors);

/*
 * Construct a param node from a symbol
 */
Param* param(Symbol name);

/*
 * Construct a node with already attached type info
 */
Param* param_with_type(Symbol name, TypeExpr* expr);

/*
 * Returns a new list with node as the head
 */
ParamList* add_param(ParamList* list, Param* param);

/*
 * Creates list with a single param node which has the given name
 */
ParamList* param_list(Param* param);

/*
 * Returns a reversed param list. *Mutates the list*
 */
ParamList* reverse_params(ParamList* list);


Ctor* ctor_noarg(Symbol name);

Ctor* ctor_warg(Symbol name, TypeExpr* typexpr);

CtorList* ctor_list(Ctor* ctor);

CtorList* add_ctor(CtorList* list, Ctor* ctor);

CtorList* reverse_ctors(CtorList* list);

/*
 * The following all create binary expression nodes
 */
Expr* plus(Expr* left, Expr* right);
Expr* minus(Expr* left, Expr* right);
Expr* multiply(Expr* left, Expr* right);
Expr* divide(Expr* left, Expr* right);
Expr* equal(Expr* left, Expr* right);
Expr* lessthan(Expr* left, Expr* right);
Expr* lessequal(Expr* left, Expr* right);

/*
 * Creates a function application expression node
 */
Expr* apply(Expr* left, Expr* right);

/*
 * Creates an expression node for the use of a bound name
 * The *var* name is a bit of a misnomer as these will not be
 * mutable
 */
Expr* var(Symbol name);

/*
 * The () expression which has type unit
 */
Expr* unit_expr();

/*
 * Creates an expression node holding a literal int
 */
Expr* intval(int value);

/*
 * Create a string expression node
 */
Expr* strval(Symbol text);

/*
 * Creates a local function binding and subexpression node which uses
 * the binding
 */
Expr* local_func(Symbol name, ParamList* params, Expr* body, Expr* subexpr);

Expr* local_func_w_type(
        Symbol name, ParamList* params, TypeExpr* resulttype,
        Expr* body, Expr* subexpr);

/*
 * Creates a recursive function node, where-by the body can refer to the
 * function name
 */
Expr* local_recfunc(Symbol name, ParamList* params, Expr* body, Expr* subexpr);

Expr* local_recfunc_w_type(
        Symbol name, ParamList* params, TypeExpr* resulttype, Expr* body,
        Expr* subexpr);


/*
 * Creates a local variable binding and subexpression node which uses
 * the binding
 */
Expr* local_binding(Pattern* pattern, Expr* init, Expr* subexpr);

/*
 * Creates a if-then-else expression node
 */
Expr* ifexpr(Expr* condition, Expr* btrue, Expr* bfalse);

/*
 * Creates a list expression from the given expression list
 */
Expr* list(ExprList* exprs);

/*
 * Creates a vector expression of the given expression list
 */
Expr* vector(ExprList* exprs);

/*
 * Creates a tuple expression
 */
Expr* tuple(ExprList* exprs);

/*
 * Only used by codegen in rewriting
 */
Expr* local_extern(
        Symbol name, TypeExpr* type, Symbol external_name, Expr* subexpr);

/*
 * Creates a match expression node
 */
Expr* match(Expr* matchexpr, CaseList* cases);

/*
 *
 */
Expr* direct_call(Symbol funcname, ExprList* args);

/*
 * Creates an empty list of expressions (used in compund expressions like lists
 * and vectors
 */
ExprList* exprlist();

/*
 * Returns a new list with to_add as the head and list as the tail
 */
ExprList* add_expr(ExprList* list, Expr* to_add);

/*
 * Returns the same list, reversed
 */
ExprList* reverse_list(ExprList* list);

/*
 * Creates a function type expression
 * 'a -> 'b
 */
TypeExpr* typearrow(TypeExpr* left, TypeExpr* right);

/*
 * Creates a function expression that is just a name
 * e.g. int, unit, float
 */
TypeExpr* typename(Symbol name);

/*
 * Creates a contrained but unknown type, these are used by the type checker
 * to leave holes in the type tree that it can come back and fill in later
 */
TypeExpr* typeconstraint(int constraint_id);

/*
 * Creates a type tuple
 */
TypeExpr* typetuple(TypeExprList* type);

/*
 * Creates a constructed type where constrname is a type constructor
 * e.g. int list or string list
 */
TypeExpr* typeconstructor(TypeExpr* param, Symbol constrname);

/*
 * A new list with a type within, head
 */
TypeExprList* type_list(TypeExpr* head);

/*
 * Cons newhead onto the head of the list
 */
TypeExprList* type_add(TypeExprList* list, TypeExpr* newhead);

/*
 * Returns a new reversed list. Does not mutate the given list
 */
TypeExprList* reversed_types(TypeExprList* list);

/*
 * Create a value-name pattern
 */
Pattern* pat_var(Symbol name);

/*
 * A pattern which discards the expresion it's matched against
 */
Pattern* pat_discard();

/*
 * A pattern that matches the head and the tail of a list
 */
Pattern* pat_cons(Pattern* head, Pattern* tail);

/*
 *
 */
Pattern* pat_tuple(PatternList* list);

Pattern* pat_constr_warg(Symbol ctor_name, Pattern* ctor_arg);
Pattern* pat_constr_noarg(Symbol ctor_name);
Pattern* pat_int(int intval);
Pattern* pat_str(Symbol strval);
Pattern* pat_nil();

/*
 * A non-empty list.
 */
PatternList* pat_nel(Pattern* head);

/*
 * Returns a new list with newhead as the head and list as the tail
 */
PatternList* add_pat(PatternList* list, Pattern* newhead);

/*
 * Temporary pattern type that doesn't contain lists so that
 * we can get tuple grammar to work with the fact that parens create sub-tuples
 */
TPat* tpat_var(Symbol name);
TPat* tpat_discard();
TPat* tpat_cons(TPat* head, TPat* tail);
TPat* tpat_tuple(TPat* first, TPat* rest);
TPat* tpat_pattern(Pattern* pattern);
TPat* tpat_constr_warg(Symbol ctor_name, TPat* ctor_arg);
TPat* tpat_constr_noarg(Symbol ctor_name);
TPat* tpat_int(int intval);
TPat* tpat_str(Symbol strval);
TPat* tpat_nil();

/*
 * Convert from a TPat to a Pattern
 */
Pattern* tpat_to_pat(TPat* tpat);

/*
 * A case node for a match or function expression
 */
Case* matchcase(Pattern* pattern, Expr* expr);

/*
 * Create a case list with just one node (head)
 */
CaseList* caselist(Case* head);

/*
 *
 */
CaseList* case_add(CaseList* list, Case* kase);

/*
 * Reverses the list and returns the new head *Mutates the list*
 */
CaseList* reverse_cases(CaseList* list);

#endif // __AST_H__
