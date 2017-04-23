#ifndef __AST_H__
#define __AST_H__

#include "symbols.h"
#include <stdio.h> // FILE*

// Pre-declare our struct types to allow recursive tree
struct DeclarationList;
struct Declaration;
struct Func;
struct Binding;
struct Type;
struct ParamList;
struct Param;
struct Expr;
struct FuncExpr;
struct BindExpr;
struct TypeExpr;

// typedef everything to make it all nicer to read and type
typedef struct DeclarationList DeclarationList;
typedef struct Declaration Declaration;
typedef struct Func Func;
typedef struct Binding Binding;
typedef struct Type Type;
typedef struct ParamList ParamList;
typedef struct Param Param;
typedef struct Expr Expr;
typedef struct FuncExpr FuncExpr;
typedef struct BindExpr BindExpr;
typedef const struct TypeExpr TypeExpr; // can use "struct TypeExpr" for mutable version in constructors

/* define our tree */

struct DeclarationList {
    Declaration* declaration;
    DeclarationList* next;
};

struct Func {
    Symbol name;
    ParamList* params;
    Expr* body;

    TypeExpr* type;
};

struct Binding {
    Symbol name;
    Expr* init;

    TypeExpr* type;
};

struct Type {
    Symbol name;
    TypeExpr* definition;
};

struct Declaration {
    enum DeclTag {
        DECL_FUNC = 1,
        DECL_BIND,
        DECL_TYPE
    }               tag;
    union {
        Func        func;
        Binding     binding;
        Type        type;
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

    int var_id; // we misuse these in a closure list and tag them as var
};


struct FuncExpr {
    Symbol name;
    ParamList* params;
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
    Symbol name;
    Expr* init;
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
        FUNC_EXPR,
        BIND_EXPR,
        IF_EXPR,
    }               tag;
    union {
        struct { /* PLUS - APPLY */
            Expr*   left;
            Expr*   right;
        };
        struct {
            Symbol  var;        /* VAR */
            int     var_id; // tag variables as they are used in the program text
            int     function_id; // tag function that closes the variable
        };
        int         intval;     /* INTVAL */
        FuncExpr    func;       /* FUNC_EXPR */
        BindExpr    binding;    /* BIND_EXPR */
        struct { /* IF_EXPR */
            Expr*   condition;
            Expr*   btrue;      // true branch
            Expr*   bfalse;     // false branch
        };
    };
    /* We will want to be able to type all our expressions */
    TypeExpr*       type;
};

struct TypeExpr {
    enum TypeExprTag {
        TYPE_NAME = 1,
        TYPE_ARROW
    } tag;
    union {
        Symbol name;
        struct {
            TypeExpr* left;
            TypeExpr* right;
        };
    };
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
 * Returns a reverse declaration list
 */
DeclarationList* reverse_declarations(DeclarationList* list);

/*
 * Create a global Func node
 */
Declaration* func(Symbol name, ParamList* params, Expr* body);

/*
 * Create a global binding node
 */
Declaration* binding(Symbol name, Expr* init);

/*
 * Creates a type declaration node
 */
Declaration* type(Symbol name, TypeExpr* definition);

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
 * Returns a reversed param list
 */
ParamList* reverse_params(ParamList* list);

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
 * Creates a local function binding and subexpression node which uses
 * the binding
 */
Expr* local_func(Symbol name, ParamList* params, Expr* body, Expr* subexpr);

/*
 * Creates a local variable binding and subexpression node which uses
 * the binding
 */
Expr* local_binding(Symbol name, Expr* init, Expr* subexpr);

/*
 * Creates a if-then-else expression node
 */
Expr* ifexpr(Expr* condition, Expr* btrue, Expr* bfalse);

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

#endif // __AST_H__
