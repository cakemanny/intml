#ifndef __AST_H__
#define __AST_H__

#include "symbols.h"

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
struct TypeInfo;

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
typedef struct TypeInfo TypeInfo;

/* define our tree */

struct DeclarationList {
    Declaration* declaration;
    DeclarationList* next;
};

struct Func {
    Symbol* name;
    ParamList* params;
    Expr* body;
};

struct Binding {
    Symbol* name;
    Expr* init;
};

struct Type {
    Symbol* name;
    /* TODO define type trees */
};

struct Declaration {
    enum DeclTag {
        DECL_FUNC = 1,
        DECL_BIND,
        DECL_TYPE
    }           tag;
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
    Symbol* name;
    /* we may want to be able to add type info here */
};


struct FuncExpr {
    Symbol* name;
    ParamList* params;
    Expr* body;
    Expr* subexpr;
};

struct BindExpr {
    Symbol* name;
    Expr* init;
    Expr* subexpr;
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
        INTVAL,
        FUNC_EXPR,
        BIND_EXPR
    }           tag;
    union {
        struct {
            Expr*   left;
            Expr*   right;
        };
        Symbol*     var;
        int         intVal;
        FuncExpr    func;
        BindExpr    binding;
    };
    /* We will want to be able to type all our expressions */
    TypeInfo*   type_info;
};

/*
 * Print the AST
 */
void print_tree(FILE* out, DeclarationList* root);

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
Declaration* func(Symbol* name, ParamList* params, Expr* body);

/*
 * Create a global binding node
 */
Declaration* binding(Symbol* name, Expr* init);

/*
 * Construct a param node from a symbol
 */
Param* param(Symbol* name);

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
Expr* var(Symbol* name);

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
Expr* local_func(Symbol* name, ParamList* params, Expr* body, Expr* subexpr);

/*
 * Creates a local variable binding and subexpression node which uses
 * the binding
 */
Expr* local_binding(Symbol* name, Expr* init, Expr* subexpr);


#endif // __AST_H__
