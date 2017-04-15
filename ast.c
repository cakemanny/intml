
#include <stdlib.h>
#include <stdio.h>
#include "ast.h"

// convenience, check memory returned by malloc
static void* xmalloc(size_t size)
{
    void* x = malloc(size);
    if (!x) {
        fprintf(stderr, "out of memory\n");
        abort();
    }
    return x;
}


DeclarationList* declaration_list(Declaration* node)
{
    DeclarationList* result = xmalloc(sizeof *result);
    result->declaration = node;
    result->next = NULL;
    return result;
}


DeclarationList* add_declaration(DeclarationList* list, Declaration* node)
{
    DeclarationList* result = xmalloc(sizeof *result);
    result->declaration = node;
    result->next = list;
    return result;
}

DeclarationList* reverse_declarations(DeclarationList* list)
{
    DeclarationList* tmp = NULL;
    DeclarationList* newhead = NULL;
    for (DeclarationList* c = list; c; c = tmp)
    {
        /*
         *  A -> B -> C -> NULL
         *  A -> NULL,          B -> C -> NULL
         *  B -> A -> NULL,     C -> NULL
         *  C -> B -> A -> NULL
         */
        tmp = c->next;      // Grab C
        c->next = newhead;  // Repoint B
        newhead = c;        // adjust new head
        // c = tmp (in for above) // move next
    }
    return newhead;
}

Declaration* func(Symbol* name, ParamList* params, Expr* body)
{
    Declaration* result = xmalloc(sizeof *result);
    result->tag = DECL_FUNC;
    result->func.name = name;
    result->func.params = params;
    result->func.body = body;
    return result;
}


Declaration* binding(Symbol* name, Expr* init)
{
    Declaration* result = xmalloc(sizeof *result);
    result->tag = DECL_BIND;
    result->binding.name = name;
    result->binding.init = init;
    return result;
}

static Param* param(Symbol* name)
{
    Param* param = xmalloc(sizeof *param);
    param->name = name;
    return param;
}
ParamList* add_param(ParamList* list, Symbol* name)
{
    ParamList* result = xmalloc(sizeof *result);
    result->param = param(name);
    result->next = list;
    return result;
}


ParamList* param_list(Symbol* name)
{
    ParamList* list = xmalloc(sizeof *list);
    list->param = param(name);
    list->next = NULL;
    return list;
}

ParamList* reverse_params(ParamList* list)
{
    ParamList* tmp;
    ParamList* newhead = NULL;
    while (list) { // three pointer shuffle!
        tmp         = list->next;
        list->next  = newhead;
        newhead     = list;
        list        = tmp;
    }
    return newhead;
}

static Expr* binexpr(int tag, Expr* left, Expr* right)
{
    Expr* result = xmalloc(sizeof *result);
    result->tag = tag;
    result->left = left;
    result->right = right;
    result->type_info = NULL;
    return result;
}
Expr* plus(Expr* left, Expr* right)
{
    return binexpr(PLUS, left, right);
}
Expr* minus(Expr* left, Expr* right)
{
    return binexpr(MINUS, left, right);
}
Expr* multiply(Expr* left, Expr* right)
{
    return binexpr(MULTIPLY, left, right);
}
Expr* divide(Expr* left, Expr* right)
{
    return binexpr(DIVIDE, left, right);
}
Expr* equal(Expr* left, Expr* right)
{
    return binexpr(EQUAL, left, right);
}
Expr* lessthan(Expr* left, Expr* right)
{
    return binexpr(LESSTHAN, left, right);
}
Expr* lessequal(Expr* left, Expr* right)
{
    return binexpr(LESSEQUAL, left, right);
}

Expr* apply(Symbol* left, Expr* right)
{
    return binexpr(APPLY, var(left), right);
}

static Expr* expr(int tag)
{
    Expr* expr = xmalloc(sizeof *expr);
    expr->tag = tag;
    /*
     * NULL out the type info because that will be deduced later
     */
    expr->type_info = NULL;
    return expr;
}

Expr* var(Symbol* name)
{
    Expr* result = expr(VAR);
    result->var = name;
    return result;
}

Expr* unit_expr()
{
    Expr* result = expr(VAR);
    result->var = symbol("()");
    return result;
}

Expr* intval(int value)
{
    Expr* result = expr(INTVAL);
    result->intVal = value;
    return result;
}

Expr* local_func(Symbol* name, ParamList* params, Expr* body, Expr* subexpr)
{
    Expr* result = expr(FUNC_EXPR);
    FuncExpr* func = &result->func;
    func->name = name;
    func->params = params;
    func->body = body;
    func->subexpr = subexpr;
    return result;
}

Expr* local_binding(Symbol* name, Expr* init, Expr* subexpr)
{
    Expr* result = expr(BIND_EXPR);
    BindExpr* bind = &result->binding;
    bind->name = name;
    bind->init = init;
    bind->subexpr = subexpr;
    return result;
}


