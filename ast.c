
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
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

Declaration* func(Symbol name, ParamList* params, Expr* body)
{
    Declaration* result = xmalloc(sizeof *result);
    result->tag = DECL_FUNC;
    result->func.name = name;
    result->func.params = params;
    result->func.body = body;
    result->func.type = NULL;
    return result;
}

Declaration* binding(Symbol name, Expr* init)
{
    Declaration* result = xmalloc(sizeof *result);
    result->tag = DECL_BIND;
    result->binding.name = name;
    result->binding.init = init;
    result->binding.type = NULL;
    return result;
}

Declaration* type(Symbol name, TypeExpr* definition)
{
    Declaration* result = xmalloc(sizeof *result);
    result->tag = DECL_TYPE;
    result->type.name = name;
    result->type.definition = definition;
    return result;
}

Param* param_with_type(Symbol name, TypeExpr* type)
{
    Param* param = xmalloc(sizeof *param);
    param->name = name;
    param->type = type;
    return param;

}
Param* param(Symbol name)
{
    return param_with_type(name, NULL);
}

ParamList* add_param(ParamList* list, Param* param)
{
    ParamList* result = xmalloc(sizeof *result);
    result->param = param;
    result->next = list;
    return result;
}


ParamList* param_list(Param* param)
{
    ParamList* list = xmalloc(sizeof *list);
    list->param = param;
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
    result->type = NULL;
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

Expr* apply(Expr* left, Expr* right)
{
    return binexpr(APPLY, left, right);
}

static Expr* expr(enum ExprTag tag)
{
    Expr* expr = xmalloc(sizeof *expr);
    expr->tag = tag;
    /*
     * NULL out the type info because that will be deduced later
     */
    expr->type= NULL;
    return expr;
}

Expr* var(Symbol name)
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

Expr* local_func(Symbol name, ParamList* params, Expr* body, Expr* subexpr)
{
    Expr* result = expr(FUNC_EXPR);
    FuncExpr* func = &result->func;
    func->name = name;
    func->params = params;
    func->body = body;
    func->subexpr = subexpr;
    func->functype = NULL; /* deduced by type checker */
    return result;
}

Expr* local_binding(Symbol name, Expr* init, Expr* subexpr)
{
    Expr* result = expr(BIND_EXPR);
    BindExpr* bind = &result->binding;
    bind->name = name;
    bind->init = init;
    bind->subexpr = subexpr;
    return result;
}

Expr* ifexpr(Expr* condition, Expr* btrue, Expr* bfalse)
{
    Expr* result = expr(IF_EXPR);
    result->condition = condition;
    result->btrue = btrue;
    result->bfalse = bfalse;
    return result;
}

static struct TypeExpr* typexpr(enum TypeExprTag tag)
{
    struct TypeExpr* result = xmalloc(sizeof *result);
    result->tag = tag;
    return result;
}

TypeExpr* typearrow(TypeExpr* left, TypeExpr* right)
{
    struct TypeExpr* result = typexpr(TYPE_ARROW);
    result->left = left;
    result->right = right;
    return result;
}

TypeExpr* typename(Symbol name)
{
    struct TypeExpr* result = typexpr(TYPE_NAME);
    result->name = name;
    return result;
}


/*----------------------------------------*\
 * Fns for printing the AST               *
\*----------------------------------------*/

void print_typexpr(FILE* out, TypeExpr* expr)
{
    switch (expr->tag) {
    case TYPE_NAME:
        fprintf(out, "%s", expr->name);
        break;
    case TYPE_ARROW:
        fputc('(', out);
        print_typexpr(out, expr->left);
        fputs(" -> ", out);
        print_typexpr(out, expr->right);
        fputc(')', out);
        break;
    }
}

void print_params(FILE* out, const ParamList* params)
{
    fputc('(', out);
    const char* prefix = ""; // all but first get space prefix
    for (const ParamList* c = params; c; c = c->next) {
        fputs(prefix, out);
        if (c->param->type) {
            fprintf(out, "(param %s : ", c->param->name);
            print_typexpr(out, c->param->type);
            fputs(")", out);
        } else {
            fprintf(out, "(param %s)", c->param->name);
        }
        prefix = " ";
    }
    fputc(')', out);
}

void print_expr(FILE* out, const Expr* expr)
{
    fputc('(', out);
    const char* sym[9] = {"", "+", "-", "*", "/", "=", "<", "<=", "apply"};
    switch (expr->tag) {
    // type out all enum members to get exhaustiveness checking from the
    // compiler
    case PLUS:
    case MINUS:
    case MULTIPLY:
    case DIVIDE:
    case EQUAL:
    case LESSTHAN:
    case LESSEQUAL:
    case APPLY:
        fprintf(out, "%s ", sym[expr->tag]);
        print_expr(out, expr->left);
        fputc(' ', out);
        print_expr(out, expr->right);
        break;
    case VAR:
        fprintf(out, "var %s", expr->var);
        break;
    case INTVAL:
        fprintf(out, "int %d", expr->intVal);
        break;
    case FUNC_EXPR:
        fprintf(out, "func %s ", expr->func.name);
        print_params(out, expr->func.params);
        fputc(' ', out);
        print_expr(out, expr->func.body);
        fputs(" 'in ", out);
        print_expr(out, expr->func.subexpr);
        break;
    case BIND_EXPR:
        fprintf(out, "let %s ", expr->binding.name);
        print_expr(out, expr->binding.init);
        fputs(" 'in ", out);
        print_expr(out, expr->binding.subexpr);
        break;
    case IF_EXPR:
        fprintf(out, "if ");
        print_expr(out, expr->condition);
        fprintf(out, " then ");
        print_expr(out, expr->btrue);
        fprintf(out, " else ");
        print_expr(out, expr->bfalse);
    }
    fputc(')', out);
}


void print_declaration(FILE* out, const Declaration* decl)
{
    fputc('(', out);
    switch (decl->tag) {
    case DECL_FUNC:
        fprintf(out, "func %s ", decl->func.name);
        print_params(out, decl->func.params);
        print_expr(out, decl->func.body);
        break;
    case DECL_BIND:
        fprintf(out, "let %s ", decl->binding.name);
        print_expr(out, decl->binding.init);
        break;
    case DECL_TYPE:
        fprintf(out, "type %s ", decl->type.name);
        print_typexpr(out, decl->type.definition);
        break;
    }
    fputc(')', out);
}

void print_tree(FILE* out, const DeclarationList* root)
{
    fputc('(', out);
    const char* prefix = "";
    for (const DeclarationList* c = root; c; c = c->next) {
        fputs(prefix, out);
        print_declaration(out, c->declaration);
        prefix = "\n "; // prefix all but first with space
    }
    fputs(")", out);
}

