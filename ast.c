
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
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

Declaration* binding(Pattern* pattern, Expr* init)
{
    Declaration* result = xmalloc(sizeof *result);
    result->tag = DECL_BIND;
    result->binding.pattern = pattern;
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
    param->var_id = -1;
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
    result->type = NULL;    /* worked out by type check */
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
    result->var_id = -1;
    result->function_id = -1;
    return result;
}

Expr* unit_expr()
{
    return expr(UNITVAL);
}

Expr* intval(int value)
{
    Expr* result = expr(INTVAL);
    result->intval = value;
    return result;
}

Expr* strval(Symbol text)
{
    Expr* result = expr(STRVAL);
    result->strval = text;
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
    func->function_id = -1; /* assigned by codegen */
    func->var_id = -1;      /* assigned by codegen */
    return result;
}

Expr* local_binding(Pattern* pattern, Expr* init, Expr* subexpr)
{
    Expr* result = expr(BIND_EXPR);
    BindExpr* bind = &result->binding;
    bind->pattern = pattern;
    bind->init = init;
    bind->subexpr = subexpr;

    bind->var_id = -1;  /* assigned by codegen */
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

Expr* list(ExprList* exprs)
{
    Expr* result = expr(LIST);
    result->expr_list = exprs;
    return result;
}

Expr* vector(ExprList* exprs)
{
    Expr* result = expr(VECTOR);
    result->expr_list = exprs;
    return result;
}

Expr* tuple(ExprList* exprs)
{
    Expr* result = expr(TUPLE);
    result->expr_list = exprs;
    return result;
}

ExprList* exprlist()
{
    return NULL;
}

ExprList* add_expr(ExprList* list, Expr* to_add)
{
    ExprList* result = xmalloc(sizeof *result);
    result->expr = to_add;
    result->next = list;
    return result;
}

ExprList* reverse_list(ExprList* list)
{
    ExprList* tmp;
    ExprList* newhead = NULL;
    while (list) { // three pointer shuffle!
        tmp         = list->next;
        list->next  = newhead;
        newhead     = list;
        list        = tmp;
    }
    return newhead;
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

TypeExpr* typeconstraint(int constraint_id)
{
    struct TypeExpr* result = typexpr(TYPE_CONSTRAINT);
    result->constraint_id = constraint_id;
    return result;
}

TypeExpr* typetuple(TypeExpr* left, TypeExpr* right)
{
    struct TypeExpr* result = typexpr(TYPE_TUPLE);
    result->left = left;
    result->right = right;
    return result;
}

TypeExpr* typeconstructor(TypeExpr* param, Symbol constructor)
{
    struct TypeExpr* result = typexpr(TYPE_CONSTRUCTOR);
    result->param = param;
    result->constructor = constructor;
    return result;
}

static struct Pattern* pat(enum PatternTag tag)
{
    struct Pattern* result = xmalloc(sizeof *result);
    result->tag = tag;
    result->type = NULL; /* worked out later in type checker */
    return result;
}
Pattern* pat_var(Symbol name)
{
    struct Pattern* result = pat(PAT_VAR);
    result->name = name;
    return result;
}

Pattern* pat_discard()
{
    return pat(PAT_DISCARD);
}

Pattern* pat_list(Pattern* head, Pattern* tail)
{
    struct Pattern* result = pat(PAT_CONS);
    result->left = head;
    result->right = tail;
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
    case TYPE_CONSTRAINT:
        fprintf(out, "'%d", expr->constraint_id);
        break;
    case TYPE_TUPLE:
        print_typexpr(out, expr->left);
        fputs(" * ", out);
        print_typexpr(out, expr->right);
        break;
    case TYPE_CONSTRUCTOR:
        fputc('(', out);
        print_typexpr(out, expr->param);
        fprintf(out, " %s", expr->constructor);
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

void print_exprlist(FILE* out, const ExprList* list)
{
    fputc('(', out);
    const char* prefix = ""; // all but first get space prefix
    for (const ExprList* c = list; c; c = c->next) {
        fputs(prefix, out);
        print_expr(out, c->expr);
        prefix = " ";
    }
    fputc(')', out);
}

void print_pattern(FILE* out, const Pattern* pat)
{
    fputs("(pattern ", out);
    switch (pat->tag) {
        case PAT_VAR:
            fprintf(out, "%s", pat->name);
            break;
        case PAT_DISCARD:
            fputc('_', out);
            break;
        case PAT_CONS:
        {
            fputs("cons ", out);
            print_pattern(out, pat->left);
            fputc(' ', out);
            print_pattern(out, pat->right);
            break;
        }
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
    case UNITVAL:
        fprintf(out, "unit ()");
        break;
    case INTVAL:
        fprintf(out, "int %d", expr->intval);
        break;
    case STRVAL:
        fprintf(out, "string \"%s\"", expr->strval);
        break;
    case FUNC_EXPR:
        fprintf(out, "func %s ", expr->func.name);
        if (expr->func.functype) {
            fputs(": ", out);
            print_typexpr(out, expr->func.functype);
            fputs(" ", out);
        }
        print_params(out, expr->func.params);
        fputc(' ', out);
        print_expr(out, expr->func.body);
        fputs(" 'in ", out);
        print_expr(out, expr->func.subexpr);
        break;
    case BIND_EXPR:
        fprintf(out, "let ");
        print_pattern(out, expr->binding.pattern);
        fputc(' ', out);
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
        break;
    case LIST:
        fprintf(out, "list ");
        print_exprlist(out, expr->expr_list);
        break;
    case VECTOR:
        fprintf(out, "vector ");
        print_exprlist(out, expr->expr_list);
        break;
    case TUPLE:
        fprintf(out, "tuple ");
        print_exprlist(out, expr->expr_list);
        break;
    }
    if (expr->type) {
        fprintf(out, " : ");
        print_typexpr(out, expr->type);
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
        fputs("let ", out);
        print_pattern(out, decl->binding.pattern);
        fputc(' ', out);
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

int tp_count(const char* fmt)
{
    int nargs = 0;
    const char *cp = fmt;
    char c;
    while ((c = *cp++)) {
        if (c == '%') {
            c = *cp++;
            switch (c) {
                case 'd':
                case 's':
                case 'T':
                case 'P': nargs++;
                case '%': break;
                case '\0': --cp; break;
                default: assert(0 && "need to add another fmt specifier");
            }
        }
    }
    return nargs;
}

#ifdef _WIN32
#   define putc_unlocked(c, stream)     _putc_nolock(c, stream)
#   define flockfile(stream)            _lock_file(stream)
#   define funlockfile(stream)          _unlock_file(stream)
#endif

void tprintfx(FILE* out, const char* fmt, int nargs, ...)
{
    const char *cp = fmt;
    char c;

    va_list valist;
    va_start(valist, nargs);

    flockfile(out);
    while ((c = *cp++)) {
        if (c == '%') {
            c = *cp++;
            switch (c) {
                case 'd':
                {   // don't bother with negative
                    int num = va_arg(valist, int);
                    do {
                        putc_unlocked('0' + num%10, out);
                        num /= 10;
                    } while (num); // use do while to get printing for 0
                    break;
                }
                case 's':
                {
                    const char* cs = va_arg(valist, const char*);
                    while (*cs) { putc_unlocked(*cs++, out); }
                    break;
                }
                case 'T':
                {
                    funlockfile(out); // not sure if re-entrant
                    print_typexpr(out, va_arg(valist, TypeExpr*));
                    flockfile(out); // not sure if re-entrant
                    break;
                }
                case 'P':
                {
                    funlockfile(out); // not sure if re-entrant
                    print_pattern(out, va_arg(valist, Pattern*));
                    flockfile(out); // not sure if re-entrant
                    break;
                }
                case '%':
                    putc_unlocked(c, out);
                    break;
                case '\0': --cp; break;
                default: assert(0 && "need to add another fmt specifier");
            }
        } else {
            putc_unlocked(c, out);
        }
    }
    funlockfile(out);

    va_end(valist);
}

