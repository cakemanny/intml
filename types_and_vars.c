/*
 * Here we store variable declarations
 * and type check our abstract syntax tree
 */
#include "types_and_vars.h"
#include <assert.h>
#include <stdlib.h>
#include <stdarg.h>
#ifndef __pure
#   define __pure __attribute__((pure))
#endif

#define EPFX    "typecheck: error: "
#define LEPFX   "typecheck:%d: error: "
#define PFX     "typecheck: "

// for where our code should never reach if case statements are complete
#define FAIL_MISSED_CASE() do {                         \
    fprintf(stderr, "function %s, file %s, line %d\n",  \
            __FUNCTION__, __FILE__, __LINE__);          \
    abort();                                            \
} while(0)

/* See header file */
int debug_type_checker = 0;

/*
 * Keep a table of global type names
 */
#define TYPE_NAMES_MAX 1024
static Type* type_names[TYPE_NAMES_MAX];
static int type_name_count = 0;

/* A numbering to use for type contraints */
#define TYPE_CONTRAINTS_MAX 1024
static int next_constraint_id = 0;
static TypeExpr* constraint_theories[TYPE_CONTRAINTS_MAX];

/*
 * A stack of variable names that may help us type expressions
 */
#define VAR_MAX 1024
static struct Var {
    Symbol name;
    TypeExpr* type;
} variable_stack[VAR_MAX];

/* ptr to the end of the stack */
static struct Var* var_stack_ptr = variable_stack;


/* make life easier printing debug messaged */
#define dbgprint(M, ...) do { \
    if (debug_type_checker) { fprintf(stderr, PFX M, ##__VA_ARGS__); } \
} while(0)

#define dbgtprintf(M, ...) do { \
    if (debug_type_checker) { tprintf(stderr, PFX M, ##__VA_ARGS__); } \
} while(0)

static TypeExpr* lookup_ornull_typexpr(Symbol name)
{
    for (int i = 0; i < type_name_count; i++) {
        if (type_names[i]->name == name) {
            return type_names[i]->definition;
        }
    }
    return NULL;
}
static TypeExpr* lookup_typexpr(Symbol name)
{
    TypeExpr* res = lookup_ornull_typexpr(name);
    if (res)
        return res;
    fprintf(stderr, "%s\n", name);
    assert(0 && "type not in type_names table");
    FAIL_MISSED_CASE();
}

static TypeExpr* deref_typexpr(Symbol name)
{
    for (;;) {
        TypeExpr* dereffed = lookup_typexpr(name);
        if (dereffed->tag != TYPE_NAME || dereffed->name == name) {
            return dereffed;
        }
        name = dereffed->name;
    }
}

__pure static _Bool typexpr_equals(TypeExpr* left, TypeExpr* right)
{
    // deref and names as far as possible, then compare trees
    // normalize input cases
    if (left->tag > right->tag) {
        return typexpr_equals(right, left);
    }
    assert(left->tag <= right-> tag);

    switch (left->tag)
    {
      case TYPE_NAME:
      {
        switch (right->tag)
        {
          case TYPE_NAME:
          {
            // compare names, otherwise compare dereffed
            if (left->name == right->name)
                return 1;
            TypeExpr* dleft = deref_typexpr(left->name);
            TypeExpr* dright = deref_typexpr(right->name);
            if (dleft->name == left->name && dright->name == right->name) {
                return 0;
            }
            return typexpr_equals(dleft, dright);
          }
          case TYPE_ARROW:
          case TYPE_CONSTRAINT:
          case TYPE_TUPLE:
          case TYPE_CONSTRUCTOR:
          {
            TypeExpr* dleft = deref_typexpr(left->name);
            if (dleft->name == left->name)
                return 0;
            return typexpr_equals(dleft, right);
          }
        }
      }
      case TYPE_ARROW: // both type_arrows or right is constraint
      {
        if (right->tag != TYPE_ARROW) {
            return 0;
        }
        return typexpr_equals(left->left, right->left)
            && typexpr_equals(left->right, right->right);
      }
      case TYPE_CONSTRAINT:
      {
        if (right->tag != TYPE_CONSTRAINT) {
            return 0; // could conform but not equal
        }
        return left->constraint_id == right->constraint_id;
      }
      case TYPE_TUPLE:
      {
        if (right->tag != TYPE_TUPLE) {
            return 0;
        }
        for (TypeExprList* l = left->type_list, * r = right->type_list;
                l || r; l = l->next, r = r->next)
        {
            if (!(l && r)) {
                return 0; // not same length
            }
            if (!typexpr_equals(l->type, r->type)) {
                return 0;
            }
        }
        return 1;
      }
      case TYPE_CONSTRUCTOR:
      {
        // until we add some more type expressions
        assert(right->tag == TYPE_CONSTRUCTOR);
        return left->constructor == right->constructor
            && typexpr_equals(left->param, right->param);
      }
    }
    FAIL_MISSED_CASE();
}

static TypeExpr* deref_if_needed(TypeExpr* t)
{
    if (t->tag == TYPE_NAME)
        return deref_typexpr(t->name);
    return t;
}

__pure static _Bool typexpr_conforms_to(TypeExpr* left, TypeExpr* right)
{
    /*
     * probably instead of NULL we should be using a special
     * type tag... or assigning special names
     */
    if (left->tag == TYPE_CONSTRAINT || right->tag == TYPE_CONSTRAINT)
        return 1;
    if (typexpr_equals(left, right))
        return 1;
    // fully expand
    TypeExpr* dleft = deref_if_needed(left);
    TypeExpr* dright = deref_if_needed(right);
    // if either were name then they don't match
    if (dleft->tag == TYPE_NAME || dright->tag == TYPE_NAME) {
        return 0;
    }
    if (dleft->tag != dright->tag) {
        return 0;
    }
    if (dleft->tag == TYPE_CONSTRUCTOR) {
        return left->constructor == right->constructor
            && typexpr_conforms_to(left->param, right->param);
    }
    if (dleft->tag == TYPE_TUPLE) {
        for (TypeExprList* l = left->type_list, * r = right->type_list;
                l || r; l = l->next, r = r->next)
        {
            if (!(l && r)) {
                return 0; // not same length
            }
            if (!typexpr_conforms_to(l->type, r->type)) {
                return 0;
            }
        }
        return 1;
    }
    // Must be both both arrow
    return typexpr_conforms_to(left->left, right->left)
        && typexpr_conforms_to(left->right, right->right);
}

static TypeExpr* new_type_constraint()
{
    if (next_constraint_id >= TYPE_CONTRAINTS_MAX) {
        // TODO: attempt to reduce in here
        fprintf(stderr, EPFX"more than %d unconstrained type variables\n",
                TYPE_CONTRAINTS_MAX);
        exit(EXIT_FAILURE);
    }
    return typeconstraint(next_constraint_id++);
}

static void add_type_name(Type* node)
{
    if (type_name_count >= TYPE_NAMES_MAX) {
        fprintf(stderr, EPFX"more than %d typenames defined\n", TYPE_NAMES_MAX);
        exit(EXIT_FAILURE);
    }
    if (lookup_ornull_typexpr(node->name) != NULL) {
        fprintf(stderr, EPFX"type %s has already been defined\n",
                node->name);
        exit(EXIT_FAILURE);
    }
    dbgprint("adding named type: %s\n", node->name);
    type_names[type_name_count++] = node;
}

static void add_builtin_type(const char* name_of_type)
{
    Type* type_node = malloc(sizeof *type_node);
    type_node->name = symbol(name_of_type);
    type_node->definition = typename(type_node->name);
    add_type_name(type_node);
}


static void push_var(Symbol name, TypeExpr* type)
{
    if (var_stack_ptr >= variable_stack + VAR_MAX) {
        fprintf(stderr, EPFX"more than %d variable bindings in single scope\n",
                VAR_MAX);
        exit(EXIT_FAILURE);
    }
    if (debug_type_checker) {
        if (type) {
            tprintf(stderr, PFX"pushing var %s : %T\n", name, type);
        } else {
            fprintf(stderr, PFX"pushing var %s\n", name);
        }
    }
    *var_stack_ptr++ = (struct Var) { name, type };
}

/*
 * Lookup @name in the symbol table
 * Giving @lineno of the ast node
 */
static struct Var* lookup_var(Symbol name, int lineno)
{
    struct Var* end = var_stack_ptr;
    while (--end >= variable_stack) {
        if (end->name == name) {
            return end;
        }
    }
    fprintf(stderr, LEPFX"value %s not in scope\n", lineno, name);
    exit(EXIT_FAILURE);
}

static void print_type_error(TypeExpr* expected, TypeExpr* actual)
{
    tprintf(stderr, "  expected: %T\n  actual: %T\n", expected, actual);
}

#define foreach(list, varname, body) \
for (typeof(*list)* varname = list; varname; varname = varname->next) { \
    body \
}

__attribute__((warn_unused_result))
__pure static _Bool solid_type(TypeExpr* type)
{
    if (!type)
        return 0;
    switch (type->tag) {
        case TYPE_NAME:
            return 1;
        case TYPE_ARROW:
            return solid_type(type->left) && solid_type(type->right);
        case TYPE_CONSTRAINT:
            return 0;
        case TYPE_CONSTRUCTOR:
            return solid_type(type->param);
        case TYPE_TUPLE:
            foreach(type->type_list, l,
                if (!solid_type(l->type))
                    return 0;
            )
            return 1;
    }
    FAIL_MISSED_CASE();
}


static _Bool type_uses_constraint(TypeExpr* type, int constraint_id)
{
    if (!type)
        return 0;
    switch (type->tag)
    {
      // the dereffed might, but dereffing will get the new value
      case TYPE_NAME: return 0;
      case TYPE_ARROW: return type_uses_constraint(type->left, constraint_id)
                       || type_uses_constraint(type->right, constraint_id);
      case TYPE_CONSTRAINT: return type->constraint_id == constraint_id;
      case TYPE_CONSTRUCTOR: return type_uses_constraint(type->param, constraint_id);
      case TYPE_TUPLE:
      {
        foreach(type->type_list, l,
            if (type_uses_constraint(l->type, constraint_id))
                return 1;
        )
        return 0;
      }
    }
    FAIL_MISSED_CASE();
}

static TypeExpr* replaced_constraint(TypeExpr*, int, TypeExpr* );
static TypeExprList* replaced_constraint_list(
        TypeExprList* toreplace, int constraint_id, TypeExpr* theory)
{
    TypeExprList* tmp = NULL;
    for (TypeExprList* l = toreplace; l; l = l->next) {
        tmp = type_add(tmp, replaced_constraint(l->type, constraint_id, theory));
    }
    typeof(tmp) result = reversed_types(tmp);
    while (tmp) {
        typeof(tmp) tmp2 = tmp->next;
        free((void*)tmp); // cast away const
        tmp = tmp2;
    }
    return result;
}

__attribute__((warn_unused_result))
static TypeExpr* replaced_constraint(
        TypeExpr* toreplace, int constraint_id, TypeExpr* theory)
{
    switch (toreplace->tag)
    {
      case TYPE_NAME:
        return toreplace;
      case TYPE_ARROW:
        return typearrow(
                replaced_constraint(toreplace->left, constraint_id, theory),
                replaced_constraint(toreplace->right, constraint_id, theory));
      case TYPE_CONSTRAINT:
        return toreplace->constraint_id == constraint_id ?
            theory : toreplace;
      case TYPE_TUPLE:
        return typetuple(
                replaced_constraint_list(
                    toreplace->type_list, constraint_id, theory));
      case TYPE_CONSTRUCTOR:
        return typeconstructor(
            replaced_constraint(toreplace->param, constraint_id, theory),
            toreplace->constructor);
    }
    FAIL_MISSED_CASE();
}

static void replace_constraint_if_needed(
        TypeExpr** ptoreplace, int constraint_id, TypeExpr* theory)
{
    if (type_uses_constraint(*ptoreplace, constraint_id)) {
        *ptoreplace = replaced_constraint(*ptoreplace, constraint_id, theory);
    }
}

// pre-declare because recursive calls
static void apply_theory_expr(Expr* expr, int constraint_id, TypeExpr* theory);

/*
 * Replace the uses of type variables with given constraint_id with theory
 */
static void apply_theory_params(
        ParamList* params, int constraint_id, TypeExpr* theory)
{
    for (; params; params = params->next) {
        Param* p = params->param;
        replace_constraint_if_needed(&p->type, constraint_id, theory);
    }
}

static void apply_theory_pat(Pattern* pat, int constraint_id, TypeExpr* theory)
{
    replace_constraint_if_needed(&pat->type, constraint_id, theory);
    switch (pat->tag)
    {
      case PAT_VAR:
      case PAT_DISCARD:
        break;
      case PAT_CONS:
        apply_theory_pat(pat->left, constraint_id, theory);
        apply_theory_pat(pat->right, constraint_id, theory);
        break;
      case PAT_TUPLE:
        for (PatternList* l = pat->pat_list; l; l = l->next) {
            apply_theory_pat(l->pattern, constraint_id, theory);
        }
        break;
      case PAT_CTOR_NOARG:
        break;
      case PAT_CTOR_WARG:
        apply_theory_pat(pat->ctor_arg, constraint_id, theory);
        break;
      case PAT_INT:
      case PAT_STR:
      case PAT_NIL:
        break;
    }
}

static void apply_theory_cases(CaseList* cases, int constraint_id, TypeExpr* theory)
{
    for (; cases; cases = cases->next) {
        apply_theory_pat(cases->kase->pattern, constraint_id, theory);
        apply_theory_expr(cases->kase->expr, constraint_id, theory);
    }
}

/*
 * Replace the uses of type variables with given constraint_id with theory
 */
static void apply_theory_expr(Expr* expr, int constraint_id, TypeExpr* theory)
{
    replace_constraint_if_needed(&expr->type, constraint_id, theory);
    switch (expr->tag)
    {
      case PLUS:
      case MINUS:
      case MULTIPLY:
      case DIVIDE:
      case EQUAL:
      case LESSTHAN:
      case LESSEQUAL:
      case APPLY:
        apply_theory_expr(expr->left, constraint_id, theory);
        apply_theory_expr(expr->right, constraint_id, theory);
        break;
      case VAR:
      case UNITVAL:
      case INTVAL: // Nothing to do here
      case STRVAL:
        break;
      case RECFUNC_EXPR:
      case FUNC_EXPR:
        apply_theory_params(expr->func.params, constraint_id, theory);
        apply_theory_expr(expr->func.body, constraint_id, theory);
        apply_theory_expr(expr->func.subexpr, constraint_id, theory);

        replace_constraint_if_needed(&expr->func.functype, constraint_id, theory);
        replace_constraint_if_needed(&expr->func.resulttype, constraint_id, theory);
        break;
      case BIND_EXPR:
        apply_theory_pat(expr->binding.pattern, constraint_id, theory);
        apply_theory_expr(expr->binding.init, constraint_id, theory);
        apply_theory_expr(expr->binding.subexpr, constraint_id, theory);
        break;
      case IF_EXPR:
        apply_theory_expr(expr->condition, constraint_id, theory);
        apply_theory_expr(expr->btrue, constraint_id, theory);
        apply_theory_expr(expr->bfalse, constraint_id, theory);
        break;
      case LIST:
      case VECTOR:
      case TUPLE:
        for (ExprList* l = expr->expr_list; l; l = l->next) {
            apply_theory_expr(l->expr, constraint_id, theory);
        }
        break;
      case EXTERN_EXPR:
        assert(expr->tag != EXTERN_EXPR && "not expected");
        break;
      case MATCH_EXPR:
        apply_theory_expr(expr->matchexpr, constraint_id, theory);
        apply_theory_cases(expr->cases, constraint_id, theory);
        break;
      case DIRECT_CALL:
        assert(expr->tag != DIRECT_CALL && "not expected");
        break;
    }
}

/*
 * Replace the uses of type variables with given constraint_id with theory
 */
static void apply_theory(
        DeclarationList* root, int constraint_id, TypeExpr* theory)
{
    for (DeclarationList* c = root; c; c = c->next) {
        Declaration* decl = c->declaration;
        switch (decl->tag)
        {
          case DECL_EXTERN:
          case DECL_TYPE:
          case DECL_TYPECTOR:
            break;
          case DECL_BIND:
          {
            replace_constraint_if_needed(
                    &decl->binding.type, constraint_id, theory);

            apply_theory_pat(decl->binding.pattern, constraint_id, theory);
            apply_theory_expr(decl->binding.init, constraint_id, theory);
            break;
          }
          case DECL_RECFUNC:
          case DECL_FUNC:
          {
            replace_constraint_if_needed(&decl->func.type, constraint_id, theory);
            replace_constraint_if_needed(&decl->func.resulttype, constraint_id, theory);

            apply_theory_params(decl->func.params, constraint_id, theory);
            apply_theory_expr(decl->func.body, constraint_id, theory);
            break;
          }
        }
    }
}

static void check_type_names_are_declared(TypeExpr* typexpr)
{
    switch (typexpr->tag)
    {
        case TYPE_NAME:
        {
            if (!lookup_ornull_typexpr(typexpr->name)) {
                fprintf(stderr, "type name %s was not declared\n",
                        typexpr->name);
                exit(EXIT_FAILURE);
            }
            return;
        }
        case TYPE_ARROW:
        {
            check_type_names_are_declared(typexpr->left);
            check_type_names_are_declared(typexpr->right);
            return;
        }
        case TYPE_CONSTRAINT:
        {
            return;
        }
        case TYPE_TUPLE:
        {
            for (TypeExprList* l = typexpr->type_list; l; l = l->next) {
                check_type_names_are_declared(l->type);
            }
            return;
        }
        case TYPE_CONSTRUCTOR:
        {
            // TODO: need to check the constructor name has been declared
            check_type_names_are_declared(typexpr->param);
            return;
        }
    }
}

static int len_typelist(int acc, TypeExprList* list)
{
    if (!list) {
        return acc;
    }
    return len_typelist(1 + acc, list->next);
}
#define len_typelist(x) len_typelist(0, x)

/*
 * Attempt to add a theory that these types are equal, which we will later
 * try to unify.
 * @param etype
 *      Existing type - usually the type annotation that is already
 *      attached to the tree. The non-solid type.
 * @param newtype
 *      The more solid type we have worked out applies for the expression also
 */
static int theorise_equal(TypeExpr* etype, TypeExpr* newtype)
{
    switch (etype->tag) {
      case TYPE_CONSTRAINT:
      {
        const int eid = etype->constraint_id;
        // Check whether the theories on this constraint have been updated
        if (constraint_theories[eid]) {
            if (debug_type_checker
                    && (newtype->tag != TYPE_CONSTRAINT
                        || newtype->constraint_id != eid)) {
                dbgtprintf("constraint %d already has theory %T, ignore "
                        "theory %T\n", eid, constraint_theories[eid], newtype);
            }

            // ignore newtype
            return 0;
        } else if (newtype->tag != TYPE_CONSTRAINT) {
            dbgtprintf("'%d <- %T\n", eid, newtype);
            constraint_theories[eid] = newtype;
            return 1;
        } else {
            assert(newtype->tag == TYPE_CONSTRAINT);
            const int nid = newtype->constraint_id;
            if (nid == eid) {
                return 0;
            } else if (nid < eid) {
                constraint_theories[eid] = newtype;
                return 1;
            } else {
                assert(nid > eid);
                // repoint highest ID to lowest
                if (constraint_theories[nid]) {
                    // Check their theory isn't us
                    if (constraint_theories[nid] == etype) {
                        return 0;
                    }
                    // steal their theory since we don't have one
                    dbgtprintf("'%d <- %T\n", eid, constraint_theories[nid]);
                    constraint_theories[eid] = constraint_theories[nid];
                }
                // Make us their theory
                dbgtprintf("'%d <- %T\n", nid, etype);
                constraint_theories[nid] = etype;
                return 1;
            }
        }
      }
      case TYPE_ARROW:
      {
        if (newtype->tag != TYPE_ARROW) {
            return 0;
        }
        return theorise_equal(etype->left, newtype->left)
            + theorise_equal(etype->right, newtype->right);
      }
      case TYPE_NAME:
        return 0;
      case TYPE_CONSTRUCTOR:
        if (newtype->tag != TYPE_CONSTRUCTOR)
            return 0;
        if (etype->constructor == newtype->constructor) {
            return theorise_equal(etype->param, newtype->param);
        }
        return 0;
      case TYPE_TUPLE:
      {
        if (newtype->tag != TYPE_TUPLE) { return 0; }
        if (len_typelist(etype->type_list) != len_typelist(newtype->type_list)) {
            // Not sure whether it should make it this far
            fprintf(stderr,
                    "warn: theorising equal different sized tuple types");
            return 0;
        }
        int types_added = 0;
        for (TypeExprList* l = etype->type_list, * r = newtype->type_list;
                l; l = l->next, r = r->next)
        {
            types_added += theorise_equal(l->type, r->type);
        }
        return types_added;
      }
    }
    FAIL_MISSED_CASE();
}

__attribute__((const))
static const char* expr_name(enum ExprTag tag)
{
    assert(tag != 0);
    assert(tag <= MATCH_EXPR);
    const char* expr_name[] = {
        "", "plus", "minus", "multiply", "divide",
        "equal",
        "less than", "less than or equal",
        "apply", "var", "unit", "int", "string", "recfunc",
        "func", "let", "if", "list", "vector", "tuple", "match"
    };
    return expr_name[tag];
}

#define typexpr_equals_or_exit(LN,ET,AT,M,...) do {                     \
            int __line = LN;                                            \
            TypeExpr* __et = (ET);                                      \
            TypeExpr* __at = (AT);                                      \
            if (!typexpr_equals(__et, __at)) {                          \
                fprintf(stderr, LEPFX M "\n", __line,  ##__VA_ARGS__);  \
                print_type_error(__et, __at);                           \
                exit(EXIT_FAILURE);                                     \
            }                                                           \
        } while (0)

#define typexpr_conforms_or_exit(LN,ET,AT,M,...) do {                   \
            int __line = LN;                                            \
            TypeExpr* __et = (ET);                                      \
            TypeExpr* __at = (AT);                                      \
            if (!typexpr_conforms_to(__et, __at)) {                     \
                tprintf(stderr, LEPFX M "\n", __line, ##__VA_ARGS__);   \
                print_type_error(__et, __at);                           \
                exit(EXIT_FAILURE);                                     \
            }                                                           \
        } while (0)

static int count_names_pat(Pattern* pat);
static int count_names_pat_list(PatternList* l, int acc)
{
    if (!l) return acc;
    return count_names_pat_list(l->next, acc + count_names_pat(l->pattern));
}
static int count_names_pat(Pattern* pat)
{
    switch (pat->tag)
    {
      case PAT_VAR: return 1;
      case PAT_DISCARD: return 0;
      case PAT_CONS: return count_names_pat(pat->left)
                     + count_names_pat(pat->right);
      case PAT_TUPLE: return count_names_pat_list(pat->pat_list, 0);
      case PAT_CTOR_WARG: return count_names_pat(pat->ctor_arg);
      case PAT_CTOR_NOARG: /* fall through */
      case PAT_INT:
      case PAT_STR:
      case PAT_NIL: return 0;
    }
    FAIL_MISSED_CASE();
}
static void add_names_to_array_pat(Pattern* pat, Symbol** ptr_next)
{
    switch (pat->tag)
    {
      case PAT_VAR:
          *(*ptr_next)++ = pat->name;
          return;
      case PAT_DISCARD: return;
      case PAT_CONS:
        add_names_to_array_pat(pat->left, ptr_next);
        add_names_to_array_pat(pat->right, ptr_next);
        return;
      case PAT_TUPLE:
        for (PatternList* l = pat->pat_list; l; l = l->next) {
            add_names_to_array_pat(l->pattern, ptr_next);
        }
        return;
      case PAT_CTOR_WARG:
        add_names_to_array_pat(pat->ctor_arg, ptr_next);
        return;
      case PAT_CTOR_NOARG: return;
      case PAT_INT: return;
      case PAT_STR: return;
      case PAT_NIL: return;
    }
}

static int pat_list_len(PatternList* list)
{
    int n = 0;
    for (; list; list = list->next)
        n++;
    return n;
}


static void pattern_match_and_push_vars0(Pattern* pat, TypeExpr* init_type)
{
    switch (pat->tag)
    {
      case PAT_VAR:
      {
        if (pat->type) {
            typexpr_conforms_or_exit(pat->an_line, pat->type, init_type,
                    "name %s in let binding", pat->name);
            theorise_equal(pat->type, init_type);
            theorise_equal(init_type, pat->type);
        } else {
            pat->type = init_type; // types_added++?
        }
        push_var(pat->name, init_type);
        break;
      }
      case PAT_DISCARD:
      {
        if (pat->type) {
            typexpr_conforms_or_exit(pat->an_line, pat->type, init_type,
                    "_ in let binding");
            theorise_equal(pat->type, init_type);
            theorise_equal(init_type, pat->type);
        } else {
            pat->type = init_type; //types_added++?
        }
        break;
      }
      case PAT_CONS:
      {

        init_type = deref_if_needed(init_type);
        // Now we need to have that either init_type is a list type or a
        // constraint type we can suggest is an
        if (init_type->tag == TYPE_CONSTRAINT) {
            if (!pat->left->type) {
                pat->left->type = new_type_constraint();
            }
            if (!pat->type) {
                pat->type = typeconstructor(pat->left->type, symbol("list"));
            }
            theorise_equal(init_type, pat->type);
            if (!pat->right->type) {
                pat->right->type = pat->type;
            }
            pattern_match_and_push_vars0(pat->left, pat->left->type);
            pattern_match_and_push_vars0(pat->right, pat->right->type);

        } else if (init_type->tag == TYPE_CONSTRUCTOR
                && init_type->constructor == symbol("list")) {
            pattern_match_and_push_vars0(pat->left, init_type->param);
            pattern_match_and_push_vars0(pat->right, init_type);
        } else {
            tprintf(stderr,
                    LEPFX"expected list type in init for pattern %P\n",
                    pat->an_line, pat);
            TypeExpr* expected = typeconstructor(
                    typename(symbol("'a")), symbol("list"));
            print_type_error(expected, init_type);
            free((void*)expected->param); // cast away const :'(
            free((void*)expected);
            exit(EXIT_FAILURE);
        }

        if (pat->type) {
            typexpr_conforms_or_exit(pat->an_line, pat->type, init_type,
                    "pattern %P in let binding", pat);
            theorise_equal(pat->type, init_type);
            theorise_equal(init_type, pat->type);
        } else {
            pat->type = init_type;
        }
        break;
      }
      case PAT_TUPLE:
      {
        init_type = deref_if_needed(init_type);
        if (init_type->tag == TYPE_CONSTRAINT) {
            int ntypes = pat_list_len(pat->pat_list);
            TypeExpr** typstack = malloc(ntypes * sizeof *typstack);
            if (!typstack) { perror("out of memory"); abort(); }
            int typstack_idx = 0;

            for (PatternList* l = pat->pat_list; l; l = l->next) {
                if (!l->pattern->type) {
                    typstack[typstack_idx++] =
                        l->pattern->type = new_type_constraint();
                } else {
                    typstack[typstack_idx++] = l->pattern->type;
                }
            }
            if (!pat->type) {
                TypeExprList* pat_types = NULL;
                for (int i = ntypes - 1; i >= 0; --i) {
                    pat_types = type_add(pat_types, typstack[i]);
                }
                pat->type = typetuple(pat_types);
            }
            theorise_equal(init_type, pat->type);
            for (PatternList* l = pat->pat_list; l; l = l->next) {
                pattern_match_and_push_vars0(l->pattern, l->pattern->type);
            }
        } else if (init_type->tag == TYPE_TUPLE
                && len_typelist(init_type->type_list) == pat_list_len(pat->pat_list)) {
            TypeExprList* tl = init_type->type_list;
            for (PatternList* l = pat->pat_list; l; l = l->next, tl = tl->next) {
                pattern_match_and_push_vars0(l->pattern, tl->type);
            }
        } else {
            tprintf(stderr, LEPFX"expected %d-tuple type init for pattern %P\n",
                    pat->an_line, pat_list_len(pat->pat_list), pat);
            tprintf(stderr, "  actual: %T\n", init_type);
            exit(EXIT_FAILURE);
        }

        if (pat->type) {
            typexpr_conforms_or_exit(pat->an_line, pat->type, init_type,
                    "pattern %P in let binding", pat);
            theorise_equal(pat->type, init_type);
            theorise_equal(init_type, pat->type);
        } else {
            pat->type = init_type;
        }
        break;
      }
      case PAT_CTOR_NOARG:
      {
        // Get the type that the constructor constructs:
        struct Var* found_var = lookup_var(pat->ctor_name, pat->an_line);
        TypeExpr* found_type = found_var->type;
        assert(found_type);
        assert(solid_type(found_type));

        // Check init type matches the constructors type directly
        typexpr_conforms_or_exit(pat->an_line, init_type, found_type,
                "type of pattern '%s' does not match the type the value "
                "being matched against constructs ", pat->ctor_name);
        theorise_equal(init_type, found_type);

        if (pat->type) {
            typexpr_conforms_or_exit(pat->an_line, found_type, pat->type,
                    "type of pattern does not match the type which "
                    "constructor '%s' constructs ", pat->ctor_name);
            theorise_equal(pat->type, found_type);
        } else {
            pat->type = found_type;
        }
        break;
      }
      case PAT_CTOR_WARG:
      {
        // Do we need to check that the sub-pattern can match the type the
        // constuctor constructs?
        struct Var* found_var = lookup_var(pat->ctor_name, pat->an_line);
        TypeExpr* found_type = found_var->type;
        assert(found_type->tag == TYPE_ARROW); // The constructor must be a fn

        typexpr_conforms_or_exit(pat->an_line, init_type, found_type->right,
                "type of pattern '%s _' does not match the type the value "
                "being matched against constructs ", pat->ctor_name);
        theorise_equal(init_type, found_type->right);

        // recurse - the ctor_arg has ctor fn param type
        pattern_match_and_push_vars0(pat->ctor_arg, found_type->left);

        if (pat->type) {
            typexpr_conforms_or_exit(pat->an_line, found_type->right, pat->type,
                    "type of pattern does not match the type which "
                    "constructor '%s' constructs ", pat->ctor_name);
            theorise_equal(pat->type, found_type->right);
        } else {
            pat->type = found_type->right;
        }
        break;
      }
      case PAT_INT:
      {
        TypeExpr* int_type = lookup_typexpr(symbol("int"));

        typexpr_conforms_or_exit(pat->an_line, init_type, int_type,
                "int literal pattern %d", pat->intval);

        if (!pat->type) {
            pat->type = int_type;
        }
        assert(typexpr_equals(int_type, pat->type));
        break;
      }
      case PAT_STR:
      {
        TypeExpr* str_type = lookup_typexpr(symbol("string"));

        typexpr_conforms_or_exit(pat->an_line, init_type, str_type,
                "string literal pattern \"%s\"", pat->strval);

        if (!pat->type) {
            pat->type = str_type;
        }
        assert(typexpr_equals(str_type, pat->type));
        break;
      }
      case PAT_NIL:
      {
        // This needs to have the same type as the type of the match var.
        // Just need that to be any list type

        // 1. check init_type is a list type
        init_type = deref_if_needed(init_type);
        // Now we need to have that either init_type is a list type or a
        // constraint type we can suggest is an
        if (init_type->tag == TYPE_CONSTRAINT) {
            if (!pat->type) {
                pat->type = typeconstructor(
                        new_type_constraint(), symbol("list"));
            }
            theorise_equal(init_type, pat->type);

        } else if (init_type->tag == TYPE_CONSTRUCTOR
                && init_type->constructor == symbol("list")) {
            // This is what we hope for
        } else {
            tprintf(stderr, LEPFX"expected list type in init for pattern %P\n",
                    pat->an_line, pat);
            TypeExpr* expected = typeconstructor(
                    typename(symbol("'a")), symbol("list"));
            print_type_error(expected, init_type);
            free((void*)expected->param); // cast away const :'(
            free((void*)expected);
            exit(EXIT_FAILURE);
        }

        if (pat->type) {
            typexpr_conforms_or_exit(pat->an_line, pat->type, init_type,
                    "pattern %P in let binding", pat);
            theorise_equal(pat->type, init_type);
            theorise_equal(init_type, pat->type);
        } else {
            pat->type = init_type;
        }
        break;
      }
    }
}

static void pattern_match_and_push_vars(Pattern* pat, TypeExpr* init_type)
{
    // Check for duplicate names
    int num_names = count_names_pat(pat);
    Symbol* names = malloc(num_names * sizeof *names);
    if (!names) { perror("out of memory"); abort(); }
    Symbol* end = names;
    add_names_to_array_pat(pat, &end);
    for (Symbol* it = names; it < end - 1; ++it) {
        for (Symbol* it2 = it + 1; it2 < end; ++it2) {
            if (*it == *it2) {
                fprintf(stderr, LEPFX"variable %s bound twice in pattern\n",
                        pat->an_line, *it);
                exit(EXIT_FAILURE);
            }
        }
    }
    free(names);

    pattern_match_and_push_vars0(pat, init_type);
}

static int deducetype_expr(Expr* expr)
{
    TypeExpr* int_type = lookup_typexpr(symbol("int"));
    TypeExpr* unit_type = lookup_typexpr(symbol("unit"));
    TypeExpr* string_type = lookup_typexpr(symbol("string"));

    if (expr->type) {
        check_type_names_are_declared(expr->type);
    }

    dbgprint("deduce type of %s expression\n", expr_name(expr->tag));
    int lineno = expr->an_line;

    /*
     * Pattern to use:
     * 1. deduce types in subtree
     * if success:
     *      2. compare with annotation
     * else:
     *      2. try impose annotation on subtree
     * 3/0. add variables where needed
     */

    switch (expr->tag) {
      case PLUS:
      case MINUS:
      case MULTIPLY:
      case DIVIDE:
      case LESSTHAN: // THESE should become bool in the future
      case LESSEQUAL:
      {
        int types_added =
            deducetype_expr(expr->left) + deducetype_expr(expr->right);

        typexpr_conforms_or_exit(lineno, int_type, expr->left->type,
                "left side of %s expression", expr_name(expr->tag));
        types_added += theorise_equal(expr->left->type, int_type);

        typexpr_conforms_or_exit(lineno, int_type, expr->right->type,
                "right side of %s expression", expr_name(expr->tag));
        types_added += theorise_equal(expr->right->type, int_type);

        // type should be int
        if (expr->type) {
            typexpr_equals_or_exit(expr->an_line, int_type, expr->type,
                    "%s expression", expr_name(expr->tag));
        } else {
            expr->type = int_type;
            types_added++;
        }
        return types_added;
      }
      case EQUAL:
      {
        int types_added =
            deducetype_expr(expr->left) + deducetype_expr(expr->right);
        TypeExpr* left_type = expr->left->type;
        TypeExpr* right_type = expr->right->type;

        typexpr_conforms_or_exit(lineno, left_type, right_type, "right of "
                "equals expression does not match left side in type");
        types_added += theorise_equal(left_type, right_type);
        types_added += theorise_equal(right_type, left_type);

        if (expr->type) {
            typexpr_equals_or_exit(expr->an_line, int_type, expr->type,
                    "equals expression");
        } else {
            expr->type = int_type; // result of comparison is int
            types_added++;
        }
        return types_added;
      }
      case APPLY:
      {
        // Want left and right of (f x) such  that
        // f : 'a -> 'b , x : 'a
        int types_added =
            deducetype_expr(expr->left) + deducetype_expr(expr->right);
        TypeExpr* left_type = expr->left->type;
        TypeExpr* right_type = expr->right->type;

        assert(left_type->tag);
        assert(right_type->tag);
        {
            if (left_type->tag != TYPE_ARROW) {
                if (left_type->tag == TYPE_NAME) {
                    left_type = deref_typexpr(left_type->name);
                } else if (left_type->tag == TYPE_CONSTRAINT) {
                    TypeExpr* old_left_type = left_type;

                    TypeExpr* fresult_type = new_type_constraint();
                    expr->left->type =
                        left_type = typearrow(right_type, fresult_type);
                    types_added++;
                    dbgtprintf("assign type variable to result of function %T\n",
                            expr->left->type);
                    types_added += theorise_equal(old_left_type, left_type);
                }
                if (left_type->tag != TYPE_ARROW) {
                    tprintf(stderr, LEPFX"left side of apply expression "
                            "must be a function\n  expected: %T -> 'b\n "
                            "  actual: %T\n", expr->an_line, right_type,
                            left_type);
                    exit(EXIT_FAILURE);
                }
            }
            typexpr_conforms_or_exit(lineno, left_type->left, right_type,
                    "right side of apply expression");
            dbgtprintf("left type: %T, right type %T\n", left_type,
                    right_type);
            types_added += theorise_equal(left_type->left, right_type);
            types_added += theorise_equal(right_type, left_type->left);
        }

        if (expr->type) {
            typexpr_conforms_or_exit(lineno, left_type->right, expr->type,
                    "type of apply expression does not match result "
                    "type of function on left");
            types_added += theorise_equal(expr->type, left_type->right);
            // we can work out the type of the function
            TypeExpr* inferred_type = typearrow(right_type, expr->type);
            typexpr_conforms_or_exit(lineno, expr->left->type, inferred_type,
                    "type of (RHS -> apply expr) does not match "
                    "the function type");
            if (!solid_type(left_type)) {
                types_added += theorise_equal(left_type, inferred_type);
            } else {
                free((void*)inferred_type);
            }
        } else {
            // type of an apply expression is the result type of the
            // function being applied
            expr->type = left_type->right;
            types_added++;
        }
        return types_added;
      }
      case VAR:
      {
        struct Var* found_var = lookup_var(expr->var, expr->an_line);
        TypeExpr* found_type = found_var->type;
        if (expr->type) {
            if (found_type) {
                typexpr_conforms_or_exit(lineno, found_type, expr->type,
                        "var expression type did not match previously "
                        "known type for %s", expr->var);
                if (solid_type(expr->type) && solid_type(found_type)) {
                    dbgtprintf("both use and declaration of %s have type: %T\n",
                            expr->var, found_type);
                } else if (solid_type(expr->type)) {
                    dbgtprintf("use of %s has type %T, decl has type %T\n",
                            expr->var, expr->type, found_type);
                    return theorise_equal(found_type, expr->type);
                } else if (solid_type(found_type)) {
                    dbgtprintf("use of %s has type %T, decl has type %T\n",
                            expr->var, expr->type, found_type);
                    return theorise_equal(expr->type, found_type);
                } else {
                    dbgtprintf("declaration but not use of %s have type: %T\n",
                            expr->var, found_type);
                    return theorise_equal(found_type, expr->type) +
                        theorise_equal(expr->type, found_type);
                }
            } else {
                dbgtprintf("infering var %s type from expression: %T\n",
                        expr->var, expr->type);
                found_var->type = found_type = expr->type;
            }
            return 0;
        } else {
            if (found_type) {
                dbgprint("type of %s found in lookup\n", expr->var);
                expr->type = found_type;
                return 1;
            } else {
                dbgprint("type of %s not known yet\n", expr->var);
                expr->type = found_var->type = new_type_constraint();
                return 0;
            }
        }
      }
      case UNITVAL:
      {
        if (!expr->type) {
            expr->type = unit_type;
            return 1;
        }
        assert(typexpr_equals(unit_type, expr->type));
        return 0;
      }
      case INTVAL:
      {
        // don't bother checking for annotations
        if (!expr->type) {
            expr->type = int_type;
            return 1;
        }
        assert(typexpr_equals(int_type, expr->type));
        return 0;
      }
      case STRVAL:
      {
        if (!expr->type) {
            expr->type = string_type;
            return 1;
        }
        assert(typexpr_equals(string_type, expr->type));
        return 0;
      }
      case RECFUNC_EXPR:
      case FUNC_EXPR:
      {
        int types_added = 0;
        /*
         * Basic idea: push params onto var stack, type func body
         * restore stack, add the definition and type of the func
         * type the subexpr where the func is now defined
         */
        struct Var* pre_func_name_stack_ptr = var_stack_ptr;
        if (expr->tag == RECFUNC_EXPR) {
            /* make the name available before body */
            push_var(expr->func.name, expr->func.functype);
        }
        struct Var* pre_param_stack_ptr = var_stack_ptr;

        for (const ParamList* c = expr->func.params; c; c = c->next) {
            if (c->param->type) {
                check_type_names_are_declared(c->param->type);
            }
            if (!c->param->type && c->param->name == symbol("()")) {
                c->param->type = unit_type;
                types_added++;
            }
            push_var(c->param->name, c->param->type);
        }
        struct Var* post_param_stack_ptr = var_stack_ptr;


        /*
         * type the body of the function that is being defined
         */
        types_added += deducetype_expr(expr->func.body);
        TypeExpr* bodytype = expr->func.body->type;

        if (expr->func.resulttype) {
            check_type_names_are_declared(expr->func.resulttype);
            typexpr_conforms_or_exit(lineno, expr->func.resulttype, bodytype,
                    "body of function %s", expr->func.name);
            types_added += theorise_equal(bodytype, expr->func.resulttype);
        }

        /*
         * When restore the stack pointer we should ideally be checking
         * whether the parameters have since had their types deduced
         * and checking against the params we pushed
         */

        struct Var* begin = pre_param_stack_ptr;
        for (const ParamList* c = expr->func.params; c; c = c->next) {
            Param* param = c->param;
            struct Var* it = begin++;
            assert(param->name == it->name);
            if (it->type) {
                if (param->type) {
                    // compare
                    typexpr_conforms_or_exit(lineno, it->type, param->type,
                            "param %s of function %s",
                            it->name, expr->func.name);
                    types_added += theorise_equal(param->type, it->type);
                    types_added += theorise_equal(it->type, param->type);
                } else {
                    dbgtprintf("type of param %s deduced as %T\n",
                            it->name, it->type);
                    param->type = it->type;
                    ++types_added;
                }
            } else {
                if (param->type) {
                    // we don't ++types_added as it->type isn't part
                    // of the tree - it may be used to type it later
                    // though
                    it->type = param->type;
                } else {
                    param->type = it->type = new_type_constraint();
                    ++types_added;
                }
            }
        }

        // typeof(func) = typeof(params) -> typeof(func.body)
        _Bool have_type_of_params = 1;
        for (struct Var* it = pre_param_stack_ptr;
                it != post_param_stack_ptr; ++it) {
            if (!it->type) have_type_of_params = 0;
        }

        assert(have_type_of_params);
        assert(bodytype->tag);
        {
            TypeExpr* func_type = bodytype;
            for (struct Var* it = post_param_stack_ptr;
                    --it >= pre_param_stack_ptr; )
            {
                func_type = typearrow(it->type, func_type);
            }
            dbgtprintf("worked out func %s has type: %T\n", expr->func.name,
                    func_type);

            if (expr->func.functype) {
                typexpr_conforms_or_exit(lineno, func_type, expr->func.functype,
                        "type of function %s", expr->func.name);
                types_added += theorise_equal(expr->func.functype, func_type);
                types_added += theorise_equal(func_type, expr->func.functype);
            } else {
                expr->func.functype = func_type;
                types_added++;
            }
        }

        var_stack_ptr = pre_param_stack_ptr;
        if (expr->tag != RECFUNC_EXPR) {
            push_var(expr->func.name, expr->func.functype);
        }

        /*
         * type the subexpr of the func expr
         */
        types_added += deducetype_expr(expr->func.subexpr);
        TypeExpr* subexprtype = expr->func.subexpr->type;

        assert(subexprtype->tag);
        if (expr->type) {
            typexpr_conforms_or_exit(lineno,  subexprtype, expr->type,
                    "recorded type of expression following %s definition",
                    expr->func.name);
            types_added += theorise_equal(expr->type, subexprtype);
            types_added += theorise_equal(subexprtype, expr->type);
        } else {
            expr->type = subexprtype;
            types_added++;
        }

        var_stack_ptr = pre_func_name_stack_ptr;
        return types_added;
      }
      case BIND_EXPR:
      {
        /*
         * Main idea:
         * Deduce init type, push down name, deduce subexpr type
         */
        int types_added = deducetype_expr(expr->binding.init);
        TypeExpr* init_type = expr->binding.init->type;
        assert(init_type->tag);
        dbgtprintf("deduced type for pattern %P: %T\n", expr->binding.pattern,
                init_type);

        struct Var* saved_stack_ptr = var_stack_ptr;
        pattern_match_and_push_vars(expr->binding.pattern, init_type);

        types_added += deducetype_expr(expr->binding.subexpr);
        TypeExpr* subexprtype = expr->binding.subexpr->type;

        assert(subexprtype->tag);
        if (expr->type) {
            typexpr_conforms_or_exit(lineno, subexprtype, expr->type,
                    "conflicting types for expression following binding of %P",
                    expr->binding.pattern);
            types_added += theorise_equal(expr->type, subexprtype);
            types_added += theorise_equal(subexprtype, expr->type);
        } else {
            expr->type = subexprtype;
            types_added++;
        }

        var_stack_ptr = saved_stack_ptr;
        return types_added;
      }
      case IF_EXPR:
      {
        int types_added =
            deducetype_expr(expr->condition)
            + deducetype_expr(expr->btrue)
            + deducetype_expr(expr->bfalse);

        // start by enforce int_type on the condition
        assert(expr->condition->type->tag);
        {
            typexpr_conforms_or_exit(lineno, int_type, expr->condition->type,
                    "condition of if expression");
            types_added += theorise_equal(expr->condition->type, int_type);
        }

        TypeExpr* true_type = expr->btrue->type;
        TypeExpr* false_type = expr->bfalse->type;

        assert(true_type && false_type);
        {
            // compare!
            typexpr_conforms_or_exit(lineno, true_type, false_type, "true and "
                    "false branches of if expression with different types");
            types_added += theorise_equal(true_type, false_type);
            types_added += theorise_equal(false_type, true_type);
        }

        if (expr->type) {
            typexpr_conforms_or_exit(lineno, true_type, expr->type,
                    "if expression");
            types_added += theorise_equal(expr->type, true_type);
            types_added += theorise_equal(true_type, expr->type);
        } else {
            expr->type = true_type;
            types_added++;
        }
        return types_added;
      }
      case LIST:
      case VECTOR:
      {
        Symbol constrname = symbol(expr_name(expr->tag));
        int types_added = 0;
        for (ExprList* l = expr->expr_list; l; l = l->next) {
            types_added += deducetype_expr(l->expr);
        }

        if (expr->expr_list) { /* non-empty list */
            ExprList* head = expr->expr_list;
            int pos = 2;
            for (ExprList* tail = head->next; tail; tail = tail->next, pos++) {
                TypeExpr* hdtype = head->expr->type;
                TypeExpr* tltype = tail->expr->type;
                typexpr_conforms_or_exit(lineno, hdtype, tltype, "item %d in "
                        "%s, type does not 1st item type", pos, constrname);
                types_added += theorise_equal(hdtype, tltype);
                types_added += theorise_equal(tltype, hdtype);
            }
        }

        TypeExpr* param_type = (expr->expr_list)
            ? expr->expr_list->expr->type
            : new_type_constraint();

        if (expr->type) {
            if (expr->type->tag != TYPE_CONSTRUCTOR) {
                if (expr->type->tag == TYPE_NAME) {
                    // TODO: deal with these better...
                    expr->type = deref_typexpr(expr->type->name);
                } else if (expr->type->tag == TYPE_CONSTRAINT) {
                    dbgprint("list expression had constraint type");
                    TypeExpr* old_type = expr->type;
                    expr->type = typeconstructor(param_type, constrname);
                    types_added++;
                    types_added += theorise_equal(old_type, expr->type);
                }
            }
            if (expr->type->constructor != constrname) {
                fprintf(stderr, LEPFX"%s expression\n", expr->an_line,
                        expr_name(expr->tag));
                tprintf(stderr, "  expected: 'a %s\n  actual: %T\n",
                        constrname, expr->type);
                exit(EXIT_FAILURE);
            }
        } else {
            expr->type = typeconstructor(param_type, constrname);
            types_added++;
        }
        return types_added;
      }
      case TUPLE:
      {
        int types_added = 0;
        for (ExprList* l = expr->expr_list; l; l = l->next) {
            types_added += deducetype_expr(l->expr);
        }

        TypeExprList* new_type_list =
            type_list(expr->expr_list->expr->type);
        for (ExprList* l = expr->expr_list->next; l; l = l->next) {
            new_type_list = type_add(new_type_list, l->expr->type);
        }
        TypeExpr* expected_type = typetuple(reversed_types(new_type_list));

        if (expr->type) {
            if (expr->type->tag != TYPE_TUPLE) {
                if (expr->type->tag == TYPE_NAME) {
                    expr->type = deref_typexpr(expr->type->name);
                } else if (expr->type->tag == TYPE_CONSTRAINT) {
                    dbgprint("tuple expression had constraint type");
                    TypeExpr* old_type = expr->type;
                    expr->type = expected_type;
                    types_added += 1 + theorise_equal(old_type, expr->type);
                }
            }

            typexpr_conforms_or_exit(lineno, expected_type, expr->type,
                    "tuple expression");
            types_added += theorise_equal(expr->type, expected_type);
            types_added += theorise_equal(expected_type, expr->type);
        } else {
            expr->type = expected_type;
            types_added++;
        }
        return types_added;
      }
      case EXTERN_EXPR:
        assert(expr->tag != EXTERN_EXPR && "not expected");
        break;
      case MATCH_EXPR:
      {
        // The match expression needs to match the cases on the left
        // The productions need to all be the same type - like a list
        int types_added = deducetype_expr(expr->matchexpr);
        for (CaseList* k = expr->cases; k; k = k->next) {
            pattern_match_and_push_vars(k->kase->pattern, expr->matchexpr->type);
            struct Var* saved_stack_ptr = var_stack_ptr;
            types_added += deducetype_expr(k->kase->expr);
            var_stack_ptr = saved_stack_ptr;
        }

        CaseList* head = expr->cases;
        TypeExpr* head_type = head->kase->expr->type;
        int pos = 2;
        for (CaseList* tail = head->next; tail; tail = tail->next, pos++) {
            TypeExpr* tail_type = tail->kase->expr->type;
            typexpr_conforms_or_exit(lineno, head_type, tail_type, "result "
                    "expression of case %d of match expression has type "
                    "not matching the first case", pos);
            types_added += theorise_equal(head_type, tail_type);
            types_added += theorise_equal(tail_type, head_type);
        }

        if (expr->type) {
            typexpr_conforms_or_exit(lineno, head_type, expr->type, "match expression");
            types_added += theorise_equal(expr->type, head_type);
            types_added += theorise_equal(head_type, expr->type);
        } else {
            expr->type = head_type;
            types_added++;
        }
        return types_added;
      }
      case DIRECT_CALL:
      {
        // codegen only
        assert(expr->tag != DIRECT_CALL);
        break;
      }
    }
    FAIL_MISSED_CASE();
}

__attribute__((warn_unused_result))
__pure static _Bool declares_name_pat(Pattern* pat, Symbol name)
{
    switch (pat->tag) {
      case PAT_VAR: return pat->name == name;
      case PAT_DISCARD: return 0;
      case PAT_CONS: return declares_name_pat(pat->left, name)
                     || declares_name_pat(pat->right, name);
      case PAT_TUPLE:
        for (PatternList* l = pat->pat_list; l; l = l->next) {
            if (declares_name_pat(l->pattern, name)) {
                return 1;
            }
        }
        return 0;
      case PAT_CTOR_WARG: return declares_name_pat(pat->ctor_arg, name);
      case PAT_CTOR_NOARG: /* fal through */
      case PAT_INT:
      case PAT_STR:
      case PAT_NIL: return 0;
    }
    FAIL_MISSED_CASE();
}

/*
 * A helper function for accessing the name of any of the declaration types
 */
__attribute__((warn_unused_result))
__pure static _Bool declares_name(Declaration* declaration, Symbol name)
{
    switch (declaration->tag) {
      case DECL_RECFUNC:
      case DECL_FUNC: return declaration->func.name == name;
      case DECL_BIND: return declares_name_pat(
                              declaration->binding.pattern, name);
      case DECL_TYPE: return declaration->type.name == name;
      case DECL_EXTERN: return declaration->ext.name == name;
      case DECL_TYPECTOR: return declaration->ctor.name == name;
    }
    FAIL_MISSED_CASE();
}
__attribute__((warn_unused_result))
__pure static TypeExpr** decl_type_ptr_pat(Pattern* pat, Symbol name)
{
    switch (pat->tag) {
      case PAT_VAR:
        if (name == pat->name) return &pat->type;
        else return NULL;
      case PAT_DISCARD:
        return NULL;
      case PAT_CONS:
      {
        TypeExpr** left_type = decl_type_ptr_pat(pat->left, name);
        if (left_type) return left_type;
        else return decl_type_ptr_pat(pat->right, name);
      }
      case PAT_TUPLE:
      {
        for (PatternList* pl = pat->pat_list; pl; pl = pl->next) {
            TypeExpr** ptype = decl_type_ptr_pat(pl->pattern, name);
            if (ptype)
                return ptype;
        }
        return NULL;
      }
      case PAT_CTOR_WARG: return decl_type_ptr_pat(pat->ctor_arg, name);
      case PAT_CTOR_NOARG: /* fal through */
      case PAT_INT:
      case PAT_STR:
      case PAT_NIL: return 0;
    }
    FAIL_MISSED_CASE();
}
__attribute__((warn_unused_result))
__pure static TypeExpr** decl_type_ptr(Declaration* declaration, Symbol name)
{
    switch (declaration->tag) {
        case DECL_RECFUNC:
        case DECL_FUNC: return &declaration->func.type;
        case DECL_BIND: return decl_type_ptr_pat(
                                declaration->binding.pattern, name);
                        // chances are we don't want this case:
        case DECL_TYPE: return &declaration->type.definition;
        case DECL_EXTERN: return &declaration->ext.type;
        case DECL_TYPECTOR: return NULL; // Shouldn't be used
    }
    FAIL_MISSED_CASE();
}
/*
 * A helper function for accessing the type of any declaration types
 * Note, the type of a type declaration doesn't really fit the semantics of
 * the other types of declarations
 */
__attribute__((warn_unused_result))
__pure static TypeExpr* decl_type(Declaration* declaration, Symbol name)
{
    return *decl_type_ptr(declaration, name);
}

static void type_and_check_exhaustively(DeclarationList* root)
{
    int saved_type_name_count = type_name_count;

    int types_added;
    do {
        types_added = 0;
        type_name_count = saved_type_name_count;
        struct Var* saved_stack_ptr = var_stack_ptr;

        for (DeclarationList* c = root; c; c = c->next) {
            Declaration* decl = c->declaration;
            int lineno = decl->an_line;
            switch (decl->tag)
            {
              case DECL_TYPE:
              {
                check_type_names_are_declared(decl->type.definition);
                add_type_name(&decl->type);
                break;
              }
              case DECL_BIND:
              {
                Binding binding = decl->binding;
                dbgtprintf("deducing type for toplevel binding %P\n",
                        binding.pattern);
                types_added += deducetype_expr(binding.init);
                TypeExpr* init_type = binding.init->type;

                assert(init_type->tag);
                dbgtprintf("deduced type %P: %T\n", binding.pattern, init_type);
                if (binding.type) {
                    typexpr_conforms_or_exit(lineno, binding.type, init_type,
                            "type annotation does not matched "
                            "deduced type for toplevel binding %P",
                            binding.pattern);
                    types_added += theorise_equal(binding.type, init_type);
                    types_added += theorise_equal(init_type, binding.type);
                } else {
                    decl->binding.type = binding.type = init_type;
                    ++types_added;
                }
                pattern_match_and_push_vars(binding.pattern, binding.type);
                break;
              }
              case DECL_RECFUNC: /* TODO: make this diff for rec funcs */
              case DECL_FUNC:
              {
                /* !!! A lot of this is a direct copy of work above
                 * TODO: refactor DECL_FUNC and FUNC_EXPR to share common
                 * logic
                 */
                /*
                 * Basic idea: push params onto var stack, type func body
                 * restore stack, add the definition and type of the func
                 */
                if (decl->tag == DECL_RECFUNC) {
                    /* make the name available before body */
                    push_var(decl->func.name, decl->func.type);
                }

                struct Var* pre_param_stack_ptr = var_stack_ptr;

                for (const ParamList* c = decl->func.params; c; c = c->next) {
                    if (!c->param->type && c->param->name == symbol("()")) {
                        c->param->type = lookup_typexpr(symbol("unit"));
                        types_added++;
                    }
                    push_var(c->param->name, c->param->type);
                }
                struct Var* post_param_stack_ptr = var_stack_ptr;

                types_added += deducetype_expr(decl->func.body);
                TypeExpr* bodytype = decl->func.body->type;

                if (decl->func.resulttype) {
                    typexpr_conforms_or_exit(lineno, decl->func.resulttype,
                            bodytype, "body of toplevel function %s",
                            decl->func.name);
                    types_added +=
                        theorise_equal(bodytype, decl->func.resulttype);
                }

                struct Var* begin = pre_param_stack_ptr;
                for (const ParamList* c = decl->func.params; c; c = c->next) {
                    Param* param = c->param;
                    struct Var* it = begin++;
                    assert(param->name == it->name);
                    if (it->type) {
                        if (param->type) {
                            typexpr_conforms_or_exit(lineno, it->type,
                                    param->type, "param %s of toplevel "
                                    "function %s", it->name, decl->func.name);
                            types_added +=
                                theorise_equal(param->type, it->type);
                        } else {
                            dbgtprintf("type of param %s deduced as %T\n",
                                    it->name, it->type);
                            param->type = it->type;
                            ++types_added;
                        }
                    } else {
                        if (param->type) {
                            it->type = param->type;
                        } else {
                            dbgprint("adding new type constraint for param %s\n",
                                    param->name);
                            param->type = it->type = new_type_constraint();
                            types_added++;
                        }
                    }
                }

                _Bool have_type_of_params = 1;
                for (struct Var* it = pre_param_stack_ptr;
                        it != post_param_stack_ptr; ++it)
                {
                    if (!it->type) have_type_of_params = 0;
                }

                assert(have_type_of_params);
                assert(bodytype->tag);
                {
                    TypeExpr* func_type = bodytype;
                    for (struct Var* it = post_param_stack_ptr;
                            --it >= pre_param_stack_ptr; )
                    {
                        func_type = typearrow(it->type, func_type);
                    }
                    dbgtprintf("worked out toplevel function %s has type: %T\n",
                            decl->func.name, func_type);

                    if (decl->func.type) {
                        typexpr_conforms_or_exit(lineno, func_type,
                                decl->func.type, "type of toplevel function %s",
                                decl->func.name);
                        if (!solid_type(decl->func.type)) {
                            dbgtprintf("decl->func.type: %T\n", decl->func.type);
                            dbgtprintf("functype: %T\n", func_type);
                            types_added += theorise_equal(
                                    decl->func.type, func_type);
                        } else {
                            for (struct Var* it = post_param_stack_ptr;
                                    --it >= pre_param_stack_ptr; )
                            {
                                TypeExpr* tmp = func_type;
                                func_type = func_type->right;
                                free((void*)tmp); // cast away const
                            }
                        }
                    } else {
                        decl->func.type = func_type;
                        types_added++;
                    }
                }

                // restore stack and push func decl
                var_stack_ptr = pre_param_stack_ptr;
                if (decl->tag != DECL_RECFUNC) {
                    push_var(decl->func.name, decl->func.type);
                }
                break;
              }
              case DECL_EXTERN:
              {
                if (!solid_type(decl->ext.type)) {
                    fprintf(stderr, LEPFX"external declaration does not have "
                            "complete type signature: %s\n", decl->an_line,
                            decl->ext.name);
                    exit(EXIT_FAILURE);
                }
                push_var(decl->ext.name, decl->ext.type);
                break;
              }
              case DECL_TYPECTOR:
              {
                // For tagged types, we might need to revisit the type
                // definition system

                // 0. Check that there are no duplicate constructor names
                for (CtorList* l = decl->ctor.ctors; l; l = l->next) {
                    for (CtorList* m = decl->ctor.ctors; m; m = m->next) {
                        if (l < m && l->ctor->name == m->ctor->name) {
                            fprintf(stderr, LEPFX"There are two constructors "
                                    "with the same name: %s\n", decl->an_line,
                                    l->ctor->name);
                            exit(EXIT_FAILURE);
                        }
                    }
                }

                // 1. Add the type name to a list of named types
                add_builtin_type(decl->ctor.name); // FIXME
                // 2. Add the constructors as vars
                for (CtorList* l = decl->ctor.ctors; l; l = l->next) {
                    Ctor* ctor = l->ctor;
                    switch (ctor->tag) {
                      case CTOR_NOARG:
                        push_var(ctor->name, lookup_typexpr(decl->ctor.name));
                        break;
                      case CTOR_WARG:
                      {
                        check_type_names_are_declared(ctor->typexpr);
                        TypeExpr* param_type = ctor->typexpr;
                        TypeExpr* result_type = lookup_typexpr(decl->ctor.name);
                        push_var(ctor->name, typearrow(param_type, result_type));
                        break;
                      }
                    }
                }

                //assert(0 && "TODO: implement tagged types");
                break;
              }
            }
        }

        // Again, it would be sensible to check see if any of the
        // types have been derived by their use
        if (debug_type_checker) {
            for (DeclarationList* c = root; c; c = c->next) {
                for (struct Var* it = saved_stack_ptr; it != var_stack_ptr; ++it) {
                    Declaration* decl = c->declaration;
                    if (declares_name(decl, it->name)) {
                        fprintf(stderr, "** %s\n", it->name);
                        fprintf(stderr, "  stacktype: ");
                        if (it->type) print_typexpr(stderr, it->type);
                        else fputs("(null)", stderr);
                        fprintf(stderr, "\n  tree type: ");
                        if (decl_type(decl, it->name))
                            print_typexpr(stderr, decl_type(decl, it->name));
                        else fputs("(null)", stderr);
                        fputs("\n", stderr);

                        if (it->type && !decl_type(decl, it->name)) {
                            assert(decl->tag != DECL_TYPE);
                            *decl_type_ptr(decl, it->name) = it->type;
                        }
                    }
                }
            }
        }

        /*
         * since we write all our transitive theories such that
         * 'i ~ 'j => i > j, must start from highest theory
         */
        for (int i = next_constraint_id - 1; i >= 0; --i) {
            if (constraint_theories[i]) {
                dbgtprintf("constraint %d has theory %T\n", i,
                        constraint_theories[i]);
                // Want to apply said theory to the tree and
                // then type check it
                apply_theory(root, i, constraint_theories[i]);
                // also need to apply the theory to the other theories that may
                // reference us in the branches of a type
                for (int j = 0; j < i; j++) {
                    TypeExpr** theory_j = constraint_theories + j;
                    replace_constraint_if_needed(
                            theory_j, i, constraint_theories[i]);
                }
                ++types_added; // avoid loop end

                /*
                 * In the future, for the purpose of error messages, we
                 * ought to state the current theory we are testing and show
                 * where it came from...
                 */

                if (i == next_constraint_id - 1) {
                    // Don't want "freed" constraints to be seen by new
                    // theories
                    constraint_theories[i] = NULL;
                    --next_constraint_id;
                } else {
                    // Perform compaction?
                    constraint_theories[i] = NULL;
                }
                break;
            } else {
                dbgprint("constraint %d has no theories yet\n", i);
            }
        }

        dbgprint("restoring stack pointer\n");
        var_stack_ptr = saved_stack_ptr;
    } while (types_added > 0);

}

static void print_binding_hierachy(FILE* out, SymList* hierachy)
{
    for (SymList* c = hierachy; c; c = c->next) {
        fprintf(out, "  in let binding %s\n", c->name);
    }
}

static _Bool check_if_fully_typed_pattern(Pattern* pat, SymList* hierachy)
{
    _Bool result = 1;
    if (!solid_type(pat->type)) {
        fprintf(stderr, LEPFX"unable to determine type of binding %s\n",
                pat->an_line, pat->name);
        print_binding_hierachy(stderr, hierachy);
        result = 0;
    }
    switch (pat->tag) {
      case PAT_VAR:
      case PAT_DISCARD:
        break;
      case PAT_CONS:
        result &= check_if_fully_typed_pattern(pat->left, hierachy);
        result &= check_if_fully_typed_pattern(pat->right, hierachy);
        break;
      case PAT_TUPLE:
        for (PatternList* l = pat->pat_list; l; l = l->next) {
            result &= check_if_fully_typed_pattern(l->pattern, hierachy);
        }
        break;
      case PAT_CTOR_WARG:
        result &= check_if_fully_typed_pattern(pat->ctor_arg, hierachy);
        break;
      case PAT_CTOR_NOARG:
      case PAT_INT:
      case PAT_STR:
      case PAT_NIL:
        break;
    }
    return result;
}

static _Bool check_if_fully_typed_params(ParamList* params, SymList* hierachy)
{
    _Bool result = 1;
    for (ParamList* c = params; c; c = c->next) {
        if (!solid_type(c->param->type)) {
            fprintf(stderr, LEPFX"unable to determine type of parameter %s\n",
                    c->param->an_line, c->param->name);
            print_binding_hierachy(stderr, hierachy);
            result = 0;
        }
    }
    return result;
}
static _Bool check_if_fully_typed_expr(Expr* expr, SymList* hierachy)
{
    _Bool result = 1;
    switch (expr->tag) {
      case PLUS:
      case MINUS:
      case MULTIPLY:
      case DIVIDE:
      case EQUAL:
      case LESSTHAN:
      case LESSEQUAL:
      case APPLY:
        if (!solid_type(expr->type)) {
            fprintf(stderr, LEPFX"%s expr with no type\n", expr->an_line,
                    expr_name(expr->tag));
            print_binding_hierachy(stderr, hierachy);
            result = 0;
        }
        result &= check_if_fully_typed_expr(expr->left, hierachy);
        result &= check_if_fully_typed_expr(expr->right, hierachy);
        break;
      case VAR:
        if (!solid_type(expr->type)) {
            fprintf(stderr, LEPFX"var %s with no type\n", expr->an_line,
                    expr->var);
            print_binding_hierachy(stderr, hierachy);
            result = 0;
        }
        break;
      case UNITVAL:
      case INTVAL:
      case STRVAL:
        assert(expr->type != NULL); // should damn well be typed
        break;
      case RECFUNC_EXPR:
      case FUNC_EXPR:
      {
        if (!solid_type(expr->func.functype)) {
            fprintf(stderr, LEPFX"func %s with no type\n", expr->an_line,
                    expr->func.name);
            print_binding_hierachy(stderr, hierachy);
            result = 0;
        }

        SymList* subhierachy = symbol_list_add(hierachy, expr->func.name);
        result &= check_if_fully_typed_params(expr->func.params, subhierachy);
        result &= check_if_fully_typed_expr(expr->func.body, subhierachy);
        result &= check_if_fully_typed_expr(expr->func.subexpr, subhierachy);
        free(subhierachy);
        if (!solid_type(expr->type)) {
            assert(result == 0); // better be covered by subexpr
        }
        break;
      }
      case BIND_EXPR:
      {
        // TODO: potentially add all the binding names to the hierachy?
        // Do we want to check each binding name is typed?
        result &= check_if_fully_typed_expr(expr->binding.init, hierachy);
        result &= check_if_fully_typed_expr(expr->binding.subexpr, hierachy);
        if (!solid_type(expr->type)) {
            assert(result == 0); // better be covered by subexpr
        }
        break;
      }
      case IF_EXPR:
      {
        if (!solid_type(expr->type)) {
            fprintf(stderr, LEPFX"if expression with no type\n", expr->an_line);
            print_binding_hierachy(stderr, hierachy);
            result = 0;
        }
        result &= check_if_fully_typed_expr(expr->condition, hierachy);
        result &= check_if_fully_typed_expr(expr->btrue, hierachy);
        result &= check_if_fully_typed_expr(expr->bfalse, hierachy);
        break;
      }
      case LIST:
      case VECTOR:
      case TUPLE:
      {
        if (!solid_type(expr->type)) {
            fprintf(stderr, LEPFX"%s with no type\n", expr->an_line,
                    expr_name(expr->tag));
            print_binding_hierachy(stderr, hierachy);
            result = 0;
        }
        for (ExprList* l = expr->expr_list; l; l = l->next) {
            result &= check_if_fully_typed_expr(l->expr, hierachy);
        }
        break;
      }
      case EXTERN_EXPR:
        assert(expr->tag != EXTERN_EXPR && "not expected");
        break;
      case MATCH_EXPR:
      {
        if (!solid_type(expr->type)) {
          fprintf(stderr, LEPFX"match expression with no type\n", expr->an_line);
          print_binding_hierachy(stderr, hierachy);
          result = 0;
        }
        result &= check_if_fully_typed_expr(expr->matchexpr, hierachy);
        for (CaseList* l = expr->cases; l; l = l->next) {
            result &= check_if_fully_typed_expr(l->kase->expr, hierachy);
        }
        break;
      }
      case DIRECT_CALL:
      {
        assert(expr->tag != DIRECT_CALL);
        break;
      }
    }
    return result;
}

static _Bool check_if_fully_typed_root(DeclarationList* root)
{
    _Bool result = 1;
    for (DeclarationList* c = root; c; c = c->next) {
        Declaration* decl = c->declaration;
        switch (decl->tag) {
          case DECL_TYPE:
            // Only incorrect if names were not found.
            // This is enforced by the type checker
            break;
          case DECL_RECFUNC:
          case DECL_FUNC:
          {
            SymList* hierachy = symbol_list(decl->func.name);
            if (!solid_type(decl->func.type)) {
                fprintf(stderr, LEPFX"unable to deduce type of function %s\n",
                        decl->an_line, decl->func.name);
                result = 0;
            }
            result &= check_if_fully_typed_params(decl->func.params, hierachy);
            result &= check_if_fully_typed_expr(decl->func.body, hierachy);
            free(hierachy);
            break;
          }
          case DECL_BIND:
          {
            if (!solid_type(decl->binding.type)) {
                tprintf(stderr, LEPFX"unable to deduce type of let binding %P\n",
                        decl->an_line, decl->binding.pattern);
                result = 0;
            }
            if (decl->binding.pattern->tag == PAT_VAR) {
                SymList* hierachy = symbol_list(decl->binding.pattern->name);
                result &= check_if_fully_typed_pattern(decl->binding.pattern, hierachy);
                result &= check_if_fully_typed_expr(decl->binding.init, hierachy);
                free(hierachy);
            } else {
                // TODO: Need another way of recording the hierachy
                result &= check_if_fully_typed_pattern(decl->binding.pattern, NULL);
                result &= check_if_fully_typed_expr(decl->binding.init, NULL);
            }
            break;
          }
          case DECL_EXTERN:
          {
            if (!solid_type(decl->ext.type)) {
                fprintf(stderr, LEPFX"external declaration without complete "
                        "type: %s\n", decl->an_line, decl->ext.name);
                result = 0;
            }
            break;
          }
          case DECL_TYPECTOR:
          {
              // Not sure we have to do anything with this one
              break;
          }
        }
    }
    return result;
}

void type_check_tree(DeclarationList* root)
{
    /*
     * 1. Add built-in types to the type table
     * 2. find and add globally declared type names
     * 3. Attempt to type the tree until we cannot add any more types
     *    / report errors for inconsistencies
     */

    add_builtin_type("int");
    add_builtin_type("unit");
    add_builtin_type("string");

    type_and_check_exhaustively(root);

    // Check where we were unable to deduce types
    if (!check_if_fully_typed_root(root)) {
        exit(EXIT_FAILURE);
    }

    // Wonder whether replacing functions with let bindings would
    // simplifiy analysis and code generation at this point...
}

void check_runtime_properties(DeclarationList* root)
{
    /*
     * Need a let main : unit -> int = ...
     */
    static TypeExpr* main_type;
    if (!main_type) main_type =
        typearrow(
            typename(symbol("unit")),
            typename(symbol("int")));

    for (DeclarationList* c = root; c; c = c->next)
    {
        Declaration* decl = c->declaration;
        if (declares_name(decl, symbol("main"))) {
            if (typexpr_equals(decl_type(decl, symbol("main")), main_type)) {
                switch (decl->tag) {
                  case DECL_FUNC: return;
                  case DECL_BIND: return;
                  case DECL_TYPE: break; // don't care about type
                  case DECL_EXTERN: return; // Should we allow external main
                  case DECL_RECFUNC: return; // not sure if this should be allowed
                  case DECL_TYPECTOR: break; // don't care about typectors
                }
            } else if (decl->tag != DECL_TYPE) {
                fprintf(stderr, LEPFX"main has incorrect type\n", decl->an_line);
                print_type_error(main_type, decl_type(decl, symbol("main")));
            }
        }
    }
    fprintf(stderr, EPFX"no main function found\n  val main : unit -> int\n");
    exit(EXIT_FAILURE);
}
