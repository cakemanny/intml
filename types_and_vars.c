/*
 * Here we store variable declarations
 * and type check our abstract syntax tree
 */
#include "types_and_vars.h"
#include <assert.h>
#include <stdlib.h>

#define EPFX    "typecheck: error: "
#define PFX     "typecheck: "

/* See header file */
int debug_type_checker = 0;

/*
 * Keep a table of global type names
 */
const int TYPE_NAMES_MAX = 1024;
Type* type_names[TYPE_NAMES_MAX];
int type_name_count = 0;

/*
 * A stack of variable names that may help us type expressions
 */
const int VAR_MAX = 1024;
struct Var {
    Symbol name;
    TypeExpr* type;
} variable_stack[VAR_MAX];

/* ptr to the end of the stack */
struct Var* var_stack_ptr = variable_stack;


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
    assert(0 && "type not in type_names table");
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
static _Bool typexpr_equals(TypeExpr* left, TypeExpr* right)
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
          {
            TypeExpr* dleft = deref_typexpr(left->name);
            if (dleft->name == left->name)
                return 0;
            return typexpr_equals(dleft, right);
          }
        }
      }
      case TYPE_ARROW: // both type_arrows
      {
        return typexpr_equals(left->left, right->left)
            && typexpr_equals(left->right, right->right);
      }
    }
}

static TypeExpr* deref_if_needed(TypeExpr* t)
{
    if (t->tag == TYPE_NAME)
        return deref_typexpr(t->name);
    return t;
}
// TODO: finish me!
_Bool typexpr_conforms_to(TypeExpr* left, TypeExpr* right)
{
    /*
     * probably instead of NULL we should be using a special
     * type tag... or assigning special names
     */
    if (left == NULL || right == NULL)
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
    return typexpr_conforms_to(left->left, right->left)
        && typexpr_conforms_to(left->right, right->right);
}


static void add_type_name(Type* node)
{
    if (type_name_count < TYPE_NAMES_MAX) {
        if (lookup_ornull_typexpr(node->name) != NULL) {
            fprintf(stderr, EPFX"type %s has already been defined\n",
                    node->name);
            exit(EXIT_FAILURE);
        }
        if (debug_type_checker) {
            fprintf(stdout, "typecheck: adding named type: %s\n", node->name);
        }
        type_names[type_name_count++] = node;
    } else {
        fprintf(stderr, EPFX"more than %d typenames defined\n", TYPE_NAMES_MAX);
        exit(EXIT_FAILURE);
    }
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
        fprintf(stderr, EPFX"more than %d variable bindings in single scope\n", VAR_MAX);
        exit(EXIT_FAILURE);
    }
    if (debug_type_checker) {
        printf("typecheck: pushing var %s", name);
        if (type) {
            printf(" : ");
            print_typexpr(stdout, type);
        }
        fputs("\n", stdout);
    }
    *var_stack_ptr++ = (struct Var) { name, type };
}

/*
 * Lookup @name in the symbol table
 */
static struct Var* lookup_var(Symbol name)
{
    struct Var* end = var_stack_ptr;
    while (--end >= variable_stack) {
        if (end->name == name) {
            return end;
        }
    }
    fprintf(stderr, EPFX"value %s not in scope\n", name);
    exit(EXIT_FAILURE);
}

static void print_type_error(TypeExpr* expected, TypeExpr* actual)
{
    fprintf(stderr, "  expected: ");
    print_typexpr(stderr, expected);

    fprintf(stderr, "\n  actual: ");
    print_typexpr(stderr, actual);
    fputs("\n", stderr);
}

static int imposetypeon_expr(Expr* expr, TypeExpr* newtype)
{
    switch (expr->tag) {
      case PLUS:
      case MINUS:
      case MULTIPLY:
      case DIVIDE:
      case LESSTHAN: // THESE should become bool in the future
      case LESSEQUAL:
      {
        // Do not be bullied!
        return 0;
      }
      case EQUAL:
      {
        return imposetypeon_expr(expr->left, newtype)
            + imposetypeon_expr(expr->right, newtype);
      }
      case APPLY:
      {
        // Want left and right of (f x) such  that
        // f : 'a -> 'b , x : 'a
        if (expr->right->type) {
            if (!expr->left->type) {
                newtype = typearrow(newtype, expr->right->type);
                if (debug_type_checker) {
                    printf("typecheck: updating imposing type to ");
                    print_typexpr(stdout, newtype);
                    printf(" for left side of apply\n");
                }
                return imposetypeon_expr(expr->left, newtype);
            }
        }
        return 0;
      }
      case VAR:
      {
        if (!expr->type) {
            if (debug_type_checker) {
                printf("typecheck: imposing type ");
                print_typexpr(stdout, newtype);
                printf(" on free variable %s\n", expr->var);
            }
            expr->type = newtype;
            return 1;
        }
        return 0;
      }
      case INTVAL:
      {
          return 0;
      }
      case FUNC_EXPR:
      {
          return imposetypeon_expr(expr->func.subexpr, newtype);
      }
      case BIND_EXPR:
      {
          return imposetypeon_expr(expr->binding.subexpr, newtype);
      }
      case IF_EXPR:
      {
          // Don't impose type on condition - which is bool/int
          return imposetypeon_expr(expr->btrue, newtype)
              + imposetypeon_expr(expr->bfalse, newtype);
      }
    }
}

static const char * expr_name(enum ExprTag tag)
{
    assert(tag != 0);
    assert(tag <= BIND_EXPR);
    const char* expr_name[] = {
        "", "plus", "minus", "multiply", "divide",
        "equal",
        "less than", "less than or equal",
        "apply", "var", "int", "func", "let"
    };
    return expr_name[tag];
}

static int deducetype_expr(Expr* expr)
{
    TypeExpr* int_type = lookup_typexpr(symbol("int"));
    TypeExpr* unit_type = lookup_typexpr(symbol("unit"));

    if (debug_type_checker) {
        printf("typecheck: deduce type of %s expression\n",
                expr_name(expr->tag));
    }

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
        TypeExpr* left_type = expr->left->type;
        TypeExpr* right_type = expr->right->type;

        if (left_type) {
            if (!typexpr_equals(int_type, left_type)) {
                fprintf(stderr, EPFX"left side of %s expression\n",
                        expr_name(expr->tag));
                print_type_error(int_type, left_type);
                exit(EXIT_FAILURE);
            }
        } else {
            // Come back and impose types on free variables
            types_added += imposetypeon_expr(expr->left, int_type);
        }
        if (right_type) {
            if (!typexpr_equals(int_type, right_type)) {
                fprintf(stderr, EPFX"right side of %s expression\n",
                        expr_name(expr->tag));
                print_type_error(int_type, right_type);
                exit(EXIT_FAILURE);
            }
        } else {
            // Come back and impose types on free variables
            types_added += imposetypeon_expr(expr->right, int_type);
        }
        // type should be int
        if (expr->type) {
            if (!typexpr_equals(int_type, expr->type)) {
                fprintf(stderr, EPFX"%s expression\n",
                        expr_name(expr->tag));
                print_type_error(int_type, expr->type);
                exit(EXIT_FAILURE);
            }
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

        if (left_type && right_type) {
            // compare!
            if (!typexpr_equals(left_type, right_type)) {
                fprintf(stderr, EPFX"right right of equals expression does "
                        "not match left side\n");
                print_type_error(left_type, right_type);
                exit(EXIT_FAILURE);
            }
        } else if (left_type) {
            expr->right->type = left_type;
            types_added++;
            types_added += imposetypeon_expr(expr->right, left_type);
        } else if (right_type) {
            expr->left->type = left_type;
            types_added++;
            types_added += imposetypeon_expr(expr->left, right_type);
        } else {
            if (debug_type_checker) {
                printf("typecheck: unabled to work out type of either side "
                        "of equals expression\n");
            }
        }

        if (expr->type) {
            if (!typexpr_equals(int_type, expr->type)) {
                fprintf(stderr, EPFX"equals expression\n");
                print_type_error(int_type, expr->type);
                exit(EXIT_FAILURE);
            }
        } else {
            if (left_type) {
                expr->type = int_type;
                types_added++;
            }
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

        if (left_type) {
            if (left_type->tag != TYPE_ARROW) {
                if (deref_typexpr(left_type->name)->tag == TYPE_ARROW) {
                    left_type =
                        deref_typexpr(left_type->name);
                } else {
                    fprintf(stderr, EPFX"left side of apply "
                            "expression must be a function\n");
                    fprintf(stderr, "  expected: ");
                    print_typexpr(stderr, right_type);
                    fprintf(stderr, " -> 'b\n  actual: ");
                    print_typexpr(stderr, left_type);
                    fputs("\n", stderr);
                    exit(EXIT_FAILURE);
                }
            }
            if (right_type) {
                if (!typexpr_equals(
                            left_type->left, right_type)) {
                    fprintf(stderr, EPFX"right side of apply expression\n");
                    print_type_error(
                            left_type->left, right_type);
                    exit(EXIT_FAILURE);
                }
            } else {
                // impose left on right
                expr->right->type = left_type->left;
                types_added++;
            }
        } else if (right_type) {
            // impose right on left?
            if (debug_type_checker) {
                printf("typecheck: need parent to work out type of function ");
                print_typexpr(stdout, right_type);
                printf(" -> 'b\n");
            }
        } else {
            if (debug_type_checker) {
                printf("typecheck: no idea how to work out type of apply "
                        "expression\n");
            }
        }

        if (expr->type) {
            if (left_type) {
                if (!typexpr_equals(left_type->right, expr->type)) {
                    fprintf(stderr, EPFX"type of apply expression does "
                            "not match result type of function on left\n");
                    print_type_error(left_type->right, expr->type);
                    exit(EXIT_FAILURE);
                }
            }
            if (expr->right->type) {
                // we can work out the type of the function
                TypeExpr* inferred_type = typearrow(expr->right->type, expr->type);
                if (expr->left->type) {
                    if (!typexpr_equals(expr->left->type, inferred_type)) {
                        fprintf(stderr, EPFX"type of (RHS -> apply expr) "
                                "does not match the function type\n");
                        print_type_error(expr->left->type, inferred_type);
                        free((void*)inferred_type);
                        exit(EXIT_FAILURE);
                    }
                    free((void*)inferred_type);
                } else {
                    expr->left->type = inferred_type;
                    types_added++;
                }
            }
            return types_added;
        } else {
            if (left_type) {
                expr->type = left_type->right;
                types_added++;
            }
        }

        return types_added;
      }
      case VAR:
      {
        // This is more proof that () should by it's own value type
        if (expr->var == symbol("()")) {
            if (expr->type) {
                if (!typexpr_equals(unit_type, expr->type)) {
                    fprintf(stderr, EPFX"unit value\n");
                    print_type_error(unit_type, expr->type);
                    exit(EXIT_FAILURE);
                }
                return 0;
            } else {
                expr->type = unit_type;
                return 1;
            }
        } else {
            struct Var* found_var = lookup_var(expr->var);
            TypeExpr* found_type = found_var->type;
            if (expr->type) {
                if (found_type) {
                    if (!typexpr_equals(found_type, expr->type)) {
                        fprintf(stderr, EPFX"var expression type did not "
                                "match previously known type for %s\n",
                                expr->var);
                        print_type_error(found_type, expr->type);
                        exit(EXIT_FAILURE);
                    }
                } else {
                    if (debug_type_checker) {
                        printf("typecheck: infering var %s type from "
                                "expression\n", expr->var);
                    }
                    found_var->type = found_type = expr->type;
                }
                return 0;
            } else {
                if (found_type) {
                    if (debug_type_checker) {
                        printf("typecheck: type of %s found in lookup\n",
                                expr->var);
                    }
                    expr->type = found_type;
                    return 1;
                } else {
                    if (debug_type_checker) {
                        printf("typecheck: type of %s not known yet\n",
                                expr->var);
                    }
                    return 0;
                }
            }
        }
        assert(0 && "shouldn't be possible to get here");
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
      case FUNC_EXPR:
      {
        int types_added = 0;
        /*
         * Basic idea: push params onto var stack, type func body
         * restore stack, add the definition and type of the func
         * type the subexpr where the func is now defined
         */
        struct Var* pre_param_stack_ptr = var_stack_ptr;

        for (const ParamList* c = expr->func.params; c; c = c->next) {
            if (!c->param->type && c->param->name == symbol("()")) {
                c->param->type = lookup_typexpr(symbol("unit"));
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

        /*
         * When restore the stack pointer we should ideally be checking
         * whether the parameters have since had their types deduced
         * and checking against the params we pushed
         */

        for (const ParamList* c = expr->func.params; c; c = c->next) {
            Param* param = c->param;
            struct Var* it = pre_param_stack_ptr;
            // param list has to be non-empty (otherwise we are not in this loop
            do { // so do! allowed
                if (param->name == it->name) {
                    if (it->type) {
                        if (param->type) {
                            // compare
                            if (!typexpr_equals(it->type, param->type)) {
                                fprintf(stderr,EPFX"param %s of function %s\n",
                                        it->name, expr->func.name);
                                print_type_error(it->type, param->type);
                                exit(EXIT_FAILURE);
                            }
                        } else {
                            if (debug_type_checker) {
                                printf("typecheck: type of param %s deduced as ",
                                        it->name);
                                print_typexpr(stdout, it->type);
                                fputs("\n", stdout);
                            }
                            param->type = it->type;
                            ++types_added;
                        }
                    }
                    break;
                }
            } while (++it != post_param_stack_ptr);
        }

        var_stack_ptr = pre_param_stack_ptr;
        // typeof(func) = typeof(params) -> typeof(func.body)
        _Bool have_type_of_params = 1;
        for (struct Var* it = pre_param_stack_ptr;
                it != post_param_stack_ptr; ++it) {
            if (!it->type) have_type_of_params = 0;
        }
        int num_params = 0;
        for (const ParamList* c = expr->func.params; c; c = c->next) {
            if (!c->param->type)
                have_type_of_params = 0;
            num_params++;
        }

        TypeExpr* func_type = NULL;

        if (have_type_of_params && bodytype) {
            func_type = bodytype;
            for (struct Var* it = post_param_stack_ptr;
                    --it >= pre_param_stack_ptr; )
            {
                func_type = typearrow(it->type, func_type);
            }
            if (debug_type_checker) {
                printf("typecheck: worked out func %s has type: ", expr->func.name);
                print_typexpr(stdout, func_type);
                fputs("\n", stdout);
            }

            if (expr->func.functype) {
                if (!typexpr_equals(func_type, expr->func.functype)) {
                    fprintf(stderr, EPFX"type of function %s\n",
                            expr->func.name);
                    print_type_error(func_type, expr->func.functype);
                    exit(EXIT_FAILURE);
                }
            } else {
                expr->func.functype = func_type;
                types_added++;
            }
        }

        push_var(expr->func.name, func_type);

        /*
         * type the subexpr of the func expr
         */
        types_added += deducetype_expr(expr->func.subexpr);
        TypeExpr* subexprtype = expr->func.subexpr->type;

        if (expr->type) {
            if (subexprtype) {
                if (!typexpr_equals(subexprtype, expr->type)) {
                    fprintf(stderr, EPFX"recorded type of expression "
                            "following %s definition", expr->func.name);
                    print_type_error(subexprtype, expr->type);
                    exit(EXIT_FAILURE);
                }
            } else {
                expr->func.subexpr->type = expr->type;
                types_added++;
            }
        } else {
            if (subexprtype) {
                expr->type = subexprtype;
                types_added++;
            }
        }

        /*
         * It would be good at this point to find out if the type of the
         * function was inferred from the subexpr
         */
        // TODO: do something useful with this
        if (debug_type_checker) {
            if (!func_type && pre_param_stack_ptr->type) {
                printf("typecheck: funcvar %s now has type: ",
                        pre_param_stack_ptr->name);
                print_typexpr(stdout, pre_param_stack_ptr->type);
                fputs("\n", stdout);
            }
        }

        /*
         * It's probably easier just to leak this memory rather than worry
         * about who might now be holding references to it
         * OR PERHAPS, perhaps it ouught to be pinned to our tree
         */
        //if (func_type) {
        //    for (int i = 0; i < num_params; i++) {
        //        TypeExpr* tmp = func_type;
        //        func_type = func_type->right;
        //        free((void*)tmp);
        //    }
        //}

        var_stack_ptr = pre_param_stack_ptr;
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
        if (init_type) {
            if (debug_type_checker) {
                printf("typecheck: deduced type for %s as ", expr->binding.name);
                print_typexpr(stdout, init_type);
                fputs("\n", stdout);
            }
        }

        struct Var* saved_stack_ptr = var_stack_ptr;
        push_var(expr->binding.name, init_type);

        types_added += deducetype_expr(expr->binding.subexpr);
        TypeExpr* subexprtype = expr->binding.subexpr->type;

        if (expr->type) {
            if (subexprtype) {
                if (!typexpr_equals(subexprtype, expr->type)) {
                    fprintf(stderr, EPFX"conflicting types for expression "
                            "following binding of %s ", expr->binding.name);
                    print_type_error(subexprtype, expr->type);
                    fputs("\n", stdout);
                    exit(EXIT_FAILURE);
                }
            } else {
                expr->binding.subexpr->type = expr->type;
                types_added++;
            }
        } else {
            if (subexprtype) {
                expr->type = subexprtype;
                types_added++;
            }
        }

        if (!init_type && saved_stack_ptr->type) {
            if (debug_type_checker) {
                printf("typecheck: the type of %s was deduced by it's use "
                        "in the subseqent subexpression: ", expr->binding.name);
                print_typexpr(stdout, saved_stack_ptr->type);
                fputs("\n", stdout);
            }
            expr->binding.init->type = saved_stack_ptr->type;
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
        if (expr->condition->type) {
            if (!typexpr_equals(int_type, expr->condition->type)) {
                fprintf(stderr, EPFX"condition of if expression\n");
                print_type_error(int_type, expr->condition->type);
                exit(EXIT_FAILURE);
            }
        } else {
            types_added += imposetypeon_expr(expr->condition, int_type);
        }

        TypeExpr* true_type = expr->btrue->type;
        TypeExpr* false_type = expr->bfalse->type;

        if (true_type && false_type) {
            // compare!
            if (!typexpr_equals(true_type, false_type)) {
                fprintf(stderr, EPFX"true and false branches of if "
                        "expression with different types\n");
                print_type_error(true_type, false_type);
                exit(EXIT_FAILURE);
            }
        } else if (true_type) {
            expr->bfalse->type = true_type;
            types_added++;
            types_added += imposetypeon_expr(expr->bfalse, true_type);
        } else if (false_type) {
            expr->btrue->type = true_type;
            types_added++;
            types_added += imposetypeon_expr(expr->btrue, false_type);
        } else {
            if (debug_type_checker) {
                printf("typecheck: unabled to work out type of either branch "
                        "of if expression\n");
            }
        }

        if (expr->type) {
            if (!typexpr_equals(int_type, expr->type)) {
                fprintf(stderr, EPFX"if expression\n");
                print_type_error(int_type, expr->type);
                exit(EXIT_FAILURE);
            }
        } else {
            if (true_type) {
                expr->type = int_type;
                types_added++;
            }
        }
        return types_added;
      }
    }
}

/*
 * A helper function for accessing the name of any of the declration types
 */
static Symbol decl_name(Declaration* declaration)
{
    switch (declaration->tag) {
        case DECL_FUNC: return declaration->func.name;
        case DECL_BIND: return declaration->binding.name;
        case DECL_TYPE: return declaration->type.name;
    }
}
/*
 * A helper function for accessing the type of any declaration types
 * Note, the type of a type declaration doesn't really fit the semantics of
 * the other types of declarations
 */
static TypeExpr* decl_type(Declaration* declaration)
{
    switch (declaration->tag) {
        case DECL_FUNC: return declaration->func.type;
        case DECL_BIND: return declaration->binding.type;
                        // chances are we don't want this case:
        case DECL_TYPE: fprintf(stderr, PFX"warning: unexpected use of decl_type\n");
                        return declaration->type.definition;
    }
}

void type_and_check_exhaustively(DeclarationList* root)
{
    int saved_type_name_count = type_name_count;

    int types_added;
    do {
        types_added = 0;
        type_name_count = saved_type_name_count;
        struct Var* saved_stack_ptr = var_stack_ptr;

        for (DeclarationList* c = root; c; c = c->next) {
            Declaration* decl = c->declaration;
            switch (decl->tag)
            {
              case DECL_TYPE:
              {
                add_type_name(&decl->type);
                break;
              }
              case DECL_BIND:
              {
                Binding binding = decl->binding;
                if (debug_type_checker) {
                    fprintf(stdout, "typecheck: deducing type for toplevel "
                            "binding %s\n", binding.name);
                }
                types_added += deducetype_expr(binding.init);
                TypeExpr* init_type = binding.init->type;

                if (debug_type_checker) {
                    if (init_type) {
                        fprintf(stdout, "typecheck: deduced type ");
                        print_typexpr(stdout, init_type);
                        fputs("\n", stdout);
                    } else {
                        fprintf(stdout, "typecheck: unable to deduce type for "
                                "top level binding %s\n", binding.name);
                    }
                }

                if (init_type) {
                    if (binding.type) {
                        if (!typexpr_equals(binding.type, init_type)) {
                            fprintf(stderr, EPFX"type annotation "
                                    "does not matched deduced type for "
                                    "toplevel binding %s\n", binding.name);
                            exit(EXIT_FAILURE);
                        }
                    } else {
                        decl->binding.type = binding.type = init_type;
                        ++types_added;
                    }
                } else {
                    // perhaps we should try to use the type annotation
                    // to infer the types in the expressions
                }
                push_var(binding.name, binding.type);
                break;
              }
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

                struct Var* begin = pre_param_stack_ptr;
                for (const ParamList* c = decl->func.params; c; c = c->next) {
                    Param* param = c->param;
                    struct Var* it = begin++;
                    assert(param->name == it->name);
                    if (it->type) {
                        if (param->type) {
                            if (!typexpr_equals(it->type, param->type)) {
                                fprintf(stderr, EPFX"param %s of toplevel "
                                        "function %s\n", it->name,
                                        decl->func.name);
                                print_type_error(it->type, param->type);
                                exit(EXIT_FAILURE);
                            }
                        } else {
                            if (debug_type_checker) {
                                printf("typecheck: type of param %s deduced as",
                                        it->name);
                                print_typexpr(stdout, it->type);
                                fputs("\n", stdout);
                            }
                            param->type = it->type;
                            ++types_added;
                        }
                    }
                }

                _Bool have_type_of_params = 1;
                for (struct Var* it = pre_param_stack_ptr;
                        it != post_param_stack_ptr; ++it)
                {
                    if (!it->type) have_type_of_params = 0;
                }

                TypeExpr* func_type = NULL;
                if (have_type_of_params && bodytype) {
                    func_type = bodytype;
                    for (struct Var* it = post_param_stack_ptr;
                            --it >= pre_param_stack_ptr; )
                    {
                        func_type = typearrow(it->type, func_type);
                    }
                    if (debug_type_checker) {
                        printf("typecheck: worked out toplevel function %s "
                                "has type: ", decl->func.name);
                        print_typexpr(stdout, func_type);
                        fputs("\n", stdout);
                    }
                    if (decl->func.type) {
                        if (!typexpr_equals(func_type, decl->func.type)) {
                            fprintf(stderr, EPFX"type of toplevel "
                                    "function %s\n", decl->func.name);
                            print_type_error(func_type, decl->func.type);
                            exit(EXIT_FAILURE);
                        }
                    } else {
                        decl->func.type = func_type;
                        types_added++;
                    }
                }

                // restore stack and push func decl
                var_stack_ptr = pre_param_stack_ptr;
                push_var(decl->func.name, decl->func.type);
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
                    if (it->name == decl_name(decl)) {
                        printf("** %s\n", it->name);
                        printf("  stacktype: ");
                        if (it->type) print_typexpr(stdout, it->type);
                        else fputs("(null)", stdout);
                        printf("\n  tree type: ");
                        if (decl_type(decl))
                            print_typexpr(stdout, decl_type(decl));
                        else fputs("(null)", stdout);
                        fputs("\n", stdout);
                    }
                }
            }
        }

        if (debug_type_checker) {
            printf("typecheck: restoring stack pointer\n");
        }
        var_stack_ptr = saved_stack_ptr;
    } while (types_added > 0);

}

void print_binding_hierachy(FILE* out, SymList* hierachy)
{
    for (SymList* c = hierachy; c; c = c->next) {
        fprintf(out, "  in let binding %s\n", c->name);
    }
}

_Bool check_if_fully_typed_params(ParamList* params, SymList* hierachy)
{
    _Bool result = 1;
    for (ParamList* c = params; c; c = c->next) {
        if (!c->param->type) {
            fprintf(stderr, EPFX"unabled to determine type of parameter %s\n",
                    c->param->name);
            print_binding_hierachy(stderr, hierachy);
            result = 0;
        }
    }
    return result;
}
_Bool check_if_fully_typed_expr(Expr* expr, SymList* hierachy)
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
        if (!expr->type) {
            fprintf(stderr, EPFX"%s expr with no type\n", expr_name(expr->tag));
            print_binding_hierachy(stderr, hierachy);
            result = 0;
        }
        result &= check_if_fully_typed_expr(expr->left, hierachy);
        result &= check_if_fully_typed_expr(expr->right, hierachy);
        break;
      case VAR:
        if (!expr->type) {
            fprintf(stderr, EPFX"var %s with no type\n", expr->var);
            print_binding_hierachy(stderr, hierachy);
            result = 0;
        }
        break;
      case INTVAL:
        assert(expr->type != 0); // should damn well be typed
        break;
      case FUNC_EXPR:
      {
        if (!expr->func.functype) {
            fprintf(stderr, EPFX"func %s with no type\n", expr->func.name);
            print_binding_hierachy(stderr, hierachy);
            result = 0;
        }

        SymList* subhierachy = symbol_list_add(hierachy, expr->func.name);
        result &= check_if_fully_typed_params(expr->func.params, subhierachy);
        result &= check_if_fully_typed_expr(expr->func.body, subhierachy);
        result &= check_if_fully_typed_expr(expr->func.subexpr, subhierachy);
        free(subhierachy);
        if (!expr->type) {
            assert(result == 0); // better be covered by subexpr
        }
        break;
      }
      case BIND_EXPR:
      {
        SymList* subhierachy = symbol_list_add(hierachy, expr->func.name);
        result &= check_if_fully_typed_expr(expr->binding.init, subhierachy);
        result &= check_if_fully_typed_expr(expr->binding.subexpr, subhierachy);
        free(subhierachy);
        if (!expr->type) {
            assert(result == 0); // better be covered by subexpr
        }
        break;
      }
      case IF_EXPR:
      {
        if (!expr->type) {
            fprintf(stderr, EPFX"if expression with no type\n");
            print_binding_hierachy(stderr, hierachy);
            result = 0;
        }
        result &= check_if_fully_typed_expr(expr->condition, hierachy);
        result &= check_if_fully_typed_expr(expr->btrue, hierachy);
        result &= check_if_fully_typed_expr(expr->bfalse, hierachy);
        break;
      }
    }
    return result;
}
_Bool check_if_fully_typed_root(DeclarationList* root)
{
    _Bool result = 1;
    for (DeclarationList* c = root; c; c = c->next) {
        Declaration* decl = c->declaration;
        switch (decl->tag) {
          case DECL_TYPE:
            // Only incorrect if names were not found.
            // This is enforced by the type checker
            break;
          case DECL_FUNC:
          {
            SymList* hierachy = symbol_list(decl->func.name);
            if (!decl->func.type) {
                fprintf(stderr, EPFX"unable to deduce type of function %s\n",
                        decl->binding.name);
                result = 0;
            }
            result &= check_if_fully_typed_params(decl->func.params, hierachy);
            result &= check_if_fully_typed_expr(decl->func.body, hierachy);
            free(hierachy);
            break;
          }
          case DECL_BIND:
          {
            if (!decl->binding.type) {
                fprintf(stderr, EPFX"unable to deduce type of let binding %s\n",
                        decl->binding.name);
                result = 0;
            }
            SymList* hierachy = symbol_list(decl->func.name);
            result &= check_if_fully_typed_expr(decl->binding.init, hierachy);
            free(hierachy);
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
        if (decl_name(decl) == symbol("main")) {
            if (typexpr_equals(decl_type(decl), main_type)) {
                switch (decl->tag) {
                  case DECL_FUNC: return;
                  case DECL_BIND: return;
                  case DECL_TYPE: break; // don't care about type
                }
            } else if (decl->tag != DECL_TYPE) {
                fprintf(stderr, EPFX"main has incorrect type\n");
                print_type_error(main_type, decl_type(decl));
            }
        }
    }
    fprintf(stderr, EPFX"no main function found\n  val main : unit -> int\n");
    exit(EXIT_FAILURE);
}

