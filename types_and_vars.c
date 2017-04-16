/*
 * Here we store variable declarations
 * and type check our abstract syntax tree
 */
#include "types_and_vars.h"
#include <assert.h>
#include <stdlib.h>

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

static void add_type_name(Type* node)
{
    if (type_name_count < TYPE_NAMES_MAX) {
        if (lookup_ornull_typexpr(node->name) != NULL) {
            fprintf(stderr, "typecheck: error: type %s has already been defined\n",
                    node->name);
            exit(EXIT_FAILURE);
        }
        if (debug_type_checker) {
            fprintf(stdout, "typecheck: adding named type: %s\n", node->name);
        }
        type_names[type_name_count++] = node;
    } else {
        fprintf(stderr, "typecheck: error: "
                "More than %d typenames defined\n", TYPE_NAMES_MAX);
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
        fprintf(stderr, "typecheck: error: "
                "More than %d variable bindings in single scope\n", VAR_MAX);
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
    fprintf(stderr, "typecheck: error: value %s not in scope\n", name);
    exit(EXIT_FAILURE);
}

void print_type_error(TypeExpr* expected, TypeExpr* actual)
{
    fprintf(stderr, "  expected: ");
    print_typexpr(stderr, expected);

    fprintf(stderr, "\n  actual: ");
    print_typexpr(stderr, actual);
    fputs("\n", stderr);
}

static
struct count_and_type {
    int types_added;
    TypeExpr* deduced_type;
} deducetype_expr(Expr* expr)
{
    TypeExpr* int_type = lookup_typexpr(symbol("int"));
    TypeExpr* unit_type = lookup_typexpr(symbol("unit"));

    const char* expr_name[] = {
        "", "plus", "minus", "multiply", "divide",
        "equal",
        "less than", "less than or equal",
        "apply", "var", "int", "func", "let"
    };
    if (debug_type_checker) {
        printf("typecheck: deduce type of %s expression\n",
                expr_name[expr->tag]);
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
        int types_added = 0;
        struct count_and_type resleft = deducetype_expr(expr->left);
        struct count_and_type resright = deducetype_expr(expr->right);
        types_added += resleft.types_added + resright.types_added;

        if (resleft.deduced_type) {
            if (!typexpr_equals(int_type, resleft.deduced_type)) {
                fprintf(stderr, "typecheck: error: left side of %s expression\n",
                        expr_name[expr->tag]);
                print_type_error(int_type, resleft.deduced_type);
                exit(EXIT_FAILURE);
            }
        } else {
            // Come back and impose types on free variables
        }
        if (resright.deduced_type) {
            if (!typexpr_equals(int_type, resright.deduced_type)) {
                fprintf(stderr, "typecheck: error: right side of %s expression\n",
                        expr_name[expr->tag]);
                print_type_error(int_type, resright.deduced_type);
                exit(EXIT_FAILURE);
            }
        } else {
            // Come back and impose types on free variables
        }
        // type should be int
        if (expr->type) {
            if (!typexpr_equals(int_type, expr->type)) {
                fprintf(stderr, "typecheck: error: %s expression\n",
                        expr_name[expr->tag]);
                print_type_error(int_type, expr->type);
            }
        } else {
            expr->type = int_type;
            types_added++;
        }
        return (struct count_and_type) { types_added, expr->type };
      }
      case EQUAL:
      {
        int types_added = 0;
        struct count_and_type resleft = deducetype_expr(expr->left);
        struct count_and_type resright = deducetype_expr(expr->right);
        types_added += resleft.types_added + resright.types_added;

        if (resleft.deduced_type && resright.deduced_type) {
            // compare!
            if (!typexpr_equals(resleft.deduced_type, resright.deduced_type)) {
                fprintf(stderr, "typecheck: error: right right of equals "
                        "expression does not match left side\n");
                print_type_error(resleft.deduced_type, resright.deduced_type);
            }
        } else if (resleft.deduced_type) {
            expr->right->type = resleft.deduced_type;
            // TODO: impose type on right
        } else if (resright.deduced_type) {
            expr->left->type = resleft.deduced_type;
            // TODO: impose type on left
        } else {
            if (debug_type_checker) {
                printf("typecheck: unabled to work out type of either side "
                        "of equals expression\n");
            }
        }

        if (expr->type) {
            if (!typexpr_equals(resleft.deduced_type, expr->type)) {
                fprintf(stderr, "typecheck: error: plus expression\n");
                print_type_error(int_type, expr->type);
            }
        } else {
            if (resleft.deduced_type) {
                expr->type = resleft.deduced_type;
                types_added++;
            }
        }
        return (struct count_and_type) { types_added, expr->type };
      }
      case APPLY:
      {
        // Want left and right of (f x) such  that
        // f : 'a -> 'b , x : 'a

        break;
      }
      case VAR:
      {
        // This is more proof that () should by it's own value type
        if (expr->var == symbol("()")) {
            if (expr->type) {
                if (!typexpr_equals(unit_type, expr->type)) {
                    fprintf(stderr, "typecheck: error: unit value\n");
                    print_type_error(unit_type, expr->type);
                    exit(EXIT_FAILURE);
                }
                return (struct count_and_type) { 0, expr->type };
            } else {
                expr->type = unit_type;
                return (struct count_and_type) { 1, expr->type };
            }
        } else {
            struct Var* found_var = lookup_var(expr->var);
            TypeExpr* found_type = found_var->type;
            if (expr->type) {
                if (found_type) {
                    if (!typexpr_equals(found_type, expr->type)) {
                        fprintf(stderr, "typecheck: error: var expression "
                                "type did not match previously known type "
                                "for %s\n", expr->var);
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
                return (struct count_and_type) { 0, expr->type };
            } else {
                if (found_type) {
                    if (debug_type_checker) {
                        printf("typecheck: type of %s found in lookup\n",
                                expr->var);
                    }
                    expr->type = found_type;
                    return (struct count_and_type) { 1, expr->type };
                } else {
                    if (debug_type_checker) {
                        printf("typecheck: type of %s not known yet\n",
                                expr->var);
                    }
                    return (struct count_and_type) { 0, NULL };
                }
            }
        }
        assert(0 && "shouldn't be possible to get here");
      }
      case INTVAL:
      {
        // don't bother attaching types or checking
        return (struct count_and_type) { 0, int_type };
      }
      case FUNC_EXPR:
      {
        if (debug_type_checker) {
            printf("typecheck: func expression not imlemented yet\n");
        }
        return (struct count_and_type) { 0, NULL };
      }
      case BIND_EXPR:
      {
        if (debug_type_checker) {
            printf("typecheck: let expression not imlemented yet\n");
        }
        return (struct count_and_type) { 0, NULL };
      }
    }

    return (struct count_and_type) { 0 , NULL };
}


void type_check_tree(DeclarationList* root)
{
    // 1. Add built-in types to the type table
    // 2. find and add globally declared type names

    // 3. Attempt to type the tree until we cannot add any more types
    //    / report errors for inconsistencies

    add_builtin_type("int");
    add_builtin_type("unit");
    int saved_type_name_count = type_name_count;

    int types_added;
    do {
        types_added = 0;
        type_name_count = saved_type_name_count;
        void* saved_stack_ptr = var_stack_ptr;

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
                struct count_and_type res = deducetype_expr(binding.init);
                types_added += res.types_added;

                if (debug_type_checker) {
                    if (res.deduced_type) {
                        fprintf(stdout, "typecheck: deduced type ");
                        print_typexpr(stdout, res.deduced_type);
                        fputs("\n", stdout);
                    } else {
                        fprintf(stdout, "typecheck: unable to deduce type for "
                                "top level binding %s\n", binding.name);
                    }
                }

                if (res.deduced_type) {
                    if (binding.type) {
                        if (typexpr_equals(binding.type, res.deduced_type)) {
                            // When can we free the deduced type?
                            //free(res.deduced_type);
                            if (debug_type_checker) {
                                printf("typecheck: deduced type matched "
                                        "declared type\n");
                            }
                        } else {
                            fprintf(stderr, "typecheck: error: type annotation "
                                    "does not matched deduced type for "
                                    "toplevel binding %s\n", binding.name);
                            exit(EXIT_FAILURE);
                        }
                    } else {
                        decl->binding.type = binding.type = res.deduced_type;
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

                push_var(decl->func.name, decl->func.type);
                break;
              }
            }
        }
        if (debug_type_checker) {
            printf("typecheck: restoring stack pointer\n");
        }
        var_stack_ptr = saved_stack_ptr;
    } while (types_added > 0);

}

