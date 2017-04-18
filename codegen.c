#include "codegen.h"
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>

/*
   Think how we would compile the following programs

1) let main () = 42
2) let main () = let f x = x * x in f 5
3) let main () = let f x y = x + y in f 5 6
4) let f z = let g x = z * x in g  let main () = f 5 5

1) # Probably something like this:
.text
.global _start
_start:
    mov eax, 42
    int h80
2)
_start:
    mov eax, 5  # argument
    call f      # result in eax
    int h80
main__f:
    imul eax, eax
    ret
3) # not sure.. something ugly I bet
*/

/*
  Activation records:
  |-----------------|
  |  Result         | 0
  |-----------------|
  |  &closure       | 1
  |-----------------|
  |  Argument       | 2
  |-----------------|
  |  Return Address | 3
  |-----------------|

  The closure for a function is just a struct containing the values
  the function references from above it

*/

/*
 * flag to enable or disable verbose debugging statements
 */
int debug_codegen = 0;

typedef struct Function {
    Symbol name;
    TypeExpr* type; // Param Type = type->left, ReturnType = type->right
    _Bool has_closure; // Or just set this NULL?
    // ClosureType = TypeExprList ?
} Function;

const int MAX_FNS = 1024;
Function function_table[MAX_FNS];
int fn_table_count = 0;

static void add_function(Symbol name, TypeExpr* type)
{
    if (fn_table_count >= MAX_FNS) {
        assert(0 && "shouldn't have made it this far...");
    }
    function_table[fn_table_count++] = (Function){
        .name = name, .type = type,
        .has_closure = 0
    };
}

static Symbol global_name(Symbol name, SymList* hierachy)
{
    // Format
    // module__toplevelbinding_bind1_bind2
    static const char* module_name = "ml";
    char buf[1024] = {};

    // Check total possible length
    int total_len = strlen(module_name) + 2 + strlen(name);
    for (SymList* c = hierachy; c; c = c->next) {
        total_len += 1 + strlen(c->name);
    }
    if (total_len > sizeof buf - 2) {
        fprintf(stderr, "codegen: error: global name of %s too long\n", name);
        // TODO: print hierachy
        exit(EXIT_FAILURE);
    }

    int len = sizeof buf;
    strncpy(buf, module_name, len);
    strlcat(buf, "__", len);

    // TODO: start with hierachy from end

    strlcat(buf, name, len);
    assert(strlen(buf) < sizeof buf - 1);

    return symbol(buf);
}

// These functions are designed to look a bit like ARM intructions so
// that we can easily port there
typedef const char* reg;
reg r0 = "%rax";
reg t0 = "%r10";
reg t1 = "%r11";
reg sp = "%rsp";
static void push(reg op0)
{
    printf("\tpushq\t%s\n", op0);
}
static void add(reg dst, reg op1, reg op2)
{
    if (dst == op1) {
        printf("\taddq\t%s, %s\n", op2, dst);
    } else {
        printf("\tmovq\t%s, %s\n", op1, dst);
        printf("\taddq\t%s, %s\n", op2, dst);
    }
}
static void mul(reg dst, reg op1, reg op2)
{
    if (dst == op1) {
        printf("\timulq\t%s, %s\n", op2, dst);
    } else {
        printf("\tmovq\t%s, %s\n", op1, dst);
        printf("\timulq\t%s, %s\n", op2, dst);
    }
}
static void load(reg dst, reg src, int off)
{
    printf("\tmovq\t%d(%s), %s\n", off, src, dst);
}
static void mov(reg dst, long long intval)
{
    printf("\tmovq\t$%lld, %s\n", intval, dst);
}
static void pop(reg dst)
{
    printf("\tpopq\t%s\n", dst);
}
static void gen_stack_machine_code(Expr* expr);
static void gen_sm_binop(Expr* expr)
{
    gen_stack_machine_code(expr->left);
    push(r0);
    gen_stack_machine_code(expr->right);
}
static void gen_stack_machine_code(Expr* expr)
{
    /*
     * We do a first approximation with some stack machine code and then
     * have another layer which generates real assembly to impement the
     * machine
     */
    switch (expr->tag) {
        case PLUS:
            gen_sm_binop(expr);
            pop(t0);
            add(r0, r0, t0);
            break;
        case MULTIPLY:
            gen_sm_binop(expr);
            pop(t0);
            mul(r0, r0, t0);
            break;
        case INTVAL:
        {
            mov(r0, expr->intVal);
            break;
        }
        default:
            assert(0 && "other cases need to be implemented");
    }
}

static void emit_fn_prologue()
{

}
static void emit_fn_epilogue()
{
}
static void emit_header()
{
#if defined(__APPLE__)
    fputs("\
.text\n\
.global start\n\
start:\n\
	callq	ml__main\n\
	movq	%rax, %rdi\n\
	movq	$0x2000001, %rax\n\
	syscall\n\
", stdout);
#else
    // TODO: work out the linux / windows instruction
# error "not yet defined"
#endif
}


void codegen(DeclarationList* root)
{
    /*
     * Want to call create a list of functions
     * Decide how many variables each takes
     * Create a representation of an activation record for each function
     *
     * Emit some boiler plate _start to call the main function
     *
     */

    // start by not allowing any functions other than main
    // only compile expressions

    for (DeclarationList* c = root; c; c = c->next) {
        Declaration* decl = c->declaration;
        if (decl->tag == DECL_FUNC && decl->func.name == symbol("main")) {
            emit_header();
            Symbol gname = global_name(decl->func.name, NULL);
            printf("%s:\n", gname);
            gen_stack_machine_code(decl->func.body);
            printf("\tretq\n");

            add_function(gname, decl->func.type);
        } else {
            fprintf(stderr,
                    "codegen: error: only support single main fn in cgen atm\n");
            exit(EXIT_FAILURE);
        }
    }
    if (debug_codegen) {
        for (int i = 0; i < fn_table_count; i++) {
            printf("codegen: function %s has been declared\n", function_table[i].name);
        }
    }
}






