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
  |  &Closure       | 1
  |-----------------|
  |  Argument       | 2
  |-----------------|
  |  Return Address | 3
  |-----------------|
  |  Local 1        | 4
  |-----------------|
  |  Local 2        | 5
  |-----------------|
  |  ...            |

  The closure for a function is just a struct containing the values
  the function references from above it

*/

/*
 * flag to enable or disable verbose debugging statements
 */
int debug_codegen = 0;

/*
 * File to write assembly code to
 */
FILE* cgenout = NULL;

int next_label = 0;

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
static inline void ins2(const char* instr, const char* lop, const char* rop)
{
    fprintf(cgenout, "\t%s\t%s, %s\n", instr, lop, rop);
}
static inline void ins1(const char* instr, const char* op)
{
    fprintf(cgenout, "\t%s\t%s\n", instr, op);
}
static void push(reg op0)
{
    ins1("pushq", op0);
}
static void add(reg dst, reg op1, reg op2)
{
    if (dst == op1) {
        ins2("addq", op2, dst); // left to right because we are AT&T syntax
    } else if (dst == op2) {
        ins2("addq", op1, dst);
    } else { // what if dst == op2?
        ins2("movq", op1, dst);
        ins2("addq", op2, dst);
    }
}
static void mul(reg dst, reg op1, reg op2)
{
    if (dst == op1) {
        ins2("imulq", op2, dst);
    } else if (dst == op2) {
        ins2("imulq", op1, dst);
    } else { // what if dst == op2?
        ins2("movq", op1, dst);
        ins2("imulq", op2, dst);
    }
}
static void sub(reg dst, reg minuend, reg amount)
{
    if (dst == minuend) {
        ins2("subq", amount, dst);
    } else if (dst == amount) {
        ins2("subq", minuend, dst);
        ins1("negq", dst);
    } else {
        ins2("movq", minuend, dst);
        ins2("subq", amount, dst);
    }
}
void load(reg dst, reg src, int off)
{
    fprintf(cgenout,"\tmovq\t%d(%s), %s\n", off, src, dst);
}
static void mov_imm(reg dst, long long intval)
{
    fprintf(cgenout,"\tmovq\t$%lld, %s\n", intval, dst);
}
static void mov(reg dst, reg src)
{
    ins2("movq", src, dst);
}
static void cmovl_imm(reg dst, long long intval)
{
    fprintf(cgenout,"\tcmovlq\t$%lld, %s\n", intval, dst);
}
static void cmovle_imm(reg dst, long long intval)
{
    fprintf(cgenout,"\tcmovleq\t$%lld, %s\n", intval, dst);
}
static void cmove_imm(reg dst, long long intval)
{
    fprintf(cgenout,"\tcmoveq\t$%lld, %s\n", intval, dst);
}
static void pop(reg dst)
{
    ins1("popq", dst);
}
static void cmp(reg left, reg right)
{
    ins2("cmpq", right, left); // do these
}

/*
 * Temporary labels look like L0, L1, L2
 */
static int request_label()
{
    return next_label++;
}
static void label(int label_number)
{
    fprintf(cgenout, "L%d:\n", label_number);
}
static void beq(int label) // branch equal
{
    fprintf(cgenout, "\tje\tL%d\n", label);
}
static void bne(int label) // branch equal
{
    fprintf(cgenout, "\tjne\tL%d\n", label);
}
static void bgt(int label) // branch equal
{
    fprintf(cgenout, "\tjg\tL%d\n", label);
}
static void bge(int label) // branch equal
{
    fprintf(cgenout, "\tjge\tL%d\n", label);
}
static void b(int label)
{
    fprintf(cgenout, "\tjmp\tL%d\n", label);
}



/*----------------------------------------`
| Tree walk code gen                      |
`----------------------------------------*/

static void gen_stack_machine_code(Expr* expr);
static void gen_sm_binop(Expr* expr)
{
    gen_stack_machine_code(expr->left);     // left expression into r0
    push(r0);                               // save r0
    gen_stack_machine_code(expr->right);    // right expression into r0
    pop(t0);                                // left expr into t0
}
static void gen_sm_binop_r(Expr* expr)
{
    gen_stack_machine_code(expr->right);    // right expression into r0
    push(r0);                               // save r0
    gen_stack_machine_code(expr->left);     // left expression into r0
    pop(t0);                                // right expr into t0
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
            add(r0, r0, t0); // r0 = r0 + t0
            break;
        case MULTIPLY:
            gen_sm_binop(expr);
            mul(r0, r0, t0); // r0 = r0 * t0
            break;
        case MINUS:
            gen_sm_binop_r(expr);
            sub(r0, r0, t0); // r0 = r0 - t0
            break;
        case DIVIDE:
            gen_sm_binop_r(expr);
#ifdef __x86_64__
            fprintf(cgenout, "\txorq\t%s, %s\n", "%rdx", "%rdx");
            ins1("idiv", t0);
#else
#           error "not implemented division yet"
#endif
            break;
        case LESSTHAN:
        case LESSEQUAL:
        case EQUAL:
        {
            gen_sm_binop(expr);
            mov(t1, r0);
            mov_imm(r0, 0LL);
            cmp(t0, t1);
            int end = request_label();
            if (expr->tag == LESSTHAN)          bge(end); // invert each cond
            else if (expr->tag == LESSEQUAL)    bgt(end);
            else if (expr->tag == EQUAL)        bne(end);
            else assert(0);
            mov_imm(r0, 1LL); // if so, skip this instruction
            label(end);
            break;
        }
        case APPLY:
            assert(0 && "apply not implemented yet");
            break;
        case VAR:
            // 1. Need to know what function we are in
            // 2. Need to know position the local lives in our activation
            // record, or in our closure
            assert(0 && "var not implemented yet");
            break;
        case INTVAL:
            mov_imm(r0, expr->intVal);
            break;
        case FUNC_EXPR:
        {
            // Create a label to go after the function
            // Jump to after function
            // Emit function definition
            // Emit expression where function is defined

            int after_function = request_label();
            b(after_function);
            //enter_function(expr->func.name);
            //exit_function();
            label(after_function);
            assert(0 && "functions not implemented yet");
            break;
        }
        case BIND_EXPR:
        {
            assert(0 && "let expressions not implmented yet");
            break;
        }
        case IF_EXPR:
        {
            gen_stack_machine_code(expr->condition);
            mov_imm(t0, 0LL);
            cmp(r0, t0);
            int end_of_true = request_label();
            int end_of_false = request_label();
            beq(end_of_true);
            { // true
                gen_stack_machine_code(expr->btrue);
            }
            b(end_of_false);
            label(end_of_true);
            { // false
                gen_stack_machine_code(expr->bfalse);
            }
            label(end_of_false);
            break;
        }
    }
}

// this will need a definition of local vars
static void emit_fn_prologue()
{

}
static void emit_fn_epilogue()
{
}
static void emit_header()
{
#if defined(__APPLE__) && defined(__x86_64__)
    fputs("\
.text\n\
.global start\n\
start:\n\
	callq	ml__main\n\
	movq	%rax, %rdi\n\
	movq	$0x2000001, %rax\n\
	syscall\n\
", cgenout);
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
     * Emit some boiler plate start to call the main function
     *
     */
    assert(cgenout != NULL);

    // start by not allowing any functions other than main
    // only compile expressions

    for (DeclarationList* c = root; c; c = c->next) {
        Declaration* decl = c->declaration;
        if (decl->tag == DECL_FUNC && decl->func.name == symbol("main")) {
            emit_header();
            Symbol gname = global_name(decl->func.name, NULL);
            fprintf(cgenout, "%s:\n", gname);
            gen_stack_machine_code(decl->func.body);
            fprintf(cgenout, "\tretq\n");

            add_function(gname, decl->func.type);
        } else {
            fprintf(stderr,
                    "codegen: error: only support single main fn in cgen atm\n");
            exit(EXIT_FAILURE);
        }
    }
    if (debug_codegen) {
        for (int i = 0; i < fn_table_count; i++) {
            fprintf(stderr, "codegen: function %s has been declared\n",
                    function_table[i].name);
        }
    }
}






