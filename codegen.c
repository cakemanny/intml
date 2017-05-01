#define _GNU_SOURCE // ask stdio to include asprintf
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
  |  Local 1        | 3
  |-----------------|
  |  Local 2        | 4
  |-----------------|
  |  ...            |

  The closure for a function is just a struct containing the values
  the function references from above it

*/
/*
   Struct ABI (x86_64)
   ----------
   For multi-word values (e.g. func objects), use the argument registers
   to return the subsequent bytes
   |  0  |  1  |  2  |  3  |  4  |  5  |  6  |
   | rax | rdi | rsi | rdx | rcx | r8  | r9  |

   For storage in memory, words are ordered in increase offsets
   For storage on the stack they are stored in decreasin offsets... (until we fix)
*/

/*
 * Some constants
 */
#define ENTRY_SYMBOL "start"
#define PFX "codegen: "
#define EPFX "codegen: error: "
static const size_t WORD_SIZE = sizeof(void*);

/*
 * flag to enable or disable verbose debugging statements
 */
int debug_codegen = 0;

/*
 * File to write assembly code to
 */
FILE* cgenout = NULL;
/*
 * temp file where data segment is written until it is appended to the end
 */
FILE* dataout = NULL;

typedef struct Function {
    Symbol name;
    TypeExpr* type; // Param Type = type->left, ReturnType = type->right
    ParamList* closure;
    struct Function* parent;    // The enclosing function where this is
                                // defined

    // Some temporary data for while we are walking the tree
    // and working out activation records and generating code

    int var_stack_start;    // The index in the variable stack which
                            // divides the scope of the current function with
                            // it's parent
} Function;

#define MAX_FNS 1024
static Function function_table[MAX_FNS];
static int fn_table_count = 0;

#define VAR_MAX 1024
static struct Var {
    Symbol name;
    TypeExpr* type;

    int var_id; // index into the variable_table where extra into stored
} variable_stack[VAR_MAX];

typedef struct Var Var;

/* points to next free slot in the variable stack */
static Var* var_stack_ptr = variable_stack;

static struct VarEx {
    Symbol name;
    TypeExpr* type;
    Function* func;     // Function where this is a local
    int stack_offset;   // offset from start of locals
    int size;           // size of space used on stack
} variable_table[VAR_MAX];
typedef struct VarEx VarEx;
static int var_table_count = 0;

/* Some pre-declarations */
static size_t stack_size_of_type(TypeExpr* type);


static Function* add_function_w_parent(Symbol name, TypeExpr* type, Function* parent)
{
    if (fn_table_count >= MAX_FNS) {
        assert(0 && "shouldn't have made it this far...");
    }
    function_table[fn_table_count] = (Function){
        .name = name, .type = type, .parent = parent,
        .var_stack_start = var_stack_ptr - variable_stack
    };
    return &function_table[fn_table_count++];

}

static Function* add_function(Symbol name, TypeExpr* type)
{
    return add_function_w_parent(name, type, NULL);
}

static void print_function_table(FILE* out)
{
    fprintf(out, "function table:\n");
    fprintf(out, "----------------------------------------\n");
    Function* fn = function_table;
    for (int i = 0; i < fn_table_count; i++, fn++) {
        fprintf(out, "%s : ", fn->name);
        print_typexpr(out, fn->type);
        if (fn->parent) {
            fprintf(out, " <- %s\n", fn->parent->name);
        } else {
            fputs("\n", out);
        }

        fprintf(out, "\tlocals1:\n");
        for (int j = 0; j < var_table_count; j++) {
            if (variable_table[j].func == fn) {
                VarEx* v = &variable_table[j];
                fprintf(out, "\t%04x:%04x %s : ", v->stack_offset, v->size, v->name);
                print_typexpr(out, v->type);
                fputs("\n", out);
            }
        }

        fprintf(out, "\tclosure:\n");
        for (ParamList* c = fn->closure; c; c = c->next) {
            fprintf(out, "\t\t%s : ", c->param->name);
            print_typexpr(out, c->param->type);
            fputs("\n", out);
        }
    }
    fprintf(out, "----------------------------------------\n");
}

static void push_var(Symbol name, TypeExpr* type)
{
    assert(var_stack_ptr < variable_stack + VAR_MAX);
    assert(type != NULL); // We should be fully typed at this point
    *var_stack_ptr++ = (Var){ .name = name, .type = type, .var_id = -1 };
}
static void push_vars_pat(Pattern* pat)
{
    switch (pat->tag)
    {
      case PAT_VAR:
        push_var(pat->name, pat->type);
        break;
      case PAT_DISCARD:
        break;
      case PAT_CONS:
        push_vars_pat(pat->left);
        push_vars_pat(pat->right);
        break;
    }
}

/*
 * Find a variable declaration only in current function
 */
static Var* lookup_var_in_func(Function* func, Symbol name)
{
    Var* begin = variable_stack + func->var_stack_start;
    Var* end = var_stack_ptr;
    while (--end >= begin) {
        if (end->name == name) {
            return end;
        }
    }
    if (debug_codegen) {
        fprintf(stderr, PFX"var %s not found in function %s\n", name, func->name);
        fprintf(stderr, PFX"func->var_stack_start = %d\n", func->var_stack_start);
    }
    return NULL;
}

static void add_var_to_closure(Function* func, Symbol name, TypeExpr* type)
{
    // first check if it's already in there.
    // Note that the same name must be the same var in a given closure
    for (ParamList* c = func->closure; c; c = c->next) {
        if (c->param->name == name)
            return;
    }

    func->closure = add_param(func->closure, param_with_type(name, type));
    assert(func->parent); // must have parent as we've closed over something
    if (!lookup_var_in_func(func->parent, name)) {
        add_var_to_closure(func->parent, name, type);
    }
}

static void add_var_to_locals(Function* func, Symbol name, TypeExpr* type)
{
    assert(var_table_count < VAR_MAX);
    //int var_id = var_table_count;
    if (debug_codegen) {
        fprintf(stderr, PFX"Adding local (%s : ", name);
        print_typexpr(stderr, type);
        fprintf(stderr, ") to function %s\n", func->name);
    }

    /*
     * Iterate over other locals which can be in scope
     */
    Var* begin = variable_stack + func->var_stack_start;
    Var* end = var_stack_ptr;
    int stack_offset = WORD_SIZE // Saved base pointer value
        // Result
        + stack_size_of_type(func->type->right)
        // &Closure
        + WORD_SIZE;
    for (Var* it = begin; it != end; ++it) {
        stack_offset += stack_size_of_type(it->type);
    }

    variable_table[var_table_count++] = (VarEx) {
        .name = name,
        .type = type,
        .func = func,
        .stack_offset = stack_offset,
        .size = stack_size_of_type(type)
    };

    push_var(name, type);
    (var_stack_ptr - 1)->var_id = var_table_count - 1;
}

// These functions are designed to look a bit like ARM intructions so
// that we can easily port there
typedef const char* reg;
static reg r0 = "%rax";
static reg r1 = "%rdi"; // first argument or second word of return
static reg r2 = "%rsi";
static reg r3 = "%rdx";
static reg t0 = "%r10";
static reg t1 = "%r11";
static reg sp = "%rsp";
static reg bp = "%rbp";
static inline void ins2(const char* instr, const char* lop, const char* rop)
{
    fprintf(cgenout, "\t%s\t%s, %s\n", instr, lop, rop);
}
static inline void ins2_imm(const char* instr, long long immval, const char* rop)
{
#ifdef _WIN32
    fprintf(cgenout,"\t%s\t$%I64d, %s\n", instr, immval, rop);
#else
    fprintf(cgenout,"\t%s\t$%lld, %s\n", instr, immval, rop);
#endif
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
static inline void add_imm(reg dst, long long amount)
{
    ins2_imm("addq", amount, dst);
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
static inline void sub_imm(reg dst, long long amount)
{
    ins2_imm("subq", amount, dst);
}
// load but with a comment to help debugging
static void loadc(reg dst, reg src, int off, const char* comment)
{
    fprintf(cgenout,"\tmovq\t%d(%s), %s", off, src, dst);
    if (comment) fprintf(cgenout,"\t\t# %s\n", comment);
    else fputs("\n", cgenout);
}
static void store(int off, reg dst, reg src)
{
    fprintf(cgenout,"\tmovq\t%s, %d(%s)\n", src, off, dst);
}
static void mov_imm(reg dst, long long intval)
{
    ins2_imm("movq", intval, dst);
}
static void mov(reg dst, reg src)
{
    ins2("movq", src, dst);
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
    static int next_label = 0;
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
static void call_reg(reg op)
{
    fprintf(cgenout,"\tcallq\t*%s\n", op);
}
static void call(const char* label)
{
    fprintf(cgenout, "	callq	%s\n", label);
}
static void alloc(int size)
{
    // We will need to abstract out the (calling a function) idea at some point
#if defined(__APPLE__) && defined(__x86_64__)
    mov_imm("%rdi", size);
    call("_malloc");
#elif defined(__linux__) && defined(__x86_64__)
    mov_imm("%rdi", size);
    call("malloc");
#elif defined(_WIN32) && defined(__x86_64__)
    // allocate shadow space
    ins2("subq", "$32", sp);
    mov_imm("%rcx", size);
    call("malloc");
    ins2("addq", "$32", sp);
#else
#   error "Unknown platform"
#endif
    // Now the address of the allocated memory is in rax
}

static int closure_size(const Function* func)
{
    int size = 0;
    for (ParamList* c = func->closure; c; c = c->next) {
        size += stack_size_of_type(c->param->type);
    }
    return size;
}

static int stack_required(const Function* func)
{
    int space = 0
        // Result
        + stack_size_of_type(func->type->right)
        // &Closure
        + WORD_SIZE;
    // Arguments and locals
    for (int i = 0; i < var_table_count; i++) {
        const VarEx* v = &variable_table[i];
        if (v->func == func) {
            space += v->size;
        }
    }
    assert(WORD_SIZE != 16 && 16 % WORD_SIZE == 0);
    while (space % 16 != 0) {
        space += WORD_SIZE;
    }
    return space;
}

static int closure_offset(Function* func)
{
    return -WORD_SIZE // skip base pointer
        - stack_size_of_type(func->type->right); // Skip result
}
static int argument_offset(Function* func)
{
    return closure_offset(func) - WORD_SIZE;
}

__attribute__((malloc))
static char* function_label(Function* func)
{
    assert(func - function_table >= 0);
    char* pfn;
    if (asprintf(&pfn, "%s__%ld", func->name, (long)(func - function_table)) == -1) {
        perror("out of memory");
        abort();
    }
    return pfn;
}

// this will need a definition of local vars
static void emit_fn_prologue(Function* func)
{
    char* lbl = function_label(func);
    fprintf(cgenout, "%s:\n", lbl);
    free(lbl);
    fputs("\
	pushq	%rbp\n\
	movq	%rsp, %rbp\n\
", cgenout);
    fprintf(cgenout, "\tsubq\t$%d, %s\n", stack_required(func), sp);
    // Move ptr to closure into stack location
    store(closure_offset(func), bp, r1);
    if (stack_size_of_type(func->type->left) > 0) {
        store(argument_offset(func), bp, r2);
        if (stack_size_of_type(func->type->left) > WORD_SIZE) {
            store(argument_offset(func) - WORD_SIZE, bp, r3);
        }
    }
}
static void emit_fn_epilogue(Function* func)
{
    fprintf(cgenout, "\taddq\t$%d, %s\n", stack_required(func), sp);
    fputs("\
	popq	%rbp\n\
	retq\n\
", cgenout);
}


/*----------------------------------------`
| Tree walk code gen                      |
`----------------------------------------*/
static void gen_stack_machine_code(Expr* expr);

static void gen_list_from_end(ExprList* list, int list_stack_size)
{
    assert(list_stack_size == 2 * WORD_SIZE); // just for time being
    if (list->next) {
        // put the head of the end of the list on the stack
        gen_list_from_end(list->next, list_stack_size);
        // Save the generated value on the stack...
        push(r1);
        push(r0);
        // allocate memory to save the data
        alloc(list_stack_size); // == 2 * WORD_SIZE atm
        pop(t0);
        pop(t1);
        // copy the data to memory
        store(0, r0, t0);
        store(WORD_SIZE, r0, t1);
    } else {
        mov_imm(r0, 0LL); // nil
    }
    push(r0); // save address of tail
    sub_imm(sp, WORD_SIZE);
    gen_stack_machine_code(list->expr); // result now in r0, r1, ...
    // ASSUMING elem size == WORD_SIZE
    mov(r1, r0); // data goes into word 2
    add_imm(sp, WORD_SIZE);  // return stack pointer to point at our prev r0
    pop(r0); // the address of the tail. DONE
}

/*
 * Given the result of an expression on the stack, decompose and extract the
 * values into the stack locations for the variables as declared in the
 * function information
 */
static void gen_sm_unapply_pat(Pattern* pat)
{
    switch (pat->tag)
    {
      case PAT_VAR:
      {
        const VarEx var = variable_table[pat->var_id];
        switch (var.size) { // These are meant to fall through
          case 2 * WORD_SIZE:   store(-var.stack_offset - WORD_SIZE, bp, r1);
          case WORD_SIZE:       store(-var.stack_offset, bp, r0); // Store r0 into stack
          case 0:               break;
          default:
            assert(0 && "TODO: deal with larger variables");
            break;
        }
      }
      case PAT_DISCARD:
        break;
      case PAT_CONS:
      {
        // We must have either an empty list or non-empty list in result
        // position. Assume non-empty

        /* Think func list
         * Current we have: [ rax, rdi, rsi ] = [ pnext, w1, w2 ]
         * But PAT_VAR will expect: [ rax, rdi ] = [ w1, w2 ]
         */
        // Save pnext on stack
        push(r0); // don't worry about stack alignment, promise we won't alloc
        mov(r0, r1);
        mov(r1, r2);
        // Store the result into the variable on the left side of the CONS
        gen_sm_unapply_pat(pat->left);
        // Restore pnext somewhere sensible
        pop(t0);
        // Load the right side of the CONS (the tail) into registers
        // Right side of cons expects [ rax, rdi, rsi ] = [ pnext, w1, w2 ]
        switch (stack_size_of_type(pat->right->type)) {
          case 3 * WORD_SIZE:   loadc(r2, t0, 2*WORD_SIZE,  "w2 into r2");
          case 2 * WORD_SIZE:   loadc(r1, t0, WORD_SIZE,    "w1 into r1");
          case 1 * WORD_SIZE:   loadc(r0, t0, 0,            "pnext into r0");
              break;
          default:
            assert(0 && "TODO: deal with larger variables");
            break;
        }
        gen_sm_unapply_pat(pat->right);
        break;
      }
    }
}

static void gen_sm_binop(Expr* expr)
{
    gen_stack_machine_code(expr->left);     // left expression into r0
    push(r0);                               // save r0
    push(r1);
    gen_stack_machine_code(expr->right);    // right expression into r0
    pop(t1);
    pop(t0);                                // left expr into t0
}
static void gen_sm_binop_r(Expr* expr)
{
    gen_stack_machine_code(expr->right);    // right expression into r0
    push(r0);                               // save r0
    push(r1);
    gen_stack_machine_code(expr->left);     // left expression into r0
    pop(r1);
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
            gen_sm_binop(expr); // only will work for single word values
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
        {
            gen_stack_machine_code(expr->left);     // left expression into r0+
            push(r0);
            push(r1);
            gen_stack_machine_code(expr->right);    // right expression into r0

            // and r0 contains first word of arg (even if arg is () )
            // and %rdi contains second word
            mov(r2, r0); // first word of argument into second argument pos
            if (stack_size_of_type(expr->right->type) > WORD_SIZE) {
                mov(r3, r1); // move into argument 3
            }
            pop(r1); // pop closure ptr of function object is always first param
            pop(r0);
            call_reg(r0); // Emit a callq *rax kinda thing
            // if sizeof(result->type) == WORD_SIZE
            // then %rax
            // else %rax %rdi
            break;
        }
        case VAR:
        {
            // 1. Need to know what function we are in
            // 2. Need to know position the local lives in our activation
            // record, or in our closure
            // 3. Need to know the size of us and where to put us
            if (expr->var_id != -1) { // It's a local variable
                const VarEx var = variable_table[expr->var_id];
                if (var.size > 0) {
                    char* pfn; asprintf(&pfn, "var %s local word 1", var.name);
                    // Should currently we are ordering struct members
                    // downwards... should we?
                    loadc(r0, bp, -var.stack_offset, pfn); // Load word into r0
                    if (var.size > WORD_SIZE) {
                        char* pfn2; asprintf(&pfn2, "var %s local word 2",
                                var.name);
                        // load subsequent word into r1 for "return"
                        loadc(r1, bp, -var.stack_offset - WORD_SIZE, pfn2);
                        free(pfn2);
                    }
                    free(pfn);
                }
            } else {
                assert(expr->function_id != -1);
                // We are in closure land
                // What function are we in?
                Function* curr_func = function_table + expr->function_id;
                int pos = 0;
                for (ParamList* c = curr_func->closure; c; c = c->next) {
                    Param* param = c->param;
                    if (expr->var == param->name) {
                        break;
                    }
                    if (c->next) {
                        pos += stack_size_of_type(param->type);
                    } else pos = -1;
                }
                assert(pos >= 0);
                // load address of our closure
                loadc(t0, bp, closure_offset(curr_func), "addr closure");
                // load the value from closure memory
                char* pfn; asprintf(&pfn, "var %s closure word 1", expr->var);
                loadc(r0, t0, pos, pfn);
                pfn[strlen(pfn)-1] = '2';
                if (stack_size_of_type(expr->type) > WORD_SIZE) {
                    loadc(r1, t0, pos + WORD_SIZE, pfn);
                }
                free(pfn);
            }
            break;
        }
        case INTVAL:
            mov_imm(r0, expr->intval);
            break;
        case UNITVAL:
            mov_imm(r0, 0); // Not really necessary
            break;
        case STRVAL:
        {
            // 1. Emit string constant into the constants area with a label
            // 2. Emit a load instruction
            int lvalue = request_label();
            fprintf(dataout, "L%d:\n", lvalue);
            fputs("\t.string \"", dataout);
            const char* s = expr->strval;
            while (*s != 0) {
                if (*s == '\n') { fputs("\\n", dataout); }
                else if (*s == '\\') { fputs("\\\\", dataout); }
                else if (*s == '\t') { fputs("\\t", dataout); }
                else fputc(*s, dataout);
                s++;
            }
            fputs("\"\n", dataout);

            int llen = request_label();
            fprintf(dataout, "L%d:\n", llen);
            fprintf(dataout, "\t.word %lu\n", strlen(expr->strval));
            fprintf(cgenout, "	leaq	L%d(%s), %s\n", llen, "%rip", t0);
            loadc(r0, t0, 0, "length of string");
            fprintf(cgenout, "	leaq	L%d(%s), %s\n", lvalue, "%rip", r1);
            break;
        }
        case FUNC_EXPR:
        {
            // Allocate a function object on the stack
            // Allocate the closure for said function and fill with
            // required values

            // Create a label to go after the function
            // Emit Jump to after function
            // Emit function definition
            // Emit expression where function is defined
            int after_function = request_label();
            b(after_function);
            Var* pre_param_stack_ptr = var_stack_ptr;
            Function* func = function_table + expr->func.function_id;
            for (ParamList* c = expr->func.params; c; c = c->next) {
                push_var(c->param->name, c->param->type);
                emit_fn_prologue(func);
                // Should not be possible since we rewrote the tree to
                // be single param only
                assert(c->next == NULL && "multi-params fns not supported");
            }
            gen_stack_machine_code(expr->func.body);
            emit_fn_epilogue(func);
            label(after_function);
            assert(expr->func.var_id != -1);
            assert(expr->func.var_id < var_table_count);
            const VarEx varx = variable_table[expr->func.var_id];
            char* lbl = function_label(func);
            fprintf(cgenout, "\tleaq\t%s(%s), %s\n", lbl, "%rip", r0);
            store(-varx.stack_offset, bp, r0);
            free(lbl);
            // allocate closure, fill it and store in correct location
            if (closure_size(func) > 0) {
                // Reduce stack usage, use callee-saved %r12
                alloc(closure_size(func));  // Address in r0
                store(-varx.stack_offset - WORD_SIZE, bp, r0); // Save Address
                push("%r12");
                push("%r13"); // Just do this one for alignment
                mov("%r12", r0); // But also put somewhere useful too

                int offset = 0;
                for (ParamList* c = func->closure; c; c = c->next) {
                    // Create fake varnode so we can reuse the case VAR code
                    // above
                    Expr* varnode = var(c->param->name);
                    varnode->type = c->param->type;
                    if (c->param->var_id >= 0) {
                        varnode->var_id = c->param->var_id;
                    } else {
                        varnode->function_id = -1 - c->param->var_id;
                    }
                    gen_stack_machine_code(varnode); // Load value into r0,r1
                    free(varnode);
                    // Store
                    store(offset, "%r12", r0);
                    if (stack_size_of_type(c->param->type) > WORD_SIZE) {
                        store(offset + WORD_SIZE, "%r12", r1);
                    }
                    offset += stack_size_of_type(c->param->type);
                }
                pop("%r13");
                pop("%r12"); // restore r12 as per contract
            }

            var_stack_ptr = pre_param_stack_ptr;
            push_var(expr->func.name, expr->func.functype);
            gen_stack_machine_code(expr->func.subexpr);
            var_stack_ptr = pre_param_stack_ptr;
            break;
        }
        case BIND_EXPR:
        {
            BindExpr* binding = &expr->binding;
            // Idea: emit the code for the init, assign the result
            // to a location on the stack, then emit code for the subexpr
            gen_stack_machine_code(binding->init);
            // Now the result is in r0 for ints but in r0 and r1 for funcs
            gen_sm_unapply_pat(binding->pattern);

            Var* saved_stack_ptr = var_stack_ptr;
            push_vars_pat(binding->pattern);

            gen_stack_machine_code(binding->subexpr);
            var_stack_ptr = saved_stack_ptr;
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
        case LIST:
        {
            // Idea: generate code that allocates the list items from the end
            // to the start and then leaves the start item on the stack
            if (stack_size_of_type(expr->type) > 7 * WORD_SIZE) {
                fprintf(stderr, EPFX"stack size of list too large");
                exit(EXIT_FAILURE);
            }
            gen_list_from_end(expr->expr_list, stack_size_of_type(expr->type));
            break;
        }
        case VECTOR:
        {
            assert(0 && "TODO: vector codegen");
            break;
        }
        case TUPLE:
        {
            assert(0 && "TODO: tuple codgen");
            break;
        }
        case EXTERN_EXPR: // it's quite similar...
        {
            const VarEx varx = variable_table[expr->ext.var_id];
            const char* lbl = expr->ext.external_name;
            fprintf(cgenout, "\tleaq\t%s(%s), %s\n", lbl, "%rip", r0);
            store(-varx.stack_offset, bp, r0);

            Var* pre_param_stack_ptr = var_stack_ptr;
            push_var(expr->ext.name, expr->ext.type);
            gen_stack_machine_code(expr->ext.subexpr);
            var_stack_ptr = pre_param_stack_ptr;
            break;
        }
    }
}

static void emit_header()
{
#if defined(__APPLE__) && defined(__x86_64__)
    // perhaps we should call _exit in the c library for OS X...
    fputs("\
.text\n\
.global _main\n\
_main:\n\
	andq	$-16, %rsp		# align stack\n\
	callq	start__0\n\
	movq	%rax, %rdi\n\
	callq	_exit\n\
	ud2\n\
", cgenout);
#elif defined(__linux__) && defined(__x86_64__)
    fputs("\
.section .interp\n\
	.string \"/lib64/ld-linux-x86-64.so.2\"\n\
.text\n\
.global _start\n\
_start:\n\
	andq	$-16, %rsp		# align stack\n\
	callq	start__0\n\
	movq	%rax, %rdi\n\
	movq	$60, %rax		# _exit\n\
	syscall\n\
\n\
", cgenout);
#elif defined(_WIN32) && defined(__x86_64__)
    fputs("\
.text\n\
.global main\n\
main:\n\
	callq	start__0\n\
	movq	%rax, %rcx\n\
	callq	_exit\n\
	ud2\n\
\n\
", cgenout);
#else
    // TODO: add linux arm support
# error "Unsupported platform"
#endif
}

/*
 * calculates the size of the stack held part of a variable
 */
static size_t stack_size_of_type(TypeExpr* type)
{
    switch (type->tag) {
      case TYPE_NAME:
      {
        if (type->name == symbol("int")) {
            return WORD_SIZE;
        } else if (type->name == symbol("unit")) {
            return 0;
        } else if (type->name == symbol("string")) {
            // 1: length: long long
            // 2: data:   char*
            return 2 * WORD_SIZE;
        }
        fprintf(stderr, "%s\n", type->name);
        // FIXME: we either need to keep type map in here, fully expand the
        // types in the typechecker or annotate the types with size
        // information in the typechecker...
        // fully expand is probably not the answer, since we will want
        // recursive types at some point
        assert(0 && "unknown typename");
        break;
      }
      case TYPE_ARROW:
      {
        /* Function Objects
           void* fn_ptr
           void* closure
         */
        return 2 * WORD_SIZE; // Or I suppose we should add some #defines
      }
      case TYPE_CONSTRAINT:
      {
          // We should either resolve all constraints or
          // or not emit code for the function as likely not being used
          assert(0 && "unresolved type constraint");
          break;
      }
      case TYPE_CONSTRUCTOR:
      {
          if (type->constructor == symbol("list")) {

              return WORD_SIZE + stack_size_of_type(type->param);
          } else if (type->constructor == symbol("vector")) {
              // Just a pointer to the root
              // Maybe putting the first 32 elems on stack could be an opt
              // later
              return WORD_SIZE;
          } else {
              assert(0 && "TODO: other type constructors");
          }
          break;
      }
      case TYPE_TUPLE:
      {
          // stack-based tuples - add them together
          return stack_size_of_type(type->left) +
              stack_size_of_type(type->right);
      }
    }
    abort();
}

#ifdef _WIN32
static char * stpncpy(char* dst, const char* src, size_t len)
{
    while (len > 0 && *src != '\0') {
        *dst++ = *src++;
        --len;
    }
    char* end = dst;
    while (len-- > 0) {
        *dst++ = '\0';
    }
    return end;
}
#endif

// Create name for infered anon functions created by multiple param function
// The result must be freed by calling free
static char* name_param_func(Symbol func_name, int param_idx)
{
    int len = strlen(func_name) + 1 + param_idx;
    char* pfn = malloc(len);
    if (!pfn) {
        perror("out of memory");
        abort();
    }
    char* end = stpncpy(pfn, func_name, len - 1);
    for (int i = 0; i < param_idx; i++)
        *end++ = '$'; // This isn't used is normal function names
    pfn[len-1] = '\0';
    return pfn;
}

static void calculate_activation_records_pat(Pattern* pat, Function* curr_func)
{
    switch (pat->tag)
    {
      case PAT_VAR:
        add_var_to_locals(curr_func, pat->name, pat->type);
        pat->var_id = (var_stack_ptr - 1)->var_id;
        break;
      case PAT_DISCARD:
        break; // Do nothing...
      case PAT_CONS:
        calculate_activation_records_pat(pat->left, curr_func);
        calculate_activation_records_pat(pat->right, curr_func);
        break;
    }
}

static void calculate_activation_records_expr(Expr* expr, Function* curr_func)
{
    switch (expr->tag) {
      case PLUS:
      case MINUS:
      case MULTIPLY:
      case DIVIDE:
      case LESSTHAN:
      case LESSEQUAL:
      case EQUAL:
      case APPLY:
        calculate_activation_records_expr(expr->left, curr_func);
        calculate_activation_records_expr(expr->right, curr_func);
        break;
      case VAR:
      {
        Var* var = lookup_var_in_func(curr_func, expr->var);
        if (!var) {
            // recursively add to the closure of each enclosing function
            // which does not contain the variable
            add_var_to_closure(curr_func, expr->var, expr->type);
            // tag the function to find the closure it will be found in
            expr->function_id = fn_table_count - 1;
        } else {
            // Tag the var with the var_id
            expr->var_id = var->var_id;
        }
      }
      case UNITVAL:
      case INTVAL:
      case STRVAL:
        break;
      case FUNC_EXPR:
      {
        Var* pre_param_stack_ptr = var_stack_ptr;
        Function* pre_param_curr_func = curr_func;
        {
            TypeExpr* func_type = expr->func.functype;
            assert(expr->func.params->next == NULL);
            Param* param = expr->func.params->param;
            // The next call to add_function will place the function in
            // the function table
            expr->func.function_id = fn_table_count;
            curr_func =
                add_function_w_parent(expr->func.name, func_type, curr_func);
            // treat parameters like the first local
            add_var_to_locals(curr_func, param->name, param->type);
        }
        calculate_activation_records_expr(expr->func.body, curr_func);
        // Here at this point, as we descend out of the tree, we ought to
        // react to what has happened within us.
        // The closed variables were added to the head, so reverse
        curr_func->closure = reverse_params(curr_func->closure);
        // Want to work out where each var in the closure can come from
        for (ParamList* c = curr_func->closure; c; c = c->next) {
            Var* var = lookup_var_in_func(pre_param_curr_func, c->param->name);
            if (!var) {
                // Tag as coming from closure
                int function_id = (pre_param_curr_func - function_table);
                c->param->var_id = -1 - function_id;
            } else {
                // Tag param
                c->param->var_id = var->var_id;
            }
        }
        // want to rewrite node as closure type node at some point...

        // We no longer see the params in the subexpr so restore var stack and
        // restore curr_func
        var_stack_ptr = pre_param_stack_ptr;
        curr_func = pre_param_curr_func;

        add_var_to_locals(curr_func, expr->func.name, expr->func.functype);
        expr->func.var_id = (var_stack_ptr - 1)->var_id;

        // recurse
        calculate_activation_records_expr(expr->func.subexpr, curr_func);

        var_stack_ptr = pre_param_stack_ptr;
        break;
      }
      case BIND_EXPR:
      {
        calculate_activation_records_expr(expr->binding.init, curr_func);
        Var* saved_stack_ptr = var_stack_ptr;
        // modifies var_stack_ptr
        calculate_activation_records_pat(expr->binding.pattern, curr_func);

        calculate_activation_records_expr(expr->binding.subexpr, curr_func);
        var_stack_ptr = saved_stack_ptr;
        break;
      }
      case IF_EXPR:
      {
        calculate_activation_records_expr(expr->condition, curr_func);
        calculate_activation_records_expr(expr->btrue, curr_func);
        calculate_activation_records_expr(expr->bfalse, curr_func);
        break;
      }
      case LIST:
      case VECTOR:
      case TUPLE:
      {
        for (ExprList* l = expr->expr_list; l; l = l->next) {
            calculate_activation_records_expr(l->expr, curr_func);
        }
        break;
      }
      case EXTERN_EXPR:
      {
        Var* saved_stack_ptr = var_stack_ptr;
        add_var_to_locals(curr_func, expr->ext.name, expr->ext.type);
        expr->ext.var_id = (var_stack_ptr - 1)->var_id;
        calculate_activation_records_expr(expr->ext.subexpr, curr_func);
        var_stack_ptr = saved_stack_ptr;
        break;
      }
    }
}

/*
 * Change multiple parametered functions into real functions of one
 * parameter e.g. change:
 *   let f x y = x * y in ...
 * into:
 *   let f x = let f$ y = x * y in f$ in ...
 */
static void rewrite_functions(Expr* expr)
{
    switch (expr->tag) {
      case PLUS:
      case MINUS:
      case MULTIPLY:
      case DIVIDE:
      case LESSTHAN:
      case LESSEQUAL:
      case EQUAL:
      case APPLY:
        rewrite_functions(expr->left);
        rewrite_functions(expr->right);
        break;
      case VAR:
      case UNITVAL:
      case INTVAL:
      case STRVAL:
        break;
      case FUNC_EXPR:
      {
        if (expr->func.params->next) {
            // rewrite
            char* pfn = name_param_func(expr->func.name, 1);
            Symbol new_name = symbol(pfn);
            free(pfn);
            expr->func.body =
                local_func(
                        new_name,
                        expr->func.params->next,
                        expr->func.body,
                        var(new_name));
            expr->func.params->next = NULL;
        }
        // Recurse
        rewrite_functions(expr->func.body);
        if (!expr->func.body->type) {
            // It can only have one param now!
            Expr* body = expr->func.body;
            body->type = typearrow(
                    body->func.params->param->type, body->func.body->type);
            body->func.functype
                = body->func.subexpr->type
                = body->type;
        }
        rewrite_functions(expr->func.subexpr);
      }
      case BIND_EXPR:
      {
        rewrite_functions(expr->binding.init);
        rewrite_functions(expr->binding.subexpr);
        break;
      }
      case IF_EXPR:
      {
        rewrite_functions(expr->condition);
        rewrite_functions(expr->btrue);
        rewrite_functions(expr->bfalse);
        break;
      }
      case LIST:
      case VECTOR:
      case TUPLE:
      {
        for (ExprList* l = expr->expr_list; l; l = l->next) {
            rewrite_functions(l->expr);
        }
        break;
      }
      case EXTERN_EXPR:
      {
        /*
         * Maybe we should rewrite
         *  external write : int -> string -> int
         * as:
         *  let write _1 = let write$ _2 = _write (_1, _2) in write$
         */
        rewrite_functions(expr->ext.subexpr);
        break;
      }
    }
}

/*
 * Change the program from a sequence of declarations into one big
 * expression, this reduces the number of cases we have to handle in codegen
 * Basically, change:
 *   let x = ...
 *   let y = ...
 *   let main () = ...
 * into:
 *   let x = ... in
 *   let x = ... in
 *   let main () = ... in
 *   main ()
 */
Expr* restructure_tree(DeclarationList* root)
{
    if (!root) {
        Expr* final_node = apply(var(symbol("main")), unit_expr());
        // Have to add types manually because we've passed the typechecker
        // by now
        final_node->right->type = typename(symbol("unit"));
        final_node->type = typename(symbol("int"));
        final_node->left->type =
            typearrow(final_node->right->type, final_node->type);
        return final_node;
    } else {
        Declaration* decl = root->declaration;
        switch (decl->tag) {
          case DECL_TYPE:
            // skip node
            return restructure_tree(root->next);
          case DECL_FUNC:
          {
            Expr* newnode = local_func(
                    decl->func.name,
                    decl->func.params,
                    decl->func.body,
                    restructure_tree(root->next));
            newnode->type = newnode->func.subexpr->type;
            newnode->func.functype = decl->func.type;
            return newnode;
          }
          case DECL_BIND:
          {
              Expr* newnode = local_binding(
                      decl->binding.pattern,
                      decl->binding.init,
                      restructure_tree(root->next));
              newnode->type = newnode->binding.subexpr->type;
              return newnode;
          }
          case DECL_EXTERN:
          {
              Expr* newnode = local_extern(
                      decl->ext.name,
                      decl->ext.type,
                      decl->ext.external_name,
                      restructure_tree(root->next));
              newnode->type = newnode->ext.subexpr->type;
              return newnode;
          }
        }
    }
    abort();
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

    Expr* newroot = restructure_tree(root);
    if (debug_codegen) {
        fputs("restructured tree:\n\t", stderr);
        print_expr(stderr, newroot);
        fputs("\n", stderr);
    }
    rewrite_functions(newroot);
    if (debug_codegen) {
        fputs("rewritten functions:\n\t", stderr);
        print_expr(stderr, newroot);
        fputs("\n", stderr);
    }

    Function* curr_func =
        add_function(
            symbol(ENTRY_SYMBOL),
            typearrow(typename(symbol("unit")),typename(symbol("int"))));
    calculate_activation_records_expr(newroot, curr_func);

    if (debug_codegen) {
        print_function_table(stderr);
    }

    dataout = tmpfile();
    if (!dataout) {
        perror("creating a temp file for data section");
        exit(EXIT_FAILURE);
    }

    // start by not allowing any functions other than main
    // only compile expressions
    emit_header();
    emit_fn_prologue(curr_func);
    gen_stack_machine_code(newroot);
    emit_fn_epilogue(curr_func);

    fprintf(cgenout, "\n.data\n");
    fseeko(dataout, SEEK_SET, 0);
    flockfile(dataout);
    flockfile(cgenout);
    clearerr_unlocked(dataout);
    int c;
    while ((c = getc_unlocked(dataout)) != EOF) {
        putc_unlocked(c, cgenout);
    }
    funlockfile(cgenout);
    funlockfile(dataout);
    fclose(dataout);
}

