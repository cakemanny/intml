#define _GNU_SOURCE // ask stdio to include asprintf
#include "codegen.h"
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include "platform.h"

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

   Struct ABI (arm)
   ---------
   Use the arm convention of r0-r3 for arguments and return values
   | r0 | r1 | r2 | r3 |
*/

/*
 * Some constants
 */
#define ENTRY_SYMBOL "start"
#define PFX "codegen: "
#define EPFX "codegen: error: "
#define WORD_SIZE (sizeof(void*))

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
#ifdef _WIN32
char dataoutname[L_tmpnam];
#endif

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

/*
 * A fifo of the function bodies so that we can emit them sequentially
 * rather than nested
 */
typedef struct BodiesToEmit {
    Function* function;
    Expr* body;
} BodiesToEmit;
static BodiesToEmit bodies[MAX_FNS];
BodiesToEmit* bodies_begin = bodies;
BodiesToEmit* bodies_end = bodies;

#define VAR_MAX 1024
static struct Var {
    Symbol name;
    TypeExpr* type;

    int var_id; // index into the variable_table where extra into stored
    int arity;
    Function* function; // can be NULL. ptr into function table for direct call
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

#define TYPE_NAMES_MAX 1024
static struct TypeSize {
    Symbol name;
    size_t size;
} type_sizes[TYPE_NAMES_MAX];
static int type_sizes_count = 0;

static Declaration* tagged_decls[TYPE_NAMES_MAX];
static int tagged_decls_count = 0;

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
                fprintf(out, "\t-%04x:%04x %s : ", -v->stack_offset, v->size, v->name);
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

static void push_var(Symbol name, TypeExpr* type, Function* thisfn)
{
    assert(var_stack_ptr < variable_stack + VAR_MAX);
    assert(type != NULL); // We should be fully typed at this point
    // thisfn can be null
    *var_stack_ptr++ = (Var){
        .name = name, .type = type, .var_id = -1, .function = thisfn
    };
}

static Var* push_var2(Symbol name, TypeExpr* type, Var* var_stack_ptr, _Bool arity)
{
    assert(var_stack_ptr < variable_stack + VAR_MAX);
    assert(type != NULL); // We should be fully typed at this point
    *var_stack_ptr++ = (Var){
        .name = name, .type = type, .var_id = -1, .arity = arity
    };
    return var_stack_ptr;
}
static Var* lookup_var(Symbol name, Var* var_stack_ptr)
{
    Var* end = var_stack_ptr;
    while (--end >= variable_stack) {
        if (end->name == name) {
            return end;
        }
    }
    return NULL;
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

static void add_var_to_locals(Function* func, Symbol name, TypeExpr* type, Function* thisfn)
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
    int stack_offset = -WORD_SIZE // Saved base pointer value
        // Result
        - stack_size_of_type(func->type->right)
        // &Closure
        - WORD_SIZE;
    for (Var* it = begin; it != end; ++it) {
        stack_offset -= stack_size_of_type(it->type);
    }
    // point at the first word of the data
    stack_offset -= (stack_size_of_type(type) - WORD_SIZE);

    variable_table[var_table_count++] = (VarEx) {
        .name = name,
        .type = type,
        .func = func,
        .stack_offset = stack_offset,
        .size = stack_size_of_type(type)
    };

    push_var(name, type, thisfn);
    (var_stack_ptr - 1)->var_id = var_table_count - 1;
}

static void add_type_size(Symbol name, size_t size)
{
    if (type_sizes_count >= TYPE_NAMES_MAX) {
        fprintf(stderr, EPFX"too many types, > %d", TYPE_NAMES_MAX);
        exit(EXIT_FAILURE);
    }
    type_sizes[type_sizes_count++] = (struct TypeSize){
        .name = name,
        .size = size
    };
}

// These functions are designed to look a bit like ARM intructions so
// that we can easily port there
typedef const char* reg;
#if defined(__x86_64__)
    static reg r0 = "%rax";
    static reg r1 = "%rdi"; // first argument or second word of return
    static reg r2 = "%rsi";
    static reg r3 = "%rdx";
    static reg t0 = "%r10";
    static reg t1 = "%r11";
    static reg sp = "%rsp";
    static reg bp = "%rbp";
    static reg cs0 = "%r12";
    static reg cs1 = "%r13";
    static reg cs2 = "%r14";
    static reg cs3 = "%r15";
#elif defined(__arm__)
    static reg r0 = "r0";
    static reg r1 = "r1"; // first argument or second word of return
    static reg r2 = "r2";
    static reg r3 = "r3";
    static reg t0 = "r4"; // we are saving this on stack in our fn prologue
    static reg t1 = "ip";
    static reg sp = "sp";
    static reg bp = "fp";
    static reg cs0 = "r5"; // callee-saved
    static reg cs1 = "r6";
    static reg cs2 = "r7";
    static reg cs3 = "r8";
#endif

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

#ifdef __arm__
  static inline void ins2_imm_rtl(const char* instr, const char* lop, int immval)
  {
      fprintf(cgenout, "	%s	%s, #%d\n", instr, lop, immval);
  }

  static inline void ins3(
          const char* instr, const char* dst, const char* lop, const char* rop)
  {
      fprintf(cgenout, "	%s	%s, %s, %s\n", instr, dst, lop, rop);
  }
#endif

static inline void ins1(const char* instr, const char* op)
{
    fprintf(cgenout, "\t%s\t%s\n", instr, op);
}

static void add(reg dst, reg op1, reg op2)
{
    #ifdef __x86_64__
        if (dst == op1) {
            ins2("addq", op2, dst); // left to right because we are AT&T syntax
        } else if (dst == op2) {
            ins2("addq", op1, dst);
        } else { // what if dst == op2?
            ins2("movq", op1, dst);
            ins2("addq", op2, dst);
        }
    #else
        ins3("add", dst, op1, op2);
    #endif
}
static inline void add_imm(reg dst, long long amount)
{
    #ifdef __x86_64__
        ins2_imm("addq", amount, dst);
    #else
        ins2_imm_rtl("add", dst, amount);
    #endif
}
static void mul(reg dst, reg op1, reg op2)
{
    #ifdef __x86_64__
        if (dst == op1) {
            ins2("imulq", op2, dst);
        } else if (dst == op2) {
            ins2("imulq", op1, dst);
        } else { // what if dst == op2?
            ins2("movq", op1, dst);
            ins2("imulq", op2, dst);
        }
    #else
        ins3("mul", dst, op1, op2);
    #endif
}
static void sub(reg dst, reg minuend, reg amount)
{
    #ifdef __x86_64__
        if (dst == minuend) {
            ins2("subq", amount, dst);
        } else if (dst == amount) {
            ins2("subq", minuend, dst);
            ins1("negq", dst);
        } else {
            ins2("movq", minuend, dst);
            ins2("subq", amount, dst);
        }
    #else
        ins3("sub", dst, minuend, amount);
    #endif
}
static inline void sub_imm(reg dst, long long amount)
{
    #ifdef __x86_64__
        ins2_imm("subq", amount, dst);
    #else
        ins2_imm_rtl("sub", dst, amount);
    #endif
}

static void mov(reg dst, reg src);

static void sub_imm2(reg dst, reg minuend, int immediate)
{
    #ifdef __x86_64__
        if (dst == minuend) {
            sub_imm(dst, immediate);
        } else {
            mov(dst, minuend);
            sub_imm(dst, immediate);
        }
    #else
        fprintf(cgenout, "	sub	%s, %s, #%d\n", dst, minuend, immediate);
    #endif
}

// load but with a comment to help debugging
static void loadc(reg dst, reg src, int off, const char* comment)
{
    #ifdef __x86_64__
        fprintf(cgenout,"\tmovq\t%d(%s), %s", off, src, dst);
        if (comment) fprintf(cgenout,"\t\t# %s\n", comment);
        else fputs("\n", cgenout);
    #else
        fprintf(cgenout, "	ldr	%s, [%s, #%d]", dst, src, off);
        if (comment) fprintf(cgenout, "\t\t@@ %s\n", comment);
        else fputs("\n", cgenout);
    #endif
}

static void load_regoff(reg dst, reg src, reg off, _Bool negoff)
{
    #ifdef __x86_64__
        char* sign = negoff ? "-1" : "1";
        fprintf(cgenout,"	movq	(%s, %s, %s), %s\n", off, src, sign, dst);
    #else
        char* sign = negoff ? "-" : "";
        fprintf(cgenout, "	ldr	%s, [%s, %s%s]\n", dst, src, sign, off);
    #endif
}

static void load(reg dst, reg src, int off)
{
    loadc(dst, src, off, NULL);
}
static void load2(reg base, reg op0, reg op1)
{
    #ifdef __x86_64__
        load(op0, base, 0);
        load(op1, base, WORD_SIZE);
    #else
        fprintf(cgenout, "	ldm	%s, {%s,%s}\n", base, op0, op1);
    #endif
}
static void load3(reg base, reg op0, reg op1, reg op2)
{
    #ifdef __x86_64__
        load(op0, base, 0 * WORD_SIZE);
        load(op1, base, 1 * WORD_SIZE);
        load(op2, base, 2 * WORD_SIZE);
    #else
        fprintf(cgenout, "	ldm	%s, {%s,%s,%s}\n", base, op0, op1, op2);
    #endif
}
static void load4(reg base, reg op0, reg op1, reg op2, reg op3)
{
    #ifdef __x86_64__
        load(op0, base, 0 * WORD_SIZE);
        load(op1, base, 1 * WORD_SIZE);
        load(op2, base, 2 * WORD_SIZE);
        load(op3, base, 3 * WORD_SIZE);
    #else
        fprintf(cgenout, "	ldm	%s, {%s,%s,%s,%s}\n", base, op0, op1, op2, op3);
    #endif
}

static void store(int off, reg dst, reg src)
{
    #ifdef __x86_64__
        fprintf(cgenout,"\tmovq\t%s, %d(%s)\n", src, off, dst);
    #else
        // TODO: need to handle case where offset is not a rotated byte
        fprintf(cgenout, "	str	%s, [%s, #%d]\n", src, dst, off);
    #endif
}

static void store2(reg base, reg op0, reg op1)
{
    #ifdef __x86_64__
        store(0 * WORD_SIZE, base, op0);
        store(1 * WORD_SIZE, base, op1);
    #else
        fprintf(cgenout, "	stm	%s, {%s, %s}\n", base, op0, op1);
    #endif
}

static void store3(reg base, reg op0, reg op1, reg op2)
{
    #ifdef __x86_64__
        store(0 * WORD_SIZE, base, op0);
        store(1 * WORD_SIZE, base, op1);
        store(2 * WORD_SIZE, base, op2);
    #else
        fprintf(cgenout, "	stm	%s, {%s, %s, %s}\n", base, op0, op1, op2);
    #endif
}

static void store4(reg base, reg op0, reg op1, reg op2, reg op3)
{
    #ifdef __x86_64__
        store(0 * WORD_SIZE, base, op0);
        store(1 * WORD_SIZE, base, op1);
        store(2 * WORD_SIZE, base, op2);
        store(3 * WORD_SIZE, base, op3);
    #else
        fprintf(cgenout, "	stm	%s, {%s, %s, %s, %s}\n",
                base, op0, op1, op2, op3);
    #endif
}

static void mov_imm(reg dst, long long intval)
{
    #ifdef __x86_64__
        ins2_imm("movq", intval, dst);
    #else
        // arm can only hold rotated single byte in immediate
        // use ldr and = to have assembler pick automatically whether
        // immediate or store in constant pool
        char* s; asprintf(&s, "=%lld", intval);
        ins2("ldr", dst, s);
        free(s);
    #endif
}
static void mov(reg dst, reg src)
{
    #ifdef __x86_64__
        ins2("movq", src, dst);
    #else
        ins2("mov", dst, src);
    #endif
}

static void pop(reg dst)
{
    #ifdef __x86_64__
        ins1("popq", dst);
    #else
        fprintf(cgenout, "\tpop\t{%s}\n", dst);
    #endif
}
static void pop2(reg dst0, reg dst1)
{
    #ifdef __x86_64__
        pop(dst0); pop(dst1);
    #else
        fprintf(cgenout, "\tpop\t{%s,%s}\n", dst0, dst1);
    #endif
}
static void pop4(reg dst0, reg dst1, reg dst2, reg dst3)
{
    #ifdef __x86_64__
        pop(dst0); pop(dst1); pop(dst2); pop(dst3);
    #else
        fprintf(cgenout, "\tpop\t{%s,%s,%s,%s}\n", dst0, dst1, dst2, dst3);
    #endif
}

static void popd() /* Discard item on stack*/ { add_imm(sp, WORD_SIZE); }

static void push(reg op0)
{
    #ifdef __x86_64__
        ins1("pushq", op0);
    #else
        fprintf(cgenout, "\tpush\t{%s}\n", op0);
    #endif
}
/* equivalent to push(op1) and then push(op0) */
static void push2(reg op0, reg op1)
{
    #ifdef __x86_64__
        push(op1); push(op0);
    #else
        fprintf(cgenout, "\tpush\t{%s,%s}\n", op0, op1);
    #endif
}
/* equivalent to push(op3)...push(op0) */
static void push4(reg op0, reg op1, reg op2, reg op3)
{
    #ifdef __x86_64__
        push(op3); push(op2); push(op1); push(op0);
    #else
        fprintf(cgenout, "\tpush\t{%s,%s,%s,%s}\n", op0, op1, op2, op3);
    #endif
}

static void pushd() /* used to align the stack */ { sub_imm(sp, WORD_SIZE); }

static void cmp(reg left, reg right)
{
    #ifdef __x86_64__
        ins2("cmpq", right, left); // do these
    #else
        ins2("cmp", left, right); // do these
    #endif
}

static void cmp_imm(reg left, long long imm)
{

    #ifdef __x86_64__
        ins2_imm("cmpq", imm, left);
    #else
        ins2_imm_rtl("cmp", left, imm);
    #endif
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
static void load_label(reg dst, const char* lbl)
{
    #ifdef __x86_64__
        fprintf(cgenout, "\tleaq\t%s(%s), %s\n", lbl, "%rip", dst);
    #else
        fprintf(cgenout, "	ldr	%s, =%s\n", dst, lbl);
    #endif
}
static void load_tmplabel(reg dst, int label_number)
{
    char lbl[12] = {}; // 1 byte for L, 10 for largest possible unsigned, 1 for \0
    snprintf(lbl, 12, "L%u", (unsigned)label_number);
    load_label(dst, lbl);
}
static void beq(int label) // branch equal
{
    #ifdef __x86_64__
        fprintf(cgenout, "\tje\tL%d\n", label);
    #else
        fprintf(cgenout, "\tbeq\tL%d\n", label);
    #endif
}
static void bne(int label) // branch equal
{
    #ifdef __x86_64__
        fprintf(cgenout, "\tjne\tL%d\n", label);
    #else
        fprintf(cgenout, "\tbne\tL%d\n", label);
    #endif
}
static void bgt(int label) // branch equal
{
    #ifdef __x86_64__
        fprintf(cgenout, "\tjg\tL%d\n", label);
    #else
        fprintf(cgenout, "\tbgt\tL%d\n", label);
    #endif
}
static void bge(int label) // branch equal
{
    #ifdef __x86_64__
        fprintf(cgenout, "\tjge\tL%d\n", label);
    #else
        fprintf(cgenout, "\tbge\tL%d\n", label);
    #endif
}
static void b(int label)
{
    #ifdef __x86_64__
        fprintf(cgenout, "\tjmp\tL%d\n", label);
    #else
        fprintf(cgenout, "\tb\tL%d\n", label);
    #endif
}

// These work fine on arm but not on x86
#ifdef __arm__
static void moveq_imm(reg dst, int intval)
{
    ins2_imm_rtl("moveq", dst, intval);
}
static void movne_imm(reg dst, int intval)
{
    ins2_imm_rtl("movne", dst, intval);
}
#endif // __arm__

static void movne(reg dst, reg src)
{
    #ifdef __x86_64__
        ins2("cmovneq", src, dst);
    #else
        ins2("movne", dst, src);
    #endif
}


static void call_reg(reg op)
{
    #ifdef __x86_64__
        fprintf(cgenout,"\tcallq\t*%s\n", op);
    #else
        fprintf(cgenout, "\tblx\t%s\n", op);
    #endif
}
static void call(const char* label)
{
    #ifdef __x86_64__
        fprintf(cgenout, "	callq	%s\n", label);
    #else
        fprintf(cgenout, "	bl	%s\n", label);
    #endif
}
static void alloc(int size)
{
    // We will need to abstract out the (calling a function) idea at some point
#if defined(__APPLE__) && defined(__x86_64__)
    mov_imm("%rdi", size);
    call("_ml_gc_alloc");
#elif defined(__linux__) && defined(__x86_64__)
    mov_imm("%rdi", size);
    call("ml_gc_alloc");
#elif defined(__linux__) && defined(__arm__)
    mov_imm(r0, size);
    call("ml_gc_alloc");
#elif defined(_WIN32) && defined(__x86_64__)
    // allocate shadow space
    ins2("subq", "$32", sp);
    mov_imm("%rcx", size);
    call("ml_gc_alloc");
    ins2("addq", "$32", sp);
#else
#   error "Unknown platform"
#endif
    // Now the address of the allocated memory is in rax
}
static void exit_imm(int exit_code)
{
#if defined(__APPLE__) && defined(__x86_64__)
    mov_imm("%rdi", exit_code);
    call("__exit");
#elif defined(__linux__) && defined(__x86_64__)
    mov_imm("%rdi", exit_code);
    call("_exit");
#elif defined(__linux__) && defined(__arm__)
    mov_imm(r0, exit_code);
    call("_exit");
#elif defined(_WIN32) && defined(__x86_64__)
    // allocate shadow space
    ins2("subq", "$32", sp);
    mov_imm("%rcx", exit_code);
    call("_exit");
    ins2("addq", "$32", sp);
#else
#   error "Unknown platform"
#endif
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
    return closure_offset(func) - stack_size_of_type(func->type->left);
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
    #ifdef __x86_64__
        // align function start to 2^4=16 byte boundary
        fprintf(cgenout, ".p2align	4, 0x90\n");
    #else
        fprintf(cgenout, ".global	%s\n", lbl);
        fprintf(cgenout, ".p2align	2\n");
        fprintf(cgenout, ".type	%s,%%function\n", lbl);
    #endif
    fprintf(cgenout, "%s:\n", lbl);
    free(lbl);
    #ifdef __x86_64__
        fputs("\tpushq	%rbp\n", cgenout);
        fputs("\tmovq	%rsp, %rbp\n", cgenout);
    #else
        fputs("\t.fnstart\n", cgenout);
        fputs("\t.save	{r4, fp, lr}\n", cgenout);
        fputs("\tpush	{r4, fp, lr}\n", cgenout);
        fputs("\t.setfp	fp, sp\n", cgenout);
        fputs("\tmov	fp, sp\n", cgenout);
        fprintf(cgenout, "\t.pad	#%d\n", stack_required(func));
    #endif
    sub_imm(sp, stack_required(func));
    // Move ptr to closure into stack location
    if (closure_size(func) > 0) {
        store(closure_offset(func), bp, r0);
    }
    int ssot_arg = stack_size_of_type(func->type->left);
    if (ssot_arg > 0) {
        if (ssot_arg == WORD_SIZE) {
            store(argument_offset(func), bp, r1);
        } else if (ssot_arg == 2 * WORD_SIZE) {
            store(argument_offset(func), bp, r1);
            store(argument_offset(func) + WORD_SIZE, bp, r2);
        } else if (ssot_arg == 3 * WORD_SIZE) {
            store(argument_offset(func), bp, r1);
            store(argument_offset(func) + WORD_SIZE, bp, r2);
            store(argument_offset(func) + 2*WORD_SIZE, bp, r3);
        }  else if (ssot_arg > 3 * WORD_SIZE) {
            // FIXME: do something here...
            fprintf(stderr, "TODO: support larger args sizes\n");
            store(argument_offset(func), bp, r2);
            store(argument_offset(func) + WORD_SIZE, bp, r3);
            store(argument_offset(func) + 2*WORD_SIZE, bp, r3);
        }
    }
}

static void emit_fn_epilogue(Function* func)
{
#ifdef __x86_64__
    fprintf(cgenout, "\taddq\t$%d, %s\n", stack_required(func), sp);
    fputs("\
	popq	%rbp\n\
	retq\n\
", cgenout);
#else
    fputs("\tmov sp, fp\n", cgenout);
    fputs("\tpop	{r4, fp, pc}\n", cgenout);
    char* lbl = function_label(func);
    fprintf(cgenout, "\t.size	%s, .-%s\n", lbl, lbl);
    fprintf(cgenout, "\t.cantunwind\n");
    fprintf(cgenout, "\t.fnend\n");
    free(lbl);
#endif
}

static int label_for_str_or_add(Symbol str_lit)
{
    #define LIT_LBLS_MAX 1024
    static struct { Symbol literal; int label; } lookup[LIT_LBLS_MAX];
    static int lookup_count = 0;

    // Check if we have already emitted, else emit
    for (int i = 0; i < lookup_count; i++) {
        if (str_lit == lookup[i].literal)
            return lookup[i].label;
    }

    int label = request_label();
    fprintf(dataout, ".p2align %d\n", __builtin_ctz(WORD_SIZE));
    fprintf(dataout, "L%d:\n", label);
    fputs("\t.string \"", dataout);
    const char* s = str_lit;
    while (*s != 0) {
        if (*s == '\n') { fputs("\\n", dataout); }
        else if (*s == '\\') { fputs("\\\\", dataout); }
        else if (*s == '\t') { fputs("\\t", dataout); }
        else fputc(*s, dataout);
        s++;
    }
    fputs("\"\n", dataout);

    lookup[lookup_count].literal = str_lit;
    lookup[lookup_count].label = label;
    lookup_count += 1;
    return label;
}


#ifdef __x86_64__
#   define ASM_COMMENT "# "
#else
#   define ASM_COMMENT "@ "
#endif
#define dbg_comment(FMT, ...) do {                              \
    if (debug_codegen) {                                        \
        fprintf(cgenout, ASM_COMMENT FMT "\n", ##__VA_ARGS__);  \
    }                                                           \
} while (0)


/*----------------------------------------`
| Tree walk code gen                      |
`----------------------------------------*/
static void gen_stack_machine_code(Expr* expr);

/*
 List representation
 -------------------
 P0
  -> [ P1, W01, W02, ... ]
        -> [ P2, W11, W12, ... ]
              -> [ 0, W12, W22, ... ]
 In C struct notation:
 typedef struct ListNode* LIST;
 struct ListNode {
   struct ListNode* tail;
   struct Data { W1 w1; W2 w2; ... }
 };

 This give us that typeof(((LIST)l)->tail) == LIST
 And LIST l = 0; is the empty list
 */
static void gen_list_from_end(ExprList* list, int node_size)
{
    // TODO: need to work out larger sizes
    assert(node_size <= 4 * WORD_SIZE); // just for time being
    if (list) {
        // put the tail of list in r0
        gen_list_from_end(list->next, node_size);
        push(r0); // save address of tail
        pushd(); // align stack
        gen_stack_machine_code(list->expr); // result now in r0, r1, ...
        // ASSUMING elem size == WORD_SIZE
        if (node_size >= 4 * WORD_SIZE) mov(r3, r2);
        if (node_size >= 3 * WORD_SIZE) mov(r2, r1);
        mov(r1, r0); // data goes into word 2
        popd();  // return stack pointer to point at our prev r0
        pop(r0); // the address of the tail.

        // Save the generated value on the stack...
        if (node_size >= 3 * WORD_SIZE) {
            push4(r0, r1, r2, r3);
        } else {
            push2(r0, r1);
        }
        // allocate memory to save the data
        alloc(node_size); // == 2 * WORD_SIZE atm

        pop2(t0, t1);
        // copy the data to memory
        store2(r0, t0, t1);

        if (node_size >= 3 * WORD_SIZE) {
            pop2(t0, t1);
            store(2 * WORD_SIZE, r0, t0);
            if (node_size >= 4 * WORD_SIZE)
                store(3 * WORD_SIZE, r0, t1);
        }
        // r0 is left as the pointer to list node
    } else {
        mov_imm(r0, 0LL); // nil
    }
}

int ctor_tag_value(Symbol ctor_name, Symbol type_name)
{
    for (int i = 0; i < tagged_decls_count; i++) {
        if (type_name == tagged_decls[i]->ctor.name) {
            CtorList* ctors = tagged_decls[i]->ctor.ctors;
            int tag_val = 0;
            for (CtorList* l = ctors; l; l = l->next) {
                if (l->ctor->name == ctor_name) {
                    return tag_val;
                }
                tag_val++;
            }
            break;
        }
    }
    fprintf(stderr, "constructor: %s : %s\n", ctor_name, type_name);
    assert(0 && "constructor tag not found");
    abort();
}

/*
 * Given the result of an expression on the stack, decompose and extract the
 * values into the stack locations for the variables as declared in the
 * function information
 * Leaves 1 in r0 if match successful 0 otherwise
 */
static void gen_sm_unapply_pat(Pattern* pat)
{
    switch (pat->tag)
    {
      case PAT_VAR:
      {
        const VarEx var = variable_table[pat->var_id];
        switch (var.size) { // These are meant to fall through
          case 4 * WORD_SIZE:
              sub_imm2(t0, bp, -var.stack_offset);
              store4(t0, r0, r1, r2, r3);
              break;
          case 3 * WORD_SIZE:
              sub_imm2(t0, bp, -var.stack_offset);
              store3(t0, r0, r1, r2);
              break;
          case 2 * WORD_SIZE:
              store(var.stack_offset, bp, r0);
              store(var.stack_offset + WORD_SIZE, bp, r1);
              break;
          case WORD_SIZE: store(var.stack_offset, bp, r0); // Store r0 into stack
          case 0: break;
          default:
            assert(0 && "TODO: deal with larger variables");
            break;
        }
        mov_imm(r0, 1LL);
        break;
      }
      case PAT_DISCARD:
      {
        // Discard matches anything
        mov_imm(r0, 1LL);
        break;
      }
      case PAT_CONS:
      {
        // We must have either an empty list or non-empty list in result
        // position.
        int end_of_matching = request_label();
        cmp_imm(r0, 0LL); // Check-null phead==0 => empty
        beq(end_of_matching);

        // Load head data
        mov(t0, r0);
        #ifdef __x86_64__
            fprintf(cgenout, "	leaq	%lu(%s), %s\n", WORD_SIZE, r0, t1);
        #else
            // would like to do multiple load w/offset below
            mov(t1, r0);
            add_imm(t1, WORD_SIZE);
        #endif
        switch (stack_size_of_type(pat->left->type)) {
          case 4 * WORD_SIZE:   load4(t1, r0, r1, r2, r3); break;
          case 3 * WORD_SIZE:   load3(t1, r0, r1, r2); break;
          case 2 * WORD_SIZE:   load2(t1, r0, r1); break;
          case 1 * WORD_SIZE:   loadc(r0, t1, 0,  "w1 into r0"); break;
          case 0 * WORD_SIZE:   break;
          default:
            assert(0 && "TODO: deal with larger variables");
            break;
        }
        loadc(t1, t0, 0, "pnext into t1");

        // Save pnext on stack -- if pat->left is discard or var we can elide
        // this push (later in optimization excercise)
        push(t1); // don't worry about stack alignment, promise we won't alloc
        // Store the result into the variable on the left side of the CONS
        gen_sm_unapply_pat(pat->left);
        pop(t1);
        cmp_imm(r0, 0LL); // but since left is a pattern, it may fail
        beq(end_of_matching);

        // Restore pnext where the rest of the pattern matching can use it
        mov(r0, t1);
        gen_sm_unapply_pat(pat->right);
        // Let the result of unapply of RHS be the return value of of the whole
        // unapply (since we know unapply of left succeeded at this point)

        label(end_of_matching);
        // No need to do anything here - if was null then 0 still in r0
        break;
      }
      case PAT_TUPLE:
      {
        // We can assume that we have the same type as the init value,
        // including number of items in the tuple
        assert(stack_size_of_type(pat->type) <= 4 * WORD_SIZE);
        reg rs[4] = { cs0, cs1, cs2, cs3 };
        reg drs[4] = { r0, r1, r2, r3 };
        const int word_size_ctz = __builtin_ctz(WORD_SIZE);
        int stack_size = stack_size_of_type(pat->type);
        // Shuffle data about
        dbg_comment("PAT_TUPLE: save acc to callee-saved registers");
        for (int i = 0, n = stack_size >> word_size_ctz; i < n; i++) {
            push(rs[n - 1 - i]); // Save r12-r15
            mov(rs[n - 1 - i], drs[n - 1 - i]); // copy r0-r3 int r12-r15
        }
        //  Fill variables with the data
        dbg_comment("PAT_TUPLE: fill variables with values");
        int rs_idx = 0;
        for (PatternList* l = pat->pat_list; l; l = l->next) {
            int words = stack_size_of_type(l->pattern->type) >> word_size_ctz;
            for (int i = 0; i < words; i++, rs_idx++) {
                mov(drs[i], rs[rs_idx]);
            }
            gen_sm_unapply_pat(l->pattern);
        }
        // Shuffle data back
        dbg_comment("PAT_TUPLE: restore callee-saved registers");
        for (int i = 0, n = stack_size >> word_size_ctz; i < n; i++) {
            pop(rs[i]); // Restore r12-r15 per constract
        }
        break;
      }
      case PAT_CTOR_NOARG:
      {
        // This should be the same as int comparison but just
        // for the given tag value... (which we need to work out)
        dbg_comment("PAT_CTOR_NOARG: Compare constructor %s", pat->ctor_name);

        // We should probably implement this by putting the ctors in table
        // and tagging with an ID
        assert(pat->type->tag == TYPE_NAME);
        int tag_val = ctor_tag_value(pat->ctor_name, pat->type->name);
        // Should we synthesise a PAT_INT and recurse instead?

        cmp_imm(r0, tag_val);
        mov_imm(r0, 0LL);
        int end = request_label();
        bne(end);
        mov_imm(r0, 1LL); // if so, skip this instruction
        label(end);
        break;
      }
      case PAT_CTOR_WARG:
      {
        // Check the constructor tag. If it matches then
        // shift the data left in the registers and recurse on the
        // subpattern
        dbg_comment("PAT_CTOR_WARG: Compare constructor %s", pat->ctor_name);
        assert(pat->type->tag == TYPE_NAME);
        int tag_val = ctor_tag_value(pat->ctor_name, pat->type->name);
        cmp_imm(r0, tag_val);
        mov_imm(r0, 0LL);
        int end = request_label();
        bne(end);
        // We match - Now check the subpattern!
        switch (stack_size_of_type(pat->ctor_arg->type)) {
          case 3 * WORD_SIZE: mov(r0, r1); mov(r1, r2); mov(r2, r3); break;
          case 2 * WORD_SIZE: mov(r0, r1); mov(r1, r2); break;
          case 1 * WORD_SIZE: mov(r0, r1); break;
          case 0 * WORD_SIZE: mov_imm(r0, 1LL); break; // not sure we need this...
          default: assert(0 && "TODO: larger sizes"); break;
        }
        // Check the pattern the ctor is applied to
        gen_sm_unapply_pat(pat->ctor_arg);

        label(end);
        break;
      }
      case PAT_INT:
      {
        // If r0 is equal to the literal value, then 1 else 0
        #ifdef __arm__
            // Special cases, pattern is 1 or 0:
            if (pat->intval == 0) {
                cmp_imm(r0, 0LL);
                moveq_imm(r0, 1LL);
                movne_imm(r0, 0LL);
            } else if (pat->intval != 1) {
                // This might actually work in the general case...
                cmp_imm(r0, pat->intval);
                movne_imm(r0, 0LL);
                // if they are equal, leave the value in place since truthy
            }
        #else
            // general case
            cmp_imm(r0, pat->intval);
            mov_imm(r0, 0LL);
            int end = request_label();
            bne(end);
            mov_imm(r0, 1LL); // if so, skip this instruction
            label(end);
        #endif
        break;
      }
      case PAT_STR:
      {
        int len = strlen(pat->strval);
        mov_imm(t0, len);
        // compare lengths
        cmp(r0, t0);
        mov_imm(r0, 0LL); // load false result to return if not equal
        int match_end = request_label();
        bne(match_end);

        // Now compare the actual data
        load_tmplabel(t1, label_for_str_or_add(pat->strval));
        // Save the argument string address into t0, so we can do multi-load
        // into r0,r1,r2,r3
        mov(t0, r1);

        int num_words = (len + (WORD_SIZE - 1))/WORD_SIZE;
        // Compare up to 8 words at a time?
        if (num_words <= 8) {
            // Since we have .p2align directive on the strings, should
            // get zeroes for the extra bytes
            mov_imm(r0, 1LL);
            for (int i = 0; i < num_words; i++) {
                loadc(r1, t0, i * WORD_SIZE, "word from arg");
                loadc(r2, t1, i * WORD_SIZE, "word from pat");
                if (i == (num_words - 1)) {
                    // mask off any extra bytes?
                    int excess = (num_words * WORD_SIZE) - len;
                    if (excess != 0) {
                        // TODO! // Only necessary if our allocator doesn't
                        // zero out memory
                    }
                }
                cmp(r1, r2);
                #ifdef __arm__
                    movne_imm(r0, 0LL);
                #else
                    mov_imm(r1, 0LL);
                    movne(r0, r1);
                #endif
            }
        } else {
            int loop_head = request_label();
            int no_match = request_label();
            // Need to loop this shit
            // This should almost definitely be a procedure call
            mov_imm(r0, 0LL); // offset
            mov_imm(r3, (len + WORD_SIZE - 1) / WORD_SIZE); // loop counter
            // have t0 and t1 be the end of the string a
            label(loop_head);

            load_regoff(r1, t0, r0, 0);
            load_regoff(r2, t1, r0, 0);
            cmp(r1, r2);
            bne(no_match);

            add_imm(r0, WORD_SIZE);
            sub_imm(r3, 1LL); // This sets the flags
            //cmp_imm(r3, 0LL); // so don't need this
            bne(loop_head);
            // If we get here, then the strings matched

            // match
            mov_imm(r0, 1LL);
            b(match_end);
            label(no_match);
            mov_imm(r0, 0LL);
        }

        label(match_end);
        break;
      }
      case PAT_NIL:
      {
        // I'm sure this could probably be done in a single instruction....
        cmp_imm(r0, 0LL); // Check-null phead==0 => empty
        #ifdef __arm__
            moveq_imm(r0, 1LL);
            movne_imm(r0, 0LL);
        #else
            int end = request_label();
            mov_imm(r0, 1LL); // Assume they are equal
            beq(end);
            mov_imm(r0, 0LL); // But if the are not, then execute this instruction
            label(end);
        #endif // __arm__
        break;
      }
    }
}

static void gen_sm_binop(Expr* expr)
{
    gen_stack_machine_code(expr->left);     // left expression into r0
    push2(r0, r1);                          // save r0
    gen_stack_machine_code(expr->right);    // right expression into r0
    pop2(t0, t1);                            // left expr into t0
}
static void gen_sm_binop_r(Expr* expr)
{
    gen_stack_machine_code(expr->right);    // right expression into r0
    push2(r0, r1);                           // save r0
    gen_stack_machine_code(expr->left);     // left expression into r0
    pop2(t0, t1);                            // right expr into t0
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
            // arm says Rd and Rm should be different in mul
            mul(r0, t0, r0); // r0 = t0 * 00
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
                ins3("sdiv", r0, r0, t0);
            #endif
            break;
        case LESSTHAN:
        case LESSEQUAL:
        {
            gen_sm_binop(expr);
            cmp(t0, r0);
            mov_imm(r0, 0LL);
            int end = request_label();
            if (expr->tag == LESSTHAN)          bge(end); // invert each cond
            else if (expr->tag == LESSEQUAL)    bgt(end);
            else assert(0);
            mov_imm(r0, 1LL); // if so, skip this instruction
            label(end);
            break;
        }
        case EQUAL:
        {
            gen_sm_binop(expr); // only will work for single word values
            cmp(t0, r0);
            mov_imm(r0, 0LL);
            int end = request_label();
            bne(end);
            if (stack_size_of_type(expr->left->type) > WORD_SIZE) {
                cmp(t1, r1);
                bne(end);
            }
            mov_imm(r0, 1LL); // if so, skip this instruction
            label(end);
            break;
        }
        case APPLY:
        {
            gen_stack_machine_code(expr->left);     // left expression into r0+
            push2(r0, r1);
            gen_stack_machine_code(expr->right);    // right expression into r0

            // and r0 contains first word of arg (even if arg is () )
            // and %rdi contains second word
            int ssot = stack_size_of_type(expr->right->type);
            if (ssot > WORD_SIZE) {
                if (ssot > 2 * WORD_SIZE) {
                    if (ssot > 3 * WORD_SIZE) {
                        //assert(0 && "TODO: apply: larger argument types");
                        fprintf(stderr, EPFX"warn: TODO: larger arg types\n");
                    }
                    mov(r3, r2);
                }
                mov(r2, r1); // move into argument 3
            }
            mov(r1, r0); // first word of argument into second argument pos

            pop2(t0, t1); // pop closure ptr of function object is always first param
            mov(r0, t1);
            call_reg(t0); // Emit a callq *rax kinda thing
            break;
        }
        case DIRECT_CALL:
        {
            Function* func = function_table + expr->dc_func_id;
            if (closure_size(func) > 0) {
                // var lookup to get closure reference
                Expr* varnode = var(expr->funcname);
                varnode->type = func->type;
                if (expr->dc_var_id >= 0) {
                    varnode->var_id = expr->dc_var_id;
                } else {
                    varnode->enclosing_func_id = expr->dc_closingfunc_id;
                }
                gen_stack_machine_code(varnode);
                free(varnode);

                push2(r0, r1);
            }

            for (ExprList* l = expr->args; l; l = l->next) {
                gen_stack_machine_code(l->expr);
                assert((!l->next) && "TODO: multiple args");
            }
            int ssot = stack_size_of_type(expr->args->expr->type);
            if (ssot > WORD_SIZE) {
                if (ssot > 2 * WORD_SIZE) {
                    if (ssot > 3 * WORD_SIZE) {
                        //assert(0 && "TODO: direct-call: larger argument types");
                        fprintf(stderr, EPFX"warn: TODO: larger arg types\n");
                    }
                    mov(r3, r2);
                }
                mov(r2, r1); // move into argument 3
            }
            mov(r1, r0);

            // restore looked up closure value
            // then call
            if (closure_size(func) > 0) {
                pop2(t0, t1);
                mov(r0, t1);
            }
            char* lbl = function_label(func);
            call(lbl);
            free(lbl);
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
                    loadc(r0, bp, var.stack_offset, pfn); // Load word into r0
                    if (var.size > WORD_SIZE) {
                        char* pfn2; asprintf(&pfn2, "var %s local word 2",
                                var.name);
                        // load subsequent word into r1 for "return"
                        loadc(r1, bp, var.stack_offset + WORD_SIZE, pfn2);
                        free(pfn2);
                        if (var.size > 2 * WORD_SIZE) {
                            char* pfn3; asprintf(&pfn3, "var %s local word 3",
                                    var.name);
                            // load subsequent word into r1 for "return"
                            loadc(r2, bp, var.stack_offset + 2*WORD_SIZE, pfn3);
                            free(pfn3);
                            if (var.size > 3 * WORD_SIZE) {
                                fprintf(stderr, "TODO: VAR: larger var sizes\n");
                            }
                        }
                    }
                    free(pfn);
                }
            } else {
                assert(expr->enclosing_func_id != -1);
                // We are in closure land
                // What function are we in?
                Function* curr_func = function_table + expr->enclosing_func_id;
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
                int ssot = stack_size_of_type(expr->type);
                if (ssot <= WORD_SIZE) {
                    char* pfn; asprintf(&pfn, "var %s word 1", expr->var);
                    loadc(r0, t0, pos, pfn);
                    free(pfn);
                } else {
                    if (pos != 0) {
                        add_imm(t0, pos);
                    }
                    switch (ssot) {
                        case 2 * WORD_SIZE: load2(t0, r0, r1); break;
                        case 3 * WORD_SIZE: load3(t0, r0, r1, r2); break;
                        case 4 * WORD_SIZE: load4(t0, r0, r1, r2, r3); break;
                        default: assert(0 && "TODO: VAR: larger var sizes");
                                 break;
                    }
                }
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
            // Length of string into r0 - not include terminating null...
            mov_imm(r0, strlen(expr->strval));
            // load address of string into r1
            load_tmplabel(r1, label_for_str_or_add(expr->strval));
            break;
        }
        case RECFUNC_EXPR:
        case FUNC_EXPR:
        {
            // Allocate a function object on the stack
            // Allocate the closure for said function and fill with
            // required values

            // Add function definition body to BodiesToEmit FIFO
            // Emit expression where function is defined

            Function* func = function_table + expr->func.function_id;
            assert(expr->func.params->next == NULL && "multi-params fns not supported");

            *bodies_end++ = (BodiesToEmit){
                .function = func, .body = expr->func.body
            };

            assert(expr->func.var_id != -1);
            assert(expr->func.var_id < var_table_count);
            const VarEx varx = variable_table[expr->func.var_id];
            char* lbl = function_label(func);
            load_label(r0, lbl);
            store(varx.stack_offset, bp, r0);
            free(lbl);
            // allocate closure, fill it and store in correct location
            if (closure_size(func) > 0) {
                // Reduce stack usage, use callee-saved %r12
                alloc(closure_size(func));  // Address in r0
                store(varx.stack_offset + WORD_SIZE, bp, r0); // Save Address
                push2(cs0, cs1);
                mov(cs0, r0); // But also put somewhere useful too

                int offset = 0;
                for (ParamList* c = func->closure; c; c = c->next) {
                    // Create fake varnode so we can reuse the case VAR code
                    // above
                    Expr* varnode = var(c->param->name);
                    varnode->type = c->param->type;
                    if (c->param->var_id >= 0) {
                        varnode->var_id = c->param->var_id;
                    } else {
                        varnode->enclosing_func_id = -1 - c->param->var_id;
                    }
                    gen_stack_machine_code(varnode); // Load value into r0,r1
                    free(varnode);
                    // Store
                    store(offset, cs0, r0);
                    if (stack_size_of_type(c->param->type) > WORD_SIZE) {
                        store(offset + WORD_SIZE, cs0, r1);
                    }
                    offset += stack_size_of_type(c->param->type);
                }
                pop2(cs0, cs1); // restore cs0 as per contract
            }

            gen_stack_machine_code(expr->func.subexpr);
            break;
        }
        // TODO: we can probably replace all bind and if expressions by match
        // expressions in a rewrite phase
        case BIND_EXPR:
        {
            BindExpr* binding = &expr->binding;
            // Idea: emit the code for the init, assign the result
            // to a location on the stack, then emit code for the subexpr
            gen_stack_machine_code(binding->init);

            gen_sm_unapply_pat(binding->pattern); // leaves 1 if matched
            // TODO: if match fails branch to exit
            cmp_imm(r0, 0LL);
            int match_success = request_label();
            bne(match_success);
            // Otherwise, we failed. Want to print "match failed" and exit(1)
            exit_imm(99);

            label(match_success);
            gen_stack_machine_code(binding->subexpr);
            break;
        }
        case IF_EXPR:
        {
            gen_stack_machine_code(expr->condition);
            cmp_imm(r0, 0LL);
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
            // to the start and then leaves pointer to start on the stack
            gen_list_from_end(expr->expr_list,
                    WORD_SIZE + stack_size_of_type(expr->type->param));
            break;
        }
        case VECTOR:
        {
            assert(0 && "TODO: vector codegen");
            break;
        }
        case TUPLE:
        {
            // Evaluate the expressions, left to right, then push the results
            // onto the stack, once all expressions are evaluated, pop the
            // values back into registers

            // Things to think about...
            // * how wide each each type

            // 5 may be our limit for moment because we would want to be able
            // to pass around lists which require pointer word in node
            // and there are only 6 argument registers on x86
            // Could have (char * char * ... * char) if char only takes up a
            // byte
            assert(stack_size_of_type(expr->type) <= 4 * WORD_SIZE);
            assert(stack_size_of_type(expr->type) >= 2 * WORD_SIZE);
            for (ExprList* l = expr->expr_list; l; l = l->next) {
                dbg_comment("TUPLE: gen init for tuple elem");
                gen_stack_machine_code(l->expr);
                dbg_comment("TUPLE: save calculated value");
                // Since we are going forward through the list
                // push the words on in order
                switch (stack_size_of_type(l->expr->type)) {
                    case 4 * WORD_SIZE:
                    case 3 * WORD_SIZE: push(r0); push(r1); push(r2); push(r3);
                                        break;
                    case 2 * WORD_SIZE:
                    case 1 * WORD_SIZE: push(r0); push(r1);
                    case 0:
                        break;
                    default: assert(0 && "TODO: any size tuple"); break;
                }
            }
            assert(expr->expr_list->next); // tuples always of size 2 or >
            ExprList* ls[4] = { expr->expr_list, 0, 0, 0 };
            // Start with the end
            ls[1] = ls[0]->next;
            if ((ls[2] = ls[1]->next)) {
                if ((ls[3] = ls[2]->next)) { }
            }

            // Remember that it might be that we have
            // (int * int * int * int)
            // in which case because of our double push, this is stored as
            // [ w, 0, x, 0, y, 0, z, 0 ] but want [ x, y, w, z ]

            // iterate backwars through the expression results and the
            // destination registers
            reg rs[4] = { r0, r1, r2, r3 };
            int j = -1 +
                (stack_size_of_type(expr->type) >> __builtin_ctz(WORD_SIZE));
            for (int i = 3; i >= 0; --i) {
                if (ls[i]) {
                    dbg_comment("TUPLE: load component %d" , i);
                    switch (stack_size_of_type(ls[i]->expr->type)) {
                        case 4 * WORD_SIZE: // all words important
                            pop(rs[j--]);pop(rs[j--]);pop(rs[j--]);pop(rs[j--]);
                            break;
                        case 3 * WORD_SIZE: // 1st word garbage
                            popd();pop(rs[j--]);pop(rs[j--]);pop(rs[j--]);
                            break;
                        case 2 * WORD_SIZE: // both words important again
                            pop(rs[j--]);pop(rs[j--]);
                            break;
                        case 1 * WORD_SIZE:
                            popd();pop(rs[j--]);
                            break;
                        case 0:
                            break;
                        default: assert(0 && "TODO: any size tuple"); break;
                    }
                }
            }
            break;
        }
        case EXTERN_EXPR: // it's quite similar...
        {
            const VarEx varx = variable_table[expr->ext.var_id];
            load_label(r0, expr->ext.external_name);
            store(varx.stack_offset, bp, r0);

            gen_stack_machine_code(expr->ext.subexpr);
            break;
        }
        case MATCH_EXPR:
        {
            int match_type_size = stack_size_of_type(expr->matchexpr->type);
            gen_stack_machine_code(expr->matchexpr);
            // TODO: deal with > 3 words
            assert(stack_size_of_type(expr->matchexpr->type) <= 3 * WORD_SIZE);
            // Save expression value somewhere safe
            if (match_type_size > 2 * WORD_SIZE) {
                push4(cs0, cs1, cs2, cs3);
            } else {
                push2(cs0, cs1);
            }
            switch (match_type_size) {
              case 3 * WORD_SIZE: mov(cs2, r2);
              case 2 * WORD_SIZE: mov(cs1, r1);
              case 1 * WORD_SIZE: mov(cs0, r0);
                                  break;
              default:
                assert(0 && "TODO: handle larger sizes");
            }
            int end_of_match = request_label();
            for (CaseList* k = expr->cases; k; k = k->next) {
                if (k != expr->cases) {
                    // Restore the value of the matchexpr
                    switch (match_type_size) {
                      case 3 * WORD_SIZE: mov(r2, cs2);
                      case 2 * WORD_SIZE: mov(r1, cs1);
                      case 1 * WORD_SIZE: mov(r0, cs0);
                                          break;
                      default:
                        assert(0 && "TODO: handle larger sizes");
                        break;
                    }
                }

                gen_sm_unapply_pat(k->kase->pattern); // leaves 1 if matched
                cmp_imm(r0, 0LL);
                int end_of_this_case = request_label();
                beq(end_of_this_case);
                { // true
                    gen_stack_machine_code(k->kase->expr);
                }
                b(end_of_match);
                label(end_of_this_case);
            }
            // Fell through all cases - match failure
            exit_imm(99); // TODO: call write "match failure"

            label(end_of_match);
            // Restore scratched registers
            if (match_type_size > 2 * WORD_SIZE) {
                pop4(cs0, cs1, cs2, cs3);
            } else {
                pop2(cs0, cs1);
            }
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
#elif defined(__linux__) && defined(__arm__)
    fputs("\
.section .interp\n\
	.string \"/lib/ld-linux-armhf.so.3\"\n\
.text\n\
.global _start\n\
_start:\n\
	mov		fp, sp		@ initialize frame pointer\n\
	bl		start__0	@ will leave exit code in r0\n\
	mov		r7, #1\n\
	swi		0\n\
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

        for (int i = 0; i < type_sizes_count; i++) {
            if (type->name == type_sizes[i].name)
                return type_sizes[i].size;
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
              return WORD_SIZE; // Just pointer to first node
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
          int size = 0;
          for (TypeExprList* l = type->type_list; l; l = l->next) {
              // stack-based tuples - add them together
              size += stack_size_of_type(l->type);
          }
          return size;
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
        add_var_to_locals(curr_func, pat->name, pat->type, NULL);
        pat->var_id = (var_stack_ptr - 1)->var_id;
        break;
      case PAT_DISCARD:
        break; // Do nothing...
      case PAT_CONS:
        calculate_activation_records_pat(pat->left, curr_func);
        calculate_activation_records_pat(pat->right, curr_func);
        break;
      case PAT_TUPLE:
        for (PatternList* l = pat->pat_list; l; l = l->next) {
            calculate_activation_records_pat(l->pattern, curr_func);
        }
        break;
      case PAT_CTOR_WARG:
        calculate_activation_records_pat(pat->ctor_arg, curr_func);
        break;
      case PAT_CTOR_NOARG: /* fal through */
      case PAT_INT:
      case PAT_STR:
      case PAT_NIL: break;
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
            expr->enclosing_func_id = fn_table_count - 1;
        } else {
            // Tag the var with the var_id
            expr->var_id = var->var_id;
        }
        break;
      }
      case DIRECT_CALL:
      {
        Var* var = lookup_var_in_func(curr_func, expr->funcname);
        if (!var) {
            // We only need to stick ourselves in the closure if we (funcname)
            // close over anything - fixme
            Var* var2 = lookup_var(expr->funcname, var_stack_ptr);
            if (!var2) {
                fprintf(stderr, "failed to find %s\n", expr->funcname);
            }
            assert(var2 && "direct call var not on var stack");

            add_var_to_closure(curr_func, expr->funcname, var2->type);
            expr->dc_closingfunc_id = fn_table_count - 1;
            expr->dc_func_id = (var2->function - function_table);
        } else {
            expr->dc_var_id = var->var_id;
            expr->dc_func_id = (var->function - function_table);
        }

        for (ExprList* l = expr->args; l; l = l->next) {
            calculate_activation_records_expr(l->expr, curr_func);
        }
        break;
      }
      case UNITVAL:
      case INTVAL:
      case STRVAL:
        break;
      case RECFUNC_EXPR: /* TODO make me different */
      case FUNC_EXPR:
      {
        Var* pre_func_name_stack_ptr = var_stack_ptr;
        if (expr->tag == RECFUNC_EXPR) {
            add_var_to_locals(curr_func, expr->func.name, expr->func.functype,
                    function_table + fn_table_count);
            expr->func.var_id = (var_stack_ptr - 1)->var_id;
        }
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
            add_var_to_locals(curr_func, param->name, param->type, curr_func);
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

        if (expr->tag != RECFUNC_EXPR) {
            add_var_to_locals(curr_func, expr->func.name, expr->func.functype,
                    function_table + expr->func.function_id);
            expr->func.var_id = (var_stack_ptr - 1)->var_id;
        }

        // recurse
        calculate_activation_records_expr(expr->func.subexpr, curr_func);

        var_stack_ptr = pre_func_name_stack_ptr;
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
        add_var_to_locals(curr_func, expr->ext.name, expr->ext.functype, NULL);
        expr->ext.var_id = (var_stack_ptr - 1)->var_id;
        calculate_activation_records_expr(expr->ext.subexpr, curr_func);
        var_stack_ptr = saved_stack_ptr;
        break;
      }
      case MATCH_EXPR:
      {
        calculate_activation_records_expr(expr->matchexpr, curr_func);
        for (CaseList* k = expr->cases; k; k = k->next) {
            Var* saved_stack_ptr = var_stack_ptr;
            // modifies var_stack_ptr
            calculate_activation_records_pat(k->kase->pattern, curr_func);
            calculate_activation_records_expr(k->kase->expr, curr_func);
            var_stack_ptr = saved_stack_ptr;
        }
        break;
      }
    }
}

/*
 * Replace (apply (var x) (...))
 * With (direct-call x (...))
 */
static void patch_in_direct_calls(Expr* expr, Var* var_stack_ptr)
{
    switch (expr->tag) {
      case PLUS:
      case MINUS:
      case MULTIPLY:
      case DIVIDE:
      case LESSTHAN:
      case LESSEQUAL:
      case EQUAL:
        patch_in_direct_calls(expr->left, var_stack_ptr);
        patch_in_direct_calls(expr->right, var_stack_ptr);
        break;
      case APPLY:
      {
        patch_in_direct_calls(expr->left, var_stack_ptr);
        patch_in_direct_calls(expr->right, var_stack_ptr);

        if (expr->left->tag == VAR) {
            // left side is a name, perhaps
            Var* var = lookup_var(expr->left->var, var_stack_ptr);
            if (var && var->arity > 0) {
                Expr* right = expr->right; // save a copy of our info
                Expr* left = expr->left;
                expr->tag = DIRECT_CALL;
                expr->funcname = left->var;
                expr->dc_var_id = left->var_id; // These shouldn't have
                // actually been set yet...
                expr->dc_closingfunc_id = left->enclosing_func_id;
                expr->args = add_expr(exprlist(), right);
                expr->dc_func_id = -1; // we don't know yet
                // type will stay the same
            }
        } else if (expr->left->tag == DIRECT_CALL) {
            // Perhaps we can do something here too?
            if (debug_codegen) {
                fprintf(stderr, "left side of apply was direct call to %s",
                        expr->left->funcname);
            }
            int left_call_arity = 0;
            for (ExprList* l = expr->left->args; l; l = l->next) {
                left_call_arity++;
            }
            Var* var = lookup_var(expr->left->funcname, var_stack_ptr);
            if (var && var->arity > left_call_arity) {
                // In this case we could apply to more args
                if (debug_codegen) {
                    fprintf(stderr, "direct call to %s is not fully applied",
                            expr->left->funcname);
                }
            }
        }
        break;
      }
      case VAR:
      case UNITVAL:
      case INTVAL:
      case STRVAL:
        break;
      case RECFUNC_EXPR:
      {
        var_stack_ptr = push_var2(
                expr->func.name, expr->func.functype, var_stack_ptr, 1);
        Param* param = expr->func.params->param;
        Var* sp_for_body = push_var2(param->name, param->type, var_stack_ptr, 1);
        patch_in_direct_calls(expr->func.body, sp_for_body);
        patch_in_direct_calls(expr->func.subexpr, var_stack_ptr);
        break;
      }
      case FUNC_EXPR:
      {
        Param* param = expr->func.params->param;
        Var* sp_for_body = push_var2(param->name, param->type, var_stack_ptr, 1);
        patch_in_direct_calls(expr->func.body, sp_for_body);

        var_stack_ptr = push_var2(
                expr->func.name, expr->func.functype, var_stack_ptr, 1);
        patch_in_direct_calls(expr->func.subexpr, var_stack_ptr);
        break;
      }
      case BIND_EXPR:
        patch_in_direct_calls(expr->binding.init, var_stack_ptr);
        patch_in_direct_calls(expr->binding.subexpr, var_stack_ptr);
        break;
      case IF_EXPR:
        patch_in_direct_calls(expr->condition, var_stack_ptr);
        patch_in_direct_calls(expr->btrue, var_stack_ptr);
        patch_in_direct_calls(expr->bfalse, var_stack_ptr);
        break;
      case LIST:
      case VECTOR:
      case TUPLE:
        for (ExprList* l = expr->expr_list; l; l = l->next) {
            patch_in_direct_calls(l->expr, var_stack_ptr);
        }
        break;
      case EXTERN_EXPR:
        patch_in_direct_calls(expr->ext.subexpr, push_var2(
                    expr->ext.name, expr->ext.functype, var_stack_ptr, 1));
        break;
      case MATCH_EXPR:
        patch_in_direct_calls(expr->matchexpr, var_stack_ptr);
        for (CaseList* k = expr->cases; k; k = k->next) {
            patch_in_direct_calls(k->kase->expr, var_stack_ptr);
        }
        break;
      case DIRECT_CALL:
        assert(0 && "should not exist in tree yet... but may?");
        for (ExprList* l = expr->args; l; l = l->next) {
            patch_in_direct_calls(l->expr, var_stack_ptr);
        }
        break;
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
      case RECFUNC_EXPR:
        // outer-most function needs to be recursive  inner don't
        // fall-through to normal func
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
        break;
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
      case MATCH_EXPR:
      {
        rewrite_functions(expr->matchexpr);
        for (CaseList* k = expr->cases; k; k = k->next) {
            rewrite_functions(k->kase->expr);
        }
        break;
      }
      case DIRECT_CALL:
      {
        assert(0 && "should not exist in tree yet");
        break;
      }
    }
}

static Expr* write_constructor_functions(
        Symbol tname, int idx, CtorList* list, DeclarationList* root_next);
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
static Expr* restructure_tree(DeclarationList* root)
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
            Expr* newnode = local_func_w_type(
                    decl->func.name,
                    decl->func.params,
                    decl->func.resulttype,
                    decl->func.body,
                    restructure_tree(root->next));
            newnode->type = newnode->func.subexpr->type;
            newnode->func.functype = decl->func.type;
            return newnode;
          }
          case DECL_RECFUNC:
          {
            Expr* newnode = local_recfunc_w_type(
                    decl->func.name,
                    decl->func.params,
                    decl->func.resulttype,
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
          case DECL_TYPECTOR:
          {
            // Here we want to define functions that make up the constructors
            // I think....
            return write_constructor_functions(
                    decl->ctor.name, 0, decl->ctor.ctors, root->next);
          }
        }
    }
    abort();
}

static Expr* write_constructor_functions(
        Symbol tname, int idx, CtorList* list, DeclarationList* root_next)
{
    if (!list) {
        return restructure_tree(root_next);
    } else {
        Ctor* ctor = list->ctor;
        switch (ctor->tag) {
          case CTOR_NOARG:
          {
            Pattern* pattern = pat_var(ctor->name);
            Expr* init = intval(idx);
            pattern->type =
                init->type = typename(tname);  // Should we ?!
            Expr* newnode = local_binding(pattern, init,
                    write_constructor_functions(
                        tname, idx + 1, list->next, root_next));
            newnode->type = newnode->binding.subexpr->type;
            return newnode;
          }
          case CTOR_WARG:
          {

            // create a constructor function that looks like
            // let f _1 = (idx, _1)
            Expr* body = tuple(
                    add_expr(
                        add_expr(exprlist(), var(symbol("_1"))),
                        intval(idx)));
            body->expr_list->expr->type = typename(symbol("int"));
            body->expr_list->next->expr->type = ctor->typexpr;

            Expr* newnode = local_func_w_type(
                    ctor->name,
                    param_list(param_with_type(symbol("_1"), ctor->typexpr)),
                    typename(tname),
                    body,
                    write_constructor_functions(
                        tname, idx + 1, list->next, root_next));
            body->type = newnode->func.resulttype;
            newnode->func.functype = typearrow(ctor->typexpr, body->type);
            newnode->type = newnode->func.subexpr->type;
            return newnode;
          }
        }
    }
}

static void add_custom_type_sizes(DeclarationList* root)
{
    for (DeclarationList* l = root; l; l = l->next) {
        Declaration* decl = l->declaration;
        switch (decl->tag) {
          case DECL_FUNC:
          case DECL_RECFUNC:
          case DECL_BIND:
          case DECL_EXTERN:
            // These don't declare a type
            break;
          case DECL_TYPE:
            add_type_size(
                    decl->type.name,
                    stack_size_of_type(decl->type.definition));
            break;
          case DECL_TYPECTOR:
          {
            int size_of_tag = stack_size_of_type(typename(symbol("int")));
            // The size of the held data is the max over the sizes from each
            // constructor
            int size_of_data = 0;
            for (CtorList* l = decl->ctor.ctors; l; l = l->next) {
                Ctor* ctor = l->ctor;
                switch (ctor->tag) {
                  case CTOR_NOARG:
                    break;
                  case CTOR_WARG:
                  {
                    int ssot = stack_size_of_type(ctor->typexpr);
                    size_of_data = size_of_data > ssot ? size_of_data : ssot;
                    break;
                  }
                }
            }
            add_type_size(decl->ctor.name, size_of_tag + size_of_data);

            // keep track of these for use in patterns in the unapply
            // code
            tagged_decls[tagged_decls_count++] = decl;
            break;
          }
        }
    }
}

#ifdef _WIN32
static void unlink_dataout()
{
    _unlink(dataoutname);
}
#endif

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

    add_custom_type_sizes(root);

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

    patch_in_direct_calls(newroot, var_stack_ptr);
    if (debug_codegen) {
        fputs("patched direct calls:\n\t", stderr);
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
    #ifdef _WIN32
        // this seems to return paths starting with '\'
        dataoutname[0] = '.';
        if (tmpnam_s(dataoutname + 1, sizeof dataoutname - 1)) {
            fprintf(stderr, "creating name for temp file name for data "
                    "section\n");
            exit(EXIT_FAILURE);
        }
        if (debug_codegen) {
            fprintf(stderr, "dataoutname: %s\n", dataoutname);
        }
        dataout = fopen(dataoutname, "w+");
    #else
        dataout = tmpfile();
    #endif
    if (!dataout) {
        perror("creating a temp file for data section");
        exit(EXIT_FAILURE);
    }
    #ifdef _WIN32
        atexit(unlink_dataout);
    #endif

    emit_header();
    emit_fn_prologue(curr_func);
    gen_stack_machine_code(newroot);
    emit_fn_epilogue(curr_func);

    for (; bodies_begin < bodies_end; bodies_begin++)
    {
        emit_fn_prologue(bodies_begin->function);
        gen_stack_machine_code(bodies_begin->body);
        emit_fn_epilogue(bodies_begin->function);
    }

    fprintf(cgenout, "\n.data\n");
    fseeko(dataout, SEEK_SET, 0);
    clearerr(dataout);
    flockfile(dataout);
    flockfile(cgenout);
    int c;
    while ((c = getc_unlocked(dataout)) != EOF) {
        putc_unlocked(c, cgenout);
    }
    funlockfile(cgenout);
    funlockfile(dataout);
    fclose(dataout);
}

