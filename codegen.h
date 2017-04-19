#ifndef __CODEGEN_H__
#define __CODEGEN_H__

#include "ast.h"

/*
 * Emit assembly code to cgenout for the current platform
 */
void codegen(DeclarationList* root);

/*
 * Flag to enable debug statement printing in the code generator
 */
extern int debug_codegen;

/*
 * Output file to write assembly code to
 * (would probably make more sense as an arg to codegen...)
 */
extern FILE* cgenout;

#endif /* __CODEGEN_H__ */
