#ifndef __CODEGEN_H__
#define __CODEGEN_H__

#include "ast.h"


void codegen(DeclarationList* root);

extern int debug_codegen;

extern FILE* cgenout;

#endif /* __CODEGEN_H__ */
