#ifndef __TYPES_AND_VARS_H__
#define __TYPES_AND_VARS_H__

#include "ast.h"

/*
 * Annotates the tree with missing type information and detects inconsistencies
 */
void type_check_tree(DeclarationList* root);

/*
 * A flag which can be set to allow the debug information to be printed
 * whilst the type
 */
extern int debug_type_checker;

#endif /* __TYPES_AND_VARS_H__ */
