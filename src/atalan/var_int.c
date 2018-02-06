/*
Integer constant support

Integer constants are represented as INSTR_INT cells.
They are kept in their own private scope.

(c) 2010 Rudolf Kudla 
Licensed under the MIT license: http://www.opensource.org/licenses/mit-license.php

*/

#include "language.h"

GLOBAL Var * ZERO;
GLOBAL Var * ONE;
GLOBAL Var * INTS[256];	// 0..255 integer constants (for faster access)
GLOBAL Var INT_VAR;

Var * VarInt(Int32  n)
{
	Var * var;
	BigInt bi;
	IntInit(&bi, n);
	var = VarN(&bi);
	IntFree(&bi);
	return var;
}

Var * VarN(BigInt * n)
/*
Purpose:
	Return variable representing integer constant.
*/
{
	Var * var;

	// Try to find the integer variable in hash
//	if (n>=0 && n<=255) {
//		var = INTS[n];
//		if (var != NULL) return var;
//	}

	// Try to find the integer constant in the scope

	for(var = INT_VAR.subscope; var != NULL; var = var->next_in_scope) {
		ASSERT(var->mode == INSTR_INT);
		if (var->mode == INSTR_INT && IntEq(&var->n, n)) return var;
	}

	// Constant was not found, create new one
	var = VarAllocUnused();
	var->mode =  INSTR_INT;
	var->scope = NULL;
//	var->scope = &INT_VAR;
	VarSetScope(var, &INT_VAR);

//	var = VarAllocScope(&INT_VAR, INSTR_INT, NULL, 0);
	var->type = &TINT;						// In future, the type of the constant should be the constant itself (self reference)
	IntSet(&var->n, n);

	// Put the variable into hash
//	if (n>=0 && n<=255) {
//		INTS[n] = var;
//	}

	return var;
}

void IntConstInit()
{
	MemEmpty(&INT_VAR, sizeof(INT_VAR));
	MemEmpty(&INTS, sizeof(INTS));
	ZERO = VarInt(0);
	ONE  = VarInt(1);
}
