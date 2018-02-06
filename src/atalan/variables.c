/*
Table of variables

There is one global table of variables.

(c) 2010 Rudolf Kudla 
Licensed under the MIT license: http://www.opensource.org/licenses/mit-license.php

*/

#include "language.h"

GLOBAL Var * UNUSED_VARS;		    // scope in which are all the free variables
GLOBAL VarBlock * VAR_BLOCKS;       // list of all variable blocks
GLOBAL VarBlock * LAST_VAR_BLOCK;   // list of all variable blocks

GLOBAL Var * VARS;		// global variables
GLOBAL Var * LAST_VAR;  // Last allocated variable.

GLOBAL Var * SCOPE;		// current scope
GLOBAL UInt32 TMP_IDX;
GLOBAL UInt32 TMP_LBL_IDX;


GLOBAL char VAR_NAME[128];

void IntConstInit();


/*
We have theoretical option of supporting mltiple CPUs simultaneously (this the CPUS array).
Current CPU used is stored in CPU variable.
*/

CPUType CPUS[1];			// currently, there is only one supported CPU
GLOBAL CPUType * CPU;		// current CPU (in case we use multiple CPUs in the future)

char * TMP_NAME = "_";
char * TMP_LBL_NAME = "_lbl";
char * SCOPE_NAME = "_s";
UInt32 SCOPE_IDX;


VarBlock * VarBlockAlloc()
{
	Var * var, * next;
	UInt16 i;
	VarBlock * varblk;
	varblk = MemAllocStruct(VarBlock);

	// Initialize all variables in the block as unused and put them in the list of unused variables.

	UNUSED_VARS = &varblk->vars[0];
	var = UNUSED_VARS;

	for(i=1; i < VAR_BLOCK_CAPACITY; i++) {
		next = var+1;
		var->mode = INSTR_NULL;
		var->next_in_scope = next;
		var = next;
	}
	// now initialize the last variable in the block
	var->mode = INSTR_NULL;

	return varblk;
}

Var * VarAllocUnused()
{
	Var * var;
	if (UNUSED_VARS == NULL) {
		LAST_VAR_BLOCK->next = VarBlockAlloc();
		LAST_VAR_BLOCK = LAST_VAR_BLOCK->next;
	}
	var = UNUSED_VARS;
	UNUSED_VARS = var->next_in_scope;
	var->next_in_scope = NULL;
	return var;
}

void VarInit()
{

	VARS = NULL;
	LAST_VAR = NULL;

	TMP_IDX = 1;
	TMP_LBL_IDX = 0;
	SCOPE_IDX = 0;

	VAR_BLOCKS = LAST_VAR_BLOCK = VarBlockAlloc();

	// Alloc rule procedure and rule arguments (rule arguments are local arguments of RULE_PROC)

	CPU = &CPUS[0];

	IntConstInit();


}

void VarSetScope(Var * var, Var * scope)
{
	Var * sub;

	if (var->scope != NULL) {
		InternalError("Variable already has the scope set");
	}

	var->scope = scope;

	if (scope == NULL) return;

	//==== Append as last variable in scope

	if (scope->subscope == NULL) {
		scope->subscope = var;
	} else {
		// We append the variable as last variable in the scope.
		for(sub = scope->subscope; sub->next_in_scope != NULL;) {
			sub = sub->next_in_scope;
		}
		sub->next_in_scope = var;
	}


}

Var * VarFindOp(InstrOp op, Var * left, Var * right)
/*
Purpose:
	Find variable created as combination of two other variables.
Argument:
	ref		Array is accessed using reference.
*/
{
	Var * var;
	for (var = VARS; var != NULL; var = var->next) {
		if (var->mode == op && var->adr == left && var->var == right) return var;
	}
	return NULL;
}

Var * VarNewRange(Var * min, Var * max)
{
	Var * var = VarNewOp(INSTR_RANGE, min, max);

	return var;
}

Var * VarNewTuple(Var * left, Var * right)
/*
Purpose:
	Create new tuple from the two variables.
	If the right variable is NULL, left is returned.
*/
{
	Var * var;

	if (right == NULL) return left;
	if (left == NULL) return right;

	var = VarFindOp(INSTR_TUPLE, left, right);
	if (var == NULL) {
		var = VarAllocScope(NO_SCOPE, INSTR_TUPLE, NULL, 0);
		var->type = TypeTuple(left->type, right->type);
		var->adr = left;
		var->var = right;
	}
	return var;
}

Var * VarNewDeref(Var * adr)
{
	Var * var;
	for (var = VARS; var != NULL; var = var->next) {
		if (var->mode == INSTR_DEREF && var->var == adr) return var;
	}

	var = VarAlloc(INSTR_DEREF, NULL, 0);
	var->var = adr;
	if (adr->type != NULL && adr->type->variant == TYPE_ADR) {
		var->type = adr->type->element;
	}
	return var;
}

Var * VarNewVariant(Var * left, Var * right)
{
	if (left == NULL) return right;
	if (right == NULL) return left;
	if (right == left) return left;
	return VarNewOp(INSTR_VARIANT, left, right);
}

Var * VarNewOp(InstrOp op, Var * arr, Var * idx)
/*
Purpose:
	Alloc new reference to array element.
Argument:
	ref		Array is accessed using reference.
*/
{
	Var * item;

	// Try to find same element

	Var * var = VarFindOp(op, arr, idx);
	if (var != NULL) return var;
/*
	for (var = VARS; var != NULL; var = var->next) {
		if (var->mode == op) {
			if (var->adr == arr && var->var == idx) return var;
		}
	}
*/
	item = VarAlloc(op, NULL, 0);
//	if (ref) item->submode = SUBMODE_REF;
	item->adr  = arr;

	// Type of array element variable is type of array element
	// We may attempt to address individual bytes of non-array variable as an aray
	// in such case the type of the lement is byte.
	if (arr->type != NULL) {
		if (arr->type->variant == TYPE_ARRAY) {
			item->type = arr->type->element;
		} else if (arr->type->variant == TYPE_STRUCT) {
			item->type = idx->type;
			item->submode |= (idx->submode & (SUBMODE_IN|SUBMODE_OUT|SUBMODE_REG));
		} else {
			item->type = TypeByte();
		}
	} else {
	}
	item->var  = idx;
	// If this is element from in or out variable, it is in or out too
	item->submode |= (arr->submode & (SUBMODE_IN|SUBMODE_OUT|SUBMODE_REG));
	return item;
}

Var * VarNewElement(Var * arr, Var * idx)
{
	return VarNewOp(INSTR_ELEMENT, arr, idx);
}

Var * VarNewByteElement(Var * arr, Var * idx)
/*
Purpose:
	Alloc new reference to specified byte of variable.
Argument:
	ref		Array is accessed using reference.
*/
{	
	//TODO: VarNewByteElement may create simple integer when used with two integer constants

	Var * item = VarNewOp(INSTR_BYTE, arr, idx);
	item->type = TypeByte();
	return item;
}

Var * VarNewBitElement(Var * arr, Var * idx)
{
	Var * item = VarNewOp(INSTR_BYTE, arr, idx);
	return item;
}

Var * InScope(Var * new_scope)
{
	Var * scope;
	scope = SCOPE;
	SCOPE = new_scope;
	return scope;
}

void ReturnScope(Var * prev)
{
	SCOPE = prev;
}

void ExitScope()
{
	SCOPE = SCOPE->scope;
}

void EnterLocalScope()
{
	Var * var;
	SCOPE_IDX++;
	var = VarAllocScope(SCOPE, INSTR_SCOPE, SCOPE_NAME, SCOPE_IDX);
	var->type = TypeScope();
	SCOPE = var;
}

Var * VarFirst()
{
	return VARS;
}

Var * VarNewTmp(Type * type)
{
	Var * var;
	var = VarAllocScopeTmp(NULL, INSTR_VAR, type);
	return var;
}

Bool VarIsArray(Var * var)
{
	return var->type != NULL && var->type->variant == TYPE_ARRAY;
}

Bool VarIsTmp(Var * var)
{
	return var->name == TMP_NAME;
}

Bool VarIsStructElement(Var * var)
{
	return var->adr->type->variant == TYPE_STRUCT;
}

Bool VarIsArrayElement(Var * var)
{
	Var * adr;
	if (var == NULL || var->mode != INSTR_ELEMENT) return false;
	adr = var->adr;
//	if (adr == NULL) return false;			// TODO: ELEMENT with NULL adr?
	if (adr->mode == INSTR_DEREF) return true;
	return adr->type != NULL && adr->type->variant == TYPE_ARRAY;
//	return var != NULL && var->mode == INSTR_ELEMENT && var->adr != NULL && var->adr->type != NULL && (var->adr->type->variant == TYPE_ARRAY);
}

void VarInitStr(Var * var, char * str)
{
	var->type = &TSTR;
	var->str = StrAlloc(str);
}

Var * VarNewStr(char * str)
{
	Var * var;
	var = VarAlloc(INSTR_TEXT, NULL, 0);
	VarInitStr(var, str);
	return var;
}

Var * VarNewLabel(char * name)
{
	Var * var;
	var = VarAlloc(INSTR_VAR, name, 0);
	var->type = &TLBL;
	return var;
}

Var * FindOrAllocLabel(char * name, UInt16 idx)
{
	Var * var = NULL;
	Var * proc;
	Var * scope;

	proc = VarProcScope();
	var = VarFindScope(proc, name, idx);
	if (var == NULL) {
		scope = InScope(proc);
		var = VarNewLabel(name);
		var->idx = idx;
		ReturnScope(scope);
	}
	return var;
}


void VarToLabel(Var * var)
{
	var->type = &TLBL;
	var->mode = INSTR_VAR;
}

Var * VarNewTmpLabel()
{
	Var * var;
	TMP_LBL_IDX++;
	var = VarNewLabel(NULL);
	var->name = TMP_LBL_NAME;
	var->idx  = TMP_LBL_IDX;
	return var;
}

Var * VarAllocScope(Var * scope, InstrOp mode, char * name, VarIdx idx)
{
	Var * var;

	ASSERT(mode != INSTR_INT);

	if (scope == NULL) scope = SCOPE;
	if (scope == NO_SCOPE) scope = NULL;

	var = VarAllocUnused();

	var->mode  = mode;
	var->name  = StrAlloc(name);
	var->idx   = idx;
	var->adr   = NULL;
	var->next  = NULL;
	var->type  = TUNDEFINED;		// freshly allocated variable has undefined type (but not NULL!)

	VarSetScope(var, scope);

	if (VARS == NULL) {
		VARS = var;
	} else {
		LAST_VAR->next = var;
	}
	LAST_VAR = var;
	return var;

}

Var * VarAllocScopeTmp(Var * scope, InstrOp mode, Type * type)
/*
Purpose:
	Alloc new temporary variable in specified scope.
*/
{
	Var * var;
	var = VarAllocScope(scope, mode, NULL, 0);
	var->name = TMP_NAME;
	var->idx = TMP_IDX;
	if (type == NULL) type = TUNDEFINED;
	var->type = type;
	TMP_IDX++;
	return var;
}


Var * VarClone(Var * scope, Var * var)
{
	Var * copy;

	copy = VarAllocUnused();
	MemMove(copy, var, sizeof(Var));
	VarSetScope(var, scope);
	return copy;
}

Var * VarAlloc(InstrOp mode, char * name, VarIdx idx)
/*
Purpose:
	Alloc new variable.
*/
{
	return VarAllocScope(SCOPE, mode, name, idx);
}

Var * VarFindScope(Var * scope, char * name, VarIdx idx)
/*
Purpose:
	Find variable in specified scope.
	If scope is NULL, only global variables will be searched.
*/
{
	Var * var;
	for (var = VARS; var != NULL; var = var->next) {
		if (var->scope == scope && var->mode != INSTR_VOID) {
			if (var->idx == idx && StrEqual(name, var->name)) break;
		}
	}
	return var;
}

void PrintScope(Var * scope)
/*
Purpose:
	Find variable in specified scope, and parent scopes.
	If scope is NULL, only global variables will be searched.
*/
{
	Var * var;
	PrintFmt("===== %s =======\n", scope->name);
	for (var = VARS; var != NULL; var = var->next) {
		if (var->scope == scope) {
			PrintVar(var); PrintEOL();
		}
	}
}

Var * VarFindScope2(Var * scope, char * name)
{
	Var * var = NULL;
	Var * s;
	for (s = scope; s != NULL; s = s->scope) {
		var = VarFindScope(s, name, 0);
		if (var != NULL) break;
		// For procedures whose type has been defined using predefined type, try to find arguments from this type
		if (s->type->variant == TYPE_PROC && s->type->owner != NULL && s->type->owner != s) {
			var = VarFindScope(s->type->owner, name, 0);
			if (var != NULL) break;
		}
	}
	return var;
}

Var * VarFind2(char * name)
/*
Purpose:
	Find variable in current scope.
*/
{
	return VarFindScope2(SCOPE, name);
/*
	Var * var = NULL;
	Var * s;
	for (s = SCOPE; s != NULL; s = s->scope) {
		var = VarFindScope(s, name, 0);
		if (var != NULL) break;
	}
	return var;
*/
}

Var * VarProcScope()
{
	Var * s;
	for (s = SCOPE; s != NULL; s = s->scope) {
		if (s->type != NULL && (s->type->variant == TYPE_PROC || s->type->variant == TYPE_MACRO)) break;
	}
	return s;
}

Var * VarFind(char * name, VarIdx idx)
{
	Var * var;
	for (var = VARS; var != NULL; var = var->next) {
		if (var->idx == idx && StrEqual(name, var->name)) break;
	}
	return var;
}

Var * VarFindTypeVariant(char * name, VarIdx idx, TypeVariant type_variant)
{
	Var * var;
	for (var = VARS; var != NULL; var = var->next) {
		if (var->idx == idx && StrEqual(name, var->name) && (type_variant == TYPE_UNDEFINED || (var->type != NULL && var->type->variant == type_variant))) break;
	}
	return var;
}

TypeVariant VarType(Var * var)
{
	if (var == NULL) return TYPE_VOID;
	return var->type->variant;
}

Bool VarIsLabel(Var * var)
{
	return var->type->variant == TYPE_LABEL;
}

Bool TypeIsConst(Type * type)
{
	if (type->variant == TYPE_VAR) {
		return VarIsConst(type->typevar);
	}
	return false;
}

Bool VarIsConst(Var * var)
{
	if (var == NULL) return false;
	return var->mode == INSTR_INT || var->mode == INSTR_CONST || var->mode == INSTR_TEXT || TypeIsConst(var->type);
}

Bool VarIsIntConst(Var * var)
{
	return var != NULL && (var->mode == INSTR_INT || (var->mode == INSTR_CONST && TypeIsInt(var->type)));
}

BigInt * VarIntConst(Var * var)
/*
Purpose:
	Return integer value represented by the variable, if it is constant.
*/
{
	if (var == NULL) return NULL;

	// For named const, work with it's referenced value
	while(var->mode == INSTR_CONST && var->type->variant == TYPE_INT && var->var != NULL) var = var->var;

	if (var->mode == INSTR_INT) return &var->n;
	if (TypeIsIntConst(var->type)) return &var->type->range.min;

	return NULL;
}


Bool VarIsN(Var * var, Int32 n)
{
	BigInt * i;

	i = VarIntConst(var);
	return i != NULL && IntEqN(i, n);
}

Var * VarNewType(Type * type)
{
	Var * var;
	var = VarAlloc(INSTR_VAR, NULL, 0);
	var->type = TypeType(NULL);
	var->type_value = type;
	return var;
}

Bool VarIsType(Var * var)
{
	return var != NULL && (var->mode == INSTR_TYPE || var->type->variant == TYPE_TYPE);
}

Bool VarIsUsed(Var * var)
{
	return var != NULL && (var->read > 0 || var->write > 0);
}

void VarEmitAlloc()
/*
Purpose:	
	Emit instructions allocating variables, that are not placed at specific location.
*/
{
	Var * var, *cnst, * type_var;
	Type * type;
	UInt32 size;	//, i;
	Var * dim1, * dim2;

	// Generate empty arrays

	FOR_EACH_VAR(var)
		type = var->type;
		if (type != NULL) {
			if (var->mode == INSTR_VAR && type->variant == TYPE_ARRAY && var->adr == NULL && var->instr == NULL) {

				if (VarIsUsed(var)) {
					
					size = TypeSize(type);

					// Make array aligned (it type defines address, it is definition of alignment)
					type_var = type->owner;
					if (type_var->adr != NULL) {
						EmitInstrOp(INSTR_ALIGN, NULL, type_var->adr, NULL);
					}

					ArraySize(type, &dim1, &dim2);

					if (dim2 != NULL) {
						EmitInstrOp(INSTR_LABEL, var, NULL, NULL);		// use the variable as label - this will set the address part of the variable
						EmitInstrOp(INSTR_ALLOC, var, dim1, dim2);
					} else {
						cnst = VarInt(size);
						EmitInstrOp(INSTR_LABEL, var, NULL, NULL);		// use the variable as label - this will set the address part of the variable
						EmitInstrOp(INSTR_ALLOC, var, cnst, NULL);
					}
				}
			}
		}
	NEXT_VAR
}

void VarGenerateArrays()
/*
Purpose:
	Generate instructions allocating space for arrays.
*/
{
	Var * var, *type_var;
	Type * type;
	Rule * rule;

	// Generate initialized arrays, where location is not specified

	FOR_EACH_VAR(var)
		type = var->type;
		if (type->variant == TYPE_ARRAY) {
			if ((var->mode == INSTR_VAR || var->mode == INSTR_CONST) && var->instr != NULL && var->adr == NULL) {		
				if (VarIsUsed(var)) {
					// Make array aligned (it type defines address, it is definition of alignment)
					type_var = type->owner;
					if (type_var->adr != NULL) {
						rule = InstrRule2(INSTR_ALIGN, NULL, type_var->adr, NULL);
						GenRule(rule, NULL, type_var->adr, NULL);
					}
					// Label & initializers
					GenLabel(var);
					GenBlock(var->instr);
					var->instr = NULL;
				}
			}
		}
	NEXT_VAR

	// Generate array indexes

	FOR_EACH_VAR(var)
		if (var->mode == INSTR_VAR /*|| var->mode == INSTR_INT*/) {
			type = var->type;
			if (type != NULL && type->variant == TYPE_ARRAY) {
				if (VarIsUsed(var)) {
					// If there is an index rule for the array, generate the index
					rule = InstrRule2(INSTR_ARRAY_INDEX, NULL, var, NULL);
					if (rule != NULL) {
						GenRule(rule, NULL, var, NULL);
					}
				}
			}
		}
	NEXT_VAR

	// Generate initialized arrays at specified addresses

	FOR_EACH_VAR(var)
		type = var->type;
		if (type->variant == TYPE_ARRAY) {
			if ((var->mode == INSTR_VAR || var->mode == INSTR_CONST) && var->instr != NULL && var->adr != NULL && VarIsUsed(var)) {
				rule = InstrRule2(INSTR_ORG, NULL, var->adr, NULL);
				GenRule(rule, NULL, var->adr, NULL);
				GenLabel(var);
				GenBlock(var->instr);
			}
		}
	NEXT_VAR

}

void VarResetUse()
{
	Var * var;

	FOR_EACH_VAR(var)
		if (var->type->variant != TYPE_PROC) {
			var->read = 0;
			var->write = 0;
			var->flags = var->flags & (~(VarUninitialized|VarLoop|VarLoopDependent));
		}
	NEXT_VAR
}

void InitCPU()
/*
Purpose:
	Initialize array of registers.
	All variables that have adress in REGSET are registers.
*/
{
	Var * var;

	if (CPU->MEMORY == NULL) {
		CPU->REG_CNT = 0;

		if (CPU->SCOPE == NULL) {
			InternalError("CPU scope not found");
		}

		FOR_EACH_LOCAL(CPU->SCOPE, var)
			// Only variables without address are registers.
			// The variables with address are register sets.
			if (var->mode == INSTR_VAR && var->adr == NULL && var->type->variant == TYPE_INT) {
				SetFlagOn(var->submode, SUBMODE_REG);
				CPU->REG[CPU->REG_CNT++] = var;
			}
		NEXT_LOCAL

		var = VarFindScope(CPU->SCOPE, "memory", 0);
		if (var != NULL) {
			CPU->MEMORY = var->type;
		} else {
			InternalError("CPU.memory was not defined");
		}
	}
}

void VarResetRegUse()
{
	RegIdx i;
	for(i = 0; i<CPU->REG_CNT; i++) {
		SetFlagOff(CPU->REG[i]->flags, VarUsed);
	}
}

Bool VarIsReg(Var * var)
/*
Purpose:
	Return true, if this variable is register or is stored in register(s).	
*/
{
	if (var == NULL) return false;

	if (var->mode == INSTR_VAR) {
		if (FlagOn(var->submode, SUBMODE_REG)) return true;
		return VarIsReg(var->adr);		// variable address may be register
	} else if (var->mode == INSTR_TUPLE) {
		return VarIsReg(var->adr) || VarIsReg(var->var);
	}
	return false;
}

void VarCount(Var * var, BigInt * cnt)
{
	BigInt * min, * max;
	Type * type;

	if (var == NULL) return;

	if (var->mode == INSTR_INT) {
		IntInit(cnt, 1);
	}
	if (var->mode == INSTR_RANGE) {
		VarRange(var, &min, &max);
		IntSub(cnt, max, min);
		IntAddN(cnt, 1);
	}

	type = var->type;
	if (type->variant == TYPE_INT) {
		max = TypeMax(type);
		min = TypeMin(type);
		IntSub(cnt, max, min);
		IntAddN(cnt, 1);
	}
}

UInt32 VarByteSize(Var * var)
/*
Purpose:
	Return size of variable in bytes.
*/
{
	Type * type;
	if (var != NULL) {
		type = var->type;
		if (var->mode == INSTR_ELEMENT) {
			return 1;		//TODO: Compute size in a better way
		} else if (var->mode == INSTR_BYTE) {
			return 1;
		} else if (var->mode == INSTR_INT) {
			return IntByteSize(&var->n);
		} else if (var->mode == INSTR_TEXT) {
			return StrLen(var->str);
		}
		return TypeSize(type);
	}
	return 0;
}

Bool ProcIsInterrupt(Var * proc)
{
	Type * base;
	base = proc->type;
	if (base != NULL) {
		while(
			base->base != NULL) base = base->base;
		if (base->owner == INTERRUPT) return true;
	}
	return false;
}

void ProcUse(Var * proc, UInt8 flag)
/*
Purpose:
	Mark all used procedures starting with specified root procedure.
Arguments:
	flag		VarUsedInInterupt  This procedure is used from interrupt routine
				VarProcAddress	  Address of this procedure is used
*/
{
	Instr * i;
	InstrBlock * blk;
	Var * var, * label;
	UInt16 bmk;
	Loc loc;
	UInt16 n;

	loc.proc = proc;

	proc->read++;

	if (proc->instr == NULL) return;
	if (FlagOn(proc->flags, VarProcessed)) return;

//	PrintProc(proc);

	SetFlagOn(proc->flags, flag | VarProcessed);

	if (ProcIsInterrupt(proc)) {
		flag |= VarUsedInInterupt;
	}

	// Mark all defined labels (those defined with label instruction)

	for(blk = proc->instr; blk != NULL; blk = blk->next) {
		if (blk->label != NULL) {
			SetFlagOn(blk->label->flags, VarLabelDefined);
		}
		for(i = blk->first; i != NULL; i = i->next) {
			if (i->op == INSTR_LABEL) {
				SetFlagOn(i->result->flags, VarLabelDefined);
			}
		}
	}

	for(blk = proc->instr; blk != NULL; blk = blk->next) {
		for(i = blk->first, n=1; i != NULL; i = i->next, n++) {
			if (i->op == INSTR_CALL) {
				ProcUse(i->result, flag);
//				// Procedure has side-effect, if it call a procedure with side effect
//					if (FlagOn(i->result->submode, SUBMODE_OUT)) {
//					SetFlagOn(proc->submode, SUBMODE_OUT);
//				}
			} else {
				if (i->op != INSTR_LINE) {
					if (VarType(i->arg1) == TYPE_PROC) {
						ProcUse(i->arg1, flag | VarProcAddress);
					}
					if (VarType(i->arg2) == TYPE_PROC) {
						ProcUse(i->arg2, flag | VarProcAddress);
					}
					label = i->result;
					if (label != NULL && label->type->variant == TYPE_LABEL) {
						if (FlagOff(label->flags, VarLabelDefined)) {

							loc.blk = blk;
							loc.i   = i;
							bmk = SetBookmarkLine(&loc);

							var = VarFindTypeVariant(label->name, label->idx, TYPE_LABEL);
							if (var != NULL) {
								ErrArg(var);
								SyntaxErrorBmk("Label [A] is defined in other procedure.\nIt is not possible to jump between procedures.", bmk);								
							} else {
								SyntaxErrorBmk("Label [A] is undefined", bmk);
							}
						}
					}
				}
			}
		}
	}
	SetFlagOff(proc->flags, VarProcessed);
}

Var * VarReplaceVar(Var * var, Var * from, Var * to)
/*
Purpose:
	Replace one variable by another.
	Variable may be used for example in array indexes, tuples etc.
	Replacement is performed 'in place'.
*/
{
	Var * l, * r;
	if (var == NULL) return NULL;
	if (var == from) return to;

	if (var->mode == INSTR_ELEMENT || var->mode == INSTR_TUPLE || var->mode == INSTR_RANGE) {
		l = VarReplaceVar(var->adr, from, to);
		r = VarReplaceVar(var->var, from, to);
		if (l != var->adr || r != var->var) var = VarNewOp(var->mode, l, r);

	} else if (var->mode == INSTR_DEREF) {
		l = VarReplaceVar(var->var, from, to);
		if (l != var->var) var = VarNewDeref(l);
	}
	return var;
}

void InstrBlockReplaceVar(InstrBlock * block, Var * from, Var * to)
/*
Purpose:
	Replace use of variable 'from' with variable 'to' in specified block.
	When this procedure ends, 'from' variable is no more referenced in the block.
*/
{
	Instr * i;
	InstrBlock * nb;
	for(nb = block; nb != NULL; nb = nb->next) {
		for (i = nb->first; i != NULL; i = i->next) {
			if (i->op != INSTR_LINE) {
				i->result = VarReplaceVar(i->result, from, to);
				i->arg1   = VarReplaceVar(i->arg1, from, to);
				i->arg2   = VarReplaceVar(i->arg2, from, to);
			}
		}
	}
}

void ProcReplaceVar(Var * proc, Var * from, Var * to)
/*
Purpose:
	Replace use of specific variable in a procedure by other variable.
*/
{
	InstrBlock * blk;

	for(blk = proc->instr; blk != NULL; blk = blk->next) {
		InstrBlockReplaceVar(blk, from, to);
	}
}

Bool VarIsFixed(Var * var)
/*
Purpose:
	Test, that variable is on fixed location.
	This means normal variable, argument or reference to array element with constant index.
*/
{
	if (var->mode == INSTR_VAR) return true;
	if (var->mode == INSTR_ELEMENT && var->var->mode == INSTR_INT) return true;		// access to constant array element
	return false;
}

Bool VarModifiesVar(Var * var, Var * test_var)
{
	Bool uses = false;
	if (var != NULL && test_var != NULL) {
		if (var->mode == INSTR_INT || test_var->mode == INSTR_INT) return false;

		if (var == test_var) {
			uses = true;
		} else {
			
			if (test_var->mode == INSTR_TUPLE) {
				return VarModifiesVar(var, test_var->adr) || VarModifiesVar(var, test_var->var);
			} else if (test_var->mode == INSTR_VAR && test_var->adr != NULL) {
				return VarModifiesVar(var, test_var->adr);
			}

			if (var->mode == INSTR_TUPLE) {
				return VarModifiesVar(var->adr, test_var) || VarModifiesVar(var->var, test_var);
			}

//			if (var->mode == INSTR_DEREF) {
//				uses = VarUsesVar(var->var, test_var);
//			} else if (var->mode == INSTR_ELEMENT || var->mode == INSTR_BYTE || var->mode == INSTR_TUPLE) {
//				uses = VarUsesVar(var->var, test_var) || VarUsesVar(var->adr, test_var);
//			} else if (var->adr != NULL) {
//				if (var->adr->mode != INSTR_INT) {
//					return VarUsesVar(var->adr, test_var);
//				}
//			}
		}
	}
	return uses;	
}

Bool VarUsesVar(Var * var, Var * test_var)
/*
Purpose:
	Return true, if the specified variable uses tested variable.
	It may either be same, or use the tested variable as index into array etc.
*/
{
	Bool uses = false;
	if (var != NULL && test_var != NULL) {

		if (test_var->mode == INSTR_INT) return false;

		if (var == test_var) {
			return true;
		} else {

			InstrInfo * ii = &INSTR_INFO[var->mode];

//			if (test_var->mode == INSTR_TUPLE) {
//				return VarUsesVar(var, test_var->adr) || VarUsesVar(var, test_var->var);
//			} else {

			if (var->mode == INSTR_DEREF) {
				return VarUsesVar(var->var, test_var);
			} else if (var->mode == INSTR_VAR) {
				if (var->adr != NULL) {
					if (var->adr->mode != INSTR_INT) {
						return VarUsesVar(var->adr, test_var);
					}
				}
			} else {
				if (ii->arg_type[1] == TYPE_ANY) {
					if (VarUsesVar(var->adr, test_var)) return true;
				}

				if (ii->arg_type[2] == TYPE_ANY) {
					if (VarUsesVar(var->var, test_var)) return true;
				}

//				} else if (var->mode == INSTR_ELEMENT || var->mode == INSTR_BYTE || var->mode == INSTR_TUPLE) {
//					return VarUsesVar(var->var, test_var) || VarUsesVar(var->adr, test_var);
			}
		}
		if (test_var->mode == INSTR_VAR && test_var->adr != NULL) {
			return VarUsesVar(var, test_var->adr);
		}
	}
	return false;	
}

Var * VarReg(Var * var)
/*
Purpose:
	Return register that is aliased by this variable or NULL.
*/
{
	Var * reg;

	reg = var;
	while(reg != NULL && reg->mode == INSTR_VAR) {
		if (FlagOn(reg->submode, SUBMODE_REG)) return reg;
		reg = reg->adr;
		if (reg != NULL && reg->mode == INSTR_TUPLE) return reg;
	}
	return var;
}

Bool VarIsLocal(Var * var, Var * scope)
{
	while (var != NULL) {
		if (var->scope == scope) return true;
		var = var->scope;
	}
	return false;
}

Var * VarNextLocal(Var * scope, Var * local)
{
	while(true) {
		local = local->next;
		if (local == NULL) break;
		if (local->scope == scope) break;
	}
	return local;
}

Var * VarFirstLocal(Var * scope)
{
	return VarNextLocal(scope, VARS);
}

Var * NextArg(Var * proc, Var * arg, VarSubmode submode)
{
	Var * var = arg->next;
	while(var != NULL && (var->mode != INSTR_VAR || var->scope != arg->scope || FlagOff(var->submode, submode))) var = var->next;
	return var;
}

Var * FirstArg(Var * proc, VarSubmode submode)
{
	Var * var;
	Var * owner;

	// We may call the procedure using address stored in a variable, but we need to parse the arguments using
	// procedure itself.
	if (proc->type->variant == TYPE_ADR) {
		proc = proc->type->element->owner;
	}

	// If this procedure type is defined using shared definition, use the definition to fing the arguments

	owner = proc->type->owner;
	if (owner != NULL && owner != proc) {
		proc = owner;
	}
	var = proc->next;
	while(var != NULL && (var->mode != INSTR_VAR || var->scope != proc || FlagOff(var->submode, submode))) var = var->next;
	return var;
}


Var * VarField(Var * var, char * fld_name)
/*
Purpose:
	Return property of variable.
	Following properties are supported:

	min
	max
	step
*/
{
	Var * fld = NULL;
	Type * type;
	TypeVariant vtype;

	type = var->type;
	vtype = type->variant;

	if (vtype == TYPE_INT) {
		if (StrEqual(fld_name, "min")) {
			fld = VarN(&type->range.min);
		} else if (StrEqual(fld_name, "max")) {
			fld = VarN(&type->range.max);
		}
	} else if (vtype == TYPE_ARRAY) {
		if (StrEqual(fld_name, "step")) {
			fld = VarInt(type->step);
		}
	}

	if (fld == NULL) {
		fld = VarFindScope(var, fld_name, 0);
	}
	return fld;
}

Var * VarEvalConst(Var * var)
/*
Purpose:
	Evaluate the variable, trying to make constant of it.
	Return the same variable, if no evaluating is possible.
*/
{
	Var * res = var;
	BigInt * a, * idx;
	BigInt t1, t2;

	if (res != NULL) {

		// Get the most elementary constant out of the variable

		while (res->mode == INSTR_CONST && res->type->variant != TYPE_ARRAY && res->var != NULL) {
			res = res->var;
		}

		// Getting n-th byte is same as special variant of AND
		if (res->mode == INSTR_BYTE) {
			a   = VarIntConst(var->adr);
			idx = VarIntConst(var->var);

			if (a != NULL && idx != NULL) {
				// t3 = (a >> (idx * 8)) & 0xff;
				IntSet(&t1, idx);
				IntMulN(&t1, 8);
				IntShr(&t2, a, &t1);
				IntAndN(&t2, 0xff);

				//a = (a >> (idx * 8)) & 0xff;
				res = VarN(&t2);

				IntFree(&t1);
				IntFree(&t2);			
			}
		}
	}
	return res;
}

Bool VarIsValue(Var * var)
/*
Purpose:
	Varue is value, if it directly represents numeric or text value.
*/
{
	return var != NULL && (var->mode == INSTR_INT || var->mode == INSTR_TEXT);
}

void VarLet(Var * var, Var * val)
/*
Purpose:
	Set the variable var to specified value.
	The variable must be of type INSTR_CONST or INSTR_VAR and the value is set to var property.
*/
{
	if (var != NULL && val != NULL) {
		ASSERT(var->mode == INSTR_CONST || var->mode == INSTR_VAR);

		if (VarIsValue(val)) {
			var->var = val;
		} else {
			var->var = val->var;
		}

	}
}

Bool VarIsInArg(Var * var)
{
	return FlagOn(var->submode, SUBMODE_ARG_IN);
}

Bool VarIsOutArg(Var * var)
{
	return FlagOn(var->submode, SUBMODE_ARG_OUT);
}

Bool VarIsArg(Var * var)
{
	return FlagOn(var->submode, SUBMODE_ARG_IN | SUBMODE_ARG_OUT);
}

Bool VarIsEqual(Var * left, Var * right)
{
	if (left == NULL || right == NULL) return false;
	if (left == right) return true;
	if (left->mode == INSTR_VAR && left->adr != NULL) return VarIsEqual(left->adr, right);
	if (right->mode == INSTR_VAR && right->adr != NULL) return VarIsEqual(left, right->adr);

	if (left->mode == right->mode) {
		if (left->mode == INSTR_TUPLE) {
			return VarIsEqual(left->adr, right->adr) && VarIsEqual(left->var, right->var);
		}
	}

	return false;
}

Var * VarFindAssociatedConst(Var * var, char * name)
{
	if (var == NULL) return NULL;
	return VarFindScope(var->type->owner, name, 0);
}

char * VarName(Var * var)
/*
Purpose:
	Return name of the specified variable.
	Only one name at the moment may be used.
	Next call to the function will render the name invalid.
*/
{
	VAR_NAME[0] = 0;
	if (var == NULL) return "";
	if (var->idx == 0) return var->name;
	sprintf(VAR_NAME, "%s%d", var->name, var->idx-1);
	return VAR_NAME;
}

Bool VarIsParam(Var * var)
{
	return VarIsConst(var) && FlagOn(var->submode, SUBMODE_PARAM);
}

Bool VarEq(Var * left, Var * right)
{
	BigInt * l, * r;
	if (left == right) return true;
	l = VarIntConst(left);
	r = VarIntConst(right);

	if (l != NULL && r != NULL) {
		return IntEq(l, r);
	}
	return false;
}