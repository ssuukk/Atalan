/*

Loop shift/reversal optimization routines

(c) 2012 Rudolf Kudla 
Licensed under the MIT license: http://www.opensource.org/licenses/mit-license.php

*/

#include "language.h"

/*
There routines are performed after type inferencer made it's work.
This way, we still have the loop range information deduced by inferencer.

*/

/*
OPTIMIZATION: Pointer based loops

For loops like:

:::::::::::::::::::
for i:min..max step s
	a#i = v
:::::::::::::::::::

we may want to iterate over pointer instead of using indexed array

:::::::::::::::::::::::::
for p:a.adr+min..a.adr+max step s
	@p = v
:::::::::::::::::::::::::

This optimization may be used only in specific cases. For example on 6502, there is no need to use it when the index fits into 0..255.
The index range for which the 
It is special version of loop shift.

There are several options to stop the loop:

 1. if the top index is constant and we use the whole array, we may use constant address of the array (@a + max).
 2. address of top variable may be computed in other situations
 3. it may be useful to increment the address while decrementing the counter variable

*/


Bool VarShiftIndex(Var ** p_var, Var * idx, BigInt * shift)
/*
Purpose:
	If variable is in the form arr(idx) where idx is specified value, shift it down by specified offset.
*/
{
	Var * var_idx;
	Var * var = *p_var;

	if (var != NULL) {
		if (var->mode == INSTR_ELEMENT) {
			var_idx = var->var;
			if (VarIsEqual(var_idx, idx)) {
				idx = VarNewOp(INSTR_SUB, idx, VarN(shift));
				*p_var = VarNewElement(var->adr, idx);
				return true;
			} else if (var_idx->mode == INSTR_SUB) {
			} else if (var_idx->mode == INSTR_ADD) {
			}
		}
	}
	return false;
}

Bool VarIsIdxElem(Var * var, Var * idx)
{
	return var != NULL && var->mode == INSTR_ELEMENT && var->var == idx;
}

Var * FindPtrReplace(InstrBlock * head, InstrBlock * loop_end, Var * idx)
/*
Purpose:
	Test, that it is possible to use address instead of arr#idx in the loop.
	This means:
		- there is at least one use of arr#idx
*/{
	Var * arr = NULL;
	InstrBlock * blk;
	Instr * i;

	for(blk = head; blk != loop_end; blk = blk->next) {
		for(i = blk->first; i != NULL; i = i->next) {
			if (i->op == INSTR_LINE) continue;
			if (VarIsIdxElem(i->result, idx)) return i->result->adr;
			if (VarIsIdxElem(i->arg1, idx)) return i->arg1->adr;
			if (VarIsIdxElem(i->arg2, idx)) return i->arg2->adr;
		}
	}
	return arr;
}

void VarReplaceArr(Var ** p_var, Var * arr, Var * idx, Var * ptr_ref)
{
	if (VarIsIdxElem(*p_var, idx) && (*p_var)->adr == arr) {
		*p_var = ptr_ref;
	}
}

void LoopPtr(InstrBlock * head, InstrBlock * loop_end, Var * arr, Var * idx, Var * ptr)
{
	InstrBlock * blk;
	Instr * i;
	Var * ptr_deref;

	ptr_deref = VarNewDeref(ptr);

	for(blk = head; blk != loop_end; blk = blk->next) {
		for(i = blk->first; i != NULL; i = i->next) {
			if (i->op == INSTR_LINE) continue;
			VarReplaceArr(&i->result, arr, idx, ptr_deref);
			VarReplaceArr(&i->arg1, arr, idx, ptr_deref);
			VarReplaceArr(&i->arg2, arr, idx, ptr_deref);

			if (i->op == INSTR_ADD || i->op == INSTR_SUB) {
				if (i->result == idx && i->result == idx) {
					InstrInsert(blk, i, i->op, ptr, ptr, i->arg2);
				}
			}
		}
	}
}

Bool VarShiftIsPossible(InstrBlock * head, InstrBlock * loop_end, Var * var, BigInt * shift)
{
	Bool reversal_possible = true;
	InstrBlock * blk;
	Instr * i;
	Instr i2;

	for(blk = head; blk != loop_end; blk = blk->next) {
		for(i = blk->first; i != NULL; i = i->next) {
			if (i->op == INSTR_LINE) continue;
			if (VarIsEqual(i->result, var) || VarIsEqual(i->arg1, var) || VarIsEqual(i->arg2, var)) {
				if (IS_INSTR_BRANCH(i->op) && VarIsConst(i->arg2) || VarIsConst(i->arg1)) {
					// this is comparison against constant, that's possible
				}  else {
					if (!InstrIsSelfReferencing(i)) return false;
				}
			} else {
				MemMove(&i2, i, sizeof(Instr));
				if (VarShiftIndex(&i2.result, var, shift) || VarShiftIndex(&i2.arg1, var, shift) || VarShiftIndex(&i2.arg2, var, shift)) {
					if (!InstrTranslate3(i2.op, i2.result, i2.arg1, i2.arg2, TEST_ONLY)) {
						return false;
					}
				}
			}
		}
	}
	return true;
}
/*
Var * VarShift(Var * var, Var * to_shift, Var * shift)
{
	Var * idx;
	if (var == to_shift) {
		return VarNewOp(INSTR_ADD, var, shift);
	} if (var->mode == INSTR_ELEMENT) {
		idx = VarShift(var->var, to_shift, shift);
		if (idx != var->var) {
			return VarNewOp(INSTR_ELEMENT, var->adr, idx);
		}
	}
	return var;
}
*/

Var * VarAddNMod(Var * left, BigInt * right, BigInt * modulo)
{
	Var * var = NULL;
	BigInt * l = VarIntConst(left);
	BigInt r;
	BigInt r2;

	if (l != NULL) {
		IntAdd(&r, l, right);
		IntMod(&r2, &r, modulo);

		var = VarN(&r2);
//		var = VarInt((left->n + right) % modulo);
		IntFree(&r2);
		IntFree(&r);
	}
	return var;
}

void LoopShift(InstrBlock * head, InstrBlock * loop_end, Var * var, BigInt * shift, BigInt * top)
{
	InstrBlock * blk;
	Instr * i;
	for(blk = head; blk != loop_end; blk = blk->next) {
		for(i = blk->first; i != NULL; i = i->next) {
			if (i->op == INSTR_LINE) continue;
			if (IS_INSTR_BRANCH(i->op)) {
				if (VarIsEqual(i->arg1, var)) {
					i->arg2 = VarAddNMod(i->arg2, shift, top);
				} else if (VarIsEqual(i->arg2, var)) {
					i->arg1 = VarAddNMod(i->arg1, shift, top);
				}
			} else {
				VarShiftIndex(&i->result, var, shift);
				VarShiftIndex(&i->arg1, var, shift);
				VarShiftIndex(&i->arg2, var, shift);
			}
		}
	}
}

void OptimizeLoopShift(Var * proc)
{
	Bool modified = false;
	InstrBlock * header;
	Loc preheader;
	Instr * i, * init_i;
	Loc loc;
	Var * loop_var, * ptr;
	Type * type;
	BigInt shift;
	BigInt top;
	Var * arr;

	if (Verbose(proc)) {
		PrintHeader(3, "Loop shift");
		PrintProc(proc);
	}

	loc.proc = proc;

	for(loc.blk = proc->instr; loc.blk != NULL; loc.blk = loc.blk->next) {
		header = NULL;
		if (loc.blk->jump_type == JUMP_LOOP) {
			header = loc.blk->cond_to;
			if (header == NULL) header = loc.blk->to;
		}

		if (header != NULL) {
			i = LastInstr(loc.blk);

			if (i != NULL && IS_INSTR_BRANCH(i->op)) {

				loop_var = i->arg1;

				type = i->type[ARG1];
				if (type->variant == TYPE_SEQUENCE) {
					if (type->seq.op == INSTR_ADD && TypeIsIntConst(type->seq.step) && TypeIsIntConst(type->seq.init) && TypeIsIntConst(type->seq.limit)) {

						// Find the initialization of loop variable
						// We do not need to find the initialization, we can just increment it
						
						LoopPreheader(proc, header, &preheader);

						i = preheader.i;
						if (i == NULL) {
							i = preheader.blk->last;
						} else {
							i = i->prev;
						}

						if (i != NULL) {
							if (i->op == INSTR_LET && VarIsEqual(i->result, loop_var)) {
								if (VarIsIntConst(i->arg1)) {
									init_i = i;
								}
							}
						}

						// Test, if it is possible to do some address shifts

//						arr = FindPtrReplace(header, loc.blk->next, loop_var);
						arr = NULL;
						if (arr != NULL) {
							ptr = VarNewTmp(TypeAdrOf(arr->type->element));
							InstrInsert(preheader.blk, init_i, INSTR_LET_ADR, ptr, arr, NULL);
							LoopPtr(header, loc.blk->next, arr, loop_var, ptr);
						}

						// Compute the difference (we compute difference up to 256 - zero overflow)
						
						IntInit(&top, 256);
						IntSub(&shift, &top, TypeMax(type));

						if (init_i != NULL && VarShiftIsPossible(header, loc.blk->next, loop_var, &shift)) {
							LoopShift(header, loc.blk->next, loop_var, &shift, &top);
							init_i->arg1 = VarAddNMod(init_i->arg1, &shift, &top);
						}
						IntFree(&top);
						IntFree(&shift);
						
					}
				}
			}
		}
	}
}
