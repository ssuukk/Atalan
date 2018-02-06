/*

Dead store elimination & next use information computation

(c) 2010 Rudolf Kudla 
Licensed under the MIT license: http://www.opensource.org/licenses/mit-license.php

Removal of assignments to variables that are not subsequently read, either because the lifetime of the variable ends 
or because of a subsequent assignment that will overwrite the first value.

*/

#include "language.h"

// We use variable flag VarLive to mark the variable as used.
// src_i is used as next_use information for variable.

#define VarMarkLive(V) VarMark((V), VarLive)
#define VarMarkDead(V) VarMark((V), VarDead)

//TODO: Could we use special value of next_use as live information?
//      I.e. next_use != NULL means dead, otherwise live

//TODO:
//   We should use two sets of variables for every block.
//   1. Live - variables live in this block.
//   2. Dead - variables killed in this block.
//
//   This may easily be done using one set, where one value defines live, another value defines dead.

void VarMark(Var * var, VarFlags state)
/*
Purpose:
	Mark variable as live or dead.

	Reference to array using variable may not be marked as dead or alive, as 
	it may in fact reference other variable than we think in case the variable is changes.
Input:
	var		Variable to mark as live or dead
	state	VarLive or VarDead
*/
{
	Var * var2;
	InstrInfo * ii;

	if (var == NULL) return;

	// Dereferencing reads the variable (even if it is used for writing at that address later)
	if (var->mode == INSTR_DEREF) {
		VarMarkLive(var->var);

	// If this is array access variable, mark indices as live (used)
	} if (var->mode == INSTR_ELEMENT || var->mode == INSTR_BYTE || var->mode == INSTR_BIT) {

		if (var->adr->mode == INSTR_DEREF) {
			VarMarkLive(var->adr);
		}
		
		// Index used to access array is always live
		VarMarkLive(var->var);

		// Array references with variable indexes are always live
		if (var->var->mode != INSTR_INT) state = VarLive;

	} else {

		ii = &INSTR_INFO[var->mode];

		if (ii->arg_type[1] == TYPE_ANY) {
			VarMark(var->adr, state);
		}

		if (ii->arg_type[2] == TYPE_ANY) {
			VarMark(var->var, state);
		}

		// If variable is alias for some other variable, mark the other variable too
		if (var->mode == INSTR_VAR && var->adr != NULL) {
			if (var->adr->mode == INSTR_VAR || var->adr->mode == INSTR_TUPLE) {
				VarMark(var->adr, state);
			}
		}
	}

	// We will never mark output variable as dead

	if (!OutVar(var)) {
		var->flags = (var->flags & ~VarLive) | state;

		// Each element, which has this variable as an array is marked same
		FOR_EACH_VAR(var2)
			if (var2->mode == INSTR_ELEMENT || var2->mode == INSTR_BYTE || var2->mode == INSTR_BIT) {
				if (var2->adr == var) {
					var2->flags = (var2->flags & ~VarLive) | state;
				}
			}
		NEXT_VAR
	}
}

// 0 dead
// 1 live
// 2 undecided


Bool VarInTuple(Var * var, Var * find_var)
/*
Purpose:
	Try to find the value in the tuple.
*/
{
	if (var == NULL) return false;
	if (var == find_var) return true;
	if (var->mode == INSTR_VAR && var->adr != NULL) {
		return VarInTuple(var->adr, find_var);
	}
	if (var->mode == INSTR_TUPLE) {
		return VarInTuple(var->adr, find_var) || VarInTuple(var->var, find_var);
	}
	return false;
}

Bool VarMayUseVar(Var * var, Var * test_var)
/*
Purpose:
	Test, if the two array element variables may point to the same array.
*/
{
	if (var != NULL && test_var != NULL) {
		if (test_var->mode == INSTR_ELEMENT || test_var->mode == INSTR_BYTE) {
			if (var->mode == INSTR_ELEMENT || var->mode == INSTR_BYTE) {
				// This is the same array and the index of the variable is the same
				if (var->adr == test_var->adr) {
					if (var->var->mode != INSTR_INT || test_var->var->mode != INSTR_INT || IntEq(&var->var->n, &test_var->var->n)) return true;
				}
			}
		}
	}
	return false;
}

UInt8 VarIsLiveInBlock(Var * proc, InstrBlock * block, Var * var)
/*
Purpose:
	Test, if variable is live after specified block.
*/
{
	Instr * i;
	UInt8 res1, res2;
	if (block == NULL) return 0;
	if (block->processed) return 2;

	block->processed = true;

	res1 = res2 = 2;

	// var may be _arr(0)  -> in such case, we want to test _arr as it is to check usage like @_arr
//	if ((var->mode == INSTR_ELEMENT || var->mode == INSTR_BYTE) && var->var->mode == INSTR_INT) {
//		var = var->adr;
//	}

	for (i = block->first; i != NULL; i = i->next) {
		if (i->op == INSTR_LINE) continue;

		if (i->op == INSTR_CALL || i->op == INSTR_GOTO) {
			if (VarUsesVar(i->result, var)) { res1 = 1; goto done; }
		}

		if (var->mode == INSTR_ELEMENT || var->mode == INSTR_BYTE) {
			if (VarMayUseVar(i->arg1, var) || VarMayUseVar(i->arg2, var)) { res1 = 1; goto done; }
		}

		if (VarUsesVar(i->arg1, var) || VarUsesVar(i->arg2, var)) { res1 = 1; goto done; }
		if (VarInTuple(i->result, var)) { res1 = 0; goto done;}

		// Handle processor flags modified by instruction.

		if (i->rule != NULL) {
			if (VarInTuple(i->rule->flags, var)) { res1 = 0; goto done;}
		}

		if (var->adr != NULL && i->arg1 == var->adr) { res1 = 1; goto done; }			//TODO: Maybe not necessary? (Tested by VarUsesVar)

		if (i->op == INSTR_LET_ADR) {
			if (VarIsArrayElement(i->arg1)) {
				if (var->mode == INSTR_ELEMENT) {
					if (var->adr == i->arg1->adr) { res1 = 1; goto done; }
				}
			}
		}
	}

	// If block ends and the variable is one of results, it is live

	if (block->next == NULL && FlagOn(var->submode, SUBMODE_ARG_OUT) && var->scope == proc) return 1;

	// We haven't encountered the variable, let's try blocks following this block

	res1 = VarIsLiveInBlock(proc, block->to, var);
	if (res1 != 1) {
		res1 = VarIsLiveInBlock(proc, block->cond_to, var);
		return res1;
	}

	if (res1 == 1 || res2 == 1) { res1 = 1; goto done; }
	if (res1 == 0 && res2 == 0) res1 = 0;
done:
	//Mark the source instruction, so we can compute next use information
	if (res1 == 1) { var->src_i = i; }
//	block->processed = false;
	return res1;
}

void MarkProcLive(Var * proc)
{
	//TODO: We should actually step through the code, as procedure may access and modify global variables
	Var * var;

	FOR_EACH_LOCAL(proc, var)
		if (FlagOff(var->submode, SUBMODE_ARG_IN | SUBMODE_ARG_OUT)) {
			VarMarkDead(var);
		}
	NEXT_LOCAL

	FOR_EACH_OUT_ARG(proc, var)
		VarMarkDead(var);
	NEXT_LOCAL

	// Procedure may use same variable both for input and output (for example using aliasing)
	// a:proc >x@_a <y@_a
	// In such case, marking variable as live has precedence.

	FOR_EACH_IN_ARG(proc, var)
		VarMarkLive(var);
	NEXT_LOCAL
}

void VarMarkNextUse(Var * var, Instr * i)
{
	InstrInfo * ii;
	if (var == NULL) return;
	ii = &INSTR_INFO[var->mode];

	if (var->mode == INSTR_VAR) {
		var->src_i = i;
	} else {
		if (ii->arg_type[1] == TYPE_ANY) VarMarkNextUse(var->adr, i);
		if (ii->arg_type[2] == TYPE_ANY) VarMarkNextUse(var->var, i);
	}
/*
	if (var->mode == INSTR_ELEMENT || var->mode == INSTR_TUPLE || var->mode == INSTR_DEREF || var->mode == INSTR_SUB) {
		VarMarkNextUse(var->var, i);
		VarMarkNextUse(var->adr, i);
	} else {
		var->src_i = i;
	}
*/
}

Bool VarDereferences(Var * var)
{
	InstrInfo * ii;
	if (var != NULL) {
		ii = &INSTR_INFO[var->mode];
		if (var->mode == INSTR_DEREF) return true;
		if (ii->arg_type[1] == TYPE_ANY) if (VarDereferences(var->adr)) return true;
		if (ii->arg_type[2] == TYPE_ANY) if (VarDereferences(var->var)) return true;
//		if (var->mode == INSTR_ELEMENT || var->mode == INSTR_BYTE || var->mode == INSTR_TUPLE) return VarDereferences(var->adr) || VarDereferences(var->var);
	}
	return false;
}

Bool VarIsDead(Var * var)
{
	InstrInfo * ii;
	if (var == NULL) return true;

	ii = &INSTR_INFO[var->mode];

	if (var->mode == INSTR_TUPLE) {
		return VarIsDead(var->adr) && VarIsDead(var->var);
	} if (var->mode == INSTR_VAR && FlagOff(var->submode, SUBMODE_REG) && var->adr != NULL && !VarIsConst(var->adr)) {
		return VarIsDead(var->adr);
	} else {
		return FlagOff(var->flags, VarLive) && !OutVar(var);
	}
}

Bool FlagIsDead(Var * var, Instr * i)
{
	Instr * i2;

	if (var == NULL) return true;

	if (var->mode == INSTR_TUPLE) {
		return FlagIsDead(var->adr, i) && FlagIsDead(var->var, i);
	} if (var->mode == INSTR_VAR && FlagOff(var->submode, SUBMODE_REG) && var->adr != NULL) {
		return FlagIsDead(var->adr, i);
	} else {
		if (OutVar(var)) return false;
		if (FlagOff(var->flags, VarLive)) return true;
		
		// Flag is on, test, if it is not set in previous instruction
		// If the flag is set by previous instruction, and that instruction sets the result of our let, we consider the flag dead.

		if (i->op == INSTR_LET) {
			i2 = i->prev;
			while (i2 != NULL && i2->op == INSTR_LINE) i2 = i2->prev;
			if (i2 != NULL) {
				if (VarIsEqual(i->arg1, i2->result) && VarUsesVar(i2->rule->flags, var)) {
					return true;
				}
			}
		}
		return false;
	}
}

Bool FlagsAreDead(Instr * i)
{
	return FlagIsDead(i->rule->flags, i);
}

Bool OptimizeLive(Var * proc)
{
	Bool modified = false;
	InstrBlock * blk;
	Instr * i;
	Var * var, * result;
	UInt32 n = 0, blk_n = 0;
	InstrOp op;
	Loc loc;
	UInt8 color;

	if (Verbose(proc)) {
		PrintHeader(3, "optimize live", proc->name);
		PrintProc(proc);
	}

	for(blk = proc->instr; blk != NULL; blk = blk->next) {
		
		// Compute total number of instructions in procedure, so we can report position of removed instruction
		if (Verbose(proc)) {
			n += blk_n;
			blk_n = 0;
			for(i = blk->last; i != NULL; i = i->prev) blk_n++;
			n += blk_n;
		}

		// At the beginning, all variables are dead (except procedure output variables for tail blocks)

		FOR_EACH_VAR(var)

			var->src_i = NULL;

//			if (blk->seq_no == 4 && var->mode == INSTR_BYTE && var->var->n == 0 && StrEqual(var->adr->name, "s")) {
//				Print("");
//			}

			if (StrEqual(var->name, "a") && var->scope != NULL && StrEqual(var->scope->name, "CPU")) {
				Print("");
			}

			// Non-local variables (except registers) are always considered live
			if (!VarIsLocal(var, proc) && !VarIsReg(var)) {
				SetFlagOn(var->flags, VarLive);
			} else {
				// Procedure output argument are live in last block
				if (blk->to == NULL && var->submode == SUBMODE_ARG_OUT) {
					SetFlagOn(var->flags, VarLive);
				// Local variables in last block
				} else if (blk->to == NULL && FlagOff(var->submode, SUBMODE_ARG_OUT) /*&& VarIsLocal(var, proc)*/) {
					SetFlagOff(var->flags, VarLive);

				// Out variables are always live
				} else if (FlagOn(var->submode, SUBMODE_OUT)) {
					SetFlagOn(var->flags, VarLive);
				} else {
					SetFlagOff(var->flags, VarLive);
					MarkBlockAsUnprocessed(proc->instr);
					if (VarIsLiveInBlock(proc, blk->to, var) == 1 || VarIsLiveInBlock(proc, blk->cond_to, var) == 1) {
						SetFlagOn(var->flags, VarLive);
					}
				}
			}
		NEXT_VAR

		// All output arguments are marked as live (in last block)

		if (blk->to == NULL) {
			FOR_EACH_OUT_ARG(proc, var)
				VarMarkLive(var);
			NEXT_OUT_ARG
		}

		n = InstrBlockInstrCount(blk);

		for(i = blk->last; i != NULL; i = i->prev, n--) {

			op = i->op;
			if (op == INSTR_LINE || op == INSTR_RETURN) continue;

			if (i->rule == NULL) {
				loc.proc = proc;
				loc.blk = blk;
				loc.i = i;
				InternalErrorLoc("Missing rule", &loc);
				Print("#"); PrintInt(blk->seq_no); Print("/"); PrintInt(n); InstrPrint(i);
				exit(2);
			}

			result = i->result;
			if (result != NULL) {
				if (op != INSTR_LABEL && op != INSTR_CALL) {
					if (VarIsDead(result) && !VarIsLabel(result) && !VarIsArray(result) && !VarDereferences(result)) {
						// Prevent removing instructions, that read IN SEQUENCE variable
						if ((i->arg1 == NULL || FlagOff(i->arg1->submode, SUBMODE_IN_SEQUENCE)) && (i->arg2 == NULL || FlagOff(i->arg2->submode, SUBMODE_IN_SEQUENCE))) {

							if (FlagsAreDead(i)) {
								// Test flags

								if (Verbose(proc)) {
									color = PrintColor(OPTIMIZE_COLOR);
									PrintFmt("Removing dead %ld#%ld:", blk->seq_no, n); InstrPrint(i);
									PrintColor(color);
								}
								i = InstrDelete(blk, i);
								modified = true;
								if (i == NULL) break;		// we may have removed last instruction in the block
								continue;
							}
						}
					}
				}
			}

			//===== Mark result as dead
			//      Result must be marked first dead first, to properly handle instructions like x = x + 1

			if (result != NULL) {
				if (i->op != INSTR_CALL && i->op != INSTR_GOTO) {
					VarMarkDead(result);
					i->next_use[0] = result->src_i;
					result->src_i = NULL;			// next use $$$$
					VarMarkDead(i->rule->flags);
				}
			}

			//===== Mark arguments as live (used)

			// For procedure call, we mark as live variable all variables used in that procedure
			if (i->op == INSTR_CALL || i->op == INSTR_GOTO /*result != NULL && result->type->variant == TYPE_PROC*/) {
				MarkProcLive(i->result);
				VarMarkLive(i->result);
			} else {
				VarMarkLive(i->arg1);
				VarMarkLive(i->arg2);

				// Mark next use of the parameter argument to this instruction

				if (i->arg1 != NULL) {
					i->next_use[1] = i->arg1->src_i;
					VarMarkNextUse(i->arg1, i);
				}
				if (i->arg2 != NULL) {
					i->next_use[2] = i->arg2->src_i;
					VarMarkNextUse(i->arg2, i);
				}

				if (i->result != NULL) {
					i->next_use[0] = i->result->src_i;
				}


			}
		}
	}
	return modified;
}
