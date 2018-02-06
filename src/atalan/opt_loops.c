/*

Loop optimization routines

(c) 2010 Rudolf Kudla 
Licensed under the MIT license: http://www.opensource.org/licenses/mit-license.php

*/

#include "language.h"


static Bool G_VERBOSE;

/*
TODO: Loop shift/reversal

On most processors, it is faster to test if the value reached 0, 128 or 256 (possibly 65536 etc.)
We may need to shift the loop index so, that it ends on this limit.

This means:

- we must know initialization, increment and test of loop variable
- loop variable may not be read (except the test) or written (except the increment and initialization)
- loop variable may be used as index, where we may decrement the base address

    ldx #0                  ldx #0+diff
	lda #0                  lda #0
l:  sta array,x         l:  sta array-diff,x
    inx                     inx
	cpx #40				    ;cpx #128
	bne l                   bpl l

It would be easier on symbolic level, but there could be problem with array access.
If the array is big, we may not have the option to do the base shift.
I.e.  lda (adr),y

Problems when replacing on logical level:

- When using instructions like a() = 0, the loop is not yet unwinded, so we cannot test the loop.
  Solution: make rules as macros (macro rule) -> this may unwind in first step, unrolling the loops

- We may symbolically change the loop som that it adds the start value dynamically (even if it is not constant)

1. Detect, that the loop end can be optimized. Detect loop variable (%1), step (%2) and limit (%3)
   We can use multiple instruction rule to detect the loop.

rule
   add %1, %1, %2
   cmp %1, %3:1..127
   bne l
   dead %1
=
   loop 128		  ;declaring the variable as loop with end value of 128
   add %1,%1,%2
   bpl l

2. Try to find the initialization of the loop variable (%1)

   It must be before loop, in the form let %1, %4
   If the %4 is constant, we may just add the difference.
   
3. Test, that we can replace %1 by %1-%diff in every instruction, that uses %1.
   If the %1 is on the left side (except self referencing instructions), do not perform the optimization.
   Do not perform the test (and replacement) in the looping code.

4. Perform replacement of loop end:

   add %1, %1, %2
   bpl l

*/


Var * FindMostUsedVar()
/*
Purpose:
	Find most used variable in loop.
*/
{
	UInt32 max_cnt, cnt;
	Var * top_var, * var;

	max_cnt = 0; top_var = NULL;

	FOR_EACH_VAR(var)
		//TODO: Exclude registers, in or out variables
		//Exclude:
		//   - labels
		//   - variables with specified address (typically registers)
		//   - constants
		//   - arrays (we may optimize array element access though)
		//   - variables whose size is bigger than register

		cnt = var->read + var->write;
		if (cnt > 0) {

			if (VarIsReg(var)) continue;

//			if (VERBOSE) {
//				Print("Var: "); PrintVar(var);
//			}

			if (var->mode == INSTR_ELEMENT || var->mode == INSTR_BYTE) {
				if (FlagOn(var->adr->submode, SUBMODE_IN | SUBMODE_OUT | SUBMODE_REG)) continue;
				if (var->var->mode != INSTR_INT) continue;
//				continue;
				// If array index is loop dependent, do not attempt to replace it with register
//				if (FlagOn(var->var->flags, VarLoopDependent)) continue;
			}

			if (FlagOff(var->submode, SUBMODE_IN | SUBMODE_OUT | SUBMODE_REG) 
//			 && var->mode != INSTR_INT 
			 && var->mode != INSTR_DEREF
			 && var->type != NULL && var->type->variant != TYPE_PROC
			 && !VarIsLabel(var) 
			 && !VarIsArray(var)
			 && FlagOff(var->flags, VarLoopDependent)
			) {
				if (cnt > max_cnt) {
					max_cnt = cnt;
					top_var = var;
				}
			}
		}
	NEXT_VAR

	return top_var;
}

void InstrVarLoopDependent(InstrBlock * code, InstrBlock * end)
/*
	Compute dependency of variables on loop variables.
*/
{
	Instr * i;
	Var * result;
	UInt16 flags;
	Bool  modified;

	InstrBlock * blk;
	
	do {
		modified = false;

		for(blk = code; blk != end; blk = blk->next) {
			for(i = blk->first; i != NULL; i = i->next) {

				if (i->op == INSTR_LINE) continue;

				result = i->result;
				if (result != NULL) {
					if (FlagOff(result->flags, VarLoopDependent)) {
						// In case of direct let, we want to distribute VarLoop flag too
						if (i->op == INSTR_LET) {
							flags = i->arg1->flags & (VarLoopDependent|VarLoop);
							if (result->flags != (result->flags | flags)) {
								result->flags |= flags;
								modified = true;
							}
						} else {
							flags = 0;
							if (i->arg1 != NULL && i->arg1 != result) flags |= i->arg1->flags;
							if (i->arg2 != NULL && i->arg2 != result) flags |= i->arg2->flags;
							if (FlagOn(flags, VarLoop | VarLoopDependent)) {
								result->flags |= VarLoopDependent;
								modified = true;
							}
						}
					}
				}
			}
		}
	} while(modified);
}

InstrBlock * FindLoopDominator(Var * proc, InstrBlock * header)
{
	InstrBlock * prev = NULL, * blk;
	for(blk = proc->instr; blk != header; blk = blk->next) prev = blk;
	return prev;
}

UInt32 NumberBlocks(InstrBlock * block)
{
	UInt32 seq_no;
	InstrBlock * nb;

	seq_no = 1;
	for(nb = block; nb != NULL; nb = nb->next) {
		nb->seq_no = seq_no++;
	}
	return seq_no;
}

Bool VarContains(Var * var, Var * eq)
{
	Var * v, * v2;
	if (var == NULL) return false;

	v2 = var;

retry:
//	if (v2 == eq) return true;
	v = eq;
	do {
		if (v == v2) return true;
		if (v->src_i == NULL) break;
		if (v->src_i->op != INSTR_LET) break;
		v = v->src_i->arg1;
	} while(true);

//	for(v = eq; v->src_i != NULL && v->src_i->op == INSTR_LET; v = v->src_i->arg1) {
//		if (v2 == v) return true;
//	}

	if (v2->src_i != NULL && v2->src_i->op == INSTR_LET) {
		v2 = v2->src_i->arg1;
		if (v2 != var) goto retry;
	}

//	if (var->src_i != NULL) {
//		if (var->src_i->op == INSTR_LET) {
//			if (VarContains(eq, var->src_i->arg1)) return true;
//		}
//	}
//	if (eq->src_i != NULL) {
//		if (eq->src_i->op == INSTR_LET) {
//			if (var == eq->src_i->arg1) return true;
//		}
//	}
	return false;
}

Bool LetCycles(Var * result, Var * arg1, UInt16 * p_q)
{
	Rule * rule = InstrRule2(INSTR_LET, result, arg1, NULL);
	if (rule != NULL) {
		*p_q = rule->cycles;
		return true;
	} else {
		*p_q = 0;
	}
	return false;
}

Int32 UsageQuotient(InstrBlock * header, InstrBlock * end, Var * top_var, Var * reg, Bool * p_init)
/*
Purpose:
	Compute savings achieved by replacing variable top_var by register reg.
	The bigger the value, the more suitable the register is
	0 means no gain, <0 means using the register would lead to less optimal code than if not used
Arguments:
	header	first block of the loop
	end		last block of the loop (it is still part of the loop)
	top_var	variable we want to put in a register
	reg		register to use for the variable
	>p_init	set to true, if we need to init the register with the variable value before the loop
*/{
	Var * prev_var;
	Int32 q;
	UInt16 cycles;
	UInt16 changed;
	InstrBlock * blk, * blk_exit;
	Instr * i, ti;
	UInt32 n;
	Bool first_init, mod_reg;
	UInt16 reg_use;		// number of times, the value in the register has been used since last loaded
	Rule * rule;
	Instr initial;

	blk_exit = end->next;

	// At the beginning, the quotient is 0.
	ResetValues();
	initial.op = INSTR_LET; initial.result = reg; initial.arg1 = top_var; initial.arg2 = NULL;
	VarSetSrcInstr(reg, &initial);

	q = 0;

	// We expect, we will initialize the register with the variable value before entering the loop.
	// This operation is not added to quotient, as it should not affect the speed significantly.

	*p_init = true;
	prev_var = NULL;			// previous variable contained in the register
								// this variable must be loaded, when instruction using the register is encountered
								// (if it is not top_var)
	reg_use = 0;
	first_init = true;

	// Compute usage quotient
	for(blk = header; blk != blk_exit; blk = blk->next) {
		for(i = blk->first, n = 0; i != NULL; i = i->next, n++) {

			if (i->op == INSTR_LINE) continue;

			// Call to subroutine destroys all registers, there will be spill
			if (i->op == INSTR_CALL) { q = 1; goto done; }
			
			// If there is jump except last instruction
			if (IS_INSTR_JUMP(i->op) && (i != blk->last || blk != end)) {
//				*p_init = false;
			}

			mod_reg = VarModifiesVar(i->result, reg);

			if (i->op == INSTR_LET) {
				// If this is let instruction that initializes a variable to value it already contains,
				// we will be able to remove it completely.
				if (!InVar(i->arg1) && !OutVar(i->result) && VarContains(i->result, i->arg1)) {
					if (mod_reg) first_init = false;
					ASSERT(i->rule->cycles > 0);
					q -= i->rule->cycles;
					continue;
				}
			}

			// Instruction uses the register
			if (InstrReadsVar(i, reg)) {
				reg_use++;
				// Instruction uses the register and the register has not been initialized yet.
				// This means, that the register is initialized before the loop and we cannot optimize the loop this way.
				// TODO: Maybe spill to temporary variable is enough?)
				if (first_init) {
					q = 1;
					goto done;
				}
			}

			// Instruction uses top_var and the register does not currently contain the top_var value,
			// we need to load the value to register first.
			if (InstrReadsVar(i, top_var)) {
				if (!VarContains(reg, top_var)) {
					if (!(i->op == INSTR_LET && i->result == reg && i->arg1 == top_var) && LetCycles(reg, top_var, &cycles)) {
						q += cycles;
					}
					reg_use = 0;
					// We may not had to add the load, but we still can not remove this instruction, so do not
					// continue the processing in a normal way, as it would leed to removing the instruction.
					goto next;
				}
			}

			// If we assign the register back to variable, we may remove this instruction
			if (i->op == INSTR_LET && (i->result == top_var && i->arg1 == reg)) {
				q -= i->rule->cycles;
				continue;
			} else {

				// If current instruction uses the register, and it is
				// we need to save the register and load some other.

				if (InstrUsesVar(i, reg) && !VarContains(reg, top_var)) {
					if (prev_var != NULL && LetCycles(reg, top_var, &cycles)) {
						q += cycles;
					}
				}
			}

			// If the instruction stores the result to the variable, we will want to change it to store the result in the top_reg.
			// If the register is currently used for some different purpose, we must spill it.

			if (i->result == top_var && !VarContains(reg, top_var)) {
				if (i->next_use[0] != NULL && LetCycles(top_var, reg, &cycles)) {
					q += cycles;		// TODO: we should use some temporary variable here
				}

				//TODO: In this case, we will need to load the register later, when it is used
				//We should handle the situation.
			// Will it be necessary to spill?
			// We use the variable (array) that is stored to register
			} else if (InstrSpill(i, top_var) && LetCycles(top_var, reg, &cycles)) {
				q += cycles;
			}

			memcpy(&ti, i, sizeof(Instr));
			
			changed = InstrTestReplaceVar(&ti, top_var, reg);

			// If the instruction was changed (it used top_var and it has been replaced to top_reg),
			// test, whether we are able to compile it (some register/adress mode combinations must not be available)

			if (changed > 0) {

				if (ti.op == INSTR_LET && ti.result == ti.arg1) {
					q -= i->rule->cycles;
					continue;
				} 
				rule = InstrRule(&ti);
				if (rule == NULL) {
					if (top_var->mode != INSTR_INT) {
						q = 1;		// do not use this register, as invalid code would get generated
						goto done;
					} else {
						
					}
				} else {
					ASSERT(rule->cycles > 0);
					if (i->rule->cycles >= rule->cycles) {
						q -= i->rule->cycles;			// we remove the current instruction
						q += rule->cycles;              // and add new instruction
					}
				}
			}

			// Instruction modifies the register.
			// We may need to store the value of the register in case it has been modified.

			if (mod_reg) {
				*p_init = false;
				first_init = false;
				reg_use = 0;
			}

next:
			if (i->result != NULL) {
				ResetValue(i->result);
				if (i->result == top_var) {
					if (!InstrIsSelfReferencing(i)) {
						VarSetSrcInstr(reg, &initial);
					} else {
						VarSetSrcInstr(reg, &initial);
//						VarSetSrcInstr(reg, NULL);
					}
				} else {
					VarSetSrcInstr(i->result, i);
				}
			}
		} // instr
	} // blk

	// Value of register is not known at the end of loop, but it is not initialized at the beginning of the loop
	// We must load it before first use.

	if (!*p_init) {
		if (LetCycles(reg, top_var, &cycles)) {
			q += cycles;
		}
	}
done:
	return q;
}

typedef struct {
	InstrBlock * header;
	InstrBlock * end;
} Loop;


void LoopPreheader(Var * proc, InstrBlock * header, Loc * loc)
/*
Purpose:
	Find location of instruction directly preceding the loop.
	New block will be inserted into code, if there is not single instruction.
*/
{
	loc->proc = proc;
	loc->blk = FindLoopDominator(proc, header);
	loc->i = loc->blk->last;

	if (loc->i->op != INSTR_GOTO) {
		loc->i = NULL;
	}

}

void LoopMoveToPrologue(Var * proc, InstrBlock * header, InstrBlock * from, Instr * first, Instr * last)
{
	InstrBlock * blk;
	Instr * i;
	
	blk = FindLoopDominator(proc, header);

	// Header may be terminating point of multiple loops.
	// We must detect this situation.

	if (blk->callers != NULL) {
	}


	i = blk->last;

	// Loops with condition at the beginning may start with jump to condition
	// We need to insert the initialization code before this jump.

	if (i->op == INSTR_GOTO) {
//		i = i->prev;
	} else {
		i = NULL;
	}

	InstrMoveCode(blk, i, from, first, last);

//	Print("========== move to prologue ===========\n");
//	PrintProc(proc);
}

Bool VarIsLoopDependent(Var * var, VarSet * liveset)
{
	if (var == NULL) return false;
	if (VarSetFind(liveset, var)) return false;
	if (FlagOn(var->flags, VarLoop|VarLoopDependent)) return true;
	if (var->mode != INSTR_INT) {
		if (VarIsLoopDependent(var->adr, liveset)) return true;
		if (VarIsLoopDependent(var->adr, liveset)) return true;
	}
	return false;
}

void DefsAdd(Defs * defs, InstrBlock * blk, Instr * i)
{
	Loc * def = &defs->defs[defs->count];
	def->blk = blk;
	def->i   = i;
	defs->count++;
}

void DefsInit(Defs * defs)
{
	defs->count = 0;
}

Bool LoopContainsBlock(Loop * loop, InstrBlock * blk)
{
	return loop->header->seq_no <= blk->seq_no && loop->end->seq_no >= blk->seq_no;
}

void ReachingDefsBlock(Var * var, Loc * loc, InstrBlock * blk, Instr * instr, Defs * defs)
{
	Instr * i;
	InstrBlock * caller;

	if (blk == NULL || blk->processed) return;
	if (instr == NULL) {
		i = blk->last;
	} else {
		i = instr->prev;
	}

	// Definition of the variable may be in this block
	for(; i != NULL; i = i->prev) {
		if (i->op == INSTR_LINE) continue;
		if (i->result == var) {
			DefsAdd(defs, blk, i);
			return;
		}
	}

	// Or it may be in some calling blocks
	
	// If we haven't parsed whole block, do not mark it as processed to allow parsing rest of the instructions.
	// In case this is loop block, we will parse it again, but parsing will stop at defining instruction.
	if (instr == NULL) {
		blk->processed = true;
	}
	ReachingDefsBlock(var, loc, blk->from, NULL, defs);

	for(caller = blk->callers; caller != NULL; caller = caller->next_caller) {
		ReachingDefsBlock(var, loc, caller, NULL, defs);
	}
}

void ReachingDefs(Var * proc, Var * var, Loc * loc, Defs * defs)
{
	MarkBlockAsUnprocessed(proc->instr);
	DefsInit(defs);
	ReachingDefsBlock(var, loc, loc->blk, loc->i, defs);
}

void NextDefsBlock(Var * var, Loc * loc, InstrBlock * blk, Instr * instr, Defs * defs)
{
	Instr * i;

	if (blk == NULL || blk->processed) return;

	// If the starting instruction is not defined, we use first instruction, otherwise we start from following instruction
	// (the specified one is the one we search for).
	if (instr == NULL) {
		i = blk->first;
		blk->processed = true;
	} else {
		i = instr->next;
	}

	// Next definition may be in this block
	for(; i != NULL; i = i->next) {
		if (i->op == INSTR_LINE) continue;
		if (i->result == var && !VarUsesVar(i->arg1, i->result) && !VarUsesVar(i->arg2, i->result)) {
			DefsAdd(defs, blk, i);
			return;
		}
	}

	// Or it may be in some calling blocks
	
	// If we haven't parsed whole block, do not mark it as processed to allow parsing rest of the instructions.
	// In case this is loop block, we will parse it again, but parsing will stop at defining instruction.
//	if (instr == NULL) {
//		blk->processed = true;
//	}
	NextDefsBlock(var, loc, blk->to, NULL, defs);
	NextDefsBlock(var, loc, blk->cond_to, NULL, defs);
}

void NextDefs(Var * proc, Var * var, Loc * loc, Defs * defs)
{
	MarkBlockAsUnprocessed(proc->instr);
	DefsInit(defs);
	NextDefsBlock(var, loc, loc->blk, loc->i, defs);
}

Bool VarInvariant(Var * proc, Var * var, Loc * loc, Loop * loop)
{
	Defs defs;
	UInt16 n;
	Bool out_of_loop;

	if (var == NULL) return true;
	if (VarIsConst(var)) return true;
	if (InVar(var)) return false;

	// For array access, array adr is constant (except referenced array), important is index change
	if (var->mode == INSTR_ELEMENT || var->mode == INSTR_BYTE) {
		return VarInvariant(proc, var->var, loc, loop);
	}

	DefsInit(&defs);
	ReachingDefs(proc, var, loc, &defs);

	// 0 would be undefined variable

	out_of_loop = true;
	for(n=0; n<defs.count; n++) {
		if (LoopContainsBlock(loop, defs.defs[n].blk)) {
			out_of_loop = false;
			break;
		}
	}

	if (out_of_loop) return true;
	return defs.count == 1 && FlagOn(defs.defs[0].i->flags, InstrInvariant);
}

Bool VarLoopDepBlock(Var * var, Loc * loc, InstrBlock * blk, Instr * instr)
{
	Instr * i;

	if (blk == NULL || blk->processed) return false;

	// If the starting instruction is not defined, we use first instruction, otherwise we start from following instruction
	// (the specified one is the one we search for).
	if (instr == NULL) {
		i = blk->first;
		blk->processed = true;
	} else {
		i = instr->next;
	}

	// Next definition may be in this block
	for(; i != NULL; i = i->next) {
		if (i->op == INSTR_LINE) continue;
		if (FlagOff(i->flags, InstrInvariant) && (VarUsesVar(i->arg1, var) || VarUsesVar(i->arg2, var))) {
			return true;
		}
		// This instruction sets new value to variable
		if (i->result == var) return false;
	}

	return VarLoopDepBlock(var, loc, blk->to, NULL) || VarLoopDepBlock(var, loc, blk->cond_to, NULL);
}

Bool VarLoopDep(Var * proc, Var * var, Loc * loc, Loop * loop)
{

	if (var == NULL) return false;
	if (VarIsConst(var)) return false;
	if (InVar(var)) return true;

	// For array access, array adr is constant (except referenced array), important is index change
	if (var->mode == INSTR_ELEMENT || var->mode == INSTR_BYTE) {
		return VarLoopDep(proc, var->var, loc, loop);
	}

	MarkBlockAsUnprocessed(proc->instr);
	
	return VarLoopDepBlock(var, loc, loc->blk, loc->i);

}

Bool VarInvariant2(Var * proc, Var * var, Loc * loc, Loop * loop)
{
	Defs defs;

	NextDefs(proc, var, loc, &defs);

	// In loop, number of definitions may be 0 in case of loop variables
	//:::::::::::::::::::::
	//   x = 0
	//l@
	//   x <- x + 1
	//   if x < 10 goto l@
	//:::::::::::::::::::::
	// Instructions like x = x + 1, are not considered definitions (they modify the value of x, but do not define it).
	// Zero killer must therefore mean the instruction is not invariant.

	if (defs.count == 0) return false;

	if (defs.count == 1) {
//		if (defs.defs[0].i   == loc->i) return true;
		if (defs.defs[0].blk == loc->blk) return true;
	}
	return false;
}

void PrintLoopInvariants(Loop * loop)
{
	InstrBlock * blk, * blk_exit;
	Instr * i;
	UInt32 n;

	blk_exit = loop->end->next;

	for(blk = loop->header; blk != blk_exit; blk = blk->next) {
		i = blk->first; n = 1;
		while(i != NULL) {
			PrintFmt("#%d/%d ", blk->seq_no, n);
			if (FlagOn(i->flags, InstrInvariant)) {
				Print("+");
			} else if (FlagOn(i->flags, InstrLoopDep)) {
				Print("-");
			} else {
				Print(" ");
			}
			InstrPrint(i);
			i = i->next; n++;
		}
	}
}

void OptimizeLoopInvariants(Var * proc, Loop * loop)
{
	InstrBlock * blk, * blk_exit;
	Instr * i, * i2;
	Bool change;
	Loc loc, preheader;
	UInt32 n;

//	Print("========== Invariants ================\n");
//	PrintProc(proc);

	blk_exit = loop->end->next;

	//=== Mark all instructions as variant

	for(blk = loop->header; blk != blk_exit; blk = blk->next) {
		for(i = blk->first; i != NULL; i = i->next) {
			i->flags = 0;
		}
	}

	do {
		change = false;
		for (blk = loop->header; blk != blk_exit; blk = blk->next) {
			loc.blk = blk;
			for (i = blk->first, n=1; i != NULL; i = i->next, n++) {
				loc.i = i;
				if (i->op == INSTR_LINE || IS_INSTR_BRANCH(i->op)) continue;
				if (FlagOff(i->flags, InstrInvariant)) {
					if (i->result != NULL && !OutVar(i->result)) {
						if (VarInvariant(proc, i->arg1, &loc, loop) && VarInvariant(proc, i->arg2, &loc, loop)) {
							if (VarInvariant2(proc, i->result, &loc, loop)) {
								SetFlagOn(i->flags, InstrInvariant);
								change = true;
							}
						}
					}
				}
			}
		}
	} while(change);

	// Mark all instructions that are self-referencing and are not marked as constant
/*
	for (blk = loop->header; blk != blk_exit; blk = blk->next) {
		for (i = blk->first, n=1; i != NULL; i = i->next, n++) {
			if (FlagOff(i->flags, InstrInvariant) && i->op != INSTR_LINE) {
				if (VarUsesVar(i->arg1, i->result) || VarUsesVar(i->arg2, i->result)) {
					i->flags = InstrLoopDep;
				}
			}
		}
	}

	Print("-----\n");
	PrintLoopInvariants(loop);
*/

	do {
		change = false;
		for (blk = loop->header; blk != blk_exit; blk = blk->next) {
			loc.blk = blk;
			i2 = NULL;
			for (i = blk->first, n=1; i != NULL; i = i->next, n++) {
//				loc.i = i;
				if (i->op == INSTR_LINE) continue;
				if (i2 != NULL && FlagOn(i2->flags, InstrInvariant) && FlagOff(i->flags, InstrInvariant)) {
					if (i->op == INSTR_LET && i2->op == INSTR_LET && i2->result == i->arg1) {
						SetFlagOff(i2->flags, InstrInvariant);
					}
				}
				i2 = i;
			}
		}
//		if (change) {
//			Print("-----\n");
//			PrintLoopInvariants(loop);
//		}
	} while(change);


//	Print("-----\n");
//	PrintLoopInvariants(loop);

	//==== Move all invariant instructions to preheader
	
	LoopPreheader(proc, loop->header, &preheader);
	for(blk = loop->header; blk != blk_exit; blk = blk->next) {
		i = blk->first;
		while(i != NULL) {
			i2 = i->next;
			if (FlagOn(i->flags, InstrInvariant)) {
				InstrMoveCode(preheader.blk, preheader.i, blk, i, i);
			}
			i = i2;
		}
	}
}

void CheckInstr(Instr * i)
{
	if (i->rule == NULL) {
		InternalError("invalid instruction");
	}
}

void PrintInsert(Instr * i)
{
	UInt8 color;
	if (G_VERBOSE) {
		color = PrintColor(OPTIMIZE_COLOR);
		Print(" + | ");
		EmitInstrInline(i);
		PrintColor(color);
		PrintEOL();
	}
}

void LoopInsertPrologue(Var * proc, InstrBlock * header, InstrOp op, Var * result, Var * arg1, Var * arg2)
{

	Instr * i = MemAllocStruct(Instr);
	i->op = op;
	i->result = result;
	i->arg1 = arg1;
	i->arg2 = arg2;
	i->rule = InstrRule(i);
	LoopMoveToPrologue(proc, header, NULL, i, i);
	PrintInsert(i);
}


void InstrInsertRule(InstrBlock * blk, Instr * before, InstrOp op, Var * result, Var * arg1, Var * arg2)
{

	Instr * i = MemAllocStruct(Instr);
	i->op = op;
	i->result = result;
	i->arg1 = arg1;
	i->arg2 = arg2;
	i->rule = InstrRule(i);

	CheckInstr(i);
	PrintInsert(i);
	InstrAttach(blk, before, i, i);
}

void PrintChange(Instr * i)
{
	UInt8 color;
	if (G_VERBOSE) {
		color = PrintColor(OPTIMIZE_COLOR);
		Print(" => "); EmitInstrInline(i);
		PrintColor(color);
	}
}

void PrintDelete()
{
	UInt8 color;
	if (G_VERBOSE) {
		color = PrintColor(OPTIMIZE_COLOR);
		Print(" => void"); PrintEOL();
		PrintColor(color);
	}
}

Bool OptimizeLoop(Var * proc, InstrBlock * header, InstrBlock * end)
/*
1. Find loop (starting with inner loops)
   - Every jump to label preceding the jump (backjump) forms a label
   - In nested labels, we encounter the backjump first
				<code1>
			l1@
				<code2>
			l2@
				<code3>
				if.. l2@
				<code4>
				if.. l3@
				<code5>

2. Select variable to put to register
   - Most used variable should be used
   - Some variables are already moved to index register (this is considered use too)

3. Compute cost of moving the variable to register

*/
{
	Instr * i, initial, ti, * last_mod;
	Var * top_var, * reg, * top_reg, * orig_result;
	UInt16 r, regi, changed;
	UInt32 var_size;
	Int32 q, top_q, n;
	Bool init, top_init;
	InstrBlock * blk, * last_mod_blk;
	InstrBlock * blk_exit;
	Bool var_modified;
	Bool verbose;
	Rule * rule;
	UInt8 color;

	blk_exit = end->next;

	G_VERBOSE = verbose = Verbose(proc);
	VarResetUse();
	InstrVarUse(header, blk_exit);
	InstrVarLoopDependent(header, end);

	// When processing, we assign var to register
	for(regi = 0; regi < CPU->REG_CNT; regi++) CPU->REG[regi]->var = NULL;

	while(top_var = FindMostUsedVar()) {

//		if (Verbose(proc)) {
//			Print("Most user var: "); PrintVar(top_var); PrintEOL();
//		}

		top_var->read = top_var->write = 0;
		var_size = VarByteSize(top_var);

		//====== Select the best register for the given variable
		//      let %A,%A   => index -3
		//      use of register instead of variable -1
		//      spill +3

		top_q = 0; top_reg = NULL; top_init = false;

		for(regi = 0; regi < CPU->REG_CNT; regi++) {

			reg = CPU->REG[regi];
			if (FlagOn(reg->submode, SUBMODE_IN|SUBMODE_OUT)) continue;		// exclude input/output registers
			if (reg->type->range.max == 1) continue;						// exclude flag registers
			if (var_size != VarByteSize(reg)) continue;						// exclude registers with different byte size
			if (reg->var != NULL) continue;

			if (InstrRule2(INSTR_LET, reg, top_var, NULL)) {

	//			if (StrEqual(reg->name, "x") && StrEqual(top_var->name, "i")) {
	//				Print(" ");
	//			}
				q = UsageQuotient(header, end, top_var, reg, &init);

				if (q < top_q) {
					top_q = q;
					top_reg = reg;
					top_init = init;
				}
			}
		}

		if (top_reg == NULL) continue;

		reg = top_reg;
		if (Verbose(proc)) {
			color = PrintColor(OPTIMIZE_COLOR);
			PrintFmt("*** Loop %d..%d\n", header->seq_no, end->seq_no);
			Print("Var: "); PrintVarVal(top_var); PrintEOL();
			Print("Register: "); PrintVarName(top_reg); PrintEOL();
			PrintFmt("Quotient: %d\n", top_q);
			PrintColor(color);
		}

		//TODO: If there is Let reg = var and var is not top_var, we need to spill store

		//=== Replace the use of registers

//		PrintProc(proc);

		ResetValues();
		initial.op = INSTR_LET; initial.result = top_reg; initial.arg1 = top_var; initial.arg2 = NULL;

		var_modified = false;
		// Generate instruction initializing the register used to replace the variable
		// before the start of the loop.
		// We only do this, if the variable is not initialized inside the loop.

//		if (FlagOn(top_var->flags, VarUninitialized) && !top_init) {
//			top_init = true;
//		}

 		if (top_init) {
			LoopInsertPrologue(proc, header, INSTR_LET, top_reg, top_var, NULL);
			VarSetSrcInstr(top_reg, &initial);
		}
		r = 0;

		last_mod = NULL;

		for(blk = header; blk != blk_exit; blk = blk->next) {
			if (verbose) PrintBlockHeader(blk);
			for(i = blk->first, n=0; i != NULL; i = i->next) {
retry:
				n++;
				if (i->op == INSTR_LINE) {
					if (verbose) { PrintInstrLine(n); EmitInstrInline(i); PrintEOL(); }
					continue;
				}

				// Delete unnecessary assignment
				if (i->op == INSTR_LET) {
					if (!InVar(i->arg1) && !OutVar(i->result) && VarContains(i->result, i->arg1)) {
del:
						if (verbose) { PrintInstrLine(n); EmitInstrInline(i); }
del2:					if (verbose) { PrintDelete(); }
						i = InstrDelete(blk, i);
						if (i == NULL) break;
						goto retry;	//continue;
					}
				}

				// Load the register with variable if necessary
				if (InstrReadsVar(i, top_var)) {
					if (!VarContains(top_reg, top_var)) {
						if (!(i->op == INSTR_LET && i->result == top_reg && i->arg1 == top_var)) {
							InstrInsertRule(blk, i, INSTR_LET, top_reg, top_var, NULL);
						}
					}
				}

				if (i->op == INSTR_LET && (i->result == top_var && i->arg1 == top_reg)) {
					r++;
					goto del;
				}

				if (InstrSpill(i, top_var)) {
					InstrInsertRule(blk, i, INSTR_LET, top_var, top_reg, NULL);
				}

				if (verbose) { PrintInstrLine(n); EmitInstrInline(i); }

				orig_result = i->result;

				memcpy(&ti, i, sizeof(Instr));
				changed = VarTestReplace(&ti.result, top_var, reg);
				r += changed;
				changed += VarTestReplace(&ti.arg1, top_var, reg);
				changed += VarTestReplace(&ti.arg2, top_var, reg);

				// If the instruction used variable, that contains same value as replaced register, use the register instead
				if (ti.arg1 != reg && VarContains(ti.arg1, reg)) {
					changed += VarTestReplace(&ti.result, ti.arg1, reg);
					changed += VarTestReplace(&ti.arg2, ti.arg1, reg);
					changed += VarTestReplace(&ti.arg1, ti.arg1, reg);
				}

				if (changed > 0) {

					if (i->op == INSTR_LET && ti.result == ti.arg1) {
						goto del2;
					}

					rule = InstrRule(&ti);
					if (rule != NULL && (i->rule->cycles >= rule->cycles)) {
						InstrReplaceVar(i, top_var, top_reg);

						if (i->arg1 != reg && VarContains(i->arg1, reg)) {
							VarTestReplace(&i->result, i->arg1, reg);
							VarTestReplace(&i->arg2, i->arg1, reg);
							VarTestReplace(&i->arg1, i->arg1, reg);
						}

						i->rule = rule;
						CheckInstr(i);
						PrintChange(i);
					}

				}

				if (verbose) PrintEOL();
				ResetValue(i->result);
				if (orig_result == top_var) {
//					if (!InstrIsSelfReferencing(i)) {
						VarSetSrcInstr(i->result, &initial);
//					} else {
//						VarSetSrcInstr(i->result, NULL);
//					}
					last_mod = i; last_mod_blk = blk;
				} else {
					VarSetSrcInstr(i->result, i);
				}
			}
		}

		// Value of register is not known at the end of loop, but it is not initialized at the beginning of the loop
		// We must load it before first use.

		if (!top_init && last_mod) {
			InstrInsertRule(last_mod_blk, last_mod->next, INSTR_LET, top_var, top_reg, NULL);
		}

		// If we replaced some destination by the register, store the register to destination
		if (r > 0) {
			// There may be exit label as part of the loop
			// We need to spill after it

			if (!VarIsConst(top_var)) {
				if (blk_exit == NULL || blk_exit->callers != NULL || blk_exit->from != end) {
					blk_exit = InstrBlockAlloc();
					blk_exit->to = end->to;
					blk_exit->next = end->to;
					end->next = blk_exit;
					end->to   = blk_exit;
				}
				InstrInsertRule(blk_exit, blk_exit->first, INSTR_LET, top_var, top_reg, NULL);
			}
//			InstrInsert(blk_exit, blk_exit->first, INSTR_LET, top_var, top_reg, NULL);
		}

		if (FlagOn(top_var->flags, VarLoopDependent)) {
			reg->flags |= VarLoopDependent;
		}

		return true;
	}
	return false;
}

void MarkLoops(Var * proc)
{
	InstrBlock * blk;
	NumberBlocks(proc->instr);

	for(blk = proc->instr; blk != NULL; blk = blk->next) {
		blk->jump_type = JUMP_IF;
	}

	// Test for each block, if it is end of an loop

	for(blk = proc->instr; blk != NULL; blk = blk->next) {
		if (blk->cond_to != NULL) {
			// We jump to some previous spot in the sequence
			if (blk->cond_to->seq_no <= blk->seq_no) {
				blk->jump_type = JUMP_LOOP;	//->cond_to->loop_end = blk;
			}
		}

		if (blk->to != NULL) {
			if (blk->to->seq_no <= blk->seq_no) {
				blk->jump_type = JUMP_LOOP;	//to->loop_end = blk;
			}
		}
	}

}

Bool OptimizeLoops(Var * proc)
/*
Purpose:
	Find loops in flow graph and call optimization for it.
*/
{
	Loop loop;
	InstrBlock * nb, * header;
	Bool modified = false;

	MarkLoops(proc);

	if (Verbose(proc)) {
		PrintHeader(3, "Optimize loops");
		PrintProc(proc);
	}


	for(nb = proc->instr; nb != NULL; nb = nb->next) {
		header = NULL;

		if (nb->jump_type == JUMP_LOOP) {
			header = nb->cond_to;
			if (header == NULL) header = nb->to;
		}
//		if (nb->cond_to != NULL && nb->cond_to->loop_end == nb) header = nb->cond_to;
//		if (nb->to != NULL && nb->to->loop_end == nb) header = nb->to;

		if (header != NULL) {
//			if (Verbose(proc)) {
//				Print("*** Loop %d..%d\n", header->seq_no, nb->seq_no);
//			}
			loop.header = header;
			loop.end    = nb;
//			OptimizeLoopInvariants(proc, &loop);
			modified |= OptimizeLoop(proc, header, nb);
		}

	}

	return modified;
}
