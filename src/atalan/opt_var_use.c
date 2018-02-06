/*

Assign addresses to variables

(c) 2010 Rudolf Kudla 
Licensed under the MIT license: http://www.opensource.org/licenses/mit-license.php

This module is responsible for assigning physical addresses to variables.

*/

#include "language.h"

//var_heap:0..255 or 32000..65000

GLOBAL MemHeap VAR_HEAP;		// variable heap (or zero page heap), this is heap from which variables are preferably allocated

void ProcClearProcessed(Var * proc)
{
	SetFlagOff(proc->flags, VarProcessed);
}

Bool ProcCallsProc(Var * proc, Var * called_proc)
/*
Purpose:
	Test, whether procedure 'called_proc' is called (even indirectly) from specified procedure.
*/{
	Bool calls = false;
	Instr * i;
	InstrBlock * blk;

	// We use VarProcessed to prevent resursion for recursive functions, even
	// if recursion is not supported yet.

	if (FlagOff(proc->flags, VarProcessed)) {
		SetFlagOn(proc->flags, VarProcessed);
		for(blk = proc->instr; blk != NULL; blk = blk->next) {
			for(i = blk->first; i != NULL; i = i->next) {
				if (i->op == INSTR_CALL) {
					if (i->result == called_proc || ProcCallsProc(i->result, called_proc)) {
						calls = true;
						goto done;
					}
				}
			}
		}
done:
		SetFlagOff(proc->flags, VarProcessed);
	}
	return calls;
}

typedef struct
{
	VarSet vars;
	UInt8 * collisions;			// 2D array of collisions of local_variables
	LiveSet last_block;
} VarAllocInfo;


Bool BlockIsLast(InstrBlock * blk)
{
	return (blk->to == NULL || blk->to == blk) && (blk->cond_to == NULL || blk->cond_to == blk);
}

LiveSet MergeLiveSets(InstrBlock * blk, UInt16 count, LiveSet last_block)
{
	UInt16 i;
	LiveSet src_live;
	LiveSet live = (LiveSet)MemAllocEmpty(count);

	// For the last block, begin with of last block info
	if (BlockIsLast(blk)) {
		MemMove(live, last_block, count);
	// For non-last block, use union of to and cond_to block results
	} else {
		if (blk->to != NULL) {
			MemMove(live, blk->to->analysis_data, count);
		}
		if (blk->cond_to != NULL && blk->cond_to != blk) {
			src_live = (LiveSet)blk->cond_to->analysis_data;
			for(i=0; i<count; i++) {
				live[i] = live[i] | src_live[i];
			}
		}
	}
	return live;
}

void MarkVarCollision(VarAllocInfo * info, LiveSet live, UInt16 idx)
{
	UInt16 count, i;
	count = VarSetCount(&info->vars);
	for(i=0; i<count; i++) {
		if (live[i] == 1) {
			info->collisions[idx*count+i] = 1;
			info->collisions[i*count+idx] = 1;
		}
	}
}

void VarAllocVar(VarAllocInfo * info, Var * var, LiveSet live, UInt8 mark)
/*
Purpose:
	Mark variable in live set as either live (used) or dead (assigned).
	If variable is not in the live set, do nothing.
*/
{
	UInt16 idx;
	if (var == NULL) return;
	if (var->mode == INSTR_VAR) {
		idx = var->set_index;
		if (idx >= 0 && idx < VarSetCount(&info->vars) && VarSetItem(&info->vars, idx)->key == var) {
			live[idx] = mark;
//			if (mark == 1) {
				MarkVarCollision(info, live, idx);
//			}
		}
		if (var->adr != NULL) {
			VarAllocVar(info, var->adr, live, mark);
		}

	// For array element, we mark the index variable as read, while whole array gets marked as dead/alive
	} else if (var->mode == INSTR_ELEMENT || var->mode == INSTR_BYTE) {
		VarAllocVar(info, var->adr, live, mark);
		VarAllocVar(info, var->var, live, 1);
	}
}

void VarAllocProc(VarAllocInfo * info, Var * proc, LiveSet live)
/*
Purpose:
	Mark all variables in specified live set as either used or even assigned in the procedure.
	We are basically not interested in the order of the variable use, as for the purpose of the
	variable assignment analysis, the procedure call is single instruction.
*/
{
	Var * var;
	if (proc->instr == NULL) {

		// Local instructions used by procedure (not in or out arguments)
		FOR_EACH_LOCAL(proc, var)
			if (FlagOff(var->submode, SUBMODE_ARG_IN | SUBMODE_ARG_OUT)) {
				VarAllocVar(info, var, live, 1);
			}
		NEXT_LOCAL

		FOR_EACH_OUT_ARG(proc, var)
			VarAllocVar(info, var, live, 0);
		NEXT_LOCAL

		// Procedure may use same variable both for input and output (for example using aliasing)
		// a:proc >x@_a <y@_a
		// In such case, marking variable as live has precedence.

		FOR_EACH_IN_ARG(proc, var)
			VarAllocVar(info, var, live, 1);
		NEXT_LOCAL

	} else {
	}
}

Bool VarAllocBlock(Var * proc, InstrBlock * blk, void * pinfo)
{
	Instr * i;
	VarAllocInfo * info = (VarAllocInfo *)pinfo;
	UInt16 count = VarSetCount(&info->vars);
	LiveSet live = MergeLiveSets(blk, count, info->last_block);
	InstrInfo * ii;

	// Traverse block backwards and mark variables as live/dead

	for(i = blk->last; i != NULL; i = i->prev) {
		ii = &INSTR_INFO[i->op];

		if (i->op == INSTR_CALL) {
			// mark all variables used by the procedure as live
			VarAllocProc(info, i->result, live);
		} else {

			if (ii->arg_type[0] != TYPE_VOID) {
				VarAllocVar(info, i->result, live, 0);
				// mark variable as dead
			}

			if (ii->arg_type[1] != TYPE_VOID) {
				VarAllocVar(info, i->arg1, live, 1);
			}

			if (ii->arg_type[2] != TYPE_VOID) {
				VarAllocVar(info, i->arg2, live, 1);
			}
		}
	}
	blk->analysis_data = live;

	return true;
}


Bool FilterVar(Var * var)
{
	return !VarIsLabel(var) && var->type->variant != TYPE_PROC && var->type->variant != TYPE_MACRO && !VarIsReg(var);
}

void PrintCollisions(VarAllocInfo * info)
{
	UInt16 i, j, count;

	count = VarSetCount(&info->vars);

	for(i = 0; i < count; i++) {
		for(j = 0; j < count; j++) {
			Print(info->collisions[i*count+j]==0?".":"X");
		}
		PrintEOL();
	}

}

void BlockFreeLiveSet(InstrBlock * blk, void * pinfo)
{
	MemFree(blk->analysis_data);
	blk->analysis_data = NULL;
}

void AllocateVariablesFromHeap(Var * proc, MemHeap * heap)
/*
Purpose:
	Allocate procedure local variables using specified heap.
	Try to reuse non-conflicting variables.
	Two variables are in conflict, if they are used in the same time.
*/
{
	Var * var, * var2;
	UInt32 size, adr;
	UInt16 i, j, count;
	VarAllocInfo info;

	// Get all local variables defined for the procedure.
	// As we are going to assign addresses to them, labels, procedures, macros and registers are excluded.

	VarSetInit(&info.vars);
	ProcLocalVars(proc, &info.vars, &FilterVar);
	count = VarSetCount(&info.vars);

	// Graph of variable collisions is represented as 2D array.
	// If item collision[a,b] is set to 1, it means the variables a and b are colliding (i.e. they are live at the same moment)
	info.collisions = (UInt8 *)MemAllocEmpty(count * count);

	// For first live set, mark all output variables as live.
	info.last_block = (LiveSet)MemAllocEmpty(count);
	for(i=0; i<count; i++) {
		var = VarSetItem(&info.vars, i)->key;
		if (VarIsOutArg(var)) {
			VarAllocVar(&info, var, info.last_block, 1);
		}
	}

//	VarSetPrint(&info.vars);

	OptimizeDataFlowBack(proc, &VarAllocBlock, &info);

//	PrintCollisions(&info);

	for(i = 0; i < count; i++) {
		var = VarSetItem(&info.vars, i)->key;

		// Do not assign address to variable that already has address
		if (var->adr == NULL) {
			size = TypeSize(var->type);		
			if (size > 0) {

				// Try to find another variable with address which does not collide with this variable
				for(j = 0; j < count; j++) {
					if (i != j && info.collisions[i*count+j] == 0) {
						var2 = VarSetItem(&info.vars, j)->key;
						if (VarIsIntConst(var2->adr) && !OutVar(var2) && !InVar(var2)) {
							if (TypeSize(var2->type) == size) {
								// We have found the variable, whose address we can use

								// Mark variable to which we assign address of other variable to be conflicting with same variables as the other
								MarkVarCollision(&info, &info.collisions[j*count], i);
								MarkVarCollision(&info, &info.collisions[i*count], j);
//								PrintVarName(var); Print("@"); PrintVarName(var2); PrintEOL();
//								PrintCollisions(&info);

								var->adr = var2;
								goto next;
							}
						}
					}

				}

				if (HeapAllocBlock(heap, size, &adr) || HeapAllocBlock(&VAR_HEAP, size, &adr)) {
//							PrintVarName(var); Print("@%d\n", adr);
					var->adr = VarInt(adr);
				} else {
					// failed to alloc memory
				}
			}
		}
next: ;
	}

	// Cleanup

	MemFree(info.collisions);
	MemFree(info.last_block);
	VarSetCleanup(&info.vars);
	ForEachBlock(proc->instr, &BlockFreeLiveSet, NULL);

/*
	for (var = VarFirstLocal(proc); var != NULL; var = VarNextLocal(proc, var)) {

		// Scope can contain variables in subscope, we need to allocate them too
		if (var->mode == INSTR_SCOPE) {
			AllocateVariablesFromHeap(var, heap);
		} else {
			// Do not assign address to unused variables, labels and registers
			if (var->adr == NULL && var->mode == INSTR_VAR) {
				if ((var->write > 0 || var->read > 0) && !VarIsLabel(var) && !VarIsReg(var)) {
					size = TypeSize(var->type);		
					if (size > 0) {
						if (HeapAllocBlock(heap, size, &adr) || HeapAllocBlock(&VAR_HEAP, size, &adr)) {
//							PrintVarName(var); Print("@%d\n", adr);
							var->adr = VarInt(adr);
						} else {
							// failed to alloc in zero page
						}
					}
				}
			}
		}
	}
*/
}

void AllocateVariablesFromHeapNoOptim(Var * proc, MemHeap * heap)
{
	Var * var;
	UInt32 size, adr;

	for (var = VarFirstLocal(proc); var != NULL; var = VarNextLocal(proc, var)) {

		// Scope can contain variables in subscope, we need to allocate them too
		if (var->mode == INSTR_SCOPE) {
			AllocateVariablesFromHeap(var, heap);
		} else {
			// Do not assign address to unused variables, labels and registers
			if (var->adr == NULL && var->mode == INSTR_VAR) {
				if ((var->write > 0 || var->read > 0) && !VarIsLabel(var) && !VarIsReg(var)) {
					size = TypeSize(var->type);		
					if (size > 0) {
						if (HeapAllocBlock(heap, size, &adr) || HeapAllocBlock(&VAR_HEAP, size, &adr)) {
//							PrintVarName(var); Print("@%d\n", adr);
							var->adr = VarInt(adr);
						} else {
							// failed to alloc in zero page
						}
					}
				}
			}
		}
	}
}

#define VAR_ADD 0
#define VAR_REMOVE 1

void HeapVarOp(MemHeap * heap, Var * var, int op)
{
	UInt32 size, adr;
	Var * vadr;
	BigInt * ia;

	if (var == NULL) return;

	if (var->mode == INSTR_VAR) {
		size = TypeSize(var->type);
		vadr = var->adr;
		if (size > 0 && vadr != NULL) {
			if (vadr->mode == INSTR_TUPLE) {
				HeapVarOp(heap, vadr, op);
			} else {
				ia = VarIntConst(vadr);
				if (ia != NULL) {
					adr  = IntN(ia);
					if (op == VAR_REMOVE) {
						HeapRemoveBlock(heap, adr, size);
					} else {
						HeapAddBlock(heap, adr, size);
					}
				}
			}
		}

	} else if (var->mode == INSTR_TUPLE) {
		// Tuple is ignored. If it references variables local to this scope, they will be processed separately anyways.
		//HeapVarOp(heap, var->adr, op);
		//HeapVarOp(heap, var->var, op);
	}
}

void HeapVariablesOp(MemHeap * heap, Var * scope, int op)
{
	Var * var;

	for (var = VarFirstLocal(scope); var != NULL; var = VarNextLocal(scope, var)) {
		if (var->mode == INSTR_SCOPE) {
			HeapVariablesOp(heap, var, op);
		} else {
			HeapVarOp(heap, var, op);
		}
	}
}

extern Var   ROOT_PROC;

void AllocateVariables(Var * proc)
{
	Var * proc2;
	Type * type;
	MemHeap heap;
	
	HeapInit(&heap);

	//==== Find space already allocated for other procedure variables that can be reused
	//     We can reuse variables from procedures, that this procedure 
	//     does not call (even indirectly) and which does not call this procedure.

	for(proc2 = VarFirst(); proc2 != NULL; proc2 = VarNext(proc2)) {
		type = proc2->type;
		if (type != NULL && type->variant == TYPE_PROC && proc2->read > 0 && proc2->instr != NULL) {
//			Print("%s -> %s", proc2->name, proc->name);
			if (proc2 != proc && !ProcCallsProc(proc, proc2) && !ProcCallsProc(proc2, proc)) {
//				if (/*StrEqual(proc->name, "copyblock") &&*/ StrEqual(proc->name, "drawmainscreen")) {
//					Print("***\n");
//				}
				HeapVariablesOp(&heap, proc2, VAR_ADD);
//				if (StrEqual(proc->name, "drawmainscreen")) {
//					HeapPrint(&heap);
//				}

			} else {
//				Print("... Dependent");
			}
//			Print("\n");
		}
	}

	//==== Remove space allocated by procedures we are calling or which call us

	//TODO: Procedure must not have body (may be external) and still define arguments

	for(proc2 = VarFirst(); proc2 != NULL; proc2 = VarNext(proc2)) {
		type = proc2->type;

//		if (StrEqual(proc2->name, "copyblock") && StrEqual(proc->name, "drawmainscreen")) {
//			Print("***");
//		}

		if (type != NULL && type->variant == TYPE_PROC && proc2->read > 0 && proc2->instr != NULL) {
			if (proc2 != proc && (FlagOn(proc2->flags, VarUsedInInterupt) || ProcCallsProc(proc, proc2) || ProcCallsProc(proc2, proc))) {
//				if (StrEqual(proc->name, "drawmainscreen")) {
//					HeapPrint(&heap);
//				}
				HeapVariablesOp(&heap, proc2, VAR_REMOVE);
//				if (StrEqual(proc->name, "drawmainscreen")) {
//					HeapPrint(&heap);
//				}
			}
		}
	}
	
	//==== Allocate space for all variables, that has not been assigned yet

	AllocateVariablesFromHeapNoOptim(proc, &heap);

	HeapCleanup(&heap);
}
