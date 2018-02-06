/*

Live variable analysis

(c) 2012 Rudolf Kudla 
Licensed under the MIT license: http://www.opensource.org/licenses/mit-license.php

See: http://en.wikipedia.org/wiki/Liveness_analysis



*/

#include "language.h"
// Global information used by analysis


typedef struct
{
	VarSet vars;				// variables somehow used in the procedure

} LiveInfo;


static Bool FilterVar(Var * var)
/*
	We do not perform live analysis for constants, labels, procedures or macros.
*/
{
	return !VarIsConst(var) && !VarIsLabel(var) && var->type->variant != TYPE_PROC && var->type->variant != TYPE_MACRO;
}

static void VarAddReference(Var * var, VarSet * set)
{
	if (var == NULL) return;

	if (VarIsConst(var) || VarIsLabel(var) || var->type->variant == TYPE_PROC || var->type->variant == TYPE_MACRO) return;

	if (var->mode == INSTR_VAR) {
		VarSetAdd(set, var, NULL);

	// Array references with constant index are handled like simple variable
	} else if ((var->mode == INSTR_BYTE || var->mode == INSTR_ELEMENT) && VarIsConst(var->var)) {
		VarSetAdd(set, var, NULL);		
		VarSetAdd(set, var->adr, NULL);
	} else {
		VarAddReference(var->adr, set);
		VarAddReference(var->var, set);
	}

}

void ProcAddReferencedVars(Var * proc, VarSet * set);

static Bool InstrAddReferencedVars(Loc * loc, void * data)
{
	VarSet * set = (VarSet *)data;
	Instr * i = loc->i;
	InstrInfo * ii = &INSTR_INFO[i->op];

	if (i->op == INSTR_CALL) {
		ProcAddReferencedVars(i->result, set);
	} else {

		if (ii->arg_type[0] != TYPE_VOID) {
			VarAddReference(i->result, set);
		}

		if (ii->arg_type[1] != TYPE_VOID) {
			VarAddReference(i->arg1, set);
		}

		if (ii->arg_type[2] != TYPE_VOID) {
			VarAddReference(i->arg2, set);
		}
	}

	return false;
}

void ProcAddReferencedVars(Var * proc, VarSet * set)
/*
Purpose:
	Add variables referenced in the procedure to set of referenced variables.
*/
{
	ProcInstrEnum(proc, &InstrAddReferencedVars, set);
}

void ProcReferencedVars(Var * proc, VarSet * set)
{
	VarSetEmpty(set);
	ProcAddReferencedVars(proc, set);
}

void VarSetLiveness(LiveSet live, VarSet * vars, Var * var, int mark)
{
	UInt16 idx;
	if (var == NULL) return;

	idx = var->set_index;
	if (idx >= 0 && idx < VarSetCount(vars) && VarSetItem(vars, idx)->key == var) {
		live[idx] = mark;
	}

	if (var->mode == INSTR_VAR) {
		if (var->adr != NULL) {
			VarSetLiveness(live, vars, var->adr, mark);
		}

	} else if (var->mode == INSTR_TUPLE) {
		VarSetLiveness(live, vars, var->adr, mark);
		VarSetLiveness(live, vars, var->var, mark);

	// For array element, we mark the index variable as read, while whole array gets marked as dead/alive
	} else if (var->mode == INSTR_ELEMENT || var->mode == INSTR_BYTE || var->mode == INSTR_BIT) {
		VarSetLiveness(live, vars, var->adr, mark);
		VarSetLiveness(live, vars, var->var, VarLive);

	} else {
		VarSetLiveness(live, vars, var->adr, VarLive);
		VarSetLiveness(live, vars, var->var, VarLive);
	}

}

Bool AnalyzeLiveBlock(Var * proc, InstrBlock * blk, void * pinfo)
/*
Purpose:
	Perform variable live/dead analysis on specified block.
*/
{
	InstrInfo * ii;
	Instr * i;
	LiveSet set;
	LiveInfo * info = (LiveInfo*)pinfo;
	UInt16 count, n;
	Var * var;
	LiveSet lset, rset;
	Bool changed = false;

	count = VarSetCount(&info->vars);

	set = (LiveSet)MemAlloc(count);

	// Compute block out
	
	if (blk->to == NULL && blk->cond_to == NULL) {

		for (n=0; n<count; n++) set[n] = VarDead;

		// For exit block, mark output variables and non-local variables as live
		if (blk->to == NULL && blk->cond_to == NULL) {
			for (n=0; n<count; n++) {
				var = VarSetItem(&info->vars, n)->key;
				if (VarIsOutArg(var) || !VarIsLocal(proc, var)) {
					set[n] = VarLive;
				}
			}
		}

	// Merge the two sets
	// Variable is dead, if it is dead in both branches

	} else {
		lset = (blk->to != NULL)?(LiveSet)blk->to->analysis_data:NULL;
		rset = (blk->cond_to != NULL)?(LiveSet)blk->cond_to->analysis_data:NULL;

		if (lset == NULL) lset = rset;
		if (rset == NULL) rset = lset;

		for(n=0; n<count; n++) {
			if (lset[n] == VarDead && rset[n] == VarDead) {
				set[n] = VarDead;
			} else {
				set[n] = VarLive;
			}
		}
	}

	// Traverse block backwards and mark variables as live/dead

	for(i = blk->last; i != NULL; i = i->prev) {
		ii = &INSTR_INFO[i->op];

		if (i->op == INSTR_CALL) {
			// mark all variables used by the procedure as live
//			VarAllocProc(info, i->result, live);
		} else {

			if (ii->arg_type[0] != TYPE_VOID) {
				VarSetLiveness(set, &info->vars, i->result, VarDead);
				VarSetLiveness(set, &info->vars, i->rule->flags, VarDead);		// set flags as dead
			}

			if (ii->arg_type[1] != TYPE_VOID) {
				VarSetLiveness(set, &info->vars, i->arg1, VarLive);
			}

			if (ii->arg_type[2] != TYPE_VOID) {
				VarSetLiveness(set, &info->vars, i->arg2, VarLive);
			}
		}
	}

	// Test, if set has changed
	for(n = 0; n < count && !changed; n++) {
		if (set[n] != ((LiveSet)blk->analysis_data)[n]) changed = true;
	}

	MemFree(blk->analysis_data);
	blk->analysis_data = set;

	return changed;
}

/*

If variable is dead in all following blocks, it is dead. Otherwise it is supposed to be live.

*/

void LiveVariableAnalysis(Var * proc)
{
	// initialize all last blocks (there is typically one) with return and global values

	LiveInfo info;
	InstrBlock * blk;
	LiveSet set;
	UInt16 i, count;

	VarSetInit(&info.vars);
	ProcReferencedVars(proc, &info.vars);
	count = VarSetCount(&info.vars);

	// Initialize variable live sets in every block

	for (blk = proc->instr; blk != NULL; blk = blk->next) {
		set = (LiveSet)MemAlloc(count);
		for (i=0; i<count; i++) set[i] = VarLive;
		blk->analysis_data = set;
	}

	// Perform analysis

	DataFlowAnalysis(proc, &AnalyzeLiveBlock, &info);
	
}

void FreeLiveVariableAnalysis(Var * proc)
{

}

Bool OptimizeLive2(Var * proc)
{
	return false;
}

