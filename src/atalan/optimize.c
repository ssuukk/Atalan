/*

Code optimalization routines

(c) 2010 Rudolf Kudla 
Licensed under the MIT license: http://www.opensource.org/licenses/mit-license.php

*/

#include "language.h"

//#define PEEPHOLE_SIZE 2
//extern Bool VERBOSE;
extern Var   ROOT_PROC;

void VarIncRead(Var * var)
{

	InstrInfo * ii;
	if (var != NULL) {
		ii = &INSTR_INFO[var->mode];

		var->read++;
		if (var->write == 0) {
			// variable should not be marked as uninitialized, if there has been label or jump or call
			var->flags |= VarUninitialized;
		}

//		if (var->mode == INSTR_ELEMENT || var->mode == INSTR_BYTE) {
//			VarIncRead(var->adr);
//			VarIncRead(var->var);
//		} else if (var->mode == INSTR_DEREF) {
//			VarIncRead(var->var);
//		} else if (var->mode == INSTR_TUPLE) {
//			VarIncRead(var->adr);
//			VarIncRead(var->var);			
//		} else 
		if (var->mode == INSTR_VAR) {
			// Do not increment constant used as address
			if (var->adr != NULL && var->adr->mode != INSTR_INT) VarIncRead(var->adr);
		} else if (VarIsConst(var)) {
		} else {
			VarIncRead(var->adr);
			VarIncRead(var->var);
		}
	}
}

void VarIncWrite(Var * var)
{
	if (var != NULL) {
		var->write++;
		if (var->mode == INSTR_ELEMENT || var->mode == INSTR_BYTE) {
			VarIncWrite(var->adr);
			VarIncRead(var->var);
		} else if (var->mode == INSTR_DEREF) {
			VarIncRead(var->var);
		} else if (var->mode == INSTR_TUPLE) {
			VarIncWrite(var->adr);
			VarIncWrite(var->var);
		} else {
			if (var->adr != NULL && var->adr->mode != INSTR_INT) VarIncRead(var->adr);
		}
	}
}

void InstrVarUse(InstrBlock * code, InstrBlock * end)
{
	Instr * i;
	Var * result;
	InstrBlock * blk;
	
	for(blk = code; blk != end; blk = blk->next) {
		for(i = blk->first; i != NULL; i = i->next) {

			if (i->op == INSTR_LINE) continue;
			if (i->op == INSTR_CALL) continue;		// Calls are used to compute call chains and there are other rules of computation

			// Writes are registered as last to correctly detect uninitialized variable access
			VarIncRead(i->arg1);
			VarIncRead(i->arg2);

			result = i->result;
			VarIncWrite(result);

			// In instructions like op X, X, ? or op X, ?, X, X is induction variable

			if (InstrIsSelfReferencing(i)) {
				result->flags |= VarLoop;
			}
		}
	}
}


void VarUse()
/*
Purpose:
	Compute use of variables.
*/
{
	Var * proc;
	Var * var;
	VarResetUse();

	FOR_EACH_VAR(proc)
		if (proc->type != NULL && proc->type->variant == TYPE_PROC && proc->read > 0) {
			if (proc->instr != NULL) {
				InstrVarUse(proc->instr, NULL);
			} else {
				// Procedure that has no defined body can still define variables and arguments it uses.
				// We must mark these variables as used, if the procedure is used.
				for(var = VarFirstLocal(proc); var != NULL; var = VarNextLocal(proc, var)) {
					VarIncRead(var);
				}
			}
		}
	NEXT_VAR

	InstrVarUse(ROOT_PROC.instr, NULL);

	FOR_EACH_VAR(proc)
		if (proc->type != NULL && proc->type->variant == TYPE_ARRAY && proc->mode == INSTR_VAR) {
			if (proc->read > 0 || proc->write > 0) {
				if (proc->instr != NULL) {
					InstrVarUse(proc->instr, NULL);
				}
			}
		}
	NEXT_VAR


}

//TODO: Replace variable management (keep array of those variables and reuse them)

Int16 VarTestReplace(Var ** p_var, Var * from, Var * to)
/*
Purpose:
	Create variable where 'from' variable is replaced by 'to' variable.
	Return number of replacements made (0 if none).
*/
{
	Var * var, * var2;
	Int16 n = 0;
	Int16 n2, n3;
	Var * v2, * v3;

	var = *p_var;
	if (var != NULL) {
		// TODO: We basically garantee that the int valueas are the same variable
		if (var == from || (var->mode == INSTR_INT && from->mode == INSTR_INT && IntEq(&var->n, &from->n))) {
			*p_var = to;
			n++;
		} else {
			if (var->mode == INSTR_INT) {
				// const does not get replaced
			} else if (var->mode == INSTR_TEXT) {

			} else if (var->mode == INSTR_VAR) {
				if (var->adr != NULL) {
					*p_var = var->adr;
					n = VarTestReplace(p_var, from, to);
				}
			} else if (var->mode == INSTR_CONST) {

			} else /*if (var->mode == INSTR_ELEMENT || var->mode == INSTR_BYTE || var->mode == INSTR_TUPLE)*/ {
				v2 = var->adr;
				v3 = var->var;
				n2 = VarTestReplace(&v2, from, to);
				n3 = VarTestReplace(&v3, from, to);

				// In case something was replaced, we must allocate new element, as we must not modify original variable
				// TODO: Use VarNewElement.

				if (n2 > 0 || n3 > 0) {
					var2 = VarAllocUnused();
					memcpy(var2, var, sizeof(Var));
					var2->adr = v2;
					var2->var = v3;
					var2->next = NULL;

					n += n2 + n3;
					*p_var = var2;
				}
			} /*if (var->mode == INSTR_VAR && var->adr != NULL) {
					*p_var = var->adr;
					n = VarTestReplace(p_var, from, to);
			}*/
		}
	}
	return n;
}

Int16 VarReplace(Var ** p_var, Var * from, Var * to)
/*
Purpose:
	Replace any use of 'from' by 'to'.
*/
{
	Var * var;
	Int16 n = 0;

	var = *p_var;
	if (var == from) {
		*p_var = to;
		n++;
	} else {
		if (var != NULL) {
			if (var->mode != INSTR_INT && var->mode != INSTR_VAR) {
				n += VarReplace(&var->adr, from, to);
				n += VarReplace(&var->var, from, to);
			}

//			if (var->mode == INSTR_ELEMENT || var->mode == INSTR_BYTE || var->mode == INSTR_TUPLE) {
//				n += VarReplace(&var->adr, from, to);
//				n += VarReplace(&var->var, from, to);
//			}
		}
	}
	return n;
}

Int16 InstrReplaceVar(Instr * i, Var * from, Var * to)
{
	Int16 n = 0;
	n += VarReplace(&i->result, from, to);
	n += VarReplace(&i->arg1, from, to);
	n += VarReplace(&i->arg2, from, to);
	return n;
}

Int16 InstrTestReplaceVar(Instr * i, Var * from, Var * to)
{
	Int16 n = 0;
	n += VarTestReplace(&i->result, from, to);
	n += VarTestReplace(&i->arg1, from, to);
	n += VarTestReplace(&i->arg2, from, to);
	return n;
}

Bool InstrUsesVar(Instr * i, Var * var)
{
	if (i == NULL || i->op == INSTR_LINE) return false;

	return VarUsesVar(i->result, var) 
		|| VarUsesVar(i->arg1, var)
		|| VarUsesVar(i->arg2, var);
}

Bool VarReadsVar(Var * var, Var * read_var)
{
	if (var == NULL) return false;
	if (var->mode == INSTR_VAR || var->mode == INSTR_INT || var->mode == INSTR_TUPLE) return false;

	return VarUsesVar(var->adr, read_var) || VarUsesVar(var->var, read_var);
}

Bool SameVar(Var * l, Var * r)
{
	return l != NULL && r != NULL && l == r;		// || (l->mode == INSTR_INT && r->mode == INSTR_INT && IntEq(&l->n, &r->n));
}

Bool InstrReadsVar(Instr * i, Var * var)
{
	if (i == NULL || i->op == INSTR_LINE) return false;

	return VarReadsVar(i->result, var) 
		|| SameVar(i->arg1, var) || VarUsesVar(i->arg1, var)
		|| SameVar(i->arg2, var) || VarUsesVar(i->arg2, var);
}

UInt32 InstrVarUseCount(Instr * from, Instr * to, Var * var)
/*
Purpose:
	Count number of uses of the variable in the specified block of instructions.
*/
{
	Instr * i;
	UInt32 cnt = 0;
	for(i = from; i != to; i = i->next) {
		if (InstrUsesVar(i, var)) cnt++;
	}
	return cnt;
}

Bool ArgNeedsSpill(Var * arg, Var * var)
{
	Bool spill = false;
	if (arg != NULL) {
		if (arg->mode == INSTR_ELEMENT || arg->mode == INSTR_BYTE) {
			if (arg->adr->mode == INSTR_DEREF && arg->adr->var == var->adr) {
				spill = true;
			}			
		}
	}
	return spill;
}

Bool InstrSpill(Instr * i, Var * var)
{
	Bool spill = false;
	//TODO: What about call?
//	if (i->op == INSTR_PRINT || i->op == INSTR_FORMAT) return true;

	if (var->mode == INSTR_ELEMENT || var->mode == INSTR_BYTE) {

		spill = ArgNeedsSpill(i->result, var) 
			 || ArgNeedsSpill(i->arg1, var) 
			 || ArgNeedsSpill(i->arg2, var);

		if (i->arg1 == var->adr || i->arg2 == var->adr) {
			spill = true;
		}
	}
	return spill;
}


void OptimizeCombined(Var * proc)
{
	Bool modified;
	do {
		do {
			modified = OptimizeValues(proc);
			modified |= OptimizeLive(proc);			// We need to call OptimizeLive as second, to keep next_use info
			modified |= OptimizeMergeBranchCode(proc);
		} while(modified);

		modified |= OptimizeLive(proc);	
		modified |= OptimizeVarMerge(proc);
		modified |= OptimizeLoops(proc);
	} while(modified);

}

void ProcOptimize(Var * proc)
{
	if (Verbose(proc)) {
		PrintHeader(2, proc->name);
	}
	OptimizeCombined(proc);
}

/*

==============================
Optimization: Inline expansion
==============================

Inline expansion replaces call to a procedure by actual body of the procedure.

*/

void OptimizeProcInline(Var * proc)
/*
Purpose:
	Replace every call to function that should be inlined with actual code from the function.
*/
{
	Instr * i;	//, * next;
	InstrBlock * blk;
	Var * subproc, * var, * arg;

	for(blk = proc->instr; blk != NULL; blk = blk->next) {
		for(i = blk->first; i != NULL; i = i->next) {
			if (i->op == INSTR_CALL) {
				subproc = i->result;

				// 1. We inline procedures that are called just once
				if (subproc->read == 1) {

					if (subproc->instr != NULL) {
						BufEmpty();
						FOR_EACH_ARG(subproc, arg, SUBMODE_ARG_IN+SUBMODE_ARG_OUT)
							if (arg->adr == NULL) {
								var = VarAllocScopeTmp(proc, INSTR_VAR, arg->type);
							} else {
								var = arg->adr;
							}
							BufPush(var);
							ProcReplaceVar(proc, arg, var);
						NEXT_ARG

						InScope(proc);
						GenSetDestination(blk, i);
						GenMacro(subproc, STACK);
						i = InstrDelete(blk, i);
						subproc->read--;

					}
				}
			}
		}
	}
}

