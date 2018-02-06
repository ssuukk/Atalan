/*

Code optimization routines
Values optimization.

(c) 2010 Rudolf Kudla 
Licensed under the MIT license: http://www.opensource.org/licenses/mit-license.php


- common subexpression
- copy propagation
- constant folding

*/

#include "language.h"

void PrintExp(Exp * exp);

void ExpFree(Exp ** p_exp)
{
	Exp * exp;
	exp = *p_exp;
	*p_exp = NULL;
}

Bool ExpUsesValue(Exp * exp, Var * var)
/*
Purpose:
	Test, if expression uses specified value.
	Var may be array element.
*/
{
	if (exp == NULL) return false;
	if (exp->op == INSTR_VAR) {
		return VarUsesVar(exp->var, var);
	} else {
		return ExpUsesValue(exp->arg[0], var) || ExpUsesValue(exp->arg[1], var);
	}
}

Bool VarIsOffset3(Var * l, Var * r, BigInt * diff);

static UInt16 G_EXP_NEST;

Bool ExpIsIntConst(Exp * exp, BigInt ** p_n)
{
	*p_n = NULL;
	if (exp->op == INSTR_VAR) {
		*p_n = VarIntConst(exp->var);
	}
	return *p_n != NULL;
}

Bool ExpIsOffset(Exp * lexp, Exp * rexp, BigInt * diff)
{
	return false;
}

Bool ExpIsOffsetOfVar(Exp * lexp, Var * r, BigInt * diff)
{
	Bool result = true;
	BigInt * n;
	BigInt dd;
	if (G_EXP_NEST == 100) {
		return false;
	}
	G_EXP_NEST++;

	if (lexp == NULL || r == NULL) return false;
	if (FlagOn(lexp->flags, FlagExpProcessed)) return false;

	SetFlagOn(lexp->flags, FlagExpProcessed);

	// 1. r + <const> = r  => -const
	// 2. r - <const> = r  => const
	if (lexp->op == INSTR_ADD || lexp->op == INSTR_SUB) {
		if (ExpIsIntConst(lexp->arg[1], &n) && ExpIsOffsetOfVar(lexp->arg[0], r, &dd)) {
			if (lexp->op == INSTR_SUB) {
				IntAdd(diff, &dd, n);	// n = -n;
			} else {
				IntSub(diff, &dd, n);
			}
			//*diff -= n;
			IntFree(&dd);
			goto yes;
		}
	} else if (lexp->op == INSTR_VAR) {
		if (VarIsOffset3(lexp->var, r, diff)) goto yes;
	}

	SetFlagOff(lexp->flags, FlagExpProcessed);
	result = ExpIsOffset(lexp, r->dep, diff);

yes:
	SetFlagOff(lexp->flags, FlagExpProcessed);
	return result;
}


Bool VarIsOffset3(Var * l, Var * r, BigInt * diff)
/*
Purpose:
	Test, whether r expression gives same result as l expression, except incremented by some constant value.
*/
{
	Exp * ldep;
	BigInt * il, * ir;

	if (G_EXP_NEST == 100) {
		return false;
	}
	G_EXP_NEST++;

	if (l == NULL || r == NULL) goto no;

	// 1. If the two variables are same, the difference is zero (no matter, what kind of variable it is)
	// TODO: Handle input variables

	if (l == r) goto yes0;

	// 2. Two constants are just offset

	il = VarIntConst(l); ir = VarIntConst(r);
	if (il != NULL && ir != NULL) {
		IntSub(diff, ir, il);		// = r->n - l->n;
		goto yes;
	}

	// 3. If the left variable has a dependency, test this dependency 
	ldep = l->dep;
	if (ldep != NULL) {
		if (r->dep == ldep) goto yes0;
		if (ExpIsOffsetOfVar(ldep, r, diff)) goto yes;
	}

	if (r->dep != NULL) {
		if (ExpIsOffsetOfVar(r->dep, l, diff)) {
			IntNeg(diff);
			goto yes;
		}
//		if (r->dep->op == INSTR_VAR) {
//			if (VarIsOffset3(l, r->dep->var, diff)) goto yes;
//		}
	}
no:
	return false;
yes0:
	IntInit(diff, 0); // diff = 0;
yes:
	return true;
}

Bool VarIsOffset(Var * l, Var * r, BigInt * diff)
/*
Purpose:
	Test, whether two variables at specified place differ only by a constant.
*/
{
	Bool result;
	G_EXP_NEST = 0;
	result = VarIsOffset3(l, r, diff);
	if (G_EXP_NEST == 100) {
		Print("l: "); PrintExp(l->dep); Print("\n");
		Print("l: "); PrintExp(r->dep); Print("\n");
	}
	return result;
}

void ResetValues()
{
	Var * var;
	FOR_EACH_VAR(var)
		var->src_i       = NULL;
		ExpFree(&var->dep);		// TODO: Release the expression objects
	NEXT_VAR
}

void ResetValue(Var * res)
/*
Purpose:
	The value is modified in code to different value.
	Set all references to specified value to NULL in all values.
*/
{
	Var * var;
	Instr * i;
	if (res == NULL) return;

	FOR_EACH_VAR(var)
		if (!VarIsConst(var)) {
//			if (var->name != NULL && StrEqual(var->name, "a")) {
//				i = NULL;
//			}

			i = var->src_i;
			if (i != NULL) {
				// If source instruction is assignment, set the result to source of assignment argument
				// This makes sure, that we can utilize common values in cases like
				// let _a, 10
				// let p, _a
				// let _a, 20
				// let q, _a
				// add r, p, q

				if (i->op == INSTR_LET) {
					if (i->arg1 == res) {
						i->result->src_i = i->arg1->src_i;

						// When the new source instruction uses the changed variable, it cannot be used.
						// For example:
						//  9|    let y, a
						// 10|    let a, @_arr$y
						// 11|    let w$0, a
						// 12|    add y, y, 1         <= we must reset dependency of w$0 on instr 10, as y is changing so the value of @_arr$y has changed
						// 13|    let a, @_arr$y
						// 14|    let w$1, a
						// 17|    let a, y$0
					
						if (InstrUsesVar(i->result->src_i, res)) {
							i->result->src_i = NULL;
						}
					} else {
						if (InstrUsesVar(i, res)) {
							var->src_i = NULL;
						}
					}
				} else {
					//TODO: Only reset the instruction, if there is no source
					if (i->arg1 == res || i->arg2 == res) {
						var->src_i = NULL;
					}
				}
			}
		}
	NEXT_VAR

	// If value is alias to some other value, reset it too
	//TODO: How about element (non constant, let's say?)

	var = res->adr;
	if (res->mode == INSTR_VAR && var != NULL) {
		if (var->mode == INSTR_VAR || var->mode == INSTR_TUPLE) {
			ResetValue(var);
		}
	} else if (res->mode == INSTR_TUPLE) {
		ResetValue(res->adr);
		ResetValue(res->var);
	}

	res->src_i = NULL;
}

Bool VarIsAlias(Var * var, Var * alias)
/*
Purpose:
	Return true, if the two variables are alias for same memory location.
	This may also mean than one variable is array and the other is element of that array.
*/
{
	if (var == NULL || alias == NULL) return false;
	if (var == alias) return true;
	if (var->adr != NULL && var->adr->mode == INSTR_VAR) if (VarIsAlias(var->adr, alias)) return true;
	if (alias->adr != NULL &&  alias->adr->mode == INSTR_VAR) if (VarIsAlias(var, alias->adr)) return true;
	return false;
}

void ResetVarDepRoot(Var * res)
{
	Var * var;

	FOR_EACH_VAR(var)
		if (VarIsAlias(var, res)) {
			ExpFree(&var->dep);
		}
	NEXT_VAR
}

void ResetVarDep(Var * res)
/*
Purpose:
	Specified variable is being set to some other value.
	Reset all references to specified value to NULL in all variables.
*/
{
	Var * var;
	Exp * exp;

	FOR_EACH_VAR(var)

//		if (var->adr != NULL && StrEqual(var->adr->name, "_arr")) {
//			Print("");
//			PrintExp(var->dep);
//		}

//		if (VarIsAlias(var, res)) {
//			ExpFree(&var->dep);
//		} else {
			exp = var->dep;
			if (exp != NULL) {
				if (ExpUsesValue(exp, res)) {
					ExpFree(&var->dep);
				} else if (var->mode == INSTR_DEREF && VarUsesVar(var->var, res)) {
					ExpFree(&var->dep);
				}
			}
//		}
	NEXT_VAR

	// If the variable is alias for some other variable,
	// reset aliased variable too

	var = res->adr;
	if (var != NULL) {
		if (var->mode == INSTR_VAR) {
			ResetVarDep(var);
		}
	} else {
		if (res->mode == INSTR_TUPLE) {
			ResetVarDep(var->adr);
			ResetVarDep(var->var);
		}
	}
}

/*********************************
  Expressions
**********************************/

Exp * ExpAlloc(InstrOp op)
{
	Exp * exp = MemAllocStruct(Exp);
	exp->op = op;
	return exp;
}

void ExpArg(Exp * exp, UInt8 arg_idx, Var * arg)
/*
Purpose:
	Specify argument for expression.
	If the argument has assigned dependency expression, it is used as dependency.
	If it has no dependency argument, source (INSTR_LET) dependency is created.
*/
{
	Exp * src_dep;

	if (arg != NULL) {

		src_dep = arg->dep;

		if (arg->mode == INSTR_DEREF) {
			src_dep = ExpAlloc(INSTR_DEREF);
			ExpArg(src_dep, 0, arg->var);
		} else if (VarIsArrayElement(arg) || arg->mode == INSTR_BYTE) {
			//TODO: Support for 2d arrays
			//      In assembler phase is not required (we do not have instructions for 2d indexed arrays).
			if (arg->var->mode != INSTR_INT) {
				src_dep = ExpAlloc(arg->mode);
				ExpArg(src_dep, 0, arg->adr);
				ExpArg(src_dep, 1, arg->var);
			}
		} else if (arg->mode == INSTR_TUPLE) {
			src_dep = ExpAlloc(arg->mode);
			ExpArg(src_dep, 0, arg->adr);
			ExpArg(src_dep, 1, arg->var);
		} else if (arg->mode == INSTR_VAR && arg->adr != NULL && arg->adr->mode == INSTR_TUPLE) {
			ExpArg(exp, arg_idx, arg->adr);
			return;
		}

		if (src_dep != NULL) {
			exp->arg[arg_idx] = src_dep;
		} else {
			src_dep = ExpAlloc(INSTR_VAR);
			src_dep->var = arg;
			exp->arg[arg_idx] = src_dep;
		}
	}
}

void PrintExp(Exp * exp)
{
	if (exp != NULL) {
		if (exp->op == INSTR_DEREF) {
			Print("@");
			PrintExp(exp->arg[0]);
		} else if (exp->op == INSTR_VAR) {
			PrintVarVal(exp->var);
		} else if (exp->op == INSTR_ELEMENT) {
			PrintExp(exp->arg[0]);
			Print("(");
			PrintExp(exp->arg[1]);
			Print(")");
		} else if (exp->op == INSTR_ELEMENT) {
			PrintExp(exp->arg[0]);
			Print("$");
			PrintExp(exp->arg[1]);
		} else {
			// Unary instruction
			if (exp->arg[1] == NULL) {
				PrintFmt(" %s ", OpSymbol(exp->op));
				PrintExp(exp->arg[0]);
			} else {
				Print("(");
				PrintExp(exp->arg[0]);
				PrintFmt(" %s ", OpSymbol(exp->op));
				PrintExp(exp->arg[1]);
				Print(")");
			}
		}
	}
}

Exp * ExpInstr(Instr * i)
/*
Purpose:
	Build dependency for result of instruction i.
*/
{
	Exp * exp;
	Var * arg;
	InstrOp op;

	op = i->op;
	arg = i->arg1;

	if (op == INSTR_LET) {
		if ((VarIsArrayElement(arg) || arg->mode == INSTR_BYTE) && arg->var->mode != INSTR_INT) {
			exp = ExpAlloc(arg->mode);
			ExpArg(exp, 0, arg->adr);
			ExpArg(exp, 1, arg->var);
		} else {
			if (arg->dep != NULL) {
				if (op == INSTR_VAR) {
					arg = arg->dep->var;
				} else {
					exp = arg->dep;
					goto done;
				}
			}
			exp = ExpAlloc(INSTR_VAR);
			exp->var = arg;
		}
	} else {
		exp = ExpAlloc(op);
		//todo: kill dependency, if source variables are not equal to result

		ExpArg(exp, 0, i->arg1);
		ExpArg(exp, 1, i->arg2);
	}
done:
	return exp;
}

void SetDependency(Var * var, Exp * exp)
{	
	if (var == NULL) return;

	// If we are setting value of some expression to tuple, separate elements of that tuple are reset.
	// Whole tuple however receives the dependency on the expression.

	if (var->mode == INSTR_TUPLE) {
		ResetVarDep(var->adr);
		ResetVarDep(var->var);
//		SetDependency(var->adr, exp);
//		SetDependency(var->var, exp);
//	} else {
	}
		var->dep = exp;
//	}

	if (var->mode == INSTR_VAR && var->adr != NULL && (var->adr->mode == INSTR_TUPLE || var->adr->mode == INSTR_VAR)) {
		SetDependency(var->adr, exp);
	}
}

void Dependency(Instr * i)
{
	Exp * exp;

	exp = ExpInstr(i);

	SetDependency(i->result, exp);
//	i->result->dep = exp;

	// If we set a value to result and arg1 is NULL, set the dependency to source value too (they are both same)
	// This may happen, when some self-reference expression is calculated.

	if (i->op == INSTR_LET && i->arg1->dep == NULL && i->arg1->mode != INSTR_INT) {
		exp = ExpAlloc(INSTR_VAR);
		exp->var = i->result;
		SetDependency(i->arg1, exp);
	}

/*
	InstrPrintInline(i);
	Print("       ");
	PrintVarVal(i->result);
	Print(" = ");
	PrintExp(exp);
	Print("\n");
*/
}

Bool ExpEquivalent(Exp * e1, Exp * e2)
{
	Bool eq = false;
	Var  * v1, * v2;
	if (e1 == NULL || e2 == NULL) return false;
	if (e1 == e2) return true;

	if (FlagOn(e1->flags, FlagExpProcessed) || FlagOn(e2->flags, FlagExpProcessed)) return false;

	SetFlagOn(e1->flags, FlagExpProcessed);
	SetFlagOn(e2->flags, FlagExpProcessed);

	if (e1->op == e2->op) {
		if (e1->op == INSTR_VAR) {
			v1 = e1->var; v2 = e2->var;
			if (FlagOn(v1->submode, SUBMODE_IN)) goto done;
			if (v1 == v2) { eq = true; goto done; }

			// Detect mutual dependency of two variables
			if (v1->dep != NULL && v1->dep->op == INSTR_VAR && v1->dep->var == v2) { eq = true; goto done; }

			if (v1->mode == v2->mode) {
				if (v1->type->variant == v2->type->variant) {
					if (v1->mode == INSTR_INT) {
						eq = IntEq(&v1->n, &v2->n);
						goto done;
					}
				}
			}
			eq = ExpEquivalent(v1->dep, v2->dep);
		} else {
			eq = ExpEquivalent(e1->arg[0], e2->arg[0]);
			if (eq && (e1->arg[1] != NULL || e2->arg[1] != NULL)) {
				eq = ExpEquivalent(e1->arg[1], e2->arg[1]);
			}
		}
	}
done:
	SetFlagOff(e1->flags, FlagExpProcessed);
	SetFlagOff(e2->flags, FlagExpProcessed);

	return eq;
}

Bool ExpEquivalentInstr(Exp * exp, Instr * i)
{
	Exp * exp2;
	Bool r;
	exp2 = ExpInstr(i);
	r = ExpEquivalent(exp, exp2);
	return r;
}

Bool CodeModifiesVar(Instr * from, Instr * to, Var * var)
{
	Instr * i;
	Var * result;
	for(i = from; i != NULL && i != to; i = i->next) {
		if (i->op != INSTR_LINE) {
			result = i->result;
			if (result != NULL)  {
				if (result == var) return true;
				if (var->mode == INSTR_ELEMENT || var->mode == INSTR_BYTE) {
					if (result == var->adr) return true;
					if (result == var->var) return true;
				}
			}
		}
	}
	return false;
}

void VarResetProcessed()
{
}

void ProcValuesUseLocal(Var * proc)
{
	Var * var;

	FOR_EACH_LOCAL(proc, var)
		if (!VarIsInArg(var)) {
			ResetValue(var);
			ResetVarDep(var);
			ResetVarDepRoot(var);
		}
	NEXT_LOCAL
}

void ProcValuesUse(Var * proc)
{
	Instr * i;
	InstrBlock * blk;

	if (proc->type->variant == TYPE_ADR) {
		// We know, that the reference to procedure address may not be variable with actual instructions,
		// so we use just local variables.
		// TODO: This may not be sufficient, as we do not know, what local variables may be called by the procedure.
		ProcValuesUseLocal(proc->type->element->owner);
	} else {
		if (FlagOff(proc->flags, VarProcessed)) {
			SetFlagOn(proc->flags, VarProcessed);

			// Some external procedures may be only declared, in such case,
			// we just clear it's output variables

			if (proc->instr == NULL) {
				ProcValuesUseLocal(proc);
			} else {
				for(blk = proc->instr; blk != NULL; blk = blk->next) {
					for(i = blk->first; i != NULL; i = i->next) {
						if (i->op == INSTR_LINE) {
						} else if (i->op == INSTR_CALL) {
							ProcValuesUse(i->result);
						} else if (IS_INSTR_JUMP(i->op)) {
							// jump instructions do have result, but it is label we jump to
						} else {
							if (i->result != NULL) {
								ResetValue(i->result);
								ResetVarDep(i->result);
								ResetVarDepRoot(i->result);
							}
						}
					}
				}
			}
			SetFlagOff(proc->flags, VarProcessed);
		}
	}
}

Bool VarIsZeroNonzero(Var * var, Var ** p_zero, Var ** p_non_zero)
/*
Purpose:
	Test, if variable may have just two values, 0 and some other.
	This is basically true for 0..1 integers or enumerators with 0 and other value.
*/
{
	Type * t;
	Var * zero = NULL, * non_zero = NULL;
	Var * item;

	if (var == NULL) return false;
	t = var->type;
	if (t->variant == TYPE_INT) {
		if (t->is_enum) {

			FOR_EACH_LOCAL(t->owner, item)
				if (VarIsIntConst(item)) {
					if (VarIsN(item, 0)) {
						zero = item;
					} else {
						if (non_zero != NULL) {
							// There are two different non-zero constants!
							if (!VarEq(non_zero, item)) {
								zero = NULL;
								break;
							}
						} else {
							non_zero = item;
						}
					}
				}
			NEXT_LOCAL
		} else {
			if (IntEq(&t->range.min, Int0()) && IntEq(&t->range.max, Int1())) {
				zero = ZERO;
				non_zero = ONE;
			} else if (IntEqN(&t->range.min, -1) && IntEq(&t->range.max, Int0())) {
				zero = ZERO;
				non_zero = VarInt(-1);
			}
		}
	}
	*p_zero = zero;
	*p_non_zero = non_zero;
	return zero != NULL;
}

Var * SrcVar(Var * var)
{
	if (var != NULL) {
		while (FlagOff(var->submode, SUBMODE_IN) && var->src_i != NULL && var->src_i->op == INSTR_LET) var = var->src_i->arg1;
	}
	return var;
}

/*
Var * VarStripFlags(Var * var)

Purpose:
	Return variable with register flags stripped off.

{
	Var * l, * r;
	if (VarIsReg(var) && var->type->variant == TYPE_INT && var->type->range.min == 0 && var->type->range.max == 1) {
		return NULL;
	}
	if (var->mode == INSTR_TUPLE) {
		l = VarStripFlags(var->adr);
		r = VarStripFlags(var->var);
		if (l == NULL) return r;
		if (r == NULL) return l;
	} else if (var->mode == INSTR_VAR && var->adr != NULL) {
		return VarStripFlags(var->adr);
	}

	return var;
}
*/
UInt32 GOG = 0;
//UInt32 GOG2 = 0;

void VarSetSrcInstr(Var * var, Instr * i)
{
	if (var == NULL) return;
	if (var->mode == INSTR_INT) return;

	var->src_i = i;
	if (var->mode == INSTR_TUPLE) {
		VarSetSrcInstr(var->adr, i);
		VarSetSrcInstr(var->var, i);
	} else if (var->mode == INSTR_VAR) {
		if (var->adr != NULL) {
			VarSetSrcInstr(var->adr, i);
		}
	}
}

void VarSetSrcInstr2(Var * var, Instr * i)
{
	while(i != NULL && i->op == INSTR_LET && i->arg1->src_i != NULL) {
		i = i->arg1->src_i;
	}
	VarSetSrcInstr(var, i);
}


void TransformInstrRule(Loc * loc, Rule * rule, Var * result, Var * arg1, Var * arg2, char * message)
{
	Instr * i;
	UInt8 old_color;

	i = loc->i;
	if (Verbose(loc->proc)) {
		old_color = PrintColor(GREEN+LIGHT);
		PrintFmt("%ld#%ld %s:", loc->blk->seq_no, loc->n, message); InstrPrintInline(i);
	}
	i->op = rule->op;
	i->result = result;
	i->arg1 = arg1;
	i->arg2 = arg2;
	i->rule = rule;
	if (Verbose(loc->proc)) {
		Print(" => "); InstrPrint(i);
		PrintColor(old_color);
	}
}

Bool TransformInstr(Loc * loc, InstrOp op, Var * result, Var * arg1, Var * arg2, char * message)
{
	Rule * rule;
	rule = InstrRule2(op, result, arg1, arg2);
	if (rule != NULL) {
		TransformInstrRule(loc, rule, result, arg1, arg2, message);
		return true;
	}
	return false;
}

Bool TransformInstrIfCheaper(Loc * loc, InstrOp op, Var * result, Var * arg1, Var * arg2, char * message)
{
	Rule * rule;
	rule = InstrRule2(op, result, arg1, arg2);
	// We transform the instruction only it the alternative instruction exists and the new instruction if faster.
	// We also perform the transformation, if the speed is same (we suppose the instruction may be shorter)
	if (rule != NULL && rule->cycles <= loc->i->rule->cycles) {
		TransformInstrRule(loc, rule, result, arg1, arg2, message);
		return true;
	}
	return false;
}

Bool OptimizeValues(Var * proc)
/*
   1. If assigning some value to variable (let) and the variable already contains such value, remove the let instruction

	  Do not use if:
         - destination variable is marked as out
	     - source variable is marked as in

   2. Copy propagation a <- b where b <- c  to a <- c

   3. Constant folding (Evaluate constant instructions)
*/
{
	Bool modified, m2, m3;
	Instr * i, * src_i;
//	UInt32 n;
	Var * r, * result, * arg1, * arg2, * r2;
	InstrBlock * blk;
	InstrOp op, src_op;
	char buf[32];
	BigInt diff;
	Bool  opt_increment;
	Loc loc;
	UInt16 regi;
	UInt8 color;
	Bool is_zero;

	loc.proc = proc;

	if (Verbose(proc)) {
		PrintHeader(3, "optimize values");
		PrintProc(proc);
	}

	VarUse();

	modified = false;

	for(blk = proc->instr; blk != NULL; blk = blk->next) {
		loc.blk = blk;

		ResetValues();
		loc.n = 0;
		for(i = blk->first; i != NULL; i = i->next) {
retry:
			loc.i = i;
			loc.n++;

			// Instruction may be NULL here, if we have deleted the last instruction in the block.
			if (i == NULL) break;
			// Line instructions are not processed.
			if (i->op == INSTR_LINE) continue;

			if (i->op == INSTR_CALL) {
				ProcValuesUse(i->result);
				continue;
			}

			result = i->result;

			// We are only interested in instructions that have result
			if (result != NULL) {

				if (IS_INSTR_JUMP(i->op)) {

					// jump instructions use label as result, we do not want to remove jumps
				} else {

					// Remove equivalent instructions.
					// If the instruction sets it's result to the same value it already contains,
					// remove the instruction.

					opt_increment = false;
					arg1 = i->arg1;
					src_i = result->src_i;

					// If result is equal to arg1, instructions are not equivalent, because they accumulate the result,
					// (for example sequence of mul a,a,2  mul a,a,2

					m3 = false;

					if (result != arg1 && result != i->arg2 && !OutVar(result)
					&& (arg1 == NULL || FlagOff(arg1->submode, SUBMODE_IN)))  {
//						GOG++;
//						if (GOG == 18) {
//							PrintExp(result->dep);
//						}
						m3 = ExpEquivalentInstr(result->dep, i);
					}
					 
					if (m3) {

						// Array references, that have non-const index may not be removed, as
						// we can not be sure, that the index variable has not changed since last
						// use.
						if ((result->mode == INSTR_ELEMENT || result->mode == INSTR_BYTE) && result->var->mode != INSTR_INT) {
						
						} else {
	delete_instr:
							if (Verbose(proc)) {
								color = PrintColor(OPTIMIZE_COLOR);
								PrintFmt("%ld#%ld Removing:", blk->seq_no, loc.n); InstrPrint(i);
								PrintColor(color);
							}
							i = InstrDelete(blk, i);
							modified = true;
							goto retry;
						}
					} else {
					
						/*
						===============================================
						Optimization: Replace assignment with increment
						===============================================

						Replace sequence like:
						::::::::
						let a,5
						...
						let a,6
						::::::::
						by
						::::::::
						let a,5
						...
						add a,a,1
						::::::::
						The variable ::a:: will be usually register and increment instruction 
						is shorter (and therefore faster) than assignment in such case.
						Similar variant is done for 'sub'.
						*/

						if (i->op == INSTR_LET && VarIsOffset(result, arg1, &diff)) {

							if (IntLowerN(&diff, 0)) {
								op = INSTR_SUB;
								IntNeg(&diff);
							} else {
								op = INSTR_ADD;
							}

							r2 = VarN(&diff);
							if (TransformInstrIfCheaper(&loc, op, result, result, r2, "Converting to inc/dec")) {
								arg1 = result;
								arg2 = r2;
								opt_increment = true;
								modified = true;
							}
							IntFree(&diff);
						}

						// Instruction result is set to different value, we must therefore reset
						// all references to this value from all other values.
						ResetValue(result);
						ResetVarDep(result);
					}

					// Try to replace arguments of operation by it's source (or eventually constant)

					op = i->op;

					m3 = false;		// mark when we are at the next step
					m2 = false;

					if (arg1 != NULL && arg1->src_i != NULL && FlagOff(arg1->submode, SUBMODE_IN)) {
						src_i = arg1->src_i;
						src_op = src_i->op;

						// Try to replace LO b,n LET a,b  => LO a,n LO a,n

						if (src_op == INSTR_LO || src_op == INSTR_HI || src_op == INSTR_LET_ADR) {
							if (op == INSTR_LET) {
								op = src_op;
								arg1 = src_i->arg1;
								m2 = true;
							}
						} else if (src_op == INSTR_LET) {
							// If instruction uses register, do not replace with instruction that does not use it
							if (FlagOff(src_i->arg1->submode, SUBMODE_IN) && (m3 || !(FlagOn(arg1->submode, SUBMODE_REG) && FlagOff(src_i->arg1->submode, SUBMODE_REG))) ) {
								// Do not replace simple variable with array access
								if (!(arg1->mode == INSTR_VAR && src_i->arg1->mode == INSTR_ELEMENT)) {
									arg1 = src_i->arg1;
									m2 = true;
								}
							}
						}
					}

					arg2 = i->arg2;
					if (arg2 != NULL && !InVar(arg2) && arg2->src_i != NULL ) {					
						src_i = arg2->src_i;
						src_op = src_i->op;

						if (src_op == INSTR_LET) {
							// Do not replace register source with non-register
							if (!InVar(src_i->arg1) && (m3 || !(FlagOn(arg2->submode, SUBMODE_REG) && FlagOff(src_i->arg1->submode, SUBMODE_REG))) ) {
								// Do not replace simple variable with array access
								if (arg2->read == 1 || !(arg2->mode == INSTR_VAR && src_i->arg1->mode == INSTR_ELEMENT)) {
									if (src_i->arg1->mode != INSTR_ELEMENT || !CodeModifiesVar(src_i->next, i, src_i->arg1)) {
										arg2 = src_i->arg1;
										m2 = true;
									}
								}
							}
						}
					}

					// let x,x is always removed
					if (op == INSTR_LET && result == arg1) {
						goto delete_instr;
					}

					if (m2) {
						if (TransformInstr(&loc, op, result, arg1, arg2, "Arg replace")) {
							modified = true;
							break;
						}
					}
					m3 = true;

					//==== Try to evaluate constant instructions
					// We first try to traverse the chain of assignments to it's root.
					// If there is IN variable on the road, we have to stop there
					// and replacing will not be performed.

					r = NULL;
					arg1 = SrcVar(i->arg1);
					arg2 = SrcVar(i->arg2);
					r = InstrEvalAlgebraic(i->op, arg1, arg2);

					// We have evaluated the instruction, change it to LET <result>,r
					if (r != NULL) {
						if (VarIsAlias(i->result, r)) {
							goto delete_instr;
						} else {
							if (TransformInstr(&loc, INSTR_LET, i->result, r, NULL, "Const evaluation")) {
								modified = true;
							}
						}
					}

					/*
					===========================================
					Optimization: Use value already in register
					===========================================
					Replace
					::::::::::::
					let R, var
					...
					let n, var
					::::::::::::
					by
					::::::::::::
					let R, var
					...
					let n, R 
					::::::::::::
					in case R is register and var is not register.
					*/

					if (!VarIsReg(i->arg1) && !InVar(i->arg1)) {
						// If some register contains the value we need to set
						for(regi = 0; regi < CPU->REG_CNT; regi++) {
							r = CPU->REG[regi];
							if (!VarIsEqual(r, i->arg1)) {
								if (VarIsOffset(r, i->arg1, &diff)) {
									is_zero = IntEqN(&diff, 0);
									IntFree(&diff);
									if (is_zero) {
										if (TransformInstr(&loc, i->op, i->result, r, i->arg2, "Register reuse")) {
											modified = true;
											break;
										}
									}
								}
							}
						}
					}

					VarSetSrcInstr2(result, i);
					// Create dependency tree
					Dependency(i);

				} // BRANCH
			} else { // result != NULL

				// Convert reference to constant value in expression to constant text
				if (i->op == INSTR_VAR_ARG) {
					arg1 = i->arg1;
					if (arg1 != NULL) {
						while (!InVar(arg1) && arg1->src_i != NULL && arg1->src_i->op == INSTR_LET) arg1 = arg1->src_i->arg1;
					}
					if (VarIsIntConst(arg1)) {
						sprintf(buf, "%d", IntN(&arg1->n));
						if (TransformInstr(&loc, INSTR_STR_ARG, i->result, VarNewStr(buf), VarInt(StrLen(buf)), "Arg to const")) {
							modified = true;
						}
					}
				}
			}
		}
	} // block
	return modified;
}

void CheckValues(Var * proc)
/*
Purpose:
	Check procedure before translation and perform some basic optimizations.
*/
{
	Instr * i, * i2;
	UInt32 n;
	Var * r, * result, * arg1, * arg2, * zero, * nonzero;
	InstrBlock * blk;
	InstrOp op;

	VarUse();
	for(blk = proc->instr; blk != NULL; blk = blk->next) {
		ResetValues();
		n = 1;
		for(i = blk->first; i != NULL; i = i->next, n++) {

			op = i->op;
			// Line instructions are not processed.
			if (op == INSTR_LINE) continue;

			// Try to convert compare with non-zero to compare versus zero
			// If we compare variable that has only two possible values and one of them is 0, we want
			// to always compare with the zero, because on most platforms, that test is more effective.

			if (op == INSTR_IFEQ || op == INSTR_IFNE) {
				arg1 = i->arg1;
				arg2 = i->arg2;
				if (!VarEq(arg2, ZERO) /*VarIsIntConst(arg2) && arg2->n != 0*/ && VarIsZeroNonzero(arg1, &zero, &nonzero)) {
					i->op = OpNot(i->op);
					i->arg2 = zero;
				}
			}

			result = i->result;
			if (result != NULL && !VarIsLabel(result)) {
				arg1 = SrcVar(i->arg1);
				arg2 = SrcVar(i->arg2);

				r = InstrEvalConst(op, arg1, arg2);

				// We have evaluated the instruction, change it to LET <result>,r
				if (r != NULL) {
					i->op = INSTR_LET;
					i->arg1 = r;
					i->arg2 = NULL;

				//OPTIMIZATION: Instruction merging
				// Try to convert
				//      add R1, Y, #c1
				//      add R2, R1, #c2
				//
				// We convert it to:
				//      add R1, Y, #c1
				//      add R2, Y, #c1+#c2
				} else {
					if (i->op == INSTR_ADD && VarIsConst(arg2)) {

						// Get source instruction or directly previous instruction
						i2 = arg1->src_i;
						if (i2 == NULL) {
							i2 = i->prev; while(i2 != NULL && i2->op == INSTR_LINE) i2 = i2->prev;
						}
						if (i2 != NULL && i2->result == arg1) {
							if (i2->op == INSTR_ADD) {
								if (VarIsConst(i2->arg2)) {
									if (result != arg1) {
										r = InstrEvalConst(i->op, arg2, i2->arg2);
										i2->arg2 = r;
										i->arg1 = i2->arg1;
										i->arg2 = r;

										// We don't need the first instruction anymore
										if (result->read == 1) {
											InstrDelete(blk, i2);
										}
									}
								}
							}
						}
					}
				}

				ResetValue(i->result);

				if (i->op == INSTR_LET) {
					result->src_i = i;
				}
			}
		}
	}
}

Bool OptimizeVarMerge(Var * proc)
/*
==========================
Optimization: Back merging
==========================
Optimize instruction sequences like:
:::::::::::
let a,10
...
let b,a
dead a
::::::::::::
to
::::::::::::
let b,10
...
::::::::::::

*/{

	Instr * i, * i2, ni;
	Var * result, * arg1;
	InstrBlock * blk;
	InstrOp op;
	Bool modified = false;
	Int16 q;
	UInt32 n;
	UInt8 color;

//	Print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n");
//	PrintProc(proc);

	for (blk = proc->instr; blk != NULL; blk = blk->next) {
		for (i = blk->first, n = 1; i != NULL; i = i->next, n++) {
next:
			if (i == NULL) break;
			op = i->op;
			if (op == INSTR_LET || op == INSTR_LET_ADR || op == INSTR_HI || op == INSTR_LO) {
				result = i->result;
				arg1   = i->arg1;
				if (!OutVar(result) && VarIsFixed(arg1) && i->next_use[1] == NULL) {
					for /*test*/ (i2 = i->prev; i2 != NULL; i2 = i2->prev) {

						if (i2->op == INSTR_LINE) continue;

						// Test, that it is possible to translate the instruction using new register
						// (and possibly new instruction - we may change let to let_adr etc.)

						memcpy(&ni, i2, sizeof(Instr));
						if (ni.op == INSTR_LET || ni.op == INSTR_LET_ADR || ni.op == INSTR_HI || ni.op == INSTR_LO) ni.op = op;						
						q = InstrTestReplaceVar(&ni, arg1, result);
						if (q != 0 && InstrRule(&ni) == NULL) break;

						// We successfully found source instruction, this means we can replace arg1 with result
						if (i2->result == arg1 && !VarUsesVar(i2->arg1, arg1) && !VarUsesVar(i2->arg2, arg1)) {

							//==== We have succeeded, replacing the register
							
							if (Verbose(proc)) {
								color = PrintColor(OPTIMIZE_COLOR);
								PrintFmt("Merging %ld#%ld", blk->seq_no, n); InstrPrint(i);
								PrintColor(color);
							}

							modified = true;

							for (;i2 != i; i2 = i2->next) {
								if (i2->op == INSTR_LINE) continue;
								q = InstrReplaceVar(i2, arg1, result);
								if (q > 0) {
									i2->rule = InstrRule(i2);
									if (i2->rule == NULL) {
										InternalError("Instruction in merge is not valid");
									}
								}
							}
							i = InstrDelete(blk, i);
							goto next;
						}
						if (i2->op == INSTR_CALL) break;
						if (InstrUsesVar(i2, result)) break;

					} /* test */
				}
			}
		}
	}

	if (modified && Verbose(proc)) {
		PrintProc(proc);
	}
	return modified;
}
