/*

Generator

(c) 2010 Rudolf Kudla 
Licensed under the MIT license: http://www.opensource.org/licenses/mit-license.php


This module contains functions, that are used to generate instructions.
Instructions are generated to specified procedure and block of code.

Instructions are always generated to some specified instruction block, which is located in some procedure.
Current position, where will be the next instruction generated is stored in GENLOC varaible.

*/

#include "language.h"

GLOBAL InstrBlock * BLK;
GLOBAL Instr *      INSTR;

GLOBAL InstrBlock * IBLOCK_STACK[128];
GLOBAL UInt16       IBLOCK_STACK_SIZE;

GLOBAL Var   ROOT_PROC;

RuleSet GEN_RULES;


void GenSetDestination(InstrBlock * blk, Instr * i)
/*
Purpose:
	Set block and instruction, before which will be generated next instruction.
*/
{
	BLK   = blk;
	INSTR = i;
}

void GenBegin()
/*
Purpose:
	Create new code block and let next instructions be generated into this block.
*/
{
	IBLOCK_STACK[IBLOCK_STACK_SIZE] = BLK;
	IBLOCK_STACK_SIZE++;
	BLK = InstrBlockAlloc();
	INSTR = NULL;
}

InstrBlock * GenEnd()
/*
Purpose:
	Pop last pushed code block.
*/
{
	InstrBlock * blk;
	blk = BLK;
	IBLOCK_STACK_SIZE--;
	BLK = IBLOCK_STACK[IBLOCK_STACK_SIZE];
	INSTR = NULL;
	return blk;
}

void GenRegisterRule(Rule * rule)
/*
Purpose:
	Register generator rule.
*/
{
	RuleSetAddRule(&GEN_RULES, rule);
}

void GenBlock(InstrBlock * blk)
/*
Purpose:
	Generate block of code.
	The code is attached to the generated output (copy is not made).
*/
{
	if (blk != NULL && blk->first != NULL) {
		blk->first->prev = BLK->last;
		blk->last->next  = NULL;
		if (BLK->last != NULL) {
			BLK->last->next = blk->first;
		}
		BLK->last = blk->last;
		if (BLK->first == NULL) {
			BLK->first = blk->first;
		}
		free(blk);	// free just block head
	}
}

LineNo CURRENT_LINE_NO;

void GenLine()
{
	char * line;
	UInt32 line_no;
	UInt16 line_len;

	// Generate LINE instruction.
	// Line instructions are used to be able to reference back from instructions to line of source code.
	// That way, we can report logical errors detected in instructions to user.

	if (PHASE == PHASE_PARSE && !ParsingRule() && !ParsingSystem() && CURRENT_LINE_NO != LINE_NO) {
		InstrInsert(BLK, INSTR, INSTR_LINE, NULL, NULL, NULL);
		line = LINE;
		line_no = LINE_NO;
		if (LINE_POS == 0 && PREV_LINE != NULL) {
			line = PREV_LINE;
			line_no--;
		}
		line_len = StrLen(line)-1;
		BLK->last->result    = SRC_FILE;
		BLK->last->line_no = line_no;
		BLK->last->line = StrAllocLen(line, line_len);
		CURRENT_LINE_NO = line_no;
	}
}

void GenInternal(InstrOp op, Var * result, Var * arg1, Var * arg2)
{
	Var * var;
	// For commutative or relational operations make sure the constant is the other operator
	// This simplifies further code processing.

	if (op == INSTR_ADD || op == INSTR_MUL || op == INSTR_OR || op == INSTR_AND || op == INSTR_XOR || IS_INSTR_BRANCH(op)) {
		if (op != INSTR_IFOVERFLOW && op != INSTR_IFNOVERFLOW) {
			if (VarIsConst(arg1)) {
				var = arg1; arg1 = arg2; arg2 = var;

				op = OpRelSwap(op);

			}
		}
	}
	InstrInsert(BLK, INSTR, op, result, arg1, arg2);
}

void Gen(InstrOp op, Var * result, Var * arg1, Var * arg2)
/*
Purpose:
	Generate instruction into current code block.
*/
{
	Rule * rule;
	GenLine();

	rule = RuleSetFindRule(&GEN_RULES, op, result, arg1, arg2);
	if (rule == NULL) {
		GenInternal(op, result, arg1, arg2);
	} else {
		GenMatchedRule(rule);
	}

}

void GenRule(Rule * rule, Var * result, Var * arg1, Var * arg2)
/*
Purpose:
	Generate CPU instruction translated using specified rule.
*/
{
	InstrInsert(BLK, INSTR, rule->op, result, arg1, arg2);
	BLK->last->rule = rule;
}

void GenPos(InstrOp op, Var * result, Var * arg1, Var * arg2)
{
	Instr * i;
	Gen(op, result, arg1, arg2);
	if (INSTR == NULL) {
		i = BLK->last;
	} else {
		i = INSTR->prev;
	}
	i->line_pos = OP_LINE_POS;
}

void GenLetPos(Var * result, Var * arg1)
{
	Type * rtype, * atype;

	rtype = result->type;
	atype = arg1->type;

	// TODO: We should test for chain of ADR OF ADR OF ADR ....
	//       Error should be reported when assigning address of incorrect type

	if (rtype != NULL && atype != NULL && rtype->variant == TYPE_ADR && atype->variant != TYPE_ADR) {
		GenPos(INSTR_LET_ADR, result, arg1, NULL);
	} else {
		GenPos(INSTR_LET, result, arg1, NULL);
	}
}

void GenLet(Var * result, Var * arg1)
{
	OP_LINE_POS = 0;
	GenLetPos(result, arg1);
}

void GenGoto(Var * label)
{
	if (label != NULL) {
		GenInternal(INSTR_GOTO, label, NULL, NULL);
	}
}

void GenLabel(Var * var)
{
	if (var != NULL) {
		if (var->type->variant == TYPE_UNDEFINED) {
			VarToLabel(var);
		}
		GenInternal(INSTR_LABEL, var, NULL, NULL);
	}
}

void GenLastResult(Var * var, Var * item)
/*
Purpose:
	Set result of last generated instruction.
*/
{
	Instr * i = BLK->last;
	if (i->op != INSTR_LABEL) {
		i->result = var;
	} else {
		GenLet(var, item);
	}
}

Var * GenArg(Var * macro, Var * var, Var ** args, VarSet * locals)
/*
Purpose:
	Find function (macro) argument or structure member.
*/
{

	Var * arg, * arr, * l, * r;
	UInt16 n;
	InstrOp op;

	if (var == NULL) return NULL;

	op = var->mode;

	// If this is element reference and either array or index is macro argument,
	// create new array element referencing actual array and index.
	
	if (op == INSTR_DEREF) {
		arg = GenArg(macro, var->var, args, locals);
		if (arg != var->var) {
			var = VarNewDeref(arg);
		}
	} else if (op == INSTR_ELEMENT || op == INSTR_BYTE || op == INSTR_BIT) {

		arr = GenArg(macro, var->adr, args, locals);

		if (arr != var->adr && var->var->mode == INSTR_TEXT) {
			var = VarField(arr, var->var->str);
		} else {
			arg = GenArg(macro, var->var, args, locals);	// index

			if (arr != var->adr || arg != var->var) {
				if (op == INSTR_ELEMENT) {
					var = VarNewElement(arr, arg);
				} else if (op == INSTR_BYTE) {
					var = VarNewByteElement(arr, arg);
				} else {
					var = VarNewBitElement(arr, arg);
				}
			}
		}
	} else if (op == INSTR_TUPLE || op == INSTR_SUB || op == INSTR_ADD) {
		l = GenArg(macro, var->adr, args, locals);
		r = GenArg(macro, var->var, args, locals);

		if (l != var->adr || r != var->var) {			
			var = InstrEvalConst(op, l, r);
			if (var == NULL) {
				var = VarNewOp(op, l, r);
			}
		}

	} else if (op == INSTR_VAR && VarIsArg(var)) {

		// Optimization for instruction rule
		if (VarIsRuleArg(var)) {
			return args[var->idx-1];
		}

		//TODO: Make more efficient mechanism for finding argument index
		n = 0;
		FOR_EACH_LOCAL(macro, arg)
			if (VarIsArg(arg)) {
				if (arg == var) return args[n];
				n++;
			}
		NEXT_LOCAL

	} else {

		// This is local variable in macro
		// All labels are local in macro
		if (VarIsLocal(var, macro) || VarIsLabel(var)) {
			arg = VarSetFind(locals, var);
			if (arg == NULL) {
				arg = VarAllocScopeTmp(NULL, op, var->type);
				VarSetAdd(locals, var, arg);
			}
			return arg;
		}
	}
	return var;
}

/*

When parsing macro, any assignment to temporary variable other than specified by argument is executed, instead of
generating it.

*/

extern Var * MACRO_ARG[MACRO_ARG_CNT];

void GenMacroParse(Var * macro, Var ** args)
/*
Purpose:
	Expand macro when parsing.
	This will generate line instruction if necessary.
*/
{
	GenLine();
	GenMacro(macro, args);
}

void GenMacro(Var * macro, Var ** args)
/*
Purpose:
	Generate instructions from specified macro or procedure.
	Macro arguments are replaced by variables specified in 'args' array.
	Variables and labels local to macro are replaced by copies with automatic names.
Argument:
	macro	Variable containing the macro to expand.
	args	Macro arguments (according to macro header).
*/{
	Instr * i, * i2;
	InstrOp op;
	Var * result, * arg1, * arg2, * r;
	VarSet locals;
	InstrBlock * blk;
	Bool local_result;

	blk = macro->instr;
	if (blk == NULL) return;

	VarSetInit(&locals);

	for(i = blk->first; i != NULL; i = i->next) {
		op = i->op;
		local_result = false;

		// In case, we are generating variable (means we are inlining a procedure), generate lines too
		if (op == INSTR_LINE) {
			if (macro->type->variant == TYPE_PROC) {
				InstrInsert(BLK, INSTR, INSTR_LINE, NULL, NULL, NULL);
				i2 = INSTR->prev;
				i2->result    = i->result;
				i2->line_no   = i->line_no;
				i2->line      = StrAlloc(i->line);
			}

		// Macro may contain NOP instruction, we do not generate it to result
		} else if (op != INSTR_VOID) {

			result = i->result;
			if (result != NULL) {
				// %Z variable is used as forced local argument.
				local_result = VarIsArg(result) && result->idx == ('Z' - 'A' + 1);
				if (!local_result) {
					result = GenArg(macro, result, args, &locals);
				}
			}
			arg1 = GenArg(macro, i->arg1, args, &locals);
			arg2 = GenArg(macro, i->arg2, args, &locals);

			arg1 = VarEvalConst(arg1);
			arg2 = VarEvalConst(arg2);

			// Try to evaluate constant instruction to prevent generating excess instructions.

			r = InstrEvalConst(op, arg1, arg2);
			if (r != NULL) {
				op = INSTR_LET; arg1 = r; arg2 = NULL;
			}

			// There can be temporary variable, which didn't get type, because macro argument type
			// was not known. We should derive types in a better way, but this hack
			// should suffice for now.

			if (result != NULL && result->type == NULL) result->type = arg1->type;

			// If we are setting the value to local variable, do not generate instruction

			if (local_result) {
				if (op == INSTR_LET) {
					args[25] = arg1;
				} else {
					SyntaxError("failed to evaluate constant");
				}
			} else {
				if (PHASE == PHASE_TRANSLATE) {
					if (!InstrTranslate3(op, result, arg1, arg2, 0)) {
						SyntaxError("Translation for instruction not found");
					}
				} else {
					GenInternal(op, result, arg1, arg2);
				}
			}

		}
	}
	VarSetCleanup(&locals);
}

void GenerateInit()
{

	Type * type;

	memset(&ROOT_PROC, 0, sizeof(ROOT_PROC));
	type = TypeAlloc(TYPE_PROC);
	ROOT_PROC.name = "root";
	ROOT_PROC.idx  = 0;
	ROOT_PROC.type = type;		//&ROOT_PROC_TYPE;
	ROOT_PROC.instr = NULL;

	// Initialize procedure used to evaluate rules and it's arguments (A-Z)

	InScope(&ROOT_PROC);

	BLK = NULL;

	// Alloc instruction block for root procedure.
	
	IBLOCK_STACK_SIZE = 0;
	GenBegin();
	IBLOCK_STACK_SIZE = 0;

	CURRENT_LINE_NO = 0;

	RuleSetInit(&GEN_RULES);

}
