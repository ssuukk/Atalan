/*

Compiler instructions management

(c) 2010 Rudolf Kudla 
Licensed under the MIT license: http://www.opensource.org/licenses/mit-license.php


*/

#include "language.h"

extern Var * VARS;		// global variables
GLOBAL Instr NULL_INSTR;
GLOBAL Instr * InstrNull;

extern Var * MACRO_ARG[MACRO_ARG_CNT];

Var * VarGen(Var * t)
/*
Purpose:
	Alloc variable according to specified template.
*/
{
	Var * var = NULL;
	Var * scope;
	if (t == NULL) return NULL;
	if (t->mode == INSTR_ELEMENT) {
		scope = VarGen(t->adr);
		var = VarAllocScope(scope, INSTR_VAR, t->var->str, 0);
	} else if (VarIsConst(t)) {
		var = t;
	} if (VarIsRuleArg(t)) {
		var = MACRO_ARG[t->idx-1];
	}
	return var;
}

Type * TypeGen(Type * t)
{
	Type * type;
	Var * var, * idx;
	if (t == NULL) return t;
	type = t;
	if (t->variant == TYPE_VAR) {
		var = t->typevar;
		if (var->mode == INSTR_ELEMENT) {
			if (VarIsRuleArg(var->adr)) {
				idx = MACRO_ARG[var->adr->idx-1];
				type = idx->type;
			} else {
				type = var->type;
			}
			idx = var->var;
			if (StrEqual(idx->str, "idx")) {
				type = type->index;
			}
		} else {
			return var->type;
		}
	} else if (t->variant == TYPE_ARRAY) {
		type = TypeAlloc(TYPE_ARRAY);
		type->index = TypeGen(t->index);
		type->element = TypeGen(t->element);
		type->step   = t->step;
	}
	return type;
}

void InstrDecl(Instr * i)
{
	Type * type;
	Var * var, * proto;
	proto = i->arg1;
	type = TypeGen(proto->type);
	var = VarGen(i->arg1);
	var->type = type;
}

InstrInfo INSTR_INFO[INSTR_CNT] = {
	{ INSTR_NULL,        "",    "null", {TYPE_VOID, TYPE_VOID, TYPE_VOID}, 0, NULL },
	{ INSTR_VOID,        "nop", "void", {TYPE_VOID, TYPE_VOID, TYPE_VOID}, 0, NULL },
	{ INSTR_LET,         "let", "<-", {TYPE_ANY, TYPE_ANY, TYPE_VOID}, 0, NULL },

	{ INSTR_GOTO,        "goto", "", {TYPE_LABEL, TYPE_VOID, TYPE_VOID}, 0, NULL },
	{ INSTR_IFEQ,        "ifeq", "=", {TYPE_LABEL, TYPE_ANY, TYPE_ANY}, 0, NULL },
	{ INSTR_IFNE,        "ifne", "<>", {TYPE_LABEL, TYPE_ANY, TYPE_ANY}, 0, NULL },
	{ INSTR_IFLT,        "iflt", "<=", {TYPE_LABEL, TYPE_ANY, TYPE_ANY}, 0, NULL },
	{ INSTR_IFGE,        "ifge", ">=", {TYPE_LABEL, TYPE_ANY, TYPE_ANY}, 0, NULL },
	{ INSTR_IFGT,        "ifgt", ">", {TYPE_LABEL, TYPE_ANY, TYPE_ANY}, 0, NULL },
	{ INSTR_IFLE,        "ifle", "<", {TYPE_LABEL, TYPE_ANY, TYPE_ANY}, 0, NULL },
	{ INSTR_IFOVERFLOW,  "ifoverflow", "over", {TYPE_LABEL, TYPE_VOID, TYPE_VOID}, 0, NULL },
	{ INSTR_IFNOVERFLOW, "ifnoverflow", "not over", {TYPE_LABEL, TYPE_VOID, TYPE_VOID}, 0, NULL },
	{ INSTR_IFTYPE,      "iftype", ":", {TYPE_LABEL, TYPE_ANY, TYPE_ANY}, 0, NULL },
	{ INSTR_IFNTYPE,     "ifntype", "<>:", {TYPE_LABEL, TYPE_ANY, TYPE_ANY}, 0, NULL },

	{ INSTR_PROLOGUE,    "prologue", "", {TYPE_VOID, TYPE_VOID, TYPE_VOID}, 0, NULL },
	{ INSTR_EPILOGUE,    "epilogue", "", {TYPE_VOID, TYPE_VOID, TYPE_VOID}, 0, NULL },
	{ INSTR_EMIT,        "emit", "", {TYPE_VOID, TYPE_VOID, TYPE_VOID}, 0, NULL },
	{ INSTR_VARDEF,      "vardef", "", {TYPE_ANY, TYPE_ANY, TYPE_VOID}, 0, NULL },
	{ INSTR_LABEL,       "label", "label", {TYPE_LABEL, TYPE_VOID, TYPE_VOID}, 0, NULL },

	{ INSTR_ADD,         "add", "+", {TYPE_ANY, TYPE_ANY, TYPE_ANY}, INSTR_COMMUTATIVE, NULL },
	{ INSTR_SUB,         "sub", "-", {TYPE_ANY, TYPE_ANY, TYPE_ANY}, 0, NULL },
	{ INSTR_MUL,         "mul", "*", {TYPE_ANY, TYPE_ANY, TYPE_ANY}, INSTR_COMMUTATIVE, NULL },
	{ INSTR_DIV,         "div", "/", {TYPE_ANY, TYPE_ANY, TYPE_ANY}, 0, NULL },
	{ INSTR_SQRT,        "sqrt", "sqrt", {TYPE_ANY, TYPE_ANY, TYPE_VOID}, 0, NULL },
	{ INSTR_AND,         "and", "bitand", {TYPE_ANY, TYPE_ANY, TYPE_ANY}, INSTR_COMMUTATIVE, NULL },
	{ INSTR_OR,          "or", "bitor", {TYPE_ANY, TYPE_ANY, TYPE_ANY}, INSTR_COMMUTATIVE, NULL },

	{ INSTR_ALLOC,       "alloc", "", {TYPE_ANY, TYPE_ANY, TYPE_ANY}, 0, NULL },
	{ INSTR_PROC,        "proc", "", {TYPE_ANY, TYPE_VOID, TYPE_VOID}, 0, NULL },
	{ INSTR_RETURN,      "return", "return", {TYPE_PROC, TYPE_VOID, TYPE_VOID}, 0, NULL },
	{ INSTR_ENDPROC,     "endproc", "", {TYPE_ANY, TYPE_VOID, TYPE_VOID}, 0, NULL },
	{ INSTR_CALL,        "call", "", {TYPE_PROC, TYPE_VOID, TYPE_VOID}, 0, NULL },
	{ INSTR_VAR_ARG,     "var_arg", "", {TYPE_VOID, TYPE_ANY, TYPE_ANY}, INSTR_NON_CODE, NULL },
	{ INSTR_STR_ARG,     "str_arg", "", {TYPE_VOID, TYPE_ANY, TYPE_VOID}, INSTR_NON_CODE, NULL },			// generate str (second argumend is string length -> to be removed)

	{ INSTR_DATA,        "data", "", {TYPE_VOID, TYPE_ANY, TYPE_VOID}, INSTR_NON_CODE, NULL },
	{ INSTR_FILE,        "file", "", {TYPE_VOID, TYPE_STRING, TYPE_VOID}, INSTR_NON_CODE, NULL },
	{ INSTR_ALIGN,       "align", "", {TYPE_VOID, TYPE_ANY, TYPE_VOID}, 0, NULL },
	{ INSTR_ORG,         "org", "", {TYPE_VOID, TYPE_ANY, TYPE_VOID}, 0, NULL },				// set the destination address of compilation
	{ INSTR_HI,          "hi", "hi", {TYPE_ANY, TYPE_ANY, TYPE_VOID}, 0, NULL },
	{ INSTR_LO,          "lo", "lo", {TYPE_ANY, TYPE_ANY, TYPE_VOID}, 0, NULL },
	{ INSTR_PTR,         "ptr", "", {TYPE_VOID, TYPE_ANY, TYPE_VOID}, INSTR_NON_CODE, NULL },
	{ INSTR_ARRAY_INDEX, "arrindex", "", {TYPE_VOID, TYPE_ANY, TYPE_VOID}, INSTR_NON_CODE, NULL },		// generate index for array
	{ INSTR_LET_ADR,     "let_adr", "=@", {TYPE_ADR, TYPE_ANY, TYPE_VOID}, 0, NULL },
	{ INSTR_ROL,         "rotl", "<<", {TYPE_ANY, TYPE_ANY, TYPE_ANY}, 0, NULL },				// bitwise rotate right
	{ INSTR_ROR,         "rotr", ">>", {TYPE_ANY, TYPE_ANY, TYPE_ANY}, 0, NULL },				// bitwise rotate left
	{ INSTR_DEBUG,       "debug", "", {TYPE_VOID, TYPE_VOID, TYPE_VOID}, 0, NULL },
	{ INSTR_MOD,         "mod", "mod", {TYPE_ANY, TYPE_ANY, TYPE_ANY}, 0, NULL },
	{ INSTR_XOR,         "xor", "bitxor", {TYPE_ANY, TYPE_ANY, TYPE_ANY}, INSTR_COMMUTATIVE, NULL },
	{ INSTR_NOT,         "not", "bitnot", {TYPE_ANY, TYPE_ANY, TYPE_VOID}, 0, NULL },

	{ INSTR_ASSERT_BEGIN,"assert_begin", "", {TYPE_VOID, TYPE_VOID, TYPE_VOID}, 0, NULL },
	{ INSTR_ASSERT,      "assert", "assert", {TYPE_VOID, TYPE_VOID, TYPE_VOID}, 0, NULL },
	{ INSTR_ASSERT_END,  "assert_end", "", {TYPE_VOID, TYPE_VOID, TYPE_VOID}, 0, NULL },

	{ INSTR_LINE,        "line", "", {TYPE_VOID, TYPE_VOID, TYPE_VOID}, 0, NULL },				// reference line in the source code
	{ INSTR_INCLUDE,     "include", "", {TYPE_VOID, TYPE_STRING, TYPE_VOID}, 0, NULL },
	{ INSTR_MULA,        "mula", "", {TYPE_ANY, TYPE_ANY, TYPE_VOID}, 0, NULL },				// templates for 8 - bit multiply 
	{ INSTR_MULA16,      "mula16", "", {TYPE_ANY, TYPE_ANY, TYPE_ANY}, 0, NULL },           // templates for 8 - bit multiply 

	{ INSTR_COMPILER,    "compiler", "", {TYPE_VOID, TYPE_ANY, TYPE_ANY}, 0, NULL },
	{ INSTR_CODE_END,    "code_end", "", {TYPE_VOID, TYPE_VOID, TYPE_VOID}, 0, NULL },			// end of BLK segment and start of data segment
	{ INSTR_DATA_END,    "data_end", "", {TYPE_VOID, TYPE_VOID, TYPE_VOID}, 0, NULL },			// end of data segment and start of variables segment
	{ INSTR_SRC_END,     "src_end", "", {TYPE_VOID, TYPE_VOID, TYPE_VOID}, 0, NULL },			// end of BLK segment and start of data segment
	{ INSTR_DECL,        "decl",     "decl", {TYPE_VOID, TYPE_ANY, TYPE_VOID}, 0, &InstrDecl },			// declare variable

	{ INSTR_VAR,         "", "", {TYPE_VOID, TYPE_VOID, TYPE_VOID}, 0, NULL },			// Variable (may be argument, "", "", {TYPE_VOID, TYPE_VOID, TYPE_VOID}, 0, NULL }, input, "", "", {TYPE_VOID, TYPE_VOID, TYPE_VOID}, 0, NULL }, output, "", "", {TYPE_VOID, TYPE_VOID, TYPE_VOID}, 0, NULL }, ...)
	{ INSTR_INT,       "", "", {TYPE_VOID, TYPE_VOID, TYPE_VOID}, 0, NULL },			// Integer constant
	{ INSTR_ELEMENT,     "", "#", {TYPE_VOID, TYPE_ANY, TYPE_ANY}, 0, NULL },			// <array> <index>     access array or structure element (left operand is array, "", "", {TYPE_VOID, TYPE_VOID, TYPE_VOID}, 0, NULL }, right is index)
	{ INSTR_BYTE,        "", "$", {TYPE_VOID, TYPE_ANY, TYPE_ANY}, 0, NULL },			// <var> <byte_index>  access byte of specified variable
	{ INSTR_RANGE,       "", "..", {TYPE_VOID, TYPE_ANY, TYPE_ANY}, 0, NULL },			// x..y  (l = x, "", "", {TYPE_VOID, TYPE_VOID, TYPE_VOID}, 0, NULL }, r = y) Used for slice array references
	{ INSTR_TUPLE,       "", ",", {TYPE_VOID, TYPE_ANY, TYPE_ANY}, 0, NULL },			// { INSTR_LIST <adr, "", "", {TYPE_VOID, TYPE_VOID, TYPE_VOID}, 0, NULL },var>  (var may be another tuple)
	{ INSTR_DEREF,       "", "@", {TYPE_VOID, TYPE_ANY, TYPE_VOID}, 0, NULL },			// dereference an address (var contains reference to dereferenced adr variable, "", "", {TYPE_VOID, TYPE_VOID, TYPE_VOID}, 0, NULL }, type is type in [adr of type]. Byte if untyped adr is used.
	{ INSTR_FIELD,       "", ".", {TYPE_VOID, TYPE_ANY, TYPE_VOID}, 0, NULL },			// access field of structure
	{ INSTR_TYPE,        "", "", {TYPE_VOID, TYPE_VOID, TYPE_VOID}, 0, NULL },
	{ INSTR_SCOPE,       "", "", {TYPE_VOID, TYPE_VOID, TYPE_VOID}, 0, NULL },
	{ INSTR_SRC_FILE,    "", "", {TYPE_VOID, TYPE_VOID, TYPE_VOID}, 0, NULL },			//{ INSTR_SRC_FILE variable representing source file
	{ INSTR_BIT,         "", "%", {TYPE_VOID, TYPE_ANY, TYPE_ANY}, 0, NULL },			// <var> <bit_index>  access bits of specified variable
	{ INSTR_TEXT,        "text", "", {TYPE_VOID, TYPE_VOID, TYPE_VOID}, 0, NULL },		// Text constant
	{ INSTR_VARIANT,     "variant", "", {TYPE_VOID, TYPE_ANY, TYPE_ANY}, 0, NULL },		// one of the two values (set)
	{ INSTR_CONST,       "const", "", {TYPE_VOID, TYPE_VOID, TYPE_VOID}, 0, NULL },		// Text constant

};


void InstrExecute(InstrBlock * blk)
{
	Instr * i;
	InstrInfo * ii;
	while(blk != NULL) {
		for(i = blk->first; i != NULL; i = i->next) {
			ii = &INSTR_INFO[i->op];
			if (ii->execute_fn != NULL) {
				ii->execute_fn(i);
			}
		}
		blk = blk->next;
	}
}

/*

Instruction generating function always write to this block.
Blocks may be nested, because procedures may be nested.

*/

InstrBlock * InstrBlockAlloc()
{
	return MemAllocStruct(InstrBlock);
}

void InstrBlockFree(InstrBlock * blk)
{
	Instr * i, * next;

	if (blk != NULL) {
		for(i = blk->first; i != NULL; i = next) {
			next = i->next;
			InstrFree(i);
		}
		blk->first = blk->last = NULL;
	}
}

UInt32 InstrBlockInstrCount(InstrBlock * blk)
/*
Purpose:
	Return number of instructions in the block.
*/
{
	Instr * i;
	UInt32 n = 0;
	if (blk != NULL) {
		for(i = blk->first; i != NULL; i = i->next) n++;
	}
	return n;
}

Bool InstrEquivalent(Instr * i, Instr * i2)
/*
Purpose:
	Return true, if two instructions are equivalent (executing them has same effect).
*/
{
	Bool eq;

	if (i == NULL || i2 == NULL) return false;
	if (i->op != i2->op) return false;
	
	eq = true;

	if (eq && INSTR_INFO[i->op].arg_type[RESULT] != TYPE_VOID) {
		eq = VarIsEqual(i->result, i2->result);
	}

	// Arguments must be same

	if (eq && INSTR_INFO[i->op].arg_type[ARG1] != TYPE_VOID) {
		eq = VarIsEqual(i->arg1, i2->arg1);
	}

	if (eq && INSTR_INFO[i->op].arg_type[ARG2] != TYPE_VOID) {
		eq = VarIsEqual(i->arg2, i2->arg2);
	}

	// Commutative instruction may be equivalent, if the arguments are switched
	if (!eq && FlagOn(INSTR_INFO[i->op].flags, INSTR_COMMUTATIVE)) {
		eq = VarIsEqual(i->arg1, i2->arg2) && VarIsEqual(i->arg2, i2->arg1);
	}
	
	return eq;
}

InstrOp InstrFind(char * name)
{
	UInt16 i;
	InstrInfo * ii = &INSTR_INFO[1];

	ASSERT(INSTR_NULL == 0);

	for(i=1; i<= INSTR_CNT; i++, ii++) {
		if (ii->name != NULL && StrEqual(name, ii->name)) {
			ASSERT(i == ii->op);
			return (InstrOp)i;
		}
	}

	return INSTR_NULL;
}

char * OpSymbol(InstrOp op)
{
	return INSTR_INFO[op].symbol;
}

void InstrDetach(InstrBlock * blk, Instr * first, Instr * last)
{
	Instr * next;

	if (blk != NULL && first != NULL) {

		next = last->next;
		if (first->prev != NULL) {
			first->prev->next = next;
		} else {
			blk->first = next;
		}

		if (next != NULL) {
			next->prev = first->prev;
		} else {
			blk->last = first->prev;
		}

		first->prev = NULL;
		last->next  = NULL;
	}
}

void InstrAttach(InstrBlock * blk, Instr * before, Instr * first, Instr * last)
{
	if (first == NULL) return;
	if (before == NULL) {
		first->prev = blk->last;
		if (blk->last != NULL) {
			blk->last->next = first;
		}
		blk->last = last;
		if (blk->first == NULL) blk->first = first;
		last->next = NULL;
	} else {
		first->prev = before->prev;
		last->next = before;

		if (before->prev != NULL) {
			before->prev->next = first;
		} else {
			blk->first = first;
		}
		before->prev = first;
	}
}

void InstrMoveCode(InstrBlock * to, Instr * before, InstrBlock * from, Instr * first, Instr * last)
/*
Purpose:
	Move piece of code (list of instructions) from one block to another.
*/
{
	InstrDetach(from, first, last);
	InstrAttach(to, before, first, last);
}

void InstrFree(Instr * i)
{
	if (i != NULL) {
		if (i->op == INSTR_LINE) {
//			free(i->line);
		}
		free(i);
	}
}


Instr * InstrDelete(InstrBlock * blk, Instr * i)
/*
Purpose:
	Delete the instruction from specified block.
	Instruction MUST be part of the specified block.
	Return pointer to next instruction after the deleted instruction.
*/
{
	Instr * next = NULL;

	// use Detach
	if (blk != NULL && i != NULL) {

		next = i->next;

		if (i->prev != NULL) {
			i->prev->next = next;
		} else {
			blk->first = next;
		}

		if (next != NULL) {
			i->next->prev = i->prev;
		} else {
			blk->last = i->prev;
		}

		InstrFree(i);
	}
	return next;
}

Instr * InstrInsert(InstrBlock * blk, Instr * before, InstrOp op, Var * result, Var * arg1, Var * arg2)
/*
Purpose:
	Insert instruction before specified instruction in specified block.
	If before instruction is NULL, instruction is inserted to the end of block.
*/
{
	Instr * i = MemAllocStruct(Instr);
	i->op = op;
	i->result = result;
	i->arg1 = arg1;
	i->arg2 = arg2;
	i->line_pos = 0;

	InstrAttach(blk, before, i, i);

	return i;
}

InstrOp OpNot(InstrOp op)
{
	return op ^ 1;
}

InstrOp OpRelSwap(InstrOp op)
/*
Purpose:
	Change orientation of non commutative relational operators
	This is different from NOT operation.
*/{

	switch(op) {
	case INSTR_IFLE: op = INSTR_IFGE; break;
	case INSTR_IFGE: op = INSTR_IFLE; break;
	case INSTR_IFGT: op = INSTR_IFLT; break;
	case INSTR_IFLT: op = INSTR_IFGT; break;
	default: break;
	}
	return op;
}

Var * InstrEvalConst(InstrOp op, Var * arg1, Var * arg2)
/*
Purpose:
	Try to evaluate instruction.
	Instruction may be evaluated if it's arguments are constant or some algebraic simplification may be applied.
Result:
	Return result as an variable or NULL, if instruction can not be evaluated.
*/
{
	Var * r = NULL;
	BigInt * n1, * n2;
	BigInt nr;

	/*
	==============================
	Optimization: Constant folding
	==============================

	Evaluate the expressions, whose operands are known to be constant at compile time.

	*/

	n1 = VarIntConst(arg1);
	n2 = VarIntConst(arg2);

	if (n1 != NULL && (arg2 == NULL || n2 != NULL)) {
		
//		if (arg1->type == NULL || arg1->type->variant != TYPE_INT) return NULL;

		switch(op) {
			case INSTR_SQRT:
				if (IntHigherEq(n1, Int0())) {
					IntSqrt(&nr, n1);		// r = VarInt((UInt32)sqrt(*n1));
				} else {
					SyntaxError("sqrt of negative number");
					// Error: square root of negative number
				}
				break;
			case INSTR_LO:
				IntAnd(&nr, n1, Int255());
//				r = VarInt(*n1 & 0xff);
				break;
			case INSTR_HI:
				IntSet(&nr, n1);
				IntDivN(&nr, 256);
				IntAndN(&nr, 0xff);
//				r = VarInt((*n1 >> 8) & 0xff);
				break;
			case INSTR_DIV:
				if (!IntEqN(n1, 0)) {
					IntDiv(&nr, n1, n2);
//					r = VarInt(*n1 / *n);
				} else {
					SyntaxError("division by zero");
				}
				break;
			case INSTR_MOD:
				if (!IntEqN(n1, 0)) {
					IntMod(&nr, n1, n2);
//					r = VarInt(*n1 % *n);
				} else {
					SyntaxError("division by zero");
				}
				break;
			case INSTR_MUL:
				IntMul(&nr, n1, n2);
//				r = VarInt(*n1 * *n);
				break;
			case INSTR_ADD:
				IntAdd(&nr, n1, n2);
//				r = VarInt(*n1 + *n);
				break;
			case INSTR_SUB:
				IntSub(&nr, n1, n2);
//				r = VarInt(*n1 - *n);
				break;
			case INSTR_AND:
				IntAnd(&nr, n1, n2);
//				r = VarInt(*n1 & *n);
				break;
			case INSTR_OR:
				IntOr(&nr, n1, n2);
//				r = VarInt(*n1 | *n);
				break;
			case INSTR_XOR:
				IntXor(&nr, n1, n2);
//				r = VarInt(*n1 ^ *n);
				break;
			default: 
				return NULL;
				break;
		}
		r = VarN(&nr);
		IntFree(&nr);
	}
	return r;
}

Var * InstrEvalAlgebraic(InstrOp op, Var * arg1, Var * arg2)
{
	/*
	=======================================
	Optimization: Algebraic simplifications
	=======================================

	::::::::::::::::::::::
	add x, a, 0  ->  let x, a
	add x, 0, a  ->  let x, a
	sub x, a, 0  ->  let x, a
	mul x, a, 1  ->  let x, a
	mul x, 1, a  ->  let x, a
	div x, a, 1  ->  let x, a
	::::::::::::::::::::::

	*/
	switch(op) {
	case INSTR_MUL:
		if (VarIsN(arg1, 1)) 
			return arg2;
		// continue to INSTR_DIV
	case INSTR_DIV:
		if (VarIsN(arg2, 1)) return arg1;
		break;

	case INSTR_ADD:
		if (VarIsN(arg1, 0)) return arg2;
		// continue to INSTR_SUB
	case INSTR_SUB:
		if (VarIsN(arg2, 0)) return arg1;
		break;

	case INSTR_SQRT:
		if (VarIsN(arg1, 1) || VarIsN(arg1, 0)) return arg1;
		break;
	default: break;
	}

	return InstrEvalConst(op, arg1, arg2);
}


/****************************************************************

 Generating instructions

****************************************************************/


Int32 ByteMask(Int32 n)
/*
Purpose:
	Return power of $ff ($ff, $ffff, $ffffff or $ffffffff) bigger than specified number.
*/
{
	Int32 nmask;
	nmask = 0xff;
	while(n > nmask) nmask = (nmask << 8) | 0xff;
	return nmask;
}

/*
void GenArrayInit(Var * arr, Var * init)

Purpose:
	Generate array initialization loop.
Arguments:
	arr		Reference to array or array element or array slice

{
	Type * type, * src_type, * idx1_type;
	Var * idx, * src_idx, * label, * range, * stop, * label_done;
	Int32 nmask;
	BigInt stop_n;
	Var * min1, * max1, * src_min, * src_max;
	Var * dst_arr;

	src_idx = NULL;
	type = arr->type;
	label = VarNewTmpLabel();
	idx1_type = NULL;

	if (type->variant == TYPE_ARRAY) {
		idx1_type = type->index;
	}

	if (arr->mode == INSTR_ELEMENT) {

		// If this is array of array, we may need to initialize index variable differently
		
		if (arr->adr->mode == INSTR_ELEMENT) {
			idx1_type = arr->adr->type->index;
		} else if (arr->adr->mode == INSTR_VAR) {
			idx1_type = arr->adr->type->index;
		}

		range = arr->var;
		if (range->mode == INSTR_RANGE) {
			min1 = range->adr;
			max1 = range->var;
		} else {
			min1 = arr->var;
			max1 = VarInt(arr->adr->type->index->range.max);
		}
		dst_arr = arr->adr;
	} else {
		min1 = VarInt(type->index->range.min);
		max1 = VarInt(type->index->range.max);
		dst_arr = arr;
	}

	src_type = init->type;
	
	// This is copy instruction (source is array)
	if (src_type->variant == TYPE_ARRAY) {		
		src_min = VarInt(src_type->index->range.min);
		src_max = VarInt(src_type->index->range.max + 1);
		src_idx = VarNewTmp(src_type->index);
		init = VarNewElement(init, src_idx);
		label_done = VarNewTmpLabel();
	}

	if (max1->mode == INSTR_INT) {
		stop_n = max1->n;
	
		nmask = ByteMask(stop_n);
		if (nmask == stop_n) {
			stop_n = 0;
		} else {
			stop_n++;
		}
		stop = VarInt(stop_n);
	} else {
		stop = max1;
	}

//	idx1_type = TypeAlloc(TYPE_INT);
//	idx1_type->range.min = 
	idx = VarNewTmp(idx1_type);

	GenLet(idx, min1);
	if (src_idx != NULL) {
		GenLet(src_idx, src_min);
	}
	GenLabel(label);
	GenLet(VarNewElement(dst_arr, idx), init);
	if (src_idx != NULL) {
		Gen(INSTR_ADD, src_idx, src_idx, VarInt(1));
		Gen(INSTR_IFEQ, label_done, src_idx, src_max);
	}
	Gen(INSTR_ADD, idx, idx, VarInt(1));
	Gen(INSTR_IFNE, label, idx, stop);
	if (src_idx != NULL) {
		GenLabel(label_done);
	}
}
*/
/****************************************************************

 Print tokens

****************************************************************/

void PrintVarNameNoScope(Var * var)
{
	Print(VarName(var));
//	Print(var->name);
//	if (var->idx > 0) {
//		PrintInt(var->idx-1);
//	}
}

void PrintVarName(Var * var)
{
	UInt8 oc;

	if (VarIsReg(var)) {
		oc = PrintColor(GREEN+BLUE);
		PrintVarNameNoScope(var);
		PrintColor(oc);
	} else {
		if (var->scope != NULL && var->scope != &ROOT_PROC && var->scope->name != NULL && !VarIsLabel(var)) {
			PrintVarName(var->scope);
			Print(".");
		}
		PrintVarNameNoScope(var);
	}
}

void PrintVarNameUser(Var * var)
{
	Print(var->name);
	if (var->idx > 0) {
		PrintInt(var->idx-1);
	}
}

void PrintQuotedVarName(Var * var)
{
	PrintChar('\'');
	PrintVarNameUser(var);
	PrintChar('\'');
}


void PrintVarVal(Var * var)
{
	Type * type;
	UInt8 oc;

	if (var == NULL) return;

	if (var->mode == INSTR_DEREF) {
//	if (FlagOn(var->submode, SUBMODE_REF)) {
		Print("@");
		var = var->var;
	}

	if (var->name == NULL) {
		if (VarIsArg(var)) {
			Print("#"); PrintInt(var->idx-1);
		} else if (var->mode == INSTR_ELEMENT) {
			PrintVarVal(var->adr);
			if (var->adr->type->variant == TYPE_STRUCT) {
				Print(".");
				PrintVarNameNoScope(var->var);
			} else {
				Print("#");
				PrintVarVal(var->var);
//				Print("(");
//				index = var->var;
//				while(index->mode == INSTR_ELEMENT) {
//					PrintVarVal(index->adr);
//					Print(",");
//					index = index->var;
//				}
//				PrintVarVal(index);
//				Print(")");
			}
		} else if (var->mode == INSTR_BYTE) {
			PrintVarVal(var->adr);
			oc = PrintColor(GREEN+BLUE);
			Print("$");
			PrintColor(oc);
			PrintVarVal(var->var);

		} else if (var->mode == INSTR_TYPE) {
			PrintType(var->type);
		} else {
			if (var->mode == INSTR_RANGE) {
				PrintVarVal(var->adr); Print(".."); PrintVarVal(var->var);
			} else if (var->mode == INSTR_TUPLE) {
				Print("(");
				PrintVarVal(var->adr);
				Print(",");
				PrintVarVal(var->var);
				Print(")");
			} else if (var->mode == INSTR_INT) {
				PrintBigInt(&var->n);
			} else if (var->mode == INSTR_TEXT) {
				Print("'"); Print(var->str); Print("'");
			} else {
				Print("(");
				PrintVarVal(var->adr);
				oc = PrintColor(GREEN+BLUE);
				Print(INSTR_INFO[var->mode].symbol);
				PrintColor(oc);
				PrintVarVal(var->var);
				Print(")");
			}
		}
	} else {
		PrintVarName(var);

		if (var->type->variant == TYPE_LABEL) {
			if (var->instr != NULL) {
				Print(" (#");
				PrintInt(var->instr->seq_no);
				Print(")");
			}
		}

		if (var->adr != NULL) {
			if (var->adr->mode == INSTR_TUPLE) {
				if (!VarIsReg(var)) {
					Print("@");
					PrintVarVal(var->adr);
				}
			}
		}
	}

	type = var->type;
	if (type != NULL) {
		if (type->variant == TYPE_INT && !VarIsConst(var)) {
			if (type->range.min == 0 && type->range.max == 255) {
			} else {
//				Print(":%ld..%ld", type->range.min, type->range.max);
			}
		}
	}

}

void PrintVarArgs(Var * var)
{
	Var * arg;
	Int16 n = 0;
	Print("(");
	FOR_EACH_LOCAL(var, arg)
		if (VarIsArg(arg)) {
			if (n > 0) Print(", "); 
			Print(arg->name);
		}
		n++;
	NEXT_LOCAL
	Print(")");
}

void PrintVarUser(Var * var)
{
	if (var->mode == INSTR_ELEMENT) {
		PrintVarNameUser(var->adr); Print("("); PrintVarUser(var->var); Print(")");
	} else if (var->mode == INSTR_BYTE) {
		PrintVarNameUser(var->adr);
		Print("$");
		PrintVarUser(var->var);
	} else if (var->mode == INSTR_TYPE) {
		PrintVarNameUser(var);
		PrintType(var->type);
	} else {
		PrintVarNameUser(var);
	}
}

void PrintVar(Var * var)
{
	Type * type;

	if (var->mode == INSTR_DEREF) {
		Print("@");
		var = var->var;
	}
//	if (FlagOn(var->submode, SUBMODE_REF)) {
//		Print("@");
//	}

	if (var->mode == INSTR_ELEMENT) {
		PrintVarName(var->adr);
		Print("(");
		PrintVar(var->var);
		Print(")");
	} else if (var->mode == INSTR_BYTE) {
		PrintVarName(var->adr);
		Print("$");
		PrintVar(var->var);
	} else if (var->mode == INSTR_INT) {
		PrintBigInt(&var->n);
		return;
	} else if (var->mode == INSTR_TYPE) {
		PrintType(var->type);
	} else if (var->mode == INSTR_VAR) {

		PrintVarName(var);

		if (var->adr != NULL) {
			Print("@");
			PrintVarVal(var->adr);
		}

		type = var->type;
		if (type != NULL) {
			if (type->variant == TYPE_PROC) {
				Print(":proc");
				PrintVarArgs(var);
			} else if (type->variant == TYPE_MACRO) {
				Print(":macro");
				PrintVarArgs(var);
			}
		}

		if (VarIsConst(var)) {
			Print(" = "); PrintBigInt(&var->n);
		} else {
			type = var->type;
			if (type != NULL) {
				if (type->variant == TYPE_INT) {
					if (type->range.min == 0 && type->range.min == 255) {
					} else {
						Print(":"); PrintBigInt(&type->range.min); Print(".."); PrintBigInt(&type->range.max);
					}
				}
			}
		}
	} else {
		PrintVar(var->adr);
		Print(INSTR_INFO[var->mode].symbol);
		PrintVar(var->var);
	}
//	Print("  R%ld W%ld\n", var->read, var->write);
}

void InstrPrintInline(Instr * i)
{
//	Var * inop;
	Bool r = false, a1 = false;

	if (i->op == INSTR_LINE) {
		PrintColor(BLUE+LIGHT);
		PrintFmt("file: %s, line: %d instr: %s", i->result->name, i->line_no, i->line);
		PrintColor(RED+GREEN+BLUE);
	} else if (i->op == INSTR_LABEL) {
		PrintVarVal(i->result);
		Print("@");
	} else {
		PrintFmt("   %s", INSTR_INFO[i->op].name);
	
		if (i->result != NULL) {
			Print(" ");
			PrintVarVal(i->result);
			r = true;
		}

		if (i->arg1 != NULL) {
			if (r) {
				Print(", ");
			} else {
				Print(" ");
			}

			PrintVarVal(i->arg1);
		}

		if (i->arg2 != NULL) {
			Print(", ");
			PrintVarVal(i->arg2);
		}
	}
}

void InstrPrint(Instr * i)
{
	InstrPrintInline(i);
	Print("\n");
}

void PrintBlockHeader(InstrBlock * blk)
{
	PrintFmt("#%ld/  ", blk->seq_no);
	if (blk->label != NULL) {
		Print("    ");
		PrintVarVal(blk->label);
		Print("@");
	}
	Print("\n");
}

void PrintInstrLine(UInt32 n)
{
	PrintFmt("%3ld| ", n);
}

void PrintInferType(Type * type)
{
	UInt8 old_color;
	old_color = PrintColor(BLUE);
	if (type == NULL) {
		Print("???");
	} else {
		PrintType(type);
	}
	PrintColor(old_color);
}

void CodePrint(InstrBlock * blk, UInt32 flags)
{
	Instr * i;
	UInt32 n;
	InstrInfo * ii;

	while (blk != NULL) {
		n = 1;
		PrintBlockHeader(blk);
		for(i = blk->first; i != NULL; i = i->next, n++) {
			ii = &INSTR_INFO[i->op];

			PrintInstrLine(n);
			InstrPrintInline(i);
			
			if (FlagOn(flags, PrintInferredTypes) && i->op != INSTR_LINE) {
				Print("  ");
				if (IS_INSTR_BRANCH(i->op)) {
					Print("if ");
				} else if (i->op == INSTR_CALL) {
					Print("call ");
				} else {
					if (ii->arg_type[0] != TYPE_VOID && ii->arg_type[0] != TYPE_LABEL && ii->arg_type[0] != TYPE_PROC) {
						PrintInferType(i->type[0]);
						Print(" = ");
					}
				}
				if (ii->arg_type[1] != TYPE_VOID) {
					PrintInferType(i->type[1]);
				}
				if (ii->arg_type[2] != TYPE_VOID) {
					Print(" "); Print(ii->symbol); Print(" ");
					PrintInferType(i->type[2]);
				}
				if (IS_INSTR_BRANCH(i->op)) {
					Print(" goto ");
				}
				if (ii->arg_type[0] == TYPE_LABEL || ii->arg_type[0] == TYPE_PROC) {
					PrintVar(i->result);
				}

			}
			PrintEOL();
		}

		blk = blk->next;
	}
}

void PrintProc(Var * proc)
{
	PrintProcFlags(proc, 0);
}

void PrintProcFlags(Var * proc, UInt32 flags)
{
	CodePrint(proc->instr, flags);
}


//$I
void InstrInit()
{
	InstrOp op;

	MemEmptyVar(NULL_INSTR);
	NULL_INSTR.op = INSTR_NULL;
	InstrNull = &NULL_INSTR;

	for(op=INSTR_NULL; op < INSTR_CNT; op++) {
		ASSERT(INSTR_INFO[op].op == op);
	}
}

Bool InstrIsSelfReferencing(Instr * i)
/*
Purpose:
	Return true, if the instruction references itself.
	Self referencing instructions are instructions like:

	add x, x, 1 
	
	I.e. instructions, where result is one of the arguments.

	We must take tuples into consideration:

	add (a,c,z,n), (a,c), 4

	is self referencing too.

	add x(a), a, 3
*/
{
	if (i == NULL || i->result == NULL) return false;
	return VarModifiesVar(i->result, i->arg1) || VarModifiesVar(i->result, i->arg2);
}


void ForEachBlock(InstrBlock * blk, ProcessBlockFn process_fn, void * info)
{
	while(blk != NULL) {
		process_fn(blk, info);
		blk = blk->next;
	}
}

Bool ProcInstrEnum(Var * proc, Bool (*fn)(Loc * loc, void * data), void * data)
{
	Instr * i;
	Instr * next_i;
	InstrBlock * blk;
	Loc loc;

	loc.proc = proc;

	for(blk = proc->instr; blk != NULL; blk = blk->next) {
		loc.blk = blk;
		loc.n = 1;
		for(i = blk->first; i != NULL; i = next_i) {
			next_i = i->next;
			if (i->op != INSTR_LINE) {
				loc.i = i;
				if (fn(&loc, data)) return true;
			}
			loc.n++;
		}
	}
	return false;
}
