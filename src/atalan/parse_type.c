#include "language.h"

void ExpectExpression(Var * result);
void ParseEnumItems(Type * type, UInt16 column_count);
void ParseAssign(InstrOp mode, VarSubmode submode, Type * to_type);
Var * ParseVariable();
BigInt * ParseIntConstExpression(Var * result);

Type * ParseType();
Type * ParseSubtype();
UInt8 ParseArgNo();

Bool PARSE_INLINE;

void ParseArgList(VarSubmode mode, Type * to_type)
/*
Purpose:
	Parse block with list of arguments.
	  [">" | "<"] assign
	Arguments are added to current context with submode SUBMODE_ARG_*.

	This method is used when parsing procedure or macro argument declaration or structure declaration.
*/
{
	VarSubmode submode = SUBMODE_EMPTY;
	Var * var, * adr;
	Bool out_part = false;

 	EnterBlockWithStop(TOKEN_EQUAL);			// TOKEN_EQUAL

	while (TOK != TOKEN_ERROR && !NextIs(TOKEN_BLOCK_END)) {

		if (!out_part && NextIs(TOKEN_RIGHT_ARROW)) {
			out_part = true;
		}

		submode = mode;

		if (out_part) {
			submode = SUBMODE_ARG_OUT;
		} else {

			if (NextIs(TOKEN_LOWER)) {
				submode = SUBMODE_ARG_IN;
			}
			if (NextIs(TOKEN_HIGHER)) {
				submode = SUBMODE_ARG_OUT;
			}
		}

		// Variables preceded by @ define local variables used in the procedure.
		if (NextIs(TOKEN_ADR)) {
			adr = ParseVariable();
			if (TOK) {
				var = VarAllocScopeTmp(to_type->owner, INSTR_VAR, adr->type);
				var->adr  = adr;
				NextIs(TOKEN_EOL);
				continue;
			}
		}

		if (TOK == TOKEN_ID) {
			ParseAssign(INSTR_VAR, submode, to_type);
			NextIs(TOKEN_COMMA);
			NextIs(TOKEN_EOL);
		} else {
			SyntaxError("Expected variable name");
		}
	}
}

Type * TypeAllocVar(Var * var)
{
	Type * type;
	type = TypeAlloc(TYPE_VAR);
	type->typevar = var;
	return type;
}

Type * TypeAllocArg(UInt8 arg_no, Type * arg_type)
{
	Type * type;

	type = TypeAlloc(TYPE_ARG);
	type->arg_no = arg_no;
	type->arg_type = arg_type;
	return type;
}

Type * ParseIntType()
{
	Type * type = TUNDEFINED;
	Var * var;
	Bookmark bookmark;
	UInt8 arg_no = 0;
	BigInt * bi;

	// When parsing rule, type may be preceded by %name:
	if (ParsingPattern()) {
		arg_no = ParseArgNo();
		if (arg_no > 0) {
			if (TOK == TOKEN_COLON) {
				NextToken();
			} else {
				SyntaxError("Argument name must be followed by :");
			}
		}
	}

	bookmark = SetBookmark();
	ExpectExpression(NULL);
	if (TOK) {
		var = BufPop();

		// If we parsed rule argument
		if (var->mode == INSTR_ELEMENT || VarIsRuleArg(var)) {
			type = TypeAllocVar(var);
			goto done;
		// Integer type may be defined using predefined type definition or be defined as type of other variable
		} else if (var->mode == INSTR_TYPE || var->mode == INSTR_VAR) {
			type = var->type;

			if (type->variant == TYPE_TYPE) {
				type = var->type_value;
				SetFlagOn(var->submode, SUBMODE_USED_AS_TYPE);
			}

			if (type->variant != TYPE_INT) {
				SyntaxErrorBmk("Expected integer type", bookmark);
			}
			goto done;
		} else if (var->mode == INSTR_INT) {
			type = TypeAllocConst(&var->n);
		} else {
			//TODO: If simple integer variable, use it as type range
			SyntaxErrorBmk("expected type or constant expression", bookmark);
		}

		if (TOK == TOKEN_DOTDOT) {
			NextToken();
			bookmark = SetBookmark();
			ExpectExpression(NULL);
			if (TOK) {
				var = BufPop();
				bi = VarIntConst(var);
				if (bi != NULL) {
					IntModify(&type->range.max, bi);
				} else {
					SyntaxErrorBmk("expected constant expression", bookmark);
				}
			}
		} else {
			type->range.max = type->range.min;
			type->range.min = 0;
		}

		if (type->range.min > type->range.max) {
			SyntaxErrorBmk("range minimum bigger than maximum", bookmark);
		}
	}
done:

	if (TOK) {
		if (arg_no != 0) {
			type = TypeAllocArg(arg_no, type);
		}
	}
/*
	if (TOK == TOKEN_INT) {
		type = TypeAlloc(TYPE_INT);
		type->range.min = LEX.n;
		NextToken();
		if (TOK == TOKEN_DOTDOT) {
			NextToken();
			if (TOK == TOKEN_INT) {
				type->range.max = LEX.n;
				NextToken();
			}
		} else {
			type->range.max = type->range.min;
			type->range.min = 0;
		}

		if (type->range.min > type->range.max) {
			SyntaxError("range minimum bigger than maximum");
		}
	// Sme variable
	} else if (TOK == TOKEN_ID) {
		var = VarFind2(NAME);
		if (var != NULL) {
			if (var->mode == INSTR_INT) {
				if (var->type->variant == TYPE_INT) {
					type = TypeAlloc(TYPE_INT);
					type->range.min = 0;
					type->range.max = var->n;
				} else {
					SyntaxError("Expected integer constant");
				}
			} else {
				type = var->type;
			}
			NextToken();
		} else {
			SyntaxError("$unknown variable");
		}
	} else {
		SyntaxError("Expected definition of integer type");
	}
*/
	return type;
}

void ParseEnumStruct(Type * type)
{
	Var * var;
	UInt16 column_count = 0;

 	EnterBlockWithStop(TOKEN_EQUAL);			// TOKEN_EQUAL

	while (TOK != TOKEN_ERROR && !NextIs(TOKEN_BLOCK_END)) {

		if (TOK == TOKEN_ID) {
			ParseAssign(INSTR_VAR, SUBMODE_EMPTY, type);
			NextIs(TOKEN_COMMA);
			NextIs(TOKEN_EOL);
		} else {
			SyntaxError("Expected variable name");
		}
	}

	// Convert parsed fields to constant arrays
	FOR_EACH_LOCAL(type->owner, var)
		var->type = TypeArray(type, var->type);		
//		var->type->step = TypeSize(var->type->element);
		var->mode = INSTR_INT;
		column_count++;
	NEXT_LOCAL

	NextIs(TOKEN_HORIZ_RULE);

	ParseEnumItems(type, column_count);
}

Type * ParseConstList(Type * type)
{
	Bool id_required;
	Var * var;
	BigInt last_n, * c;

	IntInit(&last_n, 0);

	EnterBlockWithStop(TOKEN_VOID);
		
	id_required = false;

	while (TOK != TOKEN_ERROR && !NextIs(TOKEN_BLOCK_END)) {

		while(NextIs(TOKEN_EOL));

		if (TOK == TOKEN_ID || (TOK >= TOKEN_KEYWORD && TOK <= TOKEN_LAST_KEYWORD)) {
			var = VarAllocScope(NO_SCOPE, INSTR_CONST, NAME, 0);
			NextToken();
			if (NextIs(TOKEN_EQUAL)) {
				SyntaxError("Unexpected equal");
			}

			if (NextIs(TOKEN_COLON)) {
				c = ParseIntConstExpression(type->owner);
				if (TOK) {
					IntModify(&last_n, c);
				}
/*
				// Parse const expression
				if (TOK == TOKEN_INT) {
					last_n = LEX.n;
					NextToken();
				} else {
					SyntaxError("expected integer value");
				}
*/
			} else {
				IntAddN(&last_n, 1);
			}

			var->var = VarN(&last_n);

			if (type->owner != SCOPE) {
				type = TypeDerive(type);
			}

			TypeAddConst(type, var);

		} else {
			if (id_required) {
				SyntaxError("expected constant identifier");
			} else {
				ExitBlock();
				break;
			}
		}
		id_required = false;
		// One code may be ended either by comma or by new line
		if (NextIs(TOKEN_COMMA)) id_required = true;
		NextIs(TOKEN_EOL);
	}
	IntFree(&last_n);
	return type;
}

Type * ParseType3()
{
	Type * type = NULL, * variant_type = NULL;
	Type * elmt, * t;
	Var * var;
	BigInt * st;

	//# "type" restrict_type
	if (NextIs(TOKEN_TYPE2)) {
		variant_type = ParseType2(INSTR_VAR);
		type = TypeType(variant_type);
	//# "enum" ["struct"]
	} else if (NextIs(TOKEN_ENUM)) {
		type = TypeAlloc(TYPE_INT);
		type->range.flexible = true;
		type->is_enum        = true;
		if (NextIs(TOKEN_STRUCT)) {
			ParseEnumStruct(type);
		} else {
			type = ParseConstList(type);
		}

	//# "proc" args
	} else if (NextIs(TOKEN_PROC)) {
		type = TypeAlloc(TYPE_PROC);
		ParseArgList(SUBMODE_ARG_IN, type);
		if (TOK) {
			ProcTypeFinalize(type);
		}
	//# "macro" args
	} else if (NextIs(TOKEN_MACRO)) {

		type = TypeAlloc(TYPE_MACRO);
		ParseArgList(SUBMODE_ARG_IN, type);

	// Struct
	} else if (NextIs(TOKEN_STRUCT)) {
		type = TypeAlloc(TYPE_STRUCT);
		ParseArgList(SUBMODE_EMPTY, type);

	// String
	} else if (NextIs(TOKEN_STRING_TYPE)) {
		type = TypeAlloc(TYPE_STRING);

	// Array
	} else if (NextIs(TOKEN_ARRAY)) {		
		type = TypeAlloc(TYPE_ARRAY);
		t = NULL;

		if (TOK == TOKEN_OPEN_P) {
			EnterBlockWithStop(TOKEN_EQUAL);
			while (TOK != TOKEN_ERROR && !NextIs(TOKEN_BLOCK_END)) {
				elmt = ParseIntType();
				if (type->index == NULL) {
					type->index = elmt;
				} else if (t != NULL) {
					t->right = TypeTuple(t->right, elmt);
					t = t->right;
				} else {
					t = TypeTuple(type->index, elmt);
					type->index = t;
				}
				NextIs(TOKEN_COMMA);
			};
		}
		
		// If no dimension has been defined, use flexible array.
		// This is possible only for constants now.

		if (TOK) {
			if (type->index == NULL) {
				elmt = TypeAlloc(TYPE_INT);
				elmt->range.flexible = true;
				elmt->range.min = 0;
				type->index = elmt;
			}
		}

		// Element STEP may be defined
		if (TOK) {
			if (NextIs(TOKEN_STEP)) {
				ExpectExpression(NULL);
				if (TOK) {
					var = STACK[0];
					st = VarIntConst(var);
					if (st != NULL) {
						type->step = IntN(st);
					} else {
						SyntaxError("Expected integer constant");
					}
				}
			}
		}

		if (TOK) {
			if (NextIs(TOKEN_OF)) {
				type->element = ParseSubtype();
			} else {
				type->element = TypeByte();
			}
		}

		if (TOK) {
			if (type->step == 0) {
				type->step = TypeSize(type->element);
			}
		}

	} else if (NextIs(TOKEN_ADR2)) {
		elmt = NULL;
		if (NextIs(TOKEN_OF)) {
			elmt = ParseSubtype();
		}
		type = TypeAdrOf(elmt);
	}
	return type;
}
/*
Type * ParseIntRange(Type * type)
{
	Var * min, * max;
	Bookmark bookmark;

	bookmark = SetBookmark();
	ParseExpressionType(TypeType(NULL));
	if (TOK && TOP != 0) {
		min = BufPop();
		max = NULL;
		if (NextIs(TOKEN_DOTDOT)) {
			ExpectExpression(NULL);
			if (TOK) {
				max = BufPop();
				type = TypeDerive(type);
			}
		}

		if (VarIsIntConst(min) && VarIsIntConst(max)) {
			type = TypeAllocRange(min, max);

			if (type->range.min > type->range.max) {
				SyntaxErrorBmk("range minimum bigger than maximum", bookmark);
			}

		} else {
			SyntaxErrorBmk("expected type constant expression", bookmark);
		}
	}
	return type;
}
*/
Type * ParseType2(InstrOp mode)
/*
Purpose:
	Parse: <int> [".." <int>] | <var> | proc <VarList>
Input:
	mode	Type of variable for which we parse.
*/
{
	
	Var * var;
	Type * type = NULL, * variant_type = NULL;
	Var * min, * max;
	Bookmark bookmark;

next:
	type = ParseType3();
	if (!TOK) return NULL;

	if (type == NULL) {
		bookmark = SetBookmark();
		ParseExpressionType(TypeType(NULL));
		if (TOK && TOP != 0) {
			min = BufPop();
			max = NULL;
			if (NextIs(TOKEN_DOTDOT)) {
				ExpectExpression(NULL);
				if (TOK) {
					max = BufPop();
				}
			}

			type = NULL;
			if (max == NULL) {
				var = min;
				if (var->mode == INSTR_TYPE) {
					type = var->type;
				} else if (var->mode == INSTR_VAR && var->type->variant == TYPE_TYPE) {
					type = var->type_value;
					SetFlagOn(var->submode, SUBMODE_USED_AS_TYPE);
				}

				// This is directly type
				if (type != NULL) {
					// For integer type, constants may be defined
					if (type->variant == TYPE_INT) {
//						type = ParseIntRange(type);
						goto const_list;
					}
					goto done;
				}
				max = var;		
			}

			if (VarIsIntConst(min) && VarIsIntConst(max)) {
				type = TypeAllocRange(min, max);

				if (type->range.min > type->range.max) {
					SyntaxErrorBmk("range minimum bigger than maximum", bookmark);
				}

			} else {
				SyntaxErrorBmk("expected type constant expression", bookmark);
			}
		} else if (TOK == TOKEN_STRING) {
			type = TypeAlloc(TYPE_VAR);
			type->typevar = VarNewStr(NAME);
			NextToken();
		}
	}

const_list:
	// Parse type specific constants
	// There can be list of constants specified in block.
	// First thing in the block must be an identifier, so we try to open the block with this in mind.
	// We try to parse constants only for integer types (in future, we may try other numberic or string types)

	if (type != NULL && type->variant == TYPE_INT && !type->is_enum) {
		if (TOK != TOKEN_OR) {
			type = ParseConstList(type);
		}
	}
done:
	if (TOK) {
		if (variant_type != NULL) {
			variant_type->right = type;
			type = variant_type;
		}

		if (NextIs(TOKEN_OR)) {
			variant_type = TypeAlloc(TYPE_VARIANT);
			variant_type->left = type;
			goto next;
		}
	}
	return type;
}

Type * ParseType()
{
	PARSE_INLINE = false;
	return ParseType2(INSTR_VAR);
}

BigInt * ParseIntConstExpression(Var * result)
{
	BigInt * bi = NULL;
	Var * var;

	ParseExpression(result);
	if (TOK) {
		var = BufPop();
		bi = VarIntConst(var);
		if (bi == NULL) {
			SyntaxError("expected constant expression");
		}
	}
	return bi;
}

Type * ParseTypeInline() 
/*
Syntax: "+" full_type | "(" full_type ")" | normal_type |  identifier | int ".." exp | "-" int ".." exp
*/{
	Type * type = NULL;
	Var * var;
	UInt16 bookmark;
	BigInt * bi;

	PARSE_INLINE = true;

	if (TOK == TOKEN_OPEN_P || TOK == TOKEN_PLUS) {
		type = ParseType2(INSTR_TYPE);
	} else {
		type = ParseType3();
		if (!TOK) return NULL;
		if (type != NULL) return type;

		if (ParseArg(&var)) {
			type = TypeAllocVar(var);
		} else if (TOK == TOKEN_ID) {
			bookmark = SetBookmark();
			var = ParseVariable();
			if (TOK) {
				if (var->mode == INSTR_TYPE) {
					type = var->type;
				} else if (var->mode == INSTR_VAR && var->type->variant == TYPE_TYPE) {
					type = var->type_value;
				} else {
					ErrArg(var);
					SyntaxErrorBmk("Variable [A] does not define type.", bookmark);
				}
			}
		} else if (TOK == TOKEN_INT || TOK == TOKEN_MINUS) {
			type = TypeAlloc(TYPE_INT);
			bi = ParseIntConstExpression(NULL);
			if (TOK) {
				IntModify(&type->range.min, bi);
				if (NextIs(TOKEN_DOTDOT)) {
					bi = ParseIntConstExpression(NULL);
					if (TOK) {
						IntModify(&type->range.max, bi);
					}
				} else {
					IntModify(&type->range.max, &type->range.min);
				}
			}
		}
	}
	return type;
}

Type * ParseSubtype()
{
	if (PARSE_INLINE) {
		return ParseTypeInline();
	} else {
		return ParseType();
	}
}


/*
	} else if (TOK == TOKEN_ID) {
		var = ParseVariable();
		if (TOK != TOKEN_ERROR) {
			if (var->type->variant == TYPE_TYPE) {
				type = var->type_value;
			} else {
				if (mode == INSTR_TYPE) {
					type = TypeDerive(var->type);
					// For integer type, constants may be defined
					if (type->variant == TYPE_INT) goto const_list;
				} else {
					if (var->mode != INSTR_TYPE) {
						Print("xxx");
					}
					type = var->type;
				}
			}
		}
//	}	//TODO: We may solve the integer type as default, when there was no other type involved

//	} else if (TOK == TOKEN_INT || TOK == TOKEN_MINUS) {
*/