#include "../common/common.h"

typedef struct VarTag Var;
typedef struct LocTag Loc;
typedef struct VarSetTag VarSet;
typedef struct RuleTag Rule;
typedef struct RuleArgTag RuleArg;

#define COLOR_ERROR (RED+LIGHT)
#define COLOR_WARNING (RED+GREEN+LIGHT)
#define COLOR_LINE_POS (RED+GREEN+BLUE)
#define COLOR_HINTS  (RED+GREEN+BLUE)

Bool Verbose(Var * proc);

typedef enum {
	PHASE_PARSE = 1,
	PHASE_TRANSLATE = 2,
	PHASE_OPTIMIZE = 3,
	PHASE_EMIT = 4
} CompilerPhase;

extern CompilerPhase PHASE;

/*************************************************************

 Lexer

*************************************************************/

typedef UInt16 LineNo;   
typedef UInt16 LinePos;  // 0 based position of character on line

#define UNDEFINED_LINE_POS 65535

typedef enum {
	TOKEN_VOID = -2,
	TOKEN_EOF  = -1,
	TOKEN_ERROR = 0,
	TOKEN_BLOCK_END,
	TOKEN_OUTDENT,
	TOKEN_ID,
	TOKEN_INT,
	TOKEN_STRING,

	// Single character tokens (any ascii character is token)

	TOKEN_EOL = 10,
	TOKEN_ADR = '@',
	TOKEN_EQUAL = '=',
	TOKEN_HIGHER = '>',
	TOKEN_LOWER = '<',
	TOKEN_COMMA = ',',
	TOKEN_COLON = ':',
	TOKEN_PERCENT = '%',
	TOKEN_OPEN_P  = '(',
	TOKEN_CLOSE_P = ')',
	TOKEN_PLUS = '+',
	TOKEN_MINUS = '-',
	TOKEN_MUL = '*',
	TOKEN_DIV = '/',
	TOKEN_DOT = '.',
	TOKEN_HASH = '#',
	TOKEN_DOLLAR = '$',
	TOKEN_BYTE_INDEX = TOKEN_DOLLAR,

	// Keyword tokens

	TOKEN_KEYWORD = 128,
	TOKEN_GOTO    = 128,
	TOKEN_IF,
	TOKEN_UNLESS,
	TOKEN_THEN,
	TOKEN_ELSE,
	TOKEN_PROC,
	TOKEN_RULE,
	TOKEN_MACRO,
	TOKEN_AND,
	TOKEN_OR,
	TOKEN_NOT,
	TOKEN_SQRT,
	TOKEN_WHILE,
	TOKEN_UNTIL,
	TOKEN_WHERE,
	TOKEN_CONST,
	TOKEN_ENUM,
	TOKEN_ARRAY,
	TOKEN_TYPE2,			// this should be TOKEN_TYPE, but there was conflict with windows.h include on Windows
	TOKEN_FILE,
	TOKEN_LO,
	TOKEN_HI,
	TOKEN_OF,
	TOKEN_FOR,
	TOKEN_IN,
	TOKEN_OUT,
	TOKEN_PARAM,
	TOKEN_INSTR,
	TOKEN_TIMES,
	TOKEN_ADR2,
	TOKEN_DEBUG,
	TOKEN_MOD,
	TOKEN_BITNOT,
	TOKEN_BITAND,
	TOKEN_BITOR,
	TOKEN_BITXOR,
	TOKEN_STRUCT,
	TOKEN_USE,
	TOKEN_REF,
	TOKEN_STEP,
	TOKEN_RETURN,
	TOKEN_SCOPE,
	TOKEN_SEQUENCE,
	TOKEN_ASSERT,
	TOKEN_EITHER,
	TOKEN_STRING_TYPE,
	TOKEN_LAST_KEYWORD = TOKEN_STRING_TYPE,

	// two character tokens
	TOKEN_LOWER_EQUAL,
	TOKEN_HIGHER_EQUAL,
	TOKEN_NOT_EQUAL,
	TOKEN_DOTDOT,
	TOKEN_RIGHT_ARROW,
	TOKEN_HORIZ_RULE

} Token;

#define KEYWORD_COUNT (TOKEN_LAST_KEYWORD - TOKEN_KEYWORD + 1)

typedef struct {
	FILE * file;
	char * line;
	char * prev_line;
	LineNo line_no;
	LinePos line_len;
	LinePos line_pos;
	Token   token;
	Int16   prev_char;
} ParseState;

typedef struct {
	UInt32   n;
	FILE * f;
	Bool   ignore_keywords;
} Lexer;

Bool SrcOpen(char * name, Bool parse_options);
void SrcClose();
FILE * FindFile(char * name, char * ext, char * path);

extern char NAME[256];

// This functions are used only by parser

void NextToken();
void NextStringToken();
void ExpectToken(Token tok);

typedef UInt16 Bookmark;
Bookmark SetBookmark();

Bool Spaces();
Bool NextCharIs(UInt8 chr);
Bool NextIs(Token tok);
Bool NextNoSpaceIs(Token tok);
void EnterBlock();
void EnterBlockWithStop(Token stop_token);
void ExitBlock();

void LexerInit();

#define MAX_LINE_LEN 32767

extern Lexer LEX;
extern Token TOK;
extern Token TOK_NO_SPACES;				// if the current token was not preceded by whitespaces, it is copied here

extern char   LINE[MAX_LINE_LEN+2];		// we reserve one extra byte for terminating EOL, one for 0
extern LineNo  LINE_NO;
extern UInt16  LINE_LEN;
extern UInt16  LINE_POS;
extern UInt16  TOKEN_POS;
extern char * PREV_LINE;
extern Var *  SRC_FILE;					// current source file
extern char PROJECT_DIR[MAX_PATH_LEN];
extern char SYSTEM_DIR[MAX_PATH_LEN];
char FILE_DIR[MAX_PATH_LEN];			// directory where the current file is stored
char FILENAME[MAX_PATH_LEN];
extern char PLATFORM[64];

typedef enum {
	INSTR_NULL = 0,
	INSTR_VOID,
	INSTR_LET,		// var, val

	INSTR_GOTO,
	INSTR_IFEQ,		// must be even!!!.
	INSTR_IFNE,
	INSTR_IFLT,
	INSTR_IFGE,
	INSTR_IFGT,
	INSTR_IFLE,
	INSTR_IFOVERFLOW,
	INSTR_IFNOVERFLOW,
	INSTR_IFTYPE,
	INSTR_IFNTYPE,

	INSTR_PROLOGUE,
	INSTR_EPILOGUE,
	INSTR_EMIT,
	INSTR_VARDEF,
	INSTR_LABEL,
	INSTR_ADD,
	INSTR_SUB,
	INSTR_MUL,
	INSTR_DIV,
	INSTR_SQRT,

	INSTR_AND,
	INSTR_OR,

	INSTR_ALLOC,
	INSTR_PROC,
	INSTR_RETURN,
	INSTR_ENDPROC,
	INSTR_CALL,
	INSTR_VAR_ARG,
	INSTR_STR_ARG,			// generate str
	INSTR_DATA,
	INSTR_FILE,
	INSTR_ALIGN,
	INSTR_ORG,				// set the destination address of compilation
	INSTR_HI,
	INSTR_LO,
	INSTR_PTR,
	INSTR_ARRAY_INDEX,		// generate index for array
	INSTR_LET_ADR,
	INSTR_ROL,				// bitwise rotate right
	INSTR_ROR,				// bitwise rotate left
	INSTR_DEBUG,
	INSTR_MOD,
	INSTR_XOR,
	INSTR_NOT,
	INSTR_ASSERT_BEGIN,
	INSTR_ASSERT,
	INSTR_ASSERT_END,

	INSTR_LINE,				// reference line in the source code
	INSTR_INCLUDE,
	INSTR_MULA,				// templates for 8 - bit multiply 
	INSTR_MULA16,           // templates for 8 - bit multiply 

	INSTR_COMPILER,
	INSTR_CODE_END,			// end of BLK segment and start of data segment
	INSTR_DATA_END,			// end of data segment and start of variables segment
	INSTR_SRC_END,
	INSTR_DECL,

	// Following 'instructions' are used in expressions
	INSTR_VAR,				// 59 Variable (may be argument, input, output, ...)
	INSTR_INT,				// 60 Integer constant
	INSTR_ELEMENT,			// 61 <array> <index>     access array or structure element (left operand is array, right is index)
	INSTR_BYTE,				// 62 <var> <byte_index>  access byte of specified variable
	INSTR_RANGE,			// 63 x..y  (l = x, r = y) Used for slice array references
	INSTR_TUPLE,			// 64 INSTR_LIST <adr,var>  (var may be another tuple)
						    // Type of tuple may be undefined, or it may be structure of types of variables in tuple
	INSTR_DEREF,			// 65 dereference an address (var contains reference to dereferenced adr variable, type is type in [adr of type]. Byte if untyped adr is used.
	INSTR_FIELD,			// 66 access field of structure
	INSTR_TYPE,				// 67
	INSTR_SCOPE,			// 68
	INSTR_SRC_FILE,			//INSTR_SRC_FILE variable representing source file
							// scope   FILE that includes (uses) this file
							// name    filename
							// n       parse state
	INSTR_BIT,
	INSTR_TEXT,				// text constant
	INSTR_VARIANT,
	INSTR_CONST,			// constant depending on type (array, procedure)
	INSTR_CNT
} InstrOp;


/*********************************************************

  Error reporting

**********************************************************/

extern UInt32 ERROR_CNT;
extern UInt32 LOGIC_ERROR_CNT;

void ErrArgClear();
void ErrArg(Var * var);
void SyntaxErrorBmk(char * text, Bookmark bookmark);
void SyntaxError(char * text);
void LogicWarning(char * text, Bookmark bookmark);
void LogicWarningLoc(char * text, Loc * loc);
void LogicError(char * text, Bookmark bookmark);
void LogicErrorLoc(char * text, Loc * loc);
void InternalError(char * text, ...);
void InternalErrorLoc(char * text, Loc * loc);
void Warning(char * text);
void EndErrorReport();

void InitErrors();

void PlatformError(char * text);

/*********************************************************

 Names

 Names are managed separatelly from variables.
 Variables do not have to be named, but if they are, they contain reference to a name.

*********************************************************/

//TODO: Names are no used yet...

typedef struct NameTag Name;

typedef struct NameTag {

	Name * next;			// next name
	char * name;

	// Position, where the name was declared

	Var *   file;			// file in which the name has been defined
	LineNo  line_no;		// line of number, on which the name has been defined
	LinePos line_pos;		// position on line at which the name has been declared
};

/*********************************************************

 Variables & types

*********************************************************/
//$V

/*

TODO: Type represented by variable

Types are currently defined using separate structure.
We would like to represent types using variables.

For example int (range) should be represented using INSTR_RANGE.

INSTR_INT   Represent type consisting of single value. (Type for constant).
              This may be used for named constants ( zero: 0 ).

INSTR_RANGE   Range given by two variables (min..max). 
              Currently only constants are supported, but we may use anything.
              

As a temporary measure, we use ::TYPE_VAR:: type. This represents type already implemented as variable.
In future, value stored in ::typevar:: field of this type will be directly stored in ::Var.type::.

*/

typedef struct TypeTag Type;
typedef struct InstrTag Instr;
typedef struct ExpTag Exp;
typedef struct InstrBlockTag InstrBlock;

typedef enum {
	TYPE_VOID = 0,
	TYPE_INT,
	TYPE_STRUCT,
	TYPE_PROC,
	TYPE_MACRO,
	TYPE_STRING,
	TYPE_ARRAY,
	TYPE_LABEL,		// label in code (all labels share same type
	TYPE_ADR,		// address (or reference)
	TYPE_VARIANT,
	TYPE_TUPLE,
	TYPE_UNDEFINED,
	TYPE_SCOPE,
	TYPE_SEQUENCE,	// numeric sequence
	TYPE_TYPE,
	TYPE_ANY,
	TYPE_ARG,		// type is argument
	TYPE_VAR		// type is specified by variable
} TypeVariant;

/*
TYPE_SEQUENCE

Step type is used in type inferring.
It describes how is a variable modified in loop, when type can not be directly defined.
It is partial type. It cannot be used to define.

*/

typedef struct {
	InstrOp op;			// step_type INSTR_ADD, INSTR_SUB, INSTR_MUL, INSTR_DIV, ...
	Type * step;		// type of argument (step value)
	Type * init;		// initial value of the step
	InstrOp compare_op;
	Type * limit;		// limit value of step (for ADD, MUL this is top value, for SUB, DIV this is bottom value)
} TypeSequence;


/*

Integer Limits

*/

#include "bigint.h"

#define INTLIMIT_MIN (-2147483647 - 1)		// To prevent error in some compilers parser
#define INTLIMIT_MAX 2147483647L

// min + N * mul  (<max)
typedef struct {
	Bool flexible;		// range has been fixed by user
	BigInt min;
	BigInt max;
} Range;

/*
 We may define constant as variable pointing to constant type. However, we need a way to define the actual value.
 This could be either done by special 'const' type or we may make setting the value to variable possible.
 In such case, the value must refer to type which refers back to it.

typedef struct {
	BigInt n;
} IntConst;

*/

#define MACRO_ARG_CNT 26

#define TypeUsed 1

struct TypeTag {
	TypeVariant  variant;	// int, struct, proc, array
	UInt16       flags;
	Bool         flexible;	// (read: inferenced)
	Bool		 is_enum;	// INSTR_INTEGER is enum
	Type * base;			// type, on which this type is based (may be NULL)
	Var * owner;			// original owner of this type
	union {
		// TYPE_INT
		Range range;
		// TYPE_ARRAY, TYPE_ADR
		struct {
			Type * index;
			Type * element;
			UInt16 step;					// Index will be multiplied by this value. Usually it is same as element size.
		};
		// TYPE_TUPLE, TYPE_VARIANT, TYPE_ADR (uses only left), TYPE_TYPE (uses only left)
		struct {
			Type * left;
			Type * right;
		};

		struct {
			UInt8 arg_no;
			Type * arg_type;
		};
		TypeSequence seq;
		// TYPE_VAR
		Var * typevar;
	};
};

/*

INSTR_DEREF

DEREF variables represent dereference of address variable.
Only address variables may be dereferenced.
For every adr variable, there may be at most one dereference variable.

If the dereferenced variable is of type ADR OF X, deref variable is of type X.

*/

typedef enum {
	SUBMODE_EMPTY = 0,

	// INSTR_VAR
	SUBMODE_IN = 1,
	SUBMODE_OUT = 2,		// even procedure may be marked as out. In such case, it has some side-effect.
	SUBMODE_REG = 4,		// variable is stored in register
	SUBMODE_IN_SEQUENCE = 8,
	SUBMODE_ARG_IN  = 16,
	SUBMODE_ARG_OUT = 32,
	SUBMODE_OUT_SEQUENCE = 64,

	// General
	SUBMODE_SYSTEM = 128,				// This is system variable (defined either by system or platform)
	SUBMODE_USER_DEFINED = 256,			// Type of this variable has been explicitly defined by user (programmer)
	SUBMODE_USED_AS_TYPE = 512,

	// INSTR_SRC_FILE
	SUBMODE_MAIN_FILE = 4,	// this flag is set for main source file (asm is not included for main file, because it is generated by compiler)
	SUBMODE_SYSTEM_FILE = SUBMODE_SYSTEM,

	// INSTR_INT
	SUBMODE_PARAM    = SUBMODE_IN,
	SUBMODE_UNDEFINED = SUBMODE_OUT		// undefined constant or option (neither constant nor option may be IN or OUT)
} VarSubmode;

//REF
//		Value of ptr is address
//
//
//Element is variable, that represents value in array i.e.  a(1)
//For multiple indices, elements are nested? a(1,2)  -> elmt(elmt(a, 1),2)
//Elements are practically always temporary.


#define VarLive            1
#define VarDead			   0		// this is in fact just 0 state of VarLive, it cannot be tested
#define VarUninitialized   2
#define VarLoop            4		// loop variable (incremented during loop)
#define VarLoopDependent   8		// variable is dependent (even transitively) on some loop variable
#define VarProcessed       16		// used when detecting, whether procedure is used or not

#define VarUsedInInterupt  32		// this procedure is used from interrupt
#define VarProcAddress     64		// procedure address is required (this means, we are not allowed to inline it)

#define VarUsed            16		// for register allocation

#define VarLabelDefined    32

typedef unsigned int VarIdx;
//typedef char * Name;

typedef UInt8 VarFlags;

struct VarTag {

	InstrOp	mode;
	VarSubmode submode;

	// Variable identification (name,idx,scope)
	char *	name;
	VarIdx  idx;	 // two variables with same name but different index may exist
					 // 0 means no index, 1 means index 1 etc.
					 // variable name "x1" is automatically converted to x,1
	Var  *  scope;	 // scope, in which this variable has been declared

	VarFlags  flags;
	Var *	adr;	 // Address of variable in memory. For INSTR_TYPE, this means alignment of variable of this type.
					 // INSTR_ELEMENT	Array to which this variable belongs
					 // INSTR_TUPLE      First variable of tuple

	Type *  type;	 // Type of variable

//	int     value_nonempty;
	// TODO: Replace value_nonempty just with flag VarDefined
	union {
		BigInt  n;
		//long	n;				// for const, or function default argument (other variants of value must be supported - array, struct, etc.)
		InstrBlock * instr;		// instructions for procedure or array initialization
		char * str;
		Var * var;
		ParseState * parse_state;	// INSTR_SRC_FILE
		Type * type_value;
	};

	Var *   file;			// file in which the variable has been defined
	LineNo  line_no;		// line of number, on which the variable has been defined
	LinePos line_pos;		// position on line at which the variable has been declared
	Instr * src_i;
	Exp *   dep;
	UInt16	read;			// how many times some instruction reads this variable (if 0, this is unused)
	UInt16	write;			// how many times some instruction writes this variable (if 1 this is constant)

	UInt16  set_index;		// index in current set

	Var  *  next;			// next variable in chain

	Var  *  next_in_scope;  // in future, this will be replaced by 'next'
	Var  *  subscope;
};

/*
TODO:

Const type variable

Type may be implemented as variable.

type	- parent type of the type
var     - set of possible values (for example range 3..4, etc.)

subscope - fields defining the type properties (for example index variable for array - we would prefer to specify index in some other way, but name # may be appropriate)

*/


/*

Variable address
================

Address of variable may be:

INSTR_INT          Integer defining location of variable in main memory.
MOVE_VAR            This variable is alias for the variable specified in adr.
INSTR_TUPLE          List of variables. One bigger variable may be defined as list of smaller variables.

INSTR_LABEL          Address.
                    Address alone may have address, which specifies memory bank, in which the address should be located.
					Address may be named. (For example labels).

*/

/*
Variables are managed in blocks.

Unused variable has INSTR_NULL and is in special scope.
Unused variables are kept in a list usinf next_in_scope.

*/

#define VAR_BLOCK_CAPACITY 10

typedef struct VarBlockTag VarBlock;

struct VarBlockTag {
	VarBlock * next;
	Var vars[VAR_BLOCK_CAPACITY];
};


#define MAX_ARG_COUNT 128

/*
INSTR_LINE is special instruction, which does not affect execution of program.
It marks place, where next line in source code begins.
All instructions after INSTR_LINE until next INSTR_LINE instruction were generated as a result
of parsing the line specified in instruction.

To save memory, INSTR_LINE argument are not pointers to ordinary variables.
result     pointer to INSTR_SRC_FILE variable, that defines source file
arg1       integer line number.
arg2       pointer to text of line


*/

typedef struct MemBlockTag MemBlock;

struct MemBlockTag {
	UInt32 adr;
	UInt32 size;
};

typedef struct MemHeapTag MemHeap;

struct MemHeapTag {
	UInt32 count;
	UInt32 capacity;
	MemBlock * block;
};

void HeapInit(MemHeap * heap);
void HeapCleanup(MemHeap * heap);
void HeapAddBlock(MemHeap * heap, UInt32 adr, UInt32 size);
void HeapRemoveBlock(MemHeap * heap, UInt32 adr, UInt32 size);
Bool HeapAllocBlock(MemHeap * heap, UInt32 size, UInt32 * p_adr);
void HeapAddType(MemHeap * heap, Type * type);

void HeapUnitTest();
void HeapPrint(MemHeap * heap);

#define DATA_SEGMENT          0x1000000
#define DATA_SEGMENT_CAPACITY 0x1000000

// Variables that do not fit into defined variable space are stored in data segment.
// Data segment is allocated directly after BLK segment, however we reserve dynamic space for it at
// specified address.


/*************************************************************

  Type

*************************************************************/

void TypeInit();		// initialize the Type subsytem

UInt8 IntByteSize(BigInt * n);

Type * TypeAlloc(TypeVariant variant);
Type * TypeAllocConst(BigInt * n);
Type * TypeAllocInt(BigInt * min, BigInt * max);
Type * TypeAllocIntN(Int32 min, Int32 max);
Type * TypeAllocRange(Var * min, Var * max);
Type * TypeAllocVar(Var * var);

Type * TypeDerive(Type * base);
Type * TypeCopy(Type * base);

Type * TypeType(Type * restriction);
Type * TypeByte();
Type * TypeLongInt();
Type * TypeScope();
Type * TypeTuple(Type * left, Type * right);
Type * TypeArray(Type * index, Type * element);

Type * TypeAdrOf(Type * element);

//UInt16 TypeItemCount(Type * type);
void TypeLimits(Type * type, Var ** p_min, Var ** p_max);

//void TypeLet(Type * type, Var * var);
typedef void (*RangeTransform)(BigInt * dest, BigInt * x, BigInt * tr);
//void TypeTransform(Type * type, Var * var, InstrOp op);

void TypeAddConst(Type * type, Var * var);
Bool TypeIsSubsetOf(Type * type, Type * master);
Bool TypeIsEqual(Type * left, Type * right);

Bool TypeIsInt(Type * type);
Bool TypeIsIntConst(Type * type);
Bool TypeIsN(Type * type, Int32 n);

BigInt * TypeMax(Type * type);
BigInt * TypeMin(Type * type);

Type * TypeUnion(Type * left, Type * right);

UInt32 TypeSize(Type * type);
UInt32 TypeAdrSize();
UInt32 TypeStructAssignOffsets(Type * type);

void ArraySize(Type * type, Var ** p_dim1, Var ** p_dim2);

void PrintType(Type * type);

//--- Proc type

void ProcTypeFinalize(Type * proc);
typedef Bool (*VarFilter)(Var * var);
void ProcLocalVars(Var * proc, VarSet * set, VarFilter filter_fn);


Bool VarMatchType(Var * var, Type * type);
Bool VarMatchesType(Var * var, Type * type);

void TypeInfer(Var * proc);

extern Type TVOID;
extern Type TINT;		// used for int constants
extern Type TSTR;
extern Type TLBL;
extern Type * TUNDEFINED;

#define NO_SCOPE ((Var *)1)

/**********************************************

 Variables

***********************************************/

extern Var * ZERO;
extern Var * ONE;

void VarInit();
void InitCPU();

char * VarName(Var * var);

Var * VarFirst();
#define VarNext(v) (v)->next

Var * InScope(Var * new_scope);
void ReturnScope(Var * prev);

void EnterLocalScope();
//void EnterSubscope(Var * new_scope);
void ExitScope();



Var * VarAllocUnused();
void VarSetScope(Var * var, Var * scope);

Var * VarInt(long n);
Var * VarN(BigInt * n);

Var * VarNewStr(char * str);
Var * VarNewLabel(char * name);
Var * FindOrAllocLabel(char * name, UInt16 idx);

void VarInitStr(Var * var, char * str);

Var * VarNewTmp(Type * type);
Var * VarNewTmpLabel();
Var * VarAlloc(InstrOp mode, char * name, VarIdx idx);
Var * VarAllocScope(Var * scope, InstrOp mode, char * name, VarIdx idx);
Var * VarAllocScopeTmp(Var * scope, InstrOp mode, Type * type);
Var * VarFind(char * name, VarIdx idx);
Var * VarFindScope(Var * scope, char * name, VarIdx idx);
Var * VarFindScope2(Var * scope, char * name);
Var * VarFind2(char * name);
//Var * VarFindInProc(char * name, VarIdx idx);
Var * VarProcScope();
Var * VarFindTypeVariant(char * name, VarIdx idx, TypeVariant type_variant);

Bool VarIsZeroNonzero(Var * var, Var ** p_zero, Var ** p_non_zero);

Var * VarRuleArg(UInt8 i);

Bool VarIsConst(Var * var);
Bool VarIsParam(Var * var);
Bool VarIsType(Var * var);
Bool VarIsIntConst(Var * var);
BigInt * VarIntConst(Var * var);

Var * VarUnion(Var * left, Var * right);
Var * VarNewVariant(Var * left, Var * right);

Bool VarIsN(Var * var, Int32 n);
Bool VarIsLabel(Var * var);
Bool VarIsArray(Var * var);
Bool VarIsValue(Var * var);
Bool VarIsTmp(Var * var);
Bool VarIsStructElement(Var * var);
Bool VarIsArrayElement(Var * var);
Bool VarIsReg(Var * var);
Var * VarReg(Var * var);
Bool VarIsFixed(Var * var);
Bool VarIsLocal(Var * var, Var * scope);
Bool VarIsInArg(Var * var);
Bool VarIsOutArg(Var * var);
Bool VarIsArg(Var * var);
Bool VarIsEqual(Var * left, Var * right);

Bool VarIsRuleArg(Var * var);

void VarCount(Var * var, BigInt * cnt);
void VarRange(Var * var, BigInt ** p_min, BigInt ** p_max);

Var * VarFindAssociatedConst(Var * var, char * name);

Var * VarField(Var * var, char * fld_name);
void VarLet(Var * var, Var * val);

#define InVar(var)  FlagOn((var)->submode, SUBMODE_IN)
#define OutVar(var) FlagOn((var)->submode, SUBMODE_OUT)

UInt32 VarByteSize(Var * var);

void VarResetRegUse();

TypeVariant VarType(Var * var);
long VarParamCount(Var * var);

void VarGenerateArrays();
void VarToLabel(Var * var);

Var * VarNewType(Type * type);

Var * FirstArg(Var * proc, VarSubmode submode);
Var * NextArg(Var * proc, Var * arg, VarSubmode submode);

Var * VarFirstLocal(Var * scope);
Var * VarNextLocal(Var * scope, Var * local);

Var * VarNewElement(Var * arr, Var * idx);
Var * VarNewByteElement(Var * arr, Var * idx);
Var * VarNewBitElement(Var * arr, Var * idx);
Var * VarNewDeref(Var * var);
Var * VarNewRange(Var * min, Var * max);
Var * VarNewTuple(Var * left, Var * right);
Var * VarNewOp(InstrOp op, Var * left, Var * right);

Var * VarEvalConst(Var * var);

Bool VarEq(Var * left, Var * right);

void VarResetUse();

void VarEmitAlloc();

#define FOR_EACH_VAR(v) for(v = VARS; v != NULL; v = v->next) {
#define NEXT_VAR }

#define FOR_EACH_LOCAL(SCOPE, VAR) 	for(VAR = VarFirstLocal(SCOPE); VAR != NULL; VAR = VarNextLocal(SCOPE, VAR)) {
#define NEXT_LOCAL }

#define FOR_EACH_ARG(SCOPE, VAR, MODE) 	for(VAR = FirstArg(SCOPE, MODE); VAR != NULL; VAR = NextArg(SCOPE, VAR, MODE)) {
#define NEXT_ARG }

#define FOR_EACH_IN_ARG(SCOPE, VAR) 	FOR_EACH_ARG(SCOPE, VAR, SUBMODE_ARG_IN)
#define NEXT_IN_ARG }

#define FOR_EACH_OUT_ARG(SCOPE, VAR) 	FOR_EACH_ARG(SCOPE, VAR, SUBMODE_ARG_OUT)
#define NEXT_OUT_ARG }

void PrintVar(Var * var);
void PrintVarName(Var * var);
void PrintVarUser(Var * var);
void PrintQuotedVarName(Var * var);
void PrintScope(Var * scope);

void ProcUse(Var * proc, UInt8 flag);

void ProcessUsedProc(void (*process)(Var * proc));

typedef struct {
	Var * key;
	Var * var;
} VarTuple;

typedef struct VarSetTag {
	VarTuple * arr;
	UInt16     count;
	UInt16     capacity;
};

#define VarSetCount(set) ((set)->count)

void VarSetInit(VarSet * set);
Var * VarSetFind(VarSet * set, Var * key);
void VarSetAdd(VarSet * set, Var * key, Var * var);
Var * VarSetRemove(VarSet * set, Var * key);
void VarSetEmpty(VarSet * set);
void VarSetCleanup(VarSet * set);
VarTuple * VarSetItem(VarSet * set, UInt16 index);

void VarSetPrint(VarSet * set);

Bool ProcIsInterrupt(Var * proc);

/***********************************************************

  CPU

***********************************************************/

#define MAX_CPU_REG_COUNT 64

typedef UInt8 RegIdx;

typedef struct {
	Var * SCOPE;
	Var * REG[MAX_CPU_REG_COUNT];		// Array of registers
	RegIdx REG_CNT;		// Count of registers
	Type * MEMORY;		// Array (adr) of cpu_word
} CPUType;

extern CPUType * CPU;

/***********************************************************

  Instructions

***********************************************************/

#define IS_INSTR_BRANCH(x) ((x)>=INSTR_IFEQ && (x)<=INSTR_IFNTYPE)
#define IS_INSTR_JUMP(x) (IS_INSTR_BRANCH(x) || (x) == INSTR_GOTO)

InstrOp OpNot(InstrOp op);
InstrOp OpRelSwap(InstrOp op);

#define FlagExpProcessed 1

/*
 Expression represents tree of expressions.
 op defines operation used to compute the result of the expression.
 INSTR_VAR represents expression representing value of variable or reference to variable.
*/

struct ExpTag {
	UInt8     flags;
	InstrOp   op;			// operation
	union {
		Exp * arg[2];		// op != INSTR_VAR
		Var * var;			// op == INSTR_VAR
	};
};

/*

Compiler instruction.

We use three-address instructions. in the form  result = arg1 op arg2.

*/

struct InstrTag {
	InstrOp op;
	Rule *  rule;		// after translation, this is pointer to rule defining the operator (may be NULL for INSTR_LINE)

	//--- dest
	Var * result;

	//TODO: Merge the union to arg1,arg2 & line_no, line
	union {
		Var * arg1;
		UInt16 line_no;
	};
	union {
		Var * arg2;
		char * line;
	};

	// Position on line, on which is the token that generated the instruction.
	// If 0, it means the position is not specified (previous token should be used)

	LinePos  line_pos;

	// Type of result computed by this instruction
	// This type may differ from type defined in instruction result variable 
	// (it may be it's subset).
	// For example in case of LET x, 10 instruction, type in result_type will be 10..10, even if type of
	// x variable may be 0..255.

	// TODO: type may be union with next_use (they are not used at the same time)

	Type * type[3];				// 0 result type, 1 arg1 type 2 arg2 type
	Type * result_index_type;

	UInt8    flags;
	Instr * next_use[3];		// next use of result, arg1, arg2

	Instr * next, * prev;
};

// Instruction flags
#define InstrRestriction 1		// result type in the instruction is actually an restriction

struct LocTag {
	Var * proc;
	InstrBlock * blk;
	Instr * i;
	UInt32 n;			// sequence number of instruction in block
};

typedef struct {
	Loc defs[64];
	UInt16 count;
} Defs;

void DefsAdd(Defs * defs, InstrBlock * blk, Instr * i);
void DefsInit(Defs * defs);

void ReachingDefs(Var * proc, Var * var, Loc * loc, Defs * defs);

#define InstrInvariant 1
#define InstrLoopDep 2

/*
For instructions, where arg1 or arg2 = result, source points to instruction, that previously set the result.

1.		ror x,x,1
2.		ror x,x,1   source = 1
3.		ror x,x,1   source = 2
4.      let a,x

*/
typedef enum {
	JUMP_IF    = 0,
	JUMP_LOOP  = 1,
	JUMP_LOOP_EXIT = 2,
	JUMP_LOOP_ENTRY = 4     // may be combined with loop exit
} JumpType;

struct InstrBlockTag {

	InstrBlock * next;			// Blocks are linked in chain, so we can traverse them as required.
								// This is normally in order, in which the blocks were parsed.
	UInt32 seq_no;				// Block sequence number. It is used to determine order of blocks when detecting loops.
								// It is also usefull when debugging the compiler, as we can quickly locate an instruction by block number and
								// sequence number of the caller.

	InstrBlock * to;			// this block continues (jumps) to this block (if to == NULL, we leave the routine after the last instruction in the block)
	InstrBlock * cond_to;		// last instruction conditionally jumps to this block if the condition if true

	InstrBlock * from;			// we may come to this block from here

	InstrBlock * callers;		// list of blocks calling this block (excluding from)
	InstrBlock * next_caller;	// next caller in the chain

//	InstrBlock * loop_end;
	JumpType     jump_type;		// whether this is end of loop or some other type of branch

	Var * label;				// label that starts the block
	Bool  processed;
	Type * type;				// type computed in this block for variable when inferring types
	Instr * first, * last;		// first and last instruction of the block
	void * analysis_data;
};

InstrBlock * InstrBlockAlloc();

void InstrInit();
void InstrPrint(Instr * i);
void InstrPrintInline(Instr * i);
void InstrFree(Instr * i);

char * OpSymbol(InstrOp op);

#define PrintInferredTypes 1

void PrintVarVal(Var * var);
void PrintProc(Var * proc);
void PrintProcFlags(Var * proc, UInt32 flags);
void PrintBlockHeader(InstrBlock * blk);
void PrintInstrLine(UInt32 n);

InstrOp InstrFind(char * name);

// When we generate instructions, it is always done to defined code block
// Code blocks are managed using those procedures.


Instr * FirstInstr(InstrBlock * blk);
Instr * LastInstr(InstrBlock * blk);
void InstrBlockFree(InstrBlock * blk);
UInt32 InstrBlockInstrCount(InstrBlock * blk);

void InstrMoveCode(InstrBlock * to, Instr * after, InstrBlock * from, Instr * first, Instr * last);
Instr * InstrDelete(InstrBlock * blk, Instr * i);
Instr * InstrInsert(InstrBlock * blk, Instr * before, InstrOp op, Var * result, Var * arg1, Var * arg2);
void InstrAttach(InstrBlock * blk, Instr * before, Instr * first, Instr * last);
void InstrInsertRule(InstrBlock * blk, Instr * before, InstrOp op, Var * result, Var * arg1, Var * arg2);

InstrBlock * LastBlock(InstrBlock * block);


typedef void (*ProcessBlockFn)(InstrBlock * block, void * info);

void ForEachBlock(InstrBlock * blk, ProcessBlockFn process_fn, void * info);
Bool ProcInstrEnum(Var * proc, Bool (*fn)(Loc * loc, void * data), void * data);

extern Var * SCOPE;		// currently parsed variable (procedure, macro)
//extern Var * REGSET;	// enumerator with register sets

void InstrVarUse(InstrBlock * code, InstrBlock * end);
void VarUse();

Var * InstrEvalConst(InstrOp op, Var * arg1, Var * arg2);
Var * InstrEvalAlgebraic(InstrOp op, Var * arg1, Var * arg2);

Bookmark SetBookmarkLine(Loc * loc);
Bookmark SetBookmarkVar(Var * var);

void InstrBlockReplaceVar(InstrBlock * block, Var * from, Var * to);
void ProcReplaceVar(Var * proc, Var * from, Var * to);

Bool InstrEquivalent(Instr * i, Instr * i2);
Bool InstrIsSelfReferencing(Instr * i);

// Instruction generating

void GenerateInit();
void GenRegisterRule(Rule * rule);
void GenSetDestination(InstrBlock * blk, Instr * i);
void GenBegin();
InstrBlock * GenEnd();

void GenInternal(InstrOp op, Var * result, Var * arg1, Var * arg2);
void Gen(InstrOp op, Var * result, Var * arg1, Var * arg2);
void GenRule(Rule * rule, Var * result, Var * arg1, Var * arg2);
void GenLet(Var * result, Var * arg1);
void GenLine();
void GenLabel(Var * var);
void GenGoto(Var * var);
void GenBlock(InstrBlock * blk);
void GenMacro(Var * macro, Var ** args);
void GenMacroParse(Var * macro, Var ** args);
void GenLastResult(Var * var, Var * item);

void GenPos(InstrOp op, Var * result, Var * arg1, Var * arg2);
void GenLetPos(Var * result, Var * arg1);

#define RESULT 0
#define ARG1   1
#define ARG2   2

#define INSTR_COMMUTATIVE 1
#define INSTR_NON_CODE    2		// non-executable instruction generating data

typedef struct {
	InstrOp  op;
	char * name;
	char * symbol;
	TypeVariant arg_type[3];		// 0 = result, 1 = arg1, 2 = arg2
	UInt8   flags;
	void (*execute_fn)(Instr * i);
} InstrInfo;

extern InstrInfo INSTR_INFO[INSTR_CNT];

/***********************************************************

  Translate

***********************************************************/

extern Var * RULE_PROC;

typedef enum {
	RULE_UNDEFINED,	// INSTR_UNDEFINED
	RULE_ANY = 0,	// INSTR_VOID	// any argument
	RULE_VARIABLE,	// INSTR_VAR	// (type) variable of specified type
	RULE_CONST,		// INSTR_CONST	// (type) constant value matching specified type

	RULE_REGISTER,	// INSTR_IFEQ    // (var)  actual variable (typically used for register)
//	RULE_VALUE,		//                 RULE_VALUE is RULE_REGISTER with VarInt or VarText, ...       (n)    actual value

	RULE_VARIANT,	// INSTR_VARIANT   (var) one of specified variants (variants are specified as TUPLE variable)
	RULE_DEREF,		// INSTR_DEREF    dereference of variable (dereferenced type is always adr)
	RULE_ARG,		// argument (type of argument is defined, may be NULL)
	RULE_ELEMENT,	// INSTR_ELEMENT	// array element - var defines pattern for array, index defines pattern for index
	RULE_TUPLE,		// INSTR_TUPLE
	RULE_RANGE,		// INSTR_RANGE
	RULE_BYTE,		// INSTR_BYTE
	RULE_BIT,		// INSTR_BIT
	RULE_SUB,
	RULE_ADD,
	RULE_MUL,
	RULE_DIV,
	RULE_AND,
	RULE_OR,
	RULE_XOR,

	RULE_DESTINATION

} RuleArgVariant;

struct RuleArgTag {
	RuleArgVariant variant;
	UInt8          arg_no;		// argument index (1..26)  used for variable & const
	union {
	   Var * var;
	   Type * type;
	   RuleArg * arr;
	};
	RuleArg  * index;		        // pointer to index for array (NULL if there is no index)
							        // Rule arg is allocated in this case
							        // This type must be RULE_VARIABLE or RULE_REGISTER
							        // Type of that variable must be TYPE_ARRAY
};

struct RuleTag {
	Rule * next;
	Var *  file;			// file in which the rule has been defined
	LineNo line_no;			// line in the source code, on which the rule was defined

	InstrOp op;
	RuleArg arg[3];
	InstrBlock * to;
	Var * flags;			// for instruction rule, this is variable with flag or flags (tuple) that are modified, when this instruction is executed
	UInt8 cycles;			// How many cycles the instruction uses.
};


/*
Rules are stored in rule sets. We have three sets of rules:

 1. macro rules are used when parsing code
 2. Translate rules are used when translating instructions generated by parser to instruction rules
 3. Instruction rules (which represent actual processor instructions)

*/

typedef struct {
	Rule * rules[INSTR_CNT];
} RuleSet;

void RuleSetInit(RuleSet * ruleset);
void RuleSetAddRule(RuleSet * ruleset, Rule * rule);
Rule * RuleSetFindRule(RuleSet * ruleset, InstrOp op, Var * result, Var * arg1, Var * arg2);

void GenMatchedRule(Rule * rule);

void RuleRegister(Rule * rule);
//Bool RuleMatch(Rule * rule, Instr * i);

#define GENERATE 0
#define TEST_ONLY 1
#define BIGGER_RESULT 2

void ProcTranslate(Var * proc);
Bool InstrTranslate3(InstrOp op, Var * result, Var * arg1, Var * arg2, UInt8 mode);

void CheckValues(Var * proc);
void TranslateInit();

Rule * TranslateRule(InstrOp op, Var * result, Var * arg1, Var * arg2);

// Garbage collector

void RulesGarbageCollect();
void TypeMark(Type * type);

/*************************************************************

 Parser

*************************************************************/
typedef struct BlockTag Block;

struct BlockTag {
	Token command;		// TOK_IF, TOK_WHILE
	Var * body_label;
	Var * loop_label;
	Var * t_label;		// label, to which the condition jumps if it is true
	Var * f_label;		// label, to which the condition jumps if it is false
	Bool  not;			// set to true, if the condition should be negated
};

void ParseInit();
Bool Parse(char * name, Bool main_file, Bool parse_options);
void ProcCheck(Var * proc);

Bool ParsingSystem();
Bool ParsingRule();
Bool ParsingPattern();

void BufEmpty();
void BufPush(Var * var);
Var * BufPop();

Type * ParseType2(InstrOp mode);
Type * ParseTypeInline();

void ParseExpression(Var * result);
void ParseExpressionType(Type * result_type);
Bool ParseArg(Var ** p_var);

#define STACK_LIMIT 100
Var *  STACK[STACK_LIMIT];
UInt16 TOP;
extern LinePos OP_LINE_POS;				// Position of last parsed binary operator

/*************************************************************

Analytics

*************************************************************/

typedef Bool (*AnalyzeBlockFn)(Var * proc, InstrBlock * block, void * info);
/*
Purpose:
	Perform data flow analysis for specified block.
Arguments:
	block		Block to analyze
	info		Global information information shared between all blocks
Result:
	Return true, if there was some change in the block information, false otherwise.
*/

void DataFlowAnalysis(Var * proc, AnalyzeBlockFn block_fn, void * info);

typedef UInt8 * LiveSet;		// VarLive, VarDead, VarUndefined

void LiveVariableAnalysis(Var * proc);
void FreeLiveVariableAnalysis(Var * proc);

/*************************************************************

 Optimize phase

*************************************************************/

void OptimizeDataFlowBack(Var * proc, AnalyzeBlockFn block_fn, void * info);

void ResetValues();

Bool VarUsesVar(Var * var, Var * test_var);
Bool VarModifiesVar(Var * var, Var * test_var);
Int16 VarTestReplace(Var ** p_var, Var * from, Var * to);
Int16 VarReplace(Var ** p_var, Var * from, Var * to);

Int16 InstrTestReplaceVar(Instr * i, Var * from, Var * to);
Int16 InstrReplaceVar(Instr * i, Var * from, Var * to);

void ResetValue(Var * res);
void VarSetSrcInstr(Var * var, Instr * i);
Var * SrcVar(Var * var);

Bool InstrUsesVar(Instr * i, Var * var);
Bool InstrReadsVar(Instr * i, Var * var);
Bool InstrSpill(Instr * i, Var * var);

UInt8 VarIsLiveInBlock(Var * proc, InstrBlock * block, Var * var);
void MarkBlockAsUnprocessed(InstrBlock * block);


void ProcOptimize(Var * proc);
void GenerateBasicBlocks(Var * proc);
void MarkLoops(Var * proc);

Bool OptimizeLive(Var * proc);
Bool OptimizeLive2(Var * proc);

Bool OptimizeValues(Var * proc);
Bool OptimizeVarMerge(Var * proc);
Bool OptimizeLoops(Var * proc);

void OptimizeJumps(Var * proc);
void DeadCodeElimination(Var * proc);
Bool OptimizeMergeBranchCode(Var * proc);

void ProcClearProcessed(Var * proc);
void AllocateVariables(Var * proc);

void OptimizeProcInline(Var * proc);

void LoopPreheader(Var * proc, InstrBlock * header, Loc * loc);

void OptimizeLoopShift(Var * proc);

void InstrExecute(InstrBlock * blk);

/*************************************************************

 Emit phase

*************************************************************/


void PrintOptim(char * text);

Rule * InstrRule(Instr * instr);
Rule * InstrRule2(InstrOp op, Var * result, Var * arg1, Var * arg2);

void Emit(char * filename);
Bool EmitInstr(Instr * code);
Bool EmitInstrOp(InstrOp op, Var * result, Var * arg1, Var * arg2); 
Bool EmitInstrInline(Instr * i);
void EmitOpenBuffer(char * buf);
void EmitCloseBuffer();
void EmitChar(char c);

extern Var   ROOT_PROC;

//extern Bool VERBOSE;
void InitPlatform();
extern Var * INTERRUPT;
extern Var * MACRO_PRINT;		// Print macro
extern Var * MACRO_FORMAT;		// Format macro
extern Var * MACRO_ASSERT_PRINT;		// Assert procedure
extern Var * MACRO_ASSERT;		// Assert procedure
extern MemHeap VAR_HEAP;		// variable heap (or zero page heap), this is heap from which variables are preferably allocated
extern Var * VARS;				// list of variables
extern Bool  ASSERTS_OFF;		// do not generate asserts into output code

#define OPTIMIZE_COLOR (GREEN+LIGHT)
