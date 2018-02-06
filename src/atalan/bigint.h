typedef Int64 BigInt ;

void IntSet(BigInt * dest, BigInt * src);		// This initializes uninitialized integer
void IntInit(BigInt * dest, Int32 n);
void IntSetMin(BigInt * dest);
void IntSetMax(BigInt * dest);

Int32 IntN(BigInt * n);

BigInt * IntMin(BigInt * l, BigInt * r);
BigInt * IntMax(BigInt * l, BigInt * r);


void IntModify(BigInt * dest, BigInt * l);

BigInt * Int0();
BigInt * Int1();
BigInt * Int255();

void IntAdd(BigInt * dest, BigInt * l, BigInt * r);
void IntSub(BigInt * dest, BigInt * l, BigInt * r);
void IntMul(BigInt * dest, BigInt * l, BigInt * r);
void IntDiv(BigInt * dest, BigInt * l, BigInt * r);
void IntMod(BigInt * dest, BigInt * l, BigInt * r);
void IntShr(BigInt * dest, BigInt * l, BigInt * r);

void IntAnd(BigInt * dest, BigInt * l, BigInt * r);
void IntOr(BigInt * dest, BigInt * l, BigInt * r);
void IntXor(BigInt * dest, BigInt * l, BigInt * r);

void IntSqrt(BigInt * dest, BigInt * l);
void IntNeg(BigInt * dest);

Bool IntEq(BigInt * l, BigInt * r);
Bool IntLower(BigInt * l, BigInt * r);
Bool IntHigher(BigInt * l, BigInt * r);
Bool IntLowerEq(BigInt * l, BigInt * r);
Bool IntHigherEq(BigInt * l, BigInt * r);

Bool IntEqN(BigInt * l, Int32 N);
Bool IntHigherN(BigInt * l, Int32 N);
Bool IntLowerN(BigInt * l, Int32 N);
Bool IntLowerEqN(BigInt * l, Int32 N); 

void IntMulN(BigInt * dest, Int32 N);
void IntDivN(BigInt * dest, Int32 N);
void IntAddN(BigInt * dest, Int32 N);
void IntAndN(BigInt * dest, UInt32 N);
void IntOrN(BigInt * dest, UInt32 N);

void PrintBigInt(BigInt * n);

void IntFree(BigInt * n);

//----- Generic

void IntRangeSize(BigInt * dest, BigInt * min, BigInt * max);
// max - min + 1
