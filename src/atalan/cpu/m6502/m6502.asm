;********************************************************
;
;  ATALAN 6502 processor definition support routines
;
;  Parts of code have been taken from Effectus library.
;
;********************************************************


;Noname
;651-655         028B-028F
;
;More spare bytes that you shouldn't use because future versions of the OS might use them. 
;Locations 651 and 652 are already used by version "B" as part of the interrupt handler routines.

;OS registers we use as temporary:
;
;BUFRLO,BUFRHI   50,51 $32,$33
;BFENLO,BFENHI   52,53 $34,$35

;ADRESS  100,101   $64,$65  A temporary storage location used by the display handler for so many things that it made my mind spin and I forgot what they were.
;MLTTMP  102,103   $66,$67  More temporary storage, with aliases OPNTMP and TOADR.
;SAVADR  104,105   $68,$69  Also know as FRMADR. Also used for temporary storage. 
;                           Also not significant enough to explain (look at the OS listing if, for some reason, you really care).

;Registers used when drawing line
;ROWAC   112,113   $70,$71
;COLAC   114,115   $72,$73
;ENDPT   116,117   $74,$75
;COUNTR  126,127   $7E,$7F  COUNTR tells how many points have to be plotted before the line is finished.

;
_SYS_PRINT_SIGNED = 1

;_putchr_proc_adr = $653  ;+$654 
?argptr =   $32		;$33
?varptr	=	$34
?size	=	$15		;15		//$16 BUFADR
		.IF _SYS_PRINT_SIGNED = 1 
?sign = $16
		.ENDIF
?aux2	=	$38
?aux	=	$39

;Following assigns are defined by mc6502.atl

_TW1 = $32	;$33  mc6502.atl defines these labels as _TEMPW1, _TEMPW2.
_TW2 = $34  ;$35
_TW3 = $15	;$16
_TL1 = $70	;four byte register ($70-$73) defined under name _TEMPL1 in mc6502.atl		

;_stdbuf    = $600		;TODO: use LINBUF ($247-$26E)  (100,101 can point to it)

;Argument list:
;	0	end of list
;	1..127  Constant string of specified lenght
; 128     EOL

;   7 6 5 4 3 2 1 0 
;  |0| | | | | | | |		String of length 0..127
;  |1|0|0| | | | | |    Pointer to unsigned integer of length 1..31 bytes
;  |1|0|1| | | | | |    Pointer to signed integer of length 1..31 bytes
;  |1|1|0| | | | | |    Set parameter to 0..31
;  |1|1|1| | | | |1|    Set parameter to byte value stored at given adress
;  |1|1|1| | | |1| |    Print buffer of 'param' length pointed by variable at address
;  |1|1|1| | | |1|1|    Print buffer of 'param' length at address


;         ---- ptr to unsigned integer ---- (7-bit set)
; 129     one byte integer reference
;	130     two byte integer reference
;	131     three byte integer eference
;	132     four byte integer reference
;         ---- ptr to signed -----  (5-bit set)
; 161     one byte
; 162     two byte
; 163     three byte
; 164     four byte

_std_print_adr .proc

		lda #<_adr_putchr
		ldx #>_adr_putchr
		clc
		bcc system__print
		.endp
		
system__print_out .proc
		lda #<_out_putchr
		ldx #>_out_putchr
		clc											;TODO: No Jump, if we are directly before the _std_print
		bcc system__print
		.endp

 .IF .NOT .DEF _EOL_CHAR
		_EOL_CHAR equ 0
 .ENDIF

system__print .proc
		sta system__putchr_proc_adr 
		stx system__putchr_proc_adr+1
		
		;... continue to _std_print
;.endp
		
;_std_print	.proc

		;Get address of argument from stack
		pla
		sta ?argptr
		pla
		sta ?argptr+1
		jsr _read_byte	;just skip next byte (that is part of return address)

		;Read command from input				
command
		jsr _read_byte
		sta ?size
		tay				  ; to set the flags  (TODO: Use asl?)
		beq done		;command 0 means end of sequence
		bpl str			;1..127 constant string

		;Write n-byte integer variable

		and #$7f		; zero top bit (it's always 1)
		sta ?size
		beq eol			;size 0 means end of line (0 byte integer would be nonsense)

    ;Read address of the variable

		jsr _read_adr

		;Is this array printing instruction?
		bit ?size
		bvc number
		
		;Read the size and get address of variable
		ldy #0
		lda (?varptr),y
		sta ?size
		
		jsr _read_adr		; this is adress of pointer variable that stores the address		
		ldy #0
		lda (?varptr),y
		tax
		iny
		lda (?varptr),y
		stx ?varptr
		sta ?varptr+1
								
		;Print the specified number of bytes
buf
buf_char
		ldy #0
		lda (?varptr),y
		jsr _std_putchr
		inc ?varptr
		bne _skip_buf
		inc ?varptr+1
_skip_buf
		dec ?size
		bne buf_char
		beq	command
					
number
		.IF _SYS_PRINT_SIGNED = 1 
		;signed number has bit 5 set
		lda #0
		sta ?sign
		
		lda ?size
    and #32		;%00100000
    beq unsigned

		lda ?size	;size uses only 4 bits
		and #31		;%00011111
		sta ?size
    
    tay				;test, whether the integer is negative (i.e. top byte is negative)
    dey
    lda (?varptr),y
    bpl unsigned
		 
    dec ?sign				;sign = $ff  (-1)
		
		lda #'-'
		jsr _std_putchr
		
unsigned
		.ENDIF
		    
		jsr _std_bin_to_bcd
		jsr _std_print_hex
		clc
		bcc command		;jmp command
eol
		lda #_EOL_CHAR	
		jsr _std_putchr
		clc
		bcc command		;jmp command
		;Write constant string (size is 1..127, already stored)
					
str
		jsr _read_byte
		jsr _std_putchr
		dec ?size
		bne str
		beq	command
						
done		
		jmp (?argptr)	
		rts

_read_adr
		jsr _read_byte
		sta ?varptr		
		jsr _read_byte
		sta ?varptr+1
		rts
						
_read_byte
		ldy #0
		lda (?argptr),y
		inc ?argptr
		bne skip
		inc ?argptr+1
skip
		rts
.endp

_std_putchr .proc
		jmp (system__putchr_proc_adr)
		rts
.endp


.proc _std_print_hex
;Print hexadecimal number of arbitrary length (?size). 
;Leading zeroes are not printed.
;In:
;	?varptr		Pointer to memory containing the number
;	?size		  Number of bytes to print
;Uses:
;	?aux
;	?aux2
	
		lda ?size
		sta ?aux
		lda #0
		sta ?aux2		; number of non-zero digits on output
		beq _loop
_outbyte
		ldy ?aux		
		lda (?varptr),y
		pha
		lsr
		lsr
		lsr
		lsr
		jsr _write_digit
	
		pla
		and #$0f
		jsr _write_digit
_loop	
		dec ?aux
		bpl _outbyte
		
		;If no character has been written, write at least one 0		
		lda ?aux2
		bne _no_empty
		lda #48
		jsr _std_putchr
_no_empty
 
		rts
	
_write_digit
;In: a 4 bit digit

		tax
		bne _non_zero
		lda ?aux2
		beq _done		;this is zero and there has been no char before - no output
_non_zero
		lda hex,x
		jsr _std_putchr
		inc ?aux2
_done
		rts

hex     dta c"0123456789ABCDEF"

.endp

.proc _std_bin_to_bcd
;Convert binary number to BCD. 
;Arbitrary size (up to 127 bytes) are supported.
;In:
;	?varptr	pointer to binary number
; a   0 means unsigned number
;     $ff means signed number
;	?size	number of bytes
;Out:
;	?size	on output, returns size of resulting bcd number
;   ?varptr	on output, containg pointer to converted BCD
;Uses:
;	system__buf
;	?aux
;	?aux2

		;Compute size of resulting number 
		ldy ?size
		sty ?aux		; used to count later
		iny				;add space to result
		sty ?size
		
		;Zero the destination buffer
		lda #0
zero	sta system__buf-1,y
		dey
		bne zero
		
		;**** We convert the number byte a time
		sed
		
		;?aux = varptr(?aux)
bytes
		dec ?aux
		ldy ?aux
		lda (?varptr),y	
		.IF _SYS_PRINT_SIGNED = 1 
		eor ?sign
		.ENDIF	
		sta ?aux2
		sec				;set top bit to 1
		bcs loop		

shift_byte			
		ldx #0
		ldy ?size
bcd_mul2
		lda system__buf,x
		adc	system__buf,x			;buf2(x) = buf2(x) * 2 + carry
		sta system__buf,x
		inx
		dey								;TODO: cpx ?size
		bne bcd_mul2
			
		clc
loop	rol ?aux2		;divide by two, if result is 0, end
		bne shift_byte		
		
		lda ?aux
		bne bytes
	
		.IF _SYS_PRINT_SIGNED = 1 
		;If this is negative number, add 1
		;In case sign is $ff, asl will set the C to 1, otherwise to 0	
		lda ?sign
		asl
		ldx #0
carry1
		lda system__buf,x
		adc #0
		sta system__buf,x
		inx
		bcs carry1
		.ENDIF
					
		cld		
		
		lda #<system__buf
		sta ?varptr	
		lda #>system__buf
		sta ?varptr+1	
		rts

.endp

/*
   Neg value in A
*/

_sys_neg   .proc
		clc
		eor #$FF
		adc #1
		rts	
.endp

/*
  Multiply two signed 8-bit integers

	Paramerters:
	  a  First multiplicant (signed)
	  x  Second multiplicand (signed)
*/

_sys_mulss8   .proc

	cpx #0
	bpl _sys_mulsu8
	
	pha
	txa
	jsr _sys_neg
	tax
	pla	
	jsr _sys_mulsu8
	jmp	_sys_neg16
.endp

/*
  Multiply signed 8-bit integer with unsigned 8-bit integer

	Paramerters:
	  a  First multiplicant (signed)
	  x  Second multiplicand (unsigned)
*/

_sys_mulsu8   .proc
   cmp  #0
   bpl _sys_mul8
   jsr _sys_neg
   jsr _sys_mul8
	 		
.DEF :_sys_neg16
   ;  neg the result of multiplication   
   lda #0					;TODO: Maybe we may return the result in X,A ?
   sec
   sbc _TW2
   sta _TW2
   lda #0
   sbc _TW2+1
   sta _TW2+1
   rts
   .endp
   
/*
  Mul8 - 8-bit multiplication routine
  
  Original source:
  Book Atari Roots (Chapter Ten - Assembly Language Math)
  Hyperlink: http://www.atariarchives.org/roots/chapter_10.php
  
  Parameters:
	  a First multiplicant
	  x Second multiplicant
  Result: 
		TEMPW2,TEMPW2+1	high byte of the result
*/

_sys_mul8  .proc

MUL1 = _TW1
MUL2 = _TW1+1
RES  = _TW2		;_TW2+1

		sta MUL1			;input comes in A and X
		stx MUL2
		
		lda #0			;RES = 0
		sta RES
		
		ldx #8  
loop
		lsr MUL1		;MUL1 = MUL1 / 2
		bcc noadd
		
		clc					;RES = RES + (MUL2 * $ff)
		adc MUL2	  
noadd 
		ror @				;RES = RES / 2		(Carry is 0, when there was no add)
		ror RES

		dex
		bne loop
		
		sta RES+1
		rts

		.endp

/*
  Mul16 - 16-bit multiplication routine
    
  Parameters:
	  _TW1 First multiplicant (we use only two bytes now)
	  _TW2 Second multiplicant
  Result: 
		_TL1 Result
*/

_sys_mul16  .proc

MUL1 = _TW1
MUL2 = _TW2
RES  = _TL1
		
		lda #0      ;RES = 0;
		sta RES
		sta RES+1
		sta RES+2
		sta RES+3
		
		ldx #16  
loop
		lsr MUL1+1		;MUL1 = MUL1 / 2
		ror MUL1
		bcc noadd
		
		lda RES+2			;RES = RES + (MUL2 * $ffff)  (only upper half) 
		clc		
		adc MUL2
		sta RES+2
		lda RES+3
		adc MUL2+1
		sta RES+3
noadd
		ror RES+3				;RES = RES / 2		(Carry is 0, when there was no add)
		ror RES+2
		ror RES+1
		ror RES+0
		 
		dex
		bne loop
		
		rts

		.endp
 
/*
  Div8 - 8-bit division routine
 
  Original source:
  Book Atari Roots (Chapter Ten - Assembly Language Math)
  Hyperlink: http://www.atariarchives.org/roots/chapter_10.php
 
  Parameters:
  _TEMPW1		16-bit dividend
  a					 p1_math:  8-bit divisor
  x          number of bits of an divisir (8 max)
 
  Result:
  x STORE1: 8-bit quotient
  a STORE2: 8-bit remainder
*/

_sys_div8  .proc
  ldx #08  ; For an 8-bit divisor
	
_sys_div

divisor  = _TW2
quotient = _TW2+1

  sta divisor
	lda #0     				; for non-8 divis
	sta quotient
  lda _TW1+1
  sec 
  sbc divisor
dloop 
  php  					; The loop that divides 
  rol quotient
  asl _TW1
  rol @
  plp
  bcc addit
  sbc divisor
  jmp next
addit 
  adc divisor
next  
  dex
  bne dloop
  bcs fini
  adc divisor
  clc
fini
	tax            ; x = remainder
  lda quotient   ; a = quotient
  rol
  rts
 
  .endp

/*

  Square Root

  Calculates the 8 bit root and 9 bit remainder of a 16 bit unsigned integer in
  Numberl/Numberh. The result is always in the range 0 to 255 and is held in
  Root, the remainder is in the range 0 to 511 and is held in Reml/Remh

  partial results are held in templ/temph

  Destroys A, X registers.

	Arguments:
	
	_TEMPW1  16-bit number to compute root of
	
	<a        Square root
	<_TEMPW2  Remainder of square
	
*/

_sys_sqrt16 .proc

Numberl		= _TW1		; number to find square root of low byte
Numberh		= _TW1+1	; number to find square root of high byte
Reml		  = _TW2		; remainder low byte
Remh		  = _TW2+1	; remainder high byte
templ		  = _TL1		; temp partial low byte
temph		  = _TL1+1	; temp partial high byte
Root		  = _TL1+2	; square root

	LDA	#$00		; clear A
	STA	Reml		; clear remainder low byte
	STA	Remh		; clear remainder high byte
	STA	Root		; clear Root
	LDX	#$08		; 8 pairs of bits to do
Loop
	ASL	Root		; Root = Root * 2

	ASL	Numberl		; shift highest bit of number ..
	ROL	Numberh		;
	ROL	Reml		; .. into remainder
	ROL	Remh		;

	ASL	Numberl		; shift highest bit of number ..
	ROL	Numberh		;
	ROL	Reml		; .. into remainder
	ROL	Remh		;

	LDA	Root		; copy Root ..
	STA	templ		; .. to templ
	LDA	#$00		; clear byte
	STA	temph		; clear temp high byte

	SEC			; +1
	ROL	templ		; temp = temp * 2 + 1
	ROL	temph		;

	LDA	Remh		; get remainder high byte
	CMP	temph		; comapre with partial high byte
	BCC	Next		; skip sub if remainder high byte smaller

	BNE	Subtr		; do sub if <> (must be remainder>partial !)

	LDA	Reml		; get remainder low byte
	CMP	templ		; comapre with partial low byte
	BCC	Next		; skip sub if remainder low byte smaller

				; else remainder>=partial so subtract then
				; and add 1 to root. carry is always set here
Subtr
	LDA	Reml		; get remainder low byte
	SBC	templ		; subtract partial low byte
	STA	Reml		; save remainder low byte
	LDA	Remh		; get remainder high byte
	SBC	temph		; subtract partial high byte
	STA	Remh		; save remainder high byte

	INC	Root		; increment Root
Next
	DEX			; decrement bit pair count
	BNE	Loop		; loop if not all done

	lda Root
	
	RTS
.endp