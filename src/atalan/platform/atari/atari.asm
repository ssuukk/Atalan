_EOL_CHAR equ 155

_adr_putchr .proc
;Write byte to address specified by _arr and increment the address by 1.
	
	;Convert ASCII to ATARI character screen code
	;
	;The rules are following:
	;0..31   +64
	;32..95  -32
	;96..127 0  (codes are same)
	 	
	tax
	rol 
	rol 
	rol 
	rol
	and #3
	tay
	txa
	eor tbl,y
	
	;write the char to specified address	
	ldy #0
	sta (_arr),y

	inc _arr
	bne skip1
	inc _arr+1
skip1
	rts

tbl	dta b(%01000000)		;%0 00 00000		00..31	+64
		dta b(%00100000)		;%0 01 00000		32..63	-32
		dta b(%01100000)		;%0 10 00000    64..95  -32
		dta b(%00000000)		;%0 11 00000    96..127 0
	
.endp

_out_putchr .proc
;Write character from A to output.
	
		tax
		lda $347	;ICPUTB+1
		pha
		lda $346	;ICPUTB
		pha
		txa
		rts

.endp
