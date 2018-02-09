.proc _adr_putchr
;Write byte to address specified by _arr and increment the address by 1.
		 	
.ifdef _arr
	;write the char to specified address	
	ldy #0
	sta (_arr),y

	inc _arr
	bne skip1
	inc _arr+1
skip1:
	rts
.endif
.endproc

.proc _out_putchr
;Write character from A to output.
		jsr $ffd2
.endproc

.proc _KGetCursor
	sec
	jmp $fff0
.endproc

.proc _KSetCursor
	clc
	jmp $fff0
.endproc
