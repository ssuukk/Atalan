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

.proc _disableIrq
	sei
	rts
.endproc

.proc _enableIrq
	cli
	rts
.endproc

.proc _disableTimerAIrq
	lda #%01111111
	sta $dc0d
	rts
.endproc

.proc _enableRasterIrq
	lda $d01a
	ora #1
	sta $d01a
	rts
.endproc 

.proc _disableRasterIrq
	lda $d01a
	and %11111110
	sta $d01a
	rts
.endproc

.proc _screenMode24
	lda #$1b
	sta $d011
	rts
.endproc

.proc _screenMode25
	lda $d011
	ora #8
	sta $d011
	rts
.endproc 

.proc _ackRasterIrq
	asl $d019
	rts
.endproc

