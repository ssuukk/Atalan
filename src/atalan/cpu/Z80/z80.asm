;Print characters based on format string stored after the call instruction.
;Plaftorm should define:
;
;   Platform.NEW_LINE character

system__print_mem PROC
;Destination address is in BC
	ld hl, store_char
	jr  z80_print
	ENDP

system__print_out PROC
	ld hl, platform__print_char
	ENDP

;IN: HL  address of print char procedure 
z80_print  PROC

	local token,no_eol,chars,numbers,exit

	ld (emit_char+1), hl
	pop hl
	
token:	
	ld a, (hl)
	inc hl
	cp 0
	jr z,exit
	cp 128
	jr nz, no_eol
	call emit_eol
	jr token
no_eol:
	jr nc,numbers 
	
	ld d,a
	;print D characters from BC address
chars:			
	ld a,(hl)
	inc hl
	call emit_char  ;print_char
	dec d
	jr nz,chars
	jr token

numbers:
	and 127
	ld c, (hl)		; read address of the variable
	inc hl
	ld b, (hl)
	inc hl
	
	push hl
	
	ld h, 0
	ld l, a
			
	ld a, (bc)
	dec l				;if we are supposed to read only one byte, we are done with reading
	ld l, a
	jr z, one_byte	
	inc bc				; read second byte
	ld a, (bc)
	ld h, a
one_byte:			
	call print_int2
	pop hl
	jr token
	
exit:
	push hl
	ret 
	ENDP
	
emit_eol:
	ld a, platform__new_line_char	;13	
emit_char:
	jp platform__print_char

store_char: PROC
	ld (de),a
	inc de
	ENDP

;-----------------------------
print_int2: PROC
	push bc
	push hl
	ld  d, 0			;set to 1 when some non-zero is printed
		
	ld	bc,-10000
	call	digit
	ld	bc,-1000
	call	digit
	ld	bc,-100
	call	digit
	ld	c,-10
	call	digit
	ld	c,b
	inc d			   ;make sure at least one digit is printed (even if it is 0)
	call    digit
	pop bc
	pop hl
	ret
	
digit	
	ld	a,'0'-1
Num2	
	inc	a
	add	hl,bc
	jr	c,Num2
	sbc	hl,bc

	cp '0'
	jr nz, do_print
	inc d
	dec d
	jr z, no_print
do_print
    inc d
	call emit_char
no_print	
	ret
		
	ENDP

; this routine performs the operation HL=H*E
_mul8  PROC                       
	ld d,0                         ; clearing D and L
	ld l,d
	ld b,8                         ; we have 8 bits
loop:
	add hl,hl                      ; advancing a bit
	jp nc,skip	                   ; if zero, we skip the addition (jp is used for speed)
	add hl,de                      ; adding to the product if necessary
skip:
	djnz loop
	ret
	ENDP