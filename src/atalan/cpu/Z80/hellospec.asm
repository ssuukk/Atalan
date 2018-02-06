
	org 30000

tv_flag	equ 5C3Ch

start

	; Directs rst 10h output to main screen.
	xor a
	ld (tv_flag),a

	call system__print_out
	db 7,"Number:"
	db 128+1
	dw num
	db 1, "/"
	db 128+2
	dw num2
	
	db 2, " x"
	db 128
	db 3, "EOL"
	db 0
	
;	ld a, 'Z'
;	call print_char
	ret

num	db 123
num2 dw 10317

platform__new_line_char  EQU 13

;Print one character on screen	
platform__print_char PROC
	push hl
	rst 10h
	pop hl
	ret

	ENDP

	include "z80.asm"

;hello	db "Hello, world.", 0Dh, 0

	end start
