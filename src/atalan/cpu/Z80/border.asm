	org 30000

tv_flag	equ 5C3Ch

start
	ld a, 0
l1
	inc a
	out (254), a

	jp l1
	ret

	end start
