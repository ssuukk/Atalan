;********************************
;ATALAN nes background asm functions
;********************************
; created by Marcel Cevani 31.01.2011
; tnx to Atalan the snake & rudla :)

;_TEMPB1 equ 54  array offset
;_TEMPB2 equ 55  array offset

; ATALAN code
; PPU_DATA_ADR = $20
; PPU_DATA_ADR = $00
; for x:0..1023
;       PPU_DATA = titleNametable(x)

bgLoadNameTable .proc
			sta _TEMPB1
			stx _TEMPB1+1
			ldy #00
			ldx #04
			lda #$20 
copyArray: 
			sta $2006
			sty $2006
cpyloop_n: 
			lda (_TEMPB1),y
			sta $2007
			iny
			bne cpyloop_n
			inc _TEMPB1+1
			dex
			bne cpyloop_n 
			rts
			.endp

;  ATALAN code	 
;  PPU_DATA_ADR = $3F
;  PPU_DATA_ADR = $00
;  for i:0..15
;        PPU_DATA  = bgPal(i)
			    
bgLoadPal .proc
			sta _TEMPB1
			stx _TEMPB1+1
			ldy #$00
			lda #$3F
copyArray: 
			sta $2006
			sty $2006
cpyloop_p: 
			lda (_TEMPB1),y
			sta $2007
			iny
			cpy #$10
			bne cpyloop_p 
			rts
			.endp
