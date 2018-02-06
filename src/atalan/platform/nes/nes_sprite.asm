;********************************
;ATALAN nes sprite asm functions
;********************************
; created by Marcel Cevani 31.01.2011
; tnx to Atalan the snake & rudla :)

;_TEMPB1 equ 54  array offset
;_TEMPB2 equ 55  array offset

;  ATALAN code	 
;  PPU_DATA_ADR = $3F
;  PPU_DATA_ADR = $10
;  for i:0..15
;        PPU_DATA  = spritePal(i)
			    
sprLoadPal .proc
			sta _TEMPB1
			stx _TEMPB1+1
			lda $2002
			ldx #$10
			ldy #$00
			lda #$3F
copyArray:  sta $2006
			stx $2006
cpyloop_sr: lda (_TEMPB1),y
			sta $2007
			iny
			cpy #$10
			bne cpyloop_sr 
			rts
			.endp

