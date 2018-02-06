_arr equ 168
_TEMPW1 equ 50
_TEMPW2 equ 52
_TEMPL1 equ 112
timer equ 20
RANDOM equ 53770
WSYNC equ 54282
STICK equ 632
DMACTL equ 559
COLOR0 equ 708
COLPF equ 53270
pmg__base equ 54279
player__x equ 53248
player__scolor equ 704
player__color equ 53266
player__size equ 53256
player__bitmap equ pmg__memory+4*256
missile__x equ 53252
missile__size equ 53260
GRACTL equ 53277
GTICTLS equ 623
AUDF1 equ 53760
AUDC1 equ 53761
key__none equ 255
CH equ 764
SDLSTL equ 560
NMIEN equ 54286
VDSLST equ 512
VVBLKD equ 548
rmt_vars equ 170
PORTB equ 54017
screen equ 41136
mem equ 0
vcnt equ 193
textbuf equ 194
besttime equ 209
playfield equ 1552
k equ 224
l equ 225
xbtmp equ 1536
ybtmp equ 1537
btmp1 equ 1542
btmp2 equ 1543
CONSOL equ 53279
dir equ 226
play equ 227
piccnt equ 228
tsec equ 229
tdsec equ 230
tmin equ 231
tdmin equ 232
moves equ 233
bmoves equ 235
currcol equ 237
currentbuf equ 238
tmp1 equ 240
xtilesize equ 241
ytilesize equ 242
col equ 243
root__D_UP equ 0
root__D_RIGHT equ 1
root__D_DOWN equ 2
root__D_LEFT equ 3
root__T_EMPTY equ 15
root__XOFF equ 1
root__YOFF equ 8
sleep__time equ 128
copyblock__srcbm equ 129
copyblock__xbm equ 128
copyblock__ybm equ 131
copyblock__wbm equ 133
copyblock__xscr equ 134
copyblock__yscr equ 135
copyblock__xsize equ 136
copyblock__ysize equ 137
copyblock__ct equ 138
copyblock__i equ 140
_s3__j equ 141
text__xt equ 142
text__yt equ 143
text__len equ 144
text__ii equ 145
text__jj equ 146
copytile__xbm equ 142
copytile__ybm equ 143
copytile__xscr equ 144
copytile__yscr equ 145
copytile__xsize equ 146
copytile__ysize equ 147
eraserect__xscr equ 128
eraserect__yscr equ 129
eraserect__width equ 130
eraserect__height equ 131
eraserect__i equ 132
_s6__j equ 133
_s7___21 equ 134
findempty__x equ 128
findempty__y equ 129
gettile__tile equ 128
gettile__tx equ 129
gettile__ty equ 130
_s11___25 equ 148
_s11___27 equ 149
_s11__tile equ 150
_s11__i equ 151
_s11__j equ 152
movetile__xs equ 148
movetile__ys equ 149
movetile__m equ 150
movetile__show equ 151
movetile__tile equ 152
movetile__xoffset equ 153
movetile__yoffset equ 154
movetile__z equ 155
_s12___31 equ 156
_s12___33 equ 158
clrscr__i equ 128
_s13__j equ 129
changepicture__c equ 128
countgoodtiles__res equ 128
_s16__tile equ 129
_s18__tile equ 128
setboardcolor__col equ 128
changecolor__col equ 129
drawmainscreen__j equ 153
drawmainscreen__ii equ 154
drawmainscreen__kk equ 155
shuffletiles__c equ 160
shuffletiles__x equ 161
shuffletiles__y equ 162
shuffletiles__cnt equ 163
_s22__j equ 128
_s23__i equ 129
_s24__i equ 130
_s25__i equ 131
_s26__i equ 132
_s27__i equ 128
drawbesttime__tcnt equ 148
drawbesttime__tmpb equ 149
initscreen___86 equ 133
initscreen___87 equ 135
titlescreen__ctmp equ 164
titlescreen___89 equ 165
game__cnt equ 164
game__ss equ 165
game__x equ 166
game__y equ 167
_91 equ 244
_92 equ 246
_93 equ 247
_94 equ 248
_95 equ 249
_96 equ 250
_97 equ 251
   org $2e0
   dta a($2000)
   org $2000
;### fifteen.atl(50) piccnt:byte= RANDOM bitand 3
   lda RANDOM
   and #3
   sta piccnt
;### fifteen.atl(722) PORTB = PORTB bitor 2
   lda PORTB
   ora #2
   sta PORTB
;### fifteen.atl(723) music'init musmod
   lda #<musmod
   sta _91
   lda #>musmod
   sta _91+1
   ldx _91
   ldy _91+1
   lda #0
   jsr rmt_init
;### fifteen.atl(724) initscreen
   jsr initscreen
;### fifteen.atl(725) besttime(0)=0
   lda #0
   sta besttime
;### fifteen.atl(726) bmoves=999
   lda #231
   sta bmoves
   lda #3
   sta bmoves+1
;### fifteen.atl(732) 	game
   jmp _lbl154
_lbl156:
;### fifteen.atl(728) 	titlescreen
   jsr titlescreen
;### fifteen.atl(729) 	game
   jsr game
_lbl154:
;### fifteen.atl(727) while 1=1
   lda #1
   cmp #1
   jeq _lbl156
_lbl155:
   jmp _lbl155
dl:
;### fifteen.atl(64) const dl:array of byte = ( 2 times $70, $4f,  screen ,
   dta b(112)
   dta b(112)
   dta b(79)
   dta a(screen)
;### fifteen.atl(65) 	32 times $f,
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
;### fifteen.atl(66) 	$8f,
   dta b(143)
;### fifteen.atl(67) 	18 times $f
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
;### fifteen.atl(68) 	$8f,
   dta b(143)
;### fifteen.atl(69) 	12 times $f ,
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
;### fifteen.atl(70) 	$8f,
   dta b(143)
;### fifteen.atl(71) 	29 times $f ,
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
;### fifteen.atl(72) 	$8f
   dta b(143)
;### fifteen.atl(73) 	$f $f
   dta b(15)
   dta b(15)
;### fifteen.atl(74) 	$4f , 0, $B0 ,
   dta b(79)
   dta b(0)
   dta b(176)
;### fifteen.atl(75) 	40  times $f,
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
;### fifteen.atl(76) 	$8f
   dta b(143)
;### fifteen.atl(77) 	60  times $f,
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
;### fifteen.atl(78) 	$4f screen
   dta b(79)
   dta a(screen)
;### fifteen.atl(79) 	7 times $f
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
   dta b(15)
;### fifteen.atl(80) 	$41, dl
   dta b(65)
   dta a(dl)
buf1:
;### fifteen.atl(84) const buf1: array(4608) = file "einstein-tongue.pbm"
   ins 'einstein-tongue.pbm'
buf2:
;### fifteen.atl(85) const buf2: array(4608) = file "miner.pbm"
   ins 'miner.pbm'
buf3:
;### fifteen.atl(86) const buf3: array(4608) = file "johnromero.pbm"
   ins 'johnromero.pbm'
buf4:
;### fifteen.atl(87) const buf4: array(4608) = file "clinteastwood.pbm"
   ins 'clinteastwood.pbm'
leafh:
;### fifteen.atl(88) const leafh: array(47) = file "leafhoriz.pbm"
   ins 'leafhoriz.pbm'
leafv:
;### fifteen.atl(89) const leafv: array(47) = file "leafvert.pbm"
   ins 'leafvert.pbm'
mains:
;### fifteen.atl(90) const mains: array(2800) = file "15.pbm"
   ins '15.pbm'
fonts:
;### fifteen.atl(91) const fonts: array = file "STENCILstr.FNT"
   ins 'STENCILstr.FNT'
musmod:
;### fifteen.atl(92) const musmod:array = file "little15stripped.rmt"
   ins 'little15stripped.rmt'
pmg__memory_lo:  :8 dta l(pmg__memory + #*256)
pmg__memory_hi:  :8 dta h(pmg__memory + #*256)
player__bitmap_lo:  :4 dta l(player__bitmap + #*256)
player__bitmap_hi:  :4 dta h(player__bitmap + #*256)
screen_lo:  :200 dta l(screen + #*40)
screen_hi:  :200 dta h(screen + #*40)
playfield_lo:  :4 dta l(playfield + #*4)
playfield_hi:  :4 dta h(playfield + #*4)
set_col .proc
   pha
   txa
   pha
   tya
   pha
;### fifteen.atl(96) 	if vcnt = 0
   lda vcnt
   cmp #0
   jne _lbl4
;### fifteen.atl(97) 		inc vcnt
   inc vcnt
;### fifteen.atl(98) 		wait'line
   sta WSYNC
;### fifteen.atl(99) 		player(3).color = $9e
   lda #158
   sta player__color+3
;### fifteen.atl(100) 		COLPF(3) = $96
   lda #150
   sta COLPF+3
;### fifteen.atl(101) 		player(3).x=180
   lda #180
   sta player__x+3
;### fifteen.atl(102) 		missile(2).x=166
   lda #166
   sta missile__x+2
;### fifteen.atl(103) 		missile(3).x=172
   lda #172
   sta missile__x+3
;### fifteen.atl(104) 		wait'line
   sta WSYNC
;### fifteen.atl(105) 		player(3).color = $8e
   lda #142
   sta player__color+3
;### fifteen.atl(106) 		COLPF(3) = $86
   lda #134
   sta COLPF+3
;### fifteen.atl(107) 		wait'line
   sta WSYNC
;### fifteen.atl(108) 		player(3).color = $5e
   lda #94
   sta player__color+3
;### fifteen.atl(109) 		COLPF(3) = $56
   lda #86
   sta COLPF+3
;### fifteen.atl(110) 		wait'line
   sta WSYNC
;### fifteen.atl(111) 		player(3).color = $4e
   lda #78
   sta player__color+3
;### fifteen.atl(112) 		COLPF(3) = $46
   lda #70
   sta COLPF+3
;### fifteen.atl(113) 	else if vcnt = 1
   jmp _lbl12
_lbl4:
   lda vcnt
   cmp #1
   jne _lbl6
;### fifteen.atl(114) 		wait'line
   sta WSYNC
;### fifteen.atl(115) 		COLPF(3) = $4e
   lda #78
   sta COLPF+3
;### fifteen.atl(116) 		player(3).x=166
   lda #166
   sta player__x+3
;### fifteen.atl(117) 		missile(2).x=192
   lda #192
   sta missile__x+2
;### fifteen.atl(118) 		missile(3).x=200
   lda #200
   sta missile__x+3
;### fifteen.atl(119) 		inc vcnt
   inc vcnt
;### fifteen.atl(120) 	else if vcnt = 2
   jmp _lbl12
_lbl6:
   lda vcnt
   cmp #2
   jne _lbl7
;### fifteen.atl(121) 		col=$4c
   lda #76
   sta col
;### fifteen.atl(126) 		inc vcnt
   jmp _lbl8
_lbl10:
;### fifteen.atl(123) 			wait'line
   sta WSYNC
;### fifteen.atl(124) 			player(3).color = col
   lda col
   sta player__color+3
;### fifteen.atl(125) 			col=col-2
   lda col
   sec
   sbc #2
   sta col
_lbl8:
;### fifteen.atl(122) 		while col>$40
   lda #64
   cmp col
   jcc _lbl10
_lbl9:
   inc vcnt
;### fifteen.atl(127) 	else if vcnt = 3
   jmp _lbl12
_lbl7:
   lda vcnt
   cmp #3
   jne _lbl11
;### fifteen.atl(128) 		wait'line
   sta WSYNC
;### fifteen.atl(129) 		player(3).color = $56
   lda #86
   sta player__color+3
;### fifteen.atl(130) 		COLPF(3) = $56
   lda #86
   sta COLPF+3
;### fifteen.atl(131) 		wait'line
   sta WSYNC
;### fifteen.atl(132) 		COLPF(3) = $84
   lda #132
   sta COLPF+3
;### fifteen.atl(133) 		player(3).color = $86
   lda #134
   sta player__color+3
;### fifteen.atl(134) 		wait'line
   sta WSYNC
;### fifteen.atl(135) 		COLPF(3) = $94
   lda #148
   sta COLPF+3
;### fifteen.atl(136) 		player(3).color = $96
   lda #150
   sta player__color+3
;### fifteen.atl(137) 		inc vcnt
   inc vcnt
;### fifteen.atl(138) 	else if vcnt = 4
   jmp _lbl12
_lbl11:
   lda vcnt
   cmp #4
   jne _lbl12
;### fifteen.atl(139) 		wait'line
   sta WSYNC
;### fifteen.atl(140) 		COLPF(3) = $9a
   lda #154
   sta COLPF+3
;### fifteen.atl(141) 		player(3).color = $9c
   lda #156
   sta player__color+3
;### fifteen.atl(142) 		inc vcnt
   inc vcnt
_lbl12:
   pla
   tay
   pla
   tax
   pla
   rti
.endp
sleep .proc
;### fifteen.atl(157) 	tmp1 = timer
   lda timer
   sta tmp1
;### fifteen.atl(158) 	tmp1 = tmp1 + time
   lda tmp1
   clc
   adc sleep__time
   sta tmp1
;### fifteen.atl(161) 	while timer<>tmp1
   jmp _lbl15
_lbl15:
;### fifteen.atl(159) 	while timer<>tmp1
   lda timer
   cmp tmp1
   jne _lbl15
_lbl14:
   rts
.endp
copyblock .proc
;### fifteen.atl(163) 	ct:card=0
   lda #0
   sta copyblock__ct
   lda #0
   sta copyblock__ct+1
;### fifteen.atl(164) 	i:byte=0
   lda #0
   sta copyblock__i
;### fifteen.atl(187) 		inc i
   jmp _lbl16
_lbl21:
;### fifteen.atl(167) 		xbtmp=i +xscr 
   lda copyblock__i
   clc
   adc copyblock__xscr
   sta xbtmp
;### fifteen.atl(168) 		ybtmp=yscr
   lda copyblock__yscr
   sta ybtmp
;### fifteen.atl(172) 		ct = srcbm
   lda copyblock__srcbm
   sta copyblock__ct
   lda copyblock__srcbm+1
   sta copyblock__ct+1
;### fifteen.atl(173) 		ct = ct + ybm
   lda copyblock__ct
   clc
   adc copyblock__ybm+0
   sta copyblock__ct
   lda copyblock__ct+1
   adc copyblock__ybm+1
   sta copyblock__ct+1
;### fifteen.atl(174) 		ct = ct + xbm
   lda copyblock__ct
   clc
   adc copyblock__xbm
   sta copyblock__ct
   jcc _lbl157
   inc copyblock__ct+1
_lbl157:
;### fifteen.atl(175) 		ct = ct + i
   lda copyblock__ct
   clc
   adc copyblock__i
   sta copyblock__ct
   jcc _lbl158
   inc copyblock__ct+1
_lbl158:
;### fifteen.atl(178) 		j:byte=ysize
   lda copyblock__ysize
   sta _s3__j
;### fifteen.atl(185) 		inc i
   jmp _lbl18
_lbl20:
;### fifteen.atl(180) 			screen(xbtmp,ybtmp)=mem(ct)
   lda #<mem
   sta _arr
   lda #>mem
   clc
   adc copyblock__ct+1
   sta _arr+1
   ldy copyblock__ct+0
   lda (_arr),y
   sta _92
   ldy ybtmp
   lda screen_lo,y
   sta _arr
   lda screen_hi,y
   sta _arr+1
   ldy xbtmp
   lda _92
   sta (_arr),y
;### fifteen.atl(181) 			ct = ct + wbm
   lda copyblock__ct
   clc
   adc copyblock__wbm
   sta copyblock__ct
   jcc _lbl159
   inc copyblock__ct+1
_lbl159:
;### fifteen.atl(182) 			inc ybtmp
   inc ybtmp
;### fifteen.atl(183) 			dec j
   dec _s3__j
_lbl18:
;### fifteen.atl(179) 		while j>0
   lda #0
   cmp _s3__j
   jcc _lbl20
_lbl19:
   inc copyblock__i
_lbl16:
;### fifteen.atl(165) 	while i<xsize 
   lda copyblock__i
   cmp copyblock__xsize
   jcc _lbl21
_lbl17:
   rts
.endp
text .proc
;### fifteen.atl(189) 	ii:byte=0
   lda #0
   sta text__ii
;### fifteen.atl(199) 		inc ii
   jmp _lbl22
_lbl25:
;### fifteen.atl(192) 		jj=textbuf(ii)
   ldx text__ii
   lda textbuf,x
   sta _93
   lda _93
   sta text__jj
   lda #0
   sta text__jj+1
;### fifteen.atl(193) 		if jj>=96 jj=jj-32
   lda text__jj+1
   cmp #0
   jcc _lbl24
   jne _lbl160
   lda text__jj
   sbc #96
   jcc _lbl24
_lbl160:
   lda text__jj
   sec
   sbc #32
   sta text__jj
   lda text__jj+1
   sbc #0
   sta text__jj+1
_lbl24:
;### fifteen.atl(194) 		jj=jj*8
   lda text__jj
   sta _TEMPW1
   lda text__jj+1
   sta _TEMPW1+1
   lda #8
   sta _TEMPW2
   lda #0
   sta _TEMPW2+1
   jsr _sys_mul16
   lda _TEMPL1
   sta text__jj
   lda _TEMPL1+1
   sta text__jj+1
;### fifteen.atl(196) 		copyblock fonts 0 jj 1 xt+ii yt 1 8
   lda #<fonts
   sta copyblock__srcbm
   lda #>fonts
   sta copyblock__srcbm+1
   lda #0
   sta copyblock__xbm
   lda text__jj
   sta copyblock__ybm
   lda text__jj+1
   sta copyblock__ybm+1
   lda #1
   sta copyblock__wbm
   lda text__xt
   clc
   adc text__ii
   sta copyblock__xscr
   lda text__yt
   sta copyblock__yscr
   lda #1
   sta copyblock__xsize
   lda #8
   sta copyblock__ysize
   jsr copyblock
;### fifteen.atl(197) 		inc ii
   inc text__ii
_lbl22:
;### fifteen.atl(191) 	while ii<len
   lda text__ii
   cmp text__len
   jcc _lbl25
_lbl23:
   rts
.endp
copytile .proc
;### fifteen.atl(202) 	copyblock currentbuf xbm*6 ybm*1152 24 xscr yscr xsize ysize
   lda currentbuf
   sta copyblock__srcbm
   lda currentbuf+1
   sta copyblock__srcbm+1
   lda copytile__xbm
   asl
   clc
   adc copytile__xbm
   asl
   sta copyblock__xbm
   lda copytile__ybm
   sta _TEMPW1
   lda #0
   sta _TEMPW1+1
   lda #128
   sta _TEMPW2
   lda #4
   sta _TEMPW2+1
   jsr _sys_mul16
   lda _TEMPL1
   sta copyblock__ybm
   lda _TEMPL1+1
   sta copyblock__ybm+1
   lda #24
   sta copyblock__wbm
   lda copytile__xscr
   sta copyblock__xscr
   lda copytile__yscr
   sta copyblock__yscr
   lda copytile__xsize
   sta copyblock__xsize
   lda copytile__ysize
   sta copyblock__ysize
   jsr copyblock
   rts
.endp
eraserect .proc
;### fifteen.atl(205) 	i=0
   lda #0
   sta eraserect__i
;### fifteen.atl(214) 		inc i
   jmp _lbl26
_lbl31:
;### fifteen.atl(207) 		ybtmp=yscr
   lda eraserect__yscr
   sta ybtmp
;### fifteen.atl(208) 		j=height
   lda eraserect__height
   sta _s6__j
;### fifteen.atl(213) 		inc i
   jmp _lbl28
_lbl30:
;### fifteen.atl(210) 			screen(i+xscr,ybtmp)=$ff
   lda eraserect__i
   clc
   adc eraserect__xscr
   sta _s7___21
   ldy ybtmp
   lda screen_lo,y
   sta _arr
   lda screen_hi,y
   sta _arr+1
   ldy _s7___21
   lda #255
   sta (_arr),y
;### fifteen.atl(211) 			inc ybtmp
   inc ybtmp
;### fifteen.atl(212) 			dec j
   dec _s6__j
_lbl28:
;### fifteen.atl(209) 		while j>0
   lda #0
   cmp _s6__j
   jcc _lbl30
_lbl29:
   inc eraserect__i
_lbl26:
;### fifteen.atl(206) 	while i<width
   lda eraserect__i
   cmp eraserect__width
   jcc _lbl31
_lbl27:
   rts
.endp
findempty .proc
;### fifteen.atl(216) 	x=0
   lda #0
   sta findempty__x
;### fifteen.atl(217) 	y=0
   lda #0
   sta findempty__y
;### fifteen.atl(218) 	for k
   lda #0
   sta k
_lbl36:
;### fifteen.atl(219) 		for l
   lda #0
   sta l
_lbl35:
;### fifteen.atl(220) 			if playfield(k,l) = T_EMPTY
   ldy l
   lda playfield_lo,y
   sta _arr
   lda playfield_hi,y
   sta _arr+1
   ldy k
   lda (_arr),y
   sta _94
   lda _94
   cmp #root__T_EMPTY
   jne _lbl34
;### fifteen.atl(221) 				x=k
   lda k
   sta findempty__x
;### fifteen.atl(222) 				y=l
   lda l
   sta findempty__y
_lbl34:
   inc l
   lda l
   cmp #4
   jne _lbl35
   inc k
   lda k
   cmp #4
   jne _lbl36
   rts
.endp
gettile .proc
;### fifteen.atl(226) 	ty = tile bitand 3
   lda gettile__tile
   and #3
   sta gettile__ty
;### fifteen.atl(227) 	tx = tile / 4
   lda gettile__tile
   lsr
   lsr
   sta gettile__tx
;### fifteen.atl(228) 	tx = tx bitand 3
   lda gettile__tx
   and #3
   sta gettile__tx
   rts
.endp
showboard .proc
;### fifteen.atl(231) 	for k
   lda #0
   sta k
_lbl42:
;### fifteen.atl(232) 		for l
   lda #0
   sta l
_lbl41:
;### fifteen.atl(233) 			xbtmp = 6*k+XOFF
   lda k
   asl
   clc
   adc k
   asl
   sta _s11___25
   lda _s11___25
   clc
   adc #root__XOFF
   sta xbtmp
;### fifteen.atl(234) 			ybtmp = 48*l+YOFF
   lda l
   asl
   clc
   adc l
   asl
   asl
   asl
   asl
   sta _s11___27
   lda _s11___27
   clc
   adc #root__YOFF
   sta ybtmp
;### fifteen.atl(235) 			tile = playfield(k,l)
   ldy l
   lda playfield_lo,y
   sta _arr
   lda playfield_hi,y
   sta _arr+1
   ldy k
   lda (_arr),y
   sta _s11__tile
;### fifteen.atl(236) 			if tile <> T_EMPTY
   lda _s11__tile
   cmp #root__T_EMPTY
   jeq _lbl39
;### fifteen.atl(237) 				i,j=gettile tile
   lda _s11__tile
   sta gettile__tile
   jsr gettile
   lda gettile__tx
   sta _s11__i
   lda gettile__ty
   sta _s11__j
;### fifteen.atl(238) 				copytile i j xbtmp ybtmp 6 48
   lda _s11__i
   sta copytile__xbm
   lda _s11__j
   sta copytile__ybm
   lda xbtmp
   sta copytile__xscr
   lda ybtmp
   sta copytile__yscr
   lda #6
   sta copytile__xsize
   lda #48
   sta copytile__ysize
   jsr copytile
;### fifteen.atl(239) 			else
   jmp _lbl40
_lbl39:
;### fifteen.atl(240) 				eraserect  xbtmp ybtmp 6 48
   lda xbtmp
   sta eraserect__xscr
   lda ybtmp
   sta eraserect__yscr
   lda #6
   sta eraserect__width
   lda #48
   sta eraserect__height
   jsr eraserect
_lbl40:
   inc l
   lda l
   cmp #4
   jne _lbl41
   inc k
   lda k
   cmp #4
   jne _lbl42
   rts
.endp
movetile .proc
;### fifteen.atl(247) 	xoffset=1
   lda #1
   sta movetile__xoffset
;### fifteen.atl(248) 	yoffset=1
   lda #1
   sta movetile__yoffset
;### fifteen.atl(249) 	AUDC1=$22
   lda #34
   sta AUDC1
;### fifteen.atl(250) 	if m = D_UP
   lda movetile__m
   cmp #root__D_UP
   jne _lbl44
;### fifteen.atl(251) 		if ys<3
   lda movetile__ys
   cmp #3
   jcs _lbl44
;### fifteen.atl(252) 			yoffset=2
   lda #2
   sta movetile__yoffset
_lbl44:
;### fifteen.atl(254) 	if m = D_RIGHT
   lda movetile__m
   cmp #root__D_RIGHT
   jne _lbl46
;### fifteen.atl(255) 		if xs>0
   lda #0
   cmp movetile__xs
   jcs _lbl46
;### fifteen.atl(256) 			xoffset=0
   lda #0
   sta movetile__xoffset
_lbl46:
;### fifteen.atl(258) 	if m = D_DOWN
   lda movetile__m
   cmp #root__D_DOWN
   jne _lbl48
;### fifteen.atl(259) 		if ys>0
   lda #0
   cmp movetile__ys
   jcs _lbl48
;### fifteen.atl(260) 			yoffset=0
   lda #0
   sta movetile__yoffset
_lbl48:
;### fifteen.atl(262) 	if m = D_LEFT
   lda movetile__m
   cmp #root__D_LEFT
   jne _lbl50
;### fifteen.atl(263) 		if xs<3
   lda movetile__xs
   cmp #3
   jcs _lbl50
;### fifteen.atl(264) 			xoffset=2
   lda #2
   sta movetile__xoffset
_lbl50:
;### fifteen.atl(266) 	if xoffset <>1 or yoffset <>1
   lda movetile__xoffset
   cmp #1
   jne _lbl52
_lbl51:
   lda movetile__yoffset
   cmp #1
   jeq _lbl54
_lbl52:
;### fifteen.atl(268) 		xbtmp=xs+xoffset
   lda movetile__xs
   clc
   adc movetile__xoffset
   sta xbtmp
;### fifteen.atl(269) 		dec xbtmp
   dec xbtmp
;### fifteen.atl(270) 		ybtmp=ys+yoffset
   lda movetile__ys
   clc
   adc movetile__yoffset
   sta ybtmp
;### fifteen.atl(271) 		dec ybtmp
   dec ybtmp
;### fifteen.atl(272) 		playfield(xs,ys)=playfield(xbtmp,ybtmp)
   ldy ybtmp
   lda playfield_lo,y
   sta _arr
   lda playfield_hi,y
   sta _arr+1
   ldy xbtmp
   lda (_arr),y
   sta _95
   ldy movetile__ys
   lda playfield_lo,y
   sta _arr
   lda playfield_hi,y
   sta _arr+1
   ldy movetile__xs
   lda _95
   sta (_arr),y
;### fifteen.atl(273) 		playfield(xbtmp,ybtmp)=T_EMPTY
   ldy ybtmp
   lda playfield_lo,y
   sta _arr
   lda playfield_hi,y
   sta _arr+1
   ldy xbtmp
   lda #root__T_EMPTY
   sta (_arr),y
;### fifteen.atl(274) 		inc moves
   inc moves+0
   jne _lbl161
   inc moves+1
_lbl161:
;### fifteen.atl(275) 		if moves > 999 moves=999
   lda moves+1
   cmp #3
   jcc _lbl54
   jne _lbl162
   lda moves
   cmp #231
   jcc _lbl54
_lbl162:
   lda #231
   sta moves
   lda #3
   sta moves+1
_lbl54:
;### fifteen.atl(277) 	xoffset=0
   lda #0
   sta movetile__xoffset
;### fifteen.atl(278) 	yoffset=0
   lda #0
   sta movetile__yoffset
;### fifteen.atl(279) 	if show >0
   lda #0
   cmp movetile__show
   jcs _lbl58
;### fifteen.atl(280) 		z=0
   lda #0
   sta movetile__z
;### fifteen.atl(281) 		if show=2 z=5
   lda movetile__show
   cmp #2
   jne _lbl57
   lda #5
   sta movetile__z
_lbl56:
;### fifteen.atl(346) 	AUDC1=0
   jmp _lbl57
_lbl75:
;### fifteen.atl(284) 			xbtmp=xs*6+XOFF
   lda movetile__xs
   ldx #6
   jsr _sys_mul8
   lda _TEMPW2
   sta _s12___31
   lda _TEMPW2+1
   sta _s12___31+1
   lda _s12___31
   clc
   adc #root__XOFF
   sta xbtmp
;### fifteen.atl(285) 			ybtmp=ys*48+YOFF
   lda movetile__ys
   ldx #48
   jsr _sys_mul8
   lda _TEMPW2
   sta _s12___33
   lda _TEMPW2+1
   sta _s12___33+1
   lda _s12___33
   clc
   adc #root__YOFF
   sta ybtmp
;### fifteen.atl(286) 			AUDF1=z+xs
   lda movetile__z
   clc
   adc movetile__xs
   sta AUDF1
;### fifteen.atl(287) 			xtilesize=6
   lda #6
   sta xtilesize
;### fifteen.atl(288) 			ytilesize=48
   lda #48
   sta ytilesize
;### fifteen.atl(290) 			if m = D_LEFT
   lda movetile__m
   cmp #root__D_LEFT
   jne _lbl60
;### fifteen.atl(291) 				if xs<3
   lda movetile__xs
   cmp #3
   jcs _lbl60
;### fifteen.atl(292) 					xoffset=5-z
   lda #5
   sec
   sbc movetile__z
   sta movetile__xoffset
;### fifteen.atl(293) 					yoffset=0
   lda #0
   sta movetile__yoffset
;### fifteen.atl(294) 					btmp1 = xbtmp+6
   lda xbtmp
   clc
   adc #6
   sta btmp1
;### fifteen.atl(295) 					btmp1 = btmp1+xoffset
   lda btmp1
   clc
   adc movetile__xoffset
   sta btmp1
;### fifteen.atl(296) 					btmp2=ybtmp+yoffset
   lda ybtmp
   clc
   adc movetile__yoffset
   sta btmp2
;### fifteen.atl(297) 					xtilesize=1
   lda #1
   sta xtilesize
_lbl60:
;### fifteen.atl(299) 			if m = D_DOWN
   lda movetile__m
   cmp #root__D_DOWN
   jne _lbl62
;### fifteen.atl(300) 				if ys>0
   lda #0
   cmp movetile__ys
   jcs _lbl62
;### fifteen.atl(301) 					yoffset=z-5
   lda movetile__z
   sec
   sbc #5
   sta movetile__yoffset
;### fifteen.atl(302) 					yoffset = yoffset * 8
   lda movetile__yoffset
   asl
   asl
   asl
   sta movetile__yoffset
;### fifteen.atl(303) 					xoffset=0
   lda #0
   sta movetile__xoffset
;### fifteen.atl(304) 					btmp1=xbtmp+xoffset
   lda xbtmp
   clc
   adc movetile__xoffset
   sta btmp1
;### fifteen.atl(305) 					btmp2=ybtmp+yoffset
   lda ybtmp
   clc
   adc movetile__yoffset
   sta btmp2
;### fifteen.atl(306) 					btmp2=btmp2-8
   lda btmp2
   sec
   sbc #8
   sta btmp2
;### fifteen.atl(307) 					if z=5 btmp2 = btmp2-40
   lda movetile__z
   cmp #5
   jne _lbl63
   lda btmp2
   sec
   sbc #40
   sta btmp2
_lbl63:
;### fifteen.atl(308) 					ytilesize=8
   lda #8
   sta ytilesize
_lbl62:
;### fifteen.atl(309) 			if m = D_RIGHT
   lda movetile__m
   cmp #root__D_RIGHT
   jne _lbl65
;### fifteen.atl(310) 				if xs>0
   lda #0
   cmp movetile__xs
   jcs _lbl65
;### fifteen.atl(311) 					xoffset=z-5
   lda movetile__z
   sec
   sbc #5
   sta movetile__xoffset
;### fifteen.atl(312) 					yoffset=0
   lda #0
   sta movetile__yoffset
;### fifteen.atl(313) 					btmp1=xbtmp+xoffset
   lda xbtmp
   clc
   adc movetile__xoffset
   sta btmp1
;### fifteen.atl(314) 					dec btmp1
   dec btmp1
;### fifteen.atl(315) 					if z=5 btmp1 = btmp1-5
   lda movetile__z
   cmp #5
   jne _lbl66
   lda btmp1
   sec
   sbc #5
   sta btmp1
_lbl66:
;### fifteen.atl(316) 					btmp2=ybtmp+yoffset
   lda ybtmp
   clc
   adc movetile__yoffset
   sta btmp2
;### fifteen.atl(317) 					xtilesize=1
   lda #1
   sta xtilesize
_lbl65:
;### fifteen.atl(319) 			if m = D_UP
   lda movetile__m
   cmp #root__D_UP
   jne _lbl68
;### fifteen.atl(320) 				if ys<3
   lda movetile__ys
   cmp #3
   jcs _lbl68
;### fifteen.atl(321) 					yoffset=5-z
   lda #5
   sec
   sbc movetile__z
   sta movetile__yoffset
;### fifteen.atl(322) 					yoffset = yoffset * 8
   lda movetile__yoffset
   asl
   asl
   asl
   sta movetile__yoffset
;### fifteen.atl(323) 					xoffset=0
   lda #0
   sta movetile__xoffset
;### fifteen.atl(324) 					btmp1=xbtmp+xoffset
   lda xbtmp
   clc
   adc movetile__xoffset
   sta btmp1
;### fifteen.atl(325) 					btmp2=ybtmp+yoffset
   lda ybtmp
   clc
   adc movetile__yoffset
   sta btmp2
;### fifteen.atl(326) 					btmp2=btmp2+48
   lda btmp2
   clc
   adc #48
   sta btmp2
;### fifteen.atl(327) 					ytilesize=8
   lda #8
   sta ytilesize
_lbl68:
;### fifteen.atl(329) 			if xtilesize<>6 or ytilesize<>48
   lda xtilesize
   cmp #6
   jne _lbl70
_lbl69:
   lda ytilesize
   cmp #48
   jeq _lbl71
_lbl70:
;### fifteen.atl(330) 				if z=5 xtilesize=6 ytilesize=48 
   lda movetile__z
   cmp #5
   jne _lbl72
   lda #6
   sta xtilesize
   lda #48
   sta ytilesize
_lbl72:
;### fifteen.atl(331) 				eraserect btmp1 btmp2 xtilesize ytilesize
   lda btmp1
   sta eraserect__xscr
   lda btmp2
   sta eraserect__yscr
   lda xtilesize
   sta eraserect__width
   lda ytilesize
   sta eraserect__height
   jsr eraserect
_lbl71:
;### fifteen.atl(334) 			tile = playfield(xs,ys)
   ldy movetile__ys
   lda playfield_lo,y
   sta _arr
   lda playfield_hi,y
   sta _arr+1
   ldy movetile__xs
   lda (_arr),y
   sta movetile__tile
;### fifteen.atl(335) 			if tile <> T_EMPTY
   lda movetile__tile
   cmp #root__T_EMPTY
   jeq _lbl74
;### fifteen.atl(336) 				if show >0
   lda #0
   cmp movetile__show
   jcs _lbl74
;### fifteen.atl(337) 					k,l = gettile tile
   lda movetile__tile
   sta gettile__tile
   jsr gettile
   lda gettile__tx
   sta k
   lda gettile__ty
   sta l
;### fifteen.atl(338) 					btmp1=xs*6
   lda movetile__xs
   asl
   clc
   adc movetile__xs
   asl
   sta btmp1
;### fifteen.atl(339) 					btmp1=btmp1+XOFF
   inc btmp1
;### fifteen.atl(340) 					btmp1=btmp1+xoffset
   lda btmp1
   clc
   adc movetile__xoffset
   sta btmp1
;### fifteen.atl(341) 					btmp2=ys*48
   lda movetile__ys
   asl
   clc
   adc movetile__ys
   asl
   asl
   asl
   asl
   sta btmp2
;### fifteen.atl(342) 					btmp2=btmp2+YOFF
   lda btmp2
   clc
   adc #root__YOFF
   sta btmp2
;### fifteen.atl(343) 					btmp2=btmp2+yoffset
   lda btmp2
   clc
   adc movetile__yoffset
   sta btmp2
;### fifteen.atl(344) 					copytile k l btmp1 btmp2 6 48
   lda k
   sta copytile__xbm
   lda l
   sta copytile__ybm
   lda btmp1
   sta copytile__xscr
   lda btmp2
   sta copytile__yscr
   lda #6
   sta copytile__xsize
   lda #48
   sta copytile__ysize
   jsr copytile
_lbl74:
;### fifteen.atl(345) 			inc z
   inc movetile__z
_lbl57:
;### fifteen.atl(282) 		while z<6
   lda movetile__z
   cmp #6
   jcc _lbl75
_lbl58:
   lda #0
   sta AUDC1
   rts
.endp
clrscr .proc
;### fifteen.atl(350) 	i=40
   lda #40
   sta clrscr__i
;### fifteen.atl(358) 			screen(i,j)=255
   jmp _lbl79
_lbl81:
;### fifteen.atl(352) 		dec i
   dec clrscr__i
;### fifteen.atl(353) 		j=200
   lda #200
   sta _s13__j
;### fifteen.atl(358) 			screen(i,j)=255
   jmp _lbl78
_lbl80:
;### fifteen.atl(355) 			dec j
   dec _s13__j
;### fifteen.atl(356) 			screen(i,j)=255
   ldy _s13__j
   lda screen_lo,y
   sta _arr
   lda screen_hi,y
   sta _arr+1
   ldy clrscr__i
   lda #255
   sta (_arr),y
_lbl78:
;### fifteen.atl(354) 		while j>0
   lda #0
   cmp _s13__j
   jcc _lbl80
_lbl79:
;### fifteen.atl(351) 	while i>0
   lda #0
   cmp clrscr__i
   jcc _lbl81
_lbl77:
   rts
.endp
changepicture .proc
;### fifteen.atl(360) 	if piccnt=0
   lda piccnt
   cmp #0
   jne _lbl82
;### fifteen.atl(361) 		currentbuf=buf1
   lda #<buf1
   sta currentbuf
   lda #>buf1
   sta currentbuf+1
;### fifteen.atl(362) 		c=$1c
   lda #28
   sta changepicture__c
_lbl82:
;### fifteen.atl(363) 	if piccnt=1
   lda piccnt
   cmp #1
   jne _lbl83
;### fifteen.atl(364) 		currentbuf=buf2
   lda #<buf2
   sta currentbuf
   lda #>buf2
   sta currentbuf+1
;### fifteen.atl(365) 		c=$0e
   lda #14
   sta changepicture__c
_lbl83:
;### fifteen.atl(366) 	if piccnt=2
   lda piccnt
   cmp #2
   jne _lbl84
;### fifteen.atl(367) 		currentbuf=buf3
   lda #<buf3
   sta currentbuf
   lda #>buf3
   sta currentbuf+1
;### fifteen.atl(368) 		c=$3a
   lda #58
   sta changepicture__c
_lbl84:
;### fifteen.atl(369) 	if piccnt=3
   lda piccnt
   cmp #3
   jne _lbl85
;### fifteen.atl(370) 		currentbuf=buf4
   lda #<buf4
   sta currentbuf
   lda #>buf4
   sta currentbuf+1
;### fifteen.atl(371) 		c=$7a
   lda #122
   sta changepicture__c
_lbl85:
;### fifteen.atl(373) 	inc piccnt
   inc piccnt
;### fifteen.atl(374) 	if piccnt>=4 piccnt=0
   lda piccnt
   cmp #4
   jcc _lbl86
   lda #0
   sta piccnt
_lbl86:
   rts
.endp
countgoodtiles .proc
;### fifteen.atl(379) 	res=0
   lda #0
   sta countgoodtiles__res
;### fifteen.atl(380) 	for k
   lda #0
   sta k
_lbl91:
;### fifteen.atl(381) 		for l
   lda #0
   sta l
_lbl90:
;### fifteen.atl(382) 			tile=k*4
   lda k
   asl
   asl
   sta _s16__tile
;### fifteen.atl(383) 			tile=tile+l
   lda _s16__tile
   clc
   adc l
   sta _s16__tile
;### fifteen.atl(384) 			if playfield(k,l) = tile inc res
   ldy l
   lda playfield_lo,y
   sta _arr
   lda playfield_hi,y
   sta _arr+1
   ldy k
   lda (_arr),y
   sta _96
   lda _96
   cmp _s16__tile
   jne _lbl89
   inc countgoodtiles__res
_lbl89:
   inc l
   lda l
   cmp #4
   jne _lbl90
   inc k
   lda k
   cmp #4
   jne _lbl91
   rts
.endp
inittiles .proc
;### fifteen.atl(387) 	for k
   lda #0
   sta k
_lbl95:
;### fifteen.atl(388) 		for l
   lda #0
   sta l
_lbl94:
;### fifteen.atl(389) 			tile=k*4
   lda k
   asl
   asl
   sta _s18__tile
;### fifteen.atl(390) 			tile=tile+l
   lda _s18__tile
   clc
   adc l
   sta _s18__tile
;### fifteen.atl(391) 			playfield(k,l)=tile
   ldy l
   lda playfield_lo,y
   sta _arr
   lda playfield_hi,y
   sta _arr+1
   ldy k
   lda _s18__tile
   sta (_arr),y
   inc l
   lda l
   cmp #4
   jne _lbl94
   inc k
   lda k
   cmp #4
   jne _lbl95
   rts
.endp
setboardcolor .proc
;### fifteen.atl(394) 	player(0).scolor=col
   lda setboardcolor__col
   sta player__scolor
;### fifteen.atl(395) 	player(1).scolor=col
   lda setboardcolor__col
   sta player__scolor+1
;### fifteen.atl(396) 	player(2).scolor=col
   lda setboardcolor__col
   sta player__scolor+2
   rts
.endp
changecolor .proc
;### fifteen.atl(400) 	btmp1=col bitand $f
   lda changecolor__col
   and #15
   sta btmp1
;### fifteen.atl(401) 	btmp2=col bitand $f0
   lda changecolor__col
   and #240
   sta btmp2
;### fifteen.atl(403) 	xbtmp=currcol bitand $f
   lda currcol
   and #15
   sta xbtmp
;### fifteen.atl(404) 	ybtmp=currcol bitand $f0
   lda currcol
   and #240
   sta ybtmp
;### fifteen.atl(419) 		sleep 1
   jmp _lbl96
_lbl100:
;### fifteen.atl(408) 		if xbtmp<btmp1
   lda xbtmp
   cmp btmp1
   jcs _lbl98
;### fifteen.atl(410) 			xbtmp = xbtmp + 2
   lda xbtmp
   clc
   adc #2
   sta xbtmp
;### fifteen.atl(411) 		else
   jmp _lbl99
_lbl98:
;### fifteen.atl(413) 			xbtmp = xbtmp - 2
   lda xbtmp
   sec
   sbc #2
   sta xbtmp
_lbl99:
;### fifteen.atl(414) 		currcol = btmp2
   lda btmp2
   sta currcol
;### fifteen.atl(415) 		currcol = currcol + xbtmp
   lda currcol
   clc
   adc xbtmp
   sta currcol
;### fifteen.atl(416) 		setboardcolor currcol
   lda currcol
   sta setboardcolor__col
   jsr setboardcolor
;### fifteen.atl(417) 		sleep 1
   lda #1
   sta sleep__time
   jsr sleep
_lbl96:
;### fifteen.atl(407) 	while btmp1 <> xbtmp
   lda btmp1
   cmp xbtmp
   jne _lbl100
_lbl97:
   rts
.endp
drawmainscreen .proc
;### fifteen.atl(421) 	copyblock mains 0 0 14 26 0 14 200
   lda #<mains
   sta copyblock__srcbm
   lda #>mains
   sta copyblock__srcbm+1
   lda #0
   sta copyblock__xbm
   lda #0
   sta copyblock__ybm
   lda #0
   sta copyblock__ybm+1
   lda #14
   sta copyblock__wbm
   lda #26
   sta copyblock__xscr
   lda #0
   sta copyblock__yscr
   lda #14
   sta copyblock__xsize
   lda #200
   sta copyblock__ysize
   jsr copyblock
;### fifteen.atl(422) 	j=8
   lda #8
   sta drawmainscreen__j
;### fifteen.atl(423) 	ii:byte=1
   lda #1
   sta drawmainscreen__ii
;### fifteen.atl(436) 	tmp1 = changepicture
   jmp _lbl101
_lbl106:
;### fifteen.atl(427) 		copyblock leafh 0 0 6 ii 0 6 8
   lda #<leafh
   sta copyblock__srcbm
   lda #>leafh
   sta copyblock__srcbm+1
   lda #0
   sta copyblock__xbm
   lda #0
   sta copyblock__ybm
   lda #0
   sta copyblock__ybm+1
   lda #6
   sta copyblock__wbm
   lda drawmainscreen__ii
   sta copyblock__xscr
   lda #0
   sta copyblock__yscr
   lda #6
   sta copyblock__xsize
   lda #8
   sta copyblock__ysize
   jsr copyblock
;### fifteen.atl(428) 		kk=0
   lda #0
   sta drawmainscreen__kk
;### fifteen.atl(433) 		j=j+48
   jmp _lbl103
_lbl105:
;### fifteen.atl(431) 			copyblock leafv 0 0 1 kk j 1 48
   lda #<leafv
   sta copyblock__srcbm
   lda #>leafv
   sta copyblock__srcbm+1
   lda #0
   sta copyblock__xbm
   lda #0
   sta copyblock__ybm
   lda #0
   sta copyblock__ybm+1
   lda #1
   sta copyblock__wbm
   lda drawmainscreen__kk
   sta copyblock__xscr
   lda drawmainscreen__j
   sta copyblock__yscr
   lda #1
   sta copyblock__xsize
   lda #48
   sta copyblock__ysize
   jsr copyblock
;### fifteen.atl(432) 			kk=kk+25
   lda drawmainscreen__kk
   clc
   adc #25
   sta drawmainscreen__kk
_lbl103:
;### fifteen.atl(430) 		while kk<=25
   lda #25
   cmp drawmainscreen__kk
   jcs _lbl105
_lbl104:
   lda drawmainscreen__j
   clc
   adc #48
   sta drawmainscreen__j
;### fifteen.atl(434) 		ii=ii+6
   lda drawmainscreen__ii
   clc
   adc #6
   sta drawmainscreen__ii
_lbl101:
;### fifteen.atl(425) 	while ii<24
   lda drawmainscreen__ii
   cmp #24
   jcc _lbl106
_lbl102:
   jsr changepicture
   lda changepicture__c
   sta tmp1
;### fifteen.atl(437) 	showboard
   jsr showboard
;### fifteen.atl(438) 	currcol=0
   lda #0
   sta currcol
;### fifteen.atl(439) 	changecolor tmp1
   lda tmp1
   sta changecolor__col
   jsr changecolor
   rts
.endp
shuffletiles .proc
;### fifteen.atl(442) 	c:byte=0
   lda #0
   sta shuffletiles__c
loop:
;### fifteen.atl(444) 	dir=RANDOM bitand 3
   lda RANDOM
   and #3
   sta dir
;### fifteen.atl(445) 	x,y=findempty
   jsr findempty
   lda findempty__x
   sta shuffletiles__x
   lda findempty__y
   sta shuffletiles__y
;### fifteen.atl(446) 	movetile x y dir 2
   lda shuffletiles__x
   sta movetile__xs
   lda shuffletiles__y
   sta movetile__ys
   lda dir
   sta movetile__m
   lda #2
   sta movetile__show
   jsr movetile
;### fifteen.atl(447) 	cnt=countgoodtiles
   jsr countgoodtiles
   lda countgoodtiles__res
   sta shuffletiles__cnt
;### fifteen.atl(448) 	if c<50 inc c
   lda shuffletiles__c
   cmp #50
   jcs _lbl107
   inc shuffletiles__c
_lbl107:
;### fifteen.atl(449) 	if cnt>14 or c<50 goto loop
   lda #14
   cmp shuffletiles__cnt
   jcc loop
_lbl108:
   lda shuffletiles__c
   cmp #50
   jcc loop
_lbl109:
_lbl110:
   rts
.endp
cycle .proc
;### fifteen.atl(453) 	if play = 1
   lda play
   cmp #1
   jne _lbl111
;### fifteen.atl(454) 		music'play
   jsr rmt_play
_lbl111:
;### fifteen.atl(455) 	vcnt=0
   lda #0
   sta vcnt
   jmp $e462
.endp
fillpmg .proc
;### fifteen.atl(460) 	for j:3..7
   lda #3
   sta _s22__j
_lbl115:
;### fifteen.atl(461) 		for i:0..255
   lda #0
   sta _s23__i
_lbl114:
;### fifteen.atl(462) 			pmg(j).memory(i)=$0
   ldy _s22__j
   lda pmg__memory_lo,y
   sta _arr
   lda pmg__memory_hi,y
   sta _arr+1
   ldy _s23__i
   lda #0
   sta (_arr),y
   inc _s23__i
   lda _s23__i
   cmp #0
   jne _lbl114
   inc _s22__j
   lda _s22__j
   cmp #8
   jne _lbl115
;### fifteen.atl(464) 	for i:32..223
   lda #32
   sta _s24__i
_lbl117:
;### fifteen.atl(465) 		pmg(4).memory(i)=$ff
   ldy #4
   lda pmg__memory_lo,y
   sta _arr
   lda pmg__memory_hi,y
   sta _arr+1
   ldy _s24__i
   lda #255
   sta (_arr),y
;### fifteen.atl(466) 		pmg(5).memory(i)=$ff
   ldy #5
   lda pmg__memory_lo,y
   sta _arr
   lda pmg__memory_hi,y
   sta _arr+1
   ldy _s24__i
   lda #255
   sta (_arr),y
;### fifteen.atl(467) 		pmg(6).memory(i)=$ff
   ldy #6
   lda pmg__memory_lo,y
   sta _arr
   lda pmg__memory_hi,y
   sta _arr+1
   ldy _s24__i
   lda #255
   sta (_arr),y
   inc _s24__i
   lda _s24__i
   cmp #224
   jne _lbl117
;### fifteen.atl(470) 	for i:24..231
   lda #24
   sta _s25__i
_lbl119:
;### fifteen.atl(471) 		pmg(3).memory(i)=$ff
   ldy #3
   lda pmg__memory_lo,y
   sta _arr
   lda pmg__memory_hi,y
   sta _arr+1
   ldy _s25__i
   lda #255
   sta (_arr),y
;### fifteen.atl(472) 		pmg(7).memory(i)=$fe
   ldy #7
   lda pmg__memory_lo,y
   sta _arr
   lda pmg__memory_hi,y
   sta _arr+1
   ldy _s25__i
   lda #254
   sta (_arr),y
   inc _s25__i
   lda _s25__i
   cmp #232
   jne _lbl119
;### fifteen.atl(474) 	for i:150..159
   lda #150
   sta _s26__i
_lbl121:
;### fifteen.atl(475) 		pmg(7).memory(i)=$00
   ldy #7
   lda pmg__memory_lo,y
   sta _arr
   lda pmg__memory_hi,y
   sta _arr+1
   ldy _s26__i
   lda #0
   sta (_arr),y
   inc _s26__i
   lda _s26__i
   cmp #160
   jne _lbl121
   rts
.endp
windowpmg .proc
;### fifteen.atl(482) 	for i:$6e..$91
   lda #110
   sta _s27__i
_lbl123:
;### fifteen.atl(483) 		pmg(4).memory(i)=$fc
   ldy #4
   lda pmg__memory_lo,y
   sta _arr
   lda pmg__memory_hi,y
   sta _arr+1
   ldy _s27__i
   lda #252
   sta (_arr),y
;### fifteen.atl(484) 		pmg(5).memory(i)=$0
   ldy #5
   lda pmg__memory_lo,y
   sta _arr
   lda pmg__memory_hi,y
   sta _arr+1
   ldy _s27__i
   lda #0
   sta (_arr),y
;### fifteen.atl(485) 		pmg(6).memory(i)=$3f
   ldy #6
   lda pmg__memory_lo,y
   sta _arr
   lda pmg__memory_hi,y
   sta _arr+1
   ldy _s27__i
   lda #63
   sta (_arr),y
   inc _s27__i
   lda _s27__i
   cmp #146
   jne _lbl123
   rts
.endp
setpmg .proc
;### fifteen.atl(489) 	init'pmgbase
   lda #>pmg__memory
   sta pmg__base
;### fifteen.atl(490) 	fillpmg
   jsr fillpmg
;### fifteen.atl(495) 	GTICTLS=$18
   lda #24
   sta GTICTLS
;### fifteen.atl(497) 	player(0).x=52
   lda #52
   sta player__x
;### fifteen.atl(498) 	player(1).x=84
   lda #84
   sta player__x+1
;### fifteen.atl(499) 	player(2).x=116
   lda #116
   sta player__x+2
;### fifteen.atl(500) 	player(3).x=166
   lda #166
   sta player__x+3
;### fifteen.atl(501) 	player(0).size=3
   lda #3
   sta player__size
;### fifteen.atl(502) 	player(1).size=3
   lda #3
   sta player__size+1
;### fifteen.atl(503) 	player(2).size=3
   lda #3
   sta player__size+2
;### fifteen.atl(504) 	player(3).size=3
   lda #3
   sta player__size+3
;### fifteen.atl(505) 	missile(0).x=152
   lda #152
   sta missile__x
;### fifteen.atl(506) 	missile(1).x=160
   lda #160
   sta missile__x+1
;### fifteen.atl(507) 	player(3).x=166
   lda #166
   sta player__x+3
;### fifteen.atl(508) 	missile(2).x=192
   lda #192
   sta missile__x+2
;### fifteen.atl(509) 	missile(3).x=200
   lda #200
   sta missile__x+3
;### fifteen.atl(510) 	player(3).scolor=$98
   lda #152
   sta player__scolor+3
;### fifteen.atl(511) 	COLOR0(3)=$96
   lda #150
   sta COLOR0+3
;### fifteen.atl(512) 	missile.size=$ff
   lda #255
   sta missile__size
;### fifteen.atl(513) 	DMACTL = playfield + dl + players + missiles + hires_sprites
   lda #62
   sta DMACTL
;### fifteen.atl(514) 	GRACTL = players + missiles
   lda #3
   sta GRACTL
   rts
.endp
start_timer .proc
;### fifteen.atl(520) 	timer=0
   lda #0
   sta timer
;### fifteen.atl(521) 	tsec=0
   lda #0
   sta tsec
;### fifteen.atl(522) 	tdsec=0
   lda #0
   sta tdsec
;### fifteen.atl(523) 	tmin=0
   lda #0
   sta tmin
;### fifteen.atl(524) 	tdmin=0
   lda #0
   sta tdmin
   rts
.endp
update_timer .proc
;### fifteen.atl(528) 	if timer>49
   lda #49
   cmp timer
   jcs _lbl124
;### fifteen.atl(529) 		timer = timer -50
   lda timer
   sec
   sbc #50
   sta timer
;### fifteen.atl(530) 		inc tsec
   inc tsec
_lbl124:
;### fifteen.atl(532) 	if tsec=10
   lda tsec
   cmp #10
   jne _lbl125
;### fifteen.atl(533) 		tsec=0
   lda #0
   sta tsec
;### fifteen.atl(534) 		inc tdsec
   inc tdsec
_lbl125:
;### fifteen.atl(536) 	if tdsec=6
   lda tdsec
   cmp #6
   jne _lbl126
;### fifteen.atl(537) 		tdsec=0
   lda #0
   sta tdsec
;### fifteen.atl(538) 		inc tmin
   inc tmin
_lbl126:
;### fifteen.atl(540) 	if tmin=10
   lda tmin
   cmp #10
   jne _lbl127
;### fifteen.atl(541) 		tmin=0
   lda #0
   sta tmin
;### fifteen.atl(542) 		inc tdmin
   inc tdmin
_lbl127:
;### fifteen.atl(544) 	if tdmin=6
   lda tdmin
   cmp #6
   jne _lbl128
;### fifteen.atl(545) 		tdmin=5
   lda #5
   sta tdmin
;### fifteen.atl(546) 		tmin=9
   lda #9
   sta tmin
;### fifteen.atl(547) 		tdsec=5
   lda #5
   sta tdsec
;### fifteen.atl(548) 		tsec=9
   lda #9
   sta tsec
_lbl128:
   rts
.endp
drawbesttime .proc
;### fifteen.atl(551) 	if besttime(0) <> 0
   lda besttime
   sta _97
   lda _97
   cmp #0
   jeq _lbl129
;### fifteen.atl(553) 		tcnt=0
   lda #0
   sta drawbesttime__tcnt
;### fifteen.atl(560) 		text 26 181 14
   jmp _lbl130
_lbl132:
;### fifteen.atl(556) 			tmpb= besttime(tcnt)
   ldx drawbesttime__tcnt
   lda besttime,x
   sta drawbesttime__tmpb
;### fifteen.atl(557) 			textbuf(tcnt)=tmpb
   lda drawbesttime__tmpb
   ldx drawbesttime__tcnt
   sta textbuf,x
;### fifteen.atl(559) 			inc tcnt
   inc drawbesttime__tcnt
_lbl130:
;### fifteen.atl(555) 		while tcnt<14
   lda drawbesttime__tcnt
   cmp #14
   jcc _lbl132
_lbl131:
   lda #26
   sta text__xt
   lda #181
   sta text__yt
   lda #14
   sta text__len
   jsr text
;### fifteen.atl(562) 		textbuf="IN [bmoves] MOVES!   "
   lda #<textbuf
   sta _arr
   lda #>textbuf
   sta _arr+1
   jsr _std_print_adr
   dta b(3),c'IN '
   dta b(130),a(bmoves)
   dta b(10),c' MOVES!   '
   dta b(0)
;### fifteen.atl(563) 		text 27 192 13
   lda #27
   sta text__xt
   lda #192
   sta text__yt
   lda #13
   sta text__len
   jsr text
_lbl129:
   rts
.endp
initscreen .proc
;### fifteen.atl(568) 	DMACTL=0
   lda #0
   sta DMACTL
;### fifteen.atl(569) 	sdlstl = dl
   lda #<dl
   sta SDLSTL
   lda #>dl
   sta SDLSTL+1
;### fifteen.atl(570) 	currentbuf=buf1
   lda #<buf1
   sta currentbuf
   lda #>buf1
   sta currentbuf+1
;### fifteen.atl(577) 	setpmg
   jsr setpmg
;### fifteen.atl(578) 	play=0
   lda #0
   sta play
;### fifteen.atl(579) 	on'vbi cycle
   lda #<cycle
   sta initscreen___86
   lda #>cycle
   sta initscreen___86+1
   lda initscreen___86
   sta VVBLKD
   lda initscreen___86+1
   sta VVBLKD+1
;### fifteen.atl(580) 	on'dli set_col
   lda #<set_col
   sta initscreen___87
   lda #>set_col
   sta initscreen___87+1
   lda initscreen___87
   sta VDSLST
   lda initscreen___87+1
   sta VDSLST+1
   lda #192
   sta NMIEN
   rts
.endp
titlescreen .proc
;### fifteen.atl(583) 	DMACTL=0
   lda #0
   sta DMACTL
;### fifteen.atl(584) 	clrscr
   jsr clrscr
;### fifteen.atl(585) 	piccnt = RANDOM bitand 3
   lda RANDOM
   and #3
   sta piccnt
;### fifteen.atl(586) 	inittiles
   jsr inittiles
;### fifteen.atl(587) 	drawmainscreen
   jsr drawmainscreen
;### fifteen.atl(588) 	fillpmg
   jsr fillpmg
;### fifteen.atl(589) 	DMACTL = playfield + dl + players + missiles + hires_sprites
   lda #62
   sta DMACTL
;### fifteen.atl(591) 	CH=62
   lda #62
   sta CH
;### fifteen.atl(593) 	COLOR0(1) = $c0
   lda #192
   sta COLOR0+1
;### fifteen.atl(594) 	COLOR0(2) = $c8
   lda #200
   sta COLOR0+2
;### fifteen.atl(595) 	COLOR0(4) = $c0
   lda #192
   sta COLOR0+4
;### fifteen.atl(597) 	drawbesttime
   jsr drawbesttime
loop:
;### fifteen.atl(600) 	if CH = 28  ; ESC 
   lda CH
   cmp #28
   jne _lbl134
;### fifteen.atl(601) 		CH = none
   lda #key__none
   sta CH
;### fifteen.atl(602) 		ctmp=countgoodtiles
   jsr countgoodtiles
   lda countgoodtiles__res
   sta titlescreen__ctmp
;### fifteen.atl(603) 		if ctmp<16
   lda titlescreen__ctmp
   cmp #16
   jcs _lbl134
;### fifteen.atl(604) 			ctmp=currcol
   lda currcol
   sta titlescreen__ctmp
;### fifteen.atl(605) 			changecolor 0
   lda #0
   sta changecolor__col
   jsr changecolor
;### fifteen.atl(606) 			inittiles
   jsr inittiles
;### fifteen.atl(607) 			showboard
   jsr showboard
;### fifteen.atl(608) 			changecolor ctmp
   lda titlescreen__ctmp
   sta changecolor__col
   jsr changecolor
_lbl134:
;### fifteen.atl(610) 	if CH=62
   lda CH
   cmp #62
   jne _lbl137
;### fifteen.atl(611) 		CH = none
   lda #key__none
   sta CH
;### fifteen.atl(612) 		if play = 1
   lda play
   cmp #1
   jne _lbl136
;### fifteen.atl(613) 			music'off
   jsr rmt_silence
;### fifteen.atl(614) 			play = 0
   lda #0
   sta play
;### fifteen.atl(615) 		else
   jmp _lbl137
_lbl136:
;### fifteen.atl(616) 			music'init musmod
   lda #<musmod
   sta titlescreen___89
   lda #>musmod
   sta titlescreen___89+1
   ldx titlescreen___89
   ldy titlescreen___89+1
   lda #0
   jsr rmt_init
;### fifteen.atl(617) 			play=1
   lda #1
   sta play
_lbl137:
;### fifteen.atl(619) 	if CONSOL = 3
   lda CONSOL
   cmp #3
   jne _lbl138
;### fifteen.atl(620) 		shuffletiles
   jsr shuffletiles
;### fifteen.atl(621) 		showboard
   jsr showboard
;### fifteen.atl(622) 		goto loop
   jmp loop
_lbl138:
;### fifteen.atl(624) 	if CONSOL = 5
   lda CONSOL
   cmp #5
   jne _lbl139
;### fifteen.atl(625) 		changecolor 0
   lda #0
   sta changecolor__col
   jsr changecolor
;### fifteen.atl(626) 		btmp1 = changepicture
   jsr changepicture
   lda changepicture__c
   sta btmp1
;### fifteen.atl(627) 		showboard
   jsr showboard
;### fifteen.atl(628) 		changecolor btmp1
   lda btmp1
   sta changecolor__col
   jsr changecolor
;### fifteen.atl(629) 		goto loop
   jmp loop
_lbl139:
;### fifteen.atl(632) 	if CONSOL <> 6
   lda CONSOL
   cmp #6
   jne loop
_lbl140:
   rts
.endp
game .proc
;### fifteen.atl(638) 	showboard
   jsr showboard
;### fifteen.atl(639) 	play=0
   lda #0
   sta play
;### fifteen.atl(640) 	music'off
   jsr rmt_silence
;### fifteen.atl(641) 	eraserect 26 140 14 60
   lda #26
   sta eraserect__xscr
   lda #140
   sta eraserect__yscr
   lda #14
   sta eraserect__width
   lda #60
   sta eraserect__height
   jsr eraserect
;### fifteen.atl(643) 	cnt=countgoodtiles
   jsr countgoodtiles
   lda countgoodtiles__res
   sta game__cnt
;### fifteen.atl(644) 	if cnt=16 shuffletiles showboard
   lda game__cnt
   cmp #16
   jne _lbl141
   jsr shuffletiles
   jsr showboard
_lbl141:
;### fifteen.atl(645) 	moves=0
   lda #0
   sta moves
   lda #0
   sta moves+1
;### fifteen.atl(646) 	start_timer
   jsr start_timer
;### fifteen.atl(648) 	drawbesttime
   jsr drawbesttime
gameloop:
;### fifteen.atl(651) 	CH = none
   lda #key__none
   sta CH
;### fifteen.atl(653) 	cnt=countgoodtiles
   jsr countgoodtiles
   lda countgoodtiles__res
   sta game__cnt
;### fifteen.atl(655) 	textbuf="Good tiles:[cnt] "
   lda #<textbuf
   sta _arr
   lda #>textbuf
   sta _arr+1
   jsr _std_print_adr
   dta b(11),c'Good tiles:'
   dta b(129),a(game__cnt)
   dta b(1),c' '
   dta b(0)
;### fifteen.atl(656) 	text 27 141 13
   lda #27
   sta text__xt
   lda #141
   sta text__yt
   lda #13
   sta text__len
   jsr text
;### fifteen.atl(658) 	update_timer
   jsr update_timer
;### fifteen.atl(659) 	textbuf="Time: [tdmin][tmin]:[tdsec][tsec]"
   lda #<textbuf
   sta _arr
   lda #>textbuf
   sta _arr+1
   jsr _std_print_adr
   dta b(6),c'Time: '
   dta b(129),a(tdmin)
   dta b(129),a(tmin)
   dta b(1),c':'
   dta b(129),a(tdsec)
   dta b(129),a(tsec)
   dta b(0)
;### fifteen.atl(660) 	text 27 152 11
   lda #27
   sta text__xt
   lda #152
   sta text__yt
   lda #11
   sta text__len
   jsr text
;### fifteen.atl(662) 	if cnt=16
   lda game__cnt
   cmp #16
   jne _lbl142
;### fifteen.atl(663) 		textbuf="            "
   lda #<textbuf
   sta _arr
   lda #>textbuf
   sta _arr+1
   jsr _std_print_adr
   dta b(12),c'            '
   dta b(0)
;### fifteen.atl(664) 		text 7 86 12
   lda #7
   sta text__xt
   lda #86
   sta text__yt
   lda #12
   sta text__len
   jsr text
;### fifteen.atl(665) 		text 7 102 12
   lda #7
   sta text__xt
   lda #102
   sta text__yt
   lda #12
   sta text__len
   jsr text
;### fifteen.atl(666) 		text 7 114 12
   lda #7
   sta text__xt
   lda #114
   sta text__yt
   lda #12
   sta text__len
   jsr text
;### fifteen.atl(667) 		windowpmg
   jsr windowpmg
;### fifteen.atl(668) 		textbuf=" Well done! "
   lda #<textbuf
   sta _arr
   lda #>textbuf
   sta _arr+1
   jsr _std_print_adr
   dta b(12),c' Well done! '
   dta b(0)
;### fifteen.atl(669) 		text 7 94 12
   lda #7
   sta text__xt
   lda #94
   sta text__yt
   lda #12
   sta text__len
   jsr text
;### fifteen.atl(670) 		textbuf=" Time:[tdmin][tmin]:[tdsec][tsec] "
   lda #<textbuf
   sta _arr
   lda #>textbuf
   sta _arr+1
   jsr _std_print_adr
   dta b(6),c' Time:'
   dta b(129),a(tdmin)
   dta b(129),a(tmin)
   dta b(1),c':'
   dta b(129),a(tdsec)
   dta b(129),a(tsec)
   dta b(1),c' '
   dta b(0)
;### fifteen.atl(671) 		text 7 106 12
   lda #7
   sta text__xt
   lda #106
   sta text__yt
   lda #12
   sta text__len
   jsr text
;### fifteen.atl(672) 		if moves<bmoves
   lda moves
   cmp bmoves
   lda moves+1
   sbc bmoves+1
   jcs _lbl143
;### fifteen.atl(673) 			besttime="BESTTIME:[tdmin][tmin]:[tdsec][tsec]"
   lda #<besttime
   sta _arr
   lda #>besttime
   sta _arr+1
   jsr _std_print_adr
   dta b(9),c'BESTTIME:'
   dta b(129),a(tdmin)
   dta b(129),a(tmin)
   dta b(1),c':'
   dta b(129),a(tdsec)
   dta b(129),a(tsec)
   dta b(0)
;### fifteen.atl(674) 			sleep 100
   lda #100
   sta sleep__time
   jsr sleep
;### fifteen.atl(675) 			textbuf=" NEW RECORD!"
   lda #<textbuf
   sta _arr
   lda #>textbuf
   sta _arr+1
   jsr _std_print_adr
   dta b(12),c' NEW RECORD!'
   dta b(0)
;### fifteen.atl(676) 			text 7 94 12
   lda #7
   sta text__xt
   lda #94
   sta text__yt
   lda #12
   sta text__len
   jsr text
;### fifteen.atl(677) 			textbuf=" [moves] MOVES!    "
   lda #<textbuf
   sta _arr
   lda #>textbuf
   sta _arr+1
   jsr _std_print_adr
   dta b(1),c' '
   dta b(130),a(moves)
   dta b(11),c' MOVES!    '
   dta b(0)
;### fifteen.atl(678) 			text 7 106 12
   lda #7
   sta text__xt
   lda #106
   sta text__yt
   lda #12
   sta text__len
   jsr text
;### fifteen.atl(679) 			bmoves=moves
   lda moves
   sta bmoves
   lda moves+1
   sta bmoves+1
;### fifteen.atl(680) 			dir=0
   lda #0
   sta dir
;### fifteen.atl(681) 			AUDC1=$68
   lda #104
   sta AUDC1
;### fifteen.atl(688) 			AUDC1=0
   jmp _lbl144
_lbl146:
;### fifteen.atl(683) 				AUDF1=dir
   lda dir
   sta AUDF1
;### fifteen.atl(684) 				setboardcolor RANDOM
   lda RANDOM
   sta setboardcolor__col
   jsr setboardcolor
;### fifteen.atl(685) 				sleep 1
   lda #1
   sta sleep__time
   jsr sleep
;### fifteen.atl(686) 				inc dir
   inc dir
_lbl144:
;### fifteen.atl(682) 			while dir<200
   lda dir
   cmp #200
   jcc _lbl146
_lbl145:
   lda #0
   sta AUDC1
;### fifteen.atl(689) 			setboardcolor currcol
   lda currcol
   sta setboardcolor__col
   jsr setboardcolor
;### fifteen.atl(690) 			dir=0
   lda #0
   sta dir
;### fifteen.atl(691) 		else
   jmp _lbl147
_lbl143:
;### fifteen.atl(692) 			sleep 200
   lda #200
   sta sleep__time
   jsr sleep
_lbl147:
;### fifteen.atl(694) 		CH=28
   lda #28
   sta CH
;### fifteen.atl(695) 		goto gameend
   jmp _lbl153
_lbl142:
;### fifteen.atl(697) 	textbuf="Moves: [moves]     "
   lda #<textbuf
   sta _arr
   lda #>textbuf
   sta _arr+1
   jsr _std_print_adr
   dta b(7),c'Moves: '
   dta b(130),a(moves)
   dta b(5),c'     '
   dta b(0)
;### fifteen.atl(698) 	text 27 163 12
   lda #27
   sta text__xt
   lda #163
   sta text__yt
   lda #12
   sta text__len
   jsr text
;### fifteen.atl(700) 	ss:stick'state = STICK(0)
   lda STICK
   sta game__ss
;### fifteen.atl(702) 	dir=0
   lda #0
   sta dir
;### fifteen.atl(703) 	if ss = 14 dir = 1
   lda game__ss
   cmp #14
   jne _lbl148
   lda #1
   sta dir
_lbl148:
;### fifteen.atl(704) 	if ss = 7  dir = 2
   lda game__ss
   cmp #7
   jne _lbl149
   lda #2
   sta dir
_lbl149:
;### fifteen.atl(705) 	if ss = 13 dir = 3
   lda game__ss
   cmp #13
   jne _lbl150
   lda #3
   sta dir
_lbl150:
;### fifteen.atl(706) 	if ss = 11 dir = 4
   lda game__ss
   cmp #11
   jne _lbl151
   lda #4
   sta dir
_lbl151:
;### fifteen.atl(707) 	if dir <> 0 
   lda dir
   cmp #0
   jeq _lbl152
;### fifteen.atl(708) 		x,y=findempty
   jsr findempty
   lda findempty__x
   sta game__x
   lda findempty__y
   sta game__y
;### fifteen.atl(709) 		dec dir
   dec dir
;### fifteen.atl(710) 		movetile x y dir 1
   lda game__x
   sta movetile__xs
   lda game__y
   sta movetile__ys
   lda dir
   sta movetile__m
   lda #1
   sta movetile__show
   jsr movetile
;### fifteen.atl(711) 		dir=0
   lda #0
   sta dir
;### fifteen.atl(712) 		sleep 1
   lda #1
   sta sleep__time
   jsr sleep
_lbl152:
;### fifteen.atl(714) 	if CH <> 28 goto gameloop  ; ESC 
   lda CH
   cmp #28
   jne gameloop
_lbl153:
;### fifteen.atl(718) 	CH = none
   lda #key__none
   sta CH
   rts
.endp
   icl '../../src/platform/atari/atari.asm'
   icl '../../src/processor/m6502/m6502.asm'
   icl '../../src/platform/atari/rmt.asm'

   .align 2048
pmg__memory:
   .ds 8*256
