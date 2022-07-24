	 EVEN
	 xdef	BitmapGet
BitmapGet:
	 movem.l #$1f3e,-(sp)
	move.l 44(sp),a2
	move.l 48(sp),a3
	move.l a3,-(sp)
	move.l a2,-(sp)
	 jsr BitmapInit
	clr.w d3
	move.b (a2),d3
	clr.w d2
	move.b 1(a2),d2
	clr.w d5
	move.b 3(a2),d5
	sub.w d2,d5
	addq.w #1,d5
	clr.w d1
	move.b 2(a2),d1
	sub.w d3,d1
	addq.w #1,d1
	move.w d1,d0
	and.w #7,d0
	moveq.l #8,d4
	sub.w d0,d4
	move.w d4,d0
	move.w #255,a5
	move.w a5,d4
	asr.w d0,d4
	lsl.w d0,d4
	move.w d4,a5
	move.w d1,d7
	addq.w #7,d7
	asr.w #3,d7
	addq.l #4,a3
	move.w CURRENT_INCY,d1
	muls.w d1,d2
	move.w d3,d0
	asr.w #3,d0
	add.w d0,d2
	move.l CURRENT_SCREEN,a0
	lea 0(a0,d2.w),a0
	move.w d3,d4
	and.w #7,d4
	move.w d1,a6
	sub.w d7,a6
	addq.l #8,sp
	 beq LBitMap2
	moveq.l #8,d6
	sub.w d4,d6
	tst.w d5
	 beq LBitMap1
	sub.l a4,a4
	sub.l a2,a2
	sub.l a1,a1
	 EVEN
LBitMap12:
	move.b (a0)+,d0
	move.w a4,d1
	move.b d0,d1
	move.w d1,a4
	move.w a4,d0
	lsl.w d4,d0
	move.b d0,(a3)
	cmp.w #1,d7
	 ble LBitMap26
	clr.w d3
	move.w d7,d2
	subq.w #1,d2
	clr.w d1
	 EVEN
LBitMap11:
	move.b (a0),d3
	move.w d3,d0
	asr.w d6,d0
	or.b (a3),d0
	move.b d0,(a3)+
	move.b (a0)+,d0
	move.b d0,d1
	move.w d1,d0
	lsl.w d4,d0
	move.b d0,(a3)
	subq.w #1,d2
	 bne LBitMap11
LBitMap26:
	move.w a1,d0
	move.b (a0),d0
	move.w d0,a1
	asr.w d6,d0
	or.b (a3),d0
	move.w a5,d1
	and.b d0,d1
	move.b d1,(a3)+
	subq.w #1,d5
	move.l a2,d0
	move.w a6,d0
	move.l d0,a2
	add.l a2,a0
	tst.w d5
	 bne LBitMap12
	 bra LBitMap1
	 EVEN
LBitMap2:
	tst.w d5
	 beq LBitMap1
	moveq.l #0,d0
	 EVEN
LBitMap23:
	tst.w d7
	 ble LBitMap29
	move.w d7,d2
	 EVEN
LBitMap22:
	move.b (a0)+,(a3)+
	subq.w #1,d2
	 bne LBitMap22
LBitMap29:
	move.w a5,d1
	and.b d1,-1(a3)
	subq.w #1,d5
	move.w a6,d0
	add.l d0,a0
	tst.w d5
	 bne LBitMap23
LBitMap1:
	 movem.l (sp)+,#$7cf8
	rts
	 EVEN
	 xdef	SpriteX8_or
SpriteX8_or:
	 movem.l #$1f3e,-(sp)
	move.w 44(sp),d3
	move.w 48(sp),d6
	move.l 50(sp),a1
	move.w 54(sp),a5
	move.w CURRENT_INCY,d2
	move.w d2,d0
	muls.w 46(sp),d0
	move.w d3,d1
	asr.w #3,d1
	add.w d1,d0
	move.w d0,a0
	add.l 56(sp),a0
	move.w d3,d5
	and.w #7,d5
	move.w d2,a6
	sub.w a5,a6
	 beq LBitMap31
	moveq.l #8,d7
	sub.w d5,d7
	tst.w d6
	 beq LBitMap30
	sub.l a4,a4
	sub.l a3,a3
	sub.l a2,a2
	 EVEN
LBitMap41:
	move.w a4,d0
	move.b (a1),d0
	move.w d0,a4
	asr.w d5,d0
	or.b (a0),d0
	move.b d0,(a0)+
	moveq.l #1,d1
	cmp.w a5,d1
	 bge LBitMap55
	clr.w d4
	move.w a5,d2
	subq.w #1,d2
	clr.w d3
	 EVEN
LBitMap40:
	move.b (a1)+,d0
	move.b d0,d4
	move.w d4,d0
	lsl.w d7,d0
	move.b (a0),d1
	or.b d0,d1
	move.b d1,(a0)
	move.b (a1),d3
	move.w d3,d0
	asr.w d5,d0
	or.b d0,d1
	move.b d1,(a0)+
	subq.w #1,d2
	 bne LBitMap40
LBitMap55:
	move.b (a1)+,d0
	move.w a2,d1
	move.b d0,d1
	move.w d1,a2
	move.w a2,d0
	lsl.w d7,d0
	or.b d0,(a0)
	subq.w #1,d6
	move.l a3,d0
	move.w a6,d0
	move.l d0,a3
	add.l a3,a0
	tst.w d6
	 bne LBitMap41
	 bra LBitMap30
	 EVEN
LBitMap31:
	tst.w d6
	 beq LBitMap30
	moveq.l #0,d1
	 EVEN
LBitMap52:
	move.w a5,d0
	 ble LBitMap58
	move.w a5,d2
	 EVEN
LBitMap51:
	move.b (a0),d0
	or.b (a1)+,d0
	move.b d0,(a0)+
	subq.w #1,d2
	 bne LBitMap51
LBitMap58:
	subq.w #1,d6
	move.w a6,d1
	add.l d1,a0
	tst.w d6
	 bne LBitMap52
LBitMap30:
	 movem.l (sp)+,#$7cf8
	rts
	 EVEN
	 xdef	SpriteX8_and
SpriteX8_and:
	 movem.l #$1f3e,-(sp)
	move.w 44(sp),d3
	move.w 48(sp),d6
	move.l 50(sp),a1
	move.w 54(sp),a5
	move.w CURRENT_INCY,d2
	move.w d2,d0
	muls.w 46(sp),d0
	move.w d3,d1
	asr.w #3,d1
	add.w d1,d0
	move.w d0,a0
	add.l 56(sp),a0
	move.w d3,d5
	and.w #7,d5
	move.w d2,a6
	sub.w a5,a6
	 beq LBitMap60
	moveq.l #8,d7
	sub.w d5,d7
	tst.w d6
	 beq LBitMap59
	sub.l a4,a4
	sub.l a3,a3
	sub.l a2,a2
	 EVEN
LBitMap70:
	move.w a4,d0
	move.b (a1),d0
	move.w d0,a4
	asr.w d5,d0
	and.b (a0),d0
	move.b d0,(a0)+
	moveq.l #1,d1
	cmp.w a5,d1
	 bge LBitMap84
	clr.w d4
	move.w a5,d2
	subq.w #1,d2
	clr.w d3
	 EVEN
LBitMap69:
	move.b (a1)+,d0
	move.b d0,d4
	move.w d4,d0
	lsl.w d7,d0
	move.b (a0),d1
	and.b d0,d1
	move.b d1,(a0)
	move.b (a1),d3
	move.w d3,d0
	asr.w d5,d0
	and.b d0,d1
	move.b d1,(a0)+
	subq.w #1,d2
	 bne LBitMap69
LBitMap84:
	move.b (a1)+,d0
	move.w a2,d1
	move.b d0,d1
	move.w d1,a2
	move.w a2,d0
	lsl.w d7,d0
	and.b d0,(a0)
	subq.w #1,d6
	move.l a3,d0
	move.w a6,d0
	move.l d0,a3
	add.l a3,a0
	tst.w d6
	 bne LBitMap70
	 bra LBitMap59
	 EVEN
LBitMap60:
	tst.w d6
	 beq LBitMap59
	moveq.l #0,d1
	 EVEN
LBitMap81:
	move.w a5,d0
	 ble LBitMap87
	move.w a5,d2
	 EVEN
LBitMap80:
	move.b (a0),d0
	and.b (a1)+,d0
	move.b d0,(a0)+
	subq.w #1,d2
	 bne LBitMap80
LBitMap87:
	subq.w #1,d6
	move.w a6,d1
	add.l d1,a0
	tst.w d6
	 bne LBitMap81
LBitMap59:
	 movem.l (sp)+,#$7cf8
	rts
	 EVEN
	 xdef	SpriteX8_xor
SpriteX8_xor:
	 movem.l #$1f3e,-(sp)
	move.w 44(sp),d3
	move.w 48(sp),d6
	move.l 50(sp),a1
	move.w 54(sp),a5
	move.w CURRENT_INCY,d2
	move.w d2,d0
	muls.w 46(sp),d0
	move.w d3,d1
	asr.w #3,d1
	add.w d1,d0
	move.w d0,a0
	add.l 56(sp),a0
	move.w d3,d5
	and.w #7,d5
	move.w d2,a6
	sub.w a5,a6
	 beq LBitMap89
	moveq.l #8,d7
	sub.w d5,d7
	tst.w d6
	 beq LBitMap88
	sub.l a4,a4
	sub.l a3,a3
	sub.l a2,a2
	 EVEN
LBitMap99:
	move.w a4,d0
	move.b (a1),d0
	move.w d0,a4
	asr.w d5,d0
	move.b (a0),d1
	eor.b d1,d0
	move.b d0,(a0)+
	moveq.l #1,d3
	cmp.w a5,d3
	 bge LBitMap113
	clr.w d4
	move.w a5,d2
	subq.w #1,d2
	clr.w d3
	 EVEN
LBitMap98:
	move.b (a1)+,d0
	move.b d0,d4
	move.w d4,d0
	lsl.w d7,d0
	move.b (a0),d1
	eor.b d0,d1
	move.b d1,(a0)
	move.b (a1),d3
	move.w d3,d0
	asr.w d5,d0
	eor.b d0,d1
	move.b d1,(a0)+
	subq.w #1,d2
	 bne LBitMap98
LBitMap113:
	move.b (a1)+,d0
	move.w a2,d1
	move.b d0,d1
	move.w d1,a2
	move.w a2,d0
	lsl.w d7,d0
	eor.b d0,(a0)
	subq.w #1,d6
	move.l a3,d3
	move.w a6,d3
	move.l d3,a3
	add.l a3,a0
	tst.w d6
	 bne LBitMap99
	 bra LBitMap88
	 EVEN
LBitMap89:
	tst.w d6
	 beq LBitMap88
	moveq.l #0,d1
	 EVEN
LBitMap110:
	move.w a5,d0
	 ble LBitMap116
	move.w a5,d2
	 EVEN
LBitMap109:
	move.b (a0),d0
	move.b (a1)+,d3
	eor.b d3,d0
	move.b d0,(a0)+
	subq.w #1,d2
	 bne LBitMap109
LBitMap116:
	subq.w #1,d6
	move.w a6,d1
	add.l d1,a0
	tst.w d6
	 bne LBitMap110
LBitMap88:
	 movem.l (sp)+,#$7cf8
	rts
	 EVEN
	 xdef	BitmapPut
BitmapPut:
	link.w a6,#-4
	 movem.l #$1e20,-(sp)
	move.w 8(a6),d6
	move.w 10(a6),d5
	move.l 12(a6),a2
	move.l 16(a6),d1
	move.w 20(a6),d0
	move.w (a2)+,d4
	move.w (a2)+,d3
	move.b d6,-4(a6)
	move.b d5,-3(a6)
	move.b d5,d2
	add.b d4,d2
	move.b d2,-1(a6)
	move.b d6,d2
	add.b d3,d2
	move.b d2,-2(a6)
	cmp.w #6,d0
	 bhi LBitMap117
	and.l #$FFFF,d0
	add.l d0,d0
	.set LBitMapI126,.+2
	move.w LBitMap126(pc,d0.l),d0
	jmp 2(pc,d0.w)
	 EVEN
	 EVEN
LBitMap126:
	 dc.w LBitMap119-LBitMap126
	 dc.w LBitMap123-LBitMap126
	 dc.w LBitMap120-LBitMap126
	 dc.w LBitMap117-LBitMap126
	 dc.w LBitMap121-LBitMap126
	 dc.w LBitMap123-LBitMap126
	 dc.w LBitMap124-LBitMap126
	 EVEN
LBitMap119:
	move.w #1,-(sp)
	move.l d1,-(sp)
	pea -4(a6)
	 jsr ScrRectFill
	lea 10(sp),sp
LBitMap120:
	move.l CURRENT_SCREEN,-(sp)
	subq.w #1,d3
	asr.w #3,d3
	addq.w #1,d3
	move.w d3,-(sp)
	move.l a2,-(sp)
	move.w d4,-(sp)
	move.w d5,-(sp)
	move.w d6,-(sp)
	 jsr SpriteX8_xor
LBitMap127:
	lea 16(sp),sp
	 bra LBitMap117
	 EVEN
LBitMap121:
	clr.w -(sp)
	move.l d1,-(sp)
	pea -4(a6)
	 jsr ScrRectFill
	lea 10(sp),sp
LBitMap123:
	move.l CURRENT_SCREEN,-(sp)
	subq.w #1,d3
	asr.w #3,d3
	addq.w #1,d3
	move.w d3,-(sp)
	move.l a2,-(sp)
	move.w d4,-(sp)
	move.w d5,-(sp)
	move.w d6,-(sp)
	 jsr SpriteX8_or
	 bra LBitMap127
	 EVEN
LBitMap124:
	move.l CURRENT_SCREEN,-(sp)
	subq.w #1,d3
	asr.w #3,d3
	addq.w #1,d3
	move.w d3,-(sp)
	move.l a2,-(sp)
	move.w d4,-(sp)
	move.w d5,-(sp)
	move.w d6,-(sp)
	 jsr SpriteX8_and
	 bra LBitMap127
	 EVEN
LBitMap117:
	 movem.l -24(a6),#$478
	unlk a6
	rts
