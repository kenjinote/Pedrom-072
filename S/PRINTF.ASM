	 EVEN
	 xdef	vcbprintf_display
vcbprintf_display:
	lea -36(sp),sp
	 movem.l #$1f3c,-(sp)
	move.l 76(sp),a4
	move.l 82(sp),a3
	move.l 86(sp),d3
	clr.w d4
	clr.w d7
	move.w (a3),a5
	move.w 6(a3),d6
	move.w 16(a3),d5
	lea 18(a3),a0
	move.w a5,d0
	and.w #272,d0
	 bne LPrt2
	move.w #1,6(a3)
LPrt2:
	move.w 80(sp),d0
	add.w #-88,d0
	cmp.w #32,d0
	 bhi LPrt29
	and.l #$FFFF,d0
	add.l d0,d0
	.set LPrtI60,.+2
	move.w LPrt60(pc,d0.l),d0
	jmp 2(pc,d0.w)
	 EVEN
	 EVEN
LPrt60:
	 dc.w LPrt6-LPrt60
	 dc.w LPrt29-LPrt60
	 dc.w LPrt29-LPrt60
	 dc.w LPrt29-LPrt60
	 dc.w LPrt29-LPrt60
	 dc.w LPrt29-LPrt60
	 dc.w LPrt29-LPrt60
	 dc.w LPrt29-LPrt60
	 dc.w LPrt29-LPrt60
	 dc.w LPrt29-LPrt60
	 dc.w LPrt17-LPrt60
	 dc.w LPrt29-LPrt60
	 dc.w LPrt24-LPrt60
	 dc.w LPrt29-LPrt60
	 dc.w LPrt29-LPrt60
	 dc.w LPrt29-LPrt60
	 dc.w LPrt29-LPrt60
	 dc.w LPrt24-LPrt60
	 dc.w LPrt29-LPrt60
	 dc.w LPrt29-LPrt60
	 dc.w LPrt29-LPrt60
	 dc.w LPrt29-LPrt60
	 dc.w LPrt29-LPrt60
	 dc.w LPrt11-LPrt60
	 dc.w LPrt6-LPrt60
	 dc.w LPrt29-LPrt60
	 dc.w LPrt29-LPrt60
	 dc.w LPrt29-LPrt60
	 dc.w LPrt29-LPrt60
	 dc.w LPrt24-LPrt60
	 dc.w LPrt29-LPrt60
	 dc.w LPrt29-LPrt60
	 dc.w LPrt6-LPrt60
	 EVEN
LPrt6:
	tst.l d3
	 beq LPrt3
	move.l 12(a3),a0
LPrt10:
	moveq.l #15,d0
	and.l d3,d0
	move.b 0(a0,d0.l),36(sp,d4.w)
	addq.w #1,d4
	lsr.l #4,d3
	 bne LPrt10
	 EVEN
LPrt3:
	tst.l d3
	 bne LPrt61
	tst.w d4
	 bne LPrt61
	move.b #48,36(sp)
	moveq.l #1,d4
LPrt61:
	sub.w d4,d6
	move.w d6,d0
	not.w d0
	moveq.l #15,d1
	asr.w d1,d0
	and.w d0,d6
	move.w d6,d3
	add.w d4,d3
	move.l 2(a3),-(sp)
	 jsr strlen
	add.w d0,d3
	sub.w d3,d5
	move.w a5,d3
	and.b #-128,d3
	addq.l #4,sp
	 bne LPrt114
	move.w a5,d0
	and.w #1,d0
	move.w d0,a5
	 bne LPrt63
	subq.w #1,d5
	 bmi LPrt63
	 EVEN
LPrt70:
	addq.w #1,d7
	move.l a4,-(sp)
	move.w #32,-(sp)
	move.l 8(a3),a0
	 jsr (a0)
	addq.l #6,sp
	subq.w #1,d5
	 bpl LPrt70
LPrt63:
	move.l 2(a3),a2
	move.b (a2)+,d0
	 bne LPrt75
LPrt107:
	move.w a5,d0
	 bne LPrt76
	moveq.l #48,d0
	tst.b d3
	 bne LPrt78
	moveq.l #32,d0
LPrt78:
	subq.w #1,d5
	 bmi LPrt76
	move.b d0,d3
	ext.w d3
	 EVEN
LPrt82:
	addq.w #1,d7
	move.l a4,-(sp)
	move.w d3,-(sp)
	move.l 8(a3),a0
	 jsr (a0)
	addq.l #6,sp
	subq.w #1,d5
	 bpl LPrt82
LPrt76:
	subq.w #1,d6
	 bmi LPrt110
	 EVEN
LPrt86:
	addq.w #1,d7
	move.l a4,-(sp)
	move.w #48,-(sp)
	move.l 8(a3),a0
	 jsr (a0)
	addq.l #6,sp
	subq.w #1,d6
	 bpl LPrt86
LPrt110:
	add.w d4,d7
	move.w d4,d0
	subq.w #1,d4
	tst.w d0
	 ble LPrt112
	 EVEN
LPrt90:
	move.l a4,-(sp)
	move.b 40(sp,d4.w),d1
	ext.w d1
	move.w d1,-(sp)
	move.l 8(a3),a0
	 jsr (a0)
	addq.l #6,sp
	move.w d4,d0
	subq.w #1,d4
	tst.w d0
	 bgt LPrt90
LPrt112:
	move.w a5,d3
	 beq LPrt91
	subq.w #1,d5
	 bmi LPrt91
	 EVEN
LPrt95:
	addq.w #1,d7
	move.l a4,-(sp)
	move.w #32,-(sp)
	move.l 8(a3),a0
	 jsr (a0)
	addq.l #6,sp
	subq.w #1,d5
	 bpl LPrt95
	 bra LPrt91
	 EVEN
LPrt75:
	move.l a4,-(sp)
	ext.w d0
	move.w d0,-(sp)
	move.l 8(a3),a0
	 jsr (a0)
	addq.l #6,sp
	addq.w #1,d7
	move.b (a2)+,d0
	 bne LPrt75
	 bra LPrt107
	 EVEN
LPrt114:
	move.w a5,d1
	and.w #1,d1
	move.w d1,a5
	 bra LPrt63
	 EVEN
LPrt17:
	tst.l d3
	 beq LPrt3
LPrt21:
	move.b d3,d0
	and.b #1,d0
	add.b #48,d0
	move.b d0,36(sp,d4.w)
	addq.w #1,d4
	lsr.l #1,d3
	 bne LPrt21
	 bra LPrt3
	 EVEN
LPrt11:
	tst.l d3
	 beq LPrt98
LPrt15:
	move.b d3,d0
	and.b #7,d0
	add.b #48,d0
	move.b d0,36(sp,d4.w)
	addq.w #1,d4
	lsr.l #3,d3
	 bne LPrt15
LPrt98:
	move.w a5,d0
	and.w #8,d0
	 beq LPrt3
	subq.w #1,6(a3)
	 bra LPrt3
	 EVEN
LPrt24:
	tst.l d3
	 beq LPrt3
LPrt28:
	move.w d4,a2
	pea (10).w
	move.l d3,-(sp)
	 jsr __umodsi3
	addq.l #8,sp
	add.b #48,d0
	move.b d0,36(sp,a2.l)
	addq.w #1,d4
	pea (10).w
	move.l d3,-(sp)
	 jsr __udivsi3
	addq.l #8,sp
	move.l d0,d3
	 bne LPrt28
	 bra LPrt3
	 EVEN
LPrt29:
	move.w (a0),a1
	move.w a1,d1
	and.w #32767,d1
	add.w #-16384,d1
	cmp.w #16383,d1
	 beq LPrt116
	cmp.w #-8192,d1
	 beq LPrt117
	cmp.w #8192,d1
	 beq LPrt118
	clr.b d2
	tst.w d1
	 blt LPrt119
	 EVEN
LPrt39:
	moveq.l #10,d3
	ext.l d1
	divs.w d3,d1
	move.l d1,d0
	swap d0
	add.b #48,d0
	move.b d0,36(sp,d4.w)
	addq.w #1,d4
	tst.w d1
	 bne LPrt39
	tst.b d2
	 beq LPrt43
	move.b #45,36(sp,d4.w)
	addq.w #1,d4
LPrt43:
	move.b #-107,36(sp,d4.w)
	addq.w #1,d4
	clr.b d2
	move.l 6(a0),d3
	moveq.l #7,d1
	 EVEN
LPrt50:
	tst.b d2
	 bne LPrt49
	moveq.l #15,d0
	and.l d3,d0
	 beq LPrt48
LPrt49:
	move.b d3,d0
	and.b #15,d0
	add.b #48,d0
	move.b d0,36(sp,d4.w)
	addq.w #1,d4
	moveq.l #1,d2
LPrt48:
	lsr.l #4,d3
	subq.w #1,d1
	 bpl LPrt50
	move.l 2(a0),d3
	moveq.l #6,d1
	 EVEN
LPrt57:
	tst.b d2
	 bne LPrt56
	moveq.l #15,d0
	and.l d3,d0
	 beq LPrt55
LPrt56:
	move.b d3,d0
	and.b #15,d0
	add.b #48,d0
	move.b d0,36(sp,d4.w)
	addq.w #1,d4
	moveq.l #1,d2
LPrt55:
	lsr.l #4,d3
	subq.w #1,d1
	 bpl LPrt57
	tst.b d2
	 beq LPrt58
	move.b #46,36(sp,d4.w)
	addq.w #1,d4
LPrt58:
	move.b d3,d0
	and.b #15,d0
	add.b #48,d0
	move.b d0,36(sp,d4.w)
	addq.w #1,d4
	move.w a1,d0
	 bge LPrt3
	move.b #-81,36(sp,d4.w)
	addq.w #1,d4
	 bra LPrt3
	 EVEN
LPrt119:
	neg.w d1
	moveq.l #1,d2
	 bra LPrt39
	 EVEN
LPrt118:
	move.b #-66,36(sp)
	move.w a1,d0
	 bge LPrt35
	move.b #45,37(sp)
LPrt115:
	moveq.l #2,d4
	 bra LPrt3
	 EVEN
LPrt35:
	move.b #43,37(sp)
	 bra LPrt115
	 EVEN
LPrt117:
	move.b #46,36(sp)
	move.b #48,37(sp)
	 bra LPrt115
	 EVEN
LPrt116:
	move.b #102,36(sp)
	move.b #101,37(sp)
	move.b #100,38(sp)
	move.b #110,39(sp)
	move.b #117,40(sp)
	moveq.l #5,d4
	 bra LPrt3
	 EVEN
LPrt91:
	move.w d7,d0
	 movem.l (sp)+,#$3cf8
	lea 36(sp),sp
	rts
LPrtC0:
	 dc.b "",0
LPrtC1:
	 dc.b "0123456789ABCDEF",0
LPrtC2:
	 dc.b "0X",0
LPrtC5:
	 dc.b "0b",0
LPrtC8:
	 dc.b " ",0
LPrtC9:
	 dc.b "+",0
LPrtC10:
	 dc.b "-",0
LPrtC7:
	 dc.b "0",0
LPrtC3:
	 dc.b "0123456789abcdef",0
LPrtC6:
	 dc.b "@",0
LPrtC4:
	 dc.b "0x",0
	 EVEN
	 xdef	vcbprintf
vcbprintf:
	lea -32(sp),sp
	 movem.l #$1f3e,-(sp)
	move.l 84(sp),a2
	move.l 88(sp),a3
	lea 40(sp),a5
	clr.w d7
	move.l 76(sp),48(sp)
	move.b (a2)+,d1
	 beq LPrt270
LPrt268:
	cmp.b #37,d1
	 beq LPrt124
	move.l 80(sp),-(sp)
	ext.w d1
	move.w d1,-(sp)
	move.l 8(a5),a0
	 jsr (a0)
	addq.w #1,d7
	addq.l #6,sp
LPrt121:
	move.b (a2)+,d1
	 bne LPrt268
	 bra LPrt270
	 EVEN
LPrt124:
	clr.w d4
	moveq.l #0,d6
	clr.w 6(a5)
	move.l #LPrtC0,2(a5)
	 EVEN
LPrt126:
	move.b (a2)+,d1
	move.b d1,d3
	ext.w d3
	move.w d3,d0
	cmp.w #48,d3
	 bne LPrt291
	or.w #128,d4
	 bra LPrt126
	 EVEN
LPrt291:
	cmp.w #48,d3
	 ble LPrt292
	cmp.w #122,d3
	 beq LPrt126
	 ble LPrt293
	cmp.w #124,d3
	 beq LPrt126
LPrt127:
	moveq.l #0,d3
	cmp.b #42,d1
	 beq LPrt294
	move.w d0,d2
	add.w #-48,d2
	cmp.w #9,d2
	 bhi LPrt145
LPrt149:
	move.l d3,d0
	lsl.l #3,d0
	add.l d3,d0
	move.l d0,a0
	add.l d3,a0
	lea 0(a0,d2.w),a0
	move.l a0,d3
	move.b (a2)+,d1
	move.b d1,d2
	ext.w d2
	add.w #-48,d2
	cmp.w #9,d2
	 bls LPrt149
LPrt145:
	move.l d3,d0
	not.l d0
	add.l d0,d0
	subx.l d0,d0
	move.w d3,d5
	and.w d0,d5
	cmp.b #46,d1
	 beq LPrt295
LPrt151:
	cmp.b #108,d1
	 beq LPrt164
	cmp.b #76,d1
	 beq LPrt164
	cmp.b #104,d1
	 beq LPrt167
	cmp.b #72,d1
	 beq LPrt167
	or.w #64,d4
LPrt165:
	ext.w d1
	move.w d1,a1
	move.w d1,d3
	moveq.l #121,d0
	cmp.w d1,d0
	 bcs LPrt254
	clr.l d0
	move.w d1,d0
	add.l d0,d0
	.set LPrtI267,.+2
	move.w LPrt267(pc,d0.l),d0
	jmp 2(pc,d0.w)
	 EVEN
	 EVEN
LPrt267:
	 dc.w LPrt170-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt247-LPrt267
	 dc.w LPrt247-LPrt267
	 dc.w LPrt247-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt247-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt197-LPrt267
	 dc.w LPrt247-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt207-LPrt267
	 dc.w LPrt253-LPrt267
	 dc.w LPrt227-LPrt267
	 dc.w LPrt247-LPrt267
	 dc.w LPrt247-LPrt267
	 dc.w LPrt247-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt227-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt216-LPrt267
	 dc.w LPrt213-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt247-LPrt267
	 dc.w LPrt171-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt222-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt254-LPrt267
	 dc.w LPrt201-LPrt267
	 dc.w LPrt247-LPrt267
	 EVEN
LPrt170:
	subq.l #1,a2
	 bra LPrt121
	 EVEN
LPrt197:
	move.l #LPrtC1,12(a5)
	move.w d4,d0
	and.w #8,d0
	move.l #LPrtC2,d1
LPrt290:
	tst.w d0
	 bne LPrt209
	move.l #LPrtC0,d1
LPrt209:
	move.l d1,2(a5)
	move.w d4,d0
	and.w #32,d0
	 beq LPrt210
	move.l a3,a0
	addq.l #4,a3
	move.l (a0),d6
LPrt284:
	move.w d4,d0
	and.w #16,d0
	 beq LPrt169
	and.w #-129,d4
LPrt169:
	move.w d5,16(a5)
	move.w d4,(a5)
	move.l d6,-(sp)
	move.l a5,-(sp)
	move.w a1,-(sp)
	move.l 90(sp),-(sp)
	 jsr vcbprintf_display
	add.w d0,d7
	lea 14(sp),sp
	 bra LPrt121
	 EVEN
LPrt210:
	move.l a3,a0
	addq.l #2,a3
	clr.l d6
	move.w (a0),d6
	 bra LPrt284
LPrt200:
	 EVEN
LPrt207:
	move.w d4,d0
	and.w #8,d0
	move.l #LPrtC5,d1
	 bra LPrt290
	 EVEN
LPrt253:
	move.l a3,a0
	addq.l #2,a3
	move.b 1(a0),d3
	ext.w d3
LPrt254:
	subq.w #1,d5
	move.w d4,d6
	and.w #1,d6
	 bne LPrt255
	moveq.l #48,d0
	tst.b d4
	 blt LPrt257
	moveq.l #32,d0
LPrt257:
	subq.w #1,d5
	 bmi LPrt255
	move.b d0,d4
	ext.w d4
LPrt261:
	addq.w #1,d7
	move.l 80(sp),-(sp)
	move.w d4,-(sp)
	move.l 8(a5),a0
	 jsr (a0)
	addq.l #6,sp
	subq.w #1,d5
	 bpl LPrt261
LPrt255:
	move.l 80(sp),-(sp)
	move.w d3,-(sp)
	move.l 8(a5),a0
	 jsr (a0)
	addq.w #1,d7
	addq.l #6,sp
	tst.w d6
	 beq LPrt121
	subq.w #1,d5
	 bmi LPrt121
LPrt266:
	addq.w #1,d7
	move.l 80(sp),-(sp)
	move.w #32,-(sp)
	move.l 8(a5),a0
	 jsr (a0)
	addq.l #6,sp
	subq.w #1,d5
	 bpl LPrt266
	 bra LPrt121
	 EVEN
LPrt227:
	move.w d4,d0
	and.w #32,d0
	 beq LPrt228
	move.l a3,a0
	addq.l #4,a3
	move.l (a0),a0
LPrt229:
	move.l a0,d6
	 blt LPrt296
LPrt230:
	cmp.w #0,a0
	 blt LPrt231
	move.w d4,d0
	and.w #2,d0
	 bne LPrt233
	move.w d4,d0
	and.w #4,d0
	move.l #LPrtC8,d1
LPrt288:
	tst.w d0
	 bne LPrt232
	move.l #LPrtC0,d1
LPrt232:
	move.l d1,2(a5)
	 bra LPrt284
	 EVEN
LPrt233:
	move.l #LPrtC9,d1
	 bra LPrt232
	 EVEN
LPrt231:
	move.l #LPrtC10,d1
	 bra LPrt232
	 EVEN
LPrt296:
	neg.l d6
	 bra LPrt230
	 EVEN
LPrt228:
	move.l a3,a0
	addq.l #2,a3
	move.w (a0),a0
	 bra LPrt229
	 EVEN
LPrt216:
	move.w d4,d0
	and.w #32,d0
	 beq LPrt217
	move.l a3,a0
	addq.l #4,a3
	move.l (a0),d6
LPrt218:
	move.w d4,d0
	and.w #8,d0
	move.l #LPrtC7,d1
	 bra LPrt288
	 EVEN
LPrt217:
	move.l a3,a0
	addq.l #2,a3
	clr.l d6
	move.w (a0),d6
	 bra LPrt218
	 EVEN
LPrt213:
	move.l a3,a0
	addq.l #4,a3
	move.l (a0),d6
	move.l #LPrtC3,12(a5)
	move.w d4,d0
	and.w #8,d0
	move.l #LPrtC6,d1
	tst.w d0
	 bne LPrt215
	move.l #LPrtC0,d1
LPrt215:
	move.l d1,2(a5)
	move.w #6,6(a5)
	or.w #16,d4
	 bra LPrt169
	 EVEN
LPrt171:
	move.l a3,a0
	addq.l #4,a3
	move.l (a0),a4
	move.w d4,d0
	and.w #16,d0
	 beq LPrt172
	move.w 6(a5),a0
	moveq.l #0,d6
	cmp.l d6,a0
	 ble LPrt179
	tst.b (a4)
	 beq LPrt179
LPrt178:
	addq.l #1,d6
	cmp.l d6,a0
	 ble LPrt179
	tst.b 0(a4,d6.l)
	 bne LPrt178
LPrt179:
	sub.w d6,d5
	move.w d4,d0
	and.w #1,d0
	move.w d0,a6
	 bne LPrt180
	moveq.l #48,d0
	tst.b d4
	 blt LPrt182
	moveq.l #32,d0
LPrt182:
	subq.w #1,d5
	 bmi LPrt180
	move.b d0,d3
	ext.w d3
LPrt186:
	addq.w #1,d7
	move.l 80(sp),-(sp)
	move.w d3,-(sp)
	move.l 8(a5),a0
	 jsr (a0)
	addq.l #6,sp
	subq.w #1,d5
	 bpl LPrt186
LPrt180:
	moveq.l #0,d3
	cmp.l d3,d6
	 bgt LPrt191
LPrt276:
	add.w d6,d7
	move.w a6,d2
	 beq LPrt121
	subq.w #1,d5
	 bmi LPrt121
LPrt196:
	addq.w #1,d7
	move.l 80(sp),-(sp)
	move.w #32,-(sp)
	move.l 8(a5),a0
	 jsr (a0)
	addq.l #6,sp
	subq.w #1,d5
	 bpl LPrt196
	 bra LPrt121
	 EVEN
LPrt191:
	move.l 80(sp),-(sp)
	move.b 0(a4,d3.l),d1
	ext.w d1
	move.w d1,-(sp)
	move.l 8(a5),a0
	 jsr (a0)
	addq.l #6,sp
	addq.l #1,d3
	cmp.l d3,d6
	 bgt LPrt191
	 bra LPrt276
	 EVEN
LPrt172:
	move.l a4,-(sp)
	 jsr strlen
	move.l d0,d6
	addq.l #4,sp
	 bra LPrt179
	 EVEN
LPrt222:
	move.w d4,d0
	and.w #32,d0
	 beq LPrt223
	move.l a3,a0
	addq.l #4,a3
	move.l (a0),d6
LPrt224:
	move.l #LPrtC0,2(a5)
	 bra LPrt284
	 EVEN
LPrt223:
	move.l a3,a0
	addq.l #2,a3
	clr.l d6
	move.w (a0),d6
	 bra LPrt224
	 EVEN
LPrt201:
	move.l #LPrtC3,12(a5)
	move.w d4,d0
	and.w #8,d0
	move.l #LPrtC4,d1
	 bra LPrt290
	 EVEN
LPrt247:
	or.w #256,d4
	move.w d4,d0
	and.w #16,d0
	 bne LPrt248
	move.w #1,6(a5)
LPrt248:
	move.l a3,a0
	lea 10(a3),a3
	move.l (a0),d0
	move.l 4(a0),d1
	move.w 8(a0),d2
	move.l d0,18(a5)
	move.l d1,22(a5)
	move.w d2,26(a5)
	move.w d4,d0
	and.w #2,d0
	 bne LPrt249
	move.w d4,d0
	and.w #4,d0
	move.l #LPrtC8,d1
	tst.w d0
	 bne LPrt250
	move.l #LPrtC0,d1
LPrt250:
	move.l d1,2(a5)
	 bra LPrt169
	 EVEN
LPrt249:
	move.l #LPrtC9,d1
	 bra LPrt250
	 EVEN
LPrt167:
	or.w #64,d4
LPrt281:
	move.b (a2)+,d1
	 bra LPrt165
	 EVEN
LPrt164:
	or.w #32,d4
	 bra LPrt281
	 EVEN
LPrt295:
	sub.l a0,a0
	move.b (a2)+,d1
	cmp.b #42,d1
	 beq LPrt297
	cmp.b #45,d1
	 beq LPrt298
	move.b d1,d2
	ext.w d2
	add.w #-48,d2
	cmp.w #9,d2
	 bhi LPrt153
LPrt161:
	move.l a0,d0
	lsl.l #3,d0
	add.l a0,d0
	lea 0(a0,d0.l),a0
	lea 0(a0,d2.w),a0
	move.b (a2)+,d1
	move.b d1,d2
	ext.w d2
	add.w #-48,d2
	cmp.w #9,d2
	 bls LPrt161
LPrt153:
	cmp.w #0,a0
	 blt LPrt151
	or.w #16,d4
	move.w a0,6(a5)
	 bra LPrt151
	 EVEN
LPrt298:
	move.b (a2)+,d1
	move.w #-1,a0
	cmp.b #49,d1
	 bne LPrt153
	move.w #6,a0
LPrt280:
	move.b (a2)+,d1
	 bra LPrt153
	 EVEN
LPrt297:
	move.l a3,a0
	addq.l #2,a3
	move.w (a0),a0
	 bra LPrt280
	 EVEN
LPrt294:
	move.l a3,a0
	addq.l #2,a3
	move.w (a0),d3
	ext.l d3
	 blt LPrt299
LPrt144:
	move.b (a2)+,d1
	 bra LPrt145
	 EVEN
LPrt299:
	neg.l d3
	eor.w #1,d4
	 bra LPrt144
	 EVEN
LPrt293:
	cmp.w #94,d3
	 beq LPrt126
	 bra LPrt127
	 EVEN
LPrt292:
	cmp.w #35,d3
	 bne LPrt300
	or.w #8,d4
	 bra LPrt126
	 EVEN
LPrt300:
	cmp.w #35,d3
	 ble LPrt301
	cmp.w #43,d3
	 bne LPrt302
	or.w #2,d4
	 bra LPrt126
	 EVEN
LPrt302:
	cmp.w #45,d3
	 bne LPrt127
	and.w #-129,d4
	or.w #1,d4
	 bra LPrt126
	 EVEN
LPrt301:
	cmp.w #32,d3
	 bne LPrt127
	or.w #4,d4
	 bra LPrt126
	 EVEN
LPrt270:
	move.w d7,d0
	 movem.l (sp)+,#$7cf8
	lea 32(sp),sp
	rts
	 EVEN
	 xdef	_sputc
_sputc:
	move.w 4(sp),d0
	move.l 6(sp),a0
	move.l (a0),a1
	move.b d0,(a1)
	addq.l #1,(a0)
	rts
	 EVEN
	 xdef	sprintf
sprintf:
	link.w a6,#-4
	move.l 8(a6),-4(a6)

	nop
 nop

	pea 16(a6)
	move.l 12(a6),-(sp)
	pea -4(a6)
	pea _sputc
	 jsr vcbprintf
	move.l -4(a6),a0
	clr.b (a0)
	lea 16(sp),sp
	unlk a6
	rts
	 EVEN
	 xdef	fputchar
fputchar:
	 movem.l #$1f20,-(sp)
	move.w 28(sp),d6
	 jsr FontGetSys
	move.b d0,d5
	move.w d6,-(sp)
	 jsr FontCharWidth
	move.w d0,d7
	move.w CURRENT_POINT_X,d3
	move.w CURRENT_POINT_Y,d4
	addq.l #2,sp
	cmp.w #10,d6
	 beq LPrt309
	move.w d3,d1
	add.w d0,d1
	clr.w d0
	move.b ScrRect+2,d0
	addq.w #1,d0
	cmp.w d1,d0
	 bge LPrt308
LPrt309:
	move.b d5,d3
	ext.w d3
	add.w d3,d3
	addq.w #6,d3
	add.w d3,d4
	move.w d4,d1
	add.w d3,d1
	clr.w d0
	move.b ScrRect+3,d0
	cmp.w d1,d0
	 blt LPrt313
LPrt310:
	move.w PRINTF_LINE_COUNTER,d0
	move.w d0,d1
	addq.w #1,d1
	move.w d1,PRINTF_LINE_COUNTER
	cmp.w #14,d1
	 bgt LPrt314
LPrt311:
	clr.w d3
LPrt308:
	cmp.w #10,d6
	 beq LPrt312
	move.w #4,-(sp)
	move.w d6,-(sp)
	move.w d4,-(sp)
	move.w d3,-(sp)
	 jsr DrawChar
	add.w d7,d3
	addq.l #8,sp
	 bra LPrt312
	 EVEN
LPrt314:
	move.w #2,-(sp)
	lea ST_busy,a2
	 jsr (a2)
	 jsr ngetchx
	clr.w -(sp)
	 jsr (a2)
	clr.w PRINTF_LINE_COUNTER
	addq.l #4,sp
	 bra LPrt311
	 EVEN
LPrt313:
	clr.w -(sp)
	move.w d3,-(sp)
	pea ScrRect
	pea ScrRect
	 jsr ScrRectScroll
	sub.w d3,d4
	lea 12(sp),sp
	 bra LPrt310
	 EVEN
LPrt312:
	move.w d3,CURRENT_POINT_X
	move.w d4,CURRENT_POINT_Y
	move.w d6,d0
	 movem.l (sp)+,#$4f8
	rts
	 EVEN
	 xdef	printf
printf:
	link.w a6,#0
	pea 12(a6)
	move.l 8(a6),-(sp)
	clr.l -(sp)
	pea fputchar
	 jsr vcbprintf
	lea 16(sp),sp
	unlk a6
	rts
	 EVEN
	 xdef	clrscr
clrscr:
	clr.w CURRENT_POINT_X
	clr.w CURRENT_POINT_Y
	clr.w SHELL_SAVE_Y_POS
	clr.w PRINTF_LINE_COUNTER
	 jsr ScreenClear
	move.b #1,HELP_BEING_DISPLAYED
	 jsr ST_eraseHelp
	rts
