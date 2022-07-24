	 EVEN
	 xdef	DrawClipLine
DrawClipLine:
	 movem.l #$1f3c,-(sp)
	clr.l d3
	clr.l d4
	move.l 40(sp),a0
	move.l 44(sp),a1
	move.w (a0),d5
	move.w 2(a0),a2
	move.w 4(a0),d7
	move.w 6(a0),a0
	clr.l d0
	move.b (a1),d0
	move.w d0,d1
	swap d1
	 move.w d3,d1
	move.l d1,d3
	clr.w d0
	move.b 1(a1),d0
	move.w d0,d3
	clr.l d0
	move.b 2(a1),d0
	move.w d0,d1
	swap d1
	 move.w d4,d1
	move.l d1,d4
	clr.w d0
	move.b 3(a1),d0
	move.w d0,d4
	move.w d7,d6
	sub.w d5,d6
	 bmi LCLine44
LCLine2:
	move.l d4,d2
	swap d2
	ext.l d2
	cmp.w d5,d2
	 blt LCLine1
	move.l d3,d0
	swap d0
	ext.l d0
	move.l d0,a5
	cmp.w d7,d0
	 bgt LCLine1
	cmp.w a2,d4
	 blt LCLine1
	cmp.w a0,d3
	 bgt LCLine1
	move.w a0,a1
	sub.w a2,a1
	tst.w d6
	 bne LCLine5
	move.w a2,d1
	cmp.w a0,d1
	 ble LCLine6
	move.w a0,a2
	move.w d1,a0
LCLine6:
	move.w d3,d0
	cmp.w a2,d3
	 bge LCLine7
	move.w a2,d0
LCLine7:
	move.w d0,a2
	cmp.w a0,d4
	 bge LCLine9
LCLine43:
	move.w d4,a0
LCLine9:
	move.w 48(sp),-(sp)
	move.w a0,-(sp)
	move.w d7,-(sp)
	move.w a2,-(sp)
	move.w d5,-(sp)
	 jsr DrawLine
	lea 10(sp),sp
	 bra LCLine1
	 EVEN
LCLine5:
	move.w a1,d0
	 bne LCLine10
	move.w a5,d0
	cmp.w a5,d5
	 ble LCLine11
	move.w d5,d0
LCLine11:
	move.w d0,d5
	cmp.w d7,d2
	 bge LCLine9
	move.w d2,d7
	 bra LCLine9
	 EVEN
LCLine10:
	move.w a1,d1
	tst.w d0
	 blt LCLine14
	cmp.w a5,d5
	 bge LCLine15
	cmp.w a2,d3
	 ble LCLine16
	move.w d3,a4
	sub.w a2,a4
	move.w a4,d0
	muls.w d6,d0
	ext.l d0
	divs.w d1,d0
	add.w d5,d0
	cmp.w a5,d0
	 blt LCLine16
	move.w d0,d5
	move.w d3,a2
	cmp.w d0,d2
	 blt LCLine1
LCLine20:
	move.l d4,d2
	swap d2
	ext.l d2
	cmp.w d7,d2
	 bge LCLine23
	cmp.w a0,d4
	 bge LCLine38
	move.w a0,d0
	sub.w d4,d0
	muls.w d6,d0
	move.w a1,d1
	ext.l d0
	divs.w d1,d0
	move.w d7,d1
	sub.w d0,d1
	cmp.w d1,d2
	 blt LCLine38
	move.w d1,d7
	 bra LCLine43
	 EVEN
LCLine38:
	move.l d4,d0
	swap d0
	ext.l d0
	sub.w d0,d7
	move.w a1,d1
	muls.w d7,d1
	ext.l d1
	divs.w d6,d1
	sub.w d1,a0
	move.w d0,d7
	 bra LCLine9
	 EVEN
LCLine23:
	cmp.w a0,d4
	 bge LCLine9
	sub.w d4,a0
	move.w a0,d0
	muls.w d0,d6
	move.w a1,d1
	ext.l d6
	divs.w d1,d6
	sub.w d6,d7
	 bra LCLine43
	 EVEN
LCLine16:
	move.l d3,d0
	swap d0
	ext.l d0
	move.w d0,d1
	sub.w d5,d1
	move.w d1,d5
	move.w a1,d1
	muls.w d1,d5
	ext.l d5
	divs.w d6,d5
	add.w d5,a2
	move.w d0,d5
	cmp.w a2,d4
	 bge LCLine20
	 bra LCLine1
	 EVEN
LCLine15:
	cmp.w a2,d3
	 ble LCLine20
	move.w d3,d0
	sub.w a2,d0
	muls.w d6,d0
	move.w a1,d1
	ext.l d0
	divs.w d1,d0
	add.w d0,d5
	move.w d3,a2
	cmp.w d5,d2
	 bge LCLine20
	 bra LCLine1
	 EVEN
LCLine14:
	move.w a1,d0
	neg.w d0
	move.w d0,a1
	cmp.w a5,d5
	 bge LCLine29
	cmp.w a2,d4
	 bge LCLine30
	move.w a2,a4
	sub.w d4,a4
	move.w a4,d1
	muls.w d6,d1
	ext.l d1
	divs.w d0,d1
	move.w d1,d0
	add.w d5,d0
	cmp.w a5,d0
	 blt LCLine30
	move.w d0,d5
	move.w d4,a2
	cmp.w d0,d2
	 blt LCLine1
LCLine34:
	move.l d4,d2
	swap d2
	ext.l d2
	cmp.w d7,d2
	 bge LCLine37
	cmp.w a0,d3
	 ble LCLine38
	move.w d3,d0
	sub.w a0,d0
	muls.w d6,d0
	move.w a1,d1
	ext.l d0
	divs.w d1,d0
	move.w d7,d1
	sub.w d0,d1
	cmp.w d1,d2
	 blt LCLine38
	move.w d1,d7
LCLine42:
	move.w d3,a0
	 bra LCLine9
	 EVEN
LCLine37:
	cmp.w a0,d3
	 ble LCLine9
	move.w d3,d0
	sub.w a0,d0
	muls.w d0,d6
	move.w a1,d1
	ext.l d6
	divs.w d1,d6
	sub.w d6,d7
	 bra LCLine42
	 EVEN
LCLine30:
	move.l d3,d0
	swap d0
	ext.l d0
	move.w d0,d1
	sub.w d5,d1
	move.w d1,d5
	move.w a1,d1
	muls.w d1,d5
	ext.l d5
	divs.w d6,d5
	add.w d5,a2
	move.w d0,d5
	cmp.w a2,d3
	 ble LCLine34
	 bra LCLine1
	 EVEN
LCLine29:
	cmp.w a2,d4
	 bge LCLine34
	sub.w d4,a2
	move.w a2,d0
	muls.w d6,d0
	move.w a1,d1
	ext.l d0
	divs.w d1,d0
	add.w d0,d5
	move.w d4,a2
	cmp.w d5,d2
	 bge LCLine34
	 bra LCLine1
	 EVEN
LCLine44:
	move.w d5,d0
	move.w d7,d5
	move.w d0,d7
	move.w a2,d0
	move.w a0,a2
	move.w d0,a0
	neg.w d6
	 bra LCLine2
	 EVEN
LCLine1:
	 movem.l (sp)+,#$3cf8
	rts
