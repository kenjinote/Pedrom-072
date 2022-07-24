	 EVEN
	 xdef	DrawClipEllipse
DrawClipEllipse:
	lea -16(sp),sp
	 movem.l #$1f3e,-(sp)
	move.w 64(sp),a2
	move.w 66(sp),d7
	move.w 72(sp),-(sp)
	 jsr SetCurAttr
	move.w d0,52(sp)
	move.l 70(sp),-(sp)
	 jsr SetCurClip
	addq.l #6,sp
	move.w a2,d0
	 ble LElli34
LElli2:
	tst.w d7
	 ble LElli35
LElli3:
	move.w #-1,46(sp)
	move.w #-1,48(sp)
	move.w #-1,a4
	moveq.l #-1,d6
	cmp.w a2,d7
	 bge LElli4
	sub.l a3,a3
	move.w a2,d4
	lsl.w #6,d4
	 EVEN
LElli5:
	move.w d6,44(sp)
	move.w a4,42(sp)
	move.w 48(sp),a6
	move.w 46(sp),a5
	lea 32(a3),a3
	move.w a3,d6
	asr.w #6,d6
	lea -32(a3),a3
	add.w #32,d4
	move.w d4,d1
	asr.w #6,d1
	move.w d1,a4
	add.w #-32,d4
	move.w d6,d3
	muls.w d7,d3
	move.w a2,d2
	ext.l d3
	divs.w d2,d3
	move.w d3,48(sp)
	muls.w d7,d1
	ext.l d1
	divs.w d2,d1
	move.w d1,46(sp)
	cmp.w 44(sp),d6
	 beq LElli36
LElli9:
	cmp.w 42(sp),d6
	 blt LElli37
LElli8:
	move.w 42(sp),d2
	cmp.w a4,d2
	 beq LElli38
LElli14:
	cmp.w a4,d6
	 blt LElli39
LElli13:
	move.w d4,d0
	move.w a2,d2
	ext.l d0
	divs.w d2,d0
	add.w d0,a3
	move.w a3,d0
	ext.l d0
	divs.w d2,d0
	sub.w d0,d4
	cmp.w a4,d6
	 blt LElli5
	 bra LElli19
	 EVEN
LElli39:
	move.w 62(sp),d5
	add.w d3,d5
	move.w d5,-(sp)
	move.w 62(sp),a5
	add.w a4,a5
	move.w a5,-(sp)
	lea DrawClipPix,a6
	 jsr (a6)
	addq.l #4,sp
	move.w a4,d0
	 bne LElli40
LElli15:
	tst.w d3
	 beq LElli13
	move.w 62(sp),d2
	sub.w d3,d2
	move.w d2,d3
	move.w d2,-(sp)
	move.w a5,-(sp)
	 jsr (a6)
	addq.l #4,sp
	move.w a4,d0
	 beq LElli13
	move.w d3,-(sp)
	move.w 62(sp),d1
	sub.w a4,d1
	move.w d1,-(sp)
	 jsr (a6)
	addq.l #4,sp
	 bra LElli13
	 EVEN
LElli40:
	move.w d5,-(sp)
	move.w 62(sp),d1
	sub.w a4,d1
	move.w d1,-(sp)
	 jsr (a6)
	addq.l #4,sp
	 bra LElli15
	 EVEN
LElli38:
	cmp.w a6,d3
	 bne LElli14
	 bra LElli13
	 EVEN
LElli37:
	move.w 62(sp),d5
	add.w 46(sp),d5
	move.w d5,-(sp)
	move.w 62(sp),a5
	add.w d6,a5
	move.w a5,-(sp)
	 jsr DrawClipPix
	addq.l #4,sp
	tst.w d6
	 bne LElli41
LElli10:
	tst.w 46(sp)
	 beq LElli8
	move.w 62(sp),d5
	sub.w 46(sp),d5
	move.w d5,-(sp)
	move.w a5,-(sp)
	 jsr DrawClipPix
	addq.l #4,sp
	tst.w d6
	 beq LElli8
	move.w d5,-(sp)
	move.w 62(sp),d1
	sub.w d6,d1
	move.w d1,-(sp)
	 jsr DrawClipPix
	addq.l #4,sp
	 bra LElli8
	 EVEN
LElli41:
	move.w d5,-(sp)
	move.w 62(sp),d0
	sub.w d6,d0
	move.w d0,-(sp)
	 jsr DrawClipPix
	addq.l #4,sp
	 bra LElli10
	 EVEN
LElli36:
	cmp.w a5,d1
	 bne LElli9
	 bra LElli8
	 EVEN
LElli4:
	sub.l a3,a3
	move.w d7,d4
	lsl.w #6,d4
	 EVEN
LElli20:
	move.w d6,44(sp)
	move.w a4,42(sp)
	move.w 48(sp),a6
	move.w 46(sp),a5
	lea 32(a3),a3
	move.w a3,d6
	asr.w #6,d6
	lea -32(a3),a3
	add.w #32,d4
	move.w d4,d0
	asr.w #6,d0
	move.w d0,a4
	add.w #-32,d4
	move.w a2,d3
	muls.w d6,d3
	ext.l d3
	divs.w d7,d3
	move.w d3,48(sp)
	move.w a4,d5
	move.w a2,d1
	muls.w d1,d5
	ext.l d5
	divs.w d7,d5
	move.w d5,46(sp)
	cmp.w a6,d3
	 beq LElli42
LElli24:
	cmp.w a4,d6
	 blt LElli43
LElli23:
	cmp.w a5,d5
	 beq LElli44
LElli29:
	cmp.w 42(sp),d6
	 blt LElli45
LElli28:
	move.w d4,d0
	ext.l d0
	divs.w d7,d0
	add.w d0,a3
	move.w a3,d0
	ext.l d0
	divs.w d7,d0
	sub.w d0,d4
	cmp.w a4,d6
	 blt LElli20
	 bra LElli19
	 EVEN
LElli45:
	move.w 62(sp),d3
	add.w d6,d3
	move.w d3,-(sp)
	move.w 62(sp),a5
	add.w d5,a5
	move.w a5,-(sp)
	lea DrawClipPix,a6
	 jsr (a6)
	addq.l #4,sp
	tst.w d5
	 bne LElli46
LElli30:
	tst.w d6
	 beq LElli28
	move.w 62(sp),d3
	sub.w d6,d3
	move.w d3,-(sp)
	move.w a5,-(sp)
	 jsr (a6)
	addq.l #4,sp
	tst.w d5
	 beq LElli28
	move.w d3,-(sp)
	move.w 62(sp),d2
	sub.w d5,d2
	move.w d2,-(sp)
	 jsr (a6)
	addq.l #4,sp
	 bra LElli28
	 EVEN
LElli46:
	move.w d3,-(sp)
	move.w 62(sp),d1
	sub.w d5,d1
	move.w d1,-(sp)
	 jsr (a6)
	addq.l #4,sp
	 bra LElli30
	 EVEN
LElli44:
	cmp.w 44(sp),d6
	 bne LElli29
	 bra LElli28
	 EVEN
LElli43:
	move.w 62(sp),a6
	add.w a4,a6
	move.w a6,-(sp)
	move.w 62(sp),d0
	add.w d3,d0
	move.w d0,42(sp)
	move.w d0,-(sp)
	 jsr DrawClipPix
	addq.l #4,sp
	tst.w d3
	 bne LElli47
LElli25:
	move.w a4,d2
	 beq LElli23
	move.w 62(sp),a6
	sub.w a4,a6
	move.w a6,-(sp)
	move.w 42(sp),-(sp)
	 jsr DrawClipPix
	addq.l #4,sp
	tst.w d3
	 beq LElli23
	move.w a6,-(sp)
	move.w 62(sp),d0
	sub.w d3,d0
	move.w d0,-(sp)
	 jsr DrawClipPix
	addq.l #4,sp
	 bra LElli23
	 EVEN
LElli47:
	move.w a6,-(sp)
	move.w 62(sp),d1
	sub.w d3,d1
	move.w d1,-(sp)
	 jsr DrawClipPix
	addq.l #4,sp
	 bra LElli25
	 EVEN
LElli42:
	move.w 42(sp),d2
	cmp.w a4,d2
	 bne LElli24
	 bra LElli23
	 EVEN
LElli35:
	moveq.l #1,d7
	 bra LElli3
	 EVEN
LElli34:
	move.w #1,a2
	 bra LElli2
	 EVEN
LElli19:
	move.w 50(sp),-(sp)
	 jsr SetCurAttr
	pea CLIP_TEMP_RECT
	 jsr SetCurClip
	addq.l #6,sp
	 movem.l (sp)+,#$7cf8
	lea 16(sp),sp
	rts
