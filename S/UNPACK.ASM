	
 xdef _tt_Get8Bit
_tt_Get8Bit:
    lea     _tt_InBuffer,a0
    movea.l (a0),a1           ; address of current byte */
    clr.w   d0
    move.b  (a1)+,d0          ; anyway,read first byte */
    move.l  a1,(a0)           ; and increment pointer */
    move.w  _tt_InMask,d1
    cmpi.b  #$80,d1
    bne.s   __inl_tries
    rts
__inl_tries:
    lsl.w   #8,d0              ; v = *(_tt_InBuffer++) << 8; */
    move.b  (a1),d0           ; v |= *_tt_InBuffer; */
    ; bra.s    try08 */
    cmpi.b  #$08,d1
    bhi.s   __inl_try20
    bne.s   __inl_try02
    andi.w  #$0fff,d0
    lsr.w   #4,d0
    rts
__inl_try40:
    andi.w  #$7fff,d0
    lsr.w   #7,d0
    rts
__inl_try20:
    cmpi.b  #$20,d1
    bhi.s   __inl_try40
    bcs.s   __inl_try10
    andi.w  #$3fff,d0
    lsr.w   #6,d0
    rts
__inl_try10:
    andi.w  #$1fff,d0
    lsr.w   #5,d0
    rts
__inl_try04:
    andi.w  #$07ff,d0
    lsr.w   #3,d0
    rts
__inl_try02:
    cmpi.b  #$02,d1
    bhi.s   __inl_try04
    bne.s   __inl_try01
    andi.w  #$03ff,d0
    lsr.w   #2,d0
    rts
__inl_try01:
    andi.w  #$01ff,d0
    lsr.w   #1,d0
    rts

	
 xdef _tt_GetValue
_tt_GetValue:
    move.l  d3,-(sp)
    clr.l   d1                 ; i */
    move.w  8(sp),d0          ; maxgamma */
    move.l  _tt_InBuffer,a0
    move.w  _tt_InMask,d2

__inl_loop1:
    cmp.w   d1,d0
    bls.s   __inl_exitloop
    ; !(*_tt_InBuffer & _tt_InMask) */
    move.w  d2,d3
    and.b   (a0),d3
    ; if (!(_tt_InMask >>= 1)) _tt_InMask = 0x80,_tt_InBuffer++; */
    ror.b   #1,d2          ; thx tom for the ror with carry trick ;) */
    bcc.s   __inl_samebyte1
    addq.l  #1,a0
__inl_samebyte1:
    ; test if next bit set only now */
    tst.b   d3
    beq.s   __inl_exitloop
    addq.w  #1,d1
    bra.s   __inl_loop1
__inl_exitloop:
    move.l  a0,_tt_InBuffer
    move.w  d2,_tt_InMask
    moveq   #1,d3
    lsl.w   d1,d3
    move.w  d1,-(sp)
    bsr.s       _tt_GetBits
    addq    #2,sp
    or.w    d3,d0
    move.l  (sp)+,d3
    rts

	
 xdef _tt_GetBits
_tt_GetBits:
    clr.w   d0
    move.w  4(sp),d1
    beq.s   __inl_exit          ; just in case ;) */
    movea.l _tt_InBuffer,a0    ; _tt_InBuffer,gcc makes it pc relative :) */
    movea.l d3,a1             ; use a1 to save d3: 4 cycles better than move.w d3,-(sp) */
    move.w  _tt_InMask,d2      ; pc relative too :)) */
    subq.w  #1,d1

__inl_loop0:
    lsl.w   #1,d0
    ; (*_tt_InBuffer & _tt_InMask) */
    move.w  d2,d3
    and.b   (a0),d3
    beq.s   __inl_correct0
    addq.b  #1,d0          ; here,bit #0 of d0 is never set :) it's a four cycles gain :)) */
__inl_correct0:
    ; if (!(_tt_InMask >>= 1)) _tt_InMask = 0x80,_tt_InBuffer++; */
    ror.b   #1,d2
    bcc.s   __inl_samebyte0
    addq.l  #1,a0
__inl_samebyte0:
    dbra    d1,__inl_loop0

    move.w  d2,_tt_InMask      ; saves _tt_InMask */
    move.l  a1,d3             ; restores d3 (4 cycles again :) */
    move.l  a0,_tt_InBuffer    ; saves _tt_InBuffer */
__inl_exit:
    rts


	
	 EVEN
	 xdef	_tt_Decompress
_tt_Decompress:
	lea -16(sp),sp
	 movem.l #$1f3e,-(sp)
	move.l 60(sp),a0
	cmp.b #84,2(a0)
	 beq LPack40
LPack3:
	move.w #252,d0
	 bra LPack1
	 EVEN
LPack40:
	cmp.b #80,3(a0)
	 bne LPack3
	clr.l d5
	move.b 6(a0),d5
	clr.w d7
	move.b 9(a0),d7
	clr.w d0
	move.b 10(a0),d0
	move.w d0,a5
	subq.w #1,a5
	clr.w d1
	move.b 12(a0),d1
	move.w d1,42(sp)
	moveq.l #1,d4
	move.w a5,d0
	lsl.w d0,d4
	move.w d4,46(sp)
	moveq.l #2,d1
	lsl.w d0,d1
	moveq.l #8,d4
	sub.w a5,d4
	move.w d4,44(sp)
	moveq.l #8,d6
	sub.w d7,d6
	move.w #249,d0
	cmp.w #8,d7
	 bhi LPack1
	clr.w d0
	move.b 11(a0),d0
	cmp.w 46(sp),d0
	 beq LPack41
LPack6:
	move.w #250,d0
	 bra LPack1
	 EVEN
LPack41:
	moveq.l #4,d0
	cmp.w a5,d0
	 bcc LPack6
	moveq.l #7,d4
	cmp.w a5,d4
	 bcs LPack6
	move.w #251,d0
	cmp.w #4,42(sp)
	 bhi LPack1
	lea 15(a0),a1
	move.l a1,48(sp)
	move.l 64(sp),a4
	clr.l d0
	move.b (a1),d0
	lea 16(a0,d0.l),a0
	move.l a0,_tt_InBuffer
	move.w #128,_tt_InMask
	subq.w #2,d1
	move.w d1,40(sp)
	sub.l a6,a6
	 EVEN
LPack36:
	move.w d5,d0
	tst.w d7
	 bne LPack42
LPack11:
	move.w d0,a0
	cmp.l a0,d5
	 beq LPack43
	move.w d0,d3
	lsl.w d6,d3
	move.w d6,-(sp)
	move.l a4,a2
	addq.l #1,a4
	 jsr _tt_GetBits
	or.b d0,d3
	move.b d3,(a2)
	addq.l #2,sp
	 bra LPack36
	 EVEN
LPack43:
	move.w a5,-(sp)
	lea _tt_GetValue,a2
	 jsr (a2)
	move.w d0,d3
	sub.l a3,a3
	addq.l #2,sp
	cmp.w #1,d0
	 bne LPack44
	move.l _tt_InBuffer,a0
	move.w a6,d1
	move.b (a0),d1
	move.w d1,a6
	move.w _tt_InMask,d1
	move.w a6,d0
	and.w d1,d0
	 beq LPack20

	ror.b  #1,d1
	bcc.s  __inl_tt_cim_skip0
	addq.l #1,_tt_InBuffer
__inl_tt_cim_skip0:

	move.w d1,_tt_InMask
	move.l _tt_InBuffer,a0
	clr.w d0
	move.b (a0),d0
	and.w d1,d0
	 beq LPack45

	ror.b  #1,d1
	bcc.s  __inl_tt_cim_skip2
	addq.l #1,_tt_InBuffer
__inl_tt_cim_skip2:

	move.w d1,_tt_InMask
	move.w a5,-(sp)
	 jsr (a2)
	move.w d0,d4
	addq.l #2,sp
	cmp.w 46(sp),d0
	 bcc LPack46
LPack22:
	move.w a5,-(sp)
	 jsr (a2)
	addq.l #2,sp
	cmp.w #31,d0
	 bhi LPack23
	and.l #$FFFF,d0
	move.l 48(sp),a0
	move.b 0(a0,d0.l),d0
	ext.w d0
LPack24:
	clr.w d1
	 EVEN
LPack29:
	move.b d0,(a4)+
	addq.w #1,d1
	cmp.w d1,d4
	 bcc LPack29
	 bra LPack36
	 EVEN
LPack23:
	move.w d0,d3
	add.w #-32,d3
	lsl.w #3,d3
	move.w #3,-(sp)
	 jsr _tt_GetBits
	or.w d3,d0
	addq.l #2,sp
	 bra LPack24
	 EVEN
LPack46:
	sub.w 46(sp),d4
	move.w 44(sp),d0
	lsl.w d0,d4
	move.w d0,-(sp)
	 jsr _tt_GetBits
	or.w d0,d4
	move.w a5,-(sp)
	 jsr (a2)
	subq.w #1,d0
	lsl.w #8,d0
	or.w d0,d4
	addq.l #4,sp
	 bra LPack22
	 EVEN
LPack45:

	ror.b  #1,d1
	bcc.s  __inl_tt_cim_skip1
	addq.l #1,_tt_InBuffer
__inl_tt_cim_skip1:

	move.w d1,_tt_InMask
	move.w d7,-(sp)
	lea _tt_GetBits,a3
	 jsr (a3)
	move.w d0,d3
	move.w d6,d0
	ext.l d0
	lsl.l d0,d5
	move.w d6,-(sp)
	move.l a4,a2
	addq.l #1,a4
	 jsr (a3)
	or.b d0,d5
	move.b d5,(a2)
	clr.l d5
	move.w d3,d5
	addq.l #4,sp
	 bra LPack36
	 EVEN
LPack20:

	ror.b  #1,d1
	bcc.s  __inl_tt_cim_skip3
	addq.l #1,_tt_InBuffer
__inl_tt_cim_skip3:

	move.w d1,_tt_InMask
	 jsr _tt_Get8Bit
LPack39:
	move.w d0,d2
	eor.w #255,d2
LPack19:
	clr.w d1
	moveq.l #0,d0
LPack34:
	move.w d2,d0
	move.l a4,a0
	sub.l d0,a0
	move.w a3,d4
	add.b -1(a0),d4
	move.b d4,(a4)+
	addq.w #1,d1
	cmp.w d1,d3
	 bcc LPack34
	 bra LPack36
	 EVEN
LPack44:
	move.w a5,-(sp)
	 jsr (a2)
	move.w d0,d4
	subq.w #1,d4
	addq.l #2,sp
	cmp.w 40(sp),d4
	 beq LPack47
	tst.w 42(sp)
	 bne LPack48
LPack18:
	 jsr _tt_Get8Bit
	eor.w #255,d0
	move.w d4,d2
	lsl.w #8,d2
	or.w d0,d2
	 bra LPack19
	 EVEN
LPack48:
	move.w 42(sp),d0
	lsl.w d0,d4
	move.w d0,-(sp)
	 jsr _tt_GetBits
	or.w d0,d4
	addq.l #2,sp
	 bra LPack18
	 EVEN
LPack47:
	cmp.w #3,d3
	 bls LPack9
	lea _tt_Get8Bit,a2
	 jsr (a2)
	move.w d0,a3
	 jsr (a2)
	 bra LPack39
	 EVEN
LPack9:
	clr.w d0
	 bra LPack1
	 EVEN
LPack42:
	move.w d7,-(sp)
	 jsr _tt_GetBits
	addq.l #2,sp
	 bra LPack11
	 EVEN
LPack1:
	 movem.l (sp)+,#$7cf8
	lea 16(sp),sp
	rts
