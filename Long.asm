;*
;* PedroM - Operating System for Ti-89/Ti-92+/V200.
;* Copyright (C) 2003 PpHd
;*
;* This program is free software ; you can redistribute it and/or modify it under the
;* terms of the GNU General Public License as published by the Free Software Foundation;
;* either version 2 of the License, or (at your option) any later version. 
;* 
;* This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
;* without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;* See the GNU General Public License for more details. 
;* 
;* You should have received a copy of the GNU General Public License along with this program;
;* if not, write to the 
;* Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA 

; ***************************************************************
; *								*
; *		Pedrom		/	LongCalc		*
; *								*
; ***************************************************************

; ***************************************************************
; 			Divu/modu with long
;
;	If you understand something,
;			 you are a VERY GOOD asm programmer !
;
; ***************************************************************

_ms16u16
	ext.l	d1
	bmi.s	\inf
		divu	d0,d1
		swap	d1
		ext.l	d1
		rts
\inf:	neg.l	d1
	divu	d0,d1
	swap	d1
	neg.w	d1
	ext.l	d1
	rts
	
_ds16u16:
	ext.l	d1
	bmi.s	\inf
		divu	d0,d1
		ext.l	d1
		rts
\inf:	neg.l	d1
	divu	d0,d1
	neg.w	d1
	ext.l	d1
	rts
	
_du16u16:
	swap	d1
	clr.w	d1
	swap	d1
	divu	d0,d1
	swap	d1
	clr.w	d1
	swap	d1
	rts
	
_mu16u16:
	swap	d1
	clr.w	d1
	swap	d1
	divu	d0,d1
	clr.w	d1
	swap	d1
	rts
	
_du32u32:
	lea	_du32u32_Ret(Pc),a0
	bra.s	_ds32s32_cont2

_du32u32_Ret:
	cmpa.w	#0,a1
	beq.s	\end
\loop		subq.w	#1,d1
		sub.l	d0,d2
		bhi.s	\loop
\end	rts	

_ds32s32_inf:
	neg.l	d1
	tst.l	d0
	bpl.s	_ds32s32_to
		neg.l	d0
		bra.s	_ds32s32_cont
_ds32s32_inf2:
	neg.l	d0
_ds32s32_to
	lea	\Return(Pc),a0
	bra.s	_ds32s32_cont2		
\Return:
	neg.l	d1
	rts
_ds32s32:
	tst.l	d1
	bmi.s	_ds32s32_inf
	tst.l	d0
	bmi.s	_ds32s32_inf2

_ds32s32_cont
	move.l	(a7)+,a0
_ds32s32_cont2
	swap	d0
	movea.w	d0,a1
	swap	d0
	move.w	a1,d2
	bne.s	_ds32s32_High
		; Hight part doesn't exist
		divu	d0,d1
		bvs.s	\overflow	; Overflow ?
			swap	d1
			clr.w	d1
			swap	d1
			jmp	(a0)	; Return
\overflow	move.w	d1,-(a7)	
		clr.w	d1
		swap	d1
		divu	d0,d1
		move.l	d1,d2
		move.w	(a7)+,d2
		divu	d0,d2
		swap	d1
		move.w	d2,d1
		jmp	(a0)
_ds32s32_High
	move.l	d1,-(a7)
	andi.w	#$FF00,d2
	bne.s	\very_high
	move.w	a1,d2
	andi.w	#$00F0,d2
	bne.s	\quite_high
		move.l	d0,d2
		lsr.l	#4,d2
		lsr.l	#4,d1
\calc		divu	d2,d1
		move.w	d1,d2
		exg	d7,a1
		mulu.w	d2,d7
		mulu.w	d0,d2
		swap	d2
		add.w	d7,d2
		swap	d2
		swap	d7
		addx.w	d7,d7
		ext.l	d7
		exg	d7,a1
		sub.l	(a7)+,d2
		bls.s	\toit
\trip			subq.w	#1,d1
			sub.l	d0,d2
			bhi.s	\trip
\toit		swap	d1
		clr.w	d1
		swap	d1
		jmp	(a0)
\very_high
	andi.w	#$F000,d2
	bne.s	\hyper_high
		move.l	a1,d2
		rol.l	#4,d2
		lsr.l	#8,d1
		lsr.l	#4,d1
		bra.s	\calc
\hyper_high	move.w	a1,d2
		clr.w	d1
		swap	d1
		bra.s	\calc
\quite_high	move.l	d0,d2
		lsr.l	#8,d2
		lsr.l	#8,d1
		bra.s	\calc

_mu32u32:
	pea	_ms32s32_Return(pc)
	lea	_du32u32_Ret(Pc),a0
	bra.s	_ms32s32_ent
	
_ms32s32_Return:
	move.l	d2,d1
	neg.l	d1
	rts
_ms32s32:
	tst.l	d0
	bpl.s	\ok1
		neg.l	d0
\ok1	tst.l	d1
	bmi.s	_ms32s32_inf
	
	lea	_ms32s32_Return(Pc),a0
_ms32s32_ent
	swap	d0
	movea.w	d0,a1
	swap	d0
	move.w	a1,d2
	bne	_ds32s32_High
	divu.w	d0,d1
	bvc.s	\overflow
		move.w	d1,-(a7)
		clr.w	d1
		swap	d1
		divu	d0,d1
		move.w	(a7)+,d1
		divu	d0,d1
\overflow:
	clr.w	d1
	swap	d1
	neg.l	d1
	move.l	d1,d2
	jmp	(a0)

_ms32s32_inf:
	neg.l	d1
	lea	\TheReturn(Pc),a0
	bra.s	_ms32s32_ent
\TheReturn:
	move.l	d2,d1
	rts
	
__umodsi3:
	move.l 4(sp),d1
	move.l 8(sp),d0
	bsr	_mu32u32
	move.l	d1,d0
	rts
	
__udivsi3:
	move.l 4(sp),d1
	move.l 8(sp),d0
	bsr	_du32u32
	move.l	d1,d0
	rts
