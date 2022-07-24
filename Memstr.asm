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

; Memory move / copy
; String functions

memcpy:
	move.l	4(a7),a0	; Dest
	move.l	8(a7),a1	; Src
	move.l	12(a7),d0	; Len
	beq.s	memend
memcpy_reg
\loop		move.b	(a1)+,(a0)+
		subq.l	#1,d0	; Should work with long
		bne.s	\loop
memend	move.l	4(a7),a0
	rts

bcopy:
	move.l	4(a7),a1	; Src
	move.l	8(a7),a0	; Dest
	move.w	12(a7),d0	; Len
	beq.s	\end
		subq.w	#1,d0
\loop		move.b	(a1)+,(a0)+
		dbf	d0,\loop
\end	rts
	
memmove:
	move.l	4(a7),a0	; Destw
	move.l	8(a7),a1	; Src
	move.l	12(a7),d0	; Len
	beq.s	memend
memove_reg:
	cmp.l	a0,a1		; Src <= dest
	bls.s	\greater
\loop		move.b	(a1)+,(a0)+
		subq.l	#1,d0
		bne.s	\loop
	bra.s	memend
\greater:	
	add.l	d0,a0
	add.l	d0,a1
\loop2		move.b	-(a1),-(a0)
		subq.l	#1,d0
		bne.s	\loop2
	bra.s	memend
	
_memset
memset:
	move.l	4(a7),a0
	move.w	8(a7),d2
	move.l	10(a7),d0
	beq.s	memend
\loop:		move.b	d2,(a0)+
		subq.l	#1,d0
		bne.s	\loop
	bra.s	memend

memchr:
	move.l	4(a7),a0
	move.w	8(a7),d2
	move.l	10(a7),d0
memchr_reg:
	beq.s	\error
	subq.w	#1,d0
\loop		cmp.b	(a0)+,d2
		dbeq	d0,\loop
	subq.l	#1,a0
	beq.s	\found
\error:	suba.l	a0,a0
\found	rts

memcmp:
	move.l	4(a7),a0		; s1
	move.l	8(a7),a1		; s2
	move.l	12(a7),d0		; Len
	beq.s	memcmp_end
memcmp_reg:
	subq.w	#1,d0
\loop		cmpm.b	(a1)+,(a0)+
		dbne	d0,\loop
	move.b	-(a0),d0
	sub.b	-(a1),d0
	ext.w	d0
memcmp_end:
	rts

strchr:
	move.l	4(a7),a0
	bsr.s	strlen_reg
	move.l	4(a7),a0
	move.w	8(a7),d2
	addq.l	#1,d0
	bra.s	memchr_reg

; Side Effect:
;	Doesn't destroy a1/d2	
strlen:
	move.l	4(a7),a0
strlen_reg
	move.l	a0,d1
\loop		tst.b	(a0)+
		bne.s	\loop
	move.l	a0,d0
	sub.l	d1,d0
	subq.l	#1,d0
	rts

strcat:
	move.l	4(a7),a0	; Dest
	move.l	8(a7),a1	; Src
	move.l	a0,d0
\loop		tst.b	(a0)+
		bne.s	\loop
	subq.l	#1,a0
\loop2		move.b	(a1)+,(a0)+
		bne.s	\loop2
	move.l	d0,a0
	rts

strcpy:
	move.l	4(a7),a0	; Dest
	move.l	8(a7),a1	; Src
strcpy_reg:
	move.l	a0,d0
\loop2		move.b	(a1)+,(a0)+
		bne.s	\loop2
	move.l	d0,a0
	rts

strcmp:
	move.l	4(a7),a0
	move.l	8(a7),a1
strcmp_reg:
	clr.w	d0
	clr.w	d1
	bra.s	\next
\comp:		cmp.b	(a0)+,d1
		beq.s	\next
			move.b	-(a0),d0
			sub.w	d1,d0
			rts
\next		move.b	(a1)+,d1
		bne.s	\comp
	move.b	(a0),d0	
	ext.w	d0
	rts
	
cmpstri:
	move.l	4(a7),a0
	move.l	8(a7),a1
	bra.s	\next
\comp:		cmpi.b	#'A'-1,d0
		bls.s	\NoCvtD0
		cmpi.b	#'Z',d0
		bhi.s	\NoCvtD0
			addi.b	#'a'-'A',d0
\NoCvtD0	cmpi.b	#'A'-1,d1
		bls.s	\NoCvtD1
		cmpi.b	#'Z',d1
		bhi.s	\NoCvtD1
			addi.b	#'a'-'A',d1
\NoCvtD1	cmp.b	d0,d1
		beq.s	\next
			moveq	#1,d0
			rts
\next		move.b	(a0)+,d0
		move.b	(a1)+,d1
		bne.s	\comp
	ext.w	d0
	rts

strncmp:
	move.l	4(a7),a0
	move.l	8(a7),a1
	move.l	12(a7),d1
\loop		move.b	(a0)+,d0
		beq.s	\final
		cmp.b	(a1)+,d0
		dbne	d1,\loop
	subq.l	#1,a1
\final:
	move.b	-(a0),d0
	sub.b	(a1),d0
	ext.w	d0
	rts
	
strncpy:
	move.l	4(a7),a0	; Dest 
	move.l	8(a7),a1	; Src
	move.l	12(a7),d1	; Len
\loop		move.b	(a1)+,(a0)+
		dbeq	d1,\loop
	clr.b	(a1)
	move.l	4(a7),a0	; Dest
	rts

strncat:
	move.l	4(a7),a0	; Dest
	move.l	8(a7),a1	; Src
	move.l	12(a7),d1
\loop		tst.b	(a0)+
		bne.s	\loop
	subq.l	#1,a0
\loop2		move.b	(a1)+,(a0)+
		dbeq	d1,\loop2
	clr.b	(a0)
	move.l	4(a7),a0
	rts

strcspn:
	pea	(a2)
	move.l	d3,-(sp)
	move.l	12(sp),d2
	move.l	16(sp),d3
	move.l	d2,a1
\L2:
	move.b	(a1),d1
	move.l	a1,a2
	sub.l	d2,a2
	beq.s	\L1
		move.l	d3,a0
		move.b	(a0)+,d0
		beq.s	\L13
\L11:
		move.l	a1,a2
		sub.l	d2,a2
		cmp.b	d0,d1
		beq.s	\L1
		move.b	(a0)+,d0
		bne.s	\L11
\L13:	addq.l	#1,a1
	bra.s	\L2
\L1:	move.l a2,d0
	move.l (sp)+,d3
	move.l (sp)+,a2
	rts

strpbrk:
	move.l	d3,-(sp)
	move.l	12(sp),d3
	move.l	8(sp),a0
\Loop:
		move.b	(a0),d1
		moveq	#0,d2
		tst.b	d1
		beq.s	\Found
		move.l	d3,a1
		move.b	(a1)+,d0
		beq.s	\L26
\L24:			move.l	a0,d2
			cmp.b	d0,d1
			beq.s	\Found
			move.b	(a1)+,d0
			bne.s	\L24
\L26:		addq.l	#1,a0
		bra.s	\Loop
\Found:	move.l	d2,a0
	move.l	(sp)+,d3
	rts

strrchr:
	move.l	4(sp),d1
	move.l	d1,a0
	move.b	9(sp),d0
\TheEnd:	tst.b	(a0)+
		bne.s	\TheEnd
\Loop:		cmp.b	-(a0),d0
		beq.s	\Found
		cmp.l	a0,d1
		bne.s	\Loop
	moveq	#0,d0
	bra.s	\End
\Found:	move.l a0,d0
\End:	move.l d0,a0
	rts

strspn:
	pea	(a2)
	move.l	d3,-(sp)
	move.l	12(sp),d2
	move.l	16(sp),d3
	move.l	d2,a1
\Loop:
		move.b	(a1),d1
		move.l	a1,a2
		sub.l	d2,a2
		beq.s	\Found
		move.l	d3,a0
\TinyLoop:		move.b	(a0)+,d0
			move.l	a1,a2
			sub.l	d2,a2
			beq.s	\Found
			cmp.b	d0,d1
			bne.s	\TinyLoop
		addq.l	#1,a1
		bra.s	\Loop
\Found	move.l a2,d0
	move.l (sp)+,d3
	move.l (sp)+,a2
	rts

strstr:
	pea	(a2)
	move.l	8(sp),a1
	move.l	12(sp),a2
\Loop1:
		moveq	#0,d2
\Loop2:
			move.b	0(a2,d2.l),d0
			ext.w	d0
			move.b	0(a1,d2.l),d1
			ext.w	d1
			tst.w	d0
			beq.s	\EndOfLoop2
			addq.w	#1,d2
			cmp.w	d0,d1
			beq.s	\Loop2
\EndOfLoop2:
		move.l	a1,d1
		tst.w	d0
		beq.s	\Exit
		tst.b	(a1)+
		bne.s	\Loop1
	moveq.l	#0,d1
\Exit:	move.l	d1,a0
	move.l (sp)+,a2
	rts

strerror:
	move.w	4(a7),d0
	lea	StrError_msg_str(pc),a0
	cmpi.w	#21,d0
	bhi.s	\end
\loop			tst.b	(a0)+
			bne.s	\loop
		dbf	d0,\loop
\end:	rts

;char *strtok (char *s1, const char *s2); 
strtok:
	move.l	4(a7),d0	; s1
	beq.s	\NotNull
		move.l	d0,STRTOK_PTR	; Save Ptr
\NotNull:
	tst.l	STRTOK_PTR
	beq.s	\Error
	
	move.l	STRTOK_PTR,a1
\Loop		move.b	(a1)+,d1		; Next char
		beq.s	\EndOfString		; End of string
		move.l	8(a7),a0		; s2: Token Chars
\CharLoop		move.b	(a0)+,d0	; Read next Token Char
			beq.s	\Loop		; End of Token string ? Next char in string
			cmp.b	d1,d0		; Cmp 2 chars
			beq.s	\TokenCharFound	; Yes, found
			bra.s	\CharLoop	; No next Token
\EndOfString
	move.l	STRTOK_PTR,a0
	clr.l	STRTOK_PTR
	rts
\TokenCharFound
	move.l	STRTOK_PTR,a0
	clr.b	-1(a1)		; Clear Token
	move.l	a1,STRTOK_PTR
	rts	
\Error	suba.l	a0,a0
	rts
		
