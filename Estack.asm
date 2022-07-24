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
; 			Min EStack function
;		Contrary to Tios, EStack is FIXED !
; ***************************************************************

; ATTENTION: Il semble que les variables reelles soient stockes comme cela, en ecrasant le dernier byte du reel :
;	$4000 $22003300 $xxxxxx23
; De meme sur l'EStack.

EStackInit:
	pea	(ESTACK_SIZE).w	
	bsr	HeapAllocHigh
	addq.l	#4,a7
	lea	InitError_str,a0
	cmpi.w	#ESTACK_HANDLE,d0
	beq.s	EStackReInit
		jmp	SYSTEM_ERROR

EStackReInit:
	move.w	#ESTACK_HANDLE,-(a7)
	bsr	HLock
	addq.l	#2,a7
	move.l	a0,start_estack
	move.l	a0,top_estack
	move.b	#$E9,(a0)
	adda.w	#ESTACK_SIZE-1,a0
	move.l	a0,end_estack
	rts
	
;void reset_estack_size (unsigned short NewSize);
reset_estack_size:		; Does nothing
	rts
	
;void reset_control_flags (void); 
reset_control_flags:
	rts		; Which flag can I reset ?
	
;void check_estack_size (unsigned short Size);
check_estack_size:
	move.l	top_estack,a0
	moveq	#0,d0
	move.w	4(a7),d0
	adda.l	d0,a0
	cmp.l	end_estack,a0
	bls.s	\Ok
		dc.w	$A000+600	; Memory
\Ok	rts	

push_END_TAG:
	move.w	#$E5,d0
	bra.s	push_quantum_reg

push_LIST_TAG:
	move.w	#$D9,d0
	bra.s	push_quantum_reg

;void push_quantum (ESQ Tag);
; Side effect: does not destroy d1-d2/a1
push_quantum:
	move.w	4(a7),d0
push_quantum_reg	
	move.l	top_estack,a0
	addq.l	#1,a0
	cmp.l	end_estack,a0
	bls.s	\Ok
		dc.w	$A000+600	; Memory
\Ok	move.b	d0,(a0)
	move.l	a0,top_estack
	rts

;void push_quantum_pair (ESQ Tag1, ESQ Tag2);
push_quantum_pair:
	move.l	top_estack,a0
	addq.l	#2,a0
	cmp.l	end_estack,a0
	bls.s	\Ok
		dc.w	$A000+600	; Memory
\Ok:	move.b	5(a7),-1(a0)
	move.b	7(a7),(a0)
	move.l	a0,top_estack
	rts
	
;void push_between (void *ptr1, void *ptr2); 
push_between:
	tst.b	BREAK_KEY
	beq.s	\NoBrk
		dc.w	$A0B4
\NoBrk	move.l	4(a7),a0
	move.l	8(a7),a1
	move.l	a1,d0
	sub.l	a0,d0
	move.l	top_estack,a1
	add.l	d0,a1
	cmp.l	end_estack,a1
	bls.s	\Ok
		dc.w	$A000+600
\Ok	addq.l	#1,a1
	addq.l	#1,a0
	subq.l	#1,d0
\loop		move.b	(a0)+,(a1)+
		dbf	d0,\loop
	subq.l	#1,a1
	move.l	a1,top_estack
	rts
	
;ESQ min_quantum (ESQ Tag1, ESQ Tag2);
min_quantum:
	clr.w	d0	
	move.b	7(a7),d0
	cmp.b	5(a7),d0
	bcs.s	\Done
		move.b	5(a7),d0
\Done	rts

;void push_expr_quantum (CESI ptr, ESQ Tag);
push_expr_quantum:
	move.l	4(a7),a1
	bsr.s	push_expr_quantun_sub
	addq.l	#1,top_estack
	move.l	top_estack,a0
	move.b	9(a7),(a0)
	rts

push_expr_quantun_sub:
	pea	(a1)
	pea	(a1)
	bsr	next_expression_index
	move.l	a0,(a7)
	bsr	push_between
	addq.l	#8,a7
	rts

;void push_expr2_quantum (CESI ptr1, CESI ptr2, ESQ Tag);	
push_expr2_quantum:
	move.l	4(a7),a1
	bsr.s	push_expr_quantun_sub
	move.l	8(a7),a1
	bsr.s	push_expr_quantun_sub
	addq.l	#1,top_estack
	move.l	top_estack,a0
	move.b	$D(a7),(a0)
	rts

;void delete_between (ESI ptr1, ESI ptr2);
;void deleted_between (ESI ptr1, ESI ptr2);
deleted_between:
delete_between:
	move.l	4(a7),a0
	move.l	8(a7),a1
	move.l	a1,d0
	sub.l	a0,d0
	move.l	top_estack,d1
	sub.l	d0,top_estack
	sub.l	a1,d1
	move.l	d1,-(a7)	; Number of bytes to move
	pea	1(a1)
	pea	1(a0)
	bsr	memmove
	addq.l	#8,a7
	move.l	(a7)+,d0	; Return the number of bytes
	rts
	
	
;void delete_expression (ESI ptr);	
deleted_expression:
delete_expression:
	move.l	4(a7),a0
	pea	(a0)
	pea	(a0)
	bsr.s	next_expression_index
	move.l	a0,(a7)
	bsr.s	delete_between
	addq.l	#8,a7
	rts
	
;unsigned short remaining_element_count (CESI start_ptr);
remaining_element_count:
	move.l	4(a7),a0
	clr.w	-(a7)
\loop		cmpi.b	#$E5,(a0)	; End Tag ?
		beq.s	\end
		addq.w	#1,(a7)
		bsr.s	next_expression_index_reg
		bra.s	\loop
\end	move.w	(a7)+,d0
	rts			

;ESI next_expression_index (ESI ptr);
next_expression_index:
	move.l	4(a7),a0
next_expression_index_reg
	move.b	(a0),d0
	beq.s	\DoStr			;0 -> VarName (Like Str)
	subq.l	#1,a0
	cmpi.b	#$2D,d0
	bhi	\NextExpr
	bne.s	\NoStr			;$2D -> Str
\DoStr		tst.b	-(a0)		; If VarName we skip the first letter (No matter)
		bne.s	\DoStr		; If str we check the first letter (It was just for info)
		subq.l	#1,a0
		rts
\NoStr	cmpi.b	#$23,d0	; Float
	bne.s	\NoBcd
		lea	-10(a0),a0	; $23 -> 10 bytes HERE
		rts
\NoBcd	cmpi.b	#$1C,d0
	beq.s	\DoOneByte
	cmpi.b	#$1E,d0
	bne.s	\NoOneByte
\DoOneByte	subq.l	#1,a0		; $1C -> $1E -> 1 (One byte after Tag)
		rts
\NoOneByte:
	cmpi.b	#$1F,d0
	beq.s	\Integer
	cmpi.b	#$20,d0
	bne.s	\NoInteger	; $1F/$20 -> Integers.
\Integer:	clr.w	d0
		move.b	(a0),d0			; Len
		addq.w	#1,d0
		suba.w	d0,a0
\NoInteger:
	rts	; 1 -> $1B, $24 -> $2C & $2E : 0 bytes

\NextExpr
	;$2F -> $7D One expr
	cmpi.b	#$7D,d0
	bls.s	next_expression_index_reg
	cmpi.b	#$7E,d0
	beq.s	\NoInteger	; -> ???? Start Tag ?
	cmpi.b	#$B3,d0
	bls.s	\t2Exprs		;	$7F -> $B3 -> 2 exprs
	cmpi.b	#$B6,d0			;	$B4 -> $B6 -> 3 exprs
	bls.s	\t3Exprs
	cmpi.b	#$BB,d0
	bhi.s	\NoExpr			;	$B7 -> $BB -> 4 exprs
\t4Exprs	bsr.s	next_expression_index_reg
\t3Exprs	bsr.s	next_expression_index_reg	
\t2Exprs	bsr.s	next_expression_index_reg
		bra.s	next_expression_index_reg
\NoExpr
	; $BC -> $D6 + $D9 & $DA Find END_TAG
	; FIXME: Sure ? Or some tags use NOTHING_TAG to fill unused tags ?
	cmpi.b	#$DA,d0
	bhi.s	\ExtToken
		cmpi.b	#$D7,d0		; Min 
		beq.s	\t2Exprs
		cmpi.b	#$D8,d0		; Max
		beq.s	\t2Exprs
		bra.s	\comp
\loop			bsr	next_expression_index_reg
\comp			cmpi.b	#$E5,(a0)
			bne.s	\loop
			subq.l	#1,a0
			rts		
\ExtToken
	; Extra Basic commands ? $E3 & $E4 ?
	; $DB -> $F9 -> ???
	dc.w	$A000+84	; An error
	
;short EX_getBCD (short n, float *dest); HERE
EX_getBCD:
	move.w	4(a7),-(a7)	; N
	bsr.s	EX_getArg
	addq.l	#2,a7
	move.l	a0,d0
	beq.s	\Error
		moveq	#0,d0
		cmpi.b	#$23,(a0)
		bne.s	\Error
			move.l	6(a7),a1
			lea	-10(a0),a0
			moveq	#9,d0
\loop				move.b	(a0)+,(a1)+
				dbf	d0,\loop
\Error	rts
	
;ESI EX_getArg (short n);
EX_getArg:
	move.w	d3,-(a7)
	move.w	6(a7),d3
	move.l	top_estack,a0
	bra.s	\comp
\loop		pea	(a0)
		bsr	next_expression_index
		addq.l	#4,a7
		cmpi.b	#$E5,(a0)
		beq.s	\End
\comp		dbf	d3,\loop			
\End2	move.w	(a7)+,d3
	rts
\End	suba.l	a0,a0
	bra.s	\End2	

;void push_Float (float value); HERE
push_Float:
	lea	4(a7),a1
push_Float_reg:
	move.l	top_estack,a0
	lea	11(a0),a0		; 10 for BCD + 1 for TAG
	cmp.l	end_estack,a0
	bls.s	\Ok
		dc.w	$A000+600	; Memory
\Ok:	move.l	top_estack,a0
	addq.l	#1,a0			; Skip LAST tag
	moveq	#10-1,d0
\loop		move.b	(a1)+,(a0)+
		dbf	d0,\loop
	move.b	#$23,(a0)		; BCD tag
	move.l	a0,top_estack
	rts

;float estack_number_to_Float (CESI ptr); HERE
estack_number_to_Float:
	move.l	4(a7),a0
	cmpi.b	#$23,(a0)
	beq.s	\Ok
		dc.w	$A3FC
\Ok	lea	-10(a0),a0
	lea	-10(a6),a1
	moveq	#10-1,d0
\loop		move.b	(a0)+,(a1)+
		dbf	d0,\loop
	rts
	
	
;void move_between_to_top (ESI ptr1, ESI ptr2); 
move_between_to_top:
moved_between_to_top:
	move.l	4(a7),a0
	move.l	8(a7),a1
	pea	(a1)
	pea	(a0)
	bsr	push_between
	bsr	delete_between
	addq.l	#8,a7
	rts
	
;HANDLE HS_popEStack (void);
HS_popEStack:
	move.l	top_estack,d0
	sub.l	start_estack,d0
	addq.l	#3,d0
	move.l	d0,-(a7)
	bsr	HeapAllocHigh
	addq.l	#4,a7
	move.w	d0,-(a7)
	bne.s	\Ok
		move.l	start_estack,top_estack
		dc.w	$A29E
\Ok	move.w	d0,a0
	trap	#3
	move.l	top_estack,d0
	sub.l	start_estack,d0
	addq.l	#1,d0
	move.w	d0,(a0)+	; Dest
	move.l	start_estack,a1	; Src
	bsr	memcpy_reg
	bsr	HeapUnlock
	move.l	start_estack,top_estack
	move.w	(a7)+,d0
	rts
	
;ESI TokenizeSymName (const char *src, unsigned short Flags);
TokenizeSymName:
	; Check if folder and get len
	move.l	4(a7),a0
	clr.b	d2				; No folder
	moveq	#2-1,d0				; Len = 2 (we count len+1 char)
\loop1		move.b	(a0)+,d1		; Read char
		addq.w	#1,d0			; One more char
		cmpi.b	#'\',d1
		bne.s	\NoFold
			st.b	d2		; Folder
\NoFold		tst.b	d1
		bne.s	\loop1
	; Check if copy folder first
	move.w	d0,-(a7)			; Push Len
	tst.b	d2				; Check if folder
	bne.s	\Fold1	
		move.w	2+8(a7),d1		; Check if we add Current Folder 
		andi.w	#1,d1			; 
		sne.b	d2			; Set d2.b
		beq.s	\Fold1
			lea	CUR_FOLDER_STR,a0
			bsr	strlen_reg
			addq.w	#1,d0		; for '\'
			add.w	d0,(a7)		; Add this to the len
\Fold1:	
	; Check if enought place on EStack
	bsr	check_estack_size
	addq.w	#2,a7
	; Push it on the EStack
	move.l	top_estack,a0
	move.l	a0,d1
	addq.l	#1,a0		; Skip Current TAG
	clr.b	(a0)+
	tst.b	d2
	beq.s	\NoCopyFolder
		lea	CUR_FOLDER_STR,a1
\loop2			move.b	(a1)+,(a0)+
			bne.s	\loop2
		move.b	#'\',-1(a0)
\NoCopyFolder
	move.l	4(a7),a1
\loop3			move.b	(a1)+,(a0)+
			bne.s	\loop3
	subq.l	#1,a0
	move.l	a0,top_estack
	move.l	d1,a0
	rts
	
;short estack_to_short (CESI ptr, short *value_ptr);
estack_to_short:
	move.l	4(a7),a0
	bsr.s	estack_to_int
	tst.b	d1
	beq.s	\Ok
		moveq	#-1,d0
		rts
\Ok:	move.l	8(a7),a0
	cmp.l	#32767,d0
	ble.s	\ok1
		move.w	#32767,(a0)
		moveq	#0,d0
		rts
\ok1	cmp.l	#-32768,d0
	bge.s	\ok2
		move.w	#-32768,(a0)
		moveq	#0,d0
		rts	
\ok2	move.w	d0,(a0)
	moveq	#1,d0
	rts
	
; In :
;	a0 -> EStack
estack_to_int
	move.b	(a0),d2
	cmpi.b	#$1F,d2
	beq.s	\ok
	cmpi.b	#$20,d2
	bne.s	\Fail
\ok:		moveq	#0,d0
		move.b	-(a0),d1
		beq.s	\end	
		cmpi.b	#4,d1
		bhi.s	\Fail	
\loop			lsl.l	#8,d0
			move.b	-(a0),d0
			subq.b	#1,d1
			bne.s	\loop
\end		cmpi.b	#$20,d2
		bne.s	\NoNeg
			neg.l	d0
\NoNeg:		rts
\Fail:	st.b	d1
	rts
		
;short estack_to_ushort (CESI ptr, unsigned short *value_ptr); 
estack_to_ushort:
	move.l	4(a7),a0
	bsr.s	estack_to_int
	tst.b	d1
	beq.s	\Ok
		moveq	#-1,d0
		rts
\Ok:	move.l	8(a7),a0
	cmp.l	#65535,d0
	ble.s	\ok1
		move.w	#65535,(a0)
		moveq	#0,d0
		rts
\ok1	tst.l	d0
	bge.s	\ok2
		clr.w	(a0)
		moveq	#0,d0
		rts	
\ok2	move.w	d0,(a0)
	moveq	#1,d0
	rts

;long GetValue (CESI ptr, long low, long high);
GetValue:
	move.l	4(a7),a0
	bsr	estack_to_int
	tst.b	d1
	beq.s	\Ok
		dc.w	$A610
\Ok:	cmp.l	8(a7),d0
	bge.s	\ok1
		dc.w	$A610
\ok1	cmp.l	12(a7),d0
	ble.s	\ok2
		dc.w	$A610
\ok2	rts
		


;short assign_between (ESI var, ESI low, ESI high); 
assign_between:
	move.l	4(a7),a1	; VAR name
	move.l	8(a7),d0	; Low
	move.l	12(a7),a0	; High
	pea	(a0)		; High
	suba.l	d0,a0
	move.w	a0,-(a7)
	move.w	#$4000,-(a7)
	pea	(a1)
	bsr	VarStore
	moveq	#1,d0
	rts

;void push_string (CESI expr);
push_string:
	link	a6,#$FFF0
	movem.l	d3-d4/a2,-$C(a6)
	clr.l	-(a7)
	clr.l	-(a7)
	move.l	8(a6),-(a7)
	bsr	Parse1DExpr
	move.w	d0,d4
	bne.s	\Ok1
		dc.w	$A29E
\Ok1	clr.w	(a7)
	bsr	push_quantum
	move.w	d4,a0
	trap	#3
	move.l	a0,a2
	move.l	a2,(a7)
	bsr	strlen
	moveq	#0,d3
	move.w	d0,d3
	move.w	d3,(a7)
	bsr	check_estack_size
	move.l	d3,(a7)
	pea	(a2)
	move.l	top_estack,a1
	pea	1(a1)
	bsr	memmove
	move.w	d2,(a7)
	bsr	HeapFree
	add.l	d3,top_estack
	move.w	#$2D,(a7)
	clr.w	-(a7)
	bsr	push_quantum_pair
	movem.l	-$c(a6),d3-d4/a2
	unlk	a6
	rts

;void push_zstr (const char *str); 
push_zstr:
	pea	(a2)
	move.l	8(a7),a2
	clr.w	-(a7)
	bsr	push_quantum
	clr.w	d0
\loop		move.b	(a2)+,d0
		beq.s	\done
		move.w	d0,(a7)
		bsr	push_quantum
		bra.s	\loop
\done	move.w	#$2D,(a7)
	clr.w	-(a7)
	bsr	push_quantum_pair
	addq.l	#4,a7
	move.l	(a7)+,a2
	rts

