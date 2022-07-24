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
; 			Min EStack function (2)
;		Contrary to Tios, EStack is FIXED !
; ***************************************************************

;short NG_tokenize (HANDLE hTEXT, unsigned short *ErrCode, unsigned short *ErrOffset);
NG_tokenize:
	movem.l	d3-d4/a2-a4,-(a7)
	lea	-$4C(a7),a7
	move.w	$64(a7),d3
	move.l	$66(a7),a2
	move.l	$6A(a7),a3
	move.w	d3,(a7)
	jsr	HLock
	move.l	a0,a4
	lea	4(a7),a0
	move.l	a0,(a7)
	bsr	ER_catch
	move.w	d0,d4
	bne.s	\Error
		move.l	a4,(a7)
		bsr.s	push_parse_text
		move.w	d0,(a2)
		moveq	#1,d4
		bsr	ER_success
		bra.s	\done
\Error	move.w	d4,(a2)
	move.l	error_estack,d0
	sub.w	a4,d0
	move.w	d0,(a3)
	moveq	#0,d4
\done
	move.w	d3,(a7)
	jsr	HeapUnlock
	move.w	d4,d0
	lea	$4c(a7),a7
	movem.l	(a7)+,d3-d4/a2-a4
	rts
	
;short push_parse_text (const char *str);
push_parse_text:
	movem.l	d3-d7/a2-a6,-(a7)
	move.l	44(a7),a2			; String
	move.l	top_estack,-(a7)		; Push top_estack
	bsr.s	ParseRecur			; Return in a2 the last character which can not be translated
	move.l	(a7)+,a0			; Pop top_estack
	tst.b	(a2)				; Check sucessfull
	beq.s	\Success			; If string current != 0, an error occured.
		move.l	a0,top_estack		; Fix top_estack
		dc.w	$A010			; Throw an error.
\Success
	moveq	#0,d0
	movem.l	(a7)+,d3-d7/a2-a6
	rts

; Recursive Parse a string
; VALIDATE:
;	Input Float
;	Operator priority
;	Parenthese
;	Var
;	Function
; In:
;	a2 -> String
ParseRecur:
	; Alloc Op Stack = Stack
	moveq	#1,d3				; Last character was an operator 
	clr.w	-(a7)				; Stop the OP stack
ParseLoop
		move.b	(a2),d2
		bsr	ParseIsNumber
		tst.b	d0
		bne	ParseInputFloat
		bsr	ParseIsPureVar
		tst.b	d0
		bne	ParseInputVar
		addq.l	#1,a2			; One char
		cmpi.b	#' ',d2			; ' ' Skip Space
		beq.s	ParseLoop
		cmpi.b	#',',d2			; ','
		beq.s	ParseLoop
		cmpi.b	#'(',d2
		beq	ParseInputParent
		cmpi.b	#'{',d2
		beq	ParseInputList
		cmpi.b	#'"',d2
		beq	ParseInputString
		;cmpi.b	#'[',d2
		;beq	\InputMat
		cmpi.b	#22,d2			; '->'
		beq.s	\PushStore
		cmpi.b	#'*',d2
		beq.s	\PushMult
		cmpi.b	#'+',d2
		beq.s	\PushAdd
		cmpi.b	#'-',d2
		beq.s	\PushMinus
		cmpi.b	#'/',d2
		beq.s	\PushDiv
		cmpi.b	#'^',d2
		beq.s	\PushPow
		cmpi.b	#'<',d2
		beq.s	\PushInf
		cmpi.b	#'=',d2
		beq.s	\PushEqual
		cmpi.b	#'>',d2
		beq.s	\PushSup
		cmpi.b	#156,d2
		beq.s	\PushInfEqual
		cmpi.b	#157,d2
		beq.s	\PushNotEqual
		cmpi.b	#158,d2
		beq.s	\PushSupEqual
		subq.l	#1,a2
		bra	ParseFinal


; Push an operator (on the operator Stack or on the EStack)
\PushStore	move.w	#$80,d2			; Store TAG
		bra.s	\PushCheck
\PushMult	move.w	#$8F,d2
		bra.s	\PushCheck
\PushAdd	move.w	#$8B,d2
		bra.s	\PushCheck
\PushMinus	move.w	#$8D,d2
		bra.s	\PushCheck
\PushDiv	move.w	#$91,d2
		bra.s	\PushCheck
\PushPow	move.w	#$93,d2
		bra.s	\PushCheck
\PushInf	move.w	#$85,d2
		bra.s	\PushCheck
\PushEqual	move.w	#$87,d2
		bra.s	\PushCheck
\PushSup	move.w	#$89,d2
		bra.s	\PushCheck
\PushInfEqual	move.w	#$86,d2
		bra.s	\PushCheck
\PushNotEqual	move.w	#$8A,d2
		bra.s	\PushCheck
\PushSupEqual	move.w	#$88,d2
\PushCheck	tst.b	d3
		bgt.s	\Ans
		bne.s	ParseFinal	; End of parsing
\ContOp		st.b	d3		; Last char is an Operator
\LoopOp		move.w	(a7),d0
		beq.s	\PushIt
			; Priority is (TAG+1)/4
			move.w	d2,d1
			addq.w	#1,d1
			lsr.w	#2,d1		; Priority of New op
			addq.w	#1,d0
			lsr.w	#2,d0		; Priority of Pushed Op
			cmp.w	d1,d0		; If New operator has an higher priority
			bcs.s	\PushIt		; Push op in Operator Stack
			bsr	push_quantum	; Else push Operator on EStack
			addq.l	#2,a7
			bra.s	\LoopOp
\PushIt		move.w	d2,-(a7)	; Push Operator
		bra	ParseLoop
\Ans:		; Push Last Calcul To ESTACk
		move.w	d2,-(a7)			; Temp Save of d2.w
		lea	FloatReg1,a0			; Last ANS should be in FReg1
		lea	FloatReg2+FLOAT.exponent,a1	
		bsr	FloatInternal2AMS
		lea	FloatReg2+FLOAT.exponent,a1
		bsr	push_Float_reg			; Push it
		move.w	(a7)+,d2			; Reload current op
		bra.s	\ContOp

; End of parsing
; Push all remaining Operator on the stack
; Check if d3=-1 ???? 2+
ParseFinal:
		move.w	(a7)+,d0
		beq.s	\End
		bsr	push_quantum_reg
		bra.s	ParseFinal
\End	rts



; Parse the string to push a string : search for '"'
ParseInputString				; Clearly not optimize since we call push_quantum, again & again !
		clr.b	d3			; Last Thing was a STRING
		clr.w	d0			; 0
		bsr	push_quantum_reg	; d2 = '"'
		bra.s	\Start
\Loop			bsr	push_quantum_reg
\Start			move.b	(a2)+,d0
			beq	ParseFinal	; End of string : ERROR " not found
			cmp.b	d2,d0		; Find '"'
			bne.s	\Loop
		clr.w	d0			; 0
		bsr	push_quantum_reg
		move.w	#$2D,d0			; STR_TAG
		bsr	push_quantum_reg
		bra	ParseLoop


; Parse recursevely to find a ')'					
ParseInputParent
		clr.b	d3			; Last Thing was a ( ) bloc
		pea	-1(a2)			; Push Beginning
		bsr	ParseRecur		; Parse inside and stop 
		move.l	(a7)+,a0		; Read beginning
		move.b	(a2)+,d2		; Read stopped char
		cmpi.b	#')',d2			; If it is ')', it is ok ! continue
		beq	ParseLoop		; Else an error occured.
		move.l	a0,a2			; Return
		bra	ParseFinal		; the beginning of the expression
		

; Input a List
ParseInputList
		clr.b	d3			; Last Thing was a LIST
		bsr	push_END_TAG		; END of LIST
		pea	-1(a2)			; Push Beginning
		bsr	ParseRecur		; Parse inside and stop 
		bsr	push_LIST_TAG		; Push LIST tag
		move.l	(a7)+,a0		; Read beginning
		move.b	(a2)+,d2		; Read stopped char
		cmpi.b	#'}',d2			; If it is '}', it is ok ! continue
		beq	ParseLoop		; Else an error occured.
		move.l	a0,a2			; Return
		bra	ParseFinal		; the beginning of the list


; Input a VAR name or a function
ParseInputVar
		clr.b	d3			; Last Thing was a VAR
		moveq	#-1,d1			; If var len is 1, it will do 2 loops
		move.l	a2,a3			; Beginning of the var
\Loop0			move.b	(a2)+,d2	; Read char
			addq.w	#1,d1		; One more char
			bsr	ParseIsVar	; Check if it is a var name ?
			bne.s	\Loop0		; One more char
		cmpi.w	#17,d1			; Var Name is too long !
		bhi.s	\VarError		; d1 =
		subq.l	#1,a2			; Rego on the untranslated char
		cmpi.b	#'(',d2			; Check if it is a Function
		beq.s	\Function		; or a variable name
			; Var Name
			clr.w	d0		; VAR a-z are not pushed in a single tag way
			bsr	push_quantum_reg
			subq.w	#1,d1
\Loop1				move.b	(a3)+,d0
				bsr	push_quantum_reg
				dbf	d1,\Loop1
			clr.w	d0		; 0 = VAR_TAG
			bsr	push_quantum_reg
			bra	ParseLoop

\VarError:	move.l	a3,a2
		bra	ParseFinal

\Function	addq.l	#1,a2			; Skip '('
		bsr	push_END_TAG		; Push END tag
		pea	(a3)			; Preserve Beginning 
		move.w	d1,-(a7)		; Preserve len
		bsr	ParseRecur		; Parse all args 
		move.w	(a7)+,d1		; Get len
		move.l	(a7)+,a3		; Get ptr to the beginning of the function name
		move.b	(a2)+,d2		; Read stop char
		cmpi.b	#')',d2			; Check if ok ?
		bne.s	\VarError		; If it is ')', it is ok
		clr.w	d0			; Push now the function name
		bsr	push_quantum_reg	; 0
		subq.w	#1,d1			; -1 for dbf
\Loop2			move.b	(a3)+,d0	; Read char
			bsr	push_quantum_reg ; push it
			dbf	d1,\Loop2	; Loop
		clr.w	d0			; Push 0	
		bsr	push_quantum_reg	
		move.w	#$DA,d0			; & Push USER_FUNC
		bsr	push_quantum_reg
		bra	ParseLoop

; Parse an push a Float
ParseInputFloat:
		addq.l	#1,a2			; Advance String Ptr
		; Clean FloatReg1
		clr.l	FloatReg1
		clr.l	FloatReg1+4
		clr.l	FloatReg1+8
		; Check Sign
		cmpi.b	#175,d2
		bne.s	\NotMinus
			move.w	#$FFFF,FloatReg1+FLOAT.sign
			move.b	(a2)+,d2	; Read next char
\NotMinus	lea	FloatReg1+FLOAT.mantissa,a3	; Mantisse Ptr
		moveq	#15,d7			; 16 fingers
		; Read mantisse
		moveq	#-1,d6			; Exponent
\Loop1			subi.b	#'0',d2
			bcs.s	\EndOfLoop1
			cmpi.b	#9,d2
			bhi.s	\EndOfLoop1
			addq.w	#1,d6		; Expo++
			btst.l	#0,d7
			beq.s	\Advance
				lsl.w	#4,d2
				move.b	d2,(a3)
				bra.s	\Cont
\Advance:			or.b	d2,(a3)+
\Cont:			move.b	(a2)+,d2	; Read next char
			dbf	d7,\Loop1	; Next Finger
			; No more finger left in mantisse.
			; Skip all the remaining fingers
\loop11				cmpi.b	#'.',d2
				beq.s	\loop22_e
				subi.b	#'0',d2
				bcs.s	\EndOfLoop2
				subi.b	#9,d2
				bhi.s	\EndOfLoop2
				addq.w	#1,d6		; Expo++
				move.b	(a2)+,d2
				bne.s	\loop11
				bra.s	\NoExponent
\EndOfLoop1:	; Check if '.'
		cmpi.b	#'.'-'0',d2
		bne.s	\NoPoint
			move.b	(a2)+,d2	; Read next char
\Loop2				subi.b	#'0',d2
				bcs.s	\EndOfLoop2
				cmpi.b	#9,d2
				bhi.s	\EndOfLoop2
				btst.l	#0,d7
				beq.s	\Advance2
					lsl.w	#4,d2
					move.b	d2,(a3)
					bra.s	\Cont2
\Advance2:				or.b	d2,(a3)+
\Cont2				move.b	(a2)+,d2	; Read next char
				dbf	d7,\Loop2	; Next Finger
			; No more finger left in mantisse.
			; Skip all the remaining fingers
\loop22				subi.b	#'0',d2
				bcs.s	\EndOfLoop2
				cmpi.b	#9,d2
				bhi.s	\EndOfLoop2
\loop22_e			move.b	(a2)+,d2
				bne.s	\loop22
				bra.s	\NoExponent
\EndOfLoop2:
\NoPoint	; Check if 'E'
		cmpi.b	#149-'0',d2
		bne.s	\NoExponent
			; Read decimal exponent.
			move.b	(a2)+,d2
			cmpi.b	#175,d2
			sne.b	d4
			bne.s	\NoExpoSign
				move.b	(a2)+,d2
\NoExpoSign		; Decimal
			moveq	#0,d5
			moveq	#3,d7
\Loop3				subi.b	#'0',d2
				bcs.s	\EndOfLoop3
				cmpi.b	#9,d2
				bhi.s	\EndOfLoop3
				ext.w	d2
				mulu.w	#10,d5
				add.w	d2,d5
				move.b	(a2)+,d2	; Read next char
				dbf	d7,\Loop3	; Next Finger
\EndOfLoop3		tst.b	d4
			bne.s	\NoNeg
				neg.w	d5					
\NoNeg			add.w	d5,d6
\NoExponent	
		add.w	#$4000,d6			; Average
		move.w	d6,FloatReg1+FLOAT.exponent	; Save Exponent
		bsr	FloatAdjust			; Adjust Float
		; Push Float To ESTACk
		lea	FloatReg1,a0
		lea	FloatReg2+FLOAT.exponent,a1
		bsr	FloatInternal2AMS
		lea	FloatReg2+FLOAT.exponent,a1
		bsr	push_Float_reg
		clr.b	d3				; Last Thing was a FLOAT
		subq.l	#1,a2				; Back a2
		bra	ParseLoop

; Say if d2 may be a number.
ParseIsNumber:
	cmpi.b	#'0'-1,d2	
	bls.s	\CheckPoint
	cmpi.b	#'9',d2
	bhi.s	\CheckEMinus
\IsNumber	moveq	#1,d0
		rts
\CheckPoint:
	cmpi.b	#46,d2
	seq	d0
	rts
\CheckEMinus:
	cmpi.b	#175,d2
	beq.s	\IsNumber
	cmpi.b	#149,d2
	seq	d0
	rts

; Say if d2 may be a variable
ParseIsVar:
	cmpi.b	#'\',d2
	beq.s	\Ok
	cmpi.b	#'0'-1,d2
	bls.s	ParseIsPureVar
	cmpi.b	#'9',d2
	bhi.s	ParseIsPureVar
\Ok		moveq	#1,d0
		rts

; Say if d2 may be the beginning of a var
ParseIsPureVar:
	cmpi.b	#'A'-1,d2
	bls.s	\End
	cmpi.b	#'Z',d2
	bhi.s	\Next
	addi.b	#'a'-'A',d2	
\Ok	moveq	#1,d0
	rts
\Next	cmpi.b	#'_',d2
	beq.s	\Ok
	cmpi.b	#'a'-1,d2
	bls.s	\End
	cmpi.b	#'z',d2
	bls.s	\Ok
	; 128 -> 155 / 	178-> 182 / 188->255
	cmpi.b	#128-1,d2
	bls.s	\End
	cmpi.b	#155,d2
	bls.s	\Ok
	cmpi.b	#178-1,d2
	bls.s	\End
	cmpi.b	#182,d2
	bls.s	\Ok
	cmpi.b	#188-1,d2
	bhi.s	\Ok
\End	moveq	#0,d0
	rts
		

;HANDLE NG_RPNToText (HANDLE hRPN, unsigned short NewLines, unsigned short FullPrec); 
NG_RPNToText:
	move.w	4(a7),-(a7)
	jsr	HeapLock
	bsr	HToESI
	pea	(a0)
	bsr.s	display_statements
	jsr	HeapUnlock
	addq.l	#6,a7
	rts

;HANDLE display_statements (CESI ptr, unsigned short Newlines, unsigned short FullPrec);
;HANDLE Parse1DExpr (CESI ptr, unsigned short FullPrec, unsigned short width); 
; We don't care about the extra-parameters
display_statements:
Parse1DExpr:
	move.l	4(a7),a0
	movem.l	d3-d7/a2-a6,-(a7)
	move.l	a0,a5
	pea	($100).w
	jsr	HeapAlloc
	move.w	d0,d3
	beq.s	\Error
		move.w	d3,(a7)
		jsr	HLock
		move.l	a0,a4		; Ptr
		move.w	#$100,d4	; Remaining len
		bsr.s	Display1DESI
		clr.b	(a4)
		move.w	(a7),d3
		jsr	HeapUnlock
\Error:	addq.l	#4,a7
	move.w	d3,d0
	movem.l	(a7)+,d3-d7/a2-a6
	rts
	
; In:
;	a5 -> Expression
;	a4 -> String Ptr
;	d4.w = Remaining bytes in string
Display1DESI:
	move.b	(a5),d0			; Read TAB
	subq.l	#1,a5
	tst.b	d0
	beq	Display1DVAR
	cmpi.b	#$23,d0
	beq	Display1DBCD
	cmpi.b	#$8B,d0
	beq	Display1DPLUS
	cmpi.b	#$8D,d0
	beq	Display1DMOINS
	cmpi.b	#$8F,d0
	beq	Display1DMULT
	cmpi.b	#$91,d0
	beq	Display1DDIV
	cmpi.w	#$93,d0
	beq	Display1DPOW	
	cmpi.b	#$DA,d0
	beq	Display1DFUNC
	cmpi.b	#$D9,d0			; List
	beq	Display1DZERO
	cmpi.b	#$2D,d0			; String
	beq	Display1DSTRING
	cmpi.b	#$E5,d0			; End
	beq	Display1DZERO

Display1DError:
	dc.w	$A000+999		; Error memory

Display1DVAR:
\loop1		tst.b	-(a5)
		bne.s	\loop1
	lea	1(a5),a0
\loop2		subq.w	#1,d4
		beq	Display1DError
		move.b	(a0)+,(a4)+
		bne.s	\loop2
	subq.l	#1,a4
	addq.w	#1,d4
	rts

Display1DSTRING:
	subq.w	#2,d4
	bls	Display1DError
	move.b	#'"',(a4)+
	bsr.s	Display1DVAR
	move.b	#'"',(a4)+
	rts
	
Display1DZERO:
	subq.w	#1,d4
	beq	Display1DError
	move.b	#'0',(a4)+
	rts
	
Display1DBCD: ; HERE
	sub.w	#1+1+1+15+1+3,d4
	bls	Display1DError
	move.l	a5,a0
	moveq	#5-1,d1
\loop		move.b	-1(a0),d0
		lsl.w	#8,d0
		move.b	(a0),d0
		move.w	d0,-(a7)
		subq.l	#2,a0
		dbf	d1,\loop
	pea	FloatFormat_str
	pea	(a4)
	bsr	sprintf
	lea	(10+4+4)(a7),a7
	add.w	d0,a4
	rts

Display1DPLUS:
	move.b	#'+',d3
	bra.s	Display1D2ARGS
	
Display1DMOINS:
	move.b	#'-',d3
	bra.s	Display1D2ARGS

Display1DMULT:
	move.b	#'*',d3
	bra.s	Display1D2ARGS

Display1DDIV:
	move.b	#'/',d3
	bra.s	Display1D2ARGS

Display1DPOW:
	move.b	#'/',d3

Display1D2ARGS:
	pea	(a5)
	bsr	next_expression_index
	move.l	a0,a5
	bsr	Display1DESI
	subq.w	#1,d4
	beq	Display1DError
	move.b	d3,(a4)+
	move.l	(a7),a5
	bsr	Display1DESI
	move.l	(a7)+,a5
	rts

Display1DFUNC	
	bsr	Display1DVAR		; Display Func Name
	subq.w	#1,d4
	beq	Display1DError
	move.b	#'(',(a4)+
\loop		cmpi.b	#$E5,(a5)
		beq.s	\End
		bsr	Display1DESI		; Display Arg1 (Error, it is the last arg !)
		pea	(a5)
		bsr	next_expression_index	; Next argument
		addq.l	#4,a7
		move.l	a0,a5
		subq.w	#1,d4
		beq	Display1DError
		move.b	#',',(a4)+
		bra.s	\loop
\End	move.b	#')',-1(a4)
	rts
	
	ifeq	1
DoParse1DTable
	dc.w	P1D_VarName-DoParse1DTable
	dc.w	P1D_QName-DoParse1DTable
	dc.w	P1D_SName-DoParse1DTable
	dc.w	P1D_TName-DoParse1DTable
	dc.w	P1D_UName-DoParse1DTable
	dc.w	P1D_VName-DoParse1DTable
	dc.w	P1D_WName-DoParse1DTable
	dc.w	P1D_XName-DoParse1DTable
	dc.w	P1D_YName-DoParse1DTable
	dc.w	P1D_ZName-DoParse1DTable
	dc.w	P1D_AName-DoParse1DTable
	dc.w	P1D_BName-DoParse1DTable
	dc.w	P1D_CName-DoParse1DTable
	dc.w	P1D_DName-DoParse1DTable
	dc.w	P1D_EName-DoParse1DTable
	dc.w	P1D_FName-DoParse1DTable
	dc.w	P1D_GName-DoParse1DTable
	dc.w	P1D_HName-DoParse1DTable
	dc.w	P1D_IName-DoParse1DTable
	dc.w	P1D_JName-DoParse1DTable
	dc.w	P1D_KName-DoParse1DTable
	dc.w	P1D_LName-DoParse1DTable
	dc.w	P1D_MName-DoParse1DTable
	dc.w	P1D_NName-DoParse1DTable
	dc.w	P1D_OName-DoParse1DTable
	dc.w	P1D_PName-DoParse1DTable
	dc.w	P1D_QName-DoParse1DTable
	dc.w	P1D_ExtSys-DoParse1DTable
	dc.w	P1D_ArbReal-DoParse1DTable
	dc.w	P1D_ArbInt-DoParse1DTable
	dc.w	P1D_PosInt-DoParse1DTable
	dc.w	P1D_NegInt-DoParse1DTable
	dc.w	P1D_PosFrac-DoParse1DTable
	dc.w	P1D_NegFrac-DoParse1DTable
	dc.w	P1D_Float-DoParse1DTable
	dc.w	P1D_Pi-DoParse1DTable
	dc.w	P1D_E-DoParse1DTable
	dc.w	P1D_I-DoParse1DTable
	dc.w	P1D_NegInfinity-DoParse1DTable
	dc.w	P1D_PosInfinity-DoParse1DTable
	dc.w	P1D_PoNInfinity-DoParse1DTable
	dc.w	P1D_Undef-DoParse1DTable
	dc.w	P1D_False-DoParse1DTable
	dc.w	P1D_True-DoParse1DTable
	dc.w	P1D_String-DoParse1DTable
	dc.w	P1D_Nothing-DoParse1DTable
	dc.w	P1D_ACosH-DoParse1DTable
	dc.w	P1D_ASinH-DoParse1DTable
	dc.w	P1D_ATanH-DoParse1DTable
	dc.w	P1D_CosH-DoParse1DTable
	dc.w	P1D_SinH-DoParse1DTable
	dc.w	P1D_TanH-DoParse1DTable
	dc.w	P1D_Nothing-DoParse1DTable
	dc.w	P1D_Nothing-DoParse1DTable
	dc.w	P1D_Nothing-DoParse1DTable
	dc.w	P1D_ACos-DoParse1DTable
	dc.w	P1D_ASin-DoParse1DTable
	dc.w	P1D_ATan-DoParse1DTable
	dc.w	P1D_Nothing-DoParse1DTable
	dc.w	P1D_Nothing-DoParse1DTable
	dc.w	P1D_Nothing-DoParse1DTable
	dc.w	P1D_ACos-DoParse1DTable
	dc.w	P1D_ASin-DoParse1DTable
	dc.w	P1D_ATan-DoParse1DTable
	dc.w	P1D_Cos-DoParse1DTable
	dc.w	P1D_Sin-DoParse1DTable
	dc.w	P1D_Tan-DoParse1DTable
	dc.w	P1D_Nothing-DoParse1DTable
	dc.w	P1D_Nothing-DoParse1DTable
	dc.w	P1D_Nothing-DoParse1DTable
	dc.w	P1D_Tan-DoParse1DTable
	dc.w	P1D_Abs-DoParse1DTable
	dc.w	P1D_Angle-DoParse1DTable
	dc.w	P1D_Ceiling-DoParse1DTable
	dc.w	P1D_Floor-DoParse1DTable
	dc.w	P1D_Int-DoParse1DTable
	dc.w	P1D_Sign-DoParse1DTable
	dc.w	P1D_Sqrt-DoParse1DTable
	dc.w	P1D_Exp-DoParse1DTable
	dc.w	P1D_Ln-DoParse1DTable
	dc.w	P1D_Log-DoParse1DTable
	dc.w	P1D_FPart-DoParse1DTable
	dc.w	P1D_IPart-DoParse1DTable
	dc.w	P1D_Conj-DoParse1DTable
	dc.w	P1D_Imag-DoParse1DTable
	dc.w	P1D_Real-DoParse1DTable
	dc.w	P1D_Approx-DoParse1DTable
	dc.w	P1D_TExpand-DoParse1DTable
	dc.w	P1D_TCollect-DoParse1DTable
	dc.w	P1D_GetDenom-DoParse1DTable
	dc.w	P1D_GetNum-DoParse1DTable
	dc.w	P1D_Nothing-DoParse1DTable
	dc.w	P1D_CumSum-DoParse1DTable
	dc.w	P1D_Det-DoParse1DTable
	dc.w	P1D_ColNorm-DoParse1DTable
	dc.w	P1D_RowNorm-DoParse1DTable
	dc.w	P1D_Norm-DoParse1DTable
	dc.w	P1D_Mean-DoParse1DTable
	dc.w	P1D_Median-DoParse1DTable
	dc.w	P1D_Product-DoParse1DTable
	dc.w	P1D_StdDev-DoParse1DTable
	dc.w	P1D_Sum-DoParse1DTable
	dc.w	P1D_Variance-DoParse1DTable
	dc.w	P1D_UnitV-DoParse1DTable
	dc.w	P1D_Dim-DoParse1DTable
	dc.w	P1D_Mat2List-DoParse1DTable
	dc.w	P1D_NewList-DoParse1DTable
	dc.w	P1D_RRef-DoParse1DTable
	dc.w	P1D_Ref-DoParse1DTable
	dc.w	P1D_Identity-DoParse1DTable
	dc.w	P1D_Diag-DoParse1DTable
	dc.w	P1D_ColDim-DoParse1DTable
	dc.w	P1D_RowDim-DoParse1DTable
	dc.w	P1D_Transpose-DoParse1DTable
	dc.w	P1D_Factorial-DoParse1DTable
	dc.w	P1D_Percent-DoParse1DTable
	dc.w	P1D_Radians-DoParse1DTable
	dc.w	P1D_Not-DoParse1DTable
	dc.w	P1D_Minus-DoParse1DTable
	dc.w	P1D_VecPolar-DoParse1DTable
	dc.w	P1D_VecCylind-DoParse1DTable
	dc.w	P1D_VecSphere-DoParse1DTable
	dc.w	P1D_Start-DoParse1DTable
	dc.w	P1D_IStore-DoParse1DTable
	dc.w	P1D_Store-DoParse1DTable
	dc.w	P1D_With-DoParse1DTable
	dc.w	P1D_Xor-DoParse1DTable
	dc.w	P1D_Or-DoParse1DTable
	dc.w	P1D_And-DoParse1DTable
	dc.w	P1D_Lt-DoParse1DTable
	dc.w	P1D_Le-DoParse1DTable
	dc.w	P1D_Eq-DoParse1DTable
	dc.w	P1D_Ge-DoParse1DTable
	dc.w	P1D_Gt-DoParse1DTable
	dc.w	P1D_Ne-DoParse1DTable
	dc.w	P1D_Add-DoParse1DTable
	dc.w	P1D_AddElt-DoParse1DTable
	dc.w	P1D_Sub-DoParse1DTable
	dc.w	P1D_SubElt-DoParse1DTable
	dc.w	P1D_Mul-DoParse1DTable
	dc.w	P1D_MulElt-DoParse1DTable
	dc.w	P1D_Div-DoParse1DTable
	dc.w	P1D_DivElt-DoParse1DTable
	dc.w	P1D_Pow-DoParse1DTable
	dc.w	P1D_PowElt-DoParse1DTable
	dc.w	P1D_SinCos-DoParse1DTable
	dc.w	P1D_Solve-DoParse1DTable
	dc.w	P1D_CSolve-DoParse1DTable
	dc.w	P1D_NSolve-DoParse1DTable
	dc.w	P1D_Zeros-DoParse1DTable
	dc.w	P1D_CZeros-DoParse1DTable
	dc.w	P1D_FMin-DoParse1DTable
	dc.w	P1D_FMax-DoParse1DTable
	dc.w	P1D_Complex-DoParse1DTable
	dc.w	P1D_PolyEval-DoParse1DTable
	dc.w	P1D_RandPoly-DoParse1DTable
	dc.w	P1D_CrossP-DoParse1DTable
	dc.w	P1D_DotP-DoParse1DTable
	dc.w	P1D_Gcd-DoParse1DTable
	dc.w	P1D_Lcm-DoParse1DTable
	dc.w	P1D_Mod-DoParse1DTable
	dc.w	P1D_IntDiv-DoParse1DTable
	dc.w	P1D_Remain-DoParse1DTable
	dc.w	P1D_Ncr-DoParse1DTable
	dc.w	P1D_Npr-DoParse1DTable
	dc.w	P1D_P2Rx-DoParse1DTable
	dc.w	P1D_P2Ry-DoParse1DTable
	dc.w	P1D_P2PTheta-DoParse1DTable
	dc.w	P1D_P2PR-DoParse1DTable
	dc.w	P1D_Augment-DoParse1DTable
	dc.w	P1D_NewMat-DoParse1DTable
	dc.w	P1D_RandMat-DoParse1DTable
	dc.w	P1D_Simult-DoParse1DTable
	dc.w	P1D_Part-DoParse1DTable
	dc.w	P1D_Exp2List-DoParse1DTable
	dc.w	P1D_RandNorm-DoParse1DTable
	dc.w	P1D_Mrow-DoParse1DTable
	dc.w	P1D_RowAdd-DoParse1DTable
	dc.w	P1D_RowSwap-DoParse1DTable
	dc.w	P1D_ArcLen-DoParse1DTable
	dc.w	P1D_NInt-DoParse1DTable
	dc.w	P1D_PiProduct-DoParse1DTable
	dc.w	P1D_SigmaSum-DoParse1DTable
	dc.w	P1D_MRowAdd-DoParse1DTable
	dc.w	P1D_Ans-DoParse1DTable
	dc.w	P1D_Entry-DoParse1DTable
	dc.w	P1D_Exact-DoParse1DTable
	dc.w	P1D_Logb-DoParse1DTable
	endif

;void NG_graphESI (CESI ptr, HANDLE Handle);	// Unused Handle
;void NG_approxESI (CESI ptr);
;void NG_rationalESI (CESI ptr);
NG_graphESI:
NG_approxESI:
NG_rationalESI:
	move.l	4(a7),a0
	movem.l	d3-d7/a2-a6,-(a7)
	; Evaluate Expression
	move.l	a0,a5		; CESI
	bsr.s	EvalESI		; Evaluate it
	; Push Float To ESTACk
	lea	FloatReg1,a0
	lea	FloatReg1+FLOAT.exponent,a1
	bsr	FloatInternal2AMS
	lea	FloatReg1+FLOAT.exponent,a1
	bsr	push_Float_reg
	movem.l	(a7)+,d3-d7/a2-a6
	rts
	
;void NG_execute (HANDLE Handle, short approx_flag); 
NG_execute:
	move.w	4(a7),d0
	movem.l	d3-d7/a2-a6,-(a7)
	; Get ESI ptr and lock it
	move.w	d0,-(a7)
	jsr	HeapLock
	bsr	HToESI
	; Eval it
	move.l	a0,a5
	bsr.s	EvalESI		; Evaluate it
	; Push Float To ESTACk
	lea	FloatReg1,a0
	lea	FloatReg2+FLOAT.exponent,a1
	bsr	FloatInternal2AMS
	lea	FloatReg2+FLOAT.exponent,a1
	bsr	push_Float_reg
	; Unlock file
	jsr	HeapUnlock
	addq.l	#2,a7
	movem.l	(a7)+,d3-d7/a2-a6
	rts
		
; Evaluate an ESI.
; In:
;	a5 -> ESI
; Out:
;	FReg1 = The result
; Destroy:
;	All
; Note:
;	May thrown Various Error	
EvalESI:
	move.b	(a5),d0			; Read TAB
	subq.l	#1,a5
	tst.b	d0
	beq	EvalVAR
	cmpi.b	#$23,d0
	beq	EvalBCD
	cmpi.b	#$8B,d0
	beq	EvalPLUS
	cmpi.b	#$8D,d0
	beq	EvalMOINS
	cmpi.b	#$8F,d0
	beq	EvalMULT
	cmpi.b	#$91,d0
	beq	EvalDIV
	cmpi.w	#$93,d0
	beq	EvalPOW	
	cmpi.b	#$DA,d0
	beq	EvalFUNC
	cmpi.b	#$D9,d0			; List
	beq	EvalZERO
	cmpi.b	#$2D,d0			; String
	beq	EvalZERO
	cmpi.b	#$E5,d0			; End
	beq	EvalZERO
	cmpi.b	#$80,d0
	beq	EvalSTORE
	dc.w	$A000+999		; TAG can't be interpreted.

EvalZERO:
	lea	FloatZero(pc),a0
	lea	FloatReg1,a1		; FReg1 = Result
	bra	FloatAMS2Internal

EvalBCD: ; HERE
	lea	-9(a5),a0		; AMS Float (May be unaligned)
	lea	EXEC_RAM,a1
	moveq	#10-1,d0
\loop		move.b	(a0)+,(a1)+
		dbf	d0,\loop
	lea	EXEC_RAM,a0
	lea	FloatReg1,a1		; FReg1 = Result
	bra	FloatAMS2Internal
	
EvalVAR:
	; Check if it is Pi or E ?
	tst.b	-1(a5)
	bne.s	\NoSingleVar
		move.b	(a5),d0
		lea	FloatPi(pc),a0
		cmpi.b	#140,d0
		beq.s	\CopyVar
		lea	FloatE(Pc),a0
		cmpi.b	#150,d0
		bne.s	\NoSingleVar
\CopyVar		movem.l	(a0),d1-d3
			movem.l	d1-d3,FloatReg1
			rts
\NoSingleVar
	pea	1(a5)
	jsr	SymFindPtr
	move.l	a0,d0
	bne.s	\Ok
		dc.w	$A000+999	; Variable not found
\Ok:	
	move.w	SYM_ENTRY.hVal(a0),-(a7)
	jsr	HeapGetLock
	tst.w	d0
	beq.s	\Ok3
		dc.w	$A000+190	; Circular Definition
\Ok3	jsr	HeapLock		; Lock variable
	bsr	HToESI			; Deref it
	cmpi.b	#$DA,(a0)		; Check if it is not a USER FUNC.
	beq.s	\Error
	cmpi.b	#$F3,(a0)		; Check if it is not an ASM FUNC.
	bne.s	\Ok2
\Error		dc.w	$A000+620
\Ok2	move.l	a0,a5			; We will
	bsr	EvalESI			; Evaluate ESI
	jsr	HeapUnlock
	addq.l	#2,a7
	move.l	(a7)+,a5
	rts

; Evaluate 2 argument
;	1st, result in FloatReg1
;	2nd, result in FloatReg2
Eval2ARGS:
	pea	(a5)
	bsr	EvalESI			; Eval 1st arg
	move.l	(a7)+,a5
	movem.l	FloatReg1,d0-d2
	movem.l	d0-d2,-(a7)		; Push FloatReg1
	pea	(a5)
	bsr	next_expression_index	; 2nd argument
	move.l	a0,a5
	bsr	EvalESI			; Eval 2nd argument
	move.l	(a7)+,a5
	movem.l	(a7)+,d0-d2
	movem.l	d0-d2,FloatReg2		; Eval of 2nd arg in FloatReg2
	rts
	
EvalPLUS:
	bsr	Eval2ARGS
	bra	FloatAdd
	
EvalMOINS:
	bsr	Eval2ARGS
	bra	FloatSub

EvalMULT:
	bsr	Eval2ARGS
	movem.l	FloatReg1,d0-d2/d3-d5
	movem.l	d0-d2/d3-d5,FloatReg3
	bra	FloatMult

EvalDIV:
	bsr	Eval2ARGS
	movem.l	FloatReg1,d0-d2/d3-d5
	movem.l	d3-d5,FloatReg3
	movem.l	d0-d2,FloatReg4
	bra	FloatDivide

EvalPOW:
	bsr	Eval2ARGS
	jmp	FloatPow

EvalEqual:
	bsr	Eval2ARGS
	bsr	FloatCmp
	lea	FloatOne(pc),a0
	tst.b	d0
	beq.s	\Ok
		lea	FloatZero(pc),a0
\Ok:	lea	FloatReg1,a1		; FReg1 = Result
	bra	FloatAMS2Internal

EvalNotEqual:
	bsr	Eval2ARGS
	bsr	FloatCmp
	lea	FloatOne(pc),a0
	tst.b	d0
	bne.s	\Ok
		lea	FloatZero(pc),a0
\Ok:	lea	FloatReg1,a1		; FReg1 = Result
	bra	FloatAMS2Internal

EvalSupEqual:
	bsr	Eval2ARGS
	bsr	FloatCmp
	lea	FloatOne(pc),a0
	tst.b	d0
	bge.s	\Ok
		lea	FloatZero(pc),a0
\Ok:	lea	FloatReg1,a1		; FReg1 = Result
	bra	FloatAMS2Internal

EvalSup:
	bsr	Eval2ARGS
	bsr	FloatCmp
	lea	FloatOne(pc),a0
	tst.b	d0
	bgt.s	\Ok
		lea	FloatZero(pc),a0
\Ok:	lea	FloatReg1,a1		; FReg1 = Result
	bra	FloatAMS2Internal

EvalInfEqual:
	bsr	Eval2ARGS
	bsr	FloatCmp
	lea	FloatOne(pc),a0
	tst.b	d0
	ble.s	\Ok
		lea	FloatZero(pc),a0
\Ok:	lea	FloatReg1,a1		; FReg1 = Result
	bra	FloatAMS2Internal

EvalInf:
	bsr	Eval2ARGS
	bsr	FloatCmp
	lea	FloatOne(pc),a0
	tst.b	d0
	blt.s	\Ok
		lea	FloatZero(pc),a0
\Ok:	lea	FloatReg1,a1		; FReg1 = Result
	bra	FloatAMS2Internal

EvalFUNC:
	pea	(a5)
	jsr	SymFindPtr
	move.l	a0,d0
	bne.s	\Ok
		; Check if it is a built-in function
		;bra	*
		\Cvt:	tst.b	-(a5)
			bne.s	\Cvt
		addq.l	#1,a5
		lea	InternalBcdFunctions,a2
		move.w	#InternalBcdFunctions_END-InternalBcdFunctions-1,d3
		moveq	#0,d0
		\LoopTable:
			pea	(a5)
			move.w	(a2),d0
			pea	0(a2,d0.l)
			jsr	strcmp
			addq.l	#8,a7
			tst.w	d0
			beq.s	\FindIt
			addq.l	#4,a2
			dbf	d3,\LoopTable
		dc.w	$A000+999	; Variable not found
\FindIt		subq.l	#2,a5
		pea	(a2)
		bsr	EvalESI			; Eval 1st arg
		move.l	(a7)+,a2
		move.w	2(a2),d0
		jsr	0(a2,d0.w)
		move.l	(a7)+,a5
		rts

\Ok:	
	; Skip var name
\loop		tst.b	-(a5)
		bne.s	\loop
	subq.l	#1,a5
	move.w	SYM_ENTRY.hVal(a0),-(a7)
	jsr	HeapLock		; Lock variable
	bsr	HToESI			; Deref it
	cmpi.b	#$DA,(a0)		; Check if it is not a USER FUNC.
	beq.s	EvalBASICFUNC
	cmpi.b	#$F3,(a0)
	beq.s	EvalASMFUNC
		dc.w	$A000+620
		
EvalBASICFUNC:
	dc.w	$A000+999		; TO DO
	;bsr	HeapUnlock
	;addq.l	#2,a7
	;move.l	(a7)+,a5
	;rts

EvalASMFUNC:
	move.l	top_estack,a4
	; Push les arg sur l'EStack
	bsr	push_END_TAG
\PushLoop	cmpi.b	#$E5,(a5)
		beq.s	\Done
		cmpi.b	#$2D,(a5)
		beq.s	\PushString
			pea	(a5)
			bsr	EvalESI			; Eval 1st arg
			lea	FloatReg1,a0
			lea	FloatReg1+2,a1
			bsr	FloatInternal2AMS
			lea	FloatReg1+2,a1
			bsr	push_Float_reg		; Push Float on EStack
			bsr	next_expression_index	; 2nd argument
			addq.l	#4,a7
			move.l	a0,a5
			bra.s	\PushLoop	
\PushString:	subq.l	#1,a5
		moveq	#2,d0
\len			addq.w	#1,d0
			tst.b	-(a5)
			bne.s	\len	
		move.w	d0,-(a7)
		bsr	check_estack_size
		move.w	(a7)+,d0
		subq.w	#1,d0
		move.l	top_estack,a1
		addq.l	#1,a1
		move.l	a5,a0
\CpyLoop		move.b	(a0)+,(a1)+
			dbf	d0,\CpyLoop
		subq.l	#1,a1
		move.l	a1,top_estack
		subq.l	#1,a5
		bra.s	\PushLoop
\Done	; Appeller avec kernel::exec
	move.w	(a7)+,d0
	bsr	kernel::exec
	; Poppe la valeur de retour
	move.l	top_estack,a0
	cmpi.b	#$23,(a0)
	bne.s	\NoFloat
		lea	-10(a0),a0
		lea	FloatReg1,a1
		bsr	FloatAMS2Internal
\NoFloat	
	; Restaure l'EStack
	move.l	(a7)+,a5
	move.l	a4,top_estack
	rts
		
EvalSTORE:
	; Check if Arg2 is a VAR_NAME
	tst.b	(a5)
	beq.s	\Ok
		dc.w	$A000+140
\Ok	pea	(a5)
	bsr	next_expression_index
	pea	(a0)			; ESI 
	clr.w	-(a7)			; Size (Not used...)
	move.w	#STOF_ESI,-(a7)		; Flag
	pea	(a5)			; Var Name
	jsr	VarStore		; Store Val
	lea	12(a7),a7
	move.l	(a7)+,a5
	rts
	
;short are_expressions_identical (CESI ptr1, CESI ptr2); 
are_expressions_identical:
	move.l	8(a7),a0
	bsr	next_expression_index_reg
	move.l	a0,d0
	sub.l	8(a7),d0		; Size
	pea	(a0)			; Push a0
	move.l	8(a7),a0
	bsr	next_expression_index_reg	
	move.l	(a7)+,a1		; Reload old ptr
	move.l	a0,d1
	sub.l	4(a7),d1		; Size
	cmp.l	d0,d1
	bne.s	\Different
		jsr	memcmp_reg
		tst.w	d0
		bne.s	\Different
		moveq	#1,d0
		rts
\Different
	clr.w	d0
	rts
		
	