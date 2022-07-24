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

;******************************************************************
;***                                                            ***
;***            	Misc Functions (1)			***
;***                                                            ***
;******************************************************************

Idle:
idle:
	trap	#0
	rts
	
off:
	trap	#4
	rts
	
ER_catch:
	move.l	4(a7),a0
	move.l	ERROR_LIST,d1			; Get Error List
	move.l	a0,ERROR_LIST			; Save New Error List Header
	movem.l	d3-d7/a2-a7,(a0)		; Save Registers
	lea	(7-3+1+7-2+1)*4(a0),a0
	move.l	VAR_SYSTEM1,(a0)+		; Save System Var
	move.l	top_estack,(a0)+		; Save top_estack
	move.l	(a7),(a0)+			; Save Return Address
	move.l	d1,(a0)+			; Save Old Error List to create the list
	clr.w	d0
	rts

ER_success:
	move.l	ERROR_LIST,a0			; Pop the header of the current list
	move.l	$38(a0),ERROR_LIST
	rts
	
ER_throwVar:
	move.w	4(a7),d0			; Transform error 0 in 1
	bne.s	ER_throwVar_reg
		moveq	#1,d0
ER_throwVar_reg:
	move.l	ERROR_LIST,d1			; Is List Empty ?
	bne.s	\ok
		lea	ErrorListEmpty_str(Pc),a0
		jmp	FATAL_ERROR		; Yes => Fatal Error
\ok:	move.l	d1,a0			
	movem.l	(a0)+,d3-d7/a2-a7		; Restore registers
	addq.l	#4,a7				; Pop return address (should be the same as a1)
	move.l	(a0)+,VAR_SYSTEM1
	move.l	(a0)+,top_estack		; Get top_estack
	move.l	(a0)+,a1			; Read Return address
	move.l	(a0),ERROR_LIST			; Pop current header
	jmp	(a1)				; Return to program 
						; A rts without doing an addq should do the same job.
						
OSVFreeTimer:
OSFreeTimer:
	moveq	#0,d0
	move.w	4(a7),d2
	subq.w	#1,d2
	cmpi.w	#TIMER_NUMBER,d2
	bcc.s	\error
		mulu.w	#TIMER_SIZE,d2
		lea	TIMER_TABLE,a0
		clr.w	TIMER_TYPE(a0,d2.w)
		clr.l	TIMER_CUR_VAL(a0,d2.w)
		clr.l	TIMER_CALLBACK(a0,d2.w)
		moveq	#1,d0
\error:	rts

OSTimerCurVal:
	move.w	4(a7),d0
OSTimerCurVal_Reg:
	subq.w	#1,d0
	lea	TIMER_TABLE,a0
	mulu.w	#TIMER_SIZE,d0
	move.l	TIMER_CUR_VAL(a0,d0.w),d0
	rts

OSTimerExpired:
	move.w	4(a7),d0
	bsr.s	OSTimerCurVal_Reg
	tst.l	d0
	seq	d0
	ext.w	d0
	rts
	
OSTimerRestart:
	moveq	#-1,d0
	move.w	4(a7),d2
	subq.w	#1,d2
	cmpi.w	#TIMER_NUMBER,d2
	bcc.s	\error
		mulu.w	#TIMER_SIZE,d2
		lea	TIMER_TABLE,a0
		move.l	TIMER_CUR_VAL(a0,d2.w),d0
		move.l	TIMER_RESET_VAL(a0,d2.w),TIMER_CUR_VAL(a0,d2.w)
\error:	rts

OSVRegisterTimer:
	move.l	10(a7),a1	; CallBack
	bra.s	_RegisterTimer

OSRegisterTimer:
	suba.l	a1,a1		; No Callback
_RegisterTimer:
	moveq	#0,d0
	move.w	4(a7),d2	; Timer No
	subq.w	#1,d2
	cmpi.w	#TIMER_NUMBER,d2
	bcc.s	\error
		mulu.w	#TIMER_SIZE,d2
		lea	TIMER_TABLE,a0
		tst.b	TIMER_TYPE(a0,d2.w)
		bne.s	\error
			move.l	6(a7),d0			; Read initial value of timer
			move.l	d0,TIMER_RESET_VAL(a0,d2.w)	; Reset Value
			move.l	d0,TIMER_CUR_VAL(a0,d2.w)	; Current Value of Timer
			move.l	a1,TIMER_CALLBACK(a0,d2.w)
			moveq	#TIMER_TYPE_COUNT,d0
			move.l	a1,d1
			beq.s	\done
				moveq	#TIMER_TYPE_VECTOR,d0
\done:			move.b	d0,TIMER_TYPE(a0,d2.w)
\error	rts

OSDisableBreak:
	clr.b	ENABLE_BREAK_KEY
	bra.s	OSClearBreak

OSEnableBreak:
	st.b	ENABLE_BREAK_KEY

OSClearBreak:
	st.b	$60001A			; Clear call int 6 (If SR = 2700)
	clr.b	BREAK_KEY
	rts

OSCheckBreak:
	clr.w	d0
	move.b	BREAK_KEY,d0
	rts
	
OSInitBetweenKeyDelay:
	move.w	4(a7),KEY_ORG_REPEAT_CPT
	rts

OSInitKeyInitDelay:
	move.w	4(a7),KEY_ORG_START_CPT
	rts
	
GKeyDown:
kbhit:
	move.w	TEST_PRESSED_FLAG,d0
	or.b	BREAK_KEY,d0
	rts

; short GetKey()
GetKey:
;short GKeyIn (SCR_RECT *cursor_shape, unsigned short Flags); 
GKeyIn:
	movem.l	d1-d3/a0-a1,-(a7)	; I preserve d1-d2/a0-a1
	clr.w	d0
	trap	#1			; All ints allowed
	moveq	#-1,d3			; Statut
	move.w	#2,-(a7)		; Timer 2 (APD)
\restart:
	bsr	OSTimerRestart
\wait:		bsr	OSTimerExpired		; Check APD ?
		tst.w	d0
		bne.s	\Off
		bsr	Idle			; Partial Sleep Zz
		bsr	OSCheckSilentLink	; Something received in the link port ?
		tst.w	d0
		bne.s	\Link
		cmp.w	KEY_STATUS,d3		; Check if the statut has been updated ?
		bne.s	\UpdateStatut
		tst.b	BREAK_KEY		; Check Break Key
		bne.s	\Break
		tst.w	TEST_PRESSED_FLAG	; Check other keys
		beq.s	\wait
	moveq	#0,d0
	move.w	GETKEY_CODE,d0
	clr.w	TEST_PRESSED_FLAG	; Aknowlegedment of Current Key
\End	addq.l	#2,a7			; Pop APD timer
	movem.l	(a7)+,d1-d3/a0-a1	; I preserve d1-d2/a0-a1
	rts
\Off:	trap	#4		; Sleep ZZZZZZzzzzzz
	bra.s	\restart
\Link:	bsr	OSLinkCmd	; Yes -> Interpret command
	bra.s	\restart
\Break:	clr.b	BREAK_KEY	; Clear Break Key
	move.w	#KEY_ON,d0	; Return ON Key
	or.w	KEY_STATUS,d0	; With its statut
	bra.s	\End
\UpdateStatut:
	move.w	KEY_STATUS,d0		; Update Statut in Stat Line
	move.w	d0,d3
	rol.w	#4,d0
	move.w	d0,-(a7)
	jsr	ST_modKey
	addq.l	#2,a7
	bra.s	\restart

;short ngetchx (void);
ngetchx:
	move.l	d3,-(a7)		
	clr.w	d0
	trap	#1			; All ints allowed
	moveq	#-1,d3			; Statut
\wait:		bsr	OSCheckSilentLink	; Something received in the link port ?
		tst.w	d0
		bne.s	\Link
		cmp.w	KEY_STATUS,d3		; Check if the statut has been updated ?
		bne.s	\UpdateStatut
		tst.b	BREAK_KEY		; Check Break Key
		bne.s	\Break
		tst.w	TEST_PRESSED_FLAG	; Check other keys
		beq.s	\wait
	moveq	#0,d0
	move.w	GETKEY_CODE,d0
	clr.w	TEST_PRESSED_FLAG	; Aknowlegedment of Current Key
\End	move.l	(a7)+,d3
	rts
\Link:	bsr	OSLinkCmd	; Yes -> Interpret command
	bra.s	\wait
\Break:	clr.b	BREAK_KEY	; Clear Break Key
	move.w	#KEY_ON,d0	; Return ON Key
	or.w	KEY_STATUS,d0	; With its statut
	bra.s	\End
\UpdateStatut:
	move.w	KEY_STATUS,d0		; Update Statut in Stat Line
	move.w	d0,d3
	rol.w	#4,d0
	move.w	d0,-(a7)
	jsr	ST_modKey
	addq.l	#2,a7
	bra.s	\wait

; void GKeyFlush()
GKeyFlush:
	clr.w	KEY_CUR_POS
	clr.w	TEST_PRESSED_FLAG
	clr.w	KEY_STATUS
	rts
	
; void pushkey(short key)
pushkey:
	move.w	4(a7),GETKEY_CODE
	st.b	TEST_PRESSED_FLAG
	rts
	
HToESI:
	move.w	4(a7),a0
HToESI_reg:
	trap	#3
	moveq	#0,d0
	move.w	(a0),d0
	lea	1(a0,d0.l),a0
	rts




;void HelpKeys (void);
HelpKeys:
	clr.l	-(a7)
	pea	HelpKeysText(pc)
	pea	HelpKeyTitle(pc)
	bsr	DlgMessage
	lea	12(a7),a7
	rts

OSqclear:
	move.l	4(a7),a0
	clr.l	(a0)+
	move.w	#2,(a0)+
	clr.w	(a0)+
	rts
	
	