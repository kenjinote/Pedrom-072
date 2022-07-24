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

; Scan for the hardware to know what keys are pressed 
Int_1:
	move.w	#$2600,SR
	movem.l	d0-d7/a0-a6,-(a7)
	bsr	KeyScan			; Scan all the keys of the calc
	bsr	UpDateKeyBuffer		; Update the Key Buffer
	tst.w	d4
	beq.s	\NoKey
		jsr	ST_eraseHelp	; Erase the help (A key has been pressed)
		bsr	AddKey		; Add this new key in the buffer.
\NoKey:	movem.l	(a7)+,d0-d7/a0-a6
	rte

	
; Does nothing ....
Int_2:	move.w	#$2700,SR
	move.w	#$00FF,$60001A		; acknowledge Int2
Int_3:	rte				; Clock for int 3 ?
	
; Link Auto-Int
Int_4:
	move.w	#$2700,SR
	movem.l	d0-d2/a0-a2,-(a7)
	lea	$60000C,a1	; DBus Configuration Register
	lea	$F-$C(a1),a2	; Link Byte Buffer
	; Checks if there is enought space in receive buffer before reading any link registers.
	lea	LINK_RECEIVE_QUEUE,a0
	move.w	QUEUE.used(a0),d1		; Check if we have enought space left
	cmp.w	QUEUE.size(a0),d1		; To insert a new byte. Otherwise we do nothing (Do not read the byte to avoid forgetting it).
	blt.s	\NoOverflow
		st.b	LINK_RECEIVE_OVERFLOW	; Modify OSReadLinkBlock too 
		bra	\Exit			; Do not read flags (Fixme: is it right ?)
\NoOverflow	
	move.w	(a1),d2		; Read Status
	btst	#3,d2		; Internal Activity ? ( Autostart ?)
	bne	\Exit
	; Check Link Error...
	btst	#7,d2		; Link Error ?
	beq.s	\NoResetLink
\ResetLink	move.b	#$E0,(a1)	; Reset link: AutoStart Enable, Link Disable, Link TimeOut Disable
		move.b	#$8D,(a1)	; Trigger int4 if Control Link Error, Control Autostart, Byte in Receive Buffer. AutoStart enable, Link Enable, Link TimeOut Enable
		st.b	LINK_RESET	; Link is reseted (FIXME: Needs purging Buffer ?)
		bra.s	\Exit
\NoResetLink:
	; Check Receive Buffer...
	btst	#5,d2					; Byte in receive Buffer ?
	beq.s	\NoReceiveByte
		move.b	(a2),d0				; Read Byte from Receieve Buffer
		addq.w	#1,QUEUE.used(a0)		; One more Byte in QUEUE
		move.w	QUEUE.head(a0),d1		; Read current writting offset
		move.b	d0,QUEUE.data(a0,d1.w)		; Write Byte
		addq.w	#1,d1				; Next current writting offset
		cmp.w	QUEUE.size(a0),d1		; Check from Max
		blt	\NoZero
			clr.w	d1
\NoZero		move.w	d1,QUEUE.head(a0)
		bra.s	\Exit
\NoReceiveByte:
	btst	#9,d2		; Check if the int is triggered if Transmit Buffer is empty
	beq.s	\Exit
		lea	LINK_SEND_QUEUE,a0
\SendByte:
		tst.w	QUEUE.used(a0)			; Have we sent all the bytes ?
		beq.s	\DoNotTriggerIntForSend		; Yes so stop sending bytes
		move.w	(a1),d2
		btst	#6,d2				; Is tramsit buffer empty ?
		beq.s	\Exit				; No so exit
		move.w	QUEUE.head(a0),d1
		move.b	QUEUE.data(a0,d1.w),d0		; Read data
\Wait			move.w	(a1),d2
			btst	#7,d2
			bne.s	\ResetLink		; Check if we reset link
			btst	#6,d2			; Transmit Buffer empty ?
			beq.s	\Wait
		move.b	d0,(a2)				; Write byte in transmit buffer
		subq.w	#1,QUEUE.used(a0)		; One byte sent
		addq.w	#1,d1				; Next Offset
		cmp.w	QUEUE.size(a0),d1
		blt	\NoZero2
			clr.w	d1
\NoZero2	move.w	d1,QUEUE.head(a0)		; Save new offset
		bra.s	\SendByte
\DoNotTriggerIntForSend
	bclr	#1,(a1)		; Do not triggered Int4 if Send Buffer is empty
\Exit	movem.l	(a7)+,d0-d2/a0-a2
	rte	



; Auto-Ints which allows the system timers.
Int_5:
	movem.l	d0-d7/a0-a6,-(sp)	
	lea	TIMER_TABLE,a5				; Timer table
	moveq	#TIMER_NUMBER-1,d7			; Number of timers
\timer_loop:
		move.b	TIMER_TYPE(a5),d6		; Get type of timer
		beq.s	\next				; If type ==0, this timer was freed.
		move.l	TIMER_CUR_VAL(a5),d0		; Get timer value
		beq.s	\next				; If =0, stop.
			subq.l	#1,d0			; Decremente timer
			move.l	d0,TIMER_CUR_VAL(a5)	; and save the new value
			bne.s	\next			; Check the end of the timer 
				subq.b	#TIMER_TYPE_COUNT,d6	; Check if callback timer
				beq.s	\next		
					move.l	TIMER_RESET_VAL(a5),TIMER_CUR_VAL(a5)	; Reset timer
					move.l	TIMER_CALLBACK(a5),a0			; Call the callback.
					jsr	(a0)
\next:		lea	TIMER_SIZE(a5),a5		; Next timer
		dbf	d7,\timer_loop			
	movem.l	(sp)+,d0-d7/a0-a6		
	rte

; ON Int.
;	2ND / DIAMOND : Off
;	ESC : Reset
Int_6:
	movem.l	d0/a0-a1,-(sp)		; Save d0/a0
					; Test if ESC is pressed
	btst.b	#1,$60001A		; Test if ON key if effectively pressed
	bne.s	\end			; ON key is not pressed

	lea	User_str(Pc),a0		; Abort by user
	lea	$600018,a1		; It doesn't save org port. I think it is useless.
	move.w	#KEY_ESC_ROW,(a1)	; Write mask (int1 & 5 can't be called ;)
	moveq	#$58,d0			; $58
		dbra d0,*		; (Waits)
	btst	#KEY_ESC_COL,$1B-$18(a1) ; Read Key Matrix (ESC key)
	beq	FATAL_ERROR		; Yes => Crash handler
	move.w	#$380,(a1)		; Reset Keyboard

	move.w	KEY_STATUS,d0		; Check 2nd or diamond
	cmp.w	#KEY_2ND,d0
	beq.s	\Off
	cmp.w	#KEY_DIAMOND,d0
	bne.s	\NoOff
\Off		clr.w	KEY_STATUS	; Erase 2nd/diamond flag
		clr.b	KEY_MAJ		; Clear Majusucule Key
		clr.w	-(a7)
		jsr	ST_modKey
		addq.l	#2,a7
		trap	#4
		bra.s	\end
\NoOff:	
	; Break Flag
	tst.b	ENABLE_BREAK_KEY
	beq.s	\no_set
		st.b	BREAK_KEY
\no_set:

\end:	movem.l	(a7)+,d0/a0-a1
	rte

Int_7:
	lea	ReadError_str(Pc),a0
	bra	FATAL_ERROR
		
; SUB FUNCTIONS
; Check the batteries level.	
CheckBatt:
	movem.l	d1-d7/a0-a6,-(a7)
	move.w	#$2500,SR

	lea	$600018,a0
	lea	$70001C,a3
	lea	BattTable2(Pc),a2
	cmpi.b	#1,HW_VERSION
	beq.s	\ok
		lea	BattTable1(Pc),a2
\ok	
	move.w	#$F,(A3)

	moveq	#2,d2
\loop0
		move.w	#$380,(A0)
		moveq	#$52,d0
		bsr.s	CheckBattIO
		move.w	d2,d0
		add.w	d0,d0
		move.w	0(a2,d0.w),(a0)
		moveq	#$6E,d0
		bsr.s	CheckBattIO
		bne.s	\stop
		dbf	d2,\loop0	
\stop:	
	addq.w	#1,d2
	move.w	#7,(a3)
	move.w	#$380,(A0)
	move.w	#$52,d0
	bsr.s	CheckBattIO

	move.w	#6,(a3)
	st.b	d0
	cmpi.b	#1,HW_VERSION
	bne	\end
		move.b	BattWaitStateLevel(Pc,d2.w),d0
\end:	move.b	d0,$600003
	move.w	d2,d0
	move.b	d0,BATT_LEVEL
	movem.l	(a7)+,d1-d7/a0-a6
	rts

CheckBattIO:
\loop3		btst.b	#2,$600000
		dbne	d0,\loop3
	rts

BattWaitStateLevel:
	dc.b	$CD,$DE,$EF,$FF
	
BattTable1:	dc.w	$0200,$0180,$0100
BattTable2:	dc.w	$0200,$0100,$0000
	
_WaitKeyBoard:
	moveq	#$58,d0
	dbf	d0,*
	rts

; In: 
;	Nothing
; Out:
;	d4.w = Key
; Destroy:
;	All !
KeyScan:
	lea	$600018,a0
	lea	$1B-$18(a0),a1
	lea	KEY_MASK,a2
	
	; check if a Key is pressed
	clr.w	(a0)			; Read All Keys
	bsr.s	_WaitKeyBoard
	move.b	(a1),d0
	not.b	d0
	beq.s	\NoKey
	; A key is pressed. Check for it.
	; Check which key is pressed
	clr.w	d4
	moveq	#KEY_NBR_ROW-1,d1
	move.w	#KEY_INIT_MASK,d2
\key_loop:
		move.w	d2,(a0)			; Select Row
		bsr.s	_WaitKeyBoard
		move.b	(a1),d3			; Read which Keys is pressed
		move.b	d3,d0			; add the Key Mask
		or.b	(a2),d3			; Clear Some Keys according to the mask
		not.b	d0			; UpDate the mask 
		and.b	d0,(a2)+		; 
		not.b	d3
		beq.s	\next			; A key is pressed
			moveq	#7,d0
\bit_loop:			btst	d0,d3
				dbne	d0,\bit_loop
			tst.w	d4		; A key has been already pressed ?
			bne.s	\next
				bset	d0,-1(a2)	; Update Mask so that this key won't be add once more
				move.w	d1,d4
				lsl.w	#3,d4
				add.w	d0,d4
				add.w	d4,d4
				move.w	Translate_Key_Table(Pc,d4.w),d4
				move.w	d4,KEY_PREVIOUS
				move.w	d2,KEY_CUR_ROW	; Memorize which key is currently pressed
				move.w	d0,KEY_CUR_COL
				move.w	KEY_ORG_START_CPT,KEY_CPT	; Start Delay before repeat it
\next:		ror.w	#1,d2
		dbf	d1,\key_loop		
	; Auto Repeat Feature FIXME: It seems not to work well. Why ?
	tst.w	d4		; Is a key pressed ?
	bne.s	\end		; Yes do not read the previous key.
		cmp.w	#$1000,KEY_PREVIOUS	; No repeat feature for statut keys
		bge.s	\none
		; No so check is previous key is still pressed.
		move.w	KEY_CUR_ROW,(a0)	; Select Row
		bsr.s	_WaitKeyBoard
		move.b	(a1),d3			; Read which Keys is pressed
		move.w	KEY_CUR_COL,d0
		btst	d0,d3			; Previous Key is not pressed
		bne.s	\ResetStatutKeys
			subq.w	#1,KEY_CPT	; Dec cpt.
			bne.s	\end
				move.w	KEY_ORG_REPEAT_CPT,KEY_CPT
				move.w	KEY_PREVIOUS,d4
				bra.s	\end
\NoKey:	
	clr.l	(a2)+		; Reset KEY_MASK
	clr.l	(a2)+
	clr.w	(a2)+
\none:	clr.w	d4
\end:	move.w	#$380,(a0)	; Reset to standard Key Reading.
	rts
\ResetStatutKeys:
		and.b	#RESET_KEY_STATUS_MASK,KEY_MASK+KEY_NBR_ROW-1		; Clear Mask
		bra.s	\end
	
	ifnd	TI89		; For TI-92+ and V200
Translate_Key_Table:
	dc.w	KEY_2ND,KEY_DIAMOND,KEY_SHIFT,KEY_HAND,KEY_LEFT,KEY_UP,KEY_RIGHT,KEY_DOWN
	dc.w	KEY_VOID,'z','s','w',KEY_F8,'1','2','3'
	dc.w	KEY_VOID,'x','d','e',KEY_F3,'4','5','6'
	dc.w	KEY_STO,'c','f','r',KEY_F7,'7','8','9'
	dc.w	' ','v','g','t',KEY_F2,'(',')',','
	dc.w	'/','b','h','y',KEY_F6,KEY_SIN,KEY_COS,KEY_TAN
	dc.w	'^','n','j','u',KEY_F1,KEY_LN,KEY_ENTER,'p'
	dc.w	'=','m','k','i',KEY_F5,KEY_CLEAR,KEY_APPS,'*'
	dc.w	KEY_BACK,KEY_THETA,'l','o','+',KEY_MODE,KEY_ESC,KEY_VOID
	dc.w	'-',KEY_ENTER,'a','q',KEY_F4,'0','.',KEY_SIGN
	endif
	ifd	TI89		; For TI-89
Translate_Key_Table:
	dc.w	KEY_UP,KEY_LEFT,KEY_DOWN,KEY_RIGHT,KEY_2ND,KEY_SHIFT,KEY_DIAMOND,KEY_ALPHA
	dc.w	KEY_ENTER,'+','-','*','/','^',KEY_CLEAR,KEY_F5
	dc.w	KEY_SIGN,'3','6','9',',','t',KEY_BACK,KEY_F4
	dc.w	'.','2','5','8',')','z',KEY_CATALOG,KEY_F3
	dc.w	'0','1','4','7','(','y',KEY_MODE,KEY_F2
	dc.w	KEY_APPS,KEY_STO,KEY_EE,KEY_OR,'=','x',KEY_HOME,KEY_F1
	dc.w	KEY_ESC,KEY_VOID,KEY_VOID,KEY_VOID,KEY_VOID,KEY_VOID,KEY_VOID,KEY_VOID	
	endif
	
	; UpDate the Key buffer (FIFO Buffer)
UpDateKeyBuffer:
	move.w	KEY_CUR_POS,d3
	beq.s	\no_read_of_current_key		; No Key in Buffer
	tst.w	TEST_PRESSED_FLAG		; Key has not been readen by apps.
	bne.s	\no_read_of_current_key
		; Move Key Buffer : remove the last Key
		clr.w	d0
		lea	GETKEY_CODE,a3
		subq.w	#1,d3			; Remove a key from Buffer
		move.w	d3,KEY_CUR_POS		; Save new value
		beq.s	\D			; = 0 ?
			moveq	#2,d0		; No, so a key is in the buffer
\D:		move.w	d0,TEST_PRESSED_FLAG	; 2 so that OSdqueue(kbd_queue) works fine.
\loop:			move.w	2(a3),(a3)+
			dbf	d3,\loop
\no_read_of_current_key:
	rts
	
; Add a key in the keyboard FIFO buffer.
;	KEY_2ND, KEY_SHIFT, KEY_DIAMOND and KEY_ALPHA are treated in a special way.
; In:
;	In d4.w = code (<> 0 !)
; This code MUST work :
;	tst.w	KEY_PRESSED_FLAG	; has a key been pressed?
;	beq	wait_idle
;	move.w	GETKEY_CODE,d0
;	clr.w	KEY_PRESSED_FLAG	; clear key buffer
AddKey:
	move.w	KEY_STATUS,d3		; Read Statut Key
	cmp.w	#$1000,d4		; Is it a normal key or an option key ?
	bcs.s	\normal_key
		ifd	TI89
		clr.b	d1
		cmp.w	#KEY_ALPHA,d4
		bne.s	\NoAlphaStatKey
			; Check Alpha-Alpha Combo 
			cmp.w	#KEY_ALPHA,d3
			bne.s	\NoAlphaAlphaCombo
				moveq	#1,d1
				clr.w	d4
\NoAlphaAlphaCombo	; Check Shift-Alpha Combo 
			cmp.w	#KEY_SHIFT,d3
			bne.s	\NoShiftAlphaCombo
				moveq	#2,d1
				clr.w	d4				
\NoShiftAlphaCombo
\NoAlphaStatKey
		move.b	d1,KEY_MAJ
		endif
		cmp.w	d3,d4	; Option Key: update the statut.
		bne.s	\Ok
			clr.w	d4	; Erase the statut if we pressed twice the same option key.
\Ok:		move.w	d4,KEY_STATUS	; Re KeyScan ?
		rts
\normal_key:
	cmpi.w	#KEY_SHIFT,d3		; Shift Key
	beq.s	\shift	
	cmpi.w	#KEY_DIAMOND,d3		; Diamond Key
	beq.s	\diamond
	ifd	TI89
	cmpi.w	#KEY_ALPHA,d3		; Ti89 only : Alpha Keys
	beq.s	\alpha	
	endif
	cmpi.w	#KEY_2ND,d3		; 2nd Key
	bne.s	\normal
	
\2nd:
	ifnd	TI89
	cmpi.w	#'z',d4			; 2nd + Z only alvailable on 92+/v200
	bne.s	\no_exg
		not.b	KEY_MAJ		; 2nd + Z
		bra.s	\overflow
\no_exg	
	endif
	lea	Translate_2nd(pc),a0	; Translate 2nd Keys
\Loop2nd	move.w	(a0)+,d0
		beq.s	\extended
		addq.l	#2,a0
		cmp.w	d0,d4
		bne.s	\Loop2nd
	move.w	-(a0),d4
	bra.s	\add_key

	ifd	TI89
\alpha	bsr	TranslateAlphaKey	; Translate Alpha Key
	bra.s	\add_key
	endif

\diamond:
	; Test '+' / '-'
	cmpi.w	#'+',d4
	bne.s	\NoContrastUp	
		jmp	OSContrastUp
\NoContrastUp
	cmpi.w	#'-',d4
	bne.s	\extended
		jmp	OSContrastDn
\extended				; Only or KEY_STATUS and KEY
	or.w	d3,d4
	bra.s	\add_key

\shift:					; SHIFT called
	ifd	TI89
	bsr	TranslateAlphaKey	; Translate alpha Key
	endif
	cmpi.w	#127,d4			; Check if range Ok.
	bhi.s	\extended		; No so extended
	bra.s	\MAJ			; Go to upper case
	
\normal:				; Normal Key
	move.b	KEY_MAJ,d1		
	beq.s	\add_key
	ifd	TI89
		bsr.s	TranslateAlphaKey
		subq.b	#1,d1
		beq.s	\add_key
	endif
\MAJ:	
	cmpi.w	#'a',d4
	bcs.s	\add_key
	cmpi.w	#'z',d4
	bhi.s	\add_key
		addi.w	#'A'-'a',d4

\add_key:
	bsr.s	AddKeyToFIFOKeyBuffer	
\overflow:
	clr.w	KEY_STATUS				; Clear statut
	rts

	ifd	TI89
TranslateAlphaKey:
	lea	Translate_Alpha(pc),a0
\LoopAlpha	move.b	(a0)+,d0
		beq.s	\end
		addq.l	#1,a0
		cmp.b	d0,d4
		bne.s	\LoopAlpha
	clr.w	d4
	move.b	-(a0),d4
\end	rts
	endif

; In : d4.w	
AddKeyToFIFOKeyBuffer:
	move.w	KEY_CUR_POS,d3				; Current position in Buffer
	cmpi.w	#KEY_MAX,d3				; Max size of buffer
	bcc.s	\overflow
		lea	GETKEY_CODE,a3			; Ptr to buffer
		adda.w	d3,a3				; d3*2
		move.w	d4,0(a3,d3.w)			; Write it to buffer
		addq.w	#1,d3				; One more
		move.w	d3,KEY_CUR_POS			; Save new position
		move.w	#2,TEST_PRESSED_FLAG		; A key has been pressed
\overflow:
	rts
	
;	First Key is the source and then the new key
	ifnd	TI89
Translate_2nd:
	dc.w	'q','?'
	dc.w	'w','!'
	dc.w	'e','é'
	dc.w	'r','@'
	dc.w	't','#'
	dc.w	'y',26
	dc.w	'u',252
	dc.w	'i',151
	dc.w	'o',212
	dc.w	'p','_'
	dc.w	'a','à'
	dc.w	's',129
	dc.w	'd',176
	dc.w	'f',159
	dc.w	'g',128
	dc.w	'h','&'
	dc.w	'j',190
	dc.w	'k','|'
	dc.w	'l','"'
	dc.w	'x',169
	dc.w	'c',199
	dc.w	'v',157
	dc.w	'b',39
	dc.w	'n',241
	dc.w	'm',';'
	dc.w	'=','\'
	dc.w	KEY_THETA,':'
	dc.w	'(','{'
	dc.w	')','}'
	dc.w	',','['
	dc.w	'/',']'
	dc.w	'^',140
	dc.w	'7',189
	dc.w	'8',188
	dc.w	'9',180
	dc.w	'*',168
	dc.w	'4',142
	dc.w	'5',KEY_MATH	;171
	dc.w	'6',KEY_MEM	;187
	dc.w	'-',KEY_VARLINK	;143
	dc.w	'1',KEY_EE	;149
	dc.w	'2',KEY_CATALOG	;130
	dc.w	'3',KEY_CUSTOM	;131
	dc.w	'+',KEY_CHAR	;132
	dc.w	'0','<'
	dc.w	'.','>'
	dc.w	KEY_SIGN,KEY_ANS	;170
	dc.w	KEY_BACK,KEY_INS
	dc.w	KEY_ENTER,KEY_ENTRY
	dc.w	KEY_APPS,KEY_SWITCH
	dc.w	KEY_ESC,KEY_QUIT
	dc.w	KEY_STO,KEY_RCL
	dc.w	' ','$'
	dc.w	0
	endif
	ifd	TI89
Translate_2nd:
	dc.w	KEY_F1,KEY_F6
	dc.w	KEY_F2,KEY_F7
	dc.w	KEY_F3,KEY_F8
	dc.w	KEY_ESC,KEY_QUIT
	dc.w	KEY_APPS,KEY_SWITCH
	dc.w	KEY_HOME,KEY_CUSTOM
	dc.w	KEY_MODE,26
	dc.w	KEY_CATALOG,151
	dc.w	KEY_BACK,KEY_INS
	dc.w	KEY_CLEAR,'$'
	dc.w	'x',KEY_LN
	dc.w	'y',KEY_SIN
	dc.w	'z',KEY_COS
	dc.w	't',KEY_TAN
	dc.w	'^',128+12
	dc.w	'=',39
	dc.w	'(','{'
	dc.w	')','}'
	dc.w	',','['
	dc.w	'/',']'
	dc.w	'|',176
	dc.w	'7',176+13
	dc.w	'8',176+12
	dc.w	'9',';'
	dc.w	'*',168
	dc.w	KEY_EE,159
	dc.w	'4',':'
	dc.w	'5',KEY_MATH
	dc.w	'6',KEY_MEM
	dc.w	'-',KEY_VARLINK
	dc.w	KEY_STO,KEY_RCL
	dc.w	'1','"'
	dc.w	'2','\'
	dc.w	'3',KEY_UNITS
	dc.w	'+',KEY_CHAR
	dc.w	'0','<'	
	dc.w	'.','>'
	dc.w	KEY_SIGN,KEY_ANS
	dc.w	KEY_ENTER,KEY_ENTRY
	dc.w	0
Translate_Alpha:
	dc.b	'=','a'
	dc.b	'(','b'
	dc.b	')','c'
	dc.b	',','d'
	dc.b	'/','e'
	dc.b	'|','f'
	dc.b	'7','g'
	dc.b	'8','h'
	dc.b	'9','i'
	dc.b	'*','j'
	dc.b	KEY_EE,'k'
	dc.b	'4','l'
	dc.b	'5','m'
	dc.b	'6','n'
	dc.b	'-','o'
	dc.b	KEY_STO,'p'
	dc.b	'1','q'
	dc.b	'2','r'
	dc.b	'3','s'
	dc.b	'+','u'
	dc.b	'0','v'
	dc.b	'.','w'
	dc.b	KEY_SIGN,' '
	dc.b	'x','x'
	dc.b	'y','y'
	dc.b	'z','z'
	dc.b	't','t'	
	dc.b	0
	endif
	