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

BUS_ERROR:
	lea	BusError_str(pc),a0
	bra.s	FATAL_ERROR_SR

SPURIOUS:
	lea	Spurious_str(pc),a0
	bra.s	FATAL_ERROR_SR

ADDRESS_ERROR:
	lea	Address_str(pc),a0
	bra.s	FATAL_ERROR_SR

ILLEGAL_INSTR:
	lea	Illegal_str(pc),a0
	bra.s	FATAL_ERROR_SR

ZERO_DIVIDE:
	lea	Zero_str(pc),a0
	bra.s	FATAL_ERROR_SR

CHK_INSTR:
	lea	Chk_str(pc),a0
	bra.s	FATAL_ERROR_SR

I_TRAPV:
	lea	TrapV_str(pc),a0
	bra.s	FATAL_ERROR_SR

PRIVILEGE:
	lea	Privelege_str(pc),a0
	bra.s	FATAL_ERROR_SR

TRACE:
	lea	Trace_str(pc),a0

; Error recoverable
FATAL_ERROR:
	trap	#12
FATAL_ERROR_SR:
	move.w	#$2700,SR

	move.l	a0,a2
	jsr	SetLCDMEM
	jsr	ScreenClear
	move.w	#USED_FONT,-(a7)
	jsr	FontSetSys	
	clr.w	(a7)
	pea	(a2)
	clr.l	-(a7)
	jsr	DrawStr

	; Wait ~10s
	move.l	#1000000,d0
\Stop_		subq.l	#1,d0
		bne.s	\Stop_
	jmp	OSCont		; A partial reset is possible

; Error not recoverable
SYSTEM_ERROR:
	trap	#12
	move.w	#$2700,SR

	move.l	a0,a2
	jsr	SetLCDMEM
	jsr	ScreenClear
	move.w	#USED_FONT,-(a7)
	jsr	FontSetSys
	clr.w	(a7)
	pea	(a2)
	move.w	#10,-(a7)
	clr.w	-(a7)
	jsr	DrawStr
	pea	SystemError_str(pc)
	clr.l	-(a7)
	jsr	DrawStr
	; Wait ~10s
	move.l	#1000000,d0
\Stop_		subq.l	#1,d0
		bne.s	\Stop_
	jmp	CODE_START		; Reset the calc

LINE_1010:
ER_throw:
	move.w	(sp)+,d1		; Read SR
	move.l	(sp)+,a0		; Read address of crash
	move.w	(a0),d0			; Read Opcode
	lea	Line1010_str(Pc),a0	;
	andi.w	#$0FFF,d0		; Code
	beq	FATAL_ERROR		; dc.w	$A000
	move.w	d1,SR			; Restore SR
	jmp	ER_throwVar_reg		; ER_throw

; If $FFF0, 
;	~ jsr abs.l (Return address +6 / jsr to a1 ->Crash code+2 a1+(a1)
;	Ex:	dc.w	$FFF0 dc.l JumpAdr-*
; If $FFF2,
;	ROM_CALL avec un word.
;	Example: dc.w $FFF2, HeapAlloc*4
LINE_1111:
	move.w	(sp)+,d1		; Get Old SR
	move.l	(sp)+,a0		; Get Address of the 'crash'
	move.w	(a0)+,d0		; We get the instruction and a0 ->next instruction
	subi.w	#$F800,d0		; Is it > $F800 ? 
	bls.s	\no			; No, so it is a crash (First_Window isn't a ROM_CALL)
		move.l	a0,a1		; Jsr/Jmp with a 32 bits offset	
		cmp.w	#$FFF0-$F800,d0
		bne.s	\NoRelJsr
			adda.l	(a0)+,a1	; Get the Sub Routine
			bra.s	\Jump
\NoRelJsr	cmp.w	#$FFF1-$F800,d0
		bne.s	\NoRelJmp
			adda.l	(a0)+,a1	; Get the Sub Routine
			move.w	d1,SR		; Restore SR
			jmp	(a1)		; Jmp with a 32 bits offset
\NoRelJmp	cmp.w	#$FFF2-$F800,d0
		bne.s	\NoBigRomCall
			move.w	(a0)+,d0	; Read Offset
			lsr.w	#2,d0
\NoBigRomCall	move.l	($C8).w,a1	; The address of the rom_call table
		cmp.w	-(a1),d0	; Compare rom_call and number of entries
		bcc.s	\no		; Out of range ? => Crash
			move.w	d0,VAR_SYSTEM1	; For debug purpose
			lsl.w	#2,d0		; * 4
			move.l	2(a1,d0.w),a1	; + ($C8) MAX: 8000 rom_calls
\Jump			move.w	d1,SR		; Restore SR
			pea	(a0)		; Push return address
			jmp	(a1)		; Jump to Rom_call function
\no:	lea	Line1111_str(Pc),a0
	bra	FATAL_ERROR

	
Trap_5:
Trap_6:
Trap_7:
Trap_8:
Trap_10:
Trap_13:
Trap_14:
Trap_15:
	lea	Trap_NotDefined(Pc),a0
	bra	FATAL_ERROR
	
Trap_9:
	lsl.w	#2,d0
	move.l	Trap9_Table(pc,d0.w),a0
	rte
Trap9_Table:
	dc.l	OSContrastUp
	dc.l	WinOpen
	dc.l	OSLinkReset
	dc.l	0		; TIMERV *OSTimerVectors
	dc.l	CONTRAST_VALUE  ;04       (ROM)  ?contrast
 	dc.l	WinStr
	dc.l	KBD_QUEUE	;$6-$1C ?key_buffer
	dc.l	OSqclear	; flush word buffer, set size to 1 word (push *buffer)
	dc.l	0		; table for isupper(), etc.
	dc.l	OSContrastUp	;?contrast_up()
	dc.l	OSContrastDn	; ?contrast_down()
	dc.l	OSClearBreak	;(ROM)  [60001A] = $FF, [05342] = $00
	dc.l	0		;(ROM)  getkey() table
	dc.l	OSCheckBreak	; (ROM)  ?
	dc.l	$4C00		;(RAM)       LCD memory
	dc.l	OSdequeue	;(ROM)  Boolean ?read_word_buffer(WORD *a, BUFFER *b)
	dc.l	0		;(ROM)  RAM test
	dc.l	WinMoveTo
	
Trap_11:
	lea	Trap_NotDefined(Pc),a0
	jsr	ST_helpMsg_reg
	rte
	
Trap_1:	; Interrupt mask
	move.w	(sp),-(sp)		; Push 'old' SR
	and.w	#$0F00,d0		; Filter entry
	and.w	#$F0FF,2(sp)		; Filter Flags
	or.w	d0,2(sp)		; Change SR
	move.w	(sp)+,d0		; Reload Old SR
	rte
	
	; UniOs Deref
Trap_3:
	add.w	a0,a0
	add.w	#HEAP_TABLE/2,a0
	move.l	0(a0,a0.l),a0
	rte
	
; En cas de changement de piles, on reset la calc !
; (Meme si la memoire est saine, le processeur a ete arrete).
; Il faudrait fixer un flag special, sauver TOUS les registres
; Et se demerder au boot pour reprendre apres le trap #4 : pas facile.
Trap_4:
	movem.l	d0-d7/a0-a6,-(sp)	; Save All registers
	
	lea	$600000,a6
	
	; Wait ON Key was released.
	move.w	#$2400,SR
\reset	move.w	#$A00,d1
\loop		st.b	$1A(a6)
		moveq	#$50,d0
		dbra	d0,*
		btst.b	#1,$1A(a6)
		beq.s	\reset
		dbra	d1,\loop
	move.w	#$2700,SR
	
	st.b	$1C(a6)			; Turn Off the RS completly
	clr.w	$14(a6)			; Disable Timer $600017 / Disable Int 3 / Disable Lcd on Hw1
					; Calculate CheckSum ?
	clr.b	$03(a6)			; Set Ram Wait State to the highest value
	bset.b	#5,(a6)			; ????
	move.w	#$380,$18(a6)		; Reset KeyBoard / Trig value
	st.b	$1A(a6)			; ackowlegment of On key
					; Save USP & SSP registers ?
	; Copy in RAM of the ShutDown Function (Since we disabled Flash).
	moveq	#8,d0			; Code
	bsr	IdleRam

	move.b	#$21,$1C(a6)		; Reset RS
	move.w	#$200,$18(a6)		; Reset Key board Trig value and set Battery voltage level to 
	
	; Wait a lot
	move.w	#$8000,d0
\loop1		moveq	#12*4/10,d1
		dbra	d1,*
		dbra	d0,\loop1
	btst.b	#2,(a6)
	bne.s	\all_right
		moveq	#0,d0
		bsr	IdleRam
		stop	#$2700
\all_right
	move.w	#$1B,$14(a6)		; Reset Timers Ints + Lcd Enable on Hw1
	move.b	#$DE,$3(a6)		; Wait States if < 4.0V
	move.w	#$380,$18(a6)		; Battery Voltage Level
	
	jsr	OSLinkReset		; Reset Link 
	jsr	OSClearBreak		; Clear break Key
	jsr	GKeyFlush		; Reset KeyBoard
	bsr	CheckBatt		; Reset Batt
	;pea	(20*100).w
	;move.w	#2,-(sp)		; Reset APD timer ?
	;jsr	OSRegisterTimer
	;addq.l	#6,sp
	jsr	OSContrastSet		; Set contrast
	movem.l	(sp)+,d0-d7/a0-a6
	rte

Trap_0:	; Idle
	move.w	#$2700,SR
	move.w	#$280,$600018
	moveq	#$1E,d0
	bsr.s	IdleRam
	rte	

IdleRam:
	lea	EXEC_RAM,a0
	lea	__offRAM(Pc),a1
	move.w	#(__offRAMEnd-__offRAM)/2-1,d1
\loop		move.w	(a1)+,(a0)+
		dbf	d1,\loop
	jmp	EXEC_RAM

__offRAM:
	move.w	$185E00,d1		; Shut Down Flash Rom
	move.b	d0,$600005		; Shut Down Micro-proc until an int is trigered
	nop
	move.w	d1,$185E00		; Enable Flash Rom
	nop
	nop
	rts
__offRAMEnd
	
	
Trap_12:
	move.w	(sp)+,d0
	rts

Trap_NotDefined:	dc.b	"Trap not defined",0
ReadError_str		dc.b	"Stack overflow / Protected Memory",0
User_str		dc.b	"Abort by user",0
BusError_str		dc.b	"BUS ERROR",0
Spurious_str		dc.b	"SPURIOUS ERROR",0
Address_str		dc.b	"Address error",0
Illegal_str		dc.b	"Illegal instruction",0
Zero_str		dc.b	"Divided by zero",0
Chk_str			dc.b	"Chk instruction",0
TrapV_str		dc.b	"TrapV instruction",0
Privelege_str		dc.b	"Privilege violation",0
Trace_str		dc.b	"Debug mode not available",0
Line1111_str		dc.b	"Line 1111 Emulator",0
Line1010_str		dc.b	"Line 1010 Emulator",0
SystemError_str		dc.b	"SYSTEM ERROR: PedroM is rebooting",0
RomCall_str		dc.b	"Rom call not available",0
	EVEN
