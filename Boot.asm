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
; 		Boot code : setup IO ports
; ***************************************************************

	; Init the calc
	bclr.b	#1,$70001D		; Disable Screen on HW2 (on AMS it is the first instruction. Why ?).
	move.w	#$2700,SR		; Prevent Auto Ints
	lea	($4C00).w,sp		; Setup Supervisor Stack 
	moveq	#0,d0
	
	; Setup IO ports
	lea	$600000,a5		; Port IO 6
	lea	$700000,a6		; Port IO 7
	
	move.w	#$40,$10(a6)		; Link Speed on HW2

	move.b	d0,$15(a5)		; OSC2 and LCD mem stopped on HW1 /* do not clr.b since it reads before set */

	move.w	#$8000,(a5)		; Pin 100 Enable (92 only) / Write Protect Vectors desable / RAM 256K 
	move.w	#$FFFF,$18(a5)		; Setup Batt Level to the max 
	move.w	#$380,d1		; Wait for Hardware answer
	dbf	d1,*
	btst.b	#$2,(a5)
	beq.s	\voltage_below
		move.b	d0,(a5)		; Do not set Pin100 
\voltage_below
	
	; Unprotect access to special IO ports
	lea	$1C5EA4,a0
	nop
	nop
	nop
	move.w	#$2700,SR
	move.w	d0,(a0)
	nop
	nop
	nop
	move.w	#$2700,SR
	move.w	d0,(a0)
	
	; Can not registers to access IO ports <= we have disabled the hardware protection, so a hack may be used.
	; Set Protected IO ports
	ori.b	#4+2+1,$70001F		; HW2: ??? / Enable OSC2 / 5 contrasts bits.

	moveq	#0,d0			; d0.l = 0 (Even if d0=0, I must set it again <= AntiHack).
	move.w	#$003F,$700012		; Allow Execution of all Flash ROM on HW2 (FIXME: Right value ?)
	move.w	d0,$45E00		; Allow Execution of all Flash ROM on HW1
	move.w	d0,$85E00
	move.w	d0,$C5E00
	
	; Allow Execution of all RAM on HW2	/* clr.l $700000 : This instruction is potentially dangerous since we reed it before setting it ! */
	move.l	d0,$700000
	move.l	d0,$700004
	
	; Protect access to special IO ports
	nop
	nop
	nop
	move.w	#$2700,SR
	move.w	(a0),d0
	
	; Setup IO ports
	moveq	#0,d0
	move.w	d0,$2(a5)		; Setup Bus Wait States (Very slow).
	move.w	d0,$C(a5)		; Setup Link Port (Nothing enable link ports)
	move.w	#$4C00/8,$10(a5)	; Set LCD memory address on HW1
	move.w	#$3180,$12(a5)		; Set LCD logical width / Set LCD logical height
	move.w	#$001B,$14(a5)		; Increment rate of $600017 : OSC2/2^9, / Enable $600017 / Disable Int 3 / Enable OSC2 / Enable LCd on HW1
	bset.b	#1,$1D(a6)		; Enable Screen on HW2
	move.w	#$B2,$16(a5)		; Reset $600017 cycles on HW1.
	move.w	d0,$1A(a5)		; acknowledge AutoInt 6 & AutoInt 2
	move.b	#$21,$1C(a5)		; Set LCD RS

	; Clear 256K of RAM
	suba.l	a0,a0
	moveq	#-1,d0			; 256K / 4 = 64K-1 = $FFFF
\ClearRAM_loop:
		clr.l	(a0)+
		dbf	d0,\ClearRAM_loop
		
	; Copy Vector Table
	lea	VECTORS_TABLE(PC),a0
	suba.l	a1,a1
	moveq	#$3F,d0
\VECTOR_loop	move.l	(a0)+,(a1)+
		dbf	d0,\VECTOR_loop
	ori.w	#$4,(a5)		; Protect Vector Table
	
	; Check ON-Key : if it is pressed, do not run the start script
	btst.b	#1,$60001A			; Test if ON key if pressed
	sne	RUN_START_SCRIPT		; ON key is pressed : Do not start 'start' script.

	; Check the range of the system
	; (Use only on Emulator)
	lea	OSTooBig_str(pc),a0		
	move.l	#BASE_END,a1			; Check if the system 
	cmp.l	#START_ARCHIVE-1,a1		; Doesnot overflow
	bhi	BOOT_ERROR
	move.l	#BASE1_END,a1
	cmp.l	#ROM_BASE+$18000-1,a1
	bhi	BOOT_ERROR
	
; ***************************************************************
; 		Init the Operating System
; ***************************************************************
OSStart:
	bsr	HeapInit			; Init Handles
	bsr	VATInit				; Init VAT
	bsr	EStackInit			; Init the EStack
	bsr	FlashInit			; Init the archive (Add the archived files in the VAT).
	lea	StdLib_sym(pc),a2
	lea	StdLib_FILE,a3
	bsr	VATAddSpecialFile		; Add 'stdlib' to the system.
	bra.s	OSCont2
OSCont:	; Reset OS without reseting the Heap and the VAT
	clr.b	RUN_START_SCRIPT		; Do not run the 'start' script
OSCont2	lea	($4C00).w,sp			; Setup Supervisor Stack 
	lea	($4400).w,a0
	move.l	a0,usp				; Setup User Stack
	move.l	a7,a6				; Save Stack Ptr

	; Copy org Vectors
	lea	VECTORS_TABLE(PC),a0
	lea	$40000,a1			; GHOST SPACE (To avoid unprotection)
	moveq	#$3F,d0
\VECTOR_loop	move.l	(a0)+,(a1)+
		dbf	d0,\VECTOR_loop

	bsr	FL_getHardwareParmBlock		; Get the Hardware Parm Block
	move.w	(a0)+,d1			; Read and skip size
	move.l	(a0)+,d2			; Read Calculator ( 1 : 92+ / 3 : 89 / V200 : 8)
	move.l	(a0)+,d5			; Read Hardware Revision version
	move.w	#$B2,$600016			; Reset $600017 cycles on HW1.
	moveq	#1,d3				; HW_VERSION = 1
	cmpi.w	#$16,d1				; Gate Array field to
	bls.s	\hw1				; see which hardware it is.
		move.l	($16-2-4-4)(a0),d3	; Read HW_VERSION (Even on EMULATOR, HW_VERSION =1, since the default is 1, not 2 !)
		move.w	#$CC,$600016		; Reset $600017 cycles on HW2.
\hw1	
	lea	WrongCalc_str(pc),a0
	subq.l	#CALC_BOOT_TYPE,d2		; Check if ROM and calc are compatible
	bne	BOOT_ERROR			; System Error

	; Detection of Emulator ? (Change calc / hw_version)
	clr.b	d4				; Emulator = False
	IS_WTI.s	\emu
	moveq	#-97,d1				; Vti detection by JM
	nbcd	d1
	bmi.s	\real_calc
\emu:		st.b	d4			; EMULATOR = TRUE
\real_calc

	; Save Kernel values
	lea	CALCULATOR,a0
	move.b	#CALC_KERNEL_TYPE,(a0)+		; Detec CALCULATOR
	move.b	d3,(a0)+			; Detect HW version
	move.b	d5,(a0)+			; Detect HW revision version
	move.b	d4,(a0)+			; Emulator
	
	clr.l	ERROR_LIST			; Init ER_throw
	
	; Init Timers
	clr.l	-(a7)				; Timer 0
	moveq	#TIMER_NUMBER-1,d3
\timer_loop	addq.w	#1,(a7)			; Timer++
		bsr	OSFreeTimer
		dbf	d3,\timer_loop
	; Register Batt Timer
	pea	CheckBatt
	move.l	#20*5,-(a7)			; 5s
	move.w	#1,-(a7)			; Batt Timer
	bsr	OSVRegisterTimer		; Every 5s we check the BATTS
	; Register APD Timer
	move.l	#20*100,(a7)			; 100s
	move.w	#2,-(a7)			; APD Timer
	bsr	OSRegisterTimer			; We put the calc off after 100s
	; Register LIO Timer
	move.l	#20*5,(a7)			; 20s
	move.w	#3,-(a7)			; LIO Timer
	bsr	OSRegisterTimer			; Max wait for the link functions

	; Init Cursor: Register Cursor Timer
	pea	CU_Interrupt
	move.l	#15,-(a7)			; .75s
	move.w	#4,-(a7)			; CURSOR
	bsr	OSVRegisterTimer		; Clignotement speed
	jsr	CU_stop
	
	; Init Contrast
	moveq	#$0F,d0				; Max value on HW1
	cmpi.b	#1,HW_VERSION
	bls.s	\ok
		moveq	#$1F,d0			; Max value on HW2
\ok:	move.b	d0,CONTRAST_MAX_VALUE
	lsr.b	#1,d0
	move.b	d0,CONTRAST_VALUE
	jsr	OSContrastUp

	; Init Key System
	clr.w	TEST_PRESSED_FLAG
	clr.w	KEY_CUR_POS
	clr.b	KEY_MAJ
	clr.w	KEY_STATUS
	move.w	#50,(a7)
	bsr	OSInitBetweenKeyDelay
	move.w	#130,(a7)
	bsr	OSInitKeyInitDelay
	pea	KBD_QUEUE
	bsr	OSqclear
	bsr	OSEnableBreak			; Allow Break

	; Init Graph System
	bsr	SetLCDMEM
	bsr	ScreenClear
	clr.w	(a7)
	bsr	FontSetSys
	bsr	SetCurAttr
	move.l	ScrRect(PC),ScrRectRam
	pea	ScrRect(PC)
	bsr	SetCurClip
	
	; Init Window System
	lea	DeskTopWindow,a0			
	move.l	a0,DeskTop			; Set the DeskTop ptr
	clr.w	WINDOW.Flags(a0)		; Set flags
	move.b	#1,WINDOW.CurAttr(a0)		; Set Attr
	move.b	#1,WINDOW.CurFont(a0)		; Set Font
	jsr	WinHome_reg			; Set X,Y
	clr.w	WINDOW.Background(a0)		; Set BackGround
	clr.w	WINDOW.DupScr(a0)		; Set Save Screen
	move.l	#239*256+120,d0			
	move.l	d0,WINDOW.Window(a0)		; Set Window
	move.l	d0,WINDOW.Client(a0)		; client
	move.l	d0,WINDOW.Clip(a0)		; & clip
	move.l	#LCD_MEM,WINDOW.Screen(a0)	; Set duplicate (Well it is LCD ;))
	clr.l	WINDOW.Next(a0)			; No next window
	move.l	a0,FirstWindow			; Add it in the list
	
	; Init Link System
	bsr	OSLinkReset			; Reset the link
	
	; ReInit Heap
	bsr	HeapCheck			; Check Heap
	bsr	HeapCompress			; Compress the Heap

	; ReInit EStack
	bsr	EStackReInit
	
	; ReInit Side
	jsr	init_side

	; Check ON Key : Do a system reset if it is pressed ?
	btst.b	#1,$60001A			; Test if ON key if pressed
	beq	CODE_START			; ON key is pressed : Reset the calc.
	
	move.l	a6,a7				; Fix stack
	move.w	#$0000,SR			; Start System : User Mode
	
	bra	ShellCommand			; Go to the Command Shell

BOOT_ERROR:
	jmp	SYSTEM_ERROR			; Error while booting ! System Error !
	