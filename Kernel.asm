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
;***            	Kernel routines				***
;***                                                            ***
;******************************************************************

MAX_HANDLES	EQU	HANDLE_MAX		; Nombre d'handles a sauvegarder
MAX_RAMCALL	EQU	$2A			; Nombre de RAMCALLS
VERSION_OFFSET	EQU	$10	; Offset of the version number
ROM_VECTOR	EQU	ROM_BASE+$012088

RAM_TABLE:
	dc.l	CALCULATOR
	ifnd	TI89
		dc.l	240,128,$400000,30,KEY_LEFT,KEY_RIGHT,KEY_UP,KEY_DOWN,342,345,$2000,3840,$4000
	endif
	ifd	TI89
		dc.l	160,100,$200000,20,KEY_LEFT,KEY_RIGHT,KEY_UP,KEY_DOWN,345,342,$2000,2000,$4000
	endif
	dc.l	MediumFont
	dc.l	RETURN_VALUE_ADDR
	dc.l	TEST_PRESSED_FLAG-$1c
	dc.l	HEAP_PTR
	dc.l	FOLDER_LIST_HANDLE
	dc.l	(FOLDER_LIST_HANDLE+1)
	dc.l	$0130
	dc.l	kernel::idle
	dc.l	kernel::exec
	dc.l	kernel::Ptr2Hd
	dc.l	kernel::Hd2Sym
	dc.l	kernel::LibsBegin
	dc.l	kernel::LibsEnd
	dc.l	kernel::LibsCall
	dc.l	kernel::LibsPtr	
	dc.l	kernel::LibsExec
	dc.l	kernel::HdKeep	
	dc.l	kernel::ExtractFromPack
	dc.l	kernel::ExtractFile	
	dc.l	LCD_MEM 		
	dc.l	MediumFont+$800	; font_small	
	dc.l	MediumFont+$E00	;font_large	
	dc.l	SYM_ENTRY.name	
	dc.l	SYM_ENTRY.compat
	dc.l	SYM_ENTRY.flags	
	dc.l	SYM_ENTRY.hVal	
	dc.l	SYM_ENTRY.sizeof
	dc.l	kernel::ExtractFileFromPack
	
; Hum... To be compatible with Preos
ROM_THROW	MACRO
	bsr	\1
	ENDM
HW2TSR_PATCH	MACRO
		ENDM
kernel_install_int:
	lea	FirstRun,a6		; Ptr to access data 
	rts

start_kernel_prgm:			; 'exec' function (Simpler than Preos one's)
	move.l	(a7)+,a0		; Load program address
	movem.l	d3-d7/a2-a6,-(a7)

	; Load program
	lea	-4(a0),a5		;met l'adresse du programme appelé dans a5
	
	move.l	ERROR_LIST,-(sp)	; save error frame
		
	; Protection ER_catch
	lea	-60(a7),a7		;60 for ErrorFrame
	pea	(a7)
	bsr	ER_catch		; Catch all standard Errors from Ti-Os
	addq.l	#4,a7
	tst.w	d0
	beq.s	\ok_catch		; Normal exit ?
		move.w	d0,-(a7)
		bsr	find_error_message
		addq.w	#2,a7
		move.l	a0,ErrorString
		bra.s	\Error
\ok_catch	
	bsr	kernel::relocation		; Relocation of prog -> a5
	tst.w	d0
	bne.s	\Error
	
	pea	(a5)				; save 'a5' program ref
	moveq	#0,d0
	move.w	12(a5),d0
	beq.s	\no_run
		ifnd	WTI_BP
			jsr	0(a5,d0.l)		; Execution
		endif
		ifd	WTI_BP
			lea	0(a5,d0.l),a0
			SET_WTI_BP a0
			jsr	(a0)
			CLEAR_WTI_BP
		endif
\no_run move.l	(a7)+,a5			; pop 'a5' program ref
		
	bsr	kernel::unrelocation		; Unrelocation du prog -> a5

	clr.l	ErrorString			; No error

\Error	lea	60(a7),a7			; Pop ERROR frame
	move.l	(a7)+,ERROR_LIST		; Restore ERROR list

	bsr	GKeyFlush			; Clear Keys 
	bsr	OSClearBreak			; Clear Break
	jsr	clrscr				; Clear the screen
	; Print Errors in the Help Window
	lea	-50(a7),a7
	move.l	a7,a2
	move.w	KERNEL_EXTRA_NUMBER,-(a7)
	pea	KERNEL_EXTRA_STR
	move.l	ErrorString,-(a7)
	beq.s	\noh
		pea	(a2)
		bsr	sprintf
		bsr	ST_helpMsg
\noh	lea	50(a2),a7
	movem.l (a7)+,d3-d7/a2-a6
	rts

	include	"reloc.asm"
	include	"ext.asm"

; In:
;	d4.w = Src PPG Handle
; Out:
;	d0.w = Asm Handle
ExtractPPG:
	; It is useless to check if the ppg is valid since ttunpack does the job.
	; Calculate length
	move.w	d4,a0
	trap	#3		; Deref PPG file
	addq.l	#2,a0		; Skip the PPG file size
	moveq	#0,d1
	move.w	(a0),d1		; Org size of the compresssed file
	ror.w	#8,d1		; From Big to little endian	
	move.l	d1,-(a7)	; Push size to alloc
	bsr	HeapAllocHigh	; Alloc
	move.l	d0,(a7)		; Save handle and check for null
	beq.s	\End
		lea	Decompressing(pc),a0
		bsr	ST_helpMsg_reg		; Display "Decompressing..."
		move.l	(a7),a0		; Deref the handle
		trap	#3
		pea	(a0)		; Decompress Here
		move.w	d4,a0		; Deref org PPG handle (again !)
		trap	#3
		pea	2(a0)		; Src is here
		bsr	_tt_Decompress	; Decompress it (Old sources but the new ones from Extgraph doesn't allow gamma to be 8 !)
		addq.l	#8,a7		; Pop it
		tst.w	d0
		beq.s	\End
			move.l	(a7),d0		; If failed, free the block
			bsr	HeapFree_reg	
			clr.l	(a7)
\End	bsr	ST_eraseHelp	; Erase the help message
	move.l	(a7)+,d0	; Return the created Handle
	rts
	

Decompressing	dc.b	"decompressing ...",0
mpastrouve	dc.b	"Library not found: %s",0
mwrong		dc.b	"New version needed: %s",0
mmemory		dc.b	"Not enought memory",0
errortext	dc.b	"Crash intercepted",0
wrongrom	dc.b	"%sROMCALL %#03X not supported",0
wrongkernel	dc.b	"%sRAMCALL %#03X not supported",0
parityerror	dc.b	"Illegal stub"
Void_str	dc.b	0
	EVEN
	