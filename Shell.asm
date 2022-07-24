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
;***            	Shell commands				***
;***                                                            ***
;******************************************************************

	
; Match if the 2 strings may be identical !
; In :
;	a0 -> String 1
;	a1 -> String 2
; Out:
;	d0.b = 0 if there are identical
; Note: 
;	String2 may allow wildcars '*' and '?'
Match:
	move.b	(a1)+,d1
	beq.s	\End
	cmpi.b	#'*',d1
	beq.s	\DoMany
	cmpi.b	#'?',d1
	beq.s	\Skip
	move.b	(a0)+,d0
	beq.s	\Error
	cmp.b	d0,d1
	beq.s	Match
\Error	moveq	#1,d0
	rts
\Skip	addq.l	#1,a0		; Skip this character
	bra.s	Match
\End:	move.b	(a0),d0		; If a0 is null, we succeed, else we failed
	rts
\Error2	move.b	d1,d0		; Is is the joker was '0'
	rts			; Yes, so it isn't an error, we succeed !
\DoMany	move.b	(a1)+,d1	; Stop char
\Loop		move.b	(a0)+,d0	; We loop until we found
		beq.s	\Error2		; Else the end of the string 1
		cmp.b	d0,d1		; else the stop character
		bne.s	\Loop
	movem.l	d1/a0/a1,-(a7)	; We scan recursevly from this position 
	bsr	Match		; to check the motif
	movem.l	(a7)+,d1/a0/a1	
	tst.b	d0		; If it failed, we go back, and skip this one.
	bne.s	\Loop
	rts

; Display 'Confirm' and input a string from he user.
; Is the string is 'yes', it returns -1 else 0
Confirm:
	pea	Confirmation_str(pc)
	bsr	printf
	subq.l	#4,a7
	move.l	a7,a0
	moveq	#6,d3
	bsr	Input_String
	clr.w	d0
	cmp.b	#'y',(a0)+
	bne.s	\exit
	cmp.b	#'e',(a0)+
	bne.s	\exit
	cmp.b	#'s',(a0)+
	bne.s	\exit
	tst.b	(a0)+
	seq.b	d0
\exit	addq.l	#8,a7
	rts
	
; Put a string on the screen
; In:
;	a0 -> String
;	d0.w = x
;	d1.w = y
; Out :
;	nothing
; Destroy:
;	nothing
Display_String:
	movem.l	d0-d2/a0-a1,-(a7)
	move.w	#4,-(a7)
	pea	(a0)
	move.w	d1,-(a7)
	move.w	d0,-(a7)
	bsr	DrawStr
	lea     10(a7),a7
	movem.l	(a7)+,d0-d2/a0-a1
	rts

GetCursorKey:
	jsr	CU_start
	bsr	GetKey
	move.w	d0,-(a7)
	jsr	CU_stop
	move.w	(a7)+,d0
	rts
	
; Input a string
; Support FunctionKey / History Command / Cursor / 
; In :
;	d3.w = maxchar
;	a0.l -> String to fill (maxchar+2)
; Out:
;	d0.w = string lenght
; Destroy :
;	d0
Input_String:
	movem.l	d1-d6/a0-a3,-(a7)
	moveq	#-1,d5				; History Index
	move.l	a0,a1				; a1 -> Current char
	clr.w	d4				; 0 char
	move.w	CURRENT_POINT_X,d6
\loop:	clr.b	(a1)				; NULL string
	bsr	StrWidth			; Calculate the length of the string
	add.w	d6,d0				; and X position
	move.w	d0,CURRENT_POINT_X		; Update CURSOR X
	move.w	d6,d0				
	move.b	#' ',(a1)			; Space
	move.b	#' ',1(a1)			; 2 Spaces
	move.b	#' ',2(a1)			; 3 Spaces (For TI-89)
	clr.b	3(a1)				; Null string
	move.w	CURRENT_POINT_Y,d1
	bsr.s	Display_String
	bsr	GetCursorKey
	cmp.w	#KEY_ENTER,d0			;Enter ?
	beq.s	\Enter
	cmp.w	#KEY_CLEAR,d0			;Clear ?
	beq	\Clear
	cmp.w	#KEY_BACK,d0
	beq.s	\Del
	cmp.w	#KEY_ESC,d0			;ESC ?
	beq.s	\Esc
	cmp.w	#KEY_UP,d0			;Up ?
	beq	\Up
	cmp.w	#KEY_DOWN,d0			;Down ?
	beq	\Dn
	cmpi.w	#KEY_ON,d0			; Break ?
	beq	\Completion
	cmpi.w	#KEY_F1,d0			; F1-F8 ?
	blt.s	\NoFKey
		cmpi.w	#KEY_F8,d0
		ble.s	\FKey
\NoFKey	cmp.w	#255,d0				;Charactère invalide ?
	bhi.s	\loop
	cmp.w	d3,d4				; Maxchar ?
	beq.s	\loop
	move.b	d0,(a1)+			; Save new char
	addq.w	#1,d4				; NbrChar++
	bra.s	\loop
\Del:	tst.w	d4
	beq.s	\loop
	subq.w	#1,d4
	clr.b	-(a1)
	bra	\loop
\Esc:	clr.w	d4		; No char
	clr.b	(a0)		; Null string
\Enter:	clr.b	(a1)		; Null string
	;bsr	StrWidth
	;add.w	d0,CURRENT_POINT_X	; Advance X
	move.w	d4,d0			; Number of Char
	movem.l (a7)+,d1-d6/a0-a3
	rts
; Function Key support
\FKey:	sub.w	#KEY_F1,d0
	mulu.w	#SHELL_MAX_LINE+2,d0
	lea	SHELL_FAST_KEY,a2
	adda.l	d0,a2
	tst.b	(a2)
	beq	\loop
\FLoop		move.b	(a2)+,d0
		beq	\loop
		cmpi.b	#'$',d0
		beq.s	\Enter			; End of command
		cmp.w	d3,d4			; Maxchar ?
		beq	\loop
		move.b	d0,(a1)+		; Save new char
		addq.w	#1,d4			; NbrChar++
		bra.s	\FLoop
\Clear:
	moveq	#-1,d5			; Reset History indecx
	bsr.s	\Clean
	bra	\loop
\Clean	move.w	d4,d0
	beq.s	\CleanEnd		; No characters
	pea	(a0)
	subq.l	#4,a7
	move.l	a7,a0
	move.b	d6,(a0)+
	move.b	CURRENT_POINT_Y+1,(a0)+
	move.b	#SCR_WIDTH-1,(a0)+
	move.b	CURRENT_POINT_Y+1,(a0)
	addq.b	#USED_FONT*2+6,(a0)
	move.w	#A_REVERSE,-(a7)
	pea	ScrRect(pc)
	pea	-3(a0)
	bsr	ScrRectFill
	lea	14(a7),a7
	move.l	(a7)+,a0
;	move.l	a0,a2			; Start string
;\ClearLoop:	move.b	#' ',(a2)+	; Fill with (d4+1) space
;		dbf	d0,\ClearLoop
;	clr.b	(a2)			; Null string
;	move.w	CURRENT_POINT_X,d0
;	move.w	CURRENT_POINT_Y,d1
;	bsr	Display_String		; Display the string
	move.l	a0,a1			; Reset string ptr
	clr.w	d4			; Reset length
\CleanEnd
	rts
\Up:	cmpi.w	#SHELL_HISTORY,d5
	bge	\loop
	addq.w	#1,d5
	bra.s	\CopyHistory
\Dn:	tst.w	d5					; TODO: If index = 0, clean command
	ble.s	\Clear					; TODO: Clean command (some char are not erased).
	subq.w	#1,d5
\CopyHistory
	lea	SHELL_HISTORY_TAB,a3
	move.w	d5,d0
	mulu.w	#SHELL_MAX_LINE+2,d0
	adda.l	d0,a3					; Saved String
	tst.b	(a3)					; If no pasted string
	beq.s	\FixIndex				; Return
	bsr.s	\Clean
\PasteLoop	cmp.w	d3,d4				; Maxchar ?
		beq.s	\Loop2				; Yes, quit
		move.b	(a3)+,d0			; Read char
		beq.s	\Loop2				; Check if the string is finished
		move.b	d0,(a1)+			; Save new char
		addq.w	#1,d4				; NbrChar++
		bra.s	\PasteLoop
\FixIndex
	subq.w	#1,d5					; Too high
\Loop2	bra	\loop
\Completion:
	jsr	Completion
	bra	\loop

; Copy the given string to the history table.
; In: a4 -> String
CopyToHistory:
	pea	((SHELL_HISTORY-1)*(SHELL_MAX_LINE+2)).w	; Size
	pea	SHELL_HISTORY_TAB				; From
	pea	SHELL_HISTORY_TAB+SHELL_MAX_LINE+2		; To 
	bsr	memmove
	pea	((SHELL_MAX_LINE+1)).w				; Size
	pea	(a4)						; From
	pea	SHELL_HISTORY_TAB				; To 
	bsr	memcpy
	lea	(4*6)(a7),a7
	rts

; Convertit une chaine decimale/hexadecimale en un nombre
; Support des nombres hexadecimaux
; In:
;	a0 -> chaine (NULL)
; Out:
;	d0.l = Nombre
; Destroy:
;	d0-d2/a0
str2nbr:
	cmp.b	#'0',(a0)
	bne.s	\start10
	cmp.b	#'x',1(a0)
	bne.s	\start10
		addq.l	#2,a0		; Skip '0x'
\start16:
	moveq	#0,d0
\loop		moveq	#0,d1		; Read
		move.b	(a0)+,d1	; Char
		beq.s	\end
		subi.b	#'0',d1		; From '0'
		cmpi.b	#9,d1
		bls.s	\ok
			subi.b	#'a'-'0'-10,d1
\ok		lsl.l	#4,d0
		add.l	d1,d0
		bra.s	\loop
\end:	rts

\start10:
	moveq	#0,d0
\loop10		moveq	#0,d1		; Read
		move.b	(a0)+,d1	; Char
		beq.s	\end
		subi.b	#'0',d1		; From '0'
		move.l	d0,d2
		lsl.l	#3,d0
		add.l	d2,d0
		add.l	d2,d0		; x10
		add.l	d1,d0
		bra.s	\loop10

; Translate the args from the command line
; Warning : if the command contains some space, it may fail !
; It translate wildcards '*' & '?' in files.
; In:
;	a4 -> Line Buffer
; Destroy:
;	Nothing
TranslateArgs:
	movem.l	d0-d5/a0-a5,-(a7)		
	move.l	a4,a2				; Current Command
	moveq	#0,d4
	lea	ARGV,a3				; ARGV Table
\loop:		move.l	a2,0(a3,d4.w)		; Save command (First arg)
		clr.b	d3			; No wildcards
		tst.b	(a2)			; End of string ?
		beq	\RealEndOfLine		; Yes
		addq.w	#4,d4			; Next arg
		cmpi.w	#ARG_MAX*4,d4		; CHeck if overflow ?
		bge.s	\EndOfLine
\next:			move.b	(a2)+,d0
			beq.s	\EndOfLine
			cmpi.b	#'*',d0
			bne.s	\NoStar
				st.b	d3	; Some wildcards		
\NoStar			cmpi.b	#'?',d0
			bne.s	\NoSkip
				st.b	d3	; Some wildcards
\NoSkip			cmpi.b	#' ',d0
			beq.s	\NextArg
			cmpi.b	#'"',d0
			bne.s	\next
				addq.l	#1,-4(a3,d4.w)	; Inc previous ptr to skip '"'
				clr.b	d3
\next2				move.b	(a2)+,d0
				beq.s	\RealEndOfLine
				cmpi.b	#'"',d0
				bne.s	\next2
\NextArg:	clr.b	-1(a2)		; Arg is NULL string
		tst.b	d3		; No Wildcars, ok
		beq.s	\loop
\Wild			move.l	-4(a3,d4.w),a5	; a5 -> Current Arg
			subq.w	#4,d4		; Remove this arg from the arg list
			; Search in the VAT some files which may be like the wildcards
			move.w	#FO_SINGLE_FOLDER,-(a7)	; We search in the current folder
			lea	CUR_FOLDER_STR,a1
\Cvt				tst.b	(a1)+
				bne.s	\Cvt
			pea	-1(a1)		; From ANSI to TI...
			bsr	SymFindFirst	; Go throught the current folder
			addq.l	#6,a7
			bra.s	\CmpGo
\CmpLoop			bsr	SymFindNext	; Next entry
\CmpGo				move.l	a0,d5		; No more entry ? Quit
				beq.s	\loop
				move.l	a5,a1
				bsr	Match		; Does theses str matches ?
				tst.b	d0
				bne.s	\CmpLoop	; No, next
					move.l	d5,0(a3,d4.w)	; Save this arg
					addq.w	#4,d4
					cmpi.w	#ARG_MAX*4,d4
					blt.s	\CmpLoop
					bra.s	\RealEndOfLine
\EndOfLine:
	subq.l	#1,a2
	tst.b	d3
	bne.s	\Wild
\RealEndOfLine
	lsr.w	#2,d4
	move.w	d4,ARGC
	movem.l	(a7)+,d0-d5/a0-a5
	rts

; Push the arg on the EStack for a program which is compatible with AMS.
; You must call TranslateArgs first
PushArgs:
	movem.l	d0-d3/a0-a2,-(a7)
	bsr	push_END_TAG
	move.w	ARGC,d3
	subq.w	#2,d3
	blt.s	\done
	lea	ARGV+8,a2
	adda.w	d3,a2
	adda.w	d3,a2
	adda.w	d3,a2
	adda.w	d3,a2
\loop		move.l	-(a2),-(a7)
		bsr	push_zstr
		addq.l	#4,a7
		dbf	d3,\loop
\done	movem.l	(a7)+,d0-d3/a0-a2
	rts
	
	
; Main program
; TODO: Complete rewrite it and merge it with OSInit
; In:
;	Nothing 
; Out:
;	Nothing
ShellCommand:
	; Display first line
	move.b	#USED_FONT,CURRENT_FONT		; Set current font
	bsr	clrscr	
	pea	Pedrom_str(pc)
	bsr	printf
	pea	Author_str(pc)
	bsr	printf
	; Run auto script ?
	tst.b	RUN_START_SCRIPT
	beq.s	\NoRun
		pea	StartScript_sym(pc)
		bsr	SymFindPtr			; Search file 'start'
		addq.l	#4,a7
		move.l	a0,d0
		beq.s	\NoRun
		move.w	SYM_ENTRY.hVal(a0),d0
		beq.s	\NoRun
			jsr	ExecuteScriptFile
\NoRun:	; Enter command.
	lea	(14-SHELL_MAX_LINE-6)(a7),a7

ShellCommandLoop
	; Check y position
	move.w	SHELL_SAVE_Y_POS,d0		; The window modify the vertical position of the pen.
	cmp.w	CURRENT_POINT_Y,d0		; So we check if the current position is > than the previous.
	ble.s	\Continue			; By the way, clearscreen resets the previous value too.
		move.w	d0,CURRENT_POINT_Y	; If not, we set the current value to the previous saved one.
\Continue
	cmpi.w	#SCR_HEIGHT-8,CURRENT_POINT_Y
	ble.s	\Continue2
		jsr	clrscr
\Continue2		
	clr.w	(a7)
	bsr	ST_busy				; Idle mode
	bsr	PortRestore			; Restore the normal screen
	bsr	GKeyFlush			; Clear key buffer

	move.l	a7,a4				; Input Buffer
	clr.b	(a4)+				; First byte is null
	move.b	#USED_FONT,CURRENT_FONT		; Set current font
	; Ask command
\ContInput	pea	Shell_str(pc)
		bsr	printf
		addq.l	#4,a7
		move.l	a4,a0
		moveq	#SHELL_MAX_LINE,d3
		bsr	Input_String
		tst.b	(a4)
		beq.s	\ContInput	
	bsr	CopyToHistory			; Copy the command to the history
	bsr.s	ShellExecuteCommand
	bra	ShellCommandLoop
	
; In:	a4 -> String to execute (WARNING: it is modified !, and -1(a4) =0 !)
; Destroy a2/a3/d3/d4.
ShellExecuteCommand:
	clr.w	PRINTF_LINE_COUNTER		; Restart pausing system
	pea	Return_str(pc)
	bsr	printf
	addq.l	#4,a7
	bsr	EStackReInit			; Reinit EStack before parsing
	move.w	CURRENT_POINT_Y,SHELL_SAVE_Y_POS
	; 1. Check if it an Internal Command ?
	move.l	a4,a1
	lea	CommandTable(pc),a2
\loop:		move.w	(a2)+,d0		; End of table ?
		beq	CommandNotFound
		lea	CommandTable(pc,d0.w),a3
		move.l	a1,a0
\cmp:			tst.b	(a3)		; NULL str ?
			beq.s	\InternalCommand
			cmpm.b	(a3)+,(a0)+	; Cmp str
			beq.s	\cmp
		addq.l	#2,a2			; Next command
		bra.s	\loop
\InternalCommand:
	move.b	(a0),d0
	beq.s	\ok
	cmpi.b	#' ',d0
	bne.s	\loop
\ok:	bsr	TranslateArgs			; Translate the args
	move.w	(a2),d0
	jmp	CommandTable(pc,d0.w)
ADD_COMMAND	MACRO
	dc.w	\1_str-CommandTable,\1_cmd-CommandTable
		ENDM
CommandTable:
	ADD_COMMAND	Arc
	ADD_COMMAND	Cd
	ADD_COMMAND	Clean
	ADD_COMMAND	Cls
	ADD_COMMAND	Cp
	ADD_COMMAND	DispFKey
	ADD_COMMAND	Echo
	ADD_COMMAND	Get
	ADD_COMMAND	HelpKeys
	ADD_COMMAND	Help
	ifd	EXTRA_COMMAND
		ADD_COMMAND	HexDump
	endif
	ADD_COMMAND	InstallProductCode
	ADD_COMMAND	InstallTIB
	ifd	EXTRA_COMMAND
		ADD_COMMAND	LinkSend
		ADD_COMMAND	LinkDump
	endif
	ADD_COMMAND	Ls
	ADD_COMMAND	Mem
	ADD_COMMAND	MkDir
	ADD_COMMAND	Mv
	ADD_COMMAND	Reset
	ADD_COMMAND	RmDir
	ADD_COMMAND	Rm
	ADD_COMMAND	SendCalc
	ADD_COMMAND	SetAPD
	ADD_COMMAND	SetFKey
	ADD_COMMAND	SetPath
	ADD_COMMAND	Side
	ADD_COMMAND	UnArc
	ADD_COMMAND	UnPPG
	dc.w	0

CommandNotFound:
	; 2 : Check if it a file ?
	move.l	a4,a0
	suba.l	a3,a3			; NULL
\Cvt:		move.b	(a0)+,d0
		beq.s	\CvtDone
		cmpi.b	#' ',d0
		bne.s	\Cvt
		lea	-1(a0),a3
		clr.b	(a3)
\CvtDone
;	clr.w	-(a7)
;	pea	-1(a0)			; Push filename
;	bsr	SymFindPtr		; Search for it
;	addq.l	#6,a7
	jsr	FindSymInPath
	move.l	a3,d0			; Check if we have patched the string
	beq.s	\CheckSym
		move.b	#' ',(a3)	; Restore the ' ' char.
\CheckSym
	move.l	a0,d4			; Success finding it ?
	beq	BadCommand
		; Check if it an ASM file or a PPG file
		move.w	SYM_ENTRY.hVal(a0),-(a7)	; Push Handle
		bsr	HToESI
		cmpi.b	#$F3,(a0)
		beq.s	\Run_ASM
		cmpi.b	#$F8,(a0)
		beq.s	\KernelExec
		cmpi.b	#$E0,(a0)
		beq.s	\ScriptExec
			addq.l	#2,a7			; Pop Handle
			bra.s	BadCommand		; Next interpretation
\Run_ASM	bsr	HeapDeref
		cmp.l	#'68cA',4(a0)			; Pack Archive ?
		beq.s	\KernelExec			; Yes => Exec
			bsr	TranslateArgs		; Translate the args
			bsr	PushArgs		; Push on the EStack the args.
			move.l	d4,a0			; Reget the SYM_ENTRY.
			; Even if kernel::exec allows archived program, I must add a Twin entry
			; for compatibility with AMS (Who says Tictex ?)
			bsr	Sym2HSym			; Get HSym
			move.l	d0,-(a7)
			clr.l	-(a7)
			bsr	EM_twinSymFromExtMem		; Make a Twin Symbol
			move.l	d0,(a7)
			beq.s	\ErrorTwin
				bsr	DerefSym		; Deref it
				move.w	SYM_ENTRY.hVal(a0),d0	; Get Handle
				jsr	kernel::exec		; Execute program
				bsr	DerefSym		; Rederef if (It may be wrong)
				move.l	a0,d0			; But at least it will be a file ptr
				beq.s	\ErrorTwin		; not an unkown ptr.
					bsr	SymDelTwin_reg	; Delete Twin Symbol (It checks it it is a Twin before deleting it).
\ErrorTwin		lea	10(a7),a7
			rts
\KernelExec	bsr	TranslateArgs				; Translate the args
		bsr	PushArgs				; Push on the EStack the args.
		move.w	(a7)+,d0				; HANDLE
		jmp	kernel::exec				; Execute the file
\ScriptExec:	move.w	(a7)+,d0				; HANDLE
		jmp	ExecuteScriptFile			; Execute the script

BadCommand:
	; 3 : Interpret command
	move.l	a7,a6
	lea	-60(a7),a7			; Error Stack Frame
	pea	(a7)
	bsr	ER_catch			; Catch all errors.
	tst.w	d0
	bne.s	\Error
		bsr	EStackReInit			; Reinit EStack before parsing
		pea	(a4)
		jsr	push_parse_text			; Push Text
		move.l	top_estack,(a7)
		jsr	NG_approxESI			; Evaluate it
		move.l	top_estack,(a7)
		jsr	display_statements		; Retransform it to text
		move.w	d0,a0				; Get the Handle
		move.w	d0,(a7)				; Push the handle
		trap	#3				; Deref it
		pea	(a0)				; Push its address
		bsr	printf				; Display the text
		addq.l	#4,a7				; Pop its address
		bsr	HeapFree	
		bsr	ER_success
		bra.s	\Cont
\Error	pea	CommandNotFound_str(pc)
	bsr	printf
\Cont	move.l	a6,a7
	rts

InstallProductCode_cmd
	bsr	Confirm
	tst.w	d0
	beq.s	\ret
		bsr	FL_download
\ret	rts

Reset_cmd:
	bsr	Confirm
	tst.w	d0
	beq.s	\ret
		trap	#2
\ret	rts

InstallTIB_cmd:
	bsr	Confirm
	tst.w	d0
	bne	TIB_Install
	rts

Ls_cmd:
	move.l	a6,-(a7)
	move.l	a7,a6
	; Translate args
	lea	CommandDisp_str(pc),a2
	cmpi.w	#2,ARGC
	bne.s	\Std
		move.l	ARGV+4,a0
		cmpi.b	#'-',(a0)+
		bne.s	\Std
		cmpi.b	#'h',(a0)
		beq	\Folders
		cmpi.b	#'l',(a0)
		bne.s	\Std
		lea	LsLong2_str(pc),a2
		pea	LsLong1_str(pc)
		bsr	printf
\Std:	; Display Intro String
	pea	CUR_FOLDER_STR
	pea	Dir1_str(pc)
	bsr	printf
	move.w	#1,(a7)
	clr.b	NULL_CHAR
	; Start Displaying the files
	lea	CUR_FOLDER_STR,a0
\cvt		tst.b	(a0)+
		bne.s	\cvt
	pea	-1(a0)
	bsr	SymFindFirst
	moveq	#0,d3
\loop		move.l	a0,d0
		beq.s	\end
		addq.w	#1,d3		; One more file
		move.l	a0,a3		; Save SYM_ENTRY
		move.w	SYM_ENTRY.hVal(a0),d0
		bne.s	\NoHNull
			clr.l	-(a7)	; No Ptr
			clr.w	-(a7)	; No type
			clr.w	-(a7)	; No flags
			clr.w	-(a7)	; No size
			bra.s	\Cont
\NoHNull	move.w	d0,a0
		trap	#3		; Deref file
		pea	(a0)		; Push File Ptr
		moveq	#0,d2		
		move.w	(a0)+,d2	; Read size
		clr.w	d0		
		move.b	-1(a0,d2.l),d0	; Read type
		move.w	d0,-(a7)	; and push it
		move.w	SYM_ENTRY.flags(a3),-(a7)	; Push Flags
		move.w	d2,-(a7)	; Püsh Size
\Cont		pea	(a3)		; Push Name
		pea	(a2)		; Push Format string
		bsr	printf		; Print it
		lea	18(a7),a7	; Pop args
		bsr	SymFindNext
		bra.s	\loop
\end:	move.w	d3,(a7)
	pea	Dir2_str(pc)
	bsr	printf
	move.l	a6,a7
	move.l	(a7)+,a6
	rts
\Folders
	pea	Home_str(pc)
	pea	Dir1_str(pc)
	bsr	printf
	clr.w	-(a7)
	clr.l	-(a7)
	bsr	SymFindFirst
	moveq	#0,d3
\loop2		move.l	a0,d0
		beq.s	\end
		addq.w	#1,d3		; One more file
		pea	(a0)		; Push Name
		pea	CommandDisp_str(pc)		; Push Format string
		bsr	printf		; Print it
		addq.l	#8,a7		; Pop args
		bsr	SymFindNext
		bra.s	\loop2
	
Help_cmd:
	lea	CommandTable(Pc),a2
\loop		move.w	(a2),d0
		beq.s	\end
		lea	CommandTable(Pc),a0
		pea	0(a0,d0.w)
		pea	CommandDisp_str(pc)
		bsr	printf
		addq.l	#8,a7
		addq.l	#4,a2
		bra.s	\loop
\end	rts

Mem_cmd:
	bsr	HeapCompress	; Compress the Heap
	subq.l	#8,a7
	pea	4(a7)		; Free
	pea	4(a7)		; FreeAfter GC
	clr.l	-(a7)		; InUse
	bsr	EM_survey
	lea	12(a7),a7
	move.l	(a7)+,d0
	add.l	d0,(a7)		; Free + FreeAfterGc
	bsr	HeapAvail
	move.l	d0,-(a7)
	move.l	#START_ARCHIVE,-(a7)
	sub.l	#BASE_END,(a7)	
	pea	MemDisplay_str(pc)
	bsr	printf
	lea	16(a7),a7
	rts

Get_cmd:
	lea	cmd_getcalc(pc),a0
	bra.s	CommunOneFile
SendCalc_cmd:
	lea	cmd_sendcalc(pc),a0
	bra.s	CommunOneFile
UnArc_cmd:
	lea	EM_moveSymFromExtMem(pc),a0
	bra.s	CommunOneFile
Cls_cmd:
	jmp	clrscr		; May be > 32K ?
Rm_cmd:	lea	SymDel(pc),a0
	bra.s	CommunOneFile
MkDir_cmd:
	lea	FolderAdd(pc),a0
	bra.s	CommunOneFile
RmDir_cmd:
	lea	FolderDel(pc),a0
	bra.s	CommunOneFile
Cd_cmd:	lea	FolderCur(pc),a0
	bra.s	CommunOneFile

Arc_cmd:
	lea	EM_moveSymToExtMem(pc),a0

CommunOneFile:
	cmpi.w	#2,ARGC
	bge.s	\Ok
		pea	Arg1_str(pc)
\printf		bsr	printf
		addq.l	#4,a7
		rts
\Ok:	lea	-100(a7),a7
	move.l	a7,a3			; Buffer
	move.l	a0,a2			; Function
\Loop		move.w	ARGC,d0		; ARGC is the loop counter
		subq.w	#1,d0
		beq.s	\Done		; last one is done
		move.w	d0,ARGC
		lsl.w	#2,d0		; Last argument
		lea	ARGV,a1
		move.l	0(a1,d0.w),a1	; Arg ptr
		move.l	a3,a0
		clr.b	(a0)+		; Convert Arg to Ti format
\cvt:			move.b	(a1)+,(a0)+
			bne.s	\cvt		
		clr.l	-(a7)
		pea	-1(a0)
		jsr	(a2)		; Call the function
		addq.l	#8,a7
		tst.w	d0
		bne.s	\Ok2
			pea	1(a3)
			pea	Failed_str(pc)
			bsr	printf
			addq.l	#8,a7
\Ok2		bra.s	\Loop
\Done	lea	100(a7),a7
	rts
			
Clean_cmd:
	bsr	Confirm
	tst.w	d0
	beq.s	\ret
		movem.l	d3-d7/a2-a6,-(a7)
		lea	VECTORS_TABLE(PC),a0	; Copy org Vectors
		lea	$40000,a1		; GHOST SPACE (To avoid unprotection)
		moveq	#$3F,d0
\VECTOR_loop		move.l	(a0)+,(a1)+
			dbf	d0,\VECTOR_loop
		bsr	EStackReInit		; Reset the EStack
		bsr	clean_up		; Clean the kernel files (Before the heap)
		bsr	CleanTwinFiles		; Clean the Twin files
		lea	HEAP_TABLE+4,a2		; HEAP TABLE (We skip the first one which is set to $FFFFFFFF)
		move.w	#MAX_HANDLES-2,d5	; d5 = number of handles
		moveq	#1,d3			; d3 = handle number = 1
\loop2			tst.l	(a2)+
			beq.s	\NextHandle
			move.w	d3,d0
			cmpi.w	#FOLDER_LIST_HANDLE,d0	; Do not delete Home directory !
			beq.s	\NextHandle
			cmpi.w	#ESTACK_HANDLE,d0	; Do not delete EStack !
			beq.s	\NextHandle
				bsr	kernel::Hd2Sym	; Then see if this handle is in VAT (it is a file or a folder)
				move.l	a0,d0		; Test if Null
				bne.s	\NextHandle
					move.w	d3,-(a7)	; Free this handle (It may be usefull, but 
					bsr	HeapFree
					addq.w	#2,a7
\NextHandle:		addq.w	#1,d3		; increase handle number
			dbf	d5,\loop2	; increase bit number
		; Unlock all the VAR
		move.w	#2,-(a7)
		clr.l	-(a7)
		bsr	SymFindFirst
\loop3			move.w	SYM_ENTRY.hVal(a0),(a7)
			bsr	HeapUnlock
			bsr	SymFindNext
			move.l	a0,d0
			bne.s	\loop3
		addq.l	#6,a7
		bsr	HeapCheck		; Check the heap and 
		bsr	HeapCompress		; Compress the Heap
		movem.l	(a7)+,d3-d7/a2-a6
\ret	rts
	
Mv_cmd:
	cmpi.w	#3,ARGC
	beq.s	\Ok
		pea	Arg2_str(pc)
		bsr	printf
		addq.l	#4,a7
		rts
\Ok:	; Convert to Ti format
	move.l	ARGV+4,a0		; SrcFileName
\cvt1		tst.b	(a0)+
		bne.s	\cvt1
	move.l	ARGV+8,a1		; Dest File Name
\cvt2		tst.b	(a1)+
		bne.s	\cvt2
	pea	-1(a1)			; Dest File Name
	pea	-1(a0)			; Src File Name
	bsr	SymMove
	addq.l	#8,a7
	tst.w	d0
	bne.s	\Ok2
		pea	ST_StrA(pc)
		pea	Failed_str(pc)
		bsr	printf
		addq.l	#8,a7
\Ok2:	rts
	
Cp_cmd:
	cmpi.w	#3,ARGC
	beq.s	\Ok
		pea	Arg2_str(pc)
		bsr	printf
		addq.l	#4,a7
		rts
\Ok:	; Convert to Ti format
	move.l	ARGV+4,a0		; SrcFileName
\cvt1		tst.b	(a0)+
		bne.s	\cvt1
	clr.w	-(a7)
	pea	-1(a0)
	bsr	SymFindPtr
	move.l	a0,d0
	beq.s	\Fail
		move.w	SYM_ENTRY.hVal(a0),(a7)	; Push Handle
		moveq	#0,d0
		move.w	(a7),a0
		trap	#3
		move.w	(a0),d0
		addq.l	#3,d0
		bsr	HeapAlloc_reg
		tst.w	d0
		beq.s	\Fail
			move.w	(a7),a0		; Reload Handle
			trap	#3
			move.w	(a0),d1		; Reload Size
			addq.w	#3,d1
			move.l	a0,a1		; Src = a1
			move.w	d0,(a7)		; Save Handle
			move.w	d0,a0		; Dest 
			trap	#3		; a0 = dest
\CpyLoop			move.b	(a1)+,(a0)+
				subq.w	#1,d1
				bne.s	\CpyLoop	
			move.l	ARGV+8,a1		; Dest File Name
\cvt2				tst.b	(a1)+
				bne.s	\cvt2
			pea	-1(a1)			; Dest File Name
			bsr	SymAdd
			addq.l	#4,a7
			tst.l	d0
			beq.s	\Fail2
				bsr	DerefSym_Reg
				move.w	(a7),SYM_ENTRY.hVal(a0)
				bra.s	\Ok2
\Fail2			bsr	HeapFree
\Fail	pea	ST_StrA(pc)
	pea	Failed_str(pc)
	bsr	printf
	addq.l	#8,a7
\Ok2:	addq.l	#6,a7
	rts

HelpKeys_cmd
	bra	HelpKeys

SetFKey_cmd:
	cmpi.w	#3,ARGC
	beq.s	\Ok
\Error		pea	FKeyError_str(pc)
		bsr	printf
		addq.l	#4,a7
		rts
\Ok	move.l	ARGV+4,a0		; Number
	bsr	str2nbr			; Get number
	tst.l	d0
	ble.s	\Error
	cmpi.l	#8,d0
	bgt.s	\Error
	subq.w	#1,d0
	mulu.w	#SHELL_MAX_LINE+2,d0
	lea	SHELL_FAST_KEY,a0
	adda.l	d0,a0			; Dest
	move.l	ARGV+8,a1		; Src
	bra	strcpy_reg		; Bsr+rts = bra

DispFKey_cmd:
	moveq	#1,d3
	lea	SHELL_FAST_KEY,a2
\loop		pea	(a2)
		move.w	d3,-(a7)
		pea	FastKey_str(pc)
		bsr	printf
		lea	10(a7),a7
		addq.w	#1,d3
		lea	(SHELL_MAX_LINE+2)(a2),a2
		cmpi.w	#8,d3
		ble.s	\loop
	rts

SetAPD_cmd:
	cmpi.w	#2,ARGC
	beq.s	\Ok
\Error		pea	ArgNumber_str(pc)
		bsr	printf
		addq.l	#4,a7
		rts
\Ok	move.l	ARGV+4,a0		; Number
	bsr	str2nbr			; Get number
	cmpi.w	#10,d0
	blt.s	\Error
	cmpi.w	#1000,d0
	bgt.s	\Error
	mulu.w	#20,d0
	move.l	d0,-(a7)
	move.w	#2,-(a7)
	bsr	OSFreeTimer
	bsr	OSRegisterTimer
	addq.l	#6,a7
	rts
	
Side_cmd:
	moveq	#0,d0
	cmpi.w	#1,ARGC
	beq.s	\Go
		move.l	ARGV+4,d0
\Go	move.l	d0,filename
	jsr	run_side
	jmp	clrscr
	
Echo_cmd:
	cmpi.w	#2,ARGC
	blt.s	\No
		move.l	ARGV+4,-(a7)
		pea	String_str(pc)
		bsr	printf
		addq.l	#8,a7
\No	rts
	
SetPath_cmd:	; FIXME: Avoid overflow
	lea	ARGV+4,a2
	lea	SHELL_PATH,a0
\loop		subq.w	#1,ARGC
		beq.s	\end
			move.l	(a2)+,a1
\Copy				move.b	(a1)+,(a0)+
				bne.s	\Copy
			bra.s	\loop	
\end	clr.b	(a0)+
	clr.b	(a0)
	rts
	
UnPPG_cmd:
	move.l	a7,a2
	cmpi.w	#3,ARGC
	beq.s	\Ok
		pea	Arg2_str(pc)
		bsr	printf
		bra.s	\done
\Ok:	; Find Src Name
	move.l	ARGV+4,a0		; SrcFileName
\cvt1		tst.b	(a0)+
		bne.s	\cvt1
	clr.w	-(a7)
	pea	-1(a0)			; Src File Name
	bsr	SymFindPtr
	move.l	a0,d0
	bne.s	\Ok2
\Fail		move.l	ARGV+4,-(a7)
		pea	Failed_str(pc)
		bsr	printf
		bra.s	\done
\Ok2:	; Extract PPG
	move.w	SYM_ENTRY.hVal(a0),d4	; Source Handle
	bsr	ExtractPPG		; Extract PPG
	move.w	d0,d4
	beq.s	\Fail			; FIXME: Unlocked it !
	; Add new file
	move.l	ARGV+8,a0		; DestFileName
\cvt2		tst.b	(a0)+
		bne.s	\cvt2
	pea	-1(a0)			; Dest File Name
	bsr	SymAdd
	move.l	d0,(a7)
	bsr	DerefSym
	move.l	a0,d0
	bne.s	\Ok4
		move.w	d4,(a7)
		bsr	HeapFree
		bra.s	\Fail
\Ok4	move.w	d4,SYM_ENTRY.hVal(a0)
\done:	move.l	a2,a7
	rts

	ifd	EXTRA_COMMAND
HexDump_cmd:
	cmpi.w	#2,ARGC
	beq.s	\Ok
\Error		pea	ArgNumber_str(pc)
		bsr	printf
		addq.l	#4,a7
		rts
\Ok	movem.l	d3/a3,-(a7)
	move.l	ARGV+4,a0		; Number
	bsr	str2nbr			; Get number
	move.l	d0,a3
	moveq	#8-1,d3
\loop2		moveq	#8-1,d1
		addq.l	#8,a3
\loop			clr.w	d0
			move.b	-(a3),d0
			move.w	d0,-(a7)
			dbf	d1,\loop
		pea	(a3)
		addq.l	#8,a3
		pea	HexFormat_str(pc)
		bsr	printf
		lea	(8*2+4+4)(a7),a7	
		dbf	d3,\loop2
	movem.l	(a7)+,d3/a3
	rts

LinkSend_cmd:
	movem.l	d3/a3,-(a7)
	lea	ARGV+4,a3		; Argc
	move.w	ARGC,d3			; ARGV
	subq.w	#2,d3
	blt.s	\End
\loop		move.l	(a3)+,a0	; Get Number
		bsr	str2nbr		; Translate number
		move.l	d0,EXEC_RAM	; Save it
		pea	(4).w		; Size
		pea	EXEC_RAM	; Ptr
		bsr	LIO_SendData	; Send
		addq.l	#8,a7
		dbf	d3,\loop
\End	movem.l	(a7)+,d3/a3
	rts
	
LinkDump_cmd:
	pea	Dumping_str(pc)
	bsr	printf
	addq.l	#4,a7
	bsr	OSEnableBreak
\loop		tst.w	LINK_RECEIVE_QUEUE+QUEUE.used
		beq.s	\next
			move.w	#1,-(a7)
			pea	EXEC_RAM
			bsr	OSReadLinkBlock
			clr.w	d0
			move.b	EXEC_RAM,d0
			move.w	d0,(a7)
			pea	ByteFormat_str(pc)
			bsr	printf
			lea	10(a7),a7
\next		tst.b	BREAK_KEY
		beq.s	\loop
	rts
	endif
	
	ifd	EXTRA_COMMAND
LinkDump_str		dc.b	"linkdump",0
LinkSend_str		dc.b	"linksend",0
HexDump_str		dc.b	"hexdump",0
	endif
	
InstallTIB_str		dc.b	"install tib",0
UnPPG_str		dc.b	"unppg",0
SetPath_str		dc.b	"setpath",0
Echo_str		dc.b	"echo",0
Side_str		dc.b	"side",0
SetAPD_str		dc.b	"setapd",0
DispFKey_str		dc.b	"dispfkey",0
SetFKey_str		dc.b	"setfkey",0
HelpKeys_str		dc.b	"helpkeys",0
Get_str			dc.b	"getcalc",0
Cp_str			dc.b	"cp",0
Mv_str			dc.b	"mv",0
Clean_str		dc.b	"clean",0
SendCalc_str		dc.b	"sendcalc",0
Cd_str			dc.b	"cd",0
RmDir_str		dc.b	"rmdir",0
MkDir_str		dc.b	"mkdir",0
Rm_str			dc.b	"rm",0
Cls_str			dc.b	"cls",0	
Arc_str			dc.b	"arc",0
UnArc_str		dc.b	"unarc",0
Mem_str			dc.b	"mem",0
InstallProductCode_str	dc.b	"install product code",0
Reset_str		dc.b	"reset",0
Help_str		dc.b	"help",0
Ls_str			dc.b	"ls",0
	EVEN
	