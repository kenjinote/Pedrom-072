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
;***            	Misc Functions (2)			***
;***                                                            ***
;******************************************************************

; void OSContrastUp(void)
OSContrastUp:
	move.b	CONTRAST_MAX_VALUE,d1
	move.b	CONTRAST_VALUE,d0
	and.b	d1,d0
	cmp.b	d1,d0
	beq.s	ContrastReturn
	addq.b	#1,d0
	bra.s	ContrastSet

;void OSContrastSet(void)
OSContrastSet:
	move.b	CONTRAST_VALUE,d0
	bra.s	ContrastSet

;void OSContrastDn(void)
OSContrastDn:
	move.b	CONTRAST_MAX_VALUE,d1	; =$0F sur HW1
	move.b	CONTRAST_VALUE,d0
	and.b	d1,d0
	beq.s	ContrastReturn
	subq.b	#1,d0

ContrastSet:
	move.b	d0,CONTRAST_VALUE
	ifnd	TI89
		not.b	d0		; For 92+/V200
		cmpi.b	#1,HW_VERSION
		bne.s	Contrastno_hw1
			btst.l	#0,d0
			beq.s	Contrastclearit
				bset.b	#5,$600000
Contrastcont:		lsr.b	#1,d0
Contrastno_hw1	move.b	d0,$60001D
ContrastReturn:
	endif
	ifd	TI89
		ori.b	#$80,d0		; For 89
		cmpi.b	#1,HW_VERSION
		bne.s	Contrastno_hw1
			andi.b	#$EF,d0
Contrastno_hw1	move.b	d0,$60001D
ContrastReturn:
	endif
	rts
	ifnd	TI89
Contrastclearit:	bclr.b	#5,$600000
			bra.s	Contrastcont
	endif

;short OSSetSR(short mask);
OSSetSR:
	move.w	4(a7),d0
	trap	#1
	rts

;void OSReset(void);
OSReset:
	trap	#2

; void NeedStack(unsigned short size)	
NeedStack:
	move.w	4(a7),d1
	ext.l	d1
	move.l	a7,d0
	subi.l	#$400,d0
	cmp.l	d1,d0
	bge.s	\ok
		dc.w	$A29E
\ok	rts

;short setjmp (void *j_buf); 
setjmp:
	movea.l	4(a7),a0
	movem.l	d2-d7/a2-a7,(a0)
	move.l	(a7),$30(a0)	; Useless
	moveq	#0,d0
	rts
	
;void longjmp (void *j_buf, short ret_val);
longjmp:
	move.l	4(a7),a0
	move.w	8(a7),d0
	bne.s	\not
		moveq	#1,d0
\not:	movem.l	(a0)+,d2-d7/a2-a7
	move.l	(a0),(a7)
	rts
	
;void EX_patch (void *base_addr, void *tag_ptr);
EX_patch:
	move.l	4(a7),a0		; Base Ptr
	move.l	8(a7),a1		; End Ptr
	pea	(a2)
	; Due to some nasty reason, it may be unaligned... Suck
\loop:		moveq	#0,d1
		move.b	-(a1),d1
		moveq	#0,d0
		move.b	-(a1),d0
		lsl.w	#8,d0
		or.w	d0,d1		; Patch addr
		beq.s	\EndOfReloc
		moveq	#0,d2
		move.b	-(a1),d2
		moveq	#0,d0
		move.b	-(a1),d0
		lsl.w	#8,d0
		or.w	d0,d2		; What to write
		move.l	a0,d0
		add.l	d2,d0		; Reloc it
		lea	4(a0,d1.l),a2
		move.b	d0,-(a2)
		lsr.l	#8,d0
		move.b	d0,-(a2)
		lsr.l	#8,d0
		move.b	d0,-(a2)
		lsr.l	#8,d0
		move.b	d0,-(a2)
		bra.s	\loop
\EndOfReloc:
	move.l	(a7)+,a2
	rts

;short CU_stop (void);
; Destroy only d0
CU_stop:
	clr.w	d0
	move.b	CURSOR_STATE,d0			; Read current state of cursor (active/inactive)
	clr.b	CURSOR_STATE			; Stop Cursor : stop auto-int
	tst.b	CURSOR_PHASE			; Check if cursor is currently displayed
	beq.s	\Ok
		bsr.s	CU_BlinkCursor		; Yes, so erase it
\Ok	rts

;short CU_start (void);
; Destroy only d0
CU_start:
	bsr.s	CU_stop
	st.b	CURSOR_STATE
	rts

;void CU_restore (short State);
; Destroy only d0
CU_restore:
	bsr.s	CU_stop
	move.w	4(a7),d0
	move.b	d0,CURSOR_STATE
	rts

; void CU_BlinkCursor(void)
CU_BlinkCursor:
	movem.l	d0-d2/a0,-(a7)
	not.b	CURSOR_PHASE		; change the phase 00->ff or ff->00
	move.w	CURRENT_POINT_X,d0
	move.w	CURRENT_POINT_Y,d1
	cmpi.w	#SCR_WIDTH-8,d0		; Check overflow
	bhi.s	\End
	cmpi.w	#SCR_HEIGHT-8,d1
	bhi.s	\End
	movea.l	CURRENT_SCREEN,a0	; a0 = *VideoRAM
	; calculate the offset and Mask of the cursor
	addq.w	#USED_FONT*2+5,d1	; d1 = Y-coord of bottom of cursor
	mulu	#30,d1			; d1 = line-offset of -"-
	move.w	d0,d2			; d2 = number of byte within line
	lsr.w	#3,d2			
	add.w	d2,d1			; d1 = byte offset within screen
	adda.w	d1,a0			; a0 = absolute byte offset of cursor
	andi.w	#$0007,d0		; d0 = pixel number within byte
	move.w	#%1111110000000000,d1	; d1 = cursor-mask
	lsr.w	d0,d1			; d1.b = cursor mask 2
	move.w	d1,d0			; d0.b = cursor mask 1
	lsr.w	#8,d0
	; invert the cursor
	moveq	#CURSOR_SIZE-1,d2
\Loop:		eor.b	d0,(a0)+
		eor.b	d1,(a0)+
		lea	-32(a0),a0
		dbra	d2,\Loop
\End	movem.l	(a7)+,d0-d2/a0
	rts

CU_Interrupt:
	tst.b	CURSOR_STATE		; Check if the cursor is displayed
	beq.s	\End			; No, end 
		bra.s	CU_BlinkCursor	; Yes, display it.
\End:	rts	

; void cmd_disphome (void); 
cmd_disphome:
	bra	clrscr

;void MD5Done (BN *digest, MD5_CTX *context);
MD5Done:
	move.l	4(a7),a0
	move.l	8(a7),-(a7)
	pea	1(a0)
	jsr	MD5Final
	addq.l	#8,a7
	move.l	4(a7),a0
	moveq	#16,d0
\loop		tst.b	0(a0,d0.w)
		bne.s	\done
		subq.w	#1,d0
		bgt.s	\loop
\done	move.b	d0,(a0)
	rts
	

;unsigned short OSqhead (unsigned short *dummy, void *Queue); 
OSqhead:
	move.l	8(a7),a0
	tst.w	QUEUE.used(a0)
	beq.s	\Nothing
		move.w	QUEUE.head(a0),d1
		subq.w	#2,d1
		move.w	QUEUE.data(a0,d1.w),d0
\Nothing:
	rts

;short OSqinquire (unsigned short *dest, void *Queue);
OSqinquire:
	move.l	8(a7),a0
	move.l	4(a7),a1
	moveq	#0,d0
	tst.w	QUEUE.used(a0)
	beq.s	\done
		move.w	QUEUE.tail(a0),d1
		move.w	QUEUE.data(a0,d1.w),(a1)
		moveq	#1,d0
\done:	rts

;short OSenqueue (unsigned short data, void *Queue);
OSenqueue:
	move.l	4(a7),d2
	move.l	6(a7),a0
	clr.w	d0
	move.w	QUEUE.used(a0),d1
	cmp.w	QUEUE.size(a0),d1
	bge.s	\Cant
		addq.w	#2,QUEUE.used(a0)
		move.w	QUEUE.head(a0),d1
		move.w	d2,QUEUE.data(a0,d1.w)
		addq.w	#2,d1
		cmp.w	QUEUE.size(a0),d1
		blt.s	\Ok
			clr.w	d1
\Ok		move.w	d1,QUEUE.head(a0)
		moveq	#1,d0
\Cant	rts

;short OSdequeue (unsigned short *dest, void *Queue); 
OSdequeue:
	move.l	4(a7),a1
	move.l	8(a7),a0
	moveq	#1,d0			; Return TRUE if Queue is empty
	tst.w	QUEUE.used(a0)
	beq.s	\Cant
		move.w	QUEUE.tail(a0),d1
		move.w	QUEUE.data(a0,d1.w),(a1)
		subq.w	#2,QUEUE.used(a0)
		addq.w	#2,d1
		cmp.w	QUEUE.size(a0),d1
		blt.s	\Ok
			clr.w	d1
\Ok		move.w	d1,QUEUE.tail(a0)
		moveq	#0,d0
\Cant	rts
	
;vsprintf:
;	link.w a6,#-4
;	move.l 8(a6),-4(a6)
;	move.l 16(a6),-(sp)
;	move.l 12(a6),-(sp)
;	pea -4(a6)
;	pea _sputc
;	jsr vcbprintf
;	lea 16(sp),sp
;	move.l -4(a6),a0
;	clr.b (a0)
;	unlk a6
;	rts

;short atoi (const char *str asm("a0")); 
; A little smaller than tigcc's one.
atoi:
	moveq	#0,d0
	clr.w	d1			; Sign
	; Skip space.
\SkipSpace	move.b	(a0)+,d2
		cmp.b	#' ',d2
		beq.s	\SkipSpace
	; Check sign
	cmp.b	#'-',d2
	beq.s	\Negate
		cmp.b	#-83,d2
		bne.s	\Next
\Negate			moveq	#-1,d1
			bra.s	\Cont
\Next	cmp.b	#'+',d2
	bne.s	\SignDone
\Cont		move.b	(a0)+,d2		; Read next char
\SignDone
	; While...
\loop		subi.b	#'0',d2
		bcs.s	\Done
		cmpi.b	#9,d2
		bhi.s	\Done
		ext.w	d2
		mulu.w	#10,d0
		add.w	d2,d0			; d0 = d0*10 + c - '0'
		move.b	(a0)+,d2		; Next char
		bra.s	\loop
\Done:	tst.w	d1
	beq.s	\Return
		neg.l	d0
\Return	rts

;void *kbd_queue (void); 
kbd_queue:
	lea	KBD_QUEUE,a0
	rts
	
bzero:
	move.l	4(a7),a0	; Address to fill with '0'
	move.w	8(a7),d0	; Size (!= 0 -Gcc won't called this function with size = 0 !)
	subq.w	#1,d0
	clr.w	d1
	\loop:	move.b	d1,(a0)+
		dbf	d0,\loop
	rts

;short QModeKey (short code);
QModeKey
	bsr.s	\PushT				; Push the next address
	dc.w	$110B,$0109,$010A,$102D		; It is not the return address,
	dc.w	$1109,$1036,$1108,$2051		; but the address of a table
	dc.w	$2057,$2048,$2052,$2054
	dc.w	$2059,0
\PushT	move.w	4+4(a7),-(a7)			; Push Code
	bsr.s	WordInList			; Check for it.
	addq.l	#6,a7
	rts

;short QSysKey (short code);
QSysKey:
	bsr.s	\PushT
	dc.w	$1035,$1032,$102B,$1033,0
\PushT	move.w	4+4(a7),-(a7)
	bsr.s	WordInList
	addq.l	#6,a7
	rts

;short WordInList (unsigned short Word, unsigned short *List); 
WordInList:
	move.w	4(a7),d1		; What to search
	move.l	6(a7),a0		; In table
WordInList_reg:
	moveq	#0,d0			; Fail
\loop		move.w	(a0)+,d2	; Read next word
		beq.s	\fail		; =0, end of table => Fail
		cmp.w	d1,d2		; Cmp 2 numbers
		bne.s	\loop		; Equal, success. Different, next 
	moveq	#1,d0			; Success
\fail	rts

; Find a file in the path (Shell Extension)
; In:
;	a4 -> File Name (ANSI)
; Out:
;	a0.l -> SYM_ENTRY
FindSymInPath:
	pea	(a2)
	; Search in the current or given folder.
	move.l	a4,a0
\cvt:		tst.b	(a0)+
		bne.s	\cvt
	clr.w	-(a7)
	pea	-1(a0)			; Push filename
	jsr	SymFindPtr		; Search for it
	addq.l	#6,a7
	move.l	a0,d0			; Success finding it ?
	bne.s	\Find
	; Search in the path
	lea	SHELL_PATH,a2		; PATH str finish by 2 0
\FolderLoop:	tst.b	(a2)		; =0, end of path
		beq.s	\End
		; Deref folder
		move.w	#FOLDER_LIST_HANDLE,a0
		move.l	a2,a1		; Find folder
		jsr	FindSymEntry
		move.l	a0,d0
		beq.s	\next		; Folder not found, next folder
			move.w	SYM_ENTRY.hVal(a0),a0
			move.l	a4,a1		
			jsr	FindSymEntry	; Search file in this folder
			move.l	a0,d0		; Success ?
			bne.s	\Find
\next:		tst.b	(a2)+		; Skip folder name
		bne.s	\next
		bra.s	\FolderLoop
\End	suba.l	a0,a0
\Find:	move.l	(a7)+,a2
	rts
			
; Executes a Shell Script (Shell Extension)
; In:
;	d0.w = Handle of the TEXT file.
ExecuteScriptFile:
	movem.l	a2/a4,-(a7)
	lea	(-SHELL_MAX_LINE*3-2)(a7),a7	; Temp buffer
	move.l	a7,a4				; Ptr to buffer
	clr.b	(a4)+				; NULL starting buffer.
	move.w	d0,-(a7)			; Get the file
	jsr	HLock				; and lock it !
	move.l	a0,d0				; Handle exists ?.
	beq.s	\NoScriptFile			;
	move.w	(a0)+,d0			; Read size
	move.b	-1(a0,d0.w),d1			; Read TAG
	cmpi.b	#$E0,d1				; Check if it is a TEXT TAG ?
	bne.s	\NoScriptFile			;
		addq.l	#3,a0			; Skip first infos.
		move.l	a0,a2			; First Line of command
		lea	ScriptHeader_str,a1	; Check signa
		moveq	#7,d0
		jsr	memcmp_reg		; Comp
		tst.w	d0
		bne.s	\NoScriptFile
			\Loop:	tst.b	(a2)		; EOF ?
				beq.s	\NoScriptFile
				cmpi.b	#$27,(a2)	; Comment ? '
				bne.s	\ExecuteLine
					; Next Line of command
						\SLoop:
						move.b	(a2)+,d0
						beq.s	\NoScriptFile
						cmpi.b	#$0D,d0
						bne.s	\SLoop
					addq.l	#1,a2		; Skip Next Char (Space)
					bra.s	\Loop
				\ExecuteLine:
					move.l	a4,a0
					; Next Line of command
						\ELoop:
						move.b	(a2)+,d0
						beq.s	\NoScriptFile
						move.b	d0,(a0)+
						tst.b	d0
						cmpi.b	#$0D,d0
						bne.s	\ELoop
					clr.b	-(a0)		; NULL
					addq.l	#1,a2		; Skip Next Char (Space)
					movem.l	d3-d7/a2-a6,-(a7)
					jsr	ShellExecuteCommand
					movem.l	(a7)+,d3-d7/a2-a6
					bra.s	\Loop
\NoScriptFile
	jsr	HeapUnlock		; Unlock folder
	lea	(SHELL_MAX_LINE*3+2+2)(a7),a7
	movem.l	(a7)+,a4/a2
	rts
	
;ESI StrToTokN (const char *src, unsigned char *dest);
StrToTokN:
	move.l	4(a7),a0		; ANSI src
	jsr	strlen_reg		; d0.l = src len
	move.l	4(a7),a0		; ANSI src
	move.l	8(a7),a1		; Tokn Dest
	lea	$14(a1),a1		; End of Tokn Dest
	add.l	d0,a0			; End of ANSI str
\loop		move.b	-(a0),-(a1)
		dbf	d0,\loop
	clr.b	(a1)
	move.l	8(a7),a0		; Tokn Dest
	lea	$14(a0),a0		; End of Tokn Dest
	rts

;short TokToStrN (unsigned char *dest, SYM_STR src);
TokToStrN:
	move.l	4(a7),a0		; ANSI dest
	move.l	8(a7),a1		; Tokn Src
\loop		tst.b	-(a1)		; From VAT to ANSI
		bne.s	\loop
	addq.l	#1,a1
\loop2:		move.b	(a1)+,(a0)+
		bne.s	\loop2
	moveq	#1,d0
	rts

;void HomePushEStack (void);
HomePushEStack:
;HANDLE HS_newFIFONode (void);
HS_newFIFONode:
;void HS_pushFIFONode (HANDLE Node);
HS_pushFIFONode:
	dc.w	$A010
	
;HANDLE HS_getFIFONode (unsigned short Index);
HS_getFIFONode:
;HANDLE HS_getEntry (unsigned short Index);
HS_getEntry:
;HANDLE HS_getAns (unsigned short Index);
HS_getAns:
;void HS_freeFIFONode (HANDLE Node);
HS_freeFIFONode:
;void HS_freeAll (void); 
HS_freeAll:
;HANDLE HS_deleteFIFONode (HANDLE Node);
HS_deleteFIFONode:
;void HS_chopFIFO (void);
HS_chopFIFO:
;unsigned short HS_countFIFO (void);
HS_countFIFO:
	moveq	#0,d0
	rts
	
;void HomeExecute (const char *Command, unsigned short ComLen);
HomeExecute:
	move.l	4(a7),a0
	move.w	8(a7),d0
	movem.l	d3-d7/a2-a6,-(a7)
	move.w	d0,d7
	add.w	d7,d7
	suba.w	d7,a7			; Stack Frame
	move.l	a7,a1			; Ptr
	subq.w	#1,d0			; for(i = 0 ; i < ComLen ; i++) *dest++ = *src++
	blt.s	\End
\Loop		move.b	(a0)+,(a1)+
		dbf	d0,\Loop	
	clr.b	(a1)			; Null String
	move.l	a7,a4
	jsr	ShellExecuteCommand
\End	adda.w	d7,a7			; Pop Frame
	movem.l	(a7)+,d3-d7/a2-a6
	rts

; Completion
; It is really written in 'I write until it works' style.
; Not good, but nevertheless it works well...
; In:
;	a0 -> Start of string
;	a1 -> End of string
Completion:
	movem.l	d0-d7/a0-a6,-(a7)
	suba.l	a6,a6
	clr.b	(a1)					; Make it null string
	clr.w	d4					; Length of the string
	clr.w	d6					; Number of successful entries
	;search back ' ' or start of string
\loop_char	cmp.l	a0,a1				; Check if the start of the string
		ble.s	\Done
		move.b	-(a1),d0			; Read next char
		cmpi.b	#' ',d0				; If ' ', then we have a word
		beq.s	\Done1
		cmpi.b	#'\',d0				; If '\', then it is a folder\file completion
		beq	\Folder
		addq.w	#1,d4				; One more char
		bra.s	\loop_char			; Next char
\Done1:	addq.l	#1,a1					; *a1 == ' ', so skip again ' '
\Done	; Search for a command which starts with a1 string
	tst.w	d4					; Is a char ?
	beq	\EndCompletion				; No char => No completion
	move.l	a1,a4					; Save the file in GReg4
	; Looks at the folder Table
	move.w	#FOLDER_LIST_HANDLE,a0
	bsr	CompletionFolderSearch
	moveq	#'\',d7					; Final char
	; Looks at the internal commands
	lea	CommandTable,a2
	move.l	a2,a3
\InternalSearchLoop:
		move.w	(a2),d0				; Read offset
		beq.s	\InternalSearchLoopEnd		; Check if zero
		lea	0(a3,d0.w),a0			; String Command Name
		bsr	CompletionCmp			; Check this command
		addq.l	#4,a2				; Next entry
		bra.s	\InternalSearchLoop
\InternalSearchLoopEnd
	; Looks at the files in the current folder
	move.w	#FOLDER_LIST_HANDLE,a0
	lea	CUR_FOLDER_STR,a1
	jsr	FindSymEntry
	move.w	SYM_ENTRY.hVal(a0),a0
	bsr	CompletionFolderSearch	
	; Looks the files in the PATH
	lea	SHELL_PATH,a2		; PATH str finish by 2 0
\FolderLoop:	tst.b	(a2)		; =0, end of path
		beq.s	\resolve
		; Deref folder
		move.w	#FOLDER_LIST_HANDLE,a0
		move.l	a2,a1		; Find folder
		jsr	FindSymEntry
		move.l	a0,d0
		beq.s	\NextFolder		; Folder not found, next folder
			move.w	SYM_ENTRY.hVal(a0),a0
			bsr	CompletionFolderSearch
\NextFolder	tst.b	(a2)+		; Skip folder name
		bne.s	\NextFolder
		bra.s	\FolderLoop
\Folder	; Find inside the given folder
	; a1 -> '\'
	lea	1(a1),a4				; File Ptr
	pea	(a1)					; Save ptr
	clr.b	(a1)					; Replace '\' by 0 (It is in RAM)
\Floop		cmp.l	a0,a1				; Search for the folder name
		ble.s	\FDone
		move.b	-(a1),d0			; Read char
		cmpi.b	#' ',d0				; Cmp
		bne.s	\Floop				; Continue
	addq.l	#1,a1
\FDone:		
	move.w	#FOLDER_LIST_HANDLE,a0
	jsr	FindSymEntry				; Find entry ?
	move.l	(a7)+,a1				; Reload ptr
	move.b	#'\',(a1)				; Restore string
	move.l	a0,d0
	beq	\EndCompletion
	move.w	SYM_ENTRY.hVal(a0),a0
	bsr	CompletionFolderSearch		
\resolve
	; Now we have a list of all the entries which may success
	subq.w	#1,d6					; = Number of success find
	blt.s	\EndCompletion
		; Now we have one entry
		lea	0(a6,d4.w),a1			; Ptr to entry (Remaining char).
		move.w	d5,d0				; d5 = Number of char to put
		sub.w	d4,d0				
		subq.w	#1,d0
		blt	\EndCompletion
\PutLoop		clr.w	d4
			move.b	(a1)+,d4
			bsr	AddKeyToFIFOKeyBuffer
			dbf	d0,\PutLoop
		move.w	d7,d4				; Add final ' ' or '\' character
		beq.s	\EndCompletion
			bsr	AddKeyToFIFOKeyBuffer
\EndCompletion
	movem.l	(a7)+,d0-d7/a0-a6
	rts

CompletionFolderSearch:
	trap	#3
	addq.w	#2,a0				; Skip Max
	move.w	(a0)+,d3			; Number of folders (at least one !)
	subq.w	#1,d3
	blt.s	\EndFolder
\Loop		bsr.s	CompletionCmp		; Compare and add to buffer
		lea	SYM_ENTRY.sizeof(a0),a0	; Next entry
		dbf	d3,\Loop	
\EndFolder
	rts
			
CompletionCmp:
	pea	(a0)
	move.l	a4,a1				; String Source
	move.w	d4,d2				; Length
	subq.w	#1,d2				; -1 for dbf
\IntCmp		cmpm.b	(a1)+,(a0)+		; Compare 
		dbne	d2,\IntCmp		; and decrement
	bne.s	\Ret
		; Add this string to the list of the possibility
		move.l	a6,d2
		beq.s	\FirstEntry
			; Find the max length of the 2 commands so that we put as mush char as possible
			move.l	a6,a1		; Old string
			move.l	(a7),a0		; New string
			moveq	#-1,d0		; Length
			\StrCmp:	addq.w	#1,d0
					tst.b	(a1)
					beq.s	\StrDone
					cmpm.b	(a1)+,(a0)+		; Compare 
					beq.s	\StrCmp		; 
			\StrDone:
				clr.w	d7	; No final char !
				cmp.w	d0,d5		; Get minimum between d5 and d0
				ble.s	\NewEntry
					move.w	d0,d5
					bra.s	\NewEntry
\FirstEntry	jsr	strlen
		move.w	d0,d5			; Length of command
		move.l	(a7),a6			; Save found command
		moveq	#' ',d7			; d7 = Final Char
\NewEntry	addq.w	#1,d6
\Ret	move.l	(a7)+,a0
	rts
	