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
;***            	VAT routines				***
;***                                                            ***
;******************************************************************

;******************************************************************
;***                                                            ***
;***            	Internal routines			***
;***                                                            ***
;******************************************************************

; Init the VAT. Must be called after HeapInit
VATInit:
	; Corrupt HEAP_TABLE so that I'll get correct handle for FOLDER_LIST_HANDLE and MAIN_HANDLE
	lea	HEAP_TABLE+4,a0
	moveq	#FOLDER_LIST_HANDLE-2,d0
	moveq	#-1,d1
\Loop		move.l	d1,(a0)+
		dbf	d0,\Loop
	; Alloc Folder Dir Handle
	pea	(144).w
	bsr	HeapAlloc	; Alloc enought space for a folder
	addq.l	#4,a7
	lea	InitError_str(pc),a0
	moveq	#FOLDER_LIST_HANDLE,d1
	move.w	d1,FolderListHandle
	cmp.w	d1,d0
	bne.s	\SYSTEM_ERROR
	move.w	d0,a0
	trap	#3
	move.l	#$000A0000,(a0)	; Write:000A:maxfiles, 0000:nb of files
	; Alloc 'main' directory
	lea	Main_str(pc),a1
	bsr	FolderAdd_a1
	lea	InitError_str(pc),a0
	moveq	#MAIN_LIST_HANDLE,d1
	move.w	d1,MainHandle
	cmp.w	d1,d0
	bne.s	\SYSTEM_ERROR
	; Set 'main' as the current directory
	lea	Main_str(pc),a1
	bsr	FolderCur_reg
	lea	InitError_str(pc),a0
	tst.w	d0
	beq.s	\SYSTEM_ERROR
	; Fix the HEAP_TABLE
	lea	HEAP_TABLE+4,a0
	moveq	#FOLDER_LIST_HANDLE-2,d0
\Loop2		clr.l	(a0)+
		dbf	d0,\Loop2
	rts
\SYSTEM_ERROR
	jmp	SYSTEM_ERROR

; Add a file in the VAT only if it is not already in the VAT.
; In:
;	a2 -> File Name	(AMS).
;	a3 -> File Ptr (In archive !)
VATAddSpecialFile:
	pea	(a2)				; Add file to the system ?
	bsr	SymFindPtr			; If it was already add in the archive
	move.l	a0,d0				; do not add it again
	bne.s	\Cont
		bsr	SymAdd			; Add the file
		bsr	DerefSym_Reg	
		move.l	a0,a5			; Save SYM_ENTRY
		bsr	HeapGetHandle		; Get an handle
		move.w	d0,SYM_ENTRY.hVal(a5)	; Save the handle
		move.w	#SF_ARCHIVED,SYM_ENTRY.flags(a5) ; Set as archived
		lsl.w	#2,d0			; x4 (size of an address)
		lea	HEAP_TABLE,a1		; Heap Table
		move.l	a3,0(a1,d0.w)		; Save the addr of the handle
\Cont	addq.l	#4,a7
	rts

; Destroy:
;	d0-d2/a0-a1
; a0 : handle of the list we look into
; a1 : name of the entry we look for
; Return:
;	a0 -> Sym Entry or NULL
FindSymEntry:
	move.w	d3,-(a7)
	trap	#3		; a0 : contents of the list
	addq.l	#2,a0		; skips the 1st word
	move.w	(a0)+,d3	; d2 = nb items in the list
	beq.s	\false		; -> not found

	subq.w    #1,d3		; - 1 for dbra
\search
		movem.l	a0/a1,-(a7)
		bsr	SymCmp    		; compares the strings a0 & a1
		movem.l	(a7)+,a0/a1		; restores a0&a1
		tst.w	d0			; (a0) = (a1) ?
		beq.s	\end			; -> found !
		lea	SYM_ENTRY.sizeof(a0),a0        ; else VAT pointer += 14
		dbra	d3,\search        	; search again
\false	suba.l	a0,a0            ; a0 = 0, entry not found
\end	move.w	(a7)+,d3
	rts

; Create an item in a list at the end.
; Don't forget to call SortList after completing the VAT to keep a sorted VAT !
; Input:
;	a0: hdl of a list of files or folders
; Out:
;	a0.l = Created VAT entry (To fill !) or NULL
; Destroy:
;	d0-d2/a0-a1
CreateItem:
	move.w	a0,-(a7)	; Push Handle Folder
	trap	#3
	addq.w	#1,2(a0)	; nbfiles + 1
	move.w	(a7),a0
	bsr.s	resize		; Resizes the list if needed
	move.w	(a7)+,a0	; Read handle folder
	trap	#3		; Deref it
	tst.w	d0		; enough mem ?
	bne.s	\OK		; -> yes
		subq.w	#1,2(a0)	; If it doesn't work, we remove one file
		suba.l	a0,a0		; Error
		rts
\OK	; VAT address of the last item
	move.w	2(a0),d1
	subq.w	#1,d1
	mulu.w	#SYM_ENTRY.sizeof,d1
	lea	4(a0,d1.l),a0		; Get address 
	rts


;    Resizes a folder
;
;Input: a0.w = Folder Handle
; Destroy: d0-d2/a0-a1
resize:
	move.w	a0,d1			; Save Handle
	trap	#3
	move.w	(a0)+,d2		; d2: max number of files before resize
	cmp.w	(a0),d2			; cmp maxfiles,nbfiles
	bge.s	\lower			; if <= -> list is big enough
		addq.w	#8,d2		; else resize:+8 to maxfiles
\res    move.w	d2,-(a0)		; Set new Max Files in the folder
	mulu.w	#SYM_ENTRY.sizeof,d2	; a file requires 14 bytes in VAT: d1*14 bytes needed for d1 files
	addq.l	#4,d2			; 2 bytes for nbfiles and 2 for maxfiles
	move.l	d2,-(a7)		; new size of the handle 
	move.w	d1,-(a7)		; Hdl of the fold 
	bsr	HeapRealloc		; resizes the list
	addq.l	#6,a7
	rts
\lower	subq.w	#8,d2			; -8 to maxfiles
	cmp.w	(a0),d2			; Test if it's possible to decrease maxfiles
	bge.s    \res    
	moveq	#1,d0			; Ok !
	rts

;sortlist(list handle)
;	Sorts file/folder list in alphabetical order
;	Not very fast, but I don't think it is very important
;Input:
;	a0 = file/folder list handle
;Output: nothing
SortList:
	pea	(a2)
	trap	#3		; Deref Folder handle
	addq.l	#2,a0		; Skip max files
	move.w	(a0)+,d2	; Read number of files
	subq.w	#1,d2		; If one item:end
	ble.s	\fin
	mulu.w	#SYM_ENTRY.sizeof,d2		; End - 1
	lea	SYM_ENTRY.sizeof(a0,d2.l),a2	; End Ptr
\suite		move.l	a0,a1	; Second Item
\loop			lea	SYM_ENTRY.sizeof(a1),a1	; Next Item
			cmp.l	a2,a1		; End of Folder ?
			bge.s	\stop		; Yes, so quit the 2nd loop
			move.l	(a1),d0
			cmp.l	(a0),d0		; compare the two names: A:65 B:66,.. so B is greater than A
			bhi.s	\loop		; if item with index d2+1 have a name smaller than the item with index d2:exchange them
			bcs.s	\echan
			move.l	4(a1),d0	; If equal
			cmp.l	4(a0),d0
			bhi.s	\loop
\echan:			bsr.s	Exchange
			bra.s	\loop
\stop:		lea	SYM_ENTRY.sizeof(a0),a0	; Next Item
		cmp.l	a2,a0			; End ?
		blt.s	\suite
\fin	move.l	(a7)+,a2
	rts

; Exg 2 items
; In:
;	a0 -> Sym
;	a1 -> Sym
; Destroy:
;	d0/d1
Exchange:
	moveq	#SYM_ENTRY.sizeof-1,d0
\exg:		move.b	(a0),d1
		move.b	(a1),(a0)+
		move.b	d1,(a1)+
		dbra	d0,\exg
	lea	-SYM_ENTRY.sizeof(a0),a0
	lea	-SYM_ENTRY.sizeof(a1),a1
	rts

; Delete an Item (And not the associated handle !)
; Input:
;	a0: SYM_ENTRY of the file to delete.
DeleteItem:
	pea	(a0)		; Push SYM and save it
	bsr	HeapPtrToHandle	; This version of HeapPtr is more powerfull than tios one and allows to get the handle folder ;)
	move.w	d0,d2		; Save folder handle
	move.w	d0,a0		; Get Folder Handle
	trap	#3		; Deref Folder handle
	addq.l	#2,a0		; Skip max files
	move.w	(a0),d1		; Read number of files
	subq.w	#1,d1		; -1 to nbfiles
	move.w	d1,(a0)+	; Save number of files
	mulu.w	#SYM_ENTRY.sizeof,d1		; End - 1
	add.l	d1,a0		; End Ptr
	move.l	(a7)+,a1	; Read Sym
	bsr.s	Exchange	; Exchange the last entry and the file to delete.
	move.w	d2,a0		; Folder handle
	move.w	d2,-(a7)	; Push Folder Handle
	bsr	resize		; Resize &
	move.w	(a7)+,a0	; Get Folder Handle (It may be destroyed by resize)
	bra	SortList	; Sort the list

; It returns d0 = HSYm and a0 = SYM_ENTRY of folder
FindFolderHSymFromFileHSym:
	move.l	4(a7),d0
FindFolderHSymFromFileHSym_Reg:
	swap	d0		; Get Folder Handle
	move.w	#FOLDER_LIST_HANDLE,a0
	trap	#3
	move.l	a0,a1
	addq.w	#2,a0
	move.w	(a0)+,d1
	subq.w	#1,d1
\loop		cmp.w	SYM_ENTRY.hVal(a0),d0
		beq.s	\found
		lea	SYM_ENTRY.sizeof(a0),a0
		dbf	d1,\loop
	moveq	#0,d0	; Illegal
	rts
\found	move.l	a0,d0
	sub.l	a1,d0
	swap	d0
	move.w	#FOLDER_LIST_HANDLE,d0
	swap	d0
	rts

; In :
;	a0 -> SYM_STR
;	a0 = 0,"main\toto",0
; Out:
;	a0 -> "main",0
;	a1 -> "toto",0
; Note:
;	Use internally FOLDER_TEMP
GetFolderNameAndFileName:
	moveq	#0,d0
	suba.l	a1,a1				; No file Name ptr
\cvt		cmp.b	#'\',d0
		bne.s	\no_folder
			lea	1(a0),a1	; File Name Ptr	
\no_folder:	move.b	-(a0),d0	; To ANSI
		bne.s	\cvt
	addq.l	#1,a0
	move.l	a1,d0
	bne.s	\Copy
		move.l	a0,a1			; File name
		lea	CUR_FOLDER_STR,a0	; Folder name
		rts
\Copy:	; Copy Folder Name for Zero Padding
	pea	(a1)
	lea	FOLDER_TEMP,a1
	moveq	#8-1,d1			; Max = 8 chars
\loop		move.b	(a0)+,d0
		cmpi.b	#'\',d0
		beq.s	\done
		move.b	d0,(a1)+
		dbf	d1,\loop	
\done	clr.b	(a1)
	lea	FOLDER_TEMP,a0		; FOLDER NAME
	move.l	(a7)+,a1		; FILE NAME
	rts

; From SYM_ENTRY to HSym
; More usefull than MakeHSym since it find by itself the folder !
; Note: It is due to the fact that the implementation of HeapPtrToHandle if far more better than original one !
; In :a0 -> SYM
; Out: d0 = HSym
; Destroy only d0 !
Sym2HSym:
	movem.l	d1/d2/a0/a1,-(a7)	; Push Registers
	pea	(a0)			; Push SYM
	bsr	HeapPtrToHandle		; Get handle of folder
	move.w	d0,-(a7)		; Push Handle Folder
	bsr	HeapDeref		; Get Org of folder
	moveq	#0,d0
	move.w	(a7)+,d0		; Read handle folder
	move.l	(a7)+,a1		; Read SYM
	swap	d0			; Hiword = Handle folder
	sub.l	a0,a1			; Get the offset
	move.w	a1,d0			; Write it 
	movem.l	(a7)+,d1/d2/a0/a1	; Pop registers
	rts



;******************************************************************
;***                                                            ***
;***            	TiOs Like routines			***
;***                                                            ***
;******************************************************************

;short QSysProtected (ESQ Tag);
QSysProtected
	moveq	#1,d0
	move.b	5(a7),d1
	cmpi.b	#$DC,d1
	bcs.s	\No1
	cmpi.b	#$E2,d1
	bls.s	\Done
\No1	cmpi.b	#$F3,d1
	beq.s	\Done
	cmpi.b	#$F8,d1
	beq.s	\Done
	moveq	#0,d0
\Done	rts
	

;short SymCmp (const char *s1, const char *s2);
SymCmp:
	move.l	4(a7),a0
	move.l	8(a7),a1
	pea	(8).w
	pea	(a0)
	pea	(a1)
	bsr	strncmp
	lea	12(a7),a7
	rts

;void SymCpy (char *dest, const char *src);
SymCpy:
	move.l	4(a7),a0	; Dest
	move.l	8(a7),a1	; Src
	moveq	#8-1,d0
\loop		move.b	(a1)+,(a0)+
		dbeq	d0,\loop
	rts

;void SymCpy0 (char *dest, const char *src);
SymCpy0:
	move.l	4(a7),a0	; Dest
	move.l	8(a7),a1	; Src
	moveq	#8-1,d0
\loop		move.b	(a1)+,(a0)+
		dbeq	d0,\loop
	clr.b	(a0)
	rts

;HSym MakeHSym (HANDLE FldHandle, const SYM_ENTRY *SymPtr);
MakeHsym:
	move.w	4(a7),a0	; Folder Handle
	trap	#3		; Folder Ptr *
	move.l	6(a7),a1	; SYM_ENTRY *	
	move.l	a1,d0
	beq.s	\end
		moveq	#0,d0
		move.w	4(a7),d0
		swap	d0
		sub.l	a0,a1
		move.w	a1,d0
\end:	rts

; Side effect:
;	return in a1 the folder ptr
;	It doesn't destroy d0,d2 (d0 if called with _reg)
;	It checks if the Hsym is (a few) valid.
DerefSym:
	move.l	4(a7),d0
DerefSym_Reg:
	tst.l	d0
	beq.s	\Error
		swap	d0
		movea.w	d0,a0		; Get Folder Handle
		swap	d0
		trap	#3		; Deref it
		move.l	a0,a1		; Folder Tab
		move.w	2(a1),d1	; Number of files in d1
		mulu.w	#SYM_ENTRY.sizeof,d1
		addq.w	#4,d1		; Maximum offset !
		cmp.w	d0,d1
		bls.s	\Error
			add.w	d0,a0
			rts
\Error	suba.l	a0,a0
	rts

;short IsMainFolderStr (const char *Name);
IsMainFolderStr:
	move.l	4(a7),a0
IsMainFolderStr_reg:
	lea	Main_str(Pc),a1
	bsr	strcmp_reg
	tst.w	d0
	seq	d0
	ext.w	d0
	rts

;void ResetSymFlags (unsigned short Flags);
ResetSymFlags
	move.w	d3,-(a7)
	move.w	2+4(a7),d3
	move.w	#2,-(a7)
	clr.l	-(a7)
	bsr	SymFindFirst
	addq.l	#6,a7
\loop		and.w	d3,SYM_ENTRY.flags(a0)
		bsr	SymFindNext
		move.l	a0,d0
		bne.s	\loop
	move.w	(a7)+,d3
	rts

;HSym checkCurrent (SYM_STR SymName, ESQ Type);
checkCurrent
	move.l	4(a7),-(a7)
	bsr	SymFind		; Find Sym
	move.l	d0,(a7)
	beq.s	\Fail
		bsr	DerefSym
		move.w	SYM_ENTRY.hVal(a0),a0
		bsr	HToESI_reg
		move.b	12(a7),d0
		cmp.b	(a0),d0
		bne.s	\Fail
			clr.l	(a7)
\Fail:	move.l	(a7)+,d0
	rts
	
; *** Folder op ***

;HANDLE FolderAdd (SYM_STR SymName);
FolderAdd:
	move.l	4(a7),a1
\cvt		tst.b	-(a1)		; Traditionnal convertion from TI to ANSI...
		bne.s	\cvt
	addq.l	#1,a1
FolderAdd_a1:
	pea	(a1)
	bsr	ValidateSymName
	tst.w	d0
	beq.s	\error			; Sym Name is not valid
	move.l	(a7),a1
	move.w	#FOLDER_LIST_HANDLE,a0
	bsr	FindSymEntry		; Find in folder a0 file a1
	moveq	#0,d0			; HANDLE H_NULL
	move.l	a0,d1
	bne.s	\error			; Found ? Yes => Error
		pea	(144).w
		bsr	HeapAlloc	; Alloc enought space for a folder
		move.w	d0,(a7)		; Push handle and check it
		beq.s	\error2
			move.w	d0,a0	; Folder with no files
			trap	#3
			move.l	#$000A0000,(a0)	;write:000A:maxfiles, 0000:nb of files
			move.w	#FOLDER_LIST_HANDLE,a0
			bsr	CreateItem	; Create a new folder in the folder list
			move.l	a0,d0
			beq.s	\error3
				move.l	4(a7),a1	; Get Name
				moveq	#8-1,d0
\loop					move.b	(a1)+,(a0)+
					dbf	d0,\loop
				move.l	#$00030080,(a0)+	; Compat + Flags
				move.w	(a7),(a0)+		; Handle
				move.w	#FOLDER_LIST_HANDLE,a0
				bsr	SortList
				bra.s	\error2
\error3		bsr	HeapFree	; Free folder handle
		clr.w	(a7)		; Set to H_NULL
\error2		move.w	(a7)+,d0	; Reload HANDLE (I should return HANDLE)
		addq.l	#2,a7		; Pop size 
\error	addq.l	#4,a7		; Pop a1
	rts
	
;unsigned short FolderCount (const SYM_ENTRY *SymPtr);
FolderCount:
	move.l	4(a7),a0
	move.w	SYM_ENTRY.hVal(a0),a0
	trap	#3
	move.w	2(a0),d0
	rts
	
;short FolderFind (SYM_STR SymName);
FolderFind:
	move.l	4(a7),a1		; SymName
\cvt		tst.b	-(a1)		; Convert from Token to ANSI str
		bne.s	\cvt
	addq.l	#1,a1			; a1 -> Name
	move.w	#FOLDER_LIST_HANDLE,a0	; a0.w = Folder Handle
	bsr	FindSymEntry		; Find in folder a0 file a1
	moveq	#4,d0			; d0.w = NOT_FOUND
	move.l	a0,d1
	beq.s	\Return
		move.l	a1,a0
		bsr	IsMainFolderStr_reg	; d0=0 if main, -1 if not
		neg.w	d0			; d0=0 if main, 1 if not
		addq.w	#2,d0			; d0=2 if main, 3 if not
\Return	rts

;short FolderOp (SYM_STR SymName, short Flags);
FolderOp:
	move.w	8(a7),d2		; Read flags
	btst.l	#7,d2			; Check if we lock all the folders ?
	bne.s	\all_folder		; Yes

	move.l	4(a7),a1		; Lock one folder
\cvt		tst.b	-(a1)		; Translate the name to ANSI string.
		bne.s	\cvt
	addq.l	#1,a1			; A1 -> ANSI Name of the folder to lock

	move.w	#FOLDER_LIST_HANDLE,a0
	cmpi.b	#$7F,(a1)		; Folder list itself ?
	beq.s	\found
		bsr	FindSymEntry	; Find in folder a0 file a1
		move.l	a0,d0
		beq.s	\end
			move.l	SYM_ENTRY.hVal(a0),a0
\found:	; In : a0 = Handle of the Folder to lock
	moveq	#1,d0
	and.w	8(a7),d0		; Read flags
	beq	\Unlock
	bsr	HeapLock_reg
	bra.s	\Success

\Unlock:
	bsr	HeapUnlock_reg
	bra.s	\Success

\all_folder:
	; Get function to use
	lea	HeapUnlock_reg(pc),a1
	moveq	#1,d0
	and.w	8(a7),d0		; Read flags
	beq.s	\well
		lea	HeapLock_reg(pc),a1
\well:	
	move.w	#FOLDER_LIST_HANDLE,a0
	bsr	HeapLock_reg
	move.w	#FOLDER_LIST_HANDLE,a0
	trap	#3			; Deref
	addq.l	#2,a0
	move.w	(a0)+,d2
	subq.w	#1,d2
	blt.s	\end2
\loop:		
		movem.l	a0/a1/d2,-(a7)
		move.w	SYM_ENTRY.hVal(a0),a0
		jsr	(a1)
		movem.l	(a7)+,a0/a1/d2
		lea	SYM_ENTRY.sizeof(a0),a0
		dbf	d2,\loop
\end2	move.w	#FOLDER_LIST_HANDLE,a0
	jsr	(a1)
\Success
	moveq	#1,d0
\end:	rts
	
;short FolderRename (const char *SrcName, const char *DestName);
FolderRename:
	move.l	4(a7),a1	; Src name
\cvt		tst.b	-(a1)	; To Token to ANSI
		bne.s	\cvt
	addq.l	#1,a1					; Check for Src name
	move.w	#FOLDER_LIST_HANDLE,a0			; In home
	bsr	FindSymEntry				; Does Src name exist ?
	move.l	a0,d0					; No so quit
	beq.s	\end
		pea	(a0)				; Push and preserve SYM_ENTRY of folder
		lea	Main_str(pc),a1
		bsr	strcmp_reg			; Cmp string
		tst.w	d0				; Is it main folder ?
		beq.s	\end2				; Yes so quit
			move.l	8(a7),a1		; Check dest name
			pea	(a1)			; Preserve a1 and push argument
			bsr	ValidateSymName
			move.l	(a7)+,a1		; Reload Dest Name
			tst.w	d0			; Check if name is ok ?
			beq.s	\end2			
\cvt2				tst.b	-(a1)		; To Token to ANSI
				bne.s	\cvt2
			addq.l	#1,a1			; Find it 
			move.w	#FOLDER_LIST_HANDLE,a0	; In home folder
			bsr	FindSymEntry		; Does Dest name exist ?
			move.l	a0,d0			; Check for existing folder.
			bne.s	\end2			; A1 is not destroyed.
				move.l	(a7),a0		; SYM_ENTRY of folder
				moveq	#8-1,d0
\loop					move.b	(a1)+,(a0)+	; Copy Folder new name
					dbf	d0,\loop
\end2:		addq.l	#4,a7
\end:	rts
	
;short FolderCur (SYM_STR SymName, short nonSys);
FolderCur:
	move.l	4(a7),a1	
\cvt		tst.b	-(a1)
		bne.s	\cvt
	addq.l	#1,a1
FolderCur_reg:
	move.w	#FOLDER_LIST_HANDLE,a0
	bsr	FindSymEntry
	move.l	a0,d0
	beq.s	\end		; Folder doesn't exist
		lea	CUR_FOLDER_STR,a1
		moveq	#8-1,d0
\loop			move.b	(a0)+,(a1)+
			dbf	d0,\loop
		clr.b	(a1)		; Null char
		st.b	HELP_BEING_DISPLAYED
		bsr	ST_eraseHelp
		moveq	#1,d0		; Success
\end:	rts

;void FolderGetCur (char *buffer);
FolderGetCur:
	move.l	4(a7),a0		; Dest
	lea	CUR_FOLDER_STR,a1	; Src
	bra	strcpy_reg

;short FolderDel (SYM_STR SymName, short flag); 
FolderDel:
	move.l	4(a7),a1
\cvt:		tst.b	-(a1)		; From SYM_STR to ANSI str
		bne.s	\cvt
	addq.l	#1,a1
	; Check Folder Current ?
	pea	CUR_FOLDER_STR	; Src
	pea	(a1)
	bsr	SymCmp
	tst.w	d0
	bne.s	\NoCurrent		; delete 
		lea	Main_str(pc),a1
		bsr	FolderCur_reg	; Set main as current
\NoCurrent:
	move.l	(a7)+,a1
	addq.l	#4,a7			; Skip a0
	move.w	8(a7),d0
	movem.l	d3-d4/a2,-(a7)
	move.w	d0,d4			; Delete all entries in Folder ?
	move.w	#FOLDER_LIST_HANDLE,a0
	bsr	FindSymEntry
	move.l	a0,d0		; Folder not found
	beq.s	\End
		pea	(a0)	; Push SYM ENTRY to delete according to d4
		move.w	SYM_ENTRY.hVal(a0),a0
		trap	#3
		ori.w	#$8000,-(a0)	; Lock folder handle
		lea	4(a0),a2
		move.w	(a2)+,d3
		subq.w	#1,d3
		blt.s	\NoFile
\loop			move.l	a2,a0
			bsr	SymDel_Entry	; Delete SYM
			lea	SYM_ENTRY.sizeof(a2),a2
			dbf	d3,\loop
\NoFile:	move.l	(a7),a0
		move.w	SYM_ENTRY.hVal(a0),a0
		bsr	HeapUnlock_reg		; Unlock folder
		tst.w	d4
		bne.s	\End2	; Do not destroy folder
			move.l	(a7),a0
			bsr	IsMainFolderStr_reg
			tst.w	d0
			bne.s	\End2		; Do not delete main
				move.l	(a7),a0
				move.w	SYM_ENTRY.hVal(a0),-(a7)	; Push Handle to delete
				bsr	DeleteItem	; Delete SYM_ENTRY
				bsr	HeapFree	; Free Folder Handle
				addq.l	#2,a7	
\End2		
		addq.l	#4,a7
		st.b	d0
\End:	movem.l	(a7)+,d3-d4/a2
\Ret	rts
	
; *** a SYM NAME ***

;short ValidateSymName (const char *VarName);
ValidateSymName:
	move.l	4(a7),a0		; Str File Name
ValidateSymName_reg
	move.l	a0,d0
	beq.s	\End			; NULL Ptr
	moveq	#0,d2			; Cpt 
	moveq	#0,d0			; False
	move.b	(a0)+,d1		; Void String ?
	beq.s	\End
	addq.l	#1,d2
	cmpi.b	#'a'-1,d1
	bls.s	\End
	cmpi.b	#'z',d1
	bhi.s	\End
\loop		move.b	(a0)+,d1
		beq.s	\done
		addq.l	#1,d2
		; '0' -> '9' / 'a' -> 'z' / '_'
		cmpi.b	#'_',d1
		beq.s	\loop
		cmpi.b	#'0'-1,d1
		bls.s	\End
		cmpi.b	#'9',d1
		bls.s	\loop
		cmpi.b	#'a'-1,d1
		bls.s	\End
		cmpi.b	#'z',d1
		bls.s	\loop
		bra.s	\End
\done:	subq.w	#8,d2
	sls	d0
\End	rts
		
;short HSYMtoName (HSym Sym, char *buffer);
; Side Effect:
;	Return in a1 the end of the buffer.
HSYMtoName:
	move.l	4(a7),d0
	bsr	FindFolderHSymFromFileHSym_Reg
	move.l	a0,d0
	beq.s	\End
		move.l	8(a7),a1
		moveq	#8-1,d0
\loopFolder		move.b	(a0)+,d1
			beq.s	\EndFolder
			move.b	d1,(a1)+
			dbf	d0,\loopFolder
\EndFolder	move.b	#'\',(a1)+
		move.l	4(a7),d0
		pea	(a1)
		bsr	DerefSym_Reg
		move.l	(a7)+,a1
		move.l	a0,d0
		beq.s	\End
			moveq	#8-1,d0
\loopFile			move.b	(a0)+,d1
				beq.s	\EndFile
				move.b	d1,(a1)+
				dbf	d0,\loopFile
\EndFile		clr.b	(a1)
\End:	rts


; *** FInd a SYM ***

;HSym VarRecall (SYM_STR SymName, unsigned short Flags);
VarRecall:			; forget the flag...

;HSym SymFind (SYM_STR SymName);
SymFind:
	clr.w	-(a7)
	bra.s	SymFind_entry
	
;HSym SymFindMain (SYM_STR SymName);
SymFindMain:
	move.w	#4,-(a7)
SymFind_entry:
	move.l	4+2(a7),-(a7)
	bsr.s	SymFindPtr
	addq.l	#6,a7
	move.l	a0,d0
	bne	Sym2HSym	; Calculate HSym
	rts
		
;SYM_ENTRY *SymFindPtr (SYM_STR SymName, unsigned short Flags); 
SymFindPtr:
	move.l	4(a7),a0			; SYM_STR
	bsr	GetFolderNameAndFileName
	cmp.w	#4,8(a7)
	bne.s	\NoMain
		lea	Main_str(pc),a0		; Folder = Main Folder
\NoMain	; Folder Str to Folder Handle in a0
SymFindPtr_Entry
	pea	(a1)			; Push FileName
	move.l	a0,a1			; a1 = Folder ANSI name
	move.w	#FOLDER_LIST_HANDLE,a0	; a0 = Folder Handle
	bsr	FindSymEntry		; Find SYM_ENTRY
	move.l	(a7)+,a1		; a1 File Name to search
	move.l	a0,d0
	beq.s	\Error
		move.w	SYM_ENTRY.hVal(a0),a0	; Folder Handle
		bsr	FindSymEntry		; Find a1 in Handle a0
\Error:	rts
	
;HSym FindSymInFolder (SYM_STR SymName, const char *FolderName);
FindSymInFolder:
	move.l	4(a7),a1
\cvt1		tst.b	-(a1)
		bne.s	\cvt1
	addq.l	#1,a1
	move.l	8(a7),a0
\cvt2		tst.b	-(a0)
		bne.s	\cvt2
	addq.l	#1,a0
	bsr.s	SymFindPtr_Entry
	move.l	a0,d0
	bne	Sym2HSym	; Calculate HSym
	rts
	
;HSym SymFindHome (SYM_STR SymName);
SymFindHome:
	move.l	4(a7),a0	
\cvt:		tst.b	-(a0)
		bne.s	\cvt
	addq.l	#1,a0			; ANSI Ptr
	move.l	a0,a1			; a1 = ANSI name
	move.w	#FOLDER_LIST_HANDLE,a0	; a0 = Folder Handle
	bsr	FindSymEntry		; Find SYM_ENTRY
	bra	Sym2HSym		; Calculate HSym from Sym
	
;SYM_ENTRY *SymFindFirst (SYM_STR SymName, unsigned short Flags); 
SymFindFirst:
	moveq	#3,d0
	and.w	8(a7),d0		; Flags
	move.w	d0,SYMFIND_FLAGS	; Save flags as global (Forget temp, twin, return_folder and collapse)
	cmp.w	#FO_SINGLE_FOLDER,d0
	bne.s	\Home
		move.l	4(a7),a1		; Folder Name
		move.l	a1,d0			; If NULL => Home directory
		beq.s	\Home
\cvt:			tst.b	-(a1)
			bne.s	\cvt
		addq.l	#1,a1
		cmpi.b	#$7F,(a1)		; Home Name ?
		beq.s	\Home			; yes do ome
		move.w	#FOLDER_LIST_HANDLE,a0	; Folder Handle in a0
		bsr	FindSymEntry
		move.l	a0,d0
		beq.s	SymFindNextError
		move.w	SYM_ENTRY.hVal(a0),a0
		bra.s	\folder
\Home:	move.w	#FOLDER_LIST_HANDLE,a0	; Folder Handle in a0
\folder:
	; Get the first entry
	trap	#3			; Deref it
	addq.l	#2,a0			; Skip MAX
	move.w	(a0)+,d1		; Read number of files
	bsr	Sym2HSym		; Get the first SYM_ENTRY
	tst.w	d1			; No file in folder
	beq.s	SymFindNextError
	move.l	d0,SYMFIND_HSYM
	rts
	
SymFindNextError:
	clr.l	SYMFIND_HSYM
	suba.l	a0,a0
	rts
	
;SYM_ENTRY *SymFindNext (void); 
SymFindNext:
	suba.l	a0,a0
	move.l	SYMFIND_HSYM,d0
	beq.s	SymFindNextError
		cmpi.w	#2,SYMFIND_FLAGS
		bne.s	\NoRecurse
			bsr	DerefSym_Reg		; a0 -> SYM_ENTRY of the file / a1 -> Folder
			move.w	SYM_ENTRY.flags(a0),d1
			andi.w	#SF_FOLDER,d1		; Not a folder, so no recurse 
			beq.s	\NoRecurse
				; Go down the folder : first entry
				move.w	SYM_ENTRY.hVal(a0),a0
				trap	#3
				addq.l	#2,a0
				move.w	(a0)+,d1
				bsr	Sym2HSym	; First entry
				move.l	d0,SYMFIND_HSYM
				tst.w	d1
				beq.s	\error
				rts
\error:	cmpi.w	#2,SYMFIND_FLAGS		; Recurse
	bne.s	SymFindNextError
		move.l	SYMFIND_HSYM,d0
		bsr	FindFolderHSymFromFileHSym_Reg	; Folder high Ok
		; Next Hsym in folder table
		add.w	#SYM_ENTRY.sizeof,d0
		move.l	d0,SYMFIND_HSYM
		bsr	DerefSym_Reg		; a0 -> SYM_ENTRY of the folder / a1 -> Folder Home
		move.l	a0,d0
		beq.s	SymFindNextError
		rts
\NoRecurse:	; Next Hsym
		add.w	#SYM_ENTRY.sizeof,d0
		move.l	d0,SYMFIND_HSYM
		bsr	DerefSym_Reg		; a0 -> SYM_ENTRY of the file / a1 -> Folder
		move.l	a0,d0
		beq.s	\error
		rts

;SYM_ENTRY *SymFindPrev (void); 
SymFindPrev:
	suba.l	a0,a0
	move.l	SYMFIND_HSYM,d0
	beq	SymFindNextError
		cmpi.w	#2,SYMFIND_FLAGS
		bne.s	\NoRecurse
			bsr	DerefSym_Reg		; a0 -> SYM_ENTRY of the file / a1 -> Folder
			move.w	SYM_ENTRY.flags(a0),d1
			andi.w	#SF_FOLDER,d1		; Not a folder, so no recurse 
			beq.s	\NoRecurse
				; Go Up the folder : Last entry of the previous folder.
				sub.w	#SYM_ENTRY.sizeof,d0	; Previous Folder
				blt	SymFindNextError
				bsr	DerefSym_Reg		; a0 -> SYM_ENTRY of the file / a1 -> Folder
				move.w	SYM_ENTRY.hVal(a0),a0
				trap	#3
				addq.l	#2,a0			; Skip Number of entries
				move.w	(a0)+,d1		; Last Entry
				beq.s	\error			; Error if no file
				subq.w	#1,d1			; Minus one
				mulu.w	#SYM_ENTRY.sizeof,d1
				adda.l	d1,a0			; a0 = SYM_ENTRY
				bsr	Sym2HSym		; First entry
				move.l	d0,SYMFIND_HSYM
				tst.w	d1
				beq.s	\error
				rts
\error:	cmpi.w	#2,SYMFIND_FLAGS				; Recurse
	bne	SymFindNextError				; No Recurse so end
		move.l	SYMFIND_HSYM,d0
		bsr	FindFolderHSymFromFileHSym_Reg	; Folder high Ok
		move.l	d0,SYMFIND_HSYM			; Folder in d0.l
		bsr	DerefSym_Reg			; a0 -> SYM_ENTRY of the folder / a1 -> Folder Home
		move.l	a0,d0
		beq	SymFindNextError
		rts
\NoRecurse:	; Previous Hsym
		sub.w	#SYM_ENTRY.sizeof,d0
		blt.s	\error
		move.l	d0,SYMFIND_HSYM
		bsr	DerefSym_Reg		; a0 -> SYM_ENTRY of the file / a1 -> Folder
		move.l	a0,d0
		beq.s	\error
		rts

;char *SymFindFolderName (void);
SymFindFoldername:
	move.l	SYMFIND_HSYM,d0
	beq.s	\Main
		move.l	d0,d1
		swap	d1
		cmpi.w	#FOLDER_LIST_HANDLE,d1
		beq.s	\Main
			bsr	FindFolderHSymFromFileHSym_Reg
			bra	DerefSym_Reg
\Main:	lea	Main_str(pc),a0
	rts
		
; *** Temp folders ***
;SYM_STR TempFolderName (unsigned short TempNum); 
TempFolderName:
	move.w	4(a7),-(a7)
	pea	TempFolder_str(pc)
	pea	FOLDER_TEMP+1
	bsr	sprintf
	lea	10(a7),a7
	clr.b	FOLDER_TEMP
	lea	FOLDER_TEMP+5,a0
	rts

;short FolderCurTemp (SYM_STR SymName);
FolderCurTemp:				; Nothing to do ?
	rts
	
;const char *FolderAddTemp (void); 
FolderAddTemp:
	move.w	TEMP_FOLDER_COUNT,d0
	addq.w	#1,d0
	move.w	d0,TEMP_FOLDER_COUNT
	move.w	d0,-(a7)
	bsr.s	TempFolderName
	pea	(a0)
	bsr	FolderAdd
	tst.w	d0
	bne.s	\Ok
		dc.w	$A000+890	; Memory
\Ok	bsr	FolderCurTemp
	move.l	(a7),a0
	addq.l	#6,a7
	rts

;void FolderDelTemp (void);
FolderDelTemp:
	move.w	TEMP_FOLDER_COUNT,d0
	beq.s	\end
	move.w	d0,-(a7)
	bsr.s	TempFolderName
	clr.w	(a7)			; FALSE
	pea	(a0)
	bsr	FolderDel
	subq.w	#1,TEMP_FOLDER_COUNT
	addq.l	#6,a7
\end	rts

;void FolderDelAllTemp (short StartTempNum);
FolderDelAllTemp:
	move.w	d3,-(a7)
	move.w	TEMP_FOLDER_COUNT,d3
	sub.w	6(a7),d3
\loop		bsr	FolderDelTemp
		dbf	d3,\loop
	move.w	(a7)+,d3
	rts
				
; *** Add a SYM ***

;HSym AddSymToFolder (SYM_STR SymName, SYM_STR FolderName);
AddSymToFolder:
	move.l	4(a7),a1
\cvt1		tst.b	-(a1)
		bne.s	\cvt1
	addq.l	#1,a1
	move.l	8(a7),a0
\cvt2		tst.b	-(a0)
		bne.s	\cvt2
	addq.l	#1,a0
	bra.s	SymAdd_entry
		
;HSym SymAddMain (SYM_STR SymName);
SymAddMain:
	move.l	4(a7),a0			; SYM_STR
	move.l	d3,-(a7)
	bsr	GetFolderNameAndFileName
	lea	Main_str(pc),a0
	bra.s	SymAdd_entry

;HSym SymAdd (SYM_STR SymName); 
SymAdd:
	move.l	4(a7),a0			; SYM_STR
	move.l	d3,-(a7)
	bsr	GetFolderNameAndFileName

SymAdd_entry:
	pea	(a1)			; Push FileName
	move.l	a0,a1			; a1 = Folder ANSI name
	move.w	#FOLDER_LIST_HANDLE,a0	; a0 = Folder Handle
	bsr	FindSymEntry		; Find SYM_ENTRY
	move.l	a0,d0			; Test if SYM_ENTRY is found ?
	move.w	SYM_ENTRY.hVal(a0),a0	; Load Folder Handle
	bne.s	\OkFolder		; a0 = 0 ?
		bsr	FolderAdd_a1	; a1 = Name of the folder to create
		move.w	d0,a0		; a0 = Folder Handle
		tst.w	d0
		bne.s	\OkFolder
			dc.w	$A000+330	; Throw Folder Error
\OkFolder:
	move.w	a0,d3			; Save folder Handle
	bsr	ValidateSymName		; Check file name
	tst.w	d0
	bne.s	\OkValid
		dc.w	$A000+330	; Throw an error
\OkValid
	move.l	(a7),a1			; a1 File Name to search
	move.w	d3,a0			; Get Folder Handle
	bsr	FindSymEntry		; Find a1 in Handle a0
	move.l	a0,d0
	bne.s	\FileAlreadyExists
		; Create a new file in the folder
		move.w	d3,a0		; Handle of Folder
		bsr	CreateItem	; Create a new file in the list
		move.l	a0,d0
		bne.s	\Created
		dc.w	$A000+670	; Error memory	
\Created:	move.l	(a7),a1		; Get Name
		moveq	#8-1,d0
\loop			move.b	(a1)+,(a0)+
			dbf	d0,\loop
		move.l	#$00030000,(a0)+	; Compat + Flags
		clr.w	(a0)+		; No Handle
		move.w	d3,a0		; Sort the folder
		bsr	SortList	; Sort it
		move.w	d3,a0		; Folder
		move.l	(a7),a1		; File Name
		bsr	FindSymEntry
		bsr	Sym2HSym	; Calculate HSym
		move.l	d0,(a7)		; Push HSym
		bra.s	\Return
\FileAlreadyExists
	bsr	Sym2HSym		; Calculate HSym
	move.l	d0,(a7)			; Push HSym
	move.w	SYM_ENTRY.flags(a0),d1
	andi.w	#SF_ARCHIVED+SF_LOCKED+SF_HIDDEN+SF_INVIEW,d1
	beq.s	\Ok
		dc.w	$A000+980		; Locked error
\Ok:	move.w	SYM_ENTRY.hVal(a0),-(a7)	; Push Old Handle
	clr.w	SYM_ENTRY.hVal(a0)		; Clear Handle
	bsr	HeapFree			; Free old handle
	addq.l	#2,a7
\Return	move.l	(a7)+,d0			; Pop HSym
	move.l	(a7)+,d3
	rts
	
;short HSymDel (HSym Sym);
; Contrary to tios's one, HSymDel can't delete folders
HSymDel:
	move.l	4(a7),d0			; Read HSym
	bsr	DerefSym_Reg			; SYM entry
	bra.s	SymDel_Entry			; Delete it
	
;short SymDel (SYM_STR SymName);
SymDel:
	clr.w	-(a7)
	move.l	4+2(a7),-(a7)		; Push SYM_STR
	bsr	SymFindPtr		; Find SYM_ENTRY to delete
	addq.l	#6,a7
SymDel_Entry:
	move.l	a0,d0
	beq.s	\End
		move.w	SYM_ENTRY.flags(a0),d0
		andi.w	#SF_FOLDER|SF_LOCKED|SF_HIDDEN|SF_INVIEW|SF_ARCHIVED,d0
		bne.s	\End2
			move.w	SYM_ENTRY.hVal(a0),-(a7)	; Push Handle to delete
			bsr	DeleteItem	; Delete SYM_ENTRY
			tst.w	(a7)		; Delete Handle ?
			beq.s	\NoFree
				bsr	HeapFree	; Free Handle
\NoFree			addq.l	#2,a7
			moveq	#1,d0		; TRUE
\End:	rts
\End2	moveq	#0,d0
	rts
	
;short SymDelTwin (SYM_ENTRY *SymPtr)
SymDelTwin:
	move.l	4(a7),a0		; SYM_ENTRY
SymDelTwin_reg
	move.w	SYM_ENTRY.flags(a0),d0
	andi.w	#SF_TWIN,d0
	beq.s	\Ret					; If there is no Twin flag, do nothing
		move.w	SYM_ENTRY.flags(a0),d0
		andi.w	#SF_INVIEW,d0
		bne.s	\Ret
			move.w	SYM_ENTRY.hVal(a0),-(a7)		; Push RAM Handle
			move.w	SYM_ENTRY.compat(a0),SYM_ENTRY.hVal(a0)	; Restore org handle
			clr.w	SYM_ENTRY.compat(a0)			
			andi.w	#~(SF_TWIN|SF_HIDDEN),SYM_ENTRY.flags(a0)	; Clear Twin & Hidden Flag
			ori.w	#SF_ARCHIVED,SYM_ENTRY.flags(a0)	; Set Archive Flag
			bsr	HeapFree				; Free the RAM Handle
			addq.l	#2,a7
			moveq	#1,d0
\Ret	rts
	
;HSym SymAddTwin (SYM_STR SymName)
; Output Side Effect
;	a0.l -> SYM_ENTRY 
;	d1.b = 0 if File Not Found
;	d1.b = 1 if File is not archived
SymAddTwin:
	clr.w	-(a7)
	move.l	6(a7),-(a7)
	bsr	SymFindPtr		; Find file ?
	addq.l	#6,a7
	moveq	#0,d1
	move.l	a0,d0
	bne.s	\ok
\Ret		moveq	#0,d0		; <= Because d0.uw may be != of 0 !
		rts			; An error
\ok	moveq	#1,d1
	move.w	SYM_ENTRY.flags(a0),d0
	andi.w	#SF_ARCHIVED,d0				; What to do ? I don't know what is the best.
	beq.s	\Ret					; Can not create a twin var if the symbol is not archived
	move.w	SYM_ENTRY.hVal(a0),SYM_ENTRY.compat(a0)	; Twin files doesn't work like Tios's way of doing, but i think it will work fine
	clr.w	SYM_ENTRY.hVal(a0)
	ori.w	#SF_TWIN|SF_HIDDEN,SYM_ENTRY.flags(a0)	; Set InView or Hidden Flags ?
	andi.w	#~SF_ARCHIVED,SYM_ENTRY.flags(a0)	; Fix Me : is it necessary to remove the Archive Flag ? I am not sure.
	bra	Sym2HSym

;HSym EM_twinSymFromExtMem (SYM_STR SymName, HSym Sym)
EM_twinSymFromExtMem:
	move.l	4(a7),a0
	move.l	8(a7),d0
	lea	-22(a7),a7	; Buffer
	beq.s	\SymName
		clr.w	(a7)		; The beginning with 0
		pea	2(a7)		; Push buffer addr
		move.l	d0,-(a7)
		bsr	HSYMtoName	; Get SYM name
		addq.l	#8,a7
		move.l	a1,a0
\SymName:
	pea	(a0)			; Push SYM_STR
	bsr	SymAddTwin		; Make a Twin
	move.l	d0,(a7)			; Check is success
	beq.s	\ErrorAdd
		;bsr	DerefSym	; Get SYM_ENTRY (Useless due to the side-effect)
		move.w	SYM_ENTRY.compat(a0),a0	; Get old handle
		trap	#3		; Deref it
		moveq	#0,d0
		move.w	(a0),d0		; Get var size
		addq.w	#2,d0		; +2
		move.l	d0,-(a7)
		bsr	HeapAllocHigh	; Alloc an handle
		addq.l	#4,a7
		move.w	d0,d2		; Save handle
		bsr	DerefSym	; Get SYM_ENTRY
		tst.w	d2		; Alloc failed ?
		bne.s	\Ok2
			move.w	SYM_ENTRY.compat(a0),SYM_ENTRY.hVal(a0)	; Twin files doesn't work like Tios's way of doing, but i think it will work fine
			clr.w	SYM_ENTRY.compat(a0)
			andi.w	#~(SF_TWIN|SF_HIDDEN),SYM_ENTRY.flags(a0)
			ori.w	#SF_ARCHIVED,SYM_ENTRY.flags(a0)
			clr.l	(a7)
			bra.s	\Error
\Ok2:		move.w	d2,SYM_ENTRY.hVal(a0)	; Save new handle
		move.w	SYM_ENTRY.compat(a0),a0
		trap	#3
		moveq	#0,d0
		move.w	(a0),d0		; Get var size
		addq.w	#1,d0		; +2 -1
		move.l	a0,a1
		move.w	d2,a0
		trap	#3
\Loop			move.b	(a1)+,(a0)+
			dbf	d0,\Loop
\Error	move.l	(a7)+,d0
	lea	22(a7),a7
	rts
\ErrorAdd
	tst.b	d1			; Test if not found ?
	beq.s	\Error			; Check if file is not archived ?
		bsr	Sym2HSym	; Return the HSym of the original file
		move.l	d0,(a7)		; Save it in the stack
		bra.s	\Error		; And return
	
CleanTwinFiles:
	move.w	#2,-(a7)
	clr.l	-(a7)
	bsr	SymFindFirst
	addq.l	#6,a7
\loop		move.w	SYM_ENTRY.flags(a0),d0
		andi.w	#SF_TWIN,d0
		beq.s	\next
			bsr	SymDelTwin_reg
\next		bsr	SymFindNext
		move.l	a0,d0
		bne.s	\loop
	rts	

;short SymSysVar (const char *VarName);
;short CheckSysFunc (const char *VarName, unsigned short *Index); 
;short CheckReservedName (SYM_STR SymName);
CheckReservedName:
SymSysVar:
CheckSysFunc:
	moveq	#0,d0		; No System Var !
	rts

;short SymMove (const char *SrcName, const char *DestName);
SymMove:
	clr.w	-(a7)			; Normal search
	move.l	2+4(a7),-(a7)
	bsr	SymFindPtr
	addq.l	#6,a7
	move.l	a0,d0
	beq.s	\invalid
		move.w	SYM_ENTRY.hVal(a0),-(a7)	; Preserve Handle
		move.l	2+8(a7),-(a7)			; Destination Name
		bsr	SymAdd				; Add the destination name
		addq.l	#4,a7
		bsr	DerefSym_Reg			; Deref the HSym
		move.l	a0,d0				; Check if success
		beq.s	\invalid2
			move.w	(a7)+,SYM_ENTRY.hVal(a0)	; Save the handle				
			clr.w	-(a7)			; Normal search
			move.l	2+4(a7),-(a7)		; Research the src symbol
			bsr	SymFindPtr		; So we can delete it
			addq.l	#6,a7	
			bsr	DeleteItem		; Delete SYM_ENTRY
			moveq	#1,d0
			bra.s	\ok
\invalid2	addq.l	#2,a7
\invalid:
\ok	rts

; unsigned short TokenizeName (const char *srcFile, unsigned char *destTokn);
TokenizeName:
	move.l	4(a7),a0	; Src
	bsr	strlen_reg	; Len of the source
	move.l	4(a7),a0	; Src
	move.l	8(a7),a1	; Dest
	cmpi.w	#20,d0		; String too long
	bge.s	\Error
	lea	18(a1),a1
	suba.l	d0,a1
	clr.b	(a1)+
\loop		move.b	(a0)+,(a1)+	
		bne.s	\loop
	moveq	#0,d0
\Error	rts

;void EX_stoBCD (unsigned char *VarName, float *Src); 
EX_stoBCD:
	link	a6,#$FFDC
	pea	(9).w
	move.l	$C(a6),-(a7)
	pea	-$1E(a6)
	bsr	memcpy
	move.b	#$23,-$15(a6)
	pea	-$15(a6)
	clr.w	-(a7)
	move.w	#$4000,-(a7)
	pea	-$14(a6)
	move.l	8(a6),-(a7)
	jsr	StrToTokN
	addq.l	#4,a7
	move.l	a0,(a7)
	bsr.s	VarStore
	unlk	a6
	rts
	
;HSym VarStore (SYM_STR SymName, unsigned short Flags, unsigned short Size, ...);
VarStore:
	move.w	8(a7),d0
	cmpi.w	#STOF_ESI,d0
	beq.s	VarStoreEsi
	cmpi.w	#STOF_HESI,d0
	beq.s	VarStoreHEsi
	cmpi.w	#STOF_NONE,d0
	beq	VarStoreNone
	dc.w	$A104		; Not recognize type

VarStoreNone:
	move.l	4(a7),-(a7)			; Push SYM_NAME
	bsr	SymAdd				; Add the SYM in the VAT
	addq.l	#4,a7
	rts

VarStoreHEsi:
	move.l	4(a7),-(a7)			; Push SYM_NAME
	bsr	SymAdd				; Add the SYM in the VAT
	bsr	DerefSym_Reg			; Deref the HSym
	addq.l	#4,a7
	move.w	(4+4+2+2)(a7),SYM_ENTRY.hVal(a0) ; Save Handle
	rts
	
VarStoreEsi:
	move.l	(4+4+2+2)(a7),a0		; ESI
	move.l	start_estack,-(a7)
	move.l	top_estack,-(a7)
	move.l	a0,top_estack			; END
	bsr	next_expression_index_reg
	addq.l	#1,a0
	move.l	a0,start_estack			; START
	bsr	HS_popEStack			; Pop it to a handle
	move.l	(a7)+,top_estack
	move.l	(a7)+,start_estack		; Restore the EStack
	move.w	d0,-(a7)			; Save new Handle
	move.l	(4+2)(a7),-(a7)			; Push SYM_NAME
	bsr	SymAdd				; Add the SYM in the VAT
	bsr	DerefSym_Reg			; Deref the HSym
	addq.l	#4,a7
	move.w	(a7)+,SYM_ENTRY.hVal(a0)	; Save Handle
	rts
	
;HANDLE VarCreateFolderPopup (unsigned short *CurIndex, unsigned short Flags);
; Flags: VCFP_ALL = 0x01, VCFP_SKIP_CURDIR = 0x02
VarCreateFolderPopup:
	movem.l	d3-d7/a2-a6,-(a7)
	move.l	40+4(a7),a2			; CurIndex Ptr
	move.w	40+8(a7),d4			; Flags
	clr.w	-(a7)				; Auto calculate the Height
	clr.l	-(a7)				; No title
	bsr	PopupNew
	addq.l	#6,a7
	move.w	d0,d3
	beq.s	\Error
		moveq	#1,d6			; ID
		move.w	d6,(a2)
		cmpi.w	#1,d4
		bne.s	\NoAddAll
			lea	All_str(pc),a0
			bsr.s	\AddText
\NoAddAll:	clr.l	-(a7)
		clr.w	-(a7)
		bsr	SymFindFirst
		addq.l	#6,a7
\loop			pea	(a0)
			pea	CUR_FOLDER_STR
			bsr	SymCmp
			addq.l	#4,a7
			move.l	(a7)+,a0
			tst.w	d0
			bne.s	\NoActive
			cmpi.w	#2,d4		; Skip Active ?
			beq.s	\Skip
			move.w	d6,(a2)		; Savec Active index
\NoActive		bsr.s	\AddText
\Skip			bsr	SymFindNext
			move.l	a0,d0
			bne.s	\loop
\Error:	move.w	d3,d0
	movem.l	(a7)+,d3-d7/a2-a6
	rts
\AddText	
	move.w	d6,-(a7)
	pea	(a0)
	move.w	#$FFFF,-(a7)
	move.w	d3,-(a7)
	bsr	PopupAddText
	lea	10(a7),a7
	addq.w	#1,d6
	tst.w	d0
	beq.s	\FreeError
	rts
\FreeError
	move.w	d3,(a7)
	bsr	HeapFree
	addq.l	#4,a7			; SKip return address
	clr.w	d3
	bra.s	\Error
	