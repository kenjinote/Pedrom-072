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
; *								*
; *		Pedrom		/	Flash			*
; *								*
; ***************************************************************

; Note: 
;	It is a software protection to protect invalid code from being executed.
;	Indeed, an invalid call to FlashWrite / FlashErase will destroy your archive memory !!
;	If a crash occurs, a call to FlashErase is nearly possible !
;	So FlashErase / FlashWrite doesn't work is this protection is no set.
;	Of course, it is easy to hack with a few code, but it doesn't prevent from Hack
;	but from Crash !
;	I don't want to use the same method as Ti (Only GC can use FlashErase by using the hardware protection)
;	since a bug in this function will let the calc with protection off !
;
;	I think it isn't perfect but better than AMS way's.

FLASH_MAGIC1_VALUE	EQU	$83C381CC
FLASH_MAGIC2_VALUE	EQU	$258D5565

FLASH_FUNC_ON	MACRO
	trap	#12					; Go to supervisor mode
	move.w	d0,-(a7)				; Save old SR
	move.w	#$2700,SR				; SR = $2700 (All int off)
	move.l	#FLASH_MAGIC1_VALUE,FLASH_MAGIC1	; Allow using of FlashWrite / FlashErase 
	move.l	#FLASH_MAGIC2_VALUE,FLASH_MAGIC2
		ENDM

FLASH_FUNC_OFF	MACRO
	clr.l	FLASH_MAGIC1				; Disable using of FlashErase/FlashWrite
	clr.l	FLASH_MAGIC2
	move.w	(a7)+,SR				; Reload old SR
		ENDM

; Init the flash.
; To be called, after VATInit
FlashInit:
	movem.l	d0-d7/a0-a6,-(a7)
	lea	-20(a7),a7
	move.l	a7,a6
	; Check if the sectors are correct...
	bsr	FlashCheck
	pea	AddArchiveFiles_str(pc)
	jsr	printf
	; Add in the VAT all the entries in archive.
	lea	START_ARCHIVE,a2		; First Sector
	move.l	a2,a3
	adda.l	#$10000,a3			; End of first Sector
	lea	HEAP_TABLE,a4
\SectorLoop	; Find the end of a sector
		cmp.w	#ARC_VOID,(a2)
		beq.s	\NextSector
			cmpi.w	#ARC_INUSE,(a2)
			bne.s	\NextEntry
				; Add this entry in the VAT
				move.l	a6,a0
				lea	2(a2),a1
				clr.b	(a0)+
				moveq	#8-1,d0
\NLoop1					move.b	(a1)+,(a0)+
					dbeq	d0,\NLoop1
				bne.s	\NoNull1
					subq.l	#1,a0
\NoNull1			move.b	#'\',(a0)+
				lea	10(a2),a1
				moveq	#8-1,d0
\NLoop2					move.b	(a1)+,(a0)+
					dbeq	d0,\NLoop2
				beq.s	\Null
					clr.b	(a0)+
\Null:				pea	-1(a0)
				bsr	SymAdd			; Add the Sym
				addq.l	#4,a7
				bsr	DerefSym_Reg
				move.l	a0,d0
				beq.s	\NextEntry		; Can it occurs ? 
				move.l	a0,a5
				bsr	HeapGetHandle		; Get an handle
				move.w	d0,SYM_ENTRY.hVal(a5)	; Save the handle
				move.w	#SF_ARCHIVED,SYM_ENTRY.flags(a5) ; Set as archived
				lsl.w	#2,d0
				lea	ARC_ENTRY.HeaderSize(a2),a0
				move.l	a0,0(a4,d0.w)		; Save the addr of the handle
\NextEntry		moveq	#0,d4				; Next entry
			move.w	ARC_ENTRY.HeaderSize(a2),d4	; Read the size of the file
			add.w	#ARC_ENTRY.HeaderSize+2,d4	; Add Header size +2
			moveq	#1,d1				; Calculate
			and.w	d4,d1				; d1 = 1 if odd, 0 if even
			add.w	d1,d4				; Even upper address
			add.l	d4,a2				; Next entry
			cmp.l	a3,a2				; In the same sector ?
			bcs.s	\SectorLoop			; Next Entry in Sector 
\NextSector	; Next Sector
		move.l	a3,a2			; New Start
		adda.l	#$10000,a3		; New End
		cmp.l	#END_ARCHIVE-1,a2	; Check End of Archive Memory ?
		bls	\SectorLoop
	lea	20(a6),a7
	movem.l	(a7)+,d0-d7/a0-a6
	rts

; Low Level functions for Flash access (It doesn't use Trap #B for safe reason)
; short FlashWrite_(const void *src asm("a2"), void *dest asm("a3"), size_t size asm("d3"))
FlashWrite:
	movem.l	d1-d7/a0-a6,-(sp)	; Save Registers
	trap    #12			; Go to supervisor mode
	move.w  d0,-(sp)		; Save SR

	clr.w	d5		; Set Error 

	; Check Batt
	bsr	BatTooLowFlash
	tst.b	d0
	bne	\Error
	
	; Unprotect Flash (SR = $2700)
	move.w	$5EA4,d0	; I read this because on Vti, the address $1C5EA4 & $5EA4 are the same, so if I write 0, it corrupts the heap. Very annoying, no ?
	lea	($1C5EA4).l,a0
	bclr	#1,($600015)	;turn off data to the LCD (RAM is not read)
	nop
	nop
	nop
	move	#$2700,sr
	move.w	d0,(a0)
	nop
	nop
	nop
	move	#$2700,sr
	move.w	d0,(a0)
	bset	#1,($600015)	;turn on LCD
	
	; We don't check the stack, but we don't use it
	; Check if a2 is in RAM
	cmp.l	#$3FFFF,a2
	bhi.s	\Error
	move.l	a2,d0
	andi.w	#1,d0
	bne.s	\Error		; Not aligned
	; Check if a3 is in Archive Memory
	cmp.l	#END_ARCHIVE-1,a3
	bhi.s	\Error
	cmp.l	#START_ARCHIVE-1,a3
	bls.s	\Error
	move.l	a3,d0
	andi.w	#1,d0
	bne.s	\Error		; Not aligned
	; Check if a3+d3 is in the same block of memory
	addq.l	#1,d3		; Word alignement (Long +1
	andi.w	#$FFFE,d3	; Clear low bit (does word instead of long)
	lea	-1(a3,d3.l),a4	; -1 because the last byte we'll write is a3+d3-1
	move.l	a3,d0
	move.l	a4,d1
	swap	d0
	swap	d1
	cmp.w	d0,d1
	bne.s	\Error
	; Check if a2+d3 is in RAM
	lea	-1(a2,d3.l),a4
	cmp.l	#$3FFFF,a4
	bhi.s	\Error
	; Check if the call is valid
	cmp.l	#FLASH_MAGIC1_VALUE,FLASH_MAGIC1
	bne.s	\Error
	cmp.l	#FLASH_MAGIC2_VALUE,FLASH_MAGIC2
	bne.s	\Error
	
	lsr.l	#1,d3		; Convert Byte to Word

	; Copy code to RAM and execute it
	move.w	#((\FlashWrite_ExecuteInRam_End-\FlashWrite_ExecuteInRam)/2-1),d0
	lea	\FlashWrite_ExecuteInRam(pc),a0
	lea	EXEC_RAM,a1
\Loop:		move.w	(a0)+,(a1)+
		dbf	d0,\Loop
	jmp	EXEC_RAM	; Execute code in RAM
\FlashWrite_Return:
	moveq	#1,d5		; Of it is done
\Error
	; Protect Flash
	lea	($1C5E00),a0
	bclr	#1,($600015)  ;turn off data to the LCD (RAM is not read)
	nop
	nop
	nop
	move	#$2700,sr
	move.w	(a0),d0
	nop
	nop
	nop
	move.w	#$2700,SR
	move.w	(a0),d0
	bset	#1,$600015

	move.w	d5,d0			; Error code
	move.w  (sp)+,sr		; Return to User mode
	movem.l	(sp)+,d1-d7/a0-a6	; Pop registers
	rts
  
; In :
;	a2 -> Src
;	a3 -> Dest
;	d3 = Len in words 
\FlashWrite_ExecuteInRam:
	subq.w	#1,d3			; Because of Dbf
	blt.s	\End
	move.l  a3,a4			; A4 = Command register
	move.w  #$5050,(a4)		; Clear Statut Register
\loop:
		move.w	(a2)+,d7	; Read value to write
		move.w  #$1010,(a4)	; Write Setup
		move.w  d7,(a3)+	; Write word
\wait:			move.w  (a4),d0	; Check it
			btst    #7,d0
			beq.s	\wait	; and wait that's done
		dbra    d3,\loop
	move.w	#$5050,(a4)
	move.w	#$FFFF,(a4)	; Read Memory
\End:	jmp	(\FlashWrite_Return).l
\FlashWrite_ExecuteInRam_End:
  


; Low Level functions for Flash access (It doesn't use Trap #B for safe reason)
; short FlashErase_(const void *dest asm("a2"))
FlashErase:
	movem.l	d1-d7/a0-a6,-(sp)	; Save Registers
	trap    #12			; Go to supervisor mode
	move.w  d0,-(sp)		; Save SR

	clr.w	d5		; Set Error 

	; Check Batt
	bsr	BatTooLowFlash
	tst.b	d0
	bne.s	\Error

	; Unprotect Flash (SR = $2700)
	move.w	$5EA4,d0	; I read this because on Vti, the address $1C5EA4 & $5EA4 are the same, so if I write 0, it corrupts the heap. Very annoying, no ?
	lea	($1C5EA4).l,a0
	bclr	#1,($600015)	;turn off data to the LCD (RAM is not read)
	nop
	nop
	nop
	move	#$2700,sr
	move.w	d0,(a0)
	nop
	nop
	nop
	move	#$2700,sr
	move.w	d0,(a0)
	bset	#1,($600015)	;turn on LCD
	
	; We don't check the stack, but we don't use it
	; Check if a2 is in Archive Memory
	cmp.l	#END_ARCHIVE-1,a2
	bhi.s	\Error
	cmp.l	#START_ARCHIVE-1,a2
	bls.s	\Error

	; Check if the call is valid
	cmp.l	#FLASH_MAGIC1_VALUE,FLASH_MAGIC1
	bne.s	\Error
	cmp.l	#FLASH_MAGIC2_VALUE,FLASH_MAGIC2
	bne.s	\Error
	
	; Round to the upper 64K
	move.l	a2,d0
	andi.l	#$FFFF0000,d0
	move.l	d0,a2

	; Copy code to RAM and execute it
	move.w	#((\FlashErase_ExecuteInRam_End-\FlashErase_ExecuteInRam)/2-1),d0
	lea	\FlashErase_ExecuteInRam(pc),a0
	lea	EXEC_RAM,a1
\Loop:		move.w	(a0)+,(a1)+
		dbf	d0,\Loop
	jmp	EXEC_RAM	; Execute code in RAM
\FlashErase_Return:

	moveq	#1,d5		; Of it is done
\Error	
	; Protect Flash
	lea	($1C5E00),a0
	bclr	#1,($600015)  ;turn off data to the LCD (RAM is not read)
	nop
	nop
	nop
	move	#$2700,sr
	move.w	(a0),d0
	nop
	nop
	nop
	move.w	#$2700,SR
	move.w	(a0),d0
	bset	#1,$600015

	move.w	d5,d0			; Error code
	move.w  (sp)+,sr		; Return to User mode
	movem.l	(sp)+,d1-d7/a0-a6	; Pop Registers
	rts
  
; In :
;	a2 -> Src
\FlashErase_ExecuteInRam:
	move.w	#$FFFF,(a2)	; Read ?
	move.w	#$5050,(a2)	; Set Statut register
	move.w	#$2020,(a2)	; Erase Setup
	move.w	#$D0D0,(a2)	; Erase Conform
\wait:		move.w	(a2),d0
		btst	#7,d0
		beq.s	\wait
	move.w	#$5050,(a2)
	move.w	#$FFFF,(a2)	; Read Memory
	jmp	(\FlashErase_Return).l
\FlashErase_ExecuteInRam_End:


; In:
;	Nothing:
; Out:
;	d0 = $FF if Batt are too low for flash
; Destroy:
;	d0
BatTooLowFlash:
	jsr	CheckBatt
	cmp.b	#2,d0
	slt	d0
	rts
	
; In:
;	a4 -> File
; Out:
;	d0.w = CheckSum
; Destroy:
;	d0
FlashCheckSum:
	movem.l	a4/d1-d2,-(a7)
	moveq	#0,d0
	moveq	#0,d2
	move.w	(a4),d1
	addq.w	#1,d1		; +2 (-1 for dbf)
\Loop		move.b	(a4)+,d2
		add.w	d2,d0
		dbf	d1,\Loop
	movem.l	(a7)+,a4/d1-d2
	rts
	

; Check the flash archive and valid it.
; If a sector if founded as invalid (Invalid filename or invalid checksum), it will be entirely erased.
; Quite slow (It checks if 1.85 Mo of data are valid !)
; In/Out/Destroy:
;	Nothing
FlashCheck:
	movem.l	d0-d7/a0-a6,-(a7)

	pea	CheckFlash_str(pc)
	jsr	printf
	addq.l	#4,a7
	
	FLASH_FUNC_ON
	
	lea	START_ARCHIVE,a2		; First Sector
	move.l	a2,a3
	adda.l	#$10000,a3			; End of first Sector
	moveq	#$FFFFFFFF,d7			; ARC_VOID
\SectorLoop	; Find the end of a sector
		cmp.w	(a2),d7
		beq	\FinishSector
			; Both IN_USE and DELETED should have a valid entry !
			moveq	#0,d4				; Next entry
			move.w	ARC_ENTRY.HeaderSize(a2),d4	; Read the size of the file
			add.w	#ARC_ENTRY.HeaderSize+2,d4	; Add Header size +2
			moveq	#1,d1				; Calculate
			and.w	d4,d1				; d1 = 1 if odd, 0 if even
			add.w	d1,d4				; Even upper address
			lea	2(a2),a4			; Entry Ptr (Skip Flags)
			; Check if the folder name is valid
			move.l	(a4)+,FOLDER_TEMP
			move.l	(a4)+,FOLDER_TEMP+4
			clr.b	FOLDER_TEMP+8
			lea	FOLDER_TEMP,a0
			bsr	ValidateSymName_reg
			tst.b	d0
			beq.s	\SectorError
			; Check if the file name is valid
			move.l	(a4)+,FOLDER_TEMP
			move.l	(a4)+,FOLDER_TEMP+4
			lea	FOLDER_TEMP,a0
			bsr	ValidateSymName_reg
			tst.b	d0
			beq.s	\SectorError
			; Check the checksum
			move.w	(a4)+,d5			; Read checksum
			bsr	FlashCheckSum			; Calculate the checksum
			cmp.w	d0,d5
			bne.s	\SectorError
			add.l	d4,a2				; Next entry
			cmp.l	a3,a2				; In the same sector ?
			bcs	\SectorLoop			; Next Entry in Sector 
\NextSector			; Next Sector
				move.l	a3,a2			; New Start
				adda.l	#$10000,a3		; New End
				cmp.l	#END_ARCHIVE-1,a2	; Check End of Archive Memory ?
				bls	\SectorLoop
				bra.s	\End
\SectorError:	; The sector has some errors : reset it !
		lea	-30000(a3),a2				; Get a ptr inside the sector
		bsr	FlashErase				; Fill it with $FFFF
		bra.s	\NextSector
	; Check if the end is full of $FFFF : is it usefull ? Yes, otherwise, it may try to write in it after this value
\FinishSector:	move.l	a3,d0
		sub.l	a2,d0
		bls.s	\NextSector
		lsr.l	#1,d0
		subq.w	#1,d0
\FLoop			cmp.w	(a2)+,d7
			dbne	d0,\FLoop
		bne.s	\SectorError
		bra.s	\NextSector
\End:
	FLASH_FUNC_OFF
	movem.l	(a7)+,d0-d7/a0-a6
	rts
	


; ***************************************************************
; 			High level functions
; ***************************************************************

; Format of an entry in the archive:
;	STATUT.w =
;		* $FFFF = Nothing, end of block (If it is the first entry of a block, the block is empty)
;		* $FFFE = In use (A file is in the entry)
;		* $FFFC = Deleted
;	FOLDER	= 8 chars
;	NAME	= 8 chars
;	CHECKSUM = .w
;	FILE	= SIZE.w
;		...
;	Next one or end of block
EM_blockVerifyErase:
	move.l	4(a7),d0
	andi.l	#$FFFF0000,d0
	move.l	d0,a0
	moveq	#-1,d2
	move.w	#$3FFF,d1
\loop		cmp.l	(a0)+,d2
		dbne	d1,\loop
	sne	d0
	rts
	
; Note: It WON'T work if you called it directly. You must disable the system protection of calling FlashWrite / FlashErase.
EM_abandon:
	move.w	4(a7),a0
	trap	#3				; Deref the handle
	clr.w	d0
	cmp.l	#START_ARCHIVE,a0		; check range
	bls.s	\Error
	cmp.l	#END_ARCHIVE-1,a0
	bhi.s	\Error
		; Delete ARC_ENTRY
		movem.l	a2-a3/d3,-(a7)		; Push registers
		move.w	#ARC_DELETED,-(a7)	; Push src on stack
		move.l	a7,a2			; Src
		lea	ARC_ENTRY.statut(a0),a3	; Dest
		moveq	#2,d3			; Size
		cmp.w	#ARC_INUSE,(a3)		; Do not write Delete if its't in used
		bne.s	\NoDelete
			bsr	FlashWrite	; Write in Flash
\NoDelete	addq.l	#2,a7			; Skip Src
		movem.l	(a7)+,a2-a3/d3		; Pop registers
		; Delete HANDLE in Handle Tab
		move.w	4(a7),d0		; Get handle
		lea	HEAP_TABLE,a0		; Get Heap Table
		lsl.w	#2,d0			
		clr.l	0(a0,d0.w)		; Clear handle
\Error	rts


EM_findEmptySlot:
	lea	START_ARCHIVE,a0		; First Sector
	move.l	a0,a1
	adda.l	#$10000,a1			; End of first Sector
\SectorLoop
		; Find the end of a sector
		cmp.w	#ARC_VOID,(a0)
		beq.s	\found
			moveq	#0,d0				; Next entry
			move.w	ARC_ENTRY.HeaderSize(a0),d0	; Read the size of the file
			add.w	#ARC_ENTRY.HeaderSize+2,d0	; Add Header size +2
			moveq	#1,d1				; Calculate
			and.w	d0,d1				; d1 = 1 if odd, 0 if even
			add.w	d1,d0				; Even upper address
			add.l	d0,a0				; Next entry
			cmp.l	a1,a0				; In the same sector ?
			bcs.s	\SectorLoop			; Next Entry in Sector 
\NextSector			; Next Sector
				move.l	a1,a0			; New Start
				adda.l	#$10000,a1		; New End
				cmp.l	#END_ARCHIVE-1,a0	; Check End of Archive Memory ?
				bls.s	\SectorLoop
					suba.l	a0,a0
					bra.s	\End
\found:	; Calculate Free Space
	move.l	a1,d0
	sub.l	a0,d0
	cmp.l	4(a7),d0	; Check if enought space at the end of this sector ?
	bcs.s	\NextSector	; No so next sector
		; AMS write $FFFC as statut of the found block.
		; I don't understand why I should do it, so I don't do it
		lea	ARC_ENTRY.HeaderSize(a0),a0
\End:	rts
	
	
	
EM_survey:
	movem.l	d3-d7,-(a7)
	; Set vars
	moveq	#0,d3		; In Use
	moveq	#0,d4		; FreeAfterGC
	moveq	#0,d5		; Free
	
	lea	START_ARCHIVE,a0		; First Sector
	move.l	a0,a1
	adda.l	#$10000,a1			; End of first Sector
\SectorLoop
		; Find the end of a sector
		cmp.w	#ARC_VOID,(a0)
		beq.s	\found
			cmp.w	#ARC_DELETED,(a0)
			bne.s	\NoDel
				add.l	d0,d4		; Add it to FreeAfterGC
\NoDel:			moveq	#0,d0				; Next entry
			move.w	ARC_ENTRY.HeaderSize(a0),d0	; Read the size of the file
			add.w	#ARC_ENTRY.HeaderSize+2,d0	; Add Header size +2
			moveq	#1,d1				; Calculate
			and.w	d0,d1				; d1 = 1 if odd, 0 if even
			add.w	d1,d0				; Even upper address
			add.l	d0,a0				; Next entry
			add.l	d0,d3			; Add it to InUse
			cmp.l	a1,a0				; In the same sector ?
			bcs.s	\SectorLoop			; Next Entry in Sector 
\NextSector			; Next Sector
				move.l	a1,a0			; New Start
				adda.l	#$10000,a1		; New End
				cmp.l	#END_ARCHIVE-1,a0	; Check End of Archive Memory ?
				bls.s	\SectorLoop
				bra.s	\End
\found:	; Calculate Free Space at the end of a sector
	move.l	a1,d0
	sub.l	a0,d0
	add.l	d0,d5		; Add it to Free
	bra.s	\NextSector
\End:
	move.l	4*(5+1)(a7),d0
	beq.s	\noInUse
		move.l	d0,a0
		move.l	d3,(a0)
\noInUse
	move.l	4*(5+2)(a7),d0
	beq.s	\noFreeAfterGc
		move.l	d0,a0
		move.l	d4,(a0)
\noFreeAfterGc
	move.l	4*(5+3)(a7),d0
	beq.s	\noFree
		move.l	d0,a0
		move.l	d5,(a0)
\noFree
;	move.l	4*(5+6)(a7),d0			; If set, fix the call in shell.asm
;	beq.s	\NoAll
;		move.l	d0,a0
;		move.l	#END_ARCHIVE-START_ARCHIVE,(a0)
\NoAll:	
	movem.l	(a7)+,d3-d7
	rts
	
FL_write:
	movem.l	a2-a3/d3,-(a7)
	move.l	4*(1+3+0)(a7),a2	; Src
	move.l	4*(1+3+1)(a7),a3	; Dest
	move.l	4*(1+3+2)(a7),d3	; Len
	bsr	FlashWrite		; Won't work
	movem.l	(a7)+,a2-a3/d3
	rts
	
EM_write:
	movem.l	a2-a3/d3,-(a7)
	move.l	4*(1+3+0)(a7),a2	; Src
	move.l	4*(1+3+1)(a7),a3	; Dest
	move.l	4*(1+3+2)(a7),d3	; Len
	bsr	FlashWrite		; Doesn't work ;)
	tst.w	d0
	bne.s	\Ok
		dc.w	$A3D4
\Ok	movem.l	(a7)+,a2-a3/d3
	rts
	
FL_getHardwareParmBlock:
	move.l	ROM_BASE+$104,a0
	move.l	a0,d0
	btst	#0,d0
	bne.s	\Default
	cmpi.l	#ROM_BASE+$10000,d0
	bcc.s	\Default
	cmpi.l	#ROM_BASE,d0
	bcc.s	\End
\Default
	lea	DefaultParmBlock(Pc),a0
\End	rts

DefaultParmBlock:
	dc.w	$2A	; unsigned short len; /* length of parameter block */ 
	dc.l	CALC_BOOT_TYPE	; unsigned long hardwareID; /* 1 = TI-92 Plus, 3 = TI-89 */ 
	dc.l	0	; unsigned long hardwareRevision; /* hardware revision number */ 
	dc.l	1	; unsigned long bootMajor; /* boot code version number */ 
	dc.l	1	; unsigned long bootRevision; /* boot code revision number */ 
	dc.l	0	; unsigned long bootBuild; /* boot code build number */ 
	dc.l	1	; unsigned long gateArray; /* gate array version number */  AMS set default to 2. But I think 1 is really better for default
	dc.l	$F0	; unsigned long physDisplayBitsWide; /* display width */ 
	dc.l	$80	; unsigned long physDisplayBitsTall; /* display height */ 
	dc.l	$F0	; unsigned long LCDBitsWide; /* visible display width */ 
	dc.l	$80	; unsigned long LCDBitsTall; /* visible display height */ 


EM_moveSymFromExtMem:
	move.l	4(a7),a0	; SYM_STR
	move.l	8(a7),d0	; HSYM
EM_moveSymFromExtMem_reg
	movem.l	a2-a3,-(a7)
	FLASH_FUNC_ON	
	move.l	a0,d1		; SYmStr = NULL 
	beq.s	\HSym
		pea	(a0)
		bsr	SymFind
		addq.l	#4,a7
\HSym:	tst.l	d0
	beq.s	\Error		; HSym = 0
	bsr	DerefSym_Reg
	move.l	a0,a2
	move.w	SYM_ENTRY.flags(a2),d0
	andi.w	#SF_ARCHIVED,d0
	beq.s	\Error		; Sym is not archived
	move.w	SYM_ENTRY.hVal(a2),a0
	trap	#3		; Deref Handle
	move.l	a0,a3
	moveq	#0,d0
	move.w	(a3),d0
	addq.l	#2,d0
	bsr	HeapAlloc_reg	; Alloc Space
	move.w	d0,d2
	beq.s	\Error		; Not enought RAM to alloc it
		; All is Ok. Copy to RAM.
		; Sym hasn't moved (Heap Alloc doesn't change memory. HeapFree doesn't. HeapUnlock does)
		move.w	d0,a0
		trap	#3	; Deref Handle
		move.w	(a3),d0
		addq.w	#1,d0	; +2 -1
\CpyLoop		move.b	(a3)+,(a0)+
			dbf	d0,\CpyLoop
		; Modify the SYM ENTRY (Flags and Handle)
		andi.w	#~SF_ARCHIVED,SYM_ENTRY.flags(a2)
		move.w	SYM_ENTRY.hVal(a2),-(a7)
		move.w	d2,SYM_ENTRY.hVal(a2)
		bsr	EM_abandon		; Abandon it
		addq.l	#2,a7			; EM_abandon return a non NULL value if success
\Error:
	FLASH_FUNC_OFF
	movem.l	(a7)+,a2-a3
	rts
	
; Bug: Before archiving a file, check if a file with the same foldername\filename is
; already in the FLASH, and put is as DELETED.
EM_moveSymToExtMem:
	move.l	4(a7),a0	; SYM_STR
	move.l	8(a7),d0	; HSYM
EM_moveSymToExtMem_reg:
	movem.l	a2-a6/d3-d4,-(a7)
	FLASH_FUNC_ON
	move.l	a7,a6
	move.l	a0,d1		; SYmStr = NULL 
	beq.s	\HSym
		pea	(a0)
		bsr	SymFind
\HSym:	move.l	d0,d3
	beq	\Error		; HSym = 0
	bsr	DerefSym_Reg
	move.l	a0,a5
	clr.w	d0		; Error
	move.w	SYM_ENTRY.flags(a5),d1
	andi.w	#SF_ARCHIVED|SF_TWIN|SF_HIDDEN,d1
	bne	\Error		; Sym is archived (Twin symbol are also archived), Symbol is InUSe ? Error
	move.w	SYM_ENTRY.hVal(a5),a0
	trap	#3		; Deref Handle
	move.l	a0,a4		; Src File
	moveq	#0,d0
	move.w	(a4),d0		; File Size
	add.l	#ARC_ENTRY.HeaderSize+2,d0	; +2
	move.l	d0,-(a7)
	bsr	EM_findEmptySlot	; Find somewhere to archive it ?
	move.l	a0,d0			
	bne.s	\Succ			; Can not find a sufficent space
	bsr	EM_GC			; No => Garbesh Collect !
	bsr	EM_findEmptySlot	; Find somewhere to archive it, again !
	move.l	a0,d0			
	beq.s	\Error			; Can not find a sufficent space even after GC
\Succ		lea	-ARC_ENTRY.HeaderSize(a0),a3
		; Find a free Handle
		bsr	HeapGetHandle
		move.w	d0,d4
		beq.s	\Error
		; Find Folder Name
		move.l	d3,d0
		bsr	FindFolderHSymFromFileHSym_Reg
		tst.l	d0
		beq.s	\Error
		; Archive SYM | 
		bsr	FlashCheckSum		; Calculate Check Sum
		move.w	d0,-(a7)		; Copy CheckSum in Header
		; Copy File Name
		move.l	SYM_ENTRY.name+4(a5),-(a7)
		move.l	SYM_ENTRY.name+0(a5),-(a7)
		; Copy Folder Name
		move.l	SYM_ENTRY.name+4(a0),-(a7)
		move.l	SYM_ENTRY.name+0(a0),-(a7)
		; Copy File in use
		move.w	#ARC_INUSE,-(a7)	; Statut
		move.l	a7,a2		; Src
		moveq	#ARC_ENTRY.HeaderSize,d3	; Size
		bsr	FlashWrite	;Write Header File
		; Copy File
		lea	ARC_ENTRY.HeaderSize(a3),a3	; Dest
		move.l	a4,a2				; Src
		move.w	(a4),d3				; Size
		addq.w	#2,d3				; +2
		bsr	FlashWrite	; It returns Error code d0
		; Modify SYM_ENTRY
		ori.w	#SF_ARCHIVED,SYM_ENTRY.flags(a5) ; Set as archived
		move.w	SYM_ENTRY.hVal(a5),-(a7)	; Push Previous Handle
		move.w	d4,SYM_ENTRY.hVal(a5)		; Save new handle
		lea	HEAP_TABLE,a0
		lsl.w	#2,d4
		move.l	a3,0(a0,d4.w)			; Set new Handle
		bsr	HeapFree			; Free old handles
		moveq	#1,d0
\Error	move.l	a6,a7
	FLASH_FUNC_OFF
	movem.l	(a7)+,a2-a6/d3-d4
	rts
	
	
; Since I want a very safe method, I use the boot code (Like tios)
; As a consequence, you may not be able to download it from a calc...
; It doesn't work on Vti, since there isn't any boot code.
FL_download:
	;move.l	(ROM_BASE+$4),a1	; Even if AMS reads vector $100, I prefer read vector $4
	;trap	#12			; Go to supervisor mode
	;move.w	#$2700,SR		; Stop interrupts.
	move.l	(ROM_BASE+$100),a1	; FixMe : +4 or +100 ? (Tios reads +100)
					; On AMS >=2.03,  Tios jumps to a internal function
					; which resets the archive.
					; On AMS >=1.05,  it Calls first the Function C of
					; trap #b -function 8 on AMS 1.05 - It seems it is 
					; because AMS should set on HW2 the RAM as fully 
					; executable, which it is the case in Pedrom )
	cmp.l	#ROM_BASE,a1		; Must be in the first 64K block
	bls.s	\FATAL_ERROR	
	cmp.l	#ROM_BASE+65535,a1	; Must be in the first 64K block
	bhi.s	\FATAL_ERROR		
	jmp	(a1)			; Jump to boot code, and install product code (Can be in User Mode for vector $100).
\FATAL_ERROR
	lea	Boot_str(pc),a0		; Error String
	jmp	FATAL_ERROR

; Useless functions (Wonderfull, no ?)
EM_put:		; GetAlphaStatut
	moveq	#0,d0
EM_open:	; SetAlphaStatut
	rts
	
; Bug: If it failed to alloc one handle, it forgets to free all of them !
EM_GC:
	movem.l	d3-d7/a2-a6,-(a7)
	FLASH_FUNC_ON

	move.l	a7,a6				; Save stack
	lea	START_ARCHIVE,a2		; First Sector
	move.l	a2,a3
	adda.l	#$10000,a3			; End of first Sector
	clr.b	d3				; No garbesh this sector
	moveq	#0,d5
	moveq	#0,d6				; Return value
\SectorLoop
		cmp.w	#ARC_VOID,(a2)				; Check the end of a sector
		beq.s	\EndOfSector
			cmp.w	#ARC_DELETED,(a2)		; Entry is marked as deleted  ?
			bne.s	\NoDel				; ?
				st.b	d3			; Garbesh this sector
\NoDel:			addq.l	#1,d5				; One more entry
			moveq	#0,d0				; Next entry
			move.w	ARC_ENTRY.HeaderSize(a2),d0	; Read the size of the file
			add.w	#ARC_ENTRY.HeaderSize+2,d0	; Add Header size +2 
			moveq	#1,d1				; Calculate
			and.w	d0,d1				; d1 = 1 if odd, 0 if even
			add.w	d1,d0				; Even upper address
			add.l	d0,a2				; Next entry
			cmp.l	a3,a2				; In the same sector ?
			bcs.s	\SectorLoop			; Next Entry in Sector 
\EndOfSector:		tst.b	d3				; Garbesh this Sector ?
			beq	\NextSector			; No there is no Deleted entry in this sector
				lsl.l	#3,d5			; Check Stack
				add.w	#$102,d5		; Calculate minimun Stack Size : $102 + 8 *NumberOfEntry
				cmp.l	d5,a7			; Check Stack 
				bls	\Failed
				move.l	a3,a2			; A2 = Start of the current sector
				suba.l	#$10000,a2		; 
				clr.w	-(a7)			; End of Entry list
\SectorLoop2			cmp.w	#ARC_VOID,(a2)		; Check the end of the sector
				beq.s	\Finish
					moveq	#0,d4				; Next entry
					move.w	ARC_ENTRY.HeaderSize(a2),d4	; Read the size of the file
					add.w	#ARC_ENTRY.HeaderSize+2,d4	; Add Header size +2
					moveq	#1,d1				; Calculate
					and.w	d4,d1				; d1 = 1 if odd, 0 if even
					add.w	d1,d4				; Even upper address
					cmp.w	#ARC_DELETED,(a2)		; The entry is marked as deleted ?
					beq.s	\SkipEntry			; No, so copy it in RAM
						lea	ARC_ENTRY.HeaderSize+2(a2),a0	; Ptr insied the handle
						bsr	HeapPtrToHandle_reg	; Get Org Handle
						move.w	d0,-(a7)		; check if success
						beq	\Failed
						move.l	d4,-(a7)		; Alloc Handle
						bsr	HeapAlloc		; to copy the entry in the ram
						move.w	d0,-(a7)		; Failed to alloc handle ?
						beq	\Failed			; Push handle on the stack
						move.w	d0,a0			
						trap	#3			; Deref handle
						lsr.l	#1,d4
						subq.w	#1,d4			; Calc word size
\Cpy:							move.w	(a2)+,(a0)+	
							dbf	d4,\Cpy		; Copy the entry
						moveq	#0,d4			
\SkipEntry:				add.l	d4,a2				; Next entry
					cmp.l	a3,a2				; In the same sector ?
					bcs.s	\SectorLoop2			; Next Entry in Sector 
\Finish				; Erase the sector
				lea	-30000(a3),a2		; In the sector to erase
				bsr	FlashErase		; Erase the sector
				tst.w	d0
				beq.s	\Failed			; Failed to erase it
				; Pop all handles
\RestoreLoop			move.w	(a7)+,d7		; Handle
				beq.s	\NextSector		; Size is already pushed
					bsr	EM_findEmptySlot	; It could not failed since we have empty a sector
					lea	-ARC_ENTRY.HeaderSize(a0),a3	; Dest
					move.w	d7,a0
					trap	#3		; Deref handle
					move.l	a0,a2		; Src
					move.l	(a7)+,d3	; Size
					bsr	FlashWrite	; Write in Flash (Even if it failed, we continue so that maybe the other entries succeed)
					move.w	(a7)+,d0	; Read org handle of the file
					lea	ARC_ENTRY.HeaderSize(a3),a1	; New addr
					lea	HEAP_TABLE,a0
					lsl.w	#2,d0
					move.l	a1,0(a0,d0.w)	; Save new addr of the handle
					move.w	d7,d0
					bsr	HeapFree_reg	; Free temp handle
					bra.s	\RestoreLoop
\NextSector		move.l	a3,a2			; New Start
			adda.l	#$10000,a3		; New End
			clr.b	d3			; No garbesh this sector
			moveq	#0,d5			; 0 entry
			cmp.l	#END_ARCHIVE-1,a2	; Check End of Archive Memory ?
			bls	\SectorLoop
	moveq	#1,d6
\Failed:
	move.l	d6,d0
	move.l	a6,a7
	FLASH_FUNC_OFF
	movem.l	(a7)+,d3-d7/a2-a6
	rts

;void AB_prodid (char *buffer); 
AB_prodid:
	bsr	FL_getHardwareParmBlock
	move.l	2+4*4(a0),-(a7)		; Build Number
	pea	(1).w			; Sofware Revision (Just like AMS 1.01)
	move.l	2+4*1(a0),-(a7)		; Revision Number
	move.l	2+4*0(a0),-(a7)		; hardware Id
	pea	ProductID_str(pc)
	move.l	4+5*4(a7),-(a7)
	bsr	sprintf
	lea	6*4(a7),a7
	rts

;void AB_prodname (char *buffer);
AB_prodname:
	lea	Pedrom_str(pc),a1
	move.l	4(a7),a0
	bra	strcpy_reg

;unsigned short FL_getVerNum (void);
FL_getVerNum:
	moveq	#0,d0			; Too much cryptic :p
	rts

;void cgetsn (char *dest);
cgetsn:
;short AB_serno (char *buffer);
AB_serno:
	trap	#12
	move.w	#$2700,SR
	; Unprotect Flash (SR = $2700)
	move.w	$5EA4,d0	; I read this because on Vti, the address $1C5EA4 & $5EA4 are the same, so if I write 0, it corrupts the heap. Very annoying, no ?
	lea	($1C5EA4).l,a0
	bclr	#1,($600015)	;turn off data to the LCD (RAM is not read)
	nop
	nop
	nop
	move	#$2700,sr
	move.w	d0,(a0)
	nop
	nop
	nop
	move	#$2700,sr
	move.w	d0,(a0)
	bset	#1,($600015)	;turn on LCD
	; Copy Serial Number to RAM
	lea	ROM_BASE+$10000+9,a0	; Certificate Memory + 9 (Well it seems to work on my 92+ HW1 calc)
	lea	EXEC_RAM,a1		; Where to copy
	moveq	#5-1,d0			; 5 Bytes. I don't how to access the last 2 bytes :(
\loop		move.b	(a0)+,(a1)+
		dbf	d0,\loop
	; Protect Flash
	lea	($1C5E00),a0
	bclr	#1,($600015)  ;turn off data to the LCD (RAM is not read)
	nop
	nop
	nop
	move	#$2700,sr
	move.w	(a0),d0
	nop
	nop
	nop
	move.w	#$2700,SR
	move.w	(a0),d0
	bset	#1,$600015
	; User Mode
	move.w	#0,SR
	move.l	4(a7),a0		; Buffer to copy
	move.w	EXEC_RAM+4,-(a7)	; Push Low
	move.l	EXEC_RAM,-(a7)		; Push High
	pea	SerrNo_str(pc)		; Format
	pea	(a0)			; Buffer
	bsr	sprintf			; Sprintf
	lea	(4*3+2)(a7),a7
	rts

; Install a TIB : we use a hack of TIBrecvr of JM.
; I hope I will be able to do another thing.
; I hope there isn't any bug !
TIB_Install:
	; 1. Go to supervisor mode
	trap	#12			
	move.w	#$2700,SR
	; 2. Unprotect Flash (SR = $2700)
	move.w	$5EA4,d0	; I read this because on Vti, the address $1C5EA4 & $5EA4 are the same, so if I write 0, it corrupts the heap. Very annoying, no ?
	lea	($1C5EA4).l,a0
	bclr	#1,($600015)	;turn off data to the LCD (RAM is not read)
	nop
	nop
	nop
	move	#$2700,sr
	move.w	d0,(a0)
	nop
	nop
	nop
	move	#$2700,sr
	move.w	d0,(a0)
	bset	#1,($600015)	;turn on LCD
	; 3. Copy vectors
	lea	VECTORS_TABLE(PC),a0
	lea	$40000,a1			; GHOST SPACE (To avoid unprotection)
	moveq	#$3F,d0
\VECTOR_loop	move.l	(a0)+,(a1)+
		dbf	d0,\VECTOR_loop
	; 4. Copy TIBReceive in RAM.
	lea	TIBReceiv,a0	; Source
	lea	$10000,a1	; Dest
	move.w	#(TIBReceivEnd-TIBReceiv)/4,d0
\CopyLoop	move.l	(a0)+,(a1)+
		dbf	d0,\CopyLoop
	; 5. Prepare registers
	move.b	HW_VERSION,d7
	lea	ROM_BASE,a4
	lea	ROMCALLS_TABLE,a5
	lea	$4C00,sp
	jsr	($10002+$232-$58)	;a3
	; 6. Prepare global vars
	; Zone $4000 -> 00000DFE: addr/handle
	lea	($10002+$DFE-$58),a1
	move.l	#$20000,(a1)+
	clr.w	(a1)+
	; Zone $E000 -> 00000E04: handle/addr/st_flags/rom_base
	;lea	($10002+$E04-$58),a1
	lea	$24002,a0
	clr.w	(a1)+
	movem.l	a0/a3-a4,(a1)
	move.w	#$4E71,($10002+$912-$58) ; TIB Receiver calls ST_HelpMsg after writting the first sector. But ST_HelpMsg is IN the first sector ! So it crashes. Fixs it.
	; 7. Call TIB Receiver $72A-$58
	jsr	($10002+$72A-$58)	; My own dissambly of tibrecivr tells me where is the function to called :)
	; 8. Protect Flash
	lea	($1C5E00),a0
	bclr	#1,($600015)  ;turn off data to the LCD (RAM is not read)
	nop
	nop
	nop
	move	#$2700,sr
	move.w	(a0),d0
	nop
	nop
	nop
	move.w	#$2700,SR
	move.w	(a0),d0
	bset	#1,$600015
	; 9. Display error message and reboot
	lea	TIBInstallError_str(pc),a0
	jmp	SYSTEM_ERROR
		