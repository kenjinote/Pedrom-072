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
;***            	Link routines				***
;***                                                            ***
;******************************************************************

Int4_Off:
	move.w	#$0500,d0				; Set the Interrupt Level to 5
	trap	#1					;
	move.w	d0,LINK_INT_SAVED_MASK			; Save the previous Interrupt Level
	rts
	
;void OSLinkReset (void);
OSLinkReset:
	; Make some delay to be sure the link is finished
	move.w	#$4E20,d0
	dbf	d0,*
	bsr.s	Int4_Off
	lea	$60000C,a0
	move.w	(a0),d0			; Read Link statut
	move.b	#$E0,(a0)
	addq.l	#$60000E-$60000C,a0
	ori.b	#3,(a0)
	move.w	#$100,d0
	dbf	d0,*
	andi.b	#~3,(a0)
	move.w	#$100,d0
	dbf	d0,*
	bra.s	LinkReset

;void OSLinkClose (void);
OSLinkClose:
; FIXME: Check Break Key ?
\Wait:		tst.w	LINK_SEND_QUEUE+QUEUE.used	; We send all the remaining bytes 
		beq.s	LinkReset			; Before close the link
		tst.b	LINK_RESET
		beq.s	\Wait
	bra.s	LinkReset

;void OSLinkOpen (void); 
OSLinkOpen:
	bsr	Int4_Off
LinkReset:
	clr.b	PACKET_CID
	clr.b	LINK_RESET
	clr.b	LINK_RECEIVE_OVERFLOW
	lea	LINK_SEND_QUEUE,a0
	bsr.s	ResetLinkBuffer
	lea	LINK_RECEIVE_QUEUE,a0
	bsr.s	ResetLinkBuffer
	move.b	#$8D,($60000C)

Int4_On:
	move.l	d0,-(a7)				; Push d0
	move.w	LINK_INT_SAVED_MASK,d0			; Get the saved previous Interrupt Level
	trap	#1					; Set it
	move.l	(a7)+,d0				; Pop d0
	rts

ResetLinkBuffer:
	clr.w	(a0)+
	clr.w	(a0)+
	move.w	#LINK_QUEUE.sizeof,(a0)+
	clr.w	(a0)+
	rts

;short OSWriteLinkBlock (const char *buffer, unsigned short num);
OSWriteLinkBlock:
	bsr	Int4_Off
	movea.l	4(a7),a0			; Get adress of buffer
	move.w	8(a7),d0			; Get number of bytes in buffer
	subq.w	#1,d0				; for dbf
	bmi.s	\Error				; We need at least one byte
	cmpi.w	#LINK_QUEUE.sizeof-1,d0		; and less than $80
	bhi.s	\Error		
	lea	LINK_SEND_QUEUE,a1
	move.w	QUEUE.used(a1),d1		; Number of bytes in transmit buffer
	add.w	d0,d1				; futute number of bytes
	addq.w	#1,d1
	cmp.w	4(a1),d1			; Compare to max size transmit buffer
	bhi.s	\Error				; To enought space !
	btst	#1,($60000C)			; Well if it is <> of 1, skip else set to 1
	bne.s	\NoRetrig
		bset	#1,($60000C)
\NoRetrig:
	move.w	d1,QUEUE.used(a1)		; New number of bytes in transmit buffer
	move.w	QUEUE.tail(a1),d2
\loop
		move.b	(a0)+,d1		; Read byte from buffer
		move.b	d1,QUEUE.data(a1,d2.w)	; Add it to trasmit buffer
		addq.w	#1,d2
		cmp.w	QUEUE.size(a1),d2	; Modulo of the Transmit Buffer
		blt.s	\NoZero
			clr.w	d2
\NoZero:	dbf	d0,\loop
	move.w	d2,QUEUE.tail(a1)
	clr.w	d0		
\Error:						; Well, if an error occurs, d0 is different of zero :)
	bra	Int4_On				; Does not destroy d0

;unsigned short OSReadLinkBlock (char *buffer, unsigned short num); 
OSReadLinkBlock:
	bsr	Int4_Off
	movea.l	4(a7),a0	; Get adress
	move.w	8(a7),d0	; Get number of bytes in buffer
	movem.l	d3-d4,-(a7)
	moveq	#0,d3
	lea	LINK_RECEIVE_QUEUE,a1
	move.w	QUEUE.tail(a1),d2
	move.w	QUEUE.used(a1),d1
	beq.s	\Nothing
		; Read bytes
\loop		move.b	QUEUE.data(a1,d2.w),(a0)+
		addq.w	#1,d3
		addq.w	#1,d2
		cmp.w	QUEUE.size(a1),d2
		blt.s	\NoZero
			clr.w	d2
\NoZero		subq.w	#1,QUEUE.used(a1)
		beq.s	\Nothing
		tst.b	LINK_RECEIVE_OVERFLOW	; Useless if we don't set overflow
		beq.s	\NoOverflow		; Usefull only if we do nothing in the int in case of Receiver Buffer overflow
			clr.b	LINK_RECEIVE_OVERFLOW
			move.w	$60000C,d4
			btst	#3,d4	; Internal activity ?
			bne.s	\NoOverflow
			btst	#7,d4	; Link error ?
			beq.s	\NoOverflow
			move.b	#$E0,$60000C
			move.b	#$8D,$60000C
			st.b	LINK_RESET
\NoOverflow	subq.w	#1,d0
		bne.s	\loop
\Nothing
	move.w	d2,QUEUE.tail(a1)
	move.w	d3,d0
	movem.l	(a7)+,d3-d4
	bra	Int4_On
	
;short OSLinkTxQueueActive (void);
OSLinkTxQueueActive:
; FIXME: I think that the test 'btst #1,...' can't be interupted, so I can do not turn off the ints.
	;bsr	Int4_Off
	btst	#1,$60000C
	sne	d0
	ext.w	d0
	;bra	Int4_On
	rts
	
;unsigned short OSLinkTxQueueInquire (void);
OSLinkTxQueueInquire:
	bsr	Int4_Off
	move.w	LINK_SEND_QUEUE+QUEUE.size,d0
	sub.w	LINK_SEND_QUEUE+QUEUE.used,d0
	bra	Int4_On
	
;unsigned short LIO_SendData (const void *src, unsigned long size);
LIO_SendData:
	movem.l	d3-d4/a2,-(a7)
	move.l	$14(a7),d3
	move.l	$10(a7),a2

	move.w	#3,-(a7)		; Restart LIO timer
	bsr	OSTimerRestart
	bsr	OSClearBreak

	bra.s	\cmp
\loop
		tst.b	LINK_RESET
		beq.s	\NoReset
			bsr	OSLinkReset
			moveq	#1,d0
			bra.s	\End
\NoReset:		
		tst.b	BREAK_KEY
		beq.s	\NoBreak
			moveq	#1,d0
			bra.s	\End	
\NoBreak:	bsr	OSTimerExpired
		tst.w	d0
		bne.s	\End		; Timer expired ?
		bsr	OSLinkTxQueueInquire
		moveq	#0,d4
		move.w	d0,d4
		beq.s	\WaitInt
		sub.l	d4,d3
		bge.s	\Ok
			; d3 < d4
			add.l	d4,d3
			move.w	d3,d4
			moveq	#0,d3
\Ok:		bsr	OSTimerRestart	; Send data so restart timer
		; Send d4 bytes
		move.w	d4,-(a7)
		pea	(a2)
		bsr	OSWriteLinkBlock
		addq.l	#6,a7
		add.l	d4,a2
		tst.w	d0
		beq.s	\cmp
			moveq	#1,d0
			bra.s	\End
\WaitInt
	bsr	Idle		; Enter Low power mode
\cmp		tst.l	d3
		bne.s	\loop
	moveq	#0,d0
\End:
	addq.l	#2,a7		; Pop LIO timer
	movem.l	(a7)+,d3-d4/a2
	rts
	
;unsigned short LIO_RecvData (void *dest, unsigned long size, unsigned long WaitDelay); 
LIO_RecvData:
	movem.l	d3/a2,-(a7)
	move.l	4+2*4(a7),a2	; Dest
	move.l	8+2*4(a7),d3	; Len
	move.l	12+2*4(a7),-(a7)	; Delay
	bne.s	\NoSet
		moveq	#-1,d0
		move.l	d0,(a7)	; Set to $FFFFFFF, so it will be forever :)
\NoSet	
	move.w	#3,-(a7)	; LIO timer
	bsr	OSFreeTimer
	bsr	OSRegisterTimer	; Register timer LIO
	bsr	OSClearBreak
	; Loop : Break / Timer / Receive data / Idle
	bra.s	\cmp
\loop
		tst.b	LINK_RESET
		beq.s	\NoReset
			bsr	OSLinkReset
			moveq	#1,d0
			bra.s	\End
\NoReset:		
		tst.b	BREAK_KEY	; Test break Key
		beq.s	\NoBreak
			moveq	#1,d0
			bra.s	\End	
\NoBreak:	bsr	OSTimerExpired	; Check LIO timer
		tst.w	d0
		bne.s	\End		; Timer expired ?
		; Read at most d3 bytes from buffer
		move.w	d3,-(a7)
		pea	(a2)
		bsr	OSReadLinkBlock
		addq.l	#6,a7
		moveq	#0,d1
		move.w	d0,d1
		beq.s	\WaitInt	; We have receive no bytes, so enter low power mode
		sub.w	d0,d3		; Remove d0 bytes from buffer
		adda.l	d1,a2		; Advance Ptr
		bsr	OSTimerRestart	; Restart timer since we have received some bytes
		bra.s	\cmp
\WaitInt	bsr	Idle		; Enter Low power mode
\cmp		tst.w	d3
		bne.s	\loop
	moveq	#0,d0
\End:
	addq.l	#6,a7
	movem.l	(a7)+,d3/a2
	rts
	
; If we receive at least 4 bytes through the link port.
; Test also we are not inside OSLinkCmd.
;short OSCheckSilentLink (void);
OSCheckSilentLink:
	clr.w	d0
	tst.b	PACKET_CID
	bne.s	\Nothing
		cmp.w	#4,LINK_RECEIVE_QUEUE+QUEUE.used
		scc.b	d0
		ext.w	d0
\Nothing
	rts
	

; Read a packet from the link port
; ie Very High Level link routine
ReadPacket:
	movem.l	d3-d5/a6,-(a7)
	move.l	a7,a6
ReadPacket_start
	move.l	a6,a7
	; Read Header (4 bytes) in global var !
	move.l	#30*20,-(a7)		; 30 secondes max
	pea	(4).w			; Header is 4 bytes
	pea	PACKET_MID		; PAcket Addr
	bsr	LIO_RecvData		; Receive 4 bytes ?
	tst.w	d0
	beq.s	\Ok1
		dc.w	$A000+650+1	; Link transmission
\Ok1:	
	; Swap PACKET_LEN to be Big Endian compatible
	move.w	PACKET_LEN,d0
	rol.w	#8,d0
	move.w	d0,PACKET_LEN
	; Printf
	ifd	PRINTF_PACKET_DIALOG
		move.w	PACKET_LEN,-(a7)
		move.w	PACKET_MID,-(a7)
		pea	DebugY_str(pc)
		jsr	printf	
	endif
	; If MID != 89, 09, 88, 08 : Not right calc
	move.b	PACKET_MID,d0
	beq.s	\Ok2			; 00 68 00 00 : Check if AMS if flashed (And calculator).
	cmpi.b	#$89,d0			; Pc to Ti-89 ?
	beq.s	\Ok2
	cmpi.b	#$88,d0			; 92+ to xxx
	beq.s	\Ok2
	cmpi.b	#$98,d0			; 89 to xxx
	beq.s	\Ok2
	cmpi.b	#$08,d0			; Pc to Ti-92+/89
	beq.s	\Ok2
		dc.w	$A000+650+2	; Link transmission
\Ok2	;If cid = VAR, XDP, REQ, SKIP, or RTS, there is extra data
	move.b	PACKET_CID,d4
	cmpi.b	#CID_VAR,d4
	beq.s	\ReceiveData
	cmpi.b	#CID_XDP,d4
	beq.s	\ReceiveData
	cmpi.b	#CID_REQ,d4
	beq.s	\ReceiveData
	cmpi.b	#CID_SKIP,d4
	beq.s	\ReceiveData
	cmpi.b	#CID_RTS,d4
	bne.s	\NoReceiveData
\ReceiveData	; Alloc Handle
		moveq	#0,d3
		move.w	PACKET_LEN,d3	; Len of the data
		move.l	d3,(a7)
		bsr	HeapAlloc
		tst.w	d0
		bne.s	\Ok3
			dc.w	$A000+670	; Memory	
\Ok3		move.w	d0,PACKET_HANDLE
		; Read data
		move.l	#10*20,(a7)		; 10 seconds max
		move.l	d3,-(a7)		; Recieve d3 bytes
		move.w	d0,a0
		trap	#3			; Deref handle
		pea	(a0)			; Push reiceive addr
		bsr	LIO_RecvData		; Receive d3 bytes
		tst.w	d0
		beq.s	\Ok4
			dc.w	$A000+650+1	; Link transmission
\Ok4:		; Read CheckSum
		move.l	#10*20,(a7)		; 10 seconds max
		pea	(2).w			; Recieve 2 bytes
		pea	PACKET_CHECKSUM		; Push reiceive addr
		bsr	LIO_RecvData		; Receive d3 bytes
		tst.w	d0
		beq.s	\Ok5
			dc.w	$A000+650+1	; Link transmission
\Ok5:		; CalcCheckSum
		move.w	PACKET_HANDLE,a0
		trap	#3
		move.w	PACKET_LEN,d1
		bsr	CalcPacketCheckSum		; Calc CheckSum of PACKET_HANDLE size PACKET_LEN
		cmp.w	PACKET_CHECKSUM,d0
		beq.s	\done
			; No => ResetLink / SendPacket(CID_ERR, 0, NULL) / Redo ReadPacket
			bsr	OSLinkReset
			moveq	#CID_ERR,d0	; Command
			moveq	#0,d1		; Len
			suba.l	a0,a0		; Handle
			bsr	SendPacket
			bra	ReadPacket_start
\NoReceiveData
	; CID == ERR => SendPacket(LAST_CID, LAST_LEN, LAST_PTR) / Redo ReadPacket
	cmpi.b	#CID_ERR,d4
	bne.s	\NoError
		move.w	PACKET_LAST_CID,d0
		move.w	PACKET_LAST_LEN,d1
		move.l	PACKET_LAST_PTR,a0
		bsr	SendPacket
		bra	ReadPacket_start
\NoError:
\done:	; CID != ACK => SendPacket(CID_ACK, 0, NULL)
	cmpi.b	#CID_ACK,d4
	beq	\NoAck
		moveq	#0,d1		; Len
		tst.b	PACKET_MID	; If MID = 0, then it is a Checking FLASH request
		bne.s	\Continue
			move.w	#$1001,d1	; Send Packet : $88($98), $56, $01, $10
\Continue	moveq	#CID_ACK,d0	; Command: ACK
		suba.l	a0,a0		; Handle
		bsr	SendPacket
\NoAck:
	move.l	a6,a7
	movem.l	(a7)+,d3-d5/a6
	rts

; In:
;	a0 -> Ptr
;	d1 = Size in bytes			
CalcPacketCheckSum:
	clr.w	d0
	clr.w	d2
	subq.w	#1,d1
\Loop		move.b	(a0)+,d2
		add.w	d2,d0
		dbf	d1,\Loop
	rol.w	#8,d0			; From Big to Little
	rts
	
; Send a packet from the link port
; ie Very High Level link routine
; You should use SendPacket, not this function.
; The goal of this function is to send 4 zeros between the header and the data
; (Len should be the exact value of the sent data ie you should count the 4 zeros).
; The only use of this function is to send the data part of a file
; (Ti protocol needs 4 bytes before the file, why ? I don't know).
; In:
;	d0.b = CID
;	d1.w = Len
;	a0.l -> Ptr
SendPacketAnd4Zeros:
	movem.l	d3-d5/a2/a6,-(a7)
	st.b	d5		; d5 = -1
	bra.s	SendPacket_Entry
	

; Send a packet from the link port
; ie Very High Level link routine
; In:
;	d0.b = CID
;	d1.w = Len
;	a0.l -> Ptr
SendPacket:
	movem.l	d3-d5/a2/a6,-(a7)
	clr.w	d5		; d5 = 0
SendPacket_Entry
	move.l	a7,a6			; Save stack Ptr
	move.w	#DEVICE_LINK_ID*256,d3	; MID | 00
	move.b	d0,d3			; MID | CID
	moveq	#0,d4
	move.w	d1,d4			; Save Len
	move.l	a0,a2			; Save Data Ptr
	; If Cid != ACK
	cmpi.b	#CID_ACK,d3
	beq.s	\NoSendAck
		; FIXME: Problem with 4 '0' ?
		; LastCid = Cid / LastLen = Len / LastPtr = ptr
		move.w	d3,PACKET_LAST_CID
		move.w	d4,PACKET_LAST_LEN
		move.l	a2,PACKET_LAST_PTR
\NoSendAck
	; MakeHeader(MID, CID, ByteReverse(len))
	ror.w	#8,d1			; To big endian
	move.w	d1,-(a7)
	move.w	d3,-(a7)
	move.l	a7,a0
	; SendData(Header, 4)
	pea	(4).w
	pea	(a0)
	bsr	LIO_SendData
	; Printf
	ifd	PRINTF_PACKET_DIALOG
		move.w	d4,-(a7)
		move.w	d3,-(a7)
		pea	DebugX_str(pc)
		jsr	printf	
	endif
	; if (ptr)
	move.l	a2,d0
	beq.s	\NoPtr
		; Check 4 zeros ? <AMS sends 4 '0' before the files. So do I > 
		tst.b	d5
		beq.s	\Normal
			clr.l	(a7)		; Tiny buffer of 4 '0'
			move.l	a7,a0		; Ptr to buffer
			pea	(4).w		; Size
			pea	(a0)		; Ptr
			bsr	LIO_SendData	; Send
			subq.l	#4,d4		; Since we send '0', the calcul of the checksum is still ok !
\Normal		; SendData(ptr, len)
		move.l	d4,(a7)
		pea	(a2)
		bsr	LIO_SendData
		; Calculate CheckSum
		move.l	a2,a0
		move.l	d4,d1
		bsr	CalcPacketCheckSum
		move.w	d0,PACKET_CHECKSUM
		; SendData(CheckSum, 2)
		pea	(2).w
		pea	PACKET_CHECKSUM
		bsr	LIO_SendData
\NoPtr:
	move.l	a6,a7
	movem.l	(a7)+,d3-d5/a2/a6
	rts

; Free the receive Packet
;void FreePacket(void)
FreePacket:
	move.w	PACKET_HANDLE,d0
	beq.s	\NoFree
		bsr	HeapFree_reg
		clr.w	PACKET_HANDLE
\NoFree	rts
		
;void OSLinkCmd (short NormalState);
OSLinkCmd:
	jsr	CU_stop				; Stop the cursor
	bsr	OSClearBreak			; Clear Break Key
	move.l	CURRENT_POINT_X,-(a7)
	movem.l	d3-d7/a2-a6,-(a7)
	move.l	a7,a6				; Save Stack Ptr
			
	tst.b	PACKET_CID			; Check it to avoid infinite loop
	bne	LinkCmdFinal			; ERD_dialog calls ngetchx, which calls OSLinkCmd, which calls ERD_dialog ...
						; FIXME: Sometimes it crashes... Why ?
						; ngetchx ->erd->ngetchx/Repaquet -> Stop due to the PACKET_CID test which doesn't translate the receive packet
						; Fix it in OSCheckSilentLink : Return 0 if PACKET_CID != 0
	pea	LinkProgress_str(pc)
	bsr	ST_helpMsg
	move.w	#2,(a7)				; Timer 2 (APD)
	bsr	OSTimerRestart			; Restart APD !

	; TRY
	lea	-60(a7),a7			; 60 for ErrorFrame
	pea	(a7)
	bsr	ER_catch			; Catch all standard Errors from Ti-Os
	move.w	d0,(a7)				; Check if 0 and push it 
	beq.s	\Start
	; ONERR
		bsr	FreePacket		; Free Packet if needed
		bsr	OSLinkReset		; Reset the link
		st.b	PACKET_CID		; Check it to avoid infinite loop (after OSLinkReset !)
		bsr	ERD_dialog
		bsr	OSClearBreak		; Clear Break
		bsr	OSLinkReset		; Reset the link
		bra	LinkCmdEnd
\Start	bsr	ReadPacket			; ReadPacket()

	; Translate the received packet
TranslatePacket					; Translate Packet
	move.b	PACKET_CID,d3

	; **** ACK or EOT or ERR or RDY ****
	;		Exit
	cmpi.b	#CID_ACK,d3
	beq	LinkCmdSuccess
	cmpi.b	#CID_EOT,d3
	beq	LinkCmdSuccess
	cmpi.b	#CID_ERR,d3
	beq	LinkCmdSuccess
	cmpi.b	#CID_RDY,d3
	beq	LinkCmdSuccess

	; ****	XDP or SKIP ****
	;	(Data but we don't what to do with them)
	;	FreeHandle(PACKET_DATA_HD)
	cmpi.b	#CID_SKIP,d3
	beq.s	\FreePaquet
	cmpi.b	#CID_XDP,d3
	bne.s	\NoXdp
\FreePaquet	
		bsr	FreePacket
		bra	LinkCmdSuccess
\NoXdp:	
	
	; **** VAR or RTS ****
	;	(Sent Variable to receive)
	cmpi.b	#CID_VAR,d3
	beq.s	\ReceiveVar
	cmpi.b	#CID_RTS,d3
	bne	\NoReceiveVar
\ReceiveVar
		move.w	PACKET_HANDLE,a0
		trap	#3			; Deref DATA
		; Check File Type
		lea	SupportedFileType_LIST(pc),a1
		clr.w	d1
		addq.l	#4,a0			; Skip Data Len (We don't care about it !)
		move.b	(a0)+,d1		; Read File Type
		beq.s	\OkExpr
			exg	a0,a1		; Exchange a0 (Data) & a1 (Tab)
			jsr	WordInList_reg	; Check if Type is supported.
			move.l	a1,a0		; Restore data ptr
			tst.w	d0		; Type in list ?
			bne.s	\OkExpr		; No so 
				dc.w	$A000+650+3	; Throw an error		
\OkExpr		;	Copy Name in FOLDER_TEMP buffer.
		clr.w	d0
		move.b	(a0)+,d0		; Var name len
		cmpi.b	#18,d0
		bls.s	\OkLenName
			dc.w	$A000+650+4
\OkLenName	subq.w	#1,d0
		lea	-20(a7),a7
		move.l	a7,a1
		clr.b	(a1)+
\Loop1			move.b	(a0)+,d1	; Copy it to buffer and translate it to lower case.
			cmpi.b	#'A'-1,d1
			bls.s	\NoChg
				cmpi.b	#'Z',d1
				bhi.s	\NoChg
					addi.b	#'a'-'A',d1	; Upper to lower
\NoChg			move.b	d1,(a1)+
			dbf	d0,\Loop1
		clr.b	(a1)
		pea	(a1)
		;	FreeHandle(PACKET_DATA_HD)
		bsr	FreePacket
		;	Check if name already exists (Variable type bof :()
		bsr	SymFindPtr
		move.l	a0,d0
		beq.s	\Ok1
			; We check if var is locked/archived/...
			move.w	SYM_ENTRY.flags(a0),d0
			andi.w	#SF_LOCKED|SF_OPEN|SF_INVIEW|SF_ARCHIVED|SF_TWIN|SF_LOCAL,d0
			beq.s	\Ok1
				;	Yes ? SendPacket(CID_SKIP,1,&One) / Exit
				pea	($0101).w
				move.l	a7,a0		; Ptr to 1
				moveq	#1,d1		; 1 Bytes
				moveq	#CID_SKIP,d0
				bsr	SendPacket
				bra	LinkCmdSuccess
\Ok1		;	SendPacket(CID_CTS, 0, NULL)
		suba.l	a0,a0
		moveq	#0,d1
		moveq	#CID_CTS,d0
		bsr	SendPacket
		;	If (ReadPacket() != ACK)
		;		Redo Translate packet
		bsr	ReadPacket
		cmpi.b	#CID_ACK,PACKET_CID
		bne	TranslatePacket
		;	ReadPacket() != XDP ?
		;		No => Redo translate Packet
		bsr	ReadPacket
		cmpi.b	#CID_XDP,PACKET_CID
		bne	TranslatePacket
		;	Add file with the current file name, and use as handle PACKET_DATA_HD
		; Note: it seems that at the beginning there are 4 bytes with 0
		bsr	SymAdd		; If the file already exists it will be erased.
		move.l	d0,(a7)
		bne.s	\OkAdd
			dc.w	$A000+670
\OkAdd		; Resize correctly Packet Handle by skipping the first 4 bytes
		move.w	PACKET_HANDLE,a0
		trap	#3
		lea	4(a0),a1
		move.w	PACKET_LEN,d0
		lsr.w	#1,d0
		subq.w	#1+1,d0		; 2*2 = 4 bytes + 1 for dbf
\LoopFix		move.w	(a1)+,(a0)+
			dbf	d0,\LoopFix
		moveq	#0,d0
		move.w	PACKET_LEN,d0
		subq.l	#4,d0
		move.l	d0,-(a7)
		move.w	PACKET_HANDLE,-(a7)
		bsr	HeapRealloc
		addq.l	#6,a7	
		; Save handle
		bsr	DerefSym
		move.w	PACKET_HANDLE,SYM_ENTRY.hVal(a0)
		clr.w	PACKET_HANDLE
		;	Exit (No wait for EOT)
		bra	LinkCmdSuccess
\NoReceiveVar
	; **** REQ ****
	;	Request variable
	cmpi.b	#CID_REQ,d3
	bne	\NoRequestVar
		move.w	PACKET_HANDLE,a0
		trap	#3				; Deref the Packet
		move.b	4(a0),d3			; File type Ty
		cmpi.b	#$1A,d3				; Directory List ?
		bne	\RequestFile
			; Request a directory
			; Get the directory name
			suba.l	a3,a3			; Home Name
			clr.w	d1
			move.b	5(a0),d1			; File Name length
			beq.s	\HomeDirList			; If len = 0, it is the Home Directory list
				addq.l	#6,a0
				cmpi.b	#8,d1			; Check len
				bls.s	\OkLenName2		
					dc.w	$A000+650+4	; Throw an error if folder len name too long
\OkLenName2			clr.l	-(a7)
				clr.l	-(a7)
				clr.w	-(a7)			; Create a stack frame
				lea	1(a7),a3
				subq.w	#1,d1			; For dbf
\CopyName3				move.b	(a0)+,(a3)+	; Copy Name in the stack frame
					dbf	d1,\CopyName3
\HomeDirList		
			; Create the directory list on the stack
			; In: a3->SYM_NAME (NULL for home)
			; Format :
			;	88NAME88 Ty Lk LL LH HL HH
			; + 88NAME88 : 8 bytes of the variable name
			; + Ty : type of the variable. See 2.5)
			; + Lk : 00 = nothing, 01 = locked and 03 = archived
			; + LL LH HL HH : size of the variable or the number of vars in the folder.
			moveq	#0,d4		; Cpt FIXME: Maybe I should add a dummy entry ?
			move.w	#FO_SINGLE_FOLDER,-(a7)
			pea	(a3)
			bsr	SymFindFirst
			move.l	a7,a4
\DirLoop			move.l	a0,d0
				beq.s	\DirDone
				add.w	#8+2+4,d4		; One more file	
				lea	-(8+2+4)(a7),a7		; Alloca on the stack
				move.l	SYM_ENTRY.name(a0),(a7)		; Copy SYM NAME (1)
				move.l	SYM_ENTRY.name+4(a0),4(a7)	; Copy SYM NAME (2)
				clr.w	8(a7)				; Ty / Not lock
				moveq	#0,d0
				move.w	SYM_ENTRY.flags(a0),d0
				move.w	SYM_ENTRY.hVal(a0),a0
				trap	#3
				andi.w	#SF_FOLDER,d0
				bne.s	\DirFolder
					move.w	(a0)+,d0		; Size of the file
					move.b	-1(a0,d0.l),d1		; TAG of file
					addq.w	#2,d0			; Add size size
					lea	TagTranslation(pc),a0
\TagCont					move.b	(a0)+,d2
						beq.s	\TagDone
						cmp.b	d1,d2
						bne.s	\TagCont
\TagDone				move.b	(TypeTranslation-TagTranslation-1)(a0),d1
					bra.s	\DirNext
\DirFolder				;move.w	2(a0),d0		; Number of files in folder
					moveq	#0,d0
					moveq	#$1F,d1
\DirNext			move.b	d1,8(a7)			; Type of var
				ror.w	#8,d0
				swap	d0			; Swap to Big Endian
				move.l	d0,10(a7)		; Size of the VAR
				bsr	SymFindNext
				bra.s	\DirLoop
\DirDone		; First Entry is the folder itself if isn't home
			move.l	a3,d0				; Check if it is home
			beq.s	\NoAddFolderEntry
				clr.l	-(a7)			; Len = 0
				move.w	#$1F00,-(a7)		; Type & Flags
				subq.l	#8,a7			; Alloca on the stack
\CvtFoldName				tst.b	-(a3)
					bne.s	\CvtFoldName
				addq.l	#1,a3			; Skip first (0)
				move.l	a7,a0			; Write here
				moveq	#8-1,d0			
\FoldNameLoop				move.b	(a3)+,(a0)+	; Copy 8 chars
					dbf	d0,\FoldNameLoop	; Name
				add.w	#8+2+4,d4		; One more file	
\NoAddFolderEntry			
			;	FreeHandle(PACKET_DATA_HD)
			bsr	FreePacket
			; Send a Dummy VAR Header (????) -???- How it knows the folder's files number ?
			ror.w	#8,d4				; Reverse the size.			
			lea	(DummyVarHeader_END+1),a0	; Since we want to patch it, push it on the stack 
			move.l	-(a0),-(a7)
			move.l	-(a0),-(a7)			
			move.l	-(a0),-(a7)
			move.w	d4,(a7)
			move.l	a7,a0
			moveq	#DummyVarHeader_END-DummyVarHeader,d1
			moveq	#CID_VAR,d0
			bsr	SendPacket
			lea	12(a7),a7
			; Wait for Ack
			bsr	ReadPacket
			cmpi.b	#CID_ACK,PACKET_CID
			bne	TranslatePacket
			; Wait for Wait DATA
			bsr	ReadPacket
			cmpi.b	#CID_CTS,PACKET_CID
			bne	TranslatePacket
			; Send a Directory List
			move.l	a4,d1
			sub.l	a7,d1				
			addq.l	#4,d1				; Len of the packet
			moveq	#CID_XDP,d0
			move.l	a7,a0
			bsr	SendPacketAnd4Zeros
			; Wait for ACK ?
			bsr	ReadPacket
			cmpi.b	#CID_ACK,PACKET_CID
			bne	TranslatePacket
			; Send EOT but don't wait for ACK
			move.w	#CID_EOT,d0
			clr.w	d1
			suba.l	a0,a0
			bsr	SendPacket
			bra	LinkCmdSuccess		
\RequestFile
		;	 Else request a file
		clr.w	d1
		move.b	5(a0),d1		; File Name length
		beq.s	\FileNameError32	; If NULL, then throw an error
		cmpi.b	#17,d1			; Check len
		bls.s	\OkLenName22		; < 17 ?
\FileNameError32	dc.w	$A000+650+4	; Throw an error if folder len name too long
\OkLenName22	lea	-20(a7),a7		; Create a stack frame
		addq.l	#6,a0			; Len
		move.l	a7,a2			; Pointer to buffer
		clr.b	(a2)+			; NULL the first byte
		subq.w	#1,d1			; For dbf
\CopyName4		move.b	(a0)+,(a2)+	; Copy Name in the stack frame
			dbf	d1,\CopyName4
		; Free packet handle
		bsr	FreePacket
		; Send var to the calc
		clr.b	(a2)			; NULL string
		pea	(a2)			; Push SYM Name
		bsr	sendcalc		; Send var
		bra	LinkCmdSuccess		
\NoRequestVar

	; ****  SCR **** 
	cmpi.b	#CID_SCR,d3
	bne	\NoScreen
		;	SendPacket(CID_XDP, 3840, LCD_MEM)
		lea	LCD_MEM,a0
		move.w	#3840,d1
		moveq	#CID_XDP,d0
		bsr	SendPacket
		bra	LinkCmdSuccess		
\NoScreen

	; **** CMD ****
	cmpi.b	#CID_CMD,d3
	bne.s	\NoRemoteControl
		move.w	PACKET_LEN,-(a7)
		bsr	pushkey			; Send Key
		addq.l	#2,a7
		bra	LinkCmdSuccess		
\NoRemoteControl
	
	; **** Default ****
	dc.w	$A000+650+5			; Link Transmission

	; **** End of SWITCH CASE ****
LinkCmdSuccess
	bsr	ER_success
LinkCmdEnd
	bsr	ST_eraseHelp
	clr.b	PACKET_CID
LinkCmdFinal
	move.l	a6,a7
	movem.l	(a7)+,d3-d7/a2-a6
	move.l	(a7)+,CURRENT_POINT_X
	rts

;void CheckLinkLockFlag (const SYM_ENTRY *FuncSymEntry);
CheckLinkLockFlag:
	move.l	4(a7),a0
	move.w	SYM_ENTRY.hVal(a0),-(a7)
	bsr	HToESI
	addq.l	#2,a7
	cmpi.b	#$DC,(a0)	; Check Func/Prgm tag
	bne.s	\NotPrgm
		subq.l	#2,a0
		move.l	4(a7),a1
		move.w	SYM_ENTRY.flags(a1),d0
		btst	#3,d0
		beq.s	\DoIt
		btst	#9,d0
		bne.s	\DoIt
			bset	#0,(a0)
			rts
\DoIt		bclr	#0,(a0)
\NotPrgm:
	rts

;void cmd_sendcalc (SYM_STR SymName);
;void cmd_sendchat (SYM_STR SymName);
cmd_sendchat:
cmd_sendcalc:
	link	a6,#$FFA0
	bsr	OSLinkOpen
	pea	-$5A(a6)
	bsr	ER_catch
	move.w	d0,(a7)
	bne.s	\Error
		pea	LinkProgress_str(pc)
		bsr	ST_helpMsg
		clr.l	(a7)		; No compat info
		move.w	#DEVICE_LINK_ID,-(a7)	; Device = Calculator
		clr.w	-(a7)		; Sys Var = False
		move.l	8(a6),-(a7)	; SYM_STR
		bsr.s	sendcalc
		bsr	ER_success
		bra.s	\Done
\Error:	bsr	ERD_dialog		; Display Error
	bsr	OSClearBreak		; Clear Break
\Done:	bsr	OSLinkClose
	bsr	ST_eraseHelp
	moveq	#1,d0
	unlk	a6
	rts
	
;unsigned short sendcalc (SYM_STR SymName, short allowSysVars, unsigned short DevType, unsigned char *compat); 
sendcalc:
	move.l	4(a7),a0		; SymName
	move.w	10(a7),d0		; DevType
	movem.l	d3-d4/a2-a3,-(a7)
	lea	-60(a7),a7
	move.l	a7,a3			; Temp usage for sending
	move.w	d0,d3			; DevType (Not used yet :()
	pea	(a0)			; SymStr
	bsr	SymFindPtr
	addq.l	#4,a7
	move.l	a0,d0
	beq	\Error0
		bsr	Sym2HSym	; Get HSym
		move.l	d0,d4		; Save HSym
		move.w	SYM_ENTRY.hVal(a0),a0
		trap	#3
		; Create Var Header
		move.l	a3,a1
		move.w	(a0),d1		; var size
		addq.w	#2,d1		; Real var size
		ror.w	#8,d1
		move.w	d1,(a1)+
		clr.w	(a1)+		; (a1).l = LL LH HL HH where HHHLLHLL is the size of the var in memory (real size + 2) 
		move.w	#$21*256,(a1)+	; Asm Type ;) Name Size = 0
		pea	(a1)		; Buffer
		move.l	d0,-(a7)	; HSym
		bsr	HSYMtoName
		addq.l	#4,a7
		sub.l	(a7)+,a1	; a1 = Size
		move.w	a1,d1
		move.b	d1,5(a3)	; Save Len size
		; Send Var Header
		moveq	#CID_VAR,d0	; CID = VAR
		addq.w	#6,d1		; +4 (LONG SIZE) +1 (Type) +1 (Name Size)
		move.l	a3,a0		; Ptr
		bsr	SendPacket
		; Wait for ACK ?
		bsr	ReadPacket
		cmpi.b	#CID_ACK,PACKET_CID
		bne.s	\Error
		; Wait for CTS ?
		bsr	ReadPacket
		cmpi.b	#CID_CTS,PACKET_CID
		bne.s	\Error
		; Send data part (do not forget 4x0)
		move.l	d4,d0		; Reget HSym
		bsr	DerefSym_Reg	; Rederef HSym
		move.w	SYM_ENTRY.hVal(a0),a0
		trap	#3		; Ptr
		move.w	(a0),d1		; Len
		addq.w	#4+2,d1		; + 4 '0' +2 for the size
		moveq	#CID_XDP,d0	; CID
		bsr	SendPacketAnd4Zeros
		; Wait for ACK ?
		bsr	ReadPacket
		cmpi.b	#CID_ACK,PACKET_CID
		bne.s	\Error
		; Send EOT but don't wait for ACK
		move.w	#CID_EOT,d0
		clr.w	d1
		suba.l	a0,a0
		bsr	SendPacket
		moveq	#0,d0
		bra.s	\Done		
\Error0	moveq	#2,d0
	bra.s	\Done
\Error:	moveq	#1,d0
\Done:	lea	60(a7),a7
	movem.l	(a7)+,d3-d4/a2-a3
	rts
	
;void getcalc (SYM_STR SymName); 
;void cmd_getcalc (SYM_STR SymName);
cmd_getcalc:
getcalc:
	move.l	4(a7),a0		; SymName
	movem.l	d3-d4/a2-a3,-(a7)
	lea	-60(a7),a7
	move.l	a0,a2			; Save Sym Name
	move.l	a7,a3			; Temp usage for sending
	; Create Var Header
	move.l	a3,a1
	clr.l	(a1)+			; (a1).l = LL LH HL HH where HHHLLHLL is the size of the var in memory (so fill with 0).
	move.w	#$21*256,(a1)+		; Asm Type ;) Name Size = 0
\Cvt		tst.b	-(a0)		; Get first char
		bne.s	\Cvt
	addq.l	#1,a0			; Skip 0
	moveq	#-1,d1
\loop		addq.w	#1,d1		; d1.w = strlen(symstr)
		move.b	(a0)+,(a1)+	; Copy SymName to Buffer
		bne.s	\loop	
	move.b	d1,5(a3)		; Save Len size
	; Request Var Header
	addq.w	#6,d1
	move.w	#CID_REQ,d0		; CID = REQ
	move.l	a3,a0			; Ptr
	bsr	SendPacket		; Send a request for a file
	; Wait for ACK ?
	bsr	ReadPacket
	cmpi.b	#CID_ACK,PACKET_CID
	bne.s	\Error
	; EndLess loop
\Bloop		bsr	OSCheckSilentLink	; Something received in the link port ?
		tst.w	d0
		beq.s	\NoLink
			bsr	OSLinkCmd	; Yes -> Interpret command
\NoLink:	move.l	a2,(a7)			; Check if Sym has been received
		bsr	SymFindPtr		; Search for it
		move.l	a0,d0
		bne.s	\Done
		tst.b	BREAK_KEY		; Check for Break Key
		beq.s	\Bloop
	bsr	OSClearBreak
	moveq	#2,d0
	bra.s	\Done
\Error:	moveq	#1,d0
\Done:	lea	60(a7),a7
	movem.l	(a7)+,d3-d4/a2-a3
	rts
	

DummyVarHeader:
	dc.b	$6C,$01,$00,$0E,$1A,$04,'m','a','i','n',$03
DummyVarHeader_END:
	dc.b	$00
	EVEN
SupportedFileType_LIST:
	dc.w	4,6,$a,$b,$c,$d,$e,$10,$12,$13,$14,$1C,$21,0
TagTranslation:				dc.b	$F3,$F8,$E0,$DC,$DF,$2D,$00
TypeTranslation:			dc.b	$21,$1C,$0B,$12,$10,$0C,$00
