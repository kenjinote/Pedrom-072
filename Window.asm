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
; 			Windows functions
; ***************************************************************

; ***************************************************************
; 			Internal functions
; ***************************************************************
; Fonctions legerement inspires de graphlib.

frame2:
;Input: d0.w = x
;	d1.w = y
;	d3.w = ATTR
;	d4.w = width
;	d5.w = height
	movem.l	d0-d5,-(a7)
	move.w	d0,d2		;pour x2
	add.w	d4,d2
	subq.w	#1,d2
	addq.w	#1,d0
	bsr	horiz
	add.w	d5,d1
	bsr	horiz
	subq.w	#1,d0
	addq.w	#1,d2
	sub.w	d5,d1
	move.w	d1,d2
	add.w	d5,d2
	addq.w	#1,d1
	subq.w	#1,d2
	bsr	vert
	add.w	d4,d0
	bsr	vert
	movem.l	(a7)+,d0-d5
	rts

fill:
;Input:	d0.w = x
;	d1.w = y
;	d2.w = width
;	d3.w = height
;	d4.w = ATTR
	movem.l d0-d4,-(a7)
	add.w	d0,d2
	exg	d3,d4
\loop:		bsr	horiz
		addq.w	#1,d1
		dbra	d4,\loop
	movem.l	(a7)+,d0-d4
	rts

box:
;Input: d0.w = x
;	d1.w = y
;	d2.w = width
;	d3.w = height
;	a0.l = title of the box or NULL
	movem.l	d0-d7/a0-a6,-(a7)
	; Fill in white
	moveq	#0,d4
	bsr.s	fill
	; Cadre
	move.w	d2,d4
	move.w	d3,d5
	moveq	#1,d3		;pour faire du noir
	bsr.s	frame2
	addq.w	#1,d0
	addq.w	#1,d1
	subq.w	#2,d4
	subq.w	#2,d5
	bsr.s	frame
	addq.w	#2,d0
	addq.w	#2,d1
	subq.w	#4,d4
	subq.w	#4,d5
	bsr.s	frame
	; Title in black
	move.l	a0,d2
	beq.s	\NoTitle
		move.w	d4,d2
		moveq	#8,d3
		moveq	#1,d4		;en noir
		bsr.s	fill		;le cadre noir en haut
		; Write Title
		addq.w	#2,d0
		addq.w	#2,d1
		move.b	CURRENT_FONT,d7
		clr.b	CURRENT_FONT
		move.w	#2,-(a7)
		pea	(a0)
		move.w	d1,-(a7)
		move.w	d0,-(a7)
		bsr	DrawStr
		lea	10(a7),a7
		move.b	d7,CURRENT_FONT
\NoTitle
	movem.l	(a7)+,d0-d7/a0-a6
	rts

frame:
;Input: d0.w = x
;	d1.w = y
;	d3.w = ATTR
;	d4.w = width
;	d5.w = height
	movem.l	d0-d5,-(a7)

	move.w	d0,d2		;pour x2
	add.w	d4,d2
	bsr	horiz
	add.w	d5,d1
	bsr	horiz
	sub.w	d5,d1
	move.w	d1,d2
	add.w	d5,d2
	bsr	vert
	add.w	d4,d0
	bsr	vert

	movem.l	(a7)+,d0-d5
	rts

box2:
;Input: d0.w = x
;	d1.w = y
;	d2.w = width
;	d3.w = height
;	a0.l = title of the box or NULL
	movem.l	d0-d7/a0-a6,-(a7)
	; Fill in white
	moveq	#0,d4
	bsr	fill
	; Cadre
	move.w	d2,d4
	move.w	d3,d5
	moveq	#1,d3		;pour faire du noir
	bsr.s	frame
	; Title in black
	move.l	a0,d2
	beq.s	\NoTitle
		move.w	d4,d2
		moveq	#8,d3
		moveq	#1,d4		;en noir
		bsr	fill		;le cadre noir en haut
		; Write Title
		addq.w	#2,d0
		addq.w	#2,d1
		move.b	CURRENT_FONT,d7
		clr.b	CURRENT_FONT
		move.w	#2,-(a7)
		pea	(a0)
		move.w	d1,-(a7)
		move.w	d0,-(a7)
		bsr	DrawStr
		lea	10(a7),a7
		move.b	d7,CURRENT_FONT
\NoTitle
	movem.l	(a7)+,d0-d7/a0-a6
	rts

Button:
;Input: d0.w = x
;	d1.w = y
;	d2.w = width
;	d3.w = Height
;	a0.l = Title of the button or NULL
	movem.l	d0-d7/a0-a6,-(a7)
	; Fill in Black 
	moveq	#1,d4
	bsr	fill
	; Cadre
	move.w	d2,d4
	move.w	d3,d5
	moveq	#0,d3
	addq.w	#1,d0
	addq.w	#1,d1
	subq.w	#2,d4
	subq.w	#2,d5
	bsr	frame
	; Title in A_XOR
	move.l	a0,d2
	beq.s	\NoTitle
		; Write Title
		addq.w	#2,d0
		addq.w	#2,d1
		subq.w	#1,d4
		move.b	CURRENT_FONT,d7
		clr.b	CURRENT_FONT
		move.w	d0,d2
		bsr	StrWidth
		lsr.w	#1,d0
		lsr.w	#1,d4
		add.w	d4,d2
		sub.w	d0,d2		; d2 = x + Width/2 - StrWidth/2
		move.w	#2,-(a7)
		pea	(a0)
		move.w	d1,-(a7)
		move.w	d2,-(a7)
		bsr	DrawStr
		lea	10(a7),a7
		move.b	d7,CURRENT_FONT
\NoTitle
	movem.l	(a7)+,d0-d7/a0-a6
	rts

SaveScreen:
;Input:	d0.w = X of top left-hand corner in bytes(0<X<239)
;	d1.w = Y of top left-hand corner (0<Y<128)
;	d2.w = width in pixels (0<d2<239)
;	d3.w = height (0<d3<128) (-1 to be compatible with fill)
;Output:
;	d4.w = handle to the memory containg the part of the screen.
	movem.l	d0-d3/d5-d7/a0-a6,-(a7)

	; Convert to byte
	add.w	d0,d2		; xmax
	lsr.w	#3,d0		; xmin / 8
	lsr.w	#3,d2		; xmax /8
	sub.w	d0,d2		; len in bytes -1
		
	movem.l	d0-d3,-(a7)
	addq.w	#1,d2		; Width+1
	addq.w	#1,d3		; Height+1
	mulu.w	d3,d2
	addq.l	#8,d2		; Header
	move.l	d2,-(a7)
	bsr	HeapAlloc
	addq.l	#4,a7
	move.w	d0,d4
	movem.l	(a7)+,d0-d3
	tst.w	d4
	beq.s	\exit
	move.w	d4,a0
	trap	#3

	move.w	d0,(a0)+
	move.w	d1,(a0)+
	move.w	d2,(a0)+
	move.w	d3,(a0)+
	
	move.l	CURRENT_SCREEN,a1
	mulu.w	CURRENT_INCY,d1
	add.w	d0,d1
	adda.w	d1,a1
	move.w	CURRENT_INCY,a6
	
\loopy:		move.w	d2,d5
		move.l	a1,a2
\loopx:			move.b	(a2)+,(a0)+
			dbf	d5,\loopx
		adda.w	a6,a1
		dbra	d3,\loopy

\exit	movem.l	(a7)+,d0-d3/d5-d7/a0-a6
	rts

RestoreScreen:
;Input:	d4.w = handle previously created by SaveScreen
	movem.l d0-d7/a0-a6,-(a7)
	
	move.w	d4,-(a7)	; Push Handle
	bsr.s	RedrawBackScreen
	bsr	HeapFree
	addq.l	#2,a7
	movem.l (a7)+,d0-d7/a0-a6
	rts

RedrawBackScreen:
	move.w	d4,a0
	trap	#3
	move.w	(a0)+,d0	; Read the position of the screen
	move.w	(a0)+,d1
	move.w	(a0)+,d2
	move.w	(a0)+,d3
	
	move.l	CURRENT_SCREEN,a1
	mulu.w	CURRENT_INCY,d1
	add.w	d0,d1
	adda.w	d1,a1
	move.w	CURRENT_INCY,a6
	
\loopy:		move.w	d2,d5
		move.l	a1,a2
\loopx:			move.b	(a0)+,(a2)+
			dbf	d5,\loopx
		adda.w	a6,a1
		dbf	d3,\loopy
	rts
		
; ***************************************************************
; 			Tios functions
; ***************************************************************

;SCR_RECT *RectWinToScr (const SCR_RECT *win_area, const WIN_RECT *rect, SCR_RECT *result_area);
RectWinToScr:
	movem.l	d3-d4/a2-a3,-(a7)
	move.l	$14(a7),a1
	move.l	$18(a7),a2
	move.l	$1c(a7),a3
	clr.w	d0
	move.b	(a1),d0
	move.w	(a2),d1
	add.w	d0,d1
	move.w	4(a2),d3
	add.w	d0,d3
	move.b	1(a1),d0
	move.w	2(a2),d2
	add.w	d0,d2
	move.w	6(a2),d4
	add.w	d0,d4
	move.b	2(a1),d0
	cmp.w	d0,d1
	bgt.s	\NoOverlap
		move.b	(a1),d0
		cmp.w	d0,d3
		blt.s	\NoOverlap
			move.b	3(a1),d0
			cmp.w	d0,d2
			bgt.s	\NoOverlap
				move.b	1(a1),d0
				cmp.w	d0,d4
				bge.s	\Clip
\NoOverlap
	suba.l	a0,a0
	bra.s	\End
\Clip	move.b	(a1)+,d0
	cmp.w	d0,d1
	ble.s	\min1
		move.w	d1,d0
\min1	move.b	d0,(a3)+
	move.b	(a1)+,d0
	cmp.w	d0,d2
	blt.s	\min2
		move.w	d2,d0
\min2	move.b	d0,(a3)+
	move.b	(a1)+,d1
	cmp.w	d0,d3
	bgt.s	\max1
		move.w	d3,d0
\max1	move.b	d0,(a3)+
	move.b	(a1)+,d0
	cmp.w	d0,d4
	bgt.s	\max2
		move.w	d4,d0
\max2	move.b	d0,(a3)+
	lea	-4(a3),a0
\End	movem.l	(a7)+,d3-d4/a2-a3
	rts
	
;WIN_RECT *RectWinToWin (const SCR_RECT *win_area, WIN_RECT *rect);
RectWinToWin:
	move.l	4(a7),a1
	move.l	8(a7),a0
	clr.w	d0
	clr.w	d1
	move.b	(a1)+,d0
	move.b	(a1)+,d1
	add.w	d0,(a0)+
	add.w	d1,(a0)+
	add.w	d0,(a0)+
	add.w	d1,(a0)+
	subq.l	#8,a0
	rts
	
	
NoCallBack
	moveq	#1,d0
	rts
		
; void DlgMessage(char *title, char *message);
DlgMessage:				; No button
	lea	ScrRect(pc),a0
	bsr	SetCurClip_reg		; Set current clipping
	move.l	4(a7),a0		; Title
	move.l	8(a7),a1		; Msg
	movem.l	d3-d7/a2-a6,-(a7)
	move.l	a1,a3			; Msg
	lea	-60(a7),a7
	move.l	a7,a2			; Window
	move.w	#WINDOW_Y+WINDOW_HEIGHT,-(a7)
	move.w	#WINDOW_X+WINDOW_WIDTH,-(a7)
	move.w	#WINDOW_Y,-(a7)
	move.w	#WINDOW_X,-(a7)
	move.l	a7,a1
	pea	(a0)			; Title
	move.w	#WF_SAVE_SCR|WF_DUP_SCR|WF_TTY|WF_ROUNDEDBORDER|WF_TITLE,-(a7)
	pea	(a1)
	pea	(a2)
	bsr	WinOpen
	move.l	a2,a7
	tst.w	d0
	beq.s	\memory
		pea	(a3)		; Msg
		pea	(a2)
		bsr	WinStr		; Disply msg
		bsr	WinActivate
		move.w	#9,(a7)
		bsr	ST_refDsp	; Enter = Ok ESC = CANCEL
		addq.l	#8,a7
\loop			bsr	GetKey
			cmpi.w	#KEY_ENTER,d0
			beq.s	\done2
			cmpi.w	#KEY_ESC,d0
			beq.s	\done2
			bra.s	\loop
\done2:		move.w	d0,d4
		pea	(a2)
		bsr	WinClose
		addq.l	#4,a7
		bra.s	\done
\memory:	move.l	a3,a0
		bsr	ST_helpMsg_reg
		clr.w	d4
\done:	lea	60(a7),a7
	move.w	d4,d0
	movem.l	(a7)+,d3-d7/a2-a6
	rts

ERD_process:
ERD_dialog:
	move.w	4(a7),-(a7)
	bsr.s	find_error_message
	pea	(a0)
	pea	Error_str(pc)
	bsr	DlgMessage		; No button !
	lea	10(a7),a7
	rts
	
ER_STRING	MACRO
	cmpi.w	#\1,d0
	bne.s	\\@Next
		lea	\2(pc),a0
\\@Next			
		ENDM

find_error_message:
	move.w	4(a7),d0
	lea	UnkwowError_str(pc),a0
	ER_STRING	40,ArgumentError_str
	ER_STRING	140,ArgumentNameError_str
	ER_STRING	180,BreakError_str
	ER_STRING	270,DuplicateError_str
	ER_STRING	330,FolderError_str
	ER_STRING	650,LinkTransmission_str
	ER_STRING	670,MemoryError_str
	ER_STRING	910,SyntaxError_str
	ER_STRING	930,TooFewError_str
	ER_STRING	940,TooManyError_str
	ER_STRING	990,Variable8Error_str
	ifeq	0
	ER_STRING	650+1,TimeOut_str
	ER_STRING	650+2,MIDError_str
	ER_STRING	650+3,VARError_str
	ER_STRING	650+4,LFormatError_str
	ER_STRING	650+5,CIDError_str
	endif
	rts

; ***************************************************************
; 			Windows functions
; ***************************************************************

;short WinAttr (WINDOW *w, short Attr); 
WinAttr:
	move.l	4(a7),a0	; Windows
	move.b	WINDOW.CurAttr(a0),d0
	move.b	9(a7),WINDOW.CurAttr(a0)
	rts
	
;void WinBackground (WINDOW *w, short Attr);
WinBackground
	move.l	4(a7),a0	; Windows
	move.w	8(a7),WINDOW.Background(a0)
	rts

;void WinFont (WINDOW *w, short Font);
WinFont:
	move.l	4(a7),a0	; Windows
	move.b	9(a7),d0
	move.b	d0,WINDOW.CurFont(a0)
	move.b	d0,CURRENT_FONT
	rts
	
;void WinGetCursor (WINDOW *w, short *x, short *y);
WinGetCursor
	move.l	4(a7),a0	; Windows
	move.l	8(a7),a1
	move.w	WINDOW.CursorX(a0),(a1)
	move.l	12(a7),a1
	move.w	WINDOW.CursorY(a0),(a1)
	rts

;short WinHeight (WINDOW *w); 
WinHeight:
	move.l	4(a7),a0
	clr.w	d0
	move.b	WINDOW.Client+3(a0),d0
	sub.b	WINDOW.Client+1(a0),d0
	addq.w	#1,d0
	rts

;void WinHome (WINDOW *w);
WinHome:
	move.l	4(a7),a0
WinHome_reg
	clr.w	WINDOW.CursorX(a0)
	clr.w	WINDOW.CursorY(a0)
	move.w	WINDOW.Flags(a0),d0
	btst.l	#6,d0
	beq.s	\Ok
		addq.w	#1,WINDOW.CursorX(a0)
		addq.w	#1,WINDOW.CursorY(a0)
\Ok	rts

;void WinMoveCursor (WINDOW *w, short x, short y); 
WinMoveCursor:
	move.l	4(a7),a0
	move.w	8(a7),WINDOW.CurX(a0)
	move.w	8(a7),WINDOW.CursorX(a0)
	move.w	10(a7),WINDOW.CurY(a0)
	move.w	10(a7),WINDOW.CursorY(a0)
	rts
	
;void WinMoveRel (WINDOW *w, short dx, short dy); 
WinMoveRel:
	move.l	4(a7),a0
	move.w	8(a7),d0
	add.w	d0,WINDOW.CurX(a0)
	move.w	10(a7),d0
	add.w	d0,WINDOW.CurY(a0)
	rts
	
;void WinMoveTo (WINDOW *w, short x, short y); 
WinMoveTo:
	move.l	4(a7),a0
	move.w	8(a7),WINDOW.CurX(a0)
	move.w	10(a7),WINDOW.CurY(a0)
	rts

;short WinWidth (WINDOW *w)
WinWidth:
	move.l	4(a7),a0
	clr.w	d0
	move.b	WINDOW.Client+2(a0),d0
	sub.b	WINDOW.Client+0(a0),d0
	addq.w	#1,d0
	rts

; In: 
;	a0 -> Ptr to the window
;	a1 -> Ptr to rect
; Out:
;	WIN_RECT will contain the right WinRect
_WinRectToGlobal:
	move.w	(a1)+,WIN_RECT
	move.w	(a1)+,WIN_RECT+2
	move.w	(a1)+,WIN_RECT+4
	move.w	(a1)+,WIN_RECT+6
	moveq	#0,d0
	move.b	WINDOW.Client+0(a0),d0
	swap	d0
	move.b	WINDOW.Client+1(a0),d0
	add.l	d0,WIN_RECT
	add.l	d0,WIN_RECT+4
	rts
	
; Translate to global position
; In: 
;	a0 -> WINDOW
;	d0.w = X poistion
;	d1.W = y 
; Out:
;	d0.W = x
;	d1.W = y
; Destroy d2
_WinXYToGlobal:
	clr.w	d2
	move.b	WINDOW.Client(a0),d2
	add.w	d2,d0
	move.b	WINDOW.Client+1(a0),d2
	add.w	d2,d1
	rts
		
; In:
;	Pushed twice Window
; Out:
;	d0.w = x Client (Top left)
;	d1.w = y client (Top left )
; Set all the global system vars to fit the vars of the window	
; Note: Tios seems not to restore the system vars
_WinSetSystemVar:
	move.l	8(a7),a0
	move.l	a0,CURRENT_WINDOW
	move.b	WINDOW.CurFont(a0),CURRENT_FONT
	move.b	WINDOW.CurAttr(a0),CURRENT_ATTR+1
	move.w	WINDOW.CurX(a0),d0
	move.w	WINDOW.CurY(a0),d1
	bsr.s	_WinXYToGlobal
	move.w	d0,CURRENT_POINT_X
	move.w	d1,CURRENT_POINT_Y
	pea	(a0)
	lea	WINDOW.Clip(a0),a0
	bsr	SetCurClip_reg
	move.l	(a7)+,a0
	move.l	WINDOW.Screen(a0),CURRENT_SCREEN	; Back Writting
	clr.w	d0
	move.b	WINDOW.Client+0(a0),d0
	clr.w	d1
	move.b	WINDOW.Client+1(a0),d1
	rts

; In:
;	Nothing 
; out:
;	a0 -> Window
;	Z flag set
; Note: It calls also PortRestore
_WinIsVisible:
	bsr	PortRestore			; Restore SCREEN
	move.l	CURRENT_WINDOW,a0
	btst.b	#7,WINDOW.Flags+1(a0)		; $80
	rts

WinPixSet:
	bsr	_WinSetSystemVar		; Set global vars
	add.w	8(a7),d0			; Read X
	add.w	10(a7),d1			; Read Y
	bsr	DrawClipPix_reg			; Write pixel at (x,y)
	bsr	_WinIsVisible
	beq.s	\End				; No so quit Yes, so write to LCD_MEM
		bsr	_WinSetSystemVar	; Set global vars
		add.w	8(a7),d0		; Read X
		add.w	10(a7),d1		; Read Y
		bsr	DrawClipPix_reg		; Draw pixel
\End:	rts

WinPixGet:
	bsr	_WinSetSystemVar		; Set global vars
	add.w	8(a7),d0			; Read X
	add.w	10(a7),d1			; Read Y
	lea	WINDOW.Clip(a0),a1		; Check clipping
	clr.w	d2
	move.b	(a1)+,d2
	cmp.w	d2,d0
	blt.s	\Error
	move.b	(a1)+,d2
	cmp.w	d2,d1
	blt.s	\Error
	move.b	(a1)+,d2
	cmp.w	d2,d0
	bgt.s	\Error
	move.b	(a1)+,d2
	cmp.w	d2,d1
	bgt.s	\Error
	bsr	GetPix_reg			; Get pixel at (x,y)
\End	bra	PortRestore			; Restore SCREEN (Does not alter any registers)
\Error	moveq	#0,d0
	bra.s	\End

WinClr:
	bsr	_WinSetSystemVar
	move.w	WINDOW.Background(a0),-(a7)
	pea	WINDOW.Clip(a0)
	pea	WINDOW.Client(a0)
	bsr	ScrRectFill
	bsr	_WinIsVisible
	beq.s	\No
		bsr	ScrRectFill
\No	lea	10(a7),a7
	bra	WinHome


;void WinFill (WINDOW *w, const WIN_RECT *rect, short Attr);
WinFill:
	bsr	_WinSetSystemVar
	move.w	12(a7),-(a7)		; Push ATTR
	pea	WINDOW.Clip(a0)		; Push Clip
	move.l	8+6(a7),a1		; WinRect *
	bsr	_WinRectToGlobal
	lea	WIN_RECT,a0
	move.l	a0,a1
	moveq	#4-1,d1
\loop		move.w	(a0)+,d0	; Read X
		bge.s	\ok1		; Check < 0
			clr.w	d0	; = 0
\ok1		cmpi.w	#240,d0		; Check overflow
		blt.s	\ok2
			move.w	#240,d0
\ok2		move.b	d0,(a1)+
		dbf	d1,\loop
	pea	WIN_RECT		; Push RECT
	bsr	ScrRectFill
	bsr	_WinIsVisible
	beq.s	\No
		bsr	ScrRectFill
\No	lea	10(a7),a7
	rts

;void WinFillTriangle (WINDOW *w, short x0, short y0, short x1, short y1, short x2, short y2, short Attr);
WinFillTriangle:
	bsr	_WinSetSystemVar
	move.w	20(a7),-(a7)		; Push ATTR
	pea	WINDOW.Clip(a0)
	move.w	16+6(a7),d0		; x2
	move.w	18+6(a7),d1		; y2
	bsr	_WinXYToGlobal
	move.w	d1,-(a7)		; Push y2
	move.w	d0,-(a7)
	move.w	16+6(a7),d0		; x1
	move.w	18+6(a7),d1		; y1
	bsr	_WinXYToGlobal
	move.w	d1,-(a7)		; Push y1
	move.w	d0,-(a7)		; Push x1
	move.w	16+6(a7),d0		; x0
	move.w	18+6(a7),d1		; y0
	bsr	_WinXYToGlobal
	move.w	d1,-(a7)		; Push y0
	move.w	d0,-(a7)		; Push x0
	bsr	FillTriangle
	bsr	_WinIsVisible
	beq.s	\No
		bsr	FillTriangle
\No	lea	18(a7),a7
	rts
	
;void WinFillLines2 (WINDOW *w, const WIN_RECT *lower_line, const WIN_RECT *upper_line, short Attr);
WinFillLines2:
	bsr	_WinSetSystemVar
	lea	-16(a7),a7		; Frame
	move.w	16+16(a7),-(a7)		; Push ATTR
	pea	WINDOW.Clip(a0)
	move.l	6+16+12(a7),a1		; upper_line
	move.w	(a1)+,d0
	move.w	(a1)+,d1
	bsr	_WinXYToGlobal
	move.w	d0,6+8(a7)
	move.w	d1,6+10(a7)
	move.w	(a1)+,d0
	move.w	(a1)+,d1
	bsr	_WinXYToGlobal
	move.w	d0,6+12(a7)
	move.w	d1,6+14(a7)
	move.l	6+16+8(a7),a1		; lower_line
	move.w	(a1)+,d0
	move.w	(a1)+,d1
	bsr	_WinXYToGlobal
	move.w	d0,6+0(a7)
	move.w	d1,6+2(a7)
	move.w	(a1)+,d0
	move.w	(a1)+,d1
	bsr	_WinXYToGlobal
	move.w	d0,6+4(a7)
	move.w	d1,6+6(a7)
	pea	6+8(a7)
	pea	4+6(a7)
	bsr	FillLines2
	bsr	_WinIsVisible
	beq.s	\No
		bsr	FillLines2
\No	lea	16+14(a7),a7
	rts

;void WinEllipse (WINDOW *w, short x, short y, short a, short b);
WinEllipse:
	bsr	_WinSetSystemVar
	move.w	CURRENT_ATTR,-(a7)	; Push ATTR
	pea	WINDOW.Clip(a0)		; Clip
	move.w	14(a7),-(a7)		; B
	move.w	14(a7),-(a7)		; A
	move.w	14(a7),d1		; Y
	move.w	12(a7),d0		; X
	bsr	_WinXYToGlobal
	move.w	d1,-(a7)		; Y
	move.w	d0,-(a7)		; X
	bsr	DrawClipEllipse 
	bsr	_WinIsVisible
	beq.s	\No
		bsr	DrawClipEllipse
\No	lea	14(a7),a7
	rts
	
;void WinRect (WINDOW *w, const WIN_RECT *rect, short Attr);
WinRect:
	bsr	_WinSetSystemVar
	move.w	12(a7),-(a7)		; Push ATTR
	pea	WINDOW.Clip(a0)		; Push Clip
	move.l	8+6(a7),a1		; Rect *
	bsr	_WinRectToGlobal
	pea	WIN_RECT		; Push RECT
	bsr	DrawClipRect
	bsr	_WinIsVisible
	beq.s	\No
		bsr	DrawClipRect
\No	lea	10(a7),a7
	rts

;void WinLine (WINDOW *w, const WIN_RECT *Line); 
WinLineNC:
WinLine:
	bsr	_WinSetSystemVar
	move.l	8(a7),a1		; WIN_RECT ptr
	move.w	(a1)+,d0		; x
	move.w	(a1)+,d1		; y
	bsr	_WinXYToGlobal
	move.w	d0,WIN_RECT
	move.w	d1,WIN_RECT+2
	move.w	(a1)+,d0
	move.w	(a1)+,d1
	bsr	_WinXYToGlobal
	move.w	d0,WIN_RECT+4
	move.w	d1,WIN_RECT+6
	move.w	CURRENT_ATTR,-(a7)	; Push attr
	pea	WINDOW.Clip(a0)
	pea	WIN_RECT
	bsr	DrawClipLine
	bsr	_WinIsVisible
	beq.s	\No
		bsr	DrawClipLine
\No	lea	10(a7),a7
	rts
	
;void WinLineTo (WINDOW *w, short x, short y);
WinLineTo:
	bsr	_WinSetSystemVar
	move.w	CURRENT_POINT_X,WIN_RECT+0
	move.w	CURRENT_POINT_Y,WIN_RECT+2
	move.w	8(a7),d0
	move.w	10(a7),d1
	move.w	d0,WINDOW.CurX(a0)
	move.w	d1,WINDOW.CurY(a0)
	bsr	_WinXYToGlobal
	move.w	d0,WIN_RECT+4
	move.w	d1,WIN_RECT+6
	move.w	CURRENT_ATTR,-(a7)	; Push attr
	pea	WINDOW.Clip(a0)
	pea	WIN_RECT
	bsr	DrawClipLine
	bsr	_WinIsVisible
	beq.s	\No
		bsr	DrawClipLine
\No	lea	10(a7),a7
	rts

;void WinLineRel (WINDOW *w, short dx, short dy); 
WinLineRel:
	bsr	_WinSetSystemVar
	move.w	CURRENT_POINT_X,WIN_RECT+0
	move.w	CURRENT_POINT_Y,WIN_RECT+2
	move.w	8(a7),d0		; dx
	move.w	10(a7),d1		; dy
	add.w	CURRENT_POINT_X,d0
	add.w	CURRENT_POINT_Y,d1
	move.w	d0,WIN_RECT+4
	move.w	d1,WIN_RECT+6
	move.w	CURRENT_ATTR,-(a7)	; Push attr
	pea	WINDOW.Clip(a0)
	pea	WIN_RECT
	bsr	DrawClipLine
	bsr	_WinIsVisible
	beq.s	\No
		bsr	DrawClipLine
\No	lea	10(a7),a7
	rts

;void WinScrollV (WINDOW *w, const WIN_RECT *rect, short NumRows);
WinScrollV:
	bsr	_WinSetSystemVar
	move.w	CURRENT_ATTR,-(a7)
	move.w	14(a7),-(a7)			; NumRows
	pea	WINDOW.Clip(a0)			; Clip zone
	pea	ScrRect
	bsr	ScrRectScroll
	bsr	_WinIsVisible
	beq.s	\No
		bsr	ScrRectScroll
\No	lea	12(a7),a7
	rts

;void WinScrollH (WINDOW *w, const WIN_RECT *rect, short NumRows);
WinScrollH:
	bsr	_WinSetSystemVar
	move.w	CURRENT_ATTR,-(a7)
	move.w	14(a7),-(a7)			; NumRows
	pea	WINDOW.Clip(a0)			; Clip zone
	pea	ScrRect
	bsr	ScrRectShift
	bsr	_WinIsVisible
	beq.s	\No
		bsr	ScrRectShift
\No	lea	12(a7),a7
	rts

;unsigned short WinBitmapSize (WINDOW *w, const WIN_RECT *rect); 
WinBitmapSize:
	bsr	_WinSetSystemVar
	subq.l	#4,a7
	pea	(a7)		; Result
	move.l	16(a7),-(a7)	; Rect
	pea	WINDOW.Clip(a0)	; Clip
	bsr	RectWinToScr		; TODODODODODODODOODODOD
	addq.l	#8,a7	
	move.l	a0,d0
	beq.s	\Quit
		bsr	BitmapSize
\Quit:	addq.l	#8,a7
	bra	PortRestore

;unsigned short WinBitmapGet(WINDOW *w, const WIN_RECT *rect, void *Bitmap); 
WinBitmapGet:
	bsr	_WinSetSystemVar
	subq.l	#4,a7
	move.l	16(a7),-(a7)	; Bitmap
	pea	4(a7)		; Result
	move.l	20(a7),-(a7)	; Rect
	pea	WINDOW.Clip(a0)	; Clip
	bsr	RectWinToScr
	addq.l	#8,a7		; Pop Clip & Rect
	move.l	a0,d0
	beq.s	\Quit
		bsr	BitmapGet	; Bitmap & Rect are ok
\Quit:	addq.l	#8,a7
	bra	PortRestore

;void WinBitmapPut (WINDOW *w, short x, short y, void *BitMap, short Attr);
WinBitmapPut
	bsr	_WinSetSystemVar
	move.w	16(a7),-(a7)	; ATTR
	pea	WINDOW.Clip(a0)	; Clip
	move.l	18(a7),-(a7)	; Bitmap
	move.w	20(a7),-(a7)	; Y
	move.w	20(a7),-(a7)	; X
	bsr	BitmapPut
	bsr	_WinIsVisible
	beq.s	\No
		bsr	BitmapPut
\No	lea	14(a7),a7
	rts

;short WinDupStat (WINDOW *w, short Stat);
WinDupStat:
	moveq	#1,d0		; Always TRUE (there is always a Duplicate Screen !)
	rts
	
;void WinActivate (WINDOW *w); 
WinActivate:
	move.l	4(a7),a0		; Window
	ori.w	#WF_VISIBLE|WF_ACTIVE,WINDOW.Flags(a0)	; Set as visible

;void WinBackupToScr (WINDOW *w);
WinBackupToScr:
	bsr	PortRestore
	move.l	4(a7),a0		; Window
	clr.w	-(a7)			; ATTR = A_RESET
	pea	ScrRect(pc)		; Clip area
	pea	WINDOW.Window(a0)	; Window Zone
	bsr	ScrRectFill		; Clear zone
	lea	10(a7),a7
	move.l	4(a7),a0			; Window
	movem.l	d3/a2-a5,-(a7)
	move.l	WINDOW.Screen(a0),a1	; Screen Src
	clr.w	d0
	move.b	WINDOW.Window+0(a0),d0	; X
	clr.w	d2
	move.b	WINDOW.Window+1(a0),d2	; Y
	mulu.w	CURRENT_INCY,d2
	move.w	CURRENT_INCY,a5
	lsr.w	#3,d0
	add.w	d0,d2			; Offset
	move.l	CURRENT_SCREEN,a2	; Dest
	adda.w	d2,a2			
	adda.w	d2,a1			; Src
	clr.w	d1
	clr.w	d0
	clr.w	d2
	move.b	WINDOW.Window+3(a0),d1
	sub.b	WINDOW.Window+1(a0),d1	; Height -1
	move.b	WINDOW.Window+2(a0),d0
	lsr.w	#3,d0
	move.b	WINDOW.Window+0(a0),d2
	lsr.w	#3,d2
	sub.w	d2,d0			; (/8) Width -1	
\LoopY		move.w	d0,d2
		move.l	a1,a3
		move.l	a2,a4
\LoopX			move.b	(a3)+,d3	; Src
			or.b	d3,(a4)+	; Dest
			dbf	d2,\LoopX
		adda.w	a5,a1
		adda.w	a5,a2
		dbf	d1,\LoopY
	movem.l	(a7)+,d3/a2-a5
	rts
	
;void WinEnd (WINDOW *w);
WinEnd:			; Nothing, ok
	rts
	
;void WinHide (WINDOW *w); 
WinHide:
	move.l	4(a7),a0
	andi.w	#~WF_VISIBLE,WINDOW.Flags(a0)	; Set as Hidden
	rts
	
;void WinDeactivate (WINDOW *w); 
WinDeactivate:
	move.l	4(a7),a0
	bsr	PortRestore
	movem.l	d3-d5,-(a7)
	clr.w	d0
	clr.w	d1
	clr.w	d4
	clr.w	d5
	move.b	WINDOW.Window+0(a0),d0
	move.b	WINDOW.Window+1(a0),d1
	move.b	WINDOW.Window+2(a0),d4
	move.b	WINDOW.Window+3(a0),d5
	sub.w	d0,d4
	sub.w	d1,d5
	moveq	#0,d3			; A_REVERSE
	bsr	frame
	movem.l	(a7)+,d3-d5

;void WinBegin (WINDOW *w);
WinBegin:
	move.l	4(a7),a0
	andi.w	#~WF_ACTIVE,WINDOW.Flags(a0)	; Set as invisible
	rts
	
;void WinClose (WINDOW *w)
WinClose:
	bsr	PortRestore
	move.l	4(a7),a0
	; Remove it from the linked lists
	move.l	FirstWindow,a1
	move.l	#FirstWindow,d2
\loop		cmp.l	a1,a0
		beq.s	\Found
		lea	WINDOW.Next(a1),a1
		move.l	a1,d2
		move.l	(a1),a1
		move.l	a1,d0
		bne.s	\loop	
	bra.s	\Done			; Can not found it : quit immediately
\Found:	move.l	d2,a1			; Where to fix
	move.l	WINDOW.Next(a0),(a1)	; Fix it
	; Free duplicate screen
	move.l	WINDOW.Screen(a0),-(a7)
	bsr	HeapFreePtr
	addq.l	#4,a7
	; Test if we have saved the screen
	move.l	4(a7),a0
	move.w	WINDOW.Flags(a0),d0
	andi.w	#WF_SAVE_SCR,d0
	beq.s	\NoDup
		move.w	d4,-(a7)
		move.w	WINDOW.DupScr(a0),d4
		bsr	RestoreScreen
		move.w	(a7)+,d4
		bra.s	\Done
\NoDup:	; Redraw the linked list of the lists
	move.l	FirstWindow,a0
\loop2		move.l	a0,d0
		beq.s	\Done
		move.w	WINDOW.Flags(a0),d0
		andi.w	#WF_VISIBLE,d0
		beq.s	\NoRedraw
			pea	(a0)
			bsr	WinActivate
			move.l	(a7)+,a0
\NoRedraw	move.l	WINDOW.Next(a0),a0
		bra.s	\loop2
\Done:	rts

;short WinOpen (WINDOW *w, const WIN_RECT *rect, unsigned short Flags, ...);
WinOpen:
	lea	ScrRect(pc),a0
	bsr	SetCurClip_reg		; Set Full clipping
	move.l	4(a7),a0		; WINDOW
	move.l	8(a7),a1		; Rect
	; Set flags
	move.w	12(a7),WINDOW.Flags(a0)	
	ori.w	#WF_DUP_SCR|WF_VISIBLE,WINDOW.Flags(a0)
	andi.w	#~WF_ACTIVE,WINDOW.Flags(a0) ; Not visible until WinActivate
	move.b	#1,WINDOW.CurAttr(a0)
	move.b	#USED_FONT,WINDOW.CurFont(a0)
	move.w	EV_RunningAppId,WINDOW.TaskId(a0)
	bsr	WinHome_reg
	clr.w	d1
	move.w	WINDOW.Flags(a0),d0
	andi.w	#WF_BLACK,d0
	beq.s	\NoBl
		moveq	#1,d1
\NoBl	move.w	d1,WINDOW.Background(a0)
	movem.l	d3-d7/a2-a6,-(a7)
	move.l	a1,a5
	move.l	a0,a2
	move.w	(a1)+,d0
	move.w	(a1)+,d1
	move.w	(a1)+,d2
	move.w	(a1)+,d3
	; Save the screen ?
	clr.w	WINDOW.DupScr(a2)
	move.w	WINDOW.Flags(a2),d4
	andi.w	#WF_SAVE_SCR,d4
	beq.s	\NoSave
		sub.w	d0,d2
		sub.w	d1,d3
		bsr	SaveScreen
		move.w	-(a1),d3
		move.w	-(a1),d2
		move.w	d4,WINDOW.DupScr(a2)
		bne.s	\NoSave
			andi.w	#~WF_SAVE_SCR,WINDOW.Flags(a2)
\NoSave:
	; Calculate the Window Region
	move.b	d0,WINDOW.Window+0(a2)
	move.b	d1,WINDOW.Window+1(a2)
	move.b	d2,WINDOW.Window+2(a2)
	move.b	d3,WINDOW.Window+3(a2)
	; Calculate the Client Region
	move.l	WINDOW.Window(a2),WINDOW.Client(a2)
	move.w	WINDOW.Flags(a2),d4
	andi.w	#WF_NOBORDER,d4
	bne.s	\NoBorder
		moveq	#1,d5			; Small Box
		move.w	WINDOW.Flags(a2),d4
		andi.w	#WF_ROUNDEDBORDER,d4
		beq.s	\StdBorder
			moveq	#4,d5		; Big Box
\StdBorder:	add.b	d5,WINDOW.Client+0(a2)
		add.b	d5,WINDOW.Client+1(a2)
		sub.b	d5,WINDOW.Client+2(a2)
		sub.b	d5,WINDOW.Client+3(a2)
		move.w	WINDOW.Flags(a2),d4
		andi.w	#WF_TITLE,d4
		beq.s	\NoBorder
			addq.b	#8,WINDOW.Client+1(a2)		
\NoBorder
	; Calculate the Clip Region
	move.l	WINDOW.Client(a2),WINDOW.Clip(a2)
	; Duplicate Screen
	pea	(3840).w
	bsr	HeapAllocPtr
	addq.l	#4,a7
	move.l	a0,WINDOW.Screen(a2)
	bne.s	\Ok
		; Free the duplicate
		move.w	WINDOW.DupScr(a2),d0
		beq.s	\NoFreeDup
			bsr	HeapFree_reg	
\NoFreeDup	moveq	#0,d0
		bra.s	\End
\Ok	move.l	a0,CURRENT_SCREEN
	bsr	ScreenClear
	; Draw the box
	move.w	(a5)+,d0
	move.w	(a5)+,d1
	move.w	(a5)+,d2
	move.w	(a5)+,d3
	sub.w	d0,d2
	sub.w	d1,d3
	suba.l	a0,a0
	move.w	WINDOW.Flags(a2),d4
	move.w	d4,d5
	andi.w	#WF_NOBORDER,d5
	bne.s	\NoBorder2
		move.w	d4,d5
		andi.w	#WF_TITLE,d5
		beq.s	\NoTitle
			move.l	14+40(a7),a0	; Title
\NoTitle	lea	box(pc),a1		; Big Box
		andi.w	#WF_ROUNDEDBORDER,d4
		bne.s	\BigBox
			lea	box2(pc),a1	; Small Box
\BigBox		jsr	(a1)			; Draw it
\NoBorder2:
	; Add it in the list
	move.l	FirstWindow,WINDOW.Next(a2)
	move.l	a2,FirstWindow
	; Restore the normal screen
	bsr	PortRestore
	moveq	#1,d0
\End	movem.l	(a7)+,d3-d7/a2-a6
	rts

;void DrawStaticButton (WINDOW *w, short ButtonType, short x);
DrawStaticButton:
	bsr	_WinSetSystemVar
	move.w	10(a7),d0
	clr.w	d1
	move.b	WINDOW.Client+0(a0),d1
	add.w	d1,d0		; X
	move.b	WINDOW.Client+3(a0),d1
	sub.w	#14,d1		; Y
	move.w	8(a7),d2
	cmpi.w	#6,d2
	bhi.s	\Quit
	add.w	d2,d2
	move.w	DrawStaticButtonTable(pc,d2.w),d2
	lea	DrawStaticButtonTable(pc,d2.w),a0
	move.l	a0,a1
	moveq	#45,d2		; Width
	move.l	d3,-(a7)	; Push d3
	moveq	#12,d3		; Height
	bsr	box
	bsr	_WinIsVisible
	beq.s	\No
		move.l	a1,a0
		bsr	box
\No	move.l	(a7)+,d3
\Quit	rts

DrawStaticButtonTable:
	dc.w	BT_NONE_str-DrawStaticButtonTable
	dc.w	BT_OK_str-DrawStaticButtonTable
	dc.w	BT_SAVE_str-DrawStaticButtonTable
	dc.w	BT_YES_str-DrawStaticButtonTable
	dc.w	BT_CANCEL_str-DrawStaticButtonTable
	dc.w	BT_NO_str-DrawStaticButtonTable
	dc.w	BT_GOTO_str-DrawStaticButtonTable

;void WinDrawButton(WINDOW *w, short x, short y, char *str, short width, short height);
WinDrawButton:
	bsr	_WinSetSystemVar
	move.l	d3,-(a7)
	move.w	4+8(a7),d0		; X
	move.w	4+10(a7),d1		; Y
	add.b	WINDOW.Client+0(a0),d0
	add.b	WINDOW.Client+1(a0),d1
	move.l	4+12(a7),a0		; Str
	move.w	4+16(a7),d2		; Width
	move.w	4+18(a7),d3		; Height
	bsr	Button
	bsr	_WinIsVisible
	beq.s	\No
		bsr	Button	
\No:	move.l	(a7)+,d3
	rts
	
;void WinDrawIcon(WINDOW *w, short x, short y, ICON *data);
WinDrawIcon:
	bsr	_WinSetSystemVar
	move.l	d3,-(a7)
	move.w	4+8(a7),d0		; X
	move.w	4+10(a7),d1		; Y
	add.b	WINDOW.Client+0(a0),d0
	add.b	WINDOW.Client+1(a0),d1
	move.l	4+12(a7),a0		; Str
	moveq	#2,d2			; Attr = A_XOR
	bsr	DrawIcon_reg
	bsr	_WinIsVisible
	beq.s	\No
		bsr	DrawIcon_reg
\No:	move.l	(a7)+,d3
	rts

;void WinStr (WINDOW *w, const char *str);
WinStr:
	bsr	_WinSetSystemVar
	move.l	8(a7),a1
	bra.s	WinStrXY_entry

;void WinStrXY(WINDOW *w, short x, short y, const char *str); 
WinStrXY:
	bsr	_WinSetSystemVar
	move.w	8(a7),WINDOW.CursorX(a0)
	move.w	10(a7),WINDOW.CursorY(a0)
	move.l	12(a7),a1

WinStrXY_entry:
	move.b	(a1)+,d2
	beq.s	\End
		pea	(a1)
		move.b	d2,-(a7)
		pea	(a0)
		bsr.s	WinChar
		move.l	(a7)+,a0
		addq.w	#2,a7
		move.l	(a7)+,a1
		bra.s	WinStrXY_entry
\End:	rts

;void WinCharXY(WINDOW *w, short x, short y, char c, short Count);
WinCharXY:
	bsr	_WinSetSystemVar
	move.w	8(a7),WINDOW.CursorX(a0)
	move.w	10(a7),WINDOW.CursorY(a0)
	move.b	12(a7),d2	; c
	move.w	14(a7),d0	; count
	move.w	d3,-(a7)
	move.w	d0,d3
	move.b	d2,-(a7)	; Push Char
	pea	(a0)		; Push window
	subq.w	#1,d3
	blt.s	\end
\loop		bsr	WinChar
		dbf	d3,\loop
\end	addq.l	#6,a7
	move.w	(a7)+,d3
	rts

;void WinChar (WINDOW *w, char c); 
WinChar:
	bsr	_WinSetSystemVar
	add.w	WINDOW.CursorX(a0),d0
	add.w	WINDOW.CursorY(a0),d1
	movem.l	d3-d5/d7/a2,-(sp)
	move.w	d0,d3		; X
	move.w	d1,d4		; Y
	move.l	a0,a2
	clr.w	d5
	move.b	28(sp),d5	; Char c
	move.w	d5,-(sp)
	bsr	FontCharWidth
	addq.l	#2,sp
	move.w	d0,d7		; d7 = FontCharWidth
	move.w	WINDOW.Flags(a2),d2
	andi.w	#WF_TTY,d2	; If not TTY mode, draw directly the char.
	beq	\DrawChar
	cmp.w	#10,d5
	beq.s	\NewLine
	cmp.w	#13,d5
	beq.s	\NewLine
	cmp.w	#8,d5
	bne.s	\NoClearScreen
		move.w	WINDOW.Background(a2),-(sp)	; ATTR
		pea	WINDOW.Clip(a2)			; Clip
		pea	WINDOW.Client(a2)		; CLient
		bsr	ScrRectFill			; Clear the client area in the duplicate screen
		bsr	_WinIsVisible
		beq.s	\No
			bsr	ScrRectFill		; Clear the client area	in the LCD MEM
\No:		lea	10(sp),sp
		move.l	a2,a0
		bsr	WinHome_reg
		bra	\Quit
\NoClearScreen:
	move.w	d3,d1
	add.w	d7,d1
	clr.w	d0
	move.b	WINDOW.Client+2(a2),d0	; Xmax
	addq.w	#1,d0
	cmp.w	d1,d0			; (X+Len < Xmax+1) ?
	bge.s	\Print
\NewLine:	clr.w	d3
		move.b	WINDOW.CurFont(a2),d3	; Font
		add.w	d3,d3
		addq.w	#6,d3			; d3 = Height
		add.w	d3,d4			; Y += Height
		move.w	d4,d1
		add.w	d3,d1			; Y + Height ?
		clr.w	d0
		move.b	WINDOW.Client+3(a2),d0
		cmp.w	d1,d0
		bge	\NoScroll
			move.w	WINDOW.Background(a2),-(sp)
			move.w	d3,-(sp)
			pea	WINDOW.Clip(a2)
			pea	WINDOW.Client(a2)
			bsr	ScrRectScroll		; Scroll the duplicate
			bsr	_WinIsVisible
			beq.s	\NoVisible2
				bsr	ScrRectScroll	; Scroll the LCD_MEM			
\NoVisible2		lea	12(sp),sp
			move.l	WINDOW.Screen(a2),CURRENT_SCREEN	; Restore back buffer
			sub.w	d3,d4
\NoScroll
		clr.w	d3			; X = 0
		move.b	WINDOW.Client+0(a2),d3	; X = WINDOW.Client.x0
\Print:
	cmp.w	#10,d5
	beq.s	\End
	cmp.w	#13,d5
	beq.s	\End
\DrawChar
		move.w	CURRENT_ATTR,-(sp)	; ATTR
		pea	WINDOW.Clip(a2)
		move.w	d5,-(sp)		; Char
		move.w	d4,-(sp)		; Y
		move.w	d3,-(sp)		; X
		bsr	DrawClipChar
		bsr	_WinIsVisible
		beq.s	\No3
			bsr	DrawClipChar
\No3		lea	12(a7),a7
		add.w	d7,d3			; X += Len
\End:
	clr.w	d0
	move.b	WINDOW.Client+0(a2),d0
	sub.w	d0,d3
	move.w	d3,WINDOW.CursorX(a2)
	move.b	WINDOW.Client+1(a2),d0
	sub.w	d0,d4
	move.w	d4,WINDOW.CursorY(a2)
\Quit	movem.l (sp)+,d3-d5/d7/a2
	rts
	
;short WinReOpen (WINDOW *w, const WIN_RECT *rect, unsigned short Flags, ...);
WinReOpen:
	; First check if the Window is in the Window List
	move.l	4(a7),a0		; Window
	move.l	FirstWindow,a1
\SearchLoop	move.l	a1,d0
		beq	WinOpen			; Window not found, call WinOpen and quit.
		cmp.l	a1,a0			; Found Window ?
		beq.s	\Found
		move.l	WINDOW.Next(a1),a1	; Next Window
		bra.s	\SearchLoop
\Found	bsr	PortRestore
	; Check SaveScreen ? Yes, restore screen
	movem.l	d0-d7/a0-a6,-(a7)
	move.w	WINDOW.DupScr(a0),d4
	beq.s	\Clear ; Clear it
		bsr	RedrawBackScreen
		bra.s	\Skip
\Clear:	clr.w	-(a7)
	pea	WINDOW.Window(a0)
	pea	WINDOW.Window(a0)
	bsr	ScrRectFill
	lea	10(a7),a7
\Skip	movem.l	(a7)+,d0-d7/a0-a6
	; Same Size ? 
	move.l	8(a7),a1	; rect
	move.w	4(a1),d0
	sub.w	(a1),d0
	move.b	WINDOW.Window+2(a0),d1
	sub.b	WINDOW.Window+0(a0),d1
	cmp.b	d0,d1
	bne	\ReDraw
	move.w	6(a1),d0
	sub.w	2(a1),d0
	move.b	WINDOW.Window+3(a0),d1
	sub.b	WINDOW.Window+1(a0),d1
	cmp.b	d0,d1
	bne	\ReDraw
\Move:		; Move the Duplicate Screen and quit.
		move.l	WINDOW.Window(a0),WINDOW.Clip(a0)	; Save Window
		move.l	WINDOW.Screen(a0),CURRENT_SCREEN		
		move.w	2(a1),d0			; New Y
		clr.w	d1
		move.b	WINDOW.Window+1(a0),d1		; Old Y
		sub.w	d1,d0
		beq.s	\NoScroll
			add.b	d0,WINDOW.Client+1(a0)
			add.b	d0,WINDOW.Client+3(a0)
			add.b	d0,WINDOW.Clip+1(a0)
			add.b	d0,WINDOW.Clip+3(a0)
			clr.w	-(a7)
			move.w	d0,-(a7)
			pea	FullRect(pc)
			pea	WINDOW.Window(a0)
			bsr	ScrRectScroll
			lea	12(a7),a7
\NoScroll	move.l	4(a7),a0			; Window
		move.l	8(a7),a1			; Rect
		move.w	(a1),d0				; New X
		clr.w	d1
		move.b	WINDOW.Window+0(a0),d1		; Old X
		sub.w	d1,d0
		beq.s	\NoShift
			add.b	d0,WINDOW.Client+0(a0)
			add.b	d0,WINDOW.Client+2(a0)
			add.b	d0,WINDOW.Clip+0(a0)
			add.b	d0,WINDOW.Clip+2(a0)
			neg.w	d0
			clr.w	-(a7)
			move.w	d0,-(a7)
			pea	FullRect(pc)
			pea	WINDOW.Window(a0)
			bsr	ScrRectShift
			lea	12(a7),a7
\NoShift	move.l	4(a7),a0
		move.l	WINDOW.Clip(a0),WINDOW.Window(a0)
		move.l	WINDOW.Client(a0),WINDOW.Clip(a0)
		bra	PortRestore
\ReDraw	; Different Size: Clear the Duplicate Screen, ReDraw the Window
	; ie WinClose then Jump to WinOpen
	pea	(a0)
	bsr	WinClose
	addq.l	#4,a7
	bra	WinOpen

;void DrawWinBorder (WINDOW *w, SCR_RECT *rect);
DrawWinBorder:
	movem.l	d3-d7/a2-a6,-(a7)
	clr.w	d0
	clr.w	d1
	clr.w	d2
	clr.w	d3
	move.l	48(a7),a0
	move.b	(a0)+,d0
	move.b	(a0)+,d1
	move.b	(a0)+,d2
	move.b	(a0)+,d3
	sub.w	d0,d2
	sub.w	d1,d3
	lea	ST_none_str(pc),a0
	bsr	box
	movem.l	(a7)+,d3-d7/a2-a6
	rts
	