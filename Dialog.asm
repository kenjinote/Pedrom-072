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
; 			Dialog functions
; ***************************************************************

; ***************************************************************
; 			Internal functions
; ***************************************************************

; An Item is an entry in the dialog struct (very universal since it works for Menu, ToolBar, Popup & Dialog struct !)
ITEM.X			EQU	0		; X position
ITEM.Y			EQU	2		; Y position
ITEM.Width		EQU	4		; Width
ITEM.Height		EQU	6		; Height
ITEM.FastKey		EQU	8		; Key (Ex: KEY_F1)
ITEM.Id			EQU	10		; Id = What it is return if we select this
ITEM.Attr		EQU	12		; Attr for the Item
ITEM.Font		EQU	13		; Font for the Item
ITEM.Data		EQU	14		; Data (= String / BITMAP / ICON / ...)
ITEM.ItemUp		EQU	18		; Nr of the Item which is up
ITEM.ItemDn		EQU	19		; " Down
ITEM.ItemLf		EQU	20		; " Left
ITEM.ItemRg		EQU	21		; " right
ITEM.SelectCB		EQU	22		; CallBack : short Select(WINDOW *w, short x, short y, ITEM *i); OR short Select(WINDOW *w asm("a3"), short x asm("d0"), short y asm("d1"), ITEM *i asm("a4")); return 0 if we process Key function, else it returns the key.
ITEM.UnSelectCB		EQU	26		; CallBack : void UnSelect(WINDOW *w, short x, short y, ITEM *i); OR void UnSelect(WINDOW *w asm("a3"), short x asm("d0"), short y asm("d1"), ITEM *i asm("a4"));
ITEM.DrawCB		EQU	30		; CallBack : void Draw(WINDOW *w, short x, short y, void *data, short width, short height); (Ex: WinDrawXY -the last parameters are skipped)
ITEM.DoKeyCB		EQU	34		; Callback : short DoKey(short key, ITEM *i); OR short DoKey(short key asm("d3"), ITEM *i("a4"));
ITEM.SubDialog		EQU	38		; SubDialog if we select it (Ptr or Hd)
ITEM.sizeof		EQU	42

; The special return value of DoKey are :
DOKEY.None		EQU	-1	; Nothing
DOKEY.Redraw		EQU	-2	; Redraw
DOKEY.ItemUp		EQU	-3	; Next item (Up )
DOKEY.ItemDn		EQU	-4	; Next item (Down )
DOKEY.ItemLf		EQU	-5	; Next item (Left )
DOKEY.ItemRg		EQU	-6	; Next item (Right )
DOKEY.Default		EQU	-7	; Default 

; Dialog structure
; Note: the window is allocated by the dialog function !
DIALOG.WinFlag		EQU	0	; Windows Flag
DIALOG.Title		EQU	2	; Char title ptr (if no title, then NULL )
DIALOG.Select		EQU	6	; Current selection
DIALOG.NbrItem		EQU	8	; Number of items !
DIALOG.Width		EQU	10	; Width of the Window
DIALOG.Height		EQU	12	; Height of the Window
DIALOG.DefaultKeyCB	EQU	14	; CallBack : long DefaultKey(short key asm("d3"), ITEM *i asm("a4")); return 0 if we continue the dialog.
DIALOG.ItemsTab		EQU	18

; Scroll
; In:
;	d2.w = 0 (Or -1 if the first time)
;	a2 -> Dialog
;	a3 -> Window
;	d7 = Select
;	d5 = ScrollX
;	d6 = ScrollY
; Destroy:
;	d0-d4/a0-a1/a4
DIALOG.Scroll:
	; Get Width / Height (It isn't DIALOG.Width /.Height due to border)
	clr.w	d4
	move.b	WINDOW.Client+3(a3),d4
	sub.b	WINDOW.Client+1(a3),d4
	clr.w	d3
	move.b	WINDOW.Client+2(a3),d3
	sub.b	WINDOW.Client+0(a3),d3
	; Update the (ScrollX, ScrollY) position
	move.w	d7,d0			; Current selection
	mulu.w	#ITEM.sizeof,d0
	lea	DIALOG.ItemsTab(a2,d0.w),a0	; Select Item
	move.w	ITEM.X(a0),d0
	cmp.w	d5,d0
	bge.s	\Ok1
		move.w	d0,d5
		st.b	d2
\Ok1	move.w	ITEM.Y(a0),d1
	cmp.w	d6,d1
	bge.s	\Ok2
		move.w	d1,d6
		st.b	d2
\Ok2	add.w	ITEM.Width(a0),d0
	sub.w	d3,d0
	cmp.w	d5,d0
	ble.s	\Ok3
		move.w	d0,d5
		st.b	d2
\Ok3	add.w	ITEM.Height(a0),d1
	sub.w	d4,d1
	cmp.w	d6,d1
	ble.s	\Ok4
		move.w	d1,d6
		st.b	d2
\Ok4	
	; Check if we have to redraw the dialog due to scrolling ?
	tst.b	d2
	bne.s	\Redraw
		rts		; No => Return
\Redraw	; Redraw all the dialog
	move.w	d7,-(a7)
	pea	(a3)
	bsr	WinBegin		; Work in the duplicate screen
	bsr	WinClr			; Clear the window
	lea	DIALOG.ItemsTab(a2),a4
	clr.w	d3
	move.b	WINDOW.Client+2(a3),d3
	sub.b	WINDOW.Client+0(a3),d3	; Width
	clr.w	d4
	move.b	WINDOW.Client+3(a3),d4
	sub.b	WINDOW.Client+1(a3),d4	; Height
	move.w	DIALOG.NbrItem(a2),d7
	subq.w	#1,d7
\ItemLoop	; Check intersection of this item and the client window
		move.w	ITEM.X(a4),d0
		sub.w	d5,d0			; X - ScrollX
		move.w	ITEM.Y(a4),d1
		sub.w	d6,d1			; Y - ScrollY
		cmp.w	d3,d0			; Check Clipping
		bgt.s	\Next
		cmp.w	d4,d1
		bgt.s	\Next			; Check Clipping
			move.w	ITEM.Width(a4),d2	; Xmin - Width
			add.w	d0,d2
			blt.s	\Next
			move.w	ITEM.Height(a4),d2
			add.w	d1,d2
			blt.s	\Next
				; Set Font & Attr
				move.b	ITEM.Font(a4),WINDOW.CurFont(a3)
				move.b	ITEM.Attr(a4),WINDOW.CurAttr(a3)
				; Call the Draw function
				move.w	ITEM.Height(a4),-(a7)	; Height
				move.w	ITEM.Width(a4),-(a7)	; Width
				move.l	ITEM.Data(a4),-(a7)	; Data
				move.w	d1,-(a7)		; Y
				move.w	d0,-(a7)		; X
				pea	(a3)			; Window
				move.l	ITEM.DrawCB(a4),a0
				jsr	(a0)			; Draw(Window, x, y, data, width, height)
				lea	16(a7),a7
\Next		lea	ITEM.sizeof(a4),a4
		dbf	d7,\ItemLoop		; Next Item
	bsr	WinActivate		;Display the window
	addq.l	#4,a7
	move.w	(a7)+,d7
	rts
	
; struct dummy DIALOG.Do(DIALOG *d, short x, short y);
; where struct dummy { short key; short Id;};
DIALOG.Do:
	move.l	4(a7),a0		; Dialog *
	move.w	8(a7),d0		; x
	move.w	10(a7),d1		; y
	movem.l	d3-d7/a2-a6,-(a7)
	move.l	a7,a6				; Save Stack Ptr
	move.l	a0,a2				; Dialog Ptr is a2
	lea	-68(a7),a7			; Space for Windows
	move.l	a7,a3				; Window Ptr
	tst.w	DIALOG.NbrItem(a2)		; Check if there are at least 1 item.
	beq	\Failed
	; Check the coordinate of the window.
	; (Calculate size min ?)
	; Check the flag of the window
	move.w	DIALOG.WinFlag(a2),d3
	move.l	DIALOG.Title(a2),-(a7)
	beq.s	\No1
		ori.w	#WF_TITLE,d3	; Set 
\No1	ori.w	#WF_DUP_SCR|WF_SAVE_SCR,d3
	; Open the window
	move.w	d3,-(a7)		; Flags
	; Create WIN_RECT : Check X range of the window
	tst.w	d0
	bge.s	\Okx0
		clr.w	d0
\Okx0	move.w	d0,60(a3)
	add.w	DIALOG.Width(a2),d0
	cmpi.w	#239,d0
	bls.s	\Okx1
		sub.w	#239,d0
		sub.w	d0,60(a3)
		move.w	#239,d0
\Okx1	move.w	d0,64(a3)
	; Check Y range of the window
	tst.w	d1
	bge.s	\Oky0
		clr.w	d1
\Oky0	move.w	d1,62(a3)
	add.w	DIALOG.Height(a2),d1
	cmpi.w	#127,d1
	bls.s	\Oky1
		sub.w	#127,d1
		sub.w	d1,62(a3)
		move.w	#127,d1
\Oky1	move.w	d1,66(a3)
	pea	60(a3)			; Push WIN_RECT
	pea	(a3)			; Windows
	bsr	WinOpen
	lea	14(a7),a7
	tst.w	d0
	beq	\Failed
	; Registers Alloc
	;	a2 -> Dialog
	;	a3 -> Window
	;	d7 = Select
	;	d5 = ScrollX
	;	d6 = ScrollY
	;	d3.w = Current Key
	;	d4.l = Code to return / Special code
	move.w	DIALOG.Select(a2),d7
	; Start with ScrollX
\ReDraw	clr.w	d5			; Falsify theses values
	clr.w	d6			; "
	st.b	d2
	bra.s	\DoRedraw
	; Start Main Loop
\MainLoop
		clr.w	d2			; Do not redraw the window
		; Check Select
\DoRedraw	ext.w	d7
		move.w	DIALOG.NbrItem(a2),d0
		tst.w	d7
		bge.s	\Ok
			move.w	d0,d7
			subq.w	#1,d7
\Ok		cmp.w	d0,d7
		blt.s	\Ok2
			clr.w	d7
\Ok2		bsr	DIALOG.Scroll		; Scroll & display
		; Ptr to select item
		move.w	d7,d0			; Current selection
		mulu.w	#ITEM.sizeof,d0
		lea	DIALOG.ItemsTab(a2,d0.w),a4	; Select Item
		; Call Select Function long Select(WINDOW *w, short x, short y, ITEM *i);
		pea	(a4)			; *Item
		move.w	ITEM.Y(a4),d1
		sub.w	d6,d1
		move.w	d1,-(a7)		; Y
		move.w	ITEM.X(a4),d0
		sub.w	d5,d0
		move.w	d0,-(a7)		; X
		pea	(a3)			; *Window
		move.l	ITEM.SelectCB(a4),a0
		jsr	(a0)			; Select It
		lea	12(a7),a7
		tst.l	d0			; If it returns a non null value, translate it
		beq.s	\GetKey			; From Key / Id
			move.l	d0,d3		; d3.w = Key
			move.l	d0,d4		; Returned code
			swap	d3		; d3.w = Returned Key d3.uw = Id
			tst.w	d0		; If Id >=0 
			bgt	\Success	; Then returned it
			bra.s	\KeyOk
\GetKey:	bsr	GetKey			; Get a Key
		move.l	d0,d3
\KeyOk:		;void UnSelect(WINDOW *w, short x, short y, ITEM *i);
		pea	(a4)			; *Item
		move.w	ITEM.Y(a4),d1
		sub.w	d6,d1
		move.w	d1,-(a7)		; Y
		move.w	ITEM.X(a4),d0
		sub.w	d5,d0
		move.w	d0,-(a7)		; X
		pea	(a3)			; *Window
		move.l	ITEM.UnSelectCB(a4),a0
		jsr	(a0)			; UnSelect It
		lea	12(a7),a7
		; short DoKey(short key, ITEM *i);
		pea	(a4)			; *Item
		move.w	d3,-(a7)		; Key
		move.l	ITEM.DoKeyCB(a4),a0
		jsr	(a0)			; UnSelect It
		addq.l	#6,a7
		move.l	d0,d4			; Save the return
		bge	\Success
		addq.w	#1,d0
		beq	\MainLoop		; -1 : Do Nothing
		addq.w	#1,d0
		beq	\ReDraw
		addq.w	#1,d0
		beq.s	\ItemUp
		addq.w	#1,d0
		beq.s	\ItemDn
		addq.w	#1,d0
		beq.s	\ItemLf
		addq.w	#1,d0
		beq.s	\ItemRg		
		addq.w	#1,d0
		beq.s	\Default
		bra	\MainLoop
\ItemUp:	move.b	ITEM.ItemUp(a4),d7
		bra	\MainLoop
\ItemDn:	move.b	ITEM.ItemDn(a4),d7
		bra	\MainLoop
\ItemRg:	move.b	ITEM.ItemRg(a4),d7
		bra	\MainLoop
\ItemLf:	move.b	ITEM.ItemLf(a4),d7
		bra	\MainLoop
\Default:	; Search if it is a FastKey
		move.w	DIALOG.NbrItem(a2),d0
		lea	DIALOG.ItemsTab(a2),a0
		clr.w	d1
		subq.w	#1,d0
\FastKeyLoop		cmp.w	ITEM.FastKey(a0),d3
			beq.s	\DoFastKey
			addq.w	#1,d1
			lea	ITEM.sizeof(a0),a0
			dbf	d0,\FastKeyLoop
		pea	(a4)
		move.w	d3,-(a7)			; It isn't a Fast Key
		move.l	DIALOG.DefaultKeyCB(a2),a0	; Process the Key to the default dialog processing
		move.l	a0,d0				; NULL ?
		beq.s	\Nothing			; Yes, skip it
			jsr	(a0)			; Call the sub routine
			move.l	d0,d4			; Quit ?
			bge.s	\Success		; Yes !
\Nothing	addq.l	#6,a7				; Pop the stack
		bra	\MainLoop			; 
\DoFastKey	move.w	d1,d7
		bra	\MainLoop
\SelectIt	moveq	#0,d4
		move.w	ITEM.Id(a4),d4			; Select it
\Success	; Release the window
	pea	(a3)
	bsr	WinClose
	addq.l	#4,a7
	move.l	d4,d0
\Failed
	move.l	a6,a7					; Pop frame
	movem.l	(a7)+,d3-d7/a2-a6
	rts

; ******************************************************************************************
;
;				CALL BACK for Selecting an Item
;
; ******************************************************************************************

; long Select(WINDOW *w asm("a3"), short x asm("d0), short y asm("d1"), ITEM *i asm("a4"));
; void  UnSelect(WINDOW *w asm("a3"), short x asm("d0), short y asm("d1"), ITEM *i asm("a4"));
DIALOG.CB_INVERSE:
	subq.l	#8,a7				; Stack Frame
	move.l	a7,a0				; Ptr -> Frame
	move.w	d0,(a0)+			; Save X
	move.w	d1,(a0)+			; Save y
	add.w	ITEM.Width(a4),d0
	subq.w	#1,d0
	move.w	d0,(a0)+			; Add width to xmax
	add.w	ITEM.Height(a4),d1
	subq.w	#1,d1
	move.w	d1,(a0)+			; Add height to ymax
	move.w	#A_XOR,-(a7)			; ATTR
	pea	-8(a0)				; WIN_RECT
	pea	(a3)				; WINDOW
	bsr	WinFill
	lea	18(a7),a7
	moveq	#0,d0
	rts
	
; long Select(WINDOW *w asm("a3"), short x asm("d0), short y asm("d1"), ITEM *i asm("a4"));
; void  UnSelect(WINDOW *w asm("a3"), short x asm("d0), short y asm("d1"), ITEM *i asm("a4"));
DIALOG.CB_SELECT:
	subq.l	#8,a7				; Stack Frame
	move.l	a7,a0				; Ptr -> Frame
	subq.w	#1,d0
	subq.w	#1,d1
	move.w	d0,(a0)+			; Save X
	move.w	d1,(a0)+			; Save y
	add.w	ITEM.Width(a4),d0
	addq.w	#2,d0
	move.w	d0,(a0)+			; Add width to xmax
	add.w	ITEM.Height(a4),d1
	addq.w	#2,d1
	move.w	d1,(a0)+			; Add height to ymax
	move.w	#A_XOR+$80,-(a7)		; ATTR
	pea	-8(a0)				; WIN_RECT
	pea	(a3)				; WINDOW
	bsr	WinRect
	lea	18(a7),a7
	moveq	#0,d0
	rts

; short Select(WINDOW *w asm("a3"), short x asm("d0), short y asm("d1"), ITEM *i asm("a4"));
; void  UnSelect(WINDOW *w asm("a3"), short x asm("d0), short y asm("d1"), ITEM *i asm("a4"));
DIALOG.CB_RELIEF:
	movem.l	d0-d1,-(a7)
	lea	WINDOW.Clip(a3),a0
	bsr	SetCurClip_reg
	movem.l	(a7)+,d0-d1
	add.b	WINDOW.Client+0(a3),d0
	add.b	WINDOW.Client+1(a3),d1
	move.l	d3,-(a7)
	add.w	ITEM.Height(a4),d1
	move.w	d0,d2
	add.w	ITEM.Width(a4),d2
	moveq	#A_XOR,d3
	move.l	WINDOW.Screen(a3),CURRENT_SCREEN
	bsr	horiz
	bsr	_WinIsVisible
	beq.s	\No
		bsr	horiz	
\No	move.w	d2,d0			; X 
	move.w	d1,d2			; Ymax
	sub.w	ITEM.Height(a4),d1	; Ymin
	move.l	WINDOW.Screen(a3),CURRENT_SCREEN
	bsr	vert
	bsr	_WinIsVisible
	beq.s	\No2
		bsr	vert
\No2	move.l	(a7)+,d3
	moveq	#0,d0
	rts

; ******************************************************************************************
;
;				CALL BACK for Processing PopUp
;
; ******************************************************************************************

; Standard Item :
;	Can move up/down. Can select with enter, or cancel with ESC
;	Can enter a sub-menu, or return from it with ESC
;long DoKey(short key asm("d3"), ITEM *i("a4"));
DIALOG.CB_DOKEY_POPUP:
	move.w	d3,d0			; d0.uw = Key
	swap	d0			; 
	cmpi.w	#KEY_UP,d3
	bne.s	\NoUp
		moveq	#DOKEY.ItemUp,d0	; Next Item
		rts
\NoUp:	cmpi.w	#KEY_DOWN,d3
	bne.s	\NoDn
		moveq	#DOKEY.ItemDn,d0	; Next Item
		rts
\NoDn:	cmpi.w	#KEY_ENTER,d3
	bne.s	\NoEnter
		tst.b	ITEM.ItemLf(a4)
		beq.s	\NoEsc
		tst.w	ITEM.SubDialog(a4)
		beq.s	\ReturnId		; No so process default...
\Enter:		move.w	ITEM.Y(a4),d1			; Y
		sub.w	d6,d1				; - ScrollY
		add.b	WINDOW.Client+1(a3),d1		; Add Client Window Y
		move.w	d1,-(a7)			; Push Y
		move.w	ITEM.X(a4),d0			; Get X
		add.w	ITEM.Width(a4),d0		; + Width
		sub.w	d5,d0				; -ScrollX
		add.b	WINDOW.Client+0(a3),d0		; Add Window Client X
		move.w	d0,-(a7)			; Push X
		move.w	ITEM.SubDialog(a4),a0		; Get the Handle
		trap	#3				; Deref it
		pea	(a0)				; Push it
		bsr	DIALOG.Do			; Process it
		addq.l	#8,a7
		cmpi.w	#$7FFF,d0			; Continue the dialog ?
		bne.s	\Ret
			moveq	#DOKEY.Default,d0
\Ret		rts
\ReturnId	move.w	ITEM.Id(a4),d0		; End of selection
		rts
\NoEnter:
	cmpi.w	#KEY_RIGHT,d3
	bne.s	\NoRight
		tst.b	ITEM.ItemLf(a4)
		beq.s	\NoEsc
		tst.w	ITEM.SubDialog(a4)
		bne.s	\Enter
\NoRight:
	cmpi.w	#KEY_ESC,d3
	bne.s	\NoEsc
		move.w	#$7FFF,d0		; Return 
		rts
\NoEsc	moveq	#DOKEY.Default,d0		; Default
	rts
	
	
	
; ******************************************************************************************
;
;				CALL BACK for Processing Menu
;
; ******************************************************************************************

; short Select(WINDOW *w asm("a3"), short x asm("d0), short y asm("d1"), ITEM *i asm("a4"));
DIALOG.CB_SELECT_MENU:
	bsr	DIALOG.CB_RELIEF
	move.w	10(a7),d1			; Reget Y
	add.b	WINDOW.Client+1(a3),d1		; Add Client Window Y
	;add.w	#10,d1				; +10
	add.w	ITEM.Height(a4),d1
	addq.w	#2,d1
	move.w	d1,-(a7)			; Push Y
	move.w	10(a7),d0			; Get X
	add.b	WINDOW.Client+0(a3),d0		; Add Window Client X
	move.w	d0,-(a7)			; Push X
	move.w	ITEM.SubDialog(a4),a0		; Deref the Handle
	trap	#3		
	pea	(a0)				; Push addr
	tst.w	ITEM.SubDialog(a4)		; Check if Sub Menu
	beq.s	\Ret
		bsr	DIALOG.Do		; Process it
\Ret	addq.l	#8,a7				; It returns 0 if continue
	rts
	
;long DefaultKey(short key asm("d3"), ITEM *i asm("a4"));
DIALOG.CB_DEFKEY_MENU:
	; It is called if isn't a key processing by the system.
	move.w	d3,d0
	swap	d0
	clr.w	d0
	rts
	
;short DoKey(short key asm("d3"), ITEM *i("a4"));
DIALOG.CB_DOKEY_MENU:
	move.w	d3,d0			; d0.uw = Key
	swap	d0			; 
	cmpi.w	#KEY_LEFT,d3
	bne.s	\NoUp
		moveq	#DOKEY.ItemLf,d0	; Next Item
		rts
\NoUp:	cmpi.w	#KEY_RIGHT,d3
	bne.s	\NoDn
		moveq	#DOKEY.ItemRg,d0	; Next Item
		rts
\NoDn:	cmpi.w	#KEY_ENTER,d3
	bne.s	\NoEnter
		move.w	ITEM.Id(a4),d0
		rts
\NoEnter:
	cmpi.w	#KEY_ESC,d3
	bne.s	\NoEsc
		move.w	#$7FFF,d0		; Return & Quit
		rts
\NoEsc	moveq	#DOKEY.Default,d0		; Default
	rts

;short DoKey(short key asm("d3"), ITEM *i("a4"));
DIALOG.CB_DOKEY_SUBMENU:
	move.w	d3,d0			; d0.uw = Key
	swap	d0			; 
	cmpi.w	#KEY_UP,d3
	bne.s	\NoUp
		moveq	#DOKEY.ItemUp,d0	; Next Item
		rts
\NoUp:	cmpi.w	#KEY_DOWN,d3
	bne.s	\NoDn
		moveq	#DOKEY.ItemDn,d0	; Next Item
		rts
\NoDn:	cmpi.w	#KEY_ENTER,d3
	bne.s	\NoEnter
		tst.b	ITEM.ItemLf(a4)
		beq.s	\Defau
		tst.w	ITEM.SubDialog(a4)
		beq.s	\ReturnId		; No so process default...
		move.w	ITEM.Y(a4),d1			; Y
		sub.w	d6,d1				; - ScrollY
		add.b	WINDOW.Client+1(a3),d1		; Add Client Window Y
		move.w	d1,-(a7)			; Push Y
		move.w	ITEM.X(a4),d0			; Get X
		add.w	ITEM.Width(a4),d0		; + Width
		sub.w	d5,d0				; -ScrollX
		add.b	WINDOW.Client+0(a3),d0		; Add Window Client X
		move.w	d0,-(a7)			; Push X
		move.w	ITEM.SubDialog(a4),a0		; Get the Handle
		trap	#3				; Deref it
		pea	(a0)				; Push it
		bsr	DIALOG.Do			; Process it
		addq.l	#8,a7
		cmpi.w	#$7FFF,d0			; Continue the dialog ?
		bne.s	\Ret
			moveq	#DOKEY.Default,d0
\Ret		rts
\ReturnId	move.w	ITEM.Id(a4),d0			; End of selection
		rts					; Select this Item
\NoEnter:
	cmpi.w	#KEY_ESC,d3
	bne.s	\NoEsc
\Esc		move.w	#$7FFF,d0		; Return & quit the menu
		rts
\NoEsc
	cmpi.w	#KEY_LEFT,d3
	bne.s	\NoLeft
\SystemKey	clr.w	d0			; Return & continue the menu
		rts
\NoLeft	cmpi.w	#KEY_RIGHT,d3
	beq.s	\SystemKey
\Defau	moveq	#DOKEY.Default,d0		; Default
	rts

; ******************************************************************************************
;
;				Tios PopUp
;
; ******************************************************************************************


;HANDLE PopupNew (const char *Title, short Height);
PopupNew:
	pea	(DIALOG.ItemsTab).w
	bsr	HeapAlloc
	addq.l	#4,a7
	tst.w	d0
	beq.s	\Error
		move.w	d0,a0
		trap	#3
		move.w	#WF_SAVE_SCR,DIALOG.WinFlag(a0)
		move.l	4(a7),DIALOG.Title(a0)
		move.w	8(a7),DIALOG.Height(a0)
		bne.s	\ok
			move.w	#50,DIALOG.Height(a0)
\ok		move.w	#98,DIALOG.Width(a0)
\Error	rts

;HANDLE PopupAddText (HANDLE Handle, short ParentID, const char *Text, short ID);
PopupAddText:
	move.w	4(a7),d0		; Handle
	beq	\ret
		move.w	6(a7),d1	; Parent Id
		bgt	\SubMenu	; Sub-Menu
\AddIt			move.w	d0,a0	; Add it at the top main menu
			trap	#3
			move.w	DIALOG.NbrItem(a0),d2
			addq.w	#1,d2
			mulu.w	#ITEM.sizeof,d2
			add.w	#DIALOG.ItemsTab,d2
			move.l	d2,-(a7)
			move.w	d0,-(a7)
			bsr	HeapRealloc
			addq.l	#6,a7
			tst.w	d0
			beq	\ret
				move.w	d0,a0		; Rederef Handle
				trap	#3
				move.w	DIALOG.NbrItem(a0),d2
				move.w	d2,d0
				addq.w	#1,DIALOG.NbrItem(a0)
				mulu.w	#ITEM.sizeof,d0
				lea	DIALOG.ItemsTab(a0,d0.l),a1
				clr.w	ITEM.X(a1)
				move.w	d2,d1
				lsl.w	#3,d1
				move.w	d1,ITEM.Y(a1)
				move.w	#96,ITEM.Width(a1)
				move.w	#8,ITEM.Height(a1)
				move.w	d2,d1
				add.w	#'a',d1
				move.w	d1,ITEM.FastKey(a1)
				move.w	12(a7),d1
				bne.s	\Ok
					move.w	d2,d1
					addq.w	#1,d1
\Ok:				move.w	d1,ITEM.Id(a1)
				move.b	#A_NORMAL,ITEM.Attr(a1)
				move.b	#1,ITEM.Font(a1)
				move.l	8(a7),ITEM.Data(a1)
				move.w	d2,d1
				subq.w	#1,d1
				move.b	d1,ITEM.ItemUp(a1)
				addq.w	#2,d1
				move.b	d1,ITEM.ItemDn(a1)
				st.b	ITEM.ItemLf(a1)
				move.l	#DIALOG.CB_INVERSE,ITEM.SelectCB(a1)
				move.l	#DIALOG.CB_INVERSE,ITEM.UnSelectCB(a1)
				move.l	#WinStrXY,ITEM.DrawCB(a1)
				move.l	#DIALOG.CB_DOKEY_POPUP,ITEM.DoKeyCB(a1)
				clr.l	ITEM.SubDialog(a1)
				bra.s	\Done
\SubMenu:		; Found the parent	
			bsr	FoundItemFromId
			move.l	a1,d0
			beq.s	\ret
\Found:			move.w	ITEM.SubDialog(a1),d0
			bne	\AddIt
			; Alloc a New Sub Dialog
			pea	(DIALOG.ItemsTab).w
			bsr	HeapAlloc
			addq.l	#4,a7
			tst.w	d0
			beq.s	\ret
			move.w	d0,-(a7)		; Save created Handle
			move.w	2+4(a7),d0		; Handle
			move.w	2+6(a7),d1		; Parent Id
			bsr	FoundItemFromId		; Reget the Item
			move.w	(a7)+,d0
			move.w	d0,ITEM.SubDialog(a1)	; Sauve l'handle !
			move.w	d0,a0
			trap	#3
			move.w	#WF_SAVE_SCR,DIALOG.WinFlag(a0)
			move.w	#50,DIALOG.Height(a0)
			move.w	#98,DIALOG.Width(a0)
			bra	\AddIt
\Done	move.w	4(a7),d0
\ret	rts	

;const char *PopupText (HANDLE Handle, short ID);
PopupText:
	move.w	6(a7),d1
	move.w	4(a7),d0
	bsr.s	FoundItemFromId
	suba.l	a0,a0
	move.l	a1,d0
	beq.s	\Error
		move.l	ITEM.Data(a1),a0
\Error	rts	

; In:
;	d0.w = Handle
;	d1.w = Id
; Out:
;	a1 -> Item
FoundItemFromId:
	move.w	d0,a0
	trap	#3
	move.w	DIALOG.NbrItem(a0),d2
	beq.s	\Fail
	lea	DIALOG.ItemsTab(a0),a1
	subq.w	#1,d2
\LoopSearchParent
		cmp.w	ITEM.Id(a1),d1
		beq.s	\Found
		move.w	ITEM.SubDialog(a1),d0
		beq.s	\Next
			move.w	d2,-(a7)
			pea	(a1)
			bsr	FoundItemFromId
			move.l	(a7)+,a0
			move.w	(a7)+,d2
			move.l	a1,d0
			bne.s	\Found
			move.l	a0,a1
\Next		lea	ITEM.sizeof(a1),a1
		dbf	d2,\LoopSearchParent
\Fail	suba.l	a1,a1
\Found	rts
		
;HANDLE PopupClear (HANDLE Handle);
PopupClear:
	move.w	4(a7),a0
	trap	#3
	clr.w	DIALOG.NbrItem(a0)
	rts

;short PopupDo (HANDLE Handle, short x, short y, short StartID);
PopupDo:
	move.w	8(a7),d1
	cmpi.w	#$FFFF,d1
	bne.s	\No
		moveq	#(128-50)/2,d1
\No	move.w	d1,-(a7)
	move.w	8(a7),d0
	cmpi.w	#$FFFF,d0
	bne.s	\No2
		moveq	#(240-98)/2,d0
\No2	move.w	d0,-(a7)
	move.w	8(a7),a0
	trap	#3
	pea	(a0)
	bsr	DIALOG.Do
	addq.l	#8,a7
	cmpi.w	#$7FFF,d0
	bne.s	\ret
		moveq	#0,d0
\ret	rts

; ******************************************************************************************
;
;				Tios Menus
;
; ******************************************************************************************

;HANDLE MenuNew (short Flags, short Width, short Height); 
MenuNew:
	pea	(DIALOG.ItemsTab).w
	bsr	HeapAlloc
	addq.l	#4,a7
	tst.w	d0
	beq.s	\Error
		move.w	d0,a0
		trap	#3
		move.w	#WF_NOBORDER|WF_BLACK,DIALOG.WinFlag(a0)
		move.w	#18,DIALOG.Height(a0)
		move.w	#SCR_WIDTH-1,DIALOG.Width(a0)
		move.l	#DIALOG.CB_DEFKEY_MENU,DIALOG.DefaultKeyCB(a0)
\Error	rts

;HANDLE MenuAddText (HANDLE Handle, short ParentID, const char *Text, short ID, short Flags);
; We don't care about flags since we always check if we found the parent !
MenuAddText:
	move.w	4(a7),d0		; Handle
	beq	\ret
		move.w	6(a7),d1	; Parent Id
		bgt	\SubMenu	; Sub-Menu
			; Add it in the Top Level Menu
			move.w	d0,a0	; Add it at the top main menu
			trap	#3
			move.w	DIALOG.NbrItem(a0),d2
			addq.w	#1,d2
			mulu.w	#ITEM.sizeof,d2
			add.w	#DIALOG.ItemsTab,d2
			move.l	d2,-(a7)
			move.w	d0,-(a7)
			bsr	HeapRealloc
			addq.l	#6,a7
			tst.w	d0
			beq	\ret
				move.w	d0,a0		; Rederef Handle
				trap	#3
				move.w	DIALOG.NbrItem(a0),d2
				move.w	d2,d0
				addq.w	#1,DIALOG.NbrItem(a0)
				mulu.w	#ITEM.sizeof,d0
				lea	DIALOG.ItemsTab(a0,d0.l),a1
				move.w	#5,ITEM.Y(a1)
				move.w	#8,ITEM.Height(a1)
				move.w	d2,d1
				add.w	#KEY_F1,d1
				move.w	d1,ITEM.FastKey(a1)
				move.w	12(a7),d1
				bne.s	\Ok
					move.w	d2,d1
					addq.w	#1,d1
\Ok:				move.w	d1,ITEM.Id(a1)
				move.b	#A_REVERSE,ITEM.Attr(a1)
				move.b	#1,ITEM.Font(a1)
				move.l	8(a7),ITEM.Data(a1)
				move.w	d2,d1
				subq.w	#1,d1
				move.b	d1,ITEM.ItemLf(a1)
				addq.w	#2,d1
				move.b	d1,ITEM.ItemRg(a1)
				move.l	#DIALOG.CB_SELECT_MENU,ITEM.SelectCB(a1)
				move.l	#DIALOG.CB_RELIEF,ITEM.UnSelectCB(a1)
				move.l	#WinStrXY,ITEM.DrawCB(a1)
				move.l	#DIALOG.CB_DOKEY_MENU,ITEM.DoKeyCB(a1)
				clr.l	ITEM.SubDialog(a1)
				; Set X 
				moveq	#8,d0
				tst.w	d2
				beq.s	\First		; First One skip it
					move.w	ITEM.X-ITEM.sizeof(a1),d0
					add.w	ITEM.Width-ITEM.sizeof(a1),d0
					addq.w	#8,d0
\First				move.w	d0,ITEM.X(a1)
				; Set Width
				move.l	8(a7),a0	; Text
				bsr	strlen_reg	; Does not destroy a1
				mulu.w	#6,d0		; x6 = In pixel
				move.w	d0,ITEM.Width(a1)
				bra	\Done
\SubMenu:		; Found the parent	
			bsr	FoundItemFromId
			move.l	a1,d0
			beq	\ret
			move.w	ITEM.SubDialog(a1),d0
			bne.s	\AddIt
				; Alloc a New Sub Dialog
				pea	(DIALOG.ItemsTab).w
				bsr	HeapAlloc
				addq.l	#4,a7
				tst.w	d0
				beq	\ret
					move.w	d0,-(a7)		; Save created Handle
					move.w	2+4(a7),d0		; Handle
					move.w	2+6(a7),d1		; Parent Id
					bsr	FoundItemFromId		; Reget the Item
					move.w	(a7)+,d0
					move.w	d0,ITEM.SubDialog(a1)	; Sauve l'handle !
					move.w	d0,a0
					trap	#3
					move.w	#WF_SAVE_SCR,DIALOG.WinFlag(a0)
					move.w	#70,DIALOG.Height(a0)
					move.w	#98,DIALOG.Width(a0)
\AddIt			move.w	d0,a0	; Add it at the top main menu
			trap	#3
			move.w	DIALOG.NbrItem(a0),d2
			addq.w	#1,d2
			mulu.w	#ITEM.sizeof,d2
			add.w	#DIALOG.ItemsTab,d2
			move.l	d2,-(a7)
			move.w	d0,-(a7)
			bsr	HeapRealloc
			addq.l	#6,a7
			tst.w	d0
			beq	\ret
				move.w	d0,a0		; Rederef Handle
				trap	#3
				move.w	DIALOG.NbrItem(a0),d2
				move.w	d2,d0
				addq.w	#1,DIALOG.NbrItem(a0)
				mulu.w	#ITEM.sizeof,d0
				lea	DIALOG.ItemsTab(a0,d0.l),a1
				clr.w	ITEM.X(a1)
				move.w	d2,d1
				lsl.w	#3,d1
				move.w	d1,ITEM.Y(a1)
				move.w	#96,ITEM.Width(a1)
				move.w	#8,ITEM.Height(a1)
				move.w	d2,d1
				add.w	#'a',d1
				move.w	d1,ITEM.FastKey(a1)
				move.w	12(a7),d1
				bne.s	\Ok2
					move.w	d2,d1
					addq.w	#1,d1
\Ok2:				move.w	d1,ITEM.Id(a1)
				move.b	#A_NORMAL,ITEM.Attr(a1)
				move.b	#1,ITEM.Font(a1)
				move.l	8(a7),ITEM.Data(a1)
				move.w	d2,d1
				subq.w	#1,d1
				move.b	d1,ITEM.ItemUp(a1)
				addq.w	#2,d1
				move.b	d1,ITEM.ItemDn(a1)
				st.b	ITEM.ItemLf(a1)
				move.l	#DIALOG.CB_INVERSE,ITEM.SelectCB(a1)
				move.l	#DIALOG.CB_INVERSE,ITEM.UnSelectCB(a1)
				move.l	#WinStrXY,ITEM.DrawCB(a1)
				move.l	#DIALOG.CB_DOKEY_SUBMENU,ITEM.DoKeyCB(a1)
				clr.l	ITEM.SubDialog(a1)
\Done	move.w	4(a7),d0
\ret	rts	

;HANDLE MenuAddIcon (HANDLE Handle, short ParentID, const void *Icon, short ID, short Flags); 
MenuAddIcon:
	move.w	4(a7),d0		; Handle
	beq	\ret
		move.w	6(a7),d1	; Parent Id
		bgt	\SubMenu	; Sub-Menu
			; Add it in the Top Level Menu
			move.w	d0,a0	; Add it at the top main menu
			trap	#3
			move.w	DIALOG.NbrItem(a0),d2
			addq.w	#1,d2
			mulu.w	#ITEM.sizeof,d2
			add.w	#DIALOG.ItemsTab,d2
			move.l	d2,-(a7)
			move.w	d0,-(a7)
			bsr	HeapRealloc
			addq.l	#6,a7
			tst.w	d0
			beq	\ret
				move.w	d0,a0		; Rederef Handle
				trap	#3
				move.w	DIALOG.NbrItem(a0),d2
				move.w	d2,d0
				addq.w	#1,DIALOG.NbrItem(a0)
				mulu.w	#ITEM.sizeof,d0
				lea	DIALOG.ItemsTab(a0,d0.l),a1
				clr.w	d0		; Set X
				tst.w	d2
				beq.s	\First		; First One skip it
					move.w	ITEM.X-ITEM.sizeof(a1),d0
					add.w	ITEM.Width-ITEM.sizeof(a1),d0
\First				addq.w	#8,d0
				move.w	d0,ITEM.X(a1)
				move.w	#16,ITEM.Width(a1)
				move.w	#1,ITEM.Y(a1)
				move.w	#16,ITEM.Height(a1)
				move.w	d2,d1
				add.w	#KEY_F1,d1
				move.w	d1,ITEM.FastKey(a1)
				move.w	12(a7),d1
				bne.s	\Ok
					move.w	d2,d1
					addq.w	#1,d1
\Ok:				move.w	d1,ITEM.Id(a1)
				move.b	#A_NORMAL,ITEM.Attr(a1)
				move.b	#1,ITEM.Font(a1)
				move.l	8(a7),ITEM.Data(a1)
				move.w	d2,d1
				subq.w	#1,d1
				move.b	d1,ITEM.ItemLf(a1)
				addq.w	#2,d1
				move.b	d1,ITEM.ItemRg(a1)
				;st.b	ITEM.ItemUp(a1)
				move.l	#DIALOG.CB_SELECT_MENU,ITEM.SelectCB(a1)
				move.l	#DIALOG.CB_RELIEF,ITEM.UnSelectCB(a1)
				move.l	#WinDrawIcon,ITEM.DrawCB(a1)
				move.l	#DIALOG.CB_DOKEY_MENU,ITEM.DoKeyCB(a1)
				clr.l	ITEM.SubDialog(a1)
				bra.s	\Done
\SubMenu:	clr.w	d0				; Icon can not be in Sub-Menu
		bra.s	\ret				; Well It could be neertheless easilly possible due to the Dialog struct, but I don't want to do it
\Done	move.w	4(a7),d0
\ret	rts	

;void MenuOn (HANDLE ExecHandle);
MenuOn:
	move.w	4(a7),d0
	trap	#3
	bra.s	MenuBegin_Entry
	
;HANDLE MenuBegin (const void *MenuPtr, short x, short y, unsigned short Flags, ...); 
; I assume x = y = 0 and i don't care about flags.
MenuBegin:
	move.l	4(a7),a0
MenuBegin_Entry:
	movem.l	d3-d7/a2-a6,-(a7)
	move.l	a0,a2
	clr.w	d5
	clr.w	d6
	clr.w	d7
	st.b	d2
	lea	-68(a7),a7
	move.l	a7,a3	
	move.w	#WF_DUP_SCR|WF_NOBORDER|WF_BLACK,-(a7)	; Flags
	pea	\MenuBeginWinRect(pc)
	pea	(a3)
	bsr	WinOpen
	tst.w	d0
	beq.s	\Fail
		bsr	DIALOG.Scroll			; Display it
		bsr	WinClose			; Window is already pushed
\Fail	lea	68+10(a7),a7
	move.l	a2,a0
	bsr	HeapPtrToHandle_reg
	movem.l	(a7)+,d3-d7/a2-a6
	rts
\MenuBeginWinRect
	dc.w	0,0,239,17
		
;short MenuKey (HANDLE ExecHandle, short KeyCode);	
MenuKey:
	move.w	6(a7),-(a7)
	bsr	pushkey		; Repush the key
	move.w	6(a7),a0
	clr.l	-(a7)		; X/Y
	trap	#3
	pea	(a0)
	bsr	DIALOG.Do
	lea	10(a7),a7
	cmpi.w	#$7FFF,d0	; Esc ?
	bne.s	\NoEsc
		moveq	#0,d0
\NoEsc	rts
	
;void MenuEnd (HANDLE ExecHandle); 
MenuEnd:
	bsr	PortRestore
	move.w	4(a7),-(a7)
	bsr	HeapFree

	clr.w	(a7)			; White
	pea	ScrRect(pc)		; Clip
	pea	MenuRect(pc)		; What to erase
	bsr	ScrRectFill		; Fill in White the menu
	lea	10(a7),a7
	rts

;void MenuTopStat (HANDLE ExecHandle, short Item, short State);
;void MenuSubStat (HANDLE ExecHandle, short ID, short State);
MenuTopStat:
MenuSubStat:
	move.w	4(a7),d0		; Handle
	move.w	6(a7),d1		; Id
	bsr	FoundItemFromId
	move.l	a1,d0
	beq.s	\NotFound
		tst.w	8(a7)
		sne.b	d0
		move.b	d0,ITEM.ItemLf(a1)	; Item Left is used for this ;)
\NotFound
	rts
		
; ******************************************************************************************
;
;				Tios Events
;		Only ONE application may be active AND install at the same time.
;
; ******************************************************************************************

;short CB_replaceTEXT (char *text, unsigned long len, short strip_CR);
CB_replaceTEXT:
	move.w	CLIPBOARD_HANDLE,-(a7)
	bsr	HeapFree
	addq.l	#2,a7
	move.l	8(a7),d0
	move.l	d0,CLIPBOARD_LEN		; Save Len
	bsr	HeapAlloc_reg
	tst.w	d0
	beq.s	\end
		move.w	d0,CLIPBOARD_HANDLE	; Save Handle
		move.w	d0,a0
		trap	#3
		move.l	4(a7),a1
		move.l	CLIPBOARD_LEN,d0
		subq.w	#1,d0
\loop			move.b	(a1)+,(a0)+
			dbf	d0,\loop
\end	rts

;short CB_fetchTEXT (HANDLE *hText, unsigned long *len);
CB_fetchTEXT:
	move.l	4(a7),a0
	move.w	CLIPBOARD_HANDLE,(a0)
	move.l	8(a7),a0
	move.l	CLIPBOARD_LEN,d0
	move.l	d0,(a0)
	rts

;void EV_registerMenu (void *MenuPtr);
EV_registerMenu:
	move.l	4(a7),EV_CurrentMenu
	rts

;void MenuUpdate (void);
; In my opinion, MenuUpdate is an EVENT function, not a menu function.
MenuUpdate:
	move.l	EV_CurrentMenu,d0
	beq.s	\Ret
		move.l	d0,a0
		bra	MenuBegin_Entry
\Ret	rts

;EVENT_HANDLER EV_captureEvents (EVENT_HANDLER NewHandler);
EV_captureEvents:
	move.l	EV_handler,a0
	move.l	4(a7),EV_handler
	rts
	
;void EV_clearPasteString (void);
EV_clearPasteString:
	clr.l	CLIPBOARD_LEN
	pea	CLIPBOARD_HANDLE
	bsr	HeapFreeIndir
	addq.l	#4,a7
	rts

;void EV_centralDispatcher (void);
EV_centralDispatcher:
	trap	#12
	jmp	OSCont

;WIN_RECT *EV_getSplitRect (unsigned short Side);
; No side available :)
EV_getSplitRect:
	lea	\Table(pc),a0
	rts
\Table	dc.w	0,18,SCR_WIDTH-1,SCR_HEIGHT-10

;void EV_notifySwitchGraph (void); 
;void EV_switch (void);
; No side available, so the function becomes very simple
EV_notifySwitchGraph:
EV_switch:
	rts

;void EV_paintWindows (void); 
EV_paintWindows:
	tst.b	EV_PaintingEnable
	bne.s	\ret
\Loop		bsr.s	EV_paintOneWindow
		tst.w	d0
		bne.s	\Loop
\ret	rts

;short EV_restorePainting (short blockPaint);
EV_restorePainting:
	move.w	4(a7),d1
EV_restorePainting_entry
	clr.w	d0
	move.b	EV_PaintingEnable,d0
	move.b	d1,EV_PaintingEnable
	rts

;short EV_suspendPainting (void);	
EV_suspendPainting:
	moveq	#2,d1	
	bra.s	EV_restorePainting_entry

;short EV_paintOneWindow (void);
EV_paintOneWindow:
	movem.l	a2-a3,-(a7)
	lea	-$14(a7),a7
	suba.l	a3,a3
	move.l	FirstWindow,a2
	bra.s	\SearchLoop
\Loop		move.w	WINDOW.Flags(a2),d0
		andi.w	#WF_DIRTY,d0
		beq.s	\NoDirty
			move.l	a2,a3
\NoDirty	move.l	WINDOW.Next(a2),a2
\SearchLoop	move.l	a2,d0
		bne.s	\Loop
	move.l	d3,d0
	beq.s	\NoDirtyWindow	; 6E
		bclr	#5,WINDOW.Flags(a3)
		move.w	WINDOW.Flags(a3),d0
		andi.w	#WF_VISIBLE,d0
		beq.s	\NoVisible	; 68
		move.w	WINDOW.TaskId(a3),d0
		bmi.s	\NoVisible
		cmpi.w	#MAX_TASKID,d0
		bge.s	\NoVisible
			move.w	#$760,6(a7)
			move.l	a3,$E(a7)
			lea	6(a7),a0
			move.l	a0,(a7)
			move.w	6(a3),-(a7)
			bsr	EV_sendEvent
			addq.l	#2,a7
			move.l	FirstWindow,a2
			bra.s	\SearchLoop2
\Loop2				pea	WINDOW.Window(a3)
				pea	WINDOW.Window(a2)
				bsr	QScrRectOverlap
				addq.l	#8,a7
				tst.w	d0
				beq.s	\Next
					bset	#5,WINDOW.Flags(a2)
\Next				move.l	WINDOW.Next(a2),a2
\SearchLoop2			cmp.l	a3,a2
				bne.s	\Loop2
\NoVisible	moveq	#1,d0
		bra.s	\Done
\NoDirtyWindow	moveq	#0,d0
\Done	lea	$14(a7),a7
	movem.l	(a7)+,a2-a3
	rts

;void EV_sendString (unsigned short XR_String); 
EV_sendString:
	lea	-$14(a7),a7
	move.w	#$723,$6(a7)
	move.w	$18(a7),(a7)
	bsr.s	XR_stringPtr
	move.l	a0,$E(a7)
	lea	$6(a7),a1
	move.l	a1,(a7)
	move.w	#$FFFF,-(a7)
	bsr.s	EV_sendEvent
	lea	$16(a7),a7
	rts

;const char *XR_stringPtr (long XR_string_no);
XR_stringPtr:
	moveq	#0,d0
	move.w	4(a7),d0
	add.w	d0,d0
	move.w	XR_stringPtrTable(pc,d0.w),d0
	lea	STRINGS,a0
	adda.l	d0,a0
	rts
; *********************************************
; *  Macros for processing easilly XR_string  *
XRSTRING_CPT	SET	0
ADD_XRSTRING	MACRO
XR_\1		set	XRSTRING_CPT
XRSTRING_CPT	set	XRSTRING_CPT+1
	dc.w	\1_XRstr-STRINGS
		ENDM
; *********************************************
XR_stringPtrTable
	ADD_XRSTRING	Ln
	ADD_XRSTRING	Exp
	ADD_XRSTRING	Sin
	ADD_XRSTRING	Cos
	ADD_XRSTRING	Tan
	ADD_XRSTRING	ASin
	ADD_XRSTRING	ACos
	ADD_XRSTRING	ATan
	ADD_XRSTRING	Sqrt
	ADD_XRSTRING	Int
	ADD_XRSTRING	Der
	ADD_XRSTRING	Sigma
	ADD_XRSTRING	Inv
	ADD_XRSTRING	Ans

;void EV_startTask (unsigned short StartType);
EV_startTask:
	lea	-$14(a7),a7
	move.w	#$702,$6(a7)
	clr.w	(a7)
	bsr	EV_getSplitRect
	move.l	a0,$E(a7)
	move.b	$19(a7),$12(a7)
	lea	$6(a7),a0
	move.l	a0,(a7)
	move.w	#$FFFE,-(a7)
	bsr.s	EV_sendEvent
	lea	$16(a7),a7
	rts

;void EV_sendEvent (short TaskID, EVENT *event);
;void EV_sendEventSide (short TaskID, EVENT *event, unsigned short Side);
EV_sendEvent:
EV_sendEventSide:
	movem.l	d3-d4/a2,-(a7)
	subq.l	#4,a7
	move.w	$14(a7),d3
	move.l	$16(a7),a2
	move.w	EV_RunningAppId,d4
	cmpi.w	#$FFFF,d3
	bne.s	\NoRunning
		move.w	d4,d3
\NoRunning
	cmpi.w	#$FFFE,d3
	bne.s	\NoCurrent
		move.w	EV_CurrentAppId,d3
\NoCurrent
	move.w	d4,2(a2)		; Task Id
	move.w	$1a(a7),4(a2)		; Side ?
	clr.w	6(a2)			; Statut
	move.w	d3,EV_RunningAppId	; Change the Running App (!= Current !)
	move.l	EV_hook,d0
	beq.s	\NoHook
		move.l	a2,(a7)
		move.l	d0,a0
		jsr	(a0)		; TSR powa !
\NoHook	move.l	EV_handler,a0
	move.l	a0,d0
	beq.s	\Std
		cmpi.w	#$760,(a2)
		bne.s	\jump
\Std	cmpi.w	#$FFFD,d3
	beq.s	\Nothing
		lsl.w	#2,d3
		move.l	EV_handlerTable(pc,d3.w),a0
\jump	move.l	a2,(a7)
	jsr	(a0)
\Nothing
	move.w	d4,EV_RunningAppId
	addq.l	#4,a7
	movem.l	(a7)+,d3-d4/a2
	rts

EV_handlerTable
	dc.l	EV_defaultHandler 

;void EV_startSide (short *saveTaskID, short TaskID, unsigned short Side);
EV_startSide:
	link	a6,#$FFE4
	movem.l	d3-d4,-8(a6)
	move.w	$c(a6),d3
	move.w	$e(a6),d4
	move.w	#$702,-$16(a6)
	move.w	d4,(a7)
	bsr	EV_getSplitRect
	move.l	a0,-$E(a6)
	clr.b	-$A(a6)
	move.l	8(a6),a0
	move.w	d3,(a0)
	move.w	d4,(a7)
	pea	-$16(a6)
	move.w	d3,-(a7)
	bsr	EV_sendEventSide
	move.w	d3,EV_CurrentAppId
	move.w	#$703,-$16(a6)
	bsr	EV_sendEventSide
	move.w	#$704,-$16(a6)
	bsr	EV_sendEventSide
	movem.l	-8(a6),d3-d4
	unlk	a6
	rts
	
;void EV_startApp (short TaskID, unsigned short StartType);
EV_startApp:
	movem.l	d3-d5,-(a7)
	move.w	$10(a7),d3
	move.w	$12(a7),d4
	subq.l	#2,a7
	cmp.w	EV_CurrentAppId,d3
	bne.s	\NoCurrent	;292
		tst.w	d4
		beq.s	\Nul	;284
			move.w	#$705,(a7)
			bsr.s	SendEvent
			move.w	#$706,(a7)
			bsr.s	SendEvent
			move.w	#$707,(a7)
			bsr.s	SendEvent
			move.w	d4,(a7)
			bsr	EV_startTask
			move.w	#$703,(a7)
			bsr.s	SendEvent
			move.w	#$704,(a7)
			bsr.s	SendEvent
			bra.s	\Done
\Nul		move.w	#$708,(a7)
		bsr.s	SendEvent
		bra.s	\Done
\NoCurrent	move.w	#$705,(a7)
		bsr.s	SendEvent
		move.w	#$706,(a7)
		bsr.s	SendEvent
		move.w	#$707,(a7)
		bsr.s	SendEvent
		move.w	d3,EV_CurrentAppId
		move.w	d4,(a7)
		bsr	EV_startTask
		move.w	#$703,(a7)
		bsr.s	SendEvent
		move.w	#$704,(a7)
		bsr.s	SendEvent
\Done	addq.l	#2,a7
	movem.l	(a7)+,d3-d5		
	rts
		
SendEvent:
	lea	-$14(a7),a7
	move.w	$18(a7),6(a7)
	pea	6(a7)
	move.w	#$FFFE,-(a7)
	bsr	EV_sendEvent
	lea	$1C(a7),a7
	rts

		
;void EV_eventLoop (void); 
EV_eventLoop:
	movem.l	d3/a2-a4,-(a7)
	lea	-$14(a7),a7
	clr.w	EV_globalERD
	bra.s	\StartLoop
\Loop:		; CB8	
		move.w	#2,(a7)
		bsr	OSTimerExpired
		tst.w	d0
		beq.s	\NoOff
			bsr	off
\StartLoop		move.w	#2,(a7)
			bsr	OSTimerRestart
\NoOff		bsr	OSClearBreak
		bsr	OSEnableBreak
		move.w	EV_globalERD,(a7)
		beq.s	\NoErd
			clr.w	EV_globalERD
			bsr	ERD_dialog
			bra.s	\Loop	
\NoErd		moveq	#0,d0
		move.l	d0,d3
		move.w	d0,$c(a7)
		tst.l	EV_globalPasteString
		beq.s	\NoEvPasteString
			move.w	#2,a0
\Continue		move.l	EV_globalPasteString,a1
			move.b	(a1)+,d3
			move.l	a1,EV_globalPasteString
			cmp.w	a0,d3
			beq.s	\Continue
			tst.w	d3
			bne.s	\SendKey
			clr.l	EV_globalPasteString		; End of string
\NoEvPasteString:
		bsr	kbhit
		tst.w	d0
		beq.s	\NoKey
			bsr	GetKey
			move.w	d0,d3
			andi.w	#$F800,d0
			ori.w	#$76DE,d0
			move.w	d0,$C(a7)
\NoKey		tst.w	d3
		beq.s	\NoSendKey
\SendKey		move.w	#1,(a7)
			bsr	ST_busy
			move.w	#$710,4(a7)
			move.w	d3,$e(a7)
			pea	4(a7)
			move.w	EV_CurrentAppId,-(a7)
			bsr	EV_sendEvent
			addq.l	#6,a7
			move.w	#2,(a7)
			bsr	OSTimerRestart
			bra	\Loop			
\NoSendKey			
		tst.b	CURSOR_STATE
		beq.s	\NoCursor
			move.w	#4,(a7)
			bsr	OSTimerExpired
			tst.w	d0
			beq.s	\NoCursor
			move.w	#$740,4(a7)
			pea	4(a7)
			move.w	EV_CurrentAppId,-(a7)
			bsr	EV_sendEvent
			addq.l	#6,a7
			bra	\Loop
\NoCursor	jsr	CleanTwinFiles
		tst.b	EV_PaintingEnable
		bne.s	\NoPainting
			bsr	EV_paintOneWindow
			tst.w	d0
			beq.s	\NoPainting
			move.w	#2,(a7)
			bsr	OSTimerRestart
			bra	\Loop
\NoPainting	move.w	#$700,(a7)
		bsr	SendEvent
		clr.w	(a7)
		bsr	ST_busy
		bsr	idle
		bra	\Loop

;unsigned short EV_getc (unsigned short busy, EVENT *event);
EV_getc:
	movem.l	d3/a2-a3,-(a7)
	move.l	$12(a7),a2
	moveq	#0,d3
	move.w	$10(a7),-(a7)
	bsr	ST_busy
	bra.s	\StartLoop
\Loop		move.w	#2,(a7)
		bsr	OSTimerExpired
		tst.w	d0
		beq.s	\NoExpired
			bsr	off
\StartLoop		move.w	#2,(a7)
			bsr	OSTimerRestart
\NoExpired	bsr	kbhit
		tst.w	d0
		beq.s	\NoKey
			bsr	GetKey
			move.w	d0,d3
			move.w	#2,(a7)
			bsr	OSTimerRestart
			move.w	#$710,(a2)
			move.w	d3,$a(a2)
			bra.s	\Done
\NoKey		tst.b	CURSOR_STATE
		beq.s	\NoCursor
			move.w	#4,(a7)
			bsr	OSTimerExpired
			tst.w	d0
			beq.s	\NoCursor
			move.w	#$740,(a2)
			bra.s	\Done
\NoCursor	bsr	idle
		bra.s	\Loop
\Done:	tst.w	d3
	beq.s	\NoSetBusy
		move.w	#1,(a7)
		bsr	ST_busy
\NoSetBusy
	move.w	d3,d0
	addq.l	#2,a7
	movem.l	(a7)+,d3/a2-a3
	rts
	
;void EV_defaultHandler (EVENT *event);
EV_defaultHandler:
	move.l	4(a7),a0
	move.w	(a0),d0		; Message
	cmpi.w	#CM_ACTIVATE,d0
	beq	MenuUpdate
	cmpi.w	#CM_DEACTIVATE,d0
	beq.s	\DeActivate
	cmpi.w	#CM_KEYPRESS,d0
	beq.s	\KeyPress
	cmpi.w	#CM_STRING,d0
	beq.s	\String
	cmpi.w	#CM_HSTRING,d0
	beq.s	\HString
	cmpi.w	#CM_STORE,d0
	beq.s	\Store
	cmpi.w	#CM_RECALL,d0
	beq.s	\Recall
	rts			; Nothing to do
\DeActivate:
	; FIXME : CustomEnd / MenuEnd ?
	; MenuEnd will destroy the menu (Cannot be reopen). So I do nothing than clear the zone.
	clr.w	-(a7)			; White
	pea	ScrRect(pc)		; Clip
	pea	MenuRect(pc)		; What to erase
	bsr	ScrRectFill		; Fill in White the menu
	lea	10(a7),a7
	rts
\String:
	move.l	8(a0),EV_globalPasteString
	rts
\HString:
	move.w	8(a0),-(a7)
	bsr	HeapFree
	addq.l	#2,a7
	rts
\Store:	lea	-$14(a7),a7
	move.w	#$710,$6(a7)
	move.w	#$16,$10(a7)
	pea	6(a7)
	move.w	#$FFFF,-(a7)
	bsr	EV_sendEvent
	lea	$1A(a7),a7
	rts
\Recall	move.w	#1,-(a7)
	bsr	handleRclKey
	addq.l	#2,a7
	rts
\KeyPress
	; System / mode / extra keys ?	
	move.w	10(a2),d0		; Keys
	; F1 -> F8 : Pass to Registered Menu
	cmpi.w	#KEY_F1,d0
	blt.s	\NoMenu
	cmpi.w	#KEY_F8,d0
	bgt.s	\NoMenu
		move.w	d0,-(a7)
		move.l	EV_CurrentMenu,a0
		bsr	HeapPtrToHandle_reg
		move.w	d0,-(a7)
		bsr	MenuKey
		tst.w	d0
		beq.s	\ret
			move.w	d0,(a7)
			bsr	SendEvent
\ret		addq.l	#4,a7
		rts		
\NoMenu
	; APPS / MATH / CATALOG / CUSTOM / CHAR / VAR-LINK / QUIT / SWITCH / ...
	cmpi.w	#KEY_MODE,d0
	beq	MO_modeDialog
	cmpi.w	#KEY_OFF,d0
	beq	off
	cmpi.w	#KEY_DIAMOND+'k',d0
	beq	HelpKeys
	cmpi.w	#KEY_SWITCH,d0
	beq	EV_switch
	cmpi.w	#KEY_2ND+'2',d0
	beq	CAT_dialog
	cmpi.w	#KEY_VARLINK,d0
	beq	handleVarLinkKey
	cmpi.w	#KEY_APPS,d0
	bne.s	\NoApps
		clr.l	-(a7)
		pea	AppsText
		pea	AppsDialogTitle
		bsr	DlgMessage
		lea	12(a7),a7
		rts
\NoApps	cmpi.w	#KEY_QUIT,d0			; What to do ?
	bne	\NoQuit
		clr.l	-(a7)
		bsr	EV_startApp		; Start App 0 as current
		addq.l	#4,a7
		rts
\NoQuit	cmpi.w	#KEY_2ND+'3',d0
	bne.s	\NoCustom
		tst.w	EV_customHandle
		bne	CustomEnd
		bra	CustomBegin		
\NoCustom	
	cmpi.w	#KEY_2ND+'+',d0
	bne.s	\NoChar
		clr.l	-(a7)
		pea	DialogChar(pc)
		bsr	DIALOG.Do
		addq.l	#8,a7
		cmpi.w	#$7FFF,d0
		beq.s	\ret2
			lea	DialogChar+DIALOG.ItemsTab(pc),a0
			mulu.w	#ITEM.sizeof,d0
			adda.l	d0,a0
			clr.w	d0
			move.b	ITEM.Data(a0),d0	; Key to send
			lea	-$14(a7),a7
			move.w	#CM_KEYPRESS,$6(a7)
			move.w	d0,$10(a7)
			pea	6(a7)
			move.w	#$FFFF,-(a7)
			bsr	EV_sendEvent
			lea	$1A(a7),a7
\ret2		rts
\NoChar:	
	; CUT / COPY / PASTE -> SendEvent
	cmpi.w	#KEY_DIAMOND+'c',d0
	bne.s	\NoCopy
		move.w	#CM_MENU_COPY,-(a7)
		bra.s	\Send
\NoCopy	cmpi.w	#KEY_DIAMOND+'x',d0
	bne.s	\NoCut
		move.w	#CM_MENU_CUT,-(a7)
		bra.s	\Send
\NoCut	cmpi.w	#KEY_DIAMOND+'v',d0
	bne.s	\NoPaste
		move.w	#CM_MENU_PASTE,-(a7)
		bra.s	\Send
\NoPaste	
	; STO: SendEvend(CM_STORE)
	cmpi.w	#KEY_STO,d0
	bne.s	\NoSto
		move.w	#CM_STORE,-(a7)
\Send		bsr	SendEvent
		addq.l	#2,a7
		rts
\NoSto	; Recall: SendEvent(CM_RECALL)
	cmpi.w	#KEY_2ND+KEY_STO,d0
	bne.s	\NoRecall
		move.w	#CM_RECALL,-(a7)
		bra.s	\Send
\NoRecall
	; Home/Y=/Window/Graph/TblSet/Table ? Ignored
	; LN / EXP / SIN / COS/ TAN / ASIN / ACOS/ATAN / SQRT / INTEGRAL / DERIVATE / Sigma / -1
	cmpi.w	#KEY_LN,d0
	bne.s	\NoLn
		moveq	#XR_Ln,d1
\SendXR		move.w	d1,-(a7)
		bsr	EV_sendString
		addq.l	#2,a7
		rts
\NoLn	moveq	#XR_Exp,d1
	cmpi.w	#KEY_2ND+KEY_LN,d0
	beq.s	\SendXR
	moveq	#XR_Sin,d1
	cmpi.w	#KEY_SIN,d0
	beq.s	\SendXR
	moveq	#XR_Cos,d1
	cmpi.w	#KEY_COS,d0
	beq.s	\SendXR
	moveq	#XR_Tan,d1
	cmpi.w	#KEY_TAN,d0
	beq.s	\SendXR
	moveq	#XR_ASin,d1
	cmpi.w	#KEY_2ND+KEY_SIN,d0
	beq.s	\SendXR
	moveq	#XR_ACos,d1
	cmpi.w	#KEY_2ND+KEY_COS,d0
	beq.s	\SendXR
	moveq	#XR_ATan,d1
	cmpi.w	#KEY_2ND+KEY_TAN,d0
	beq.s	\SendXR
	moveq	#XR_Sqrt,d1
	cmpi.w	#KEY_2ND+'*',d0
	beq.s	\SendXR
	moveq	#XR_Int,d1
	cmpi.w	#KEY_2ND+'7',d0
	beq.s	\SendXR
	moveq	#XR_Der,d1
	cmpi.w	#KEY_2ND+'8',d0
	beq.s	\SendXR
	moveq	#XR_Ans,d1
	cmpi.w	#KEY_2ND+173,d0
	beq.s	\SendXR
	moveq	#XR_Sigma,d1
	cmpi.w	#KEY_2ND+'4',d0
	beq.s	\SendXR
	moveq	#XR_Inv,d1
	cmpi.w	#KEY_2ND+'9',d0
	beq.s	\SendXR
	rts
	
handleVarLinkKey:
DialogChar:
	illegal

; ******************************************************************************************
;
;				Tios Mode functions
;		Since no mode is available, the functions become very simple.
;			If I implement this one day, I may change it
; ******************************************************************************************
;void MO_currentOptions (void);
;void MO_defaults (void); 
;void MO_digestOptions (short Folder); 
;short MO_isMultigraphTask (short TaskID);
;void MO_notifyModeChange (short Flags);
MO_currentOptions:
MO_defaults:
MO_digestOptions:
MO_isMultigraphTask:
MO_notifyModeChange:
	moveq	#0,d0
	rts

;void MO_modeDialog (void);
MO_modeDialog:
	clr.l	-(a7)
	pea	ModeText
	pea	AppsDialogTitle
	bsr	DlgMessage
	lea	12(a7),a7
	rts

;void MO_sendQuit (short TaskID, short Side);
MO_sendQuit:
	link	a6,#$FFE4
	movem.l	d3-d4,-8(a6)
	move.w	8(a6),d3
	cmpi.w	#$FFFD,d3
	beq.s	\None
		move.w	#$705,-$16(a6)
		clr.w	(a7)
		pea	-$16(a6)
		move.w	d3,-(a7)
		bsr	EV_sendEventSide
		move.w	#$706,-$16(a6)
		bsr	EV_sendEventSide
		move.w	#$707,-$16(a6)
		bsr	EV_sendEventSide
		move.w	#$FFFD,EV_RunningAppId
		move.w	#$FFFD,EV_CurrentAppId
\None	movem.l	-8(a6),d3-d4
	unlk	a6
	rts

; ******************************************************************************************
;				Dialog Tios functions
; ******************************************************************************************

; long Select(WINDOW *w asm("a3"), short x asm("d0), short y asm("d1"), ITEM *i asm("a4"));
; void  UnSelect(WINDOW *w asm("a3"), short x asm("d0), short y asm("d1"), ITEM *i asm("a4"));
DIALOG.CB_TAG:
	subq.l	#8,a7				; Stack Frame
	move.l	a7,a0				; Ptr -> Frame
	move.w	#1,(a0)+			; X1 = 1
	move.w	d1,(a0)+			; Y1
	move.w	#3,(a0)+			; X2
	addq.w	#6,d1
	move.w	d1,(a0)+			; Y2
	move.w	#A_XOR,-(a7)			; ATTR
	pea	-8(a0)				; WIN_RECT
	pea	(a3)				; WINDOW
	bsr	WinFill
	lea	18(a7),a7
	moveq	#0,d0
	rts

; Base Item :
;	Can move up/down. Can select with enter, or cancel with ESC.
;long DoKey(short key asm("d3"), ITEM *i("a4"));
DIALOG.CB_BASE_DOKEY:
	move.w	d3,d0			; d0.uw = Key
	swap	d0			; 
	cmpi.w	#KEY_UP,d3
	bne.s	\NoUp
		moveq	#DOKEY.ItemUp,d0	; Next Item
		rts
\NoUp:	cmpi.w	#KEY_DOWN,d3
	bne.s	\NoDn
		moveq	#DOKEY.ItemDn,d0	; Next Item
		rts
\NoDn:	cmpi.w	#KEY_ENTER,d3
	bne.s	\NoEnter
		move.w	ITEM.Id(a4),d0		; End of selection
		rts
\NoEnter:
	cmpi.w	#KEY_ESC,d3
	bne.s	\NoEsc
		move.w	#$7FFF,d0		; Return 
		rts
\NoEsc	moveq	#DOKEY.Default,d0		; Default
	rts

; Base Item :
;	Can move up/down. Can select with enter, or cancel with ESC.
;	Can fill the buffer with standard keys.
;long DoKey(short key asm("d3"), ITEM *i("a4"));
DIALOG.CB_REQUEST_DOKEY:
	move.w	d3,d0			; d0.uw = Key
	swap	d0			; 
	cmpi.w	#KEY_UP,d3
	bne.s	\NoUp
		moveq	#DOKEY.ItemUp,d0	; Next Item
		rts
\NoUp:	cmpi.w	#KEY_DOWN,d3
	bne.s	\NoDn
		moveq	#DOKEY.ItemDn,d0	; Next Item
		rts
\NoDn:	cmpi.w	#KEY_ENTER,d3
	bne.s	\NoEnter
		move.w	ITEM.Id(a4),d0		; End of selection
		rts
\NoEnter:
	cmpi.w	#KEY_ESC,d3
	bne.s	\NoEsc
		move.w	#$7FFF,d0		; Return ESC
		rts
\NoEsc	
	; GetFirstAndLastRequestPtr
	move.l	ITEM.Data(a4),a0
	move.l	a0,a1
\loop		move.b	(a1)+,d0
		bne.s	\loop
	subq.l	#1,a1
	; Fill Data
	cmp.w	#KEY_CLEAR,d3			;Clear ?
	bne.s	\NoClear
		clr.b	(a0)
		moveq	#DOKEY.Redraw,d0
		rts
\NoClear
	cmp.w	#KEY_BACK,d3
	bne.s	\NoDel
		cmp.l	a0,a1
		beq.s	\Default
		clr.b	-(a1)
		moveq	#DOKEY.Redraw,d0
		rts
\NoDel:
	cmpi.w	#' ',d3
	blt.s	\Default
	cmpi.w	#255,d3
	bhi.s	\Default
		move.w	ITEM.SubDialog(a4),d1	; MaxLen
		adda.w	d1,a0
		cmp.l	a0,a1
		beq.s	\Default
		move.b	d3,(a1)+
		clr.b	(a1)+
		moveq	#DOKEY.Redraw,d0
		rts
\Default:
	moveq	#DOKEY.Default,d0		; Default
	rts
		
; Base Item :
;	Can move up/down. Can select with enter, or cancel with ESC.
;	Can enter a Menu.
;long DoKey(short key asm("d3"), ITEM *i("a4"));
DIALOG.CB_PULLDOWN_DOKEY:
	move.w	d3,d0			; d0.uw = Key
	swap	d0			; 
	cmpi.w	#KEY_UP,d3
	bne.s	\NoUp
		moveq	#DOKEY.ItemUp,d0	; Next Item
		rts
\NoUp:	cmpi.w	#KEY_DOWN,d3
	bne.s	\NoDn
		moveq	#DOKEY.ItemDn,d0	; Next Item
		rts
\NoDn:	cmpi.w	#KEY_ENTER,d3
	bne.s	\NoEnter
		move.w	ITEM.Id(a4),d0		; End of selection
		rts
\NoEnter:
	cmpi.w	#KEY_RIGHT,d3
	bne.s	\NoRight
		move.w	ITEM.Y(a4),d1			; Y
		sub.w	d6,d1				; - ScrollY
		add.b	WINDOW.Client+1(a3),d1		; Add Client Window Y
		move.w	d1,-(a7)			; Push Y
		move.w	ITEM.X(a4),d0			; Get X
		add.w	ITEM.Width(a4),d0		; + Width
		sub.w	d5,d0				; -ScrollX
		add.b	WINDOW.Client+0(a3),d0		; Add Window Client X
		move.w	d0,-(a7)			; Push X
		move.w	ITEM.SubDialog(a4),a0		; Get the Handle
		trap	#3				; Deref it
		pea	(a0)				; Push it
		bsr	DIALOG.Do			; Process it
		addq.l	#8,a7		
		subq.w	#1,d0
		bsr	_PullDownFillIndexAndData
		moveq	#DOKEY.Redraw,d0
		rts
\NoRight:
	cmpi.w	#KEY_ESC,d3
	bne.s	\NoEsc
		move.w	#$7FFF,d0		; Return ESC
		rts
\NoEsc	moveq	#DOKEY.Default,d0		; Default
	rts

; In:
;	d0.w = Index
;	a4.l = Item
_PullDownFillIndexAndData:
	move.l	PULLDOWN_PTR,a0
	move.w	ITEM.SubDialog+2(a4),d1
	add.w	d1,d1
	move.w	d0,0(a0,d1.w)
	move.w	ITEM.SubDialog(a4),a0		; Deref Menu
	trap	#3
	mulu.w	#ITEM.sizeof,d0
	move.l	DIALOG.ItemsTab+ITEM.Data(a0,d0.l),ITEM.Data(a4)
	rts
	
;short DialogDo (HANDLE Handle, short x, short y, char* RequestBuffer, short *PulldownBuffer);	
;	If x/y =-1 => CENTER
;	RequestBuffer : Buffer to input string
;	PulldownBuffer: Initial value for PullDown Menus.
DialogDo:
	movem.l	d3-d7/a2-a6,-(a7)
	; Get Dialog Ptr
	move.w	44(a7),a0
	trap	#3
	move.l	a0,a2
	; Check	Signature
	cmp.l	#DialogSignature,DialogSigna(a2)
	bne	\Failed
	; Read Request Buffer
	move.l	50(a7),d7		; Request Buffer
	;	Save PullDownBuffer
	move.l	54(a7),PULLDOWN_PTR
	;	Scan all the items and reloc them.
	;	Search for the hightest y value for Height max
	moveq	#0,d5					; Height
	move.w	DialogSize+DIALOG.NbrItem(a2),d3	; Item Counter
	subq.w	#1,d3
	blt.s	\Done
	lea	DialogSize+DIALOG.ItemsTab(a2),a4	; Item Ptr
\ItemLoop	cmp.w	ITEM.Y(a4),d5
		bge.s	\YOk
			move.w	ITEM.Y(a4),d5
\YOk		move.l	ITEM.DoKeyCB(a4),d0
		cmp.l	#DIALOG.CB_REQUEST_DOKEY,d0
		bne.s	\NoRequest
			add.l	d7,ITEM.Data(a4)
			bra.s	\Next
\NoRequest	cmp.l	#DIALOG.CB_PULLDOWN_DOKEY,d0
		bne.s	\Next
			clr.w	d0			; Should read them
			bsr	_PullDownFillIndexAndData
\Next		lea	ITEM.sizeof(a4),a4		; Next Item
		dbf	d3,\ItemLoop	
\Done	;	Add Buttons (Update height) if necessary ?
	;	...
	;	Save Calculated Height if Height = 0
	tst.w	DialogSize+DIALOG.Height(a2)
	bne.s	\NoCalcIt
		add.w	#USED_FONT*2+8+10,d5		; Border+title = 10
		move.w	d5,DialogSize+DIALOG.Height(a2)
\NoCalcIt:
	;	Calc x/y if necessary
	move.w	46(a7),d0
	move.w	48(a7),d1
	cmpi.w	#-1,d0
	bne.s	\NoCalcXY
		move.w	#SCR_WIDTH,d0
		sub.w	DialogSize+DIALOG.Width(a2),d0
		asr.w	#1,d0
		bge.s	\CalcY
			move.w	#SCR_WIDTH-4,DialogSize+DIALOG.Width(a2)
			moveq	#1,d0
\CalcY		move.w	#SCR_HEIGHT,d1
		sub.w	DialogSize+DIALOG.Height(a2),d1
		asr.w	#1,d1
		bge.s	\NoCalcXY
			move.w	#SCR_HEIGHT-4,DialogSize+DIALOG.Height(a2)
			moveq	#1,d1
\NoCalcXY
	;	Call DIALOG.Do
	move.w	d1,-(a7)
	move.w	d0,-(a7)
	pea	DialogSize(a2)
	bsr	DIALOG.Do
	addq.l	#8,a7
	;	Unreloc the Request items.
	move.w	DialogSize+DIALOG.NbrItem(a2),d3	; Item Counter
	subq.w	#1,d3
	blt.s	\Done2
	lea	DialogSize+DIALOG.ItemsTab(a2),a0	; Item Ptr
\ItemLoop2	move.l	ITEM.DoKeyCB(a0),d1
		cmp.l	#DIALOG.CB_REQUEST_DOKEY,d1
		bne.s	\Next2
			sub.l	d7,ITEM.Data(a0)
\Next2		lea	ITEM.sizeof(a0),a0		; Next Item
		dbf	d3,\ItemLoop2
\Done2	; Quit
	swap	d0
	bra.s	\End
\Failed	clr.w	d0
\End	movem.l	(a7)+,d3-d7/a2-a6
	rts
		
;HANDLE DialogNew (short width, short height, DialogNew_t UserFunc);
;	width/height== 0 ? => Auto calculate (Always ?)
;	UserFunc: short (*DialogNew_t) (short x, long y);
;		This function is called : 
;			- whenever an item in the dialog box is created or recreated or gets a focus; (x=-2, y = index from 0) returns 0 if item not selectionnable.
;			- after the user pressed ENTER in a request box; (x=index, y= &RequetsBuffer) returns 1 or -3/-4 if redraw.
;			- after execution of any pulldown menu. (x=index, y= MenuResult) returns 1 or -3/-4 if redraw or -8 for exit.
DialogNew:
	pea	(DialogSize+DIALOG.ItemsTab).w
	bsr	HeapAlloc
	addq.l	#4,a7
	move.w	d0,-(a7)
	beq.s	\Failed
		bsr	HeapDeref
		move.l	#DialogSignature,(a0)+		; Signa
		move.l	10(a7),(a0)+			; CallBack
		clr.l	(a0)+				; Menu + Button
		move.w	#WF_ROUNDEDBORDER,DIALOG.WinFlag(a0)
		move.w	6(a7),d0
		bne.s	\WidthOk
			move.w	#SCR_WIDTH-20,d0
\WidthOk:	move.w	d0,DIALOG.Width(a0)
		move.w	8(a7),DIALOG.Height(a0)
		clr.l	DIALOG.DefaultKeyCB(a0)
\Failed	move.w	(a7)+,d0
	rts
	
;HANDLE DialogAdd (HANDLE Handle, short flags, short x, short y, short ItemType, ...);
;	flags = ????
;	ItemType =
;		2 is for adding request boxes (const char *prompt, unsigned short offset, short MaxLen, short width);
;		7 for adding texts (char* text)
;		8 for adding titles (char *title, short left, short right)
;		14 for adding pulldown menus ( const char *prompt, HANDLE MenuHandle, unsigned short index);
DialogAdd:
	movem.l	d3-d7/a2-a6,-(a7)
	move.b	#USED_FONT,CURRENT_FONT
	move.w	52(a7),d5		; Item Type
	cmpi.w	#8,d5
	bne.s	\AddItem
		; AddTitle(char *title, short left, short right)
		move.w	44(a7),d6		; Load Handle
		move.w	d6,a0			; Handle
		trap	#3			; Deref It
		move.l	54(a7),DialogSize+DIALOG.Title(a0)
		move.b	59(a7),DialogButton(a0)
		move.b	61(a7),DialogButton+1(a0)
		bra.s	_DialogAddEnd
\AddItem
	moveq	#-2,d7
	bsr	_DialogAddItem
	; AddText(char *text) / AddRequest(const char *prompt, ...) / AddMenu(const char *prompt, ...)
	move.l	54(a7),a0
	move.l	a0,ITEM.Data(a1)
	cmpi.w	#7,d5
	beq.s	_DialogAddEnd		
	; Advance X for next Items
	bsr	StrWidth
	addq.w	#6,d0
	add.w	d0,48(a7)	; X+= StrWidth(str)+8
	; Check next item
	cmpi.w	#2,d5
	beq.s	\AddRequest
	cmpi.w	#14,d5
	beq.s	\AddPulldown
	bra.s	_DialogAddEnd

\AddRequest:
	;Request(const char *prompt, unsigned short offset, short MaxLen, short width);
	bsr	_DialogAddItem		; Add a new Item
	moveq	#0,d0
	move.w	58(a7),d0		; Offset
	move.l	d0,ITEM.Data(a1)	; Data (Needs a relloc section !)
	move.l	60(a7),ITEM.SubDialog(a1)	; MaxLen & Width
	move.l	#DIALOG.CB_REQUEST_DOKEY,ITEM.DoKeyCB(a1)
	bra.s	_DialogAddEnd
	
\AddPulldown:
	;Pulldown(const char *prompt, HANDLE MenuHandle, unsigned short index);
	bsr	_DialogAddItem				; Add a new Item
	move.l	58(a7),ITEM.SubDialog(a1)		; Handle + index
	clr.l	ITEM.Data(a1)
	move.l	#DIALOG.CB_PULLDOWN_DOKEY,ITEM.DoKeyCB(a1)
	;bra.s	_DialogAddEnd
	
_DialogAddEnd:
	move.w	d6,d0
	movem.l	(a7)+,d3-d7/a2-a6
	rts
	
; In:
;	d7.w = Selection Up or -1
; To fill :
;	ITEM.SubDialog	& ITEM.Data
; Out:
;	a0 -> Dialog
;	a1 -> New Item
;	a2 -> Previous Item (Warning may skip some texts).
;	d7.w = Index of previous item 
;	d6.w = Handle
;	d4.w = Index of current item
_DialogAddItem:
	move.w	48(a7),a0		; Handle
	trap	#3
	cmp.l	#DialogSignature,DialogSigna(a0)
	bne	_DialogAddFailed
	move.w	DialogSize+DIALOG.NbrItem(a0),d3	; Size
	move.w	d3,d4					; Index of current Item
	cmpi.w	#-2,d7
	bne.s	\SkipD7
		move.w	d4,d7				; Index of Previous Item
		subq.w	#1,d7
\SkipD7	addq.w	#1,d3
	mulu.w	#ITEM.sizeof,d3
	add.w	#DialogSize+DIALOG.ItemsTab,d3
	move.l	d3,-(a7)
	move.w	48+4(a7),-(a7)
	bsr	HeapRealloc
	addq.l	#6,a7
	move.w	d0,d6
	beq	_DialogAddFailed
	move.w	d0,a0
	trap	#3
	addq.w	#1,DialogSize+DIALOG.NbrItem(a0)
	lea	-ITEM.sizeof(a0,d3.l),a1		; Item Ptr
	move.w	52(a7),ITEM.X(a1)
	move.w	54(a7),ITEM.Y(a1)
	subq.w	#8,ITEM.Y(a1)
	move.w	#40,ITEM.Width(a1)
	move.w	#USED_FONT*2+6,ITEM.Height(a1)
	clr.w	ITEM.FastKey(a1)
	move.w	d4,ITEM.Id(a1)
	move.b	#A_REPLACE,ITEM.Attr(a1)
	move.b	#USED_FONT,ITEM.Font(a1)
	move.b	d7,ITEM.ItemUp(a1)
	move.b	#255,ITEM.ItemDn(a1)
	clr.b	ITEM.ItemLf(a1)
	clr.b	ITEM.ItemRg(a1)
	move.l	#DIALOG.CB_TAG,ITEM.SelectCB(a1)
	move.l	#DIALOG.CB_TAG,ITEM.UnSelectCB(a1)
	move.l	#WinStrXY,ITEM.DrawCB(a1)
	move.l	#DIALOG.CB_BASE_DOKEY,ITEM.DoKeyCB(a1)
	move.w	d7,d1
	blt.s	\Done
		mulu.w	#ITEM.sizeof,d1
		lea	DialogSize+DIALOG.ItemsTab(a0,d1.l),a2
		move.b	d4,ITEM.ItemDn(a2)
\Done	rts
	
_DialogAddFailed:	
	addq.l	#4,a7
	movem.l	(a7)+,d3-d7/a2-a6
	clr.w	d0
	rts
	