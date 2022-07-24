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
;***            	Main FILE				***
;***                                                            ***
;******************************************************************

;TI92P			EQU	1	; Or TI89 Or V200 Or TI92P

PEDROM_VERSION		EQU	$0072
PEDROM_STR_VERSION	EQU	'0.72'

**********************************************************
	ifd	TI92P
ROM_BASE		EQU	$400000	; $400000 for ti92+ / $200000 for 89 & V200
	endif
	ifnd	TI92P
ROM_BASE		EQU	$200000	; $400000 for ti92+ / $200000 for 89 & V200	
	endif	
**********************************************************

	RORG	ROM_BASE+$012088
	include "Vectors.h"		; Vectors's table
	include	"Mem.h"			; Macros usefull for defining struct
	include	"Struct.h"		; Constants / Struct
	include "Vars.h"		; Global vars
	include "Wti.h"			; Wti special Macro (Preos source code)

**********************************************************
CODE_START:
Trap_2:
        include	"Boot.asm"		; Boot Code (Set IO ports, clear RAM, unprotect, ...) and go to the Shell Command loop - MUST BE THE FIRST INCLUDE FILE -
	include	"Flash.asm"		; Flash Code (Write to Flash, ...) - MUST BE THE SECOND INCLUDE FILE -
	include "Shell.asm"		; Command shell
	include "Link.asm"		; Link functions.
	include	"Strings.asm"		; String character
	include "Vat.asm"		; VAT functions.
	include "Memstr.asm"		; memcpy/strcmp/... functions
	include	"Heap.asm"		; Heap functions
	include "Graph.asm"		; Graph functions
	include	"Misc.asm"		; Various functions (1)
	include "Estack.asm"		; EStack functions (1)
	include	"RomVoid.asm"		; All other rom_calls 
BASE1_END				; End of first Base of code : MUST BE <$418000

***********************************************************
***							***
***  $418000-$419FFF       8K      [read protected]	***
***							***
***********************************************************
	RORG	ROM_BASE+$1A000
	include	"Printf.asm"		; Printf functions
	include "Kernel.asm"		; Kernel functions
	include "Window.asm"		; Window functions
	include "Dialog.asm"		; Dialog functions
	include "Bitmap.asm"		; Bitmap functions
	include "Estack2.asm"		; EStack functions (2)
	include "Bcd.asm"		; Float Functions (1)
	include	"Unpack.asm"		; Unpack (PPG) functions
	include "Ellipse.asm"		; Ellipses functions
	include "Clipline.asm"		; ClipLine functions
	include	"Long.asm"		; Long Functions (32 bits / 32 bits, ...)
	include	"Vectors.asm"		; Vectors (Error, traps, ...)
	include	"Ints.asm"		; Auto Ints 
	include "md5.asm"		; MD5 Functions
	include "misc2.asm"		; Various functions (2)
	include "float.asm"		; Float functions (2)
	include "cert.asm"		; Certificate functions.
	ifnd	TI89
		include "side92.asm"	; Side
	endif
	ifd	TI89
		include	"side89.asm"
	endif
***********************************************************
***			DATA				***
***********************************************************
		CNOP	0,4		; Long Alignement for DB92
MediumFont	incbin	"Fontes.bin"	; Font Data
		EVEN
StdLib_FILE	incbin	"stdlib.bin"	; Standard Libraries
		EVEN
TIBReceiv	incbin	"tibrcvr.bin"	; TIB Receievr		
TIBReceivEnd	EVEN
		include	"RomCalls.h"	; Romcalls table ($C8)
		EVEN			; Vti version number
		dc.b	"1.48",0,"01/04/2003",0
		
BASE_END:				; MUST BE < $430000

	END     ROM_BASE+$12000		; End of assembly
