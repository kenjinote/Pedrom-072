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

; Mem Var

; Internal data variables
data_offset		set	0
internal_data		set	1
data_global_offset	set	$4C00+$F00

; Set one data :
;	rs name,data_len

rs	MACRO
	ifeq	internal_data
\1	EQU	data_offset
data_offset	set	data_offset+\2
	endif
	ifne	internal_data
\1	EQU	data_global_offset
data_global_offset	set	data_global_offset+\2
	endif
	ENDM

; Ajoute au compteur mais ne declare pas de variables
rs_no	MACRO
	ifeq	internal_data
data_offset	set	data_offset+\2
	endif
	ifne	internal_data
data_global_offset	set	data_global_offset+\2
	endif
	ENDM

; Make a synonimous
rs_sym	MACRO
	ifeq	internal_data
\1	EQU	data_offset
	endif
	ifne	internal_data
\1	EQU	data_global_offset
	endif
	ENDM



; New data (Global data !)
NEW_DATA	MACRO
internal_data	set	1
		ENDM

; struct macros
NEW_STRUCT	MACRO			; Declaration d'une nouvelle structure
internal_data	set	0
data_offset	set	0
		ENDM
SAVE_STRUCT	MACRO			; Sauvegarde de la strucuture
\1		set	data_offset
		ENDM
MULTI_STRUCT	MACRO			; Declaration de structure ayant un parent commun
internal_data	set	0
data_offset	set	\1
		ENDM
END_STRUCT	MACRO			; Fin de la strucuture
\1		set	data_offset
		ENDM

.set	MACRO
	ENDM
