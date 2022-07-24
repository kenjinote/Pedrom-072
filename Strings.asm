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

; Strings

STRINGS:

Pedrom_str		dc.b	"PedroM v"
			dc.l	PEDROM_STR_VERSION
			dc.b	" alpha",0
Author_str		dc.b	10,169," 2003 PpHd / Time To Team",10,0
CheckFlash_str		dc.b	"Checking flash...",10,0
AddArchiveFiles_str	dc.b	"Add archived files...",10,0

HeapCorrupted_str	dc.b	"Corrupted Heap",0
VatCorrupted_str	dc.b	"Corrupted VAT",0
InitError_str		dC.b	"Error when booting system",0
Boot_str		dc.b	"BOOT code missing",0
OSTooBig_str		dc.b	"OS does not fit in the first 128K",0
ErrorListEmpty_str	dc.b	"ERROR LIST is empty",0
TIBInstallError_str	dc.b	"ERROR : TIB Install failed.",0
WrongCalc_str		dc.b	"ROM not designed for this calc",0

CommandNotFound_str	dc.b	"Syntax Error",0

Error_str		dc.b	"Error",0
UnkwowError_str		dc.b	"Unkwown error",0
ArgumentError_str	dc.b	"Argument error",0
ArgumentNameError_str	dc.b	"Argument must be a var name",0
BreakError_str		dc.b	"Break",0
FolderError_str		dc.b	"Folder",0
MemoryError_str		dc.b	"Memory",0
SyntaxError_str		dc.b	"Syntax",0
TooFewError_str		dc.b	"Too few arguments",0
TooManyError_str	dc.b	"Too many arguments",0
DuplicateError_str	dc.b	"Duplicate variable name",0
Variable8Error_str	dc.b	"Variable name is 8 limited",0
LinkTransmission_str	dc.b	"Link transmission",0
TimeOut_str		dc.b	"Link: Time Out",0
MIDError_str		dc.b	"Link: MID not supported",0
VARError_str		dc.b	"Link: VAR not supported",0
CIDError_str		dc.b	"Link: Command not supported",0
LFormatError_str	dc.b	"Link: Format",0

			dc.b	0,"start"
StartScript_sym:	dc.b	0,"stdlib"
StdLib_sym		dc.b	0
Confirmation_str	dc.b	"Are you sure ?",10," Write 'yes' to confirm:",0
Dir1_str		dc.b	"Directory of '%s'",10,0
Home_str		dc.b	"home",0
Dir2_str		dc.b	10," %d file(s)",0
LsLong1_str		dc.b	"  NAME   SIZE  FLAG TYPE  Ptr",10,0
LsLong2_str		dc.b	"%8.8s %5.5u %4.4X  %2.2X  %p",10,0
CommandDisp_str		dc.b	"%9.9s ",0
Shell_str		dc.b	10,":>",0
Return_str		dc.b	10,0
MemDisplay_str		dc.b	"Unused SystROM:   %5.5ld",10
			dc.b	"RAM Free      :  %6.6ld",10
			dc.b	"Flash ROM Free: %7.7ld",0
Arg1_str		dc.b	"Arg: filename(s)",0
Arg2_str		dc.b	"Arg: SrcFilename DestFilename",0
FKeyError_str		dc.b	"Arg: Number(1-8) String",0
FastKey_str		dc.b	"F%d: %s",10,0
Failed_str		dc.b	" %s:failed",0
ArgNumber_str		dc.b	"Arg: Number",0
Main_str		dc.b	"main",0
TempFolder_str		dc.b	"%04d",0
SerrNo_str		dc.b	"%08lX %02X ????",0
FloatFormat_str		dc.b	"%f",0
All_str			dc.b	"all",0	
ScriptHeader_str	dc.b	"!PedroM",0
String_str		dc.b	"%s",0

ST_busy_str		dc.b	"BUSY",0
ST_pause_str		dc.b	"STOP",0
ST_batt_str		dc.b	"BATT",0
ST_none_str		dc.b	"           ",0
ST_2nd_str		dc.b	" 2ND   ",0
ST_shift_str		dc.b	"SHIFT ",0
ST_diamond_str		dc.b	" 3RD   ",0
ST_alpha_str		dc.b	"ALPHA",0	

ProductID_str		dc.b	"%02lX-%lX-%lX-%lX",0
LinkProgress_str	dc.b	"Link transmission in progress...",0
HexFormat_str		dc.b	"%06lX: %02X %02X %02X %02X %02X %02X %02X %02X",10,0
Dumping_str		dc.b	"Dumping...",10,0
ByteFormat_str		dc.b	"%02X ",0

ST_Str1	dc.b	"TYPE OR USE left right up down + [ENTER]=OK AND [ESC]=CANCEL",0
ST_Str2	dc.b	"USE up AND down TO OPEN CHOICES",0
ST_Str3	dc.b	"USE left right up down + [ENTER]=OK AND [ESC]=CANCEL",0
ST_Str4	dc.b	"TYPE + [ENTER]=OK AND [ESC]=CANCEL",0
ST_Str5	dc.b	"USE left right up down OR TYPE + [ESC]=CANCEL",0
ST_Str6	dc.b	"USE left right up down + [ENTER]=OK AND [ESC]=CANCEL, OR DRAG",0
ST_Str7	dc.b	"DATA PLACED IN VARIABLE SYSDATA",0
ST_Str8	dc.b	"DATA PLACED IN HOME SCREEN HISTORY",0
ST_Str9	dc.b	"[ENTER]=OK AND [ESC]=CANCEL"
ST_StrA	dc.b	0
ST_StrB	dc.b	"USE left right + [ENTER]=OK AND [ESC]=CANCEL",0
ST_StrC	dc.b	"USE left right + [ENTER]=OK AND [ESC]=CANCEL",0
ST_StrD	dc.b	"USE [2ND] [KEYS] OR [ESC]=CANCEL",0

BT_NONE_str	dc.b  "None",0
BT_OK_str	dc.b  "Enter=OK",0 
BT_SAVE_str	dc.b  "Enter=SAVE",0 
BT_YES_str	dc.b  "Enter=YES",0
BT_CANCEL_str	dc.b  "Esc=CANCEL",0 
BT_NO_str	dc.b  "ESC=NO",0 
BT_GOTO_str	dc.b  "Enter=GOTO",0

AppsDialogTitle	dc.b	"APPLICATIONS & MODE",0
AppsText	dc.b	"Sorry, only one application implemented.",10,"You have to wait the next release.",0
ModeText	dc.b	"Sorry, mode options are not implemented.",10,"You have to wait the next release.",0

HelpKeyTitle	dc.b	"Help Keys",0
	ifnd	TI89
HelpKeysText	dc.b	"Q[?] W[!] E[é] R[@] T[#]",10
		dc.b	"Y[",18,"] U[",252,"] I[",151,"] O[",244,"] P[_]",10
		dc.b	"A[",224,"] S[",223,"] D[",176,"] F[",159,"] G[",128,"]",10
		dc.b	"H[&] J[",190,"] K[|] L[",34,"]",10
		dc.b	"Z[CAPS] X[",169,"] C[",231,"] V[",157,"]",10
		dc.b	"B['] N[~] M[;] ",136,"[:]",0
	endif		
	ifd	TI89
HelpKeysText	dc.b	"=[",157,"]",10
		dc.b	")[",169,"]",10
		dc.b	"/[!]",10
		dc.b	"*[&] ->[@]",0		
	endif
	; LN / EXP / SIN / COS/ TAN / ASIN / ACOS/ATAN / SQRT / INTEGRAL / DERIVATE / Sigma / -1 / ANS
Ln_XRstr	dc.b	"ln(",0
Exp_XRstr	dc.b	"exp(",0
Sin_XRstr	dc.b	"sin(",0
Cos_XRstr	dc.b	"cos(",0
Tan_XRstr	dc.b	"tan(",0
ASin_XRstr	dc.b	"sin",180,"(",0
ACos_XRstr	dc.b	"cos",180,"(",0
ATan_XRstr	dc.b	"tan",180,"(",0
Sqrt_XRstr	dc.b	168,"(",0
Int_XRstr	dc.b	189,"(",0
Der_XRstr	dc.b	188,"(",0
Sigma_XRstr	dc.b	142,"(",0
Inv_XRstr	dc.b	"^-1",0
Ans_XRstr	dc.b	"ans(1)",0


StrError_msg_str:
	dc.b	"undefined errno value",0
	dc.b	"no error",0
	dc.b	"no such file entry",0
	dc.b	"I/O error",0
	dc.b	"not a serial device",0
	dc.b	"out of memory",0
	dc.b	"permission denied",0
	dc.b	"block device required",0
	dc.b	"no such device",0
	dc.b	"invalid argument",0
	dc.b	"file table is full",0
	dc.b	"device directory is full",0
	dc.b	"no space left on device",0
	dc.b	"no more allocation blocks",0
	dc.b	"no more data blocks on device",0
	dc.b	"file is open",0
	dc.b	"no RAM space configured",0
	dc.b	"no heap space configured",0
	dc.b	"seek can't extend read only file",0
	dc.b	"bad file descriptor - file not open",0
	dc.b	"invalid signal number",0
	dc.b	"argument out of range",0
	dc.b	"result out of range",0

	ifd	PRINTF_PACKET_DIALOG
DebugX_str		dc.b	"Send: %04X %04X",10,0
DebugY_str		dc.b	"Recv: %04X %04X",10,0
	endif
	EVEN
	