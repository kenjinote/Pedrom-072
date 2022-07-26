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

;// Global Vars.
;// All these vars may only be accessed by the system.
;//

	NEW_DATA
	
	rs	FloatReg1,FLOAT.sizeof			;// Float register : it is used mainly by internal Floatting Point functions to overides the lack of FPU
	rs	FloatReg2,FLOAT.sizeof			
	rs	FloatReg3,FLOAT.sizeof
	rs	FloatReg4,FLOAT.sizeof
	rs	FloatPreCalculMultTab,FLOAT.sizeof*10	;// Table used by FloatMult/FLoatDivide functions to precalculted the multiplication of a floatr by the finger 0, 1, 2 ... 9.

	rs	FLASH_MAGIC1,4				;// Long: Magic number 1 : if set correctly, it will enable you to use the Flash functions.
	
	rs	CALCULATOR,1				;// Char: Kernel Calculator Version (1 for 92+)
	rs	HW_VERSION,1				;// Char: Hardware Version of the calculator (1 or 2)
	rs	HW_DISPLAY_VERSION,1			;// Void: Obsolte
	rs	EMULATOR,1				;// Bool: Emulator (0 real calc, $FF for emulator).
							;// Theses 4 variables must be in the current order.
							
	; do not add / delete belong this line
	; You can only move an item and change it with a item of the SAME SIZE
	; do not move EV_hook / Folder/Main/FirstWindow/ TEST_pressed / getkeycode
	; (TEST_PRESSED_FLAG - EV_hook) == $176 (GETKEY_CODE follow)
	; (FirstWindow - FolderListHandle) == $18C (MainHandle follow)
	; (HEAP_PTR - ScrRect) = $11A Todo
	; It was develop to keep compatibitlity with AMS 1.01 !
	rs	EV_hook,4				;// Pointer the current EVENT Hook manager (Function).
	
	rs	ERROR_LIST,4				;// Linked Error List root.
	rs	top_estack,4				;// Current position of the EStack (Expression Stack - See other docs).
	rs	VAR_SYSTEM1,4				;// Nothing  (For padding)

	rs	CURRENT_SCREEN,4			;// Target of the Graph functions. Ptr to the Current Screen (a 2D tab).

	rs	FolderListHandle,2			;// Handle of the list of the folders (Directory 'HOME')
	rs	MainHandle,2				;// Handle of the directory 'main'.

	rs	TIMER_TABLE,TIMER_SIZE*TIMER_NUMBER	;// The system timers (APD, CURSOR, LINK, USER, BATT, ...)
		
	rs	CONTRAST_MAX_VALUE,1			;// Max value of the contrast
	rs	CONTRAST_VALUE,1			;// Current value of the contrast [0..CONTRAST_MAX_VALUE]
	
	rs	CURRENT_SIZEX,2				;// Current Horizontal Size of the screen
	rs	CURRENT_SIZEY,2				;// Current Vertical Size of the screen

	rs	CURRENT_FONT,1				;// Default Font when no font is defined. (Small, Medium, Large)
	rs	BATT_LEVEL,1				;// Current level of the Batteries
	
	rs	SHELL_SAVE_Y_POS,2			;// Save the position of the Y positiob before calling any program
	
	rs	CLIP_MIN_X,2				;// Some graph functions use a clip area. Minimum X position of the rect clipping area.
	rs	CLIP_MIN_Y,2				;// Minimum Y position of the rect clipping area.
	rs	CLIP_MAX_X,2				;// Maximum X position of the rect clipping area.
	rs	CLIP_MAX_Y,2				;// Maximum Y position of the rect clipping area.
	rs	CLIP_TEMP_RECT,4			;// Previous value of the clipping area.
	
	rs	WIN_RECT_X1,2				;// MakeWin returns a global ptr to a WIN struct 
	rs	WIN_RECT_Y1,2				;// It is &WIN_RECT_X1
	rs	WIN_RECT_X2,2
	rs	WIN_RECT_Y2,2

	rs	SYMFIND_HSYM,4				;// HSYM: SymFindFirst/Next/Prev uses this to know what it is the current folder/file 
	rs	ScrRectRam,4				;// Screen Rect of the Screen (Needed in RAM  because of the support of Kernel Programs V2).
	
	rs	DRAW_CHAR,1				;// DrawChar uses it to display one char.
	rs	NULL_CHAR,1				;// Null char for draw char (It uses DrawStr) and cur_folder_str !
	rs	CUR_FOLDER_STR,10			;// String contains the name of the current folder.
	rs	FOLDER_TEMP,20				;// Temp usage for Sym functions (Dispatching file and folder).
	rs	SYMFIND_FLAGS,2				;// SymFindFirst/Next/Prev current flag of searching
	rs	TEMP_FOLDER_COUNT,2			;// Current value of the Temporary folder.
	
	rs	ARGC,2					;// Argument counter (Just like int main(int argc, char *argv[]);
	rs	ARGV,4*ARG_MAX				;// Argument Ptr
							;// Ti Link Protocol Packet
	rs	PACKET_MID,1				;// Machine Identifier
	rs	PACKET_CID,1				;// Command Identifier
	rs	PACKET_LEN,2				;// Length of the Packet
	rs	PACKET_HANDLE,2				;// Handle of the extra data
	rs	PACKET_CHECKSUM,2			;// Checksum (Used only by internal SendPacket/ReadPacket functions).
	rs	PACKET_LAST_CID,2			;// Last sent CID
	rs	PACKET_LAST_LEN,2			;// Last sent Len of Extra Data
	rs	PACKET_LAST_PTR,4			;// Ptr to the last sent Extra Data (Used by ReadPacket when it receives a CID=RESEND last packet).
	
	rs	KEY_MASK,10				;// Image of the Keyboard Matrix. Each bit set to 0 means that the corresponding key is pushed.
	rs	KEY_CUR_ROW,2				;// The current Key which is pressed is determined by its row
	rs	KEY_CUR_COL,2				;// and its collum in the Keyboard Matrix.
	rs	KEY_PREVIOUS,2				;// Last ASCII code Key (Used by Auto-Repeat feature).
	rs	KEY_CPT,2				;// Counter until repeat the current pressed Key
	
	rs	KEY_ORG_START_CPT,2			;// Original Key counter
	rs	KEY_ORG_REPEAT_CPT,2			;// After a key is pressed and repeated once, start value of the counter.
	
	rs	KEY_CUR_POS,2				;// Index of the max key in the Key Buffer
	
	rs	KEY_STATUS,2				;// Key Statut (2nd, shift, alpha, diamond).
	rs	KEY_MAJ,1				;// Bool: MAJUSCULE ? (Bool)
	rs	HELP_BEING_DISPLAYED,1			;// Bool: Does a help is currently displayed ?

	rs	KBD_QUEUE,6				;// TEST PRESSED FLAG is KBD_QUEUE.used ! GETKEY_CODE is KBD_QUEUE.data
	rs	TEST_PRESSED_FLAG,2			;// SBool: Does a key is in the Keyboard Buffer ?
	rs	GETKEY_CODE,2*KEY_MAX			;// Keyboard Buffer ?

	rs	FirstWindow,4				;// Linked Window List (I MUST ADD DeskTop just before !)
	rs	DeskTop,4				;// Window Ptr to the DeskTop (A window Struct)
	rs	HEAP_PTR,4				;// Ptr to Heap Table. RAM_CALL needs a RAM ptr to a ptr, and not a ptr. And many programs acceds using like (RAM_CALL).w so it won't work inside the rom.
	; End of do not add / delete belong this line

	rs	CURRENT_ATTR,2				;// Default Attribute (A_NORMAL, A_REVERSE, A_XOR)
	rs	CURRENT_POINT_X,2			;// Current Pen X location 
	rs	CURRENT_POINT_Y,2			;// Current Pen Y location 
	rs	CURRENT_INCY,2				;// Internal: Current increment of a row to go to the next row in bytes.
	rs	CURRENT_GRAPH_UNALIGNED,1		;// Bool: Can we use the fast (word) versions or the slow (byte) versions ?
	
	rs	CURSOR_STATE,1				;// Bool: Does the cursor is allowed to be displayed ?
	rs	PRINTF_LINE_COUNTER,2			;// Internal(used by Printf): Internal counter to count the number of lines until a pause is automaticlely set.

	rs	_tt_InBuffer,4				;// Ptr to Entry Buffer (for Unpack routines)
	rs	_tt_InMask,2				;// Mask to get the next bit (for Unpack routines)
	
	rs	CURRENT_WINDOW,4			;// Internal Ptr to the "current" window (Only valid for window functions). Used mainly to displayed easilly in both the LCD_MEM and in the Duplicate Screen of the window
	rs	WIN_RECT,8				;// Temp usage
	
	rs	DeskTopWindow,60			;// DeskTop Window Struct (See Ptr before ;)) 
	
	rs	FLASH_MAGIC2,4				;// Magic Flash Number 2 (See Number 1)
	
	rs	ENABLE_BREAK_KEY,1			;// Bool: Does the break key is enable ?
	rs	BREAK_KEY,1				;// Bool: Has the break key been pushed ?

	rs	LINK_RECEIVE_QUEUE,LINK_QUEUE.sizeof+8	;// Queue for the recieved bytes by the link 
	rs	LINK_SEND_QUEUE,LINK_QUEUE.sizeof+8	;// Queue for the send bytes by the link 
	rs	LINK_RECEIVE_OVERFLOW,1			;// Bool: Does an overflow occured when receiving bytes ?
	rs	LINK_RESET,1				;// Bool: Does the Auto-Link int has reseted ?
	rs	LINK_INT_SAVED_MASK,2			;// Temp save of the current value of the register SR 
	
	rs	CLIPBOARD_HANDLE,2			;// Handle of the ClipBoard
	rs	CLIPBOARD_LEN,4				;// Length of the ClipBoard.
	
	rs	start_estack,4				;// Start of the EStack (~ bottow_estack)
	rs	end_estack,4				;// End of the EStack
	rs	error_estack,4				;// Error position of the EStack
	
	rs	EV_CurrentMenu,4			;// Ptr to a menu struct which is the current used menu
	rs	EV_handler,4				;// Ptr to the EVENT Handler
	rs	EV_RunningAppId,2			;// Id of the Running Application
	rs	EV_CurrentAppId,2			;// Id of the Current Application
	rs	EV_PaintingEnable,1			;// Bool: Does painting is enable ?
	rs	RUN_START_SCRIPT,1			;// Run the start script ?
	rs	EV_globalERD,2				;// Current Globale ERD (Error messsage) used by EV_eventLoop
	rs	EV_globalPasteString,4			;// Internally used by EV_eventLoop to pushed a string ptr.
	rs	EV_customHandle,2			;// Custom Handle
	
	rs	HANDLE_KEEP_TABLE,4			;// Handle Keep Table
	rs	RETURN_VALUE_ADDR,4			;// RETURN_VALUE
	rs	FirstRun,0				;// PreOs var: Global Ptr
	rs	RelocStackList,4			;// PreOs var: De la relocation & error string ptr
	rs	ErrorString,4				;// PreOs var: Program Ref
	rs	KERNEL_JUMP_TO,4			;// LibsCall / LibsExec : instead of self modifying code, use global vars
	rs	KERNEL_SAVE_LIB,4			;// LibsCall / LibsExec : instead of self modifying code, use global vars
	rs	KERNEL_RETURN_ADDR,4			;// LibsCall / LibsExec : instead of self modifying code, use global vars
	rs	KERNEL_EXTRA_STR,10			;// Extra string to display in case of reloc error
	rs	KERNEL_EXTRA_NUMBER,2			;// Extra number to display in case of reloc error
		
	rs	SHELL_HISTORY_TAB,SHELL_HISTORY*(SHELL_MAX_LINE+2)
	rs	SHELL_FAST_KEY,8*(SHELL_MAX_LINE+2)
	rs	SHELL_PATH,SHELL_MAX_PATH
	
	rs	PULLDOWN_PTR,4				;// Pulldown (Dialog) global ptr.
	rs	STRTOK_PTR,4
	
	; Side vars
	rs	filename,4
	rs	file_ptr,4
	rs	end_ptr,4
	rs	page_ptr,4
	rs	curs_ptr,4
	rs	gap_ptr,4
	rs	exec_name,28
	rs	exec_name2,28
	rs	line,2
	rs	select,2
	rs	penCol,2
	rs	penRow,2
	rs	penColor,2
	rs	curCol,2
	rs	curRow,2
	rs	key_delay,2
	rs	key_rate,2
	rs	disp_indic,2;
	rs	text2_name,28
	rs	text2_pos,2
	rs	clipboard,2
	rs	cursor_on,1
	rs	modified,1
	rs	refresh,1
	rs	auto_code,1
	rs	auto_indent,1
	rs	CURSOR_PHASE,1
	rs	find_str,32
	rs	replace_str,32

	rs	EXEC_RAM,60				;// Some place in RAM to push some code which has to be executed in RAM.
	
	rs	HEAP_TABLE,HANDLE_MAX*4			;// Table of the Heap Address Ptr
	
	rs	HEAP_START,0				;// Start of the Heap

HEAP_END	EQU	256*1024			;// End of the Heap
	
	; Check
	; (TEST_PRESSED_FLAG - EV_hook) == $176 (GETKEY_CODE follow)
	; (FirstWindow - FolderListHandle) == $18C (MainHandle follow)
	; (HEAP_PTR - ScrRect) = $11A
;	ifne	(TEST_PRESSED_FLAG - EV_hook - $176)
;		#error	"TEST_PRESSED_FLAG & EV_hook are not good"
;	endif
;	ifne	(FirstWindow - FolderListHandle - $18C)
;		#error	"FirstWindow & FolderListHandle are not good"
;	endif
