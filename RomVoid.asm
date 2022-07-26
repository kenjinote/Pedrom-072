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

;FirstWindow	;	$0
;WinActivate	;	$1
;WinAttr	;	$2
;WinBackupToScr	;	$3
;WinBackground	;	$4
;WinBegin	;	$5
;WinBitmapGet	;	$6
;WinBitmapPut	;	$7
;WinBitmapSize	;	$8
;WinCharXY	;	$9
;WinChar	;	$a
;WinClose	;	$b
;WinClr	;	$c
;WinDeactivate	;	$d
;WinDupStat	;	$e
;WinEllipse	;	$f
;WinFill	;	$10
;WinFillLines2	;	$11
;WinFillTriangle	;	$12
;WinFont	;	$13
;WinGetCursor	;	$14
;WinHide	;	$15
;WinHome	;	$16
;WinLine	;	$17
;WinLineNC	;	$18
;WinLineTo	;	$19
;WinLineRel	;	$1a
;WinMoveCursor	;	$1b
;WinMoveTo	;	$1c
;WinMoveRel	;	$1d
;WinOpen	;	$1e
;WinPixGet	;	$1f
;WinPixSet	;	$20
;WinRect	;	$21
;WinReOpen	;	$22
;WinScrollH	;	$23
;WinScrollV	;	$24
;WinStr	;	$25
;WinStrXY	;	$26
;DrawWinBorder	;	$27
ScrRectDivide	;	$28
;RectWinToWin	;	$29
;RectWinToScr	;	$2a
UpdateWindows	;	$2b

;MakeWinRect	;	$2c
;ScrToWin	;	$2d
;ScrToHome	;	$2e
;ScrRect	;	$2f

Dialog	;	$30
;NoCallBack	;	$31
;DialogDo	;	$32
;DialogAdd	;	$33
;DialogNew	;	$34

;DrawStaticButton	;	$35

;MenuBegin	;	$36
MenuCheck	;	$37
;MenuEnd	;	$38
;MenuKey	;	$39
;MenuOn	;	$3a
MenuPopup	;	$3b
;MenuSubStat	;	$3c
;MenuTopStat	;	$3d
MenuTopSelect	;	$3e
MenuTopRedef	;	$3f
MenuGetTopRedef	;	$40
;MenuAddText	;	$41
;MenuAddIcon	;	$42
;MenuNew	;	$43

;PopupAddText	;	$44
;PopupNew	;	$45
;PopupClear	;	$46
;PopupDo	;	$47
;PopupText	;	$48

;MenuUpdate	;	$49

Parse2DExpr	;	$4a
Parse2DMultiExpr	;	$4b
Print2DExpr	;	$4c
Parms2D	;	$4d

;display_statements	;	$4e
;Parse1DExpr	;	$4f

;pushkey	;	$50
;ngetchx	;	$51
;kbhit	;	$52

;sprintf	;	$53

;getcalc	;	$54
;sendcalc	;	$55
LIO_Send	;	$56
LIO_Get	;	$57
LIO_Receive	;	$58
LIO_GetMultiple	;	$59
;LIO_SendData	;	$5a
;LIO_RecvData	;	$5b

;SymAdd	;	$5c
;SymAddMain	;	$5d
;SymDel	;	$5e
;HSymDel	;	$5f
;SymFind	;	$60
;SymFindMain	;	$61
;SymFindHome	;	$62
;SymMove	;	$63
;FolderAdd	;	$64
;FolderCur	;	$65
;FolderDel	;	$66
;FolderFind	;	$67
;FolderGetCur	;	$68
;FolderOp	;	$69
;FolderRename	;	$6a
;FolderCount	;	$6b
;SymFindFirst	;	$6c
;SymFindNext	;	$6d
;SymFindPrev	;	$6e
;SymFindFoldername	;	$6f
;AddSymToFolder	;	$70
;FindSymInFolder	;	$71
;FolderCurTemp	;	$72
;FolderAddTemp	;	$73
;FolderDelTemp	;	$74
;FolderDelAllTemp	;	$75
;TempFolderName	;	$76
;IsMainFolderStr	;	$77
ParseSymName	;	$78
;DerefSym	;	$79
;HSYMtoName	;	$7a
;StrToTokN	;	$7b
;TokToStrN	;	$7c

CheckGraphRef	;	$7d
ClearUserDef	;	$7e
;CheckLinkLockFlag	;	$7f
;TokenizeSymName	;	$80

;SymCmp	;	$81
;SymCpy	;	$82
;SymCpy0	;	$83
;ValidateSymName	;	$84

;VarRecall	;	$85
;VarStore	;	$86
VarStoreLink	;	$87
;QSysProtected	;	$88
;CheckSysFunc	;	$89
GetSysGraphRef	;	$8a
;CheckReservedName	;	$8b
;SymSysVar	;	$8c
ValidateStore	;	$8d
;ResetSymFlags	;	$8e

;HeapAvail	;	$8f		;
;HeapAlloc	;	$90		;
;HeapAllocESTACK	;	$91	; !!
;HeapAllocHigh	;	$92	;
;HeapAllocThrow	;	$93	;
;HeapAllocHighThrow	;	$94	;
;HeapCompress	;	$95	;
;HeapDeref	;	$96		;
;HeapFree	;	$97		;
;HeapFreeIndir	;	$98	;
;HLock	;	$99		;
;HeapLock	;	$9a		;
;HeapGetLock	;	$9b		;
;HeapMax	;	$9c		;
;HeapRealloc	;	$9d		;
;HeapSize	;	$9e		;
;HeapUnlock	;	$9f		;
;HeapMoveHigh	;	$a0	;
;HeapEnd	;	$a1		; !!
;HeapAllocPtr	;	$a2	;
;HeapFreePtr	;	$a3		;
;NeedStack	;	$a4		; !!

TE_close	;	$a5
TE_checkSlack	;	$a6
TE_empty	;	$a7
TE_focus	;	$a8
TE_handleEvent	;	$a9
TE_indicateReadOnly	;	$aa
TE_isBlank	;	$ab
TE_open	;	$ac
TE_openFixed	;	$ad
TE_pasteText	;	$ae
TE_reopen	;	$af
TE_reopenPlain	;	$b0
TE_select	;	$b1
TE_shrinkWrap	;	$b2
TE_unfocus	;	$b3
TE_updateCommand	;	$b4

_bcd_math	;	$b5
;bcdadd	;	$b6
;bcdsub	;	$b7
;bcdmul	;	$b8
;bcddiv	;	$b9
;bcdneg	;	$ba
;bcdcmp	;	$bb
;bcdlong	;	$bc
;bcdbcd	;	$bd

;EX_getArg	;	$be
;EX_getBCD	;	$bf
;EX_stoBCD	;	$c0

;CB_replaceTEXT	;	$c1
;CB_fetchTEXT	;	$c2

;CU_restore	;	$c3
;CU_start	;	$c4
;CU_stop	;	$c5

;EV_captureEvents	;	$c6
;EV_clearPasteString	;	$c7
;EV_getc	;	$c8
;EV_getSplitRect	;	$c9
;EV_notifySwitchGraph	;	$ca
;EV_paintOneWindow	;	$cb
;EV_paintWindows	;	$cc
;EV_restorePainting	;	$cd
;EV_sendEvent	;	$ce
;EV_sendEventSide	;	$cf
;EV_sendString	;	$d0
EV_setCmdCheck	;	$d1
EV_setCmdState	;	$d2
EV_setFKeyState	;	$d3
;EV_startApp	;	$d4
;EV_startSide	;	$d5
;EV_startTask	;	$d6
;EV_suspendPainting	;	$d7
;EV_switch	;	$d8

;MO_currentOptions	;	$d9
;MO_defaults	;	$da
;MO_digestOptions	;	$db
;MO_isMultigraphTask	;	$dc
;MO_modeDialog	;	$dd
;MO_notifyModeChange	;	$de
;MO_sendQuit	;	$df

;ST_angle	;	$e0
;ST_batt	;	$e1
;ST_busy	;	$e2
;ST_eraseHelp	;	$e3
;ST_folder	;	$e4
;ST_graph	;	$e5
;ST_helpMsg	;	$e6
;ST_modKey	;	$e7
;ST_precision	;	$e8
;ST_readOnly	;	$e9
;ST_stack	;	$ea
;ST_refDsp	;	$eb

;OSCheckBreak	;	$ec
;OSClearBreak	;	$ed
;OSEnableBreak	;	$ee
;OSDisableBreak	;	$ef

;OSRegisterTimer	;	$f0
;OSFreeTimer	;	$f1
;OSTimerCurVal	;	$f2
;OSTimerExpired	;	$f3
;OSTimerRestart	;	$f4

;acos	;	$f5
;asin	;	$f6
;atan	;	$f7
atan2	;	$f8
;cos	;	$f9
;sin	;	$fa
;tan	;	$fb
;cosh	;	$fc
;sinh	;	$fd
;tanh	;	$fe
;exp	;	$ff
;log	;	$100
;log10	;	$101
modf	;	$102
;pow	;	$103
;sqrt	;	$104
;ceil	;	$105
;fabs	;	$106
;floor	;	$107
;fmod	;	$108

;top_estack	;	$109
;next_expression_index	;	$10a

gr_active	;	$10b
gr_other	;	$10c

ABT_dialog	;	$10d

;HomeExecute	;	$10e
;HomePushEStack	;	$10f

SP_Define	;	$110

store_data_var	;	$111
recall_data_var	;	$112

CharNumber	;	$113
spike_optionD	;	$114
spike_geo_titles	;	$115
spike_in_editor	;	$116
dv_create_graph_titles	;	$117
spike_titles_in_editor	;	$118
dv_findColumn	;	$119
spike_chk_gr_dirty	;	$11a
GetStatValue	;	$11b

partial_len	;	$11c
paint_all_except	;	$11d
equ_select	;	$11e
equ_setStyle	;	$11f
equ_getNameInfo	;	$120
;checkCurrent	;	$121

BN_power17Mod	;	$122
BN_powerMod	;	$123
BN_prodMod	;	$124

CAT_dialog	;	$125

caddcert	;	$126
cdecrypt	;	$127
;ceof	;	$128
cfindcertfield	;	$129
;cfindfield	;	$12a
;cgetc	;	$12b
cgetcert	;	$12c
;cgetflen	;	$12d
;cgetfnl	;	$12e
;cgetnl	;	$12f
;cgetns	;	$130
cgetvernum	;	$131
;copen	;	$132
;copensub	;	$133
;cputhdr	;	$134
;cputnl	;	$135
;cputns	;	$136
;cread	;	$137
;ctell	;	$138
;cwrite	;	$139

cacos	;	$13a
casin	;	$13b
catan	;	$13c
cacosh	;	$13d
casinh	;	$13e
catanh	;	$13f
ccos	;	$140
csin	;	$141
ctan	;	$142
ccosh	;	$143
csinh	;	$144
ctanh	;	$145
csqrt	;	$146
cln	;	$147
clog10	;	$148
cexp	;	$149

CustomBegin	;	$14a
CustomMenuItem	;	$14b
CustomEnd	;	$14c

ReallocExprStruct	;	$14d
SearchExprStruct	;	$14e
handleRclKey	;	$14f
CustomFree	;	$150

;ERD_dialog	;	$151
;ERD_process	;	$152

;ER_throwVar	;	$153
;ER_catch	;	$154
;ER_success	;	$155

;EV_centralDispatcher	;	$156
;EV_defaultHandler	;	$157
;EV_eventLoop	;	$158
;EV_registerMenu	;	$159

;EX_patch	;	$15a

;EM_abandon	;	$15b
EM_blockErase	;	$15c
;EM_blockVerifyErase	;	$15d
EM_delete	;	$15e
;EM_findEmptySlot	;	$15f
;EM_GC	;	$160
;EM_moveSymFromExtMem	;	$161
;EM_moveSymToExtMem	;	$162
;EM_open	;	$163
;EM_put	;	$164
;EM_survey	;	$165
;EM_twinSymFromExtMem	;	$166
;EM_write	;	$167
EM_writeToExtMem	;	$168

FL_addCert	;	$169
;FL_download	;	$16a
;FL_getHardwareParmBlock	;	$16b
FL_getCert	;	$16c
;FL_getVerNum	;	$16d

equ_deStatus	;	$16e
;cmpstri	;	$16f
fix_loop_displacements	;	$170

;FL_write	;	$171

;fpisanint	;	$172
;fpisodd	;	$173
;round12	;	$174
;round14	;	$175

GD_Circle	;	$176
GD_Line	;	$177
GD_HVLine	;	$178
GD_Pen	;	$179
GD_Eraser	;	$17a
GD_Text	;	$17b
GD_Select	;	$17c
GD_Contour	;	$17d

;GKeyIn	;	$17e
;GKeyDown	;	$17f
;GKeyFlush	;	$180

;HelpKeys	;	$181
;QModeKey	;	$182
;QSysKey	;	$183
;WordInList	;	$184

;BitmapGet	;	$185
;BitmapInit	;	$186
;BitmapPut	;	$187
;BitmapSize	;	$188

;ScrRectFill	;	$189
;ScrRectOverlap	;	$18a
;ScrRectScroll	;	$18b
;ScrRectShift	;	$18c
;QScrRectOverlap	;	$18d
;FontGetSys	;	$18e
;FontSetSys	;	$18f
;FontCharWidth	;	$190
;DrawClipChar	;	$191
;DrawClipEllipse	;	$192
;DrawClipLine	;	$193
;DrawClipPix	;	$194
;DrawClipRect	;	$195
;DrawMultiLines	;	$196
;DrawStrWidth	;	$197
;FillTriangle	;	$198
;FillLines2	;	$199
;SetCurAttr	;	$19a
;SetCurClip	;	$19b
;LineTo	;	$19c
;MoveTo	;	$19d
;ScreenClear	;	$19e
;GetPix	;	$19f
;SaveScrState	;	$1a0
;RestoreScrState	;	$1a1
;PortSet	;	$1a2
;PortRestore	;	$1a3
;DrawChar	;	$1a4
;DrawFkey	;	$1a5
;DrawIcon	;	$1a6
;DrawLine	;	$1a7
;DrawPix	;	$1a8
;DrawStr	;	$1a9

GM_Value	;	$1aa
GM_Intersect	;	$1ab
GM_Integrate	;	$1ac
GM_Inflection	;	$1ad
GM_TanLine	;	$1ae
GM_Math1	;	$1af
GM_Derivative	;	$1b0
GM_DistArc	;	$1b1
GM_Shade	;	$1b2
YCvtFtoWin	;	$1b3

;DlgMessage	;	$1b4

SetGraphMode	;	$1b5
Regraph	;	$1b6
GrAxes	;	$1b7
gr_xres_pixel	;	$1b8
CptFuncX	;	$1b9
XCvtPtoF	;	$1ba
YCvtPtoF	;	$1bb
YCvtFtoP	;	$1bc
XCvtFtoP	;	$1bd
GrLineFlt	;	$1be
FuncLineFlt	;	$1bf
GrClipLine	;	$1c0
CptDeltax	;	$1c1
CptDeltay	;	$1c2
CkValidDelta	;	$1c3
GR_Pan	;	$1c4
FindFunc	;	$1c5
FindGrFunc	;	$1c6
grFuncName	;	$1c7
gr_initCondName	;	$1c8
CptIndep	;	$1c9
gr_CptIndepInc	;	$1ca
gr_del_locals	;	$1cb
gr_DelFolder	;	$1cc
gr_openFolder	;	$1cd
setup_more_graph_fun	;	$1ce
unlock_more_graph_fun	;	$1cf
execute_graph_func	;	$1d0
cpt_gr_fun	;	$1d1
cpt_gr_param	;	$1d2
cpt_gr_polar	;	$1d3
gr_execute_seq	;	$1d4
CountGrFunc	;	$1d5
FirstSeqPlot	;	$1d6
cleanup_seq_mem	;	$1d7
time_loop	;	$1d8
InitTimeSeq	;	$1d9
seqWebInit	;	$1da
run_one_seq	;	$1db
gr_seq_value	;	$1dc
StepCk	;	$1dd
seqStepCk	;	$1de
rngLen	;	$1df
gdb_len	;	$1e0
gdb_store	;	$1e1
gdb_recall	;	$1e2
gr_DispLabels	;	$1e3
GraphOrTableCmd	;	$1e4
ck_valid_float	;	$1e5
CreateEmptyList	;	$1e6
QSkipGraphErr	;	$1e7
gr_find_de_result	;	$1e8
InitDEAxesRng	;	$1e9
InitDEMem	;	$1ea
de_loop	;	$1eb
cleanup_de_mem	;	$1ec
gr_de_value	;	$1ed
gr_find_func_index	;	$1ee
CptLastIndepDE	;	$1ef
de_initRes	;	$1f0
gr_del_vars_in_folder	;	$1f1
gr_de_axes_lbl	;	$1f2
gr_execute_de	;	$1f3
gr_delete_fldpic	;	$1f4
gr_remove_fldpic	;	$1f5
gr_add_fldpic	;	$1f6
gr_stopic	;	$1f7
gr_find_el	;	$1f8
deStepCk	;	$1f9
gr_ck_solvergraph	;	$1fa
GR3_addContours	;	$1fb
GraphActivate	;	$1fc
GR3_freeDB	;	$1fd
GR3_handleEvent	;	$1fe
GR3_paint3d	;	$1ff
GR3_xyToWindow	;	$200
GS_PlotTrace	;	$201
GS_PlotAll	;	$202

PlotDel	;	$203
PlotPut	;	$204
PlotGet	;	$205
PlotInit	;	$206
PlotDup	;	$207
PlotSize	;	$208
PlotLookup	;	$209
QActivePlots	;	$20a
QPlotActive	;	$20b

GT_BackupToScr	;	$20c
GT_CalcDepVals	;	$20d
GT_CenterGraphCursor	;	$20e
GT_CursorKey	;	$20f
GT_DspFreeTraceCoords	;	$210
GT_DspTraceCoords	;	$211
GT_DspMsg	;	$212
GT_Error	;	$213
GT_Format	;	$214
GT_FreeTrace	;	$215
GT_IncXY	;	$216
GT_KeyIn	;	$217
GT_QFloatCursorsInRange	;	$218
GT_Regraph	;	$219
GT_Regraph_if_neccy	;	$21a
GT_Open	;	$21b
GT_SaveAs	;	$21c
GT_SelFunc	;	$21d
GT_SetGraphRange	;	$21e
GT_SetCursorXY	;	$21f
GT_ShowMarkers	;	$220
GT_Trace	;	$221
GT_ValidGraphRanges	;	$222
GT_WinBound	;	$223
GT_WinCursor	;	$224
GYcoord	;	$225
GXcoord	;	$226
;round12_err	;	$227
GT_Set_Graph_Format	;	$228
GT_PrintCursor	;	$229
GT_DE_Init_Conds	;	$22a

GZ_Box	;	$22b
GZ_Center	;	$22c
GZ_Decimal	;	$22d
GZ_Fit	;	$22e
GZ_InOut	;	$22f
GZ_Integer	;	$230
GZ_Previous	;	$231
GZ_Recall	;	$232
GZ_SetFactors	;	$233
GZ_Square	;	$234
GZ_Standard	;	$235
GZ_Stat	;	$236
GZ_Store	;	$237
GZ_Trig	;	$238

;HeapGetHandle	;	$239
;HeapPtrToHandle	;	$23a
;FreeHandles	;	$23b

;HS_chopFIFO	;	$23c
;HS_countFIFO	;	$23d
;HS_deleteFIFONode	;	$23e
;HS_freeAll	;	$23f
;HS_freeFIFONode	;	$240
;HS_getAns	;	$241
;HS_getEntry	;	$242
;HS_getFIFONode	;	$243
;HS_popEStack	;	$244
;HS_newFIFONode	;	$245
;HS_pushFIFONode	;	$246

;HToESI	;	$247

;OSInitKeyInitDelay	;	$248
;OSInitBetweenKeyDelay	;	$249

;OSCheckSilentLink	;	$24a
;OSLinkCmd	;	$24b
;OSLinkReset	;	$24c
;OSLinkOpen	;	$24d
;OSLinkClose	;	$24e
;OSReadLinkBlock	;	$24f
;OSWriteLinkBlock	;	$250
;OSLinkTxQueueInquire	;	$251
;OSLinkTxQueueActive	;	$252
LIO_SendProduct	;	$253

;MD5Init	;	$254
;MD5Update	;	$255
;MD5Final	;	$256
;MD5Done	;	$257

convert_to_TI_92	;	$258
gen_version	;	$259
is_executable	;	$25a

;NG_RPNToText	;	$25b
;NG_approxESI	;	$25c
;NG_execute	;	$25d
;NG_graphESI	;	$25e
;NG_rationalESI	;	$25f
;NG_tokenize	;	$260
NG_setup_graph_fun	;	$261
NG_cleanup_graph_fun	;	$262

;push_END_TAG	;	$263
;push_LIST_TAG	;	$264
tokenize_if_TI_92_or_text	;	$265

;setjmp	;	$266
;longjmp	;	$267

VarGraphRefBitsClear	;	$268
VarInit	;	$269

;memcpy	;	$26a
;memmove	;	$26b
;strcpy	;	$26c
;strncpy	;	$26d
;strcat	;	$26e
;strncat	;	$26f
;memcmp	;	$270
;strcmp	;	$271
;strncmp	;	$272
;memchr	;	$273
;strchr	;	$274
;strcspn	;	$275
;strpbrk	;	$276
;strrchr	;	$277
;strspn	;	$278
;strstr	;	$279
;strtok	;	$27a
;_memset	;	$27b
;memset	;	$27c
;strerror	;	$27d
;strlen	;	$27e

;SymAddTwin	;	$27f
;SymDelTwin	;	$280
LoadSymFromFindHandle	;	$281
;MakeHsym	;	$282
;SymFindPtr	;	$283

;OSVRegisterTimer	;	$284
;OSVFreeTimer	;	$285

sincos	;	$286
;asinh	;	$287
;acosh	;	$288
;atanh	;	$289
itrig	;	$28a
trig	;	$28b

VarOpen	;	$28c
VarSaveAs	;	$28d
VarNew	;	$28e
;VarCreateFolderPopup	;	$28f
VarSaveTitle	;	$290

;WinWidth	;	$291
;WinHeight	;	$292

;XR_stringPtr	;	$293
;OSReset	;	$294
SumStoChkMem	;	$295
;OSContrastUp	;	$296
;OSContrastDn	;	$297
OSKeyScan	;	$298
OSGetStatKeys	;	$299

;off	;	$29a
;idle	;	$29b
;OSSetSR	;	$29c

;AB_prodid	;	$29d
;AB_prodname	;	$29e
;AB_serno	;	$29f

cgetcertrevno	;	$2a0
;cgetsn	;	$2a1
de_rng_no_graph	;	$2a2
;EV_hook	;	$2a3

;_ds16u16	;	$2a4
;_ms16u16	;	$2a5
;_du16u16	;	$2a6
;_mu16u16	;	$2a7
;_ds32s32	;	$2a8
;_ms32s32	;	$2a9
;_du32u32	;	$2aa
;_mu32u32	;	$2ab

; All following rom calls are not accessible on TI92+ 1.00
;assign_between	;	$2ac
did_push_var_val	;	$2ad
does_push_fetch	;	$2ae
delete_list_element	;	$2af
push_ans_entry	;	$2b0
index_after_match_endtag	;	$2b1
push_indir_name	;	$2b2
push_user_func	;	$2b3
store_func_def	;	$2b4
store_to_subscripted_element	;	$2b5
index_below_display_expression_aux	;	$2b6
get_key_ptr	;	$2b7
get_list_indices	;	$2b8
get_matrix_indices	;	$2b9
init_list_indices	;	$2ba
init_matrix_indices	;	$2bb
push_float_qr_fact	;	$2bc
push_lu_fact	;	$2bd
push_symbolic_qr_fact	;	$2be
;are_expressions_identical	;	$2bf
compare_expressions	;	$2c0
;find_error_message	;	$2c1
;check_estack_size	;	$2c2
;delete_between	;	$2c3
;deleted_between	;	$2c4
;delete_expression	;	$2c5
;deleted_expression	;	$2c6
;estack_to_short	;	$2c7
;estack_to_ushort	;	$2c8
factor_base_index	;	$2c9
factor_exponent_index	;	$2ca
;GetValue	;	$2cb
im_index	;	$2cc
index_numeric_term	;	$2cd
index_of_lead_base_of_lead_term	;	$2ce
index_main_var	;	$2cf
is_advanced_tag	;	$2d0
is_antisymmetric	;	$2d1
is_complex_number	;	$2d2
is_complex0	;	$2d3
is_free_of_tag	;	$2d4
is_independent_of	;	$2d5
is_independent_of_de_seq_vars	;	$2d6
is_independent_of_tail	;	$2d7
is_independent_of_elements	;	$2d8
is_monomial	;	$2d9
is_monomial_in_kernel	;	$2da
is_narrowly_independent_of	;	$2db
is_symmetric	;	$2dc
is_tail_independent_of	;	$2dd
lead_base_index	;	$2de
lead_exponent_index	;	$2df
lead_factor_index	;	$2e0
lead_term_index	;	$2e1
main_gen_var_index	;	$2e2
map_unary_over_comparison	;	$2e3
;min_quantum	;	$2e4
;move_between_to_top	;	$2e5
;moved_between_to_top	;	$2e6
numeric_factor_index	;	$2e7
;push_between	;	$2e8		; push_between(end_EXPR *, end_EXPR*)
;push_expr_quantum	;	$2e9
;push_expr2_quantum	;	$2ea
push_next_arb_int	;	$2eb
push_next_arb_real	;	$2ec
push_next_internal_var	;	$2ed
;push_quantum	;	$2ee
;push_quantum_pair	;	$2ef
reductum_index	;	$2f0
remaining_factors_index	;	$2f1
re_index	;	$2f2
;reset_estack_size	;	$2f3
;reset_control_flags	;	$2f4
can_be_approxed	;	$2f5
compare_complex_magnitudes	;	$2f6
compare_Floats	;	$2f7
did_push_cnvrt_Float_to_integer	;	$2f8
;estack_number_to_Float	;	$2f9
;float_class	;	$2fa
;frexp10	;	$2fb
gcd_exact_whole_Floats	;	$2fc
;init_float	;	$2fd
is_Float_exact_whole_number	;	$2fe
;is_float_infinity	;	$2ff
;is_float_negative_zero	;	$300
;is_float_positive_zero	;	$301
;is_float_signed_infinity	;	$302
;is_float_transfinite	;	$303
;is_float_unsigned_inf_or_nan	;	$304
;is_float_unsigned_zero	;	$305
;is_nan	;	$306
likely_approx_to_complex_number	;	$307
likely_approx_to_number	;	$308
norm1_complex_Float	;	$309
;push_Float	;	$30a
push_Float_to_nonneg_int	;	$30b
push_Float_to_rat	;	$30c
push_cnvrt_integer_if_whole_nmb	;	$30d
push_overflow_to_infinity	;	$30e
push_pow	;	$30f
push_round_Float	;	$310
should_and_did_push_approx_arg2	;	$311
signum_Float	;	$312
did_push_to_polar	;	$313
push_degrees	;	$314
push_format	;	$315
push_getkey	;	$316
push_getfold	;	$317
push_getmode	;	$318
push_gettype	;	$319
push_instring	;	$31a
push_mrow_aux	;	$31b
push_part	;	$31c
push_pttest	;	$31d
push_pxltest	;	$31e
push_rand	;	$31f
push_randpoly	;	$320
push_setfold	;	$321
push_setgraph	;	$322
push_setmode	;	$323
push_settable	;	$324
push_str_to_expr	;	$325
;push_string	;	$326
push_switch	;	$327
push_to_cylin	;	$328
push_to_sphere	;	$329

cmd_andpic	;	$32a
cmd_blddata	;	$32b
cmd_circle	;	$32c
cmd_clrdraw	;	$32d
cmd_clrerr	;	$32e
cmd_clrgraph	;	$32f
cmd_clrhome	;	$330
cmd_clrio	;	$331
cmd_clrtable	;	$332
cmd_copyvar	;	$333
cmd_cubicreg	;	$334
cmd_custmoff	;	$335
cmd_custmon	;	$336
cmd_custom	;	$337
cmd_cycle	;	$338
cmd_cyclepic	;	$339
cmd_delfold	;	$33a
cmd_delvar	;	$33b
cmd_dialog	;	$33c
cmd_disp	;	$33d
cmd_dispg	;	$33e
;cmd_disphome	;	$33f
cmd_disptbl	;	$340
cmd_drawfunc	;	$341
cmd_drawinv	;	$342
cmd_drawparm	;	$343
cmd_drawpol	;	$344
cmd_else	;	$345
cmd_endfor	;	$346
cmd_endloop	;	$347
cmd_endtry	;	$348
cmd_endwhile	;	$349
cmd_exit	;	$34a
cmd_expreg	;	$34b
cmd_fill	;	$34c
cmd_fnoff	;	$34d
cmd_fnon	;	$34e
cmd_for	;	$34f
cmd_get	;	$350
;cmd_getcalc	;	$351
cmd_goto	;	$352
cmd_graph	;	$353
cmd_if	;	$354
cmd_ifthen	;	$355
cmd_input	;	$356
cmd_inputstr	;	$357
cmd_line	;	$358
cmd_linehorz	;	$359
cmd_linetan	;	$35a
cmd_linevert	;	$35b
cmd_linreg	;	$35c
cmd_lnreg	;	$35d
cmd_local	;	$35e
cmd_lock	;	$35f
cmd_logistic	;	$360
cmd_medmed	;	$361
cmd_movevar	;	$362
cmd_newdata	;	$363
cmd_newfold	;	$364
cmd_newpic	;	$365
cmd_newplot	;	$366
cmd_newprob	;	$367
cmd_onevar	;	$368
cmd_output	;	$369
cmd_passerr	;	$36a
cmd_pause	;	$36b
cmd_plotsoff	;	$36c
cmd_plotson	;	$36d
cmd_popup	;	$36e
cmd_powerreg	;	$36f
cmd_printobj	;	$370
cmd_prompt	;	$371
cmd_ptchg	;	$372
cmd_ptoff	;	$373
cmd_pton	;	$374
cmd_pttext	;	$375
cmd_pxlchg	;	$376
cmd_pxlcircle	;	$377
cmd_pxlhorz	;	$378
cmd_pxlline	;	$379
cmd_pxloff	;	$37a
cmd_pxlon	;	$37b
cmd_pxltext	;	$37c
cmd_pxlvert	;	$37d
cmd_quadreg	;	$37e
cmd_quartreg	;	$37f
cmd_randseed	;	$380
cmd_rclgdb	;	$381
cmd_rclpic	;	$382
cmd_rename	;	$383
cmd_request	;	$384
cmd_return	;	$385
cmd_rplcpic	;	$386
cmd_send	;	$387
;cmd_sendcalc	;	$388
;cmd_sendchat	;	$389
cmd_shade	;	$38a
cmd_showstat	;	$38b
cmd_sinreg	;	$38c
cmd_slpline	;	$38d
cmd_sorta	;	$38e
cmd_sortd	;	$38f
cmd_stogdb	;	$390
cmd_stopic	;	$391
cmd_style	;	$392
cmd_table	;	$393
cmd_text	;	$394
cmd_toolbar	;	$395
cmd_trace	;	$396
cmd_try	;	$397
cmd_twovar	;	$398
cmd_unlock	;	$399
cmd_while	;	$39a
cmd_xorpic	;	$39b
cmd_zoombox	;	$39c
cmd_zoomdata	;	$39d
cmd_zoomdec	;	$39e
cmd_zoomfit	;	$39f
cmd_zoomin	;	$3a0
cmd_zoomint	;	$3a1
cmd_zoomout	;	$3a2
cmd_zoomprev	;	$3a3
cmd_zoomrcl	;	$3a4
cmd_zoomsqr	;	$3a5
cmd_zoomstd	;	$3a6
cmd_zoomsto	;	$3a7
cmd_zoomtrig	;	$3a8

;OSenqueue	;	$3a9
;OSdequeue	;	$3aa
;OSqinquire	;	$3ab
;OSqhead	;	$3ac
;OSqclear	;	$3ad

did_push_divide_units	;	$3ae
has_unit_base	;	$3af
init_unit_system	;	$3b0
is_units_term	;	$3b1
push_auto_units_conversion	;	$3b2
push_unit_system_list	;	$3b3
setup_unit_system	;	$3b4
all_tail	;	$3b5
any_tail	;	$3b6
is_matrix	;	$3b7
is_square_matrix	;	$3b8
is_valid_smap_aggregate	;	$3b9
last_element_index	;	$3ba
map_tail	;	$3bb
map_tail_Int	;	$3bc
push_list_plus	;	$3bd
push_list_times	;	$3be
push_reversed_tail	;	$3bf
push_sq_matrix_to_whole_number	;	$3c0
push_transpose_aux	;	$3c1
push_zero_partial_column	;	$3c2
;remaining_element_count	;	$3c3
push_offset_array	;	$3c4
push_matrix_product	;	$3c5
is_pathname	;	$3c6
next_token	;	$3c7
nonblank	;	$3c8
push_parse_prgm_or_func_text	;	$3c9
;push_parse_text	;	$3ca

INVALID_ROMCALL:
	lea	RomCall_str,a0
	jmp	FATAL_ERROR
	; Calculate the ROM_CALL ?
	; The calling mechanism may be :
	;	move.l	xxx*4(an),a0
	;	jsr	(a0)
	; Or:
	;	move.l	$C8,a4
	;	add.l	#xxx*4,a4
	;	move.l	(a4),a4
	;	jsr	(a4)
	
	