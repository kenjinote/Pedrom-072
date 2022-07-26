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

 dc.l (ROMCALLS_TABLE_END-ROMCALLS_TABLE)/4

ROMCALLS_TABLE:
 dc.l FirstWindow	;	$0

 dc.l WinActivate	;	$1
 dc.l WinAttr	;	$2
 dc.l WinBackupToScr	;	$3
 dc.l WinBackground	;	$4
 dc.l WinBegin	;	$5
 dc.l WinBitmapGet	;	$6
 dc.l WinBitmapPut	;	$7
 dc.l WinBitmapSize	;	$8
 dc.l WinCharXY	;	$9
 dc.l WinChar	;	$a
 dc.l WinClose	;	$b
 dc.l WinClr	;	$c
 dc.l WinDeactivate	;	$d
 dc.l WinDupStat	;	$e
 dc.l WinEllipse	;	$f
 dc.l WinFill	;	$10
 dc.l WinFillLines2	;	$11
 dc.l WinFillTriangle	;	$12
 dc.l WinFont	;	$13
 dc.l WinGetCursor	;	$14
 dc.l WinHide	;	$15
 dc.l WinHome	;	$16
 dc.l WinLine	;	$17
 dc.l WinLineNC	;	$18
 dc.l WinLineTo	;	$19
 dc.l WinLineRel	;	$1a
 dc.l WinMoveCursor	;	$1b
 dc.l WinMoveTo	;	$1c
 dc.l WinMoveRel	;	$1d
 dc.l WinOpen	;	$1e
 dc.l WinPixGet	;	$1f
 dc.l WinPixSet	;	$20
 dc.l WinRect	;	$21
 dc.l WinReOpen	;	$22
 dc.l WinScrollH	;	$23
 dc.l WinScrollV	;	$24
 dc.l WinStr	;	$25
 dc.l WinStrXY	;	$26

 dc.l DrawWinBorder	;	$27
 dc.l ScrRectDivide	;	$28
 dc.l RectWinToWin	;	$29
 dc.l RectWinToScr	;	$2a
 dc.l UpdateWindows	;	$2b
 dc.l MakeWinRect	;	$2c
 dc.l ScrToWin	;	$2d
 dc.l ScrToHome	;	$2e
 dc.l ScrRectRam	;	$2f

 dc.l Dialog	;	$30
 dc.l NoCallBack	;	$31
 dc.l DialogDo	;	$32
 dc.l DialogAdd	;	$33
 dc.l DialogNew	;	$34
 dc.l DrawStaticButton	;	$35

 dc.l MenuBegin	;	$36
 dc.l MenuCheck	;	$37
 dc.l MenuEnd	;	$38
 dc.l MenuKey	;	$39
 dc.l MenuOn	;	$3a
 dc.l MenuPopup	;	$3b
 dc.l MenuSubStat	;	$3c
 dc.l MenuTopStat	;	$3d
 dc.l MenuTopSelect	;	$3e
 dc.l MenuTopRedef	;	$3f
 dc.l MenuGetTopRedef	;	$40
 dc.l MenuAddText	;	$41
 dc.l MenuAddIcon	;	$42
 dc.l MenuNew	;	$43

 dc.l PopupAddText	;	$44
 dc.l PopupNew	;	$45
 dc.l PopupClear	;	$46
 dc.l PopupDo	;	$47
 dc.l PopupText	;	$48

 dc.l MenuUpdate	;	$49

 dc.l Parse2DExpr	;	$4a
 dc.l Parse2DMultiExpr	;	$4b
 dc.l Print2DExpr	;	$4c
 dc.l Parms2D	;	$4d
 dc.l display_statements	;	$4e
 dc.l Parse1DExpr	;	$4f

 dc.l pushkey	;	$50
 dc.l ngetchx	;	$51
 dc.l kbhit	;	$52

 dc.l sprintf	;	$53

 dc.l getcalc	;	$54
 dc.l sendcalc	;	$55
 dc.l LIO_Send	;	$56
 dc.l LIO_Get	;	$57
 dc.l LIO_Receive	;	$58
 dc.l LIO_GetMultiple	;	$59
 dc.l LIO_SendData	;	$5a
 dc.l LIO_RecvData	;	$5b

 dc.l SymAdd	;	$5c
 dc.l SymAddMain	;	$5d
 dc.l SymDel	;	$5e
 dc.l HSymDel	;	$5f
 dc.l SymFind	;	$60
 dc.l SymFindMain	;	$61
 dc.l SymFindHome	;	$62
 dc.l SymMove	;	$63
 dc.l FolderAdd	;	$64
 dc.l FolderCur	;	$65
 dc.l FolderDel	;	$66
 dc.l FolderFind	;	$67
 dc.l FolderGetCur	;	$68
 dc.l FolderOp	;	$69
 dc.l FolderRename	;	$6a
 dc.l FolderCount	;	$6b
 dc.l SymFindFirst	;	$6c
 dc.l SymFindNext	;	$6d
 dc.l SymFindPrev	;	$6e
 dc.l SymFindFoldername	;	$6f
 dc.l AddSymToFolder	;	$70
 dc.l FindSymInFolder	;	$71
 dc.l FolderCurTemp	;	$72
 dc.l FolderAddTemp	;	$73
 dc.l FolderDelTemp	;	$74
 dc.l FolderDelAllTemp	;	$75
 dc.l TempFolderName	;	$76
 dc.l IsMainFolderStr	;	$77
 dc.l ParseSymName	;	$78
 dc.l DerefSym	;	$79
 dc.l HSYMtoName	;	$7a

 dc.l StrToTokN	;	$7b
 dc.l TokToStrN	;	$7c

 dc.l CheckGraphRef	;	$7d
 dc.l ClearUserDef	;	$7e
 dc.l CheckLinkLockFlag	;	$7f
 dc.l TokenizeSymName	;	$80
 dc.l SymCmp	;	$81
 dc.l SymCpy	;	$82
 dc.l SymCpy0	;	$83
 dc.l ValidateSymName	;	$84
 dc.l VarRecall	;	$85
 dc.l VarStore	;	$86
 dc.l VarStoreLink	;	$87
 dc.l QSysProtected	;	$88
 dc.l CheckSysFunc	;	$89
 dc.l GetSysGraphRef	;	$8a
 dc.l CheckReservedName	;	$8b
 dc.l SymSysVar	;	$8c
 dc.l ValidateStore	;	$8d
 dc.l ResetSymFlags	;	$8e

 dc.l HeapAvail	;	$8f		;
 dc.l HeapAlloc	;	$90		;
 dc.l HeapAllocESTACK	;	$91	; !!
 dc.l HeapAllocHigh	;	$92	;
 dc.l HeapAllocThrow	;	$93	;
 dc.l HeapAllocHighThrow	;	$94	;
 dc.l HeapCompress	;	$95	;
 dc.l HeapDeref	;	$96		;
 dc.l HeapFree	;	$97		;
 dc.l HeapFreeIndir	;	$98	;
 dc.l HLock	;	$99		;
 dc.l HeapLock	;	$9a		;
 dc.l HeapGetLock	;	$9b		;
 dc.l HeapMax	;	$9c		;
 dc.l HeapRealloc	;	$9d		;
 dc.l HeapSize	;	$9e		;
 dc.l HeapUnlock	;	$9f		;
 dc.l HeapMoveHigh	;	$a0	;
 dc.l HeapEnd	;	$a1		; !!
 dc.l HeapAllocPtr	;	$a2	;
 dc.l HeapFreePtr	;	$a3		;
 dc.l NeedStack	;	$a4		; !!

 dc.l TE_close	;	$a5
 dc.l TE_checkSlack	;	$a6
 dc.l TE_empty	;	$a7
 dc.l TE_focus	;	$a8
 dc.l TE_handleEvent	;	$a9
 dc.l TE_indicateReadOnly	;	$aa
 dc.l TE_isBlank	;	$ab
 dc.l TE_open	;	$ac
 dc.l TE_openFixed	;	$ad
 dc.l TE_pasteText	;	$ae
 dc.l TE_reopen	;	$af
 dc.l TE_reopenPlain	;	$b0
 dc.l TE_select	;	$b1
 dc.l TE_shrinkWrap	;	$b2
 dc.l TE_unfocus	;	$b3
 dc.l TE_updateCommand	;	$b4

 dc.l _bcd_math	;	$b5
 dc.l bcdadd	;	$b6
 dc.l bcdsub	;	$b7
 dc.l bcdmul	;	$b8
 dc.l bcddiv	;	$b9
 dc.l bcdneg	;	$ba
 dc.l bcdcmp	;	$bb
 dc.l bcdlong	;	$bc
 dc.l bcdbcd	;	$bd

 dc.l EX_getArg	;	$be
 dc.l EX_getBCD	;	$bf
 dc.l EX_stoBCD	;	$c0

 dc.l CB_replaceTEXT	;	$c1
 dc.l CB_fetchTEXT	;	$c2

 dc.l CU_restore	;	$c3
 dc.l CU_start	;	$c4
 dc.l CU_stop	;	$c5

 dc.l EV_captureEvents	;	$c6
 dc.l EV_clearPasteString	;	$c7
 dc.l EV_getc	;	$c8
 dc.l EV_getSplitRect	;	$c9
 dc.l EV_notifySwitchGraph	;	$ca
 dc.l EV_paintOneWindow	;	$cb
 dc.l EV_paintWindows	;	$cc
 dc.l EV_restorePainting	;	$cd
 dc.l EV_sendEvent	;	$ce
 dc.l EV_sendEventSide	;	$cf
 dc.l EV_sendString	;	$d0
 dc.l EV_setCmdCheck	;	$d1
 dc.l EV_setCmdState	;	$d2
 dc.l EV_setFKeyState	;	$d3
 dc.l EV_startApp	;	$d4
 dc.l EV_startSide	;	$d5
 dc.l EV_startTask	;	$d6
 dc.l EV_suspendPainting	;	$d7
 dc.l EV_switch	;	$d8

 dc.l MO_currentOptions	;	$d9
 dc.l MO_defaults	;	$da
 dc.l MO_digestOptions	;	$db
 dc.l MO_isMultigraphTask	;	$dc
 dc.l MO_modeDialog	;	$dd
 dc.l MO_notifyModeChange	;	$de
 dc.l MO_sendQuit	;	$df

 dc.l ST_angle	;	$e0
 dc.l ST_batt	;	$e1
 dc.l ST_busy	;	$e2
 dc.l ST_eraseHelp	;	$e3
 dc.l ST_folder	;	$e4
 dc.l ST_graph	;	$e5
 dc.l ST_helpMsg	;	$e6
 dc.l ST_modKey	;	$e7
 dc.l ST_precision	;	$e8
 dc.l ST_readOnly	;	$e9
 dc.l ST_stack	;	$ea
 dc.l ST_refDsp	;	$eb

 dc.l OSCheckBreak	;	$ec
 dc.l OSClearBreak	;	$ed
 dc.l OSEnableBreak	;	$ee
 dc.l OSDisableBreak	;	$ef

 dc.l OSRegisterTimer	;	$f0
 dc.l OSFreeTimer	;	$f1
 dc.l OSTimerCurVal	;	$f2
 dc.l OSTimerExpired	;	$f3
 dc.l OSTimerRestart	;	$f4

 dc.l acos	;	$f5
 dc.l asin	;	$f6
 dc.l atan	;	$f7
 dc.l atan2	;	$f8
 dc.l cos	;	$f9
 dc.l sin	;	$fa
 dc.l tan	;	$fb
 dc.l cosh	;	$fc
 dc.l sinh	;	$fd
 dc.l tanh	;	$fe
 dc.l exp	;	$ff
 dc.l log	;	$100
 dc.l log10	;	$101
 dc.l modf	;	$102
 dc.l pow	;	$103
 dc.l sqrt	;	$104
 dc.l ceil	;	$105
 dc.l fabs	;	$106
 dc.l floor	;	$107
 dc.l fmod	;	$108

 dc.l top_estack	;	$109

 dc.l next_expression_index	;	$10a

 dc.l gr_active	;	$10b
 dc.l gr_other	;	$10c
 dc.l ABT_dialog	;	$10d
 dc.l HomeExecute	;	$10e
 dc.l HomePushEStack	;	$10f
 dc.l SP_Define	;	$110
 dc.l store_data_var	;	$111
 dc.l recall_data_var	;	$112
 dc.l CharNumber	;	$113
 dc.l spike_optionD	;	$114
 dc.l spike_geo_titles	;	$115
 dc.l spike_in_editor	;	$116
 dc.l dv_create_graph_titles	;	$117
 dc.l spike_titles_in_editor	;	$118
 dc.l dv_findColumn	;	$119
 dc.l spike_chk_gr_dirty	;	$11a
 dc.l GetStatValue	;	$11b
 dc.l partial_len	;	$11c
 dc.l paint_all_except	;	$11d
 dc.l equ_select	;	$11e
 dc.l equ_setStyle	;	$11f
 dc.l equ_getNameInfo	;	$120
 dc.l checkCurrent	;	$121
 dc.l BN_power17Mod	;	$122
 dc.l BN_powerMod	;	$123
 dc.l BN_prodMod	;	$124
 dc.l CAT_dialog	;	$125
 dc.l caddcert	;	$126
 dc.l cdecrypt	;	$127
 dc.l ceof	;	$128
 dc.l cfindcertfield	;	$129
 dc.l cfindfield	;	$12a
 dc.l cgetc	;	$12b
 dc.l cgetcert	;	$12c
 dc.l cgetflen	;	$12d
 dc.l cgetfnl	;	$12e
 dc.l cgetnl	;	$12f
 dc.l cgetns	;	$130
 dc.l cgetvernum	;	$131
 dc.l copen	;	$132
 dc.l copensub	;	$133
 dc.l cputhdr	;	$134
 dc.l cputnl	;	$135
 dc.l cputns	;	$136
 dc.l cread	;	$137
 dc.l ctell	;	$138
 dc.l cwrite	;	$139
 dc.l cacos	;	$13a
 dc.l casin	;	$13b
 dc.l catan	;	$13c
 dc.l cacosh	;	$13d
 dc.l casinh	;	$13e
 dc.l catanh	;	$13f
 dc.l ccos	;	$140
 dc.l csin	;	$141
 dc.l ctan	;	$142
 dc.l ccosh	;	$143
 dc.l csinh	;	$144
 dc.l ctanh	;	$145
 dc.l csqrt	;	$146
 dc.l cln	;	$147
 dc.l clog10	;	$148
 dc.l cexp	;	$149
 dc.l CustomBegin	;	$14a
 dc.l CustomMenuItem	;	$14b
 dc.l CustomEnd	;	$14c
 dc.l ReallocExprStruct	;	$14d
 dc.l SearchExprStruct	;	$14e
 dc.l handleRclKey	;	$14f
 dc.l CustomFree	;	$150
 dc.l ERD_dialog	;	$151
 dc.l ERD_process	;	$152
 dc.l ER_throwVar	;	$153
 dc.l ER_catch	;	$154
 dc.l ER_success	;	$155
 dc.l EV_centralDispatcher	;	$156
 dc.l EV_defaultHandler	;	$157
 dc.l EV_eventLoop	;	$158
 dc.l EV_registerMenu	;	$159
 dc.l EX_patch	;	$15a

 dc.l EM_abandon	;	$15b
 dc.l EM_blockErase	;	$15c
 dc.l EM_blockVerifyErase	;	$15d
 dc.l EM_delete	;	$15e
 dc.l EM_findEmptySlot	;	$15f
 dc.l EM_GC	;	$160

 dc.l EM_moveSymFromExtMem	;	$161
 dc.l EM_moveSymToExtMem	;	$162
 dc.l EM_open	;	$163
 dc.l EM_put	;	$164
 dc.l EM_survey	;	$165
 dc.l EM_twinSymFromExtMem	;	$166
 dc.l EM_write	;	$167
 dc.l EM_writeToExtMem	;	$168

 dc.l FL_addCert	;	$169
 dc.l FL_download	;	$16a
 dc.l FL_getHardwareParmBlock	;	$16b
 dc.l FL_getCert	;	$16c
 dc.l FL_getVerNum	;	$16d
 dc.l equ_deStatus	;	$16e
 dc.l cmpstri	;	$16f
 dc.l fix_loop_displacements	;	$170
 dc.l FL_write	;	$171

 dc.l fpisanint	;	$172
 dc.l fpisodd	;	$173
 dc.l round12	;	$174
 dc.l round14	;	$175
 dc.l GD_Circle	;	$176
 dc.l GD_Line	;	$177
 dc.l GD_HVLine	;	$178
 dc.l GD_Pen	;	$179
 dc.l GD_Eraser	;	$17a
 dc.l GD_Text	;	$17b
 dc.l GD_Select	;	$17c
 dc.l GD_Contour	;	$17d

 dc.l GKeyIn	;	$17e
 dc.l GKeyDown	;	$17f
 dc.l GKeyFlush	;	$180
 dc.l HelpKeys	;	$181
 dc.l QModeKey	;	$182
 dc.l QSysKey	;	$183
 dc.l WordInList	;	$184
 dc.l BitmapGet	;	$185
 dc.l BitmapInit	;	$186
 dc.l BitmapPut	;	$187
 dc.l BitmapSize	;	$188
 dc.l ScrRectFill	;	$189
 dc.l ScrRectOverlap	;	$18a
 dc.l ScrRectScroll	;	$18b
 dc.l ScrRectShift	;	$18c
 dc.l QScrRectOverlap	;	$18d
 dc.l FontGetSys	;	$18e
 dc.l FontSetSys	;	$18f
 dc.l FontCharWidth	;	$190
 dc.l DrawClipChar	;	$191
 dc.l DrawClipEllipse	;	$192
 dc.l DrawClipLine	;	$193
 dc.l DrawClipPix	;	$194
 dc.l DrawClipRect	;	$195
 dc.l DrawMultiLines	;	$196
 dc.l DrawStrWidth	;	$197
 dc.l FillTriangle	;	$198
 dc.l FillLines2	;	$199
 dc.l SetCurAttr	;	$19a
 dc.l SetCurClip	;	$19b
 dc.l LineTo	;	$19c
 dc.l MoveTo	;	$19d
 dc.l ScreenClear	;	$19e
 dc.l GetPix	;	$19f
 dc.l SaveScrState	;	$1a0
 dc.l RestoreScrState	;	$1a1
 dc.l PortSet	;	$1a2
 dc.l PortRestore	;	$1a3
 dc.l DrawChar	;	$1a4
 dc.l DrawFkey	;	$1a5
 dc.l DrawIcon	;	$1a6
 dc.l DrawLine	;	$1a7
 dc.l DrawPix	;	$1a8
 dc.l DrawStr	;	$1a9

 dc.l GM_Value	;	$1aa
 dc.l GM_Intersect	;	$1ab
 dc.l GM_Integrate	;	$1ac
 dc.l GM_Inflection	;	$1ad
 dc.l GM_TanLine	;	$1ae
 dc.l GM_Math1	;	$1af
 dc.l GM_Derivative	;	$1b0
 dc.l GM_DistArc	;	$1b1
 dc.l GM_Shade	;	$1b2

 dc.l YCvtFtoWin	;	$1b3
 dc.l DlgMessage	;	$1b4
 dc.l SetGraphMode	;	$1b5
 dc.l Regraph	;	$1b6
 dc.l GrAxes	;	$1b7
 dc.l gr_xres_pixel	;	$1b8
 dc.l CptFuncX	;	$1b9
 dc.l XCvtPtoF	;	$1ba
 dc.l YCvtPtoF	;	$1bb
 dc.l YCvtFtoP	;	$1bc
 dc.l XCvtFtoP	;	$1bd
 dc.l GrLineFlt	;	$1be
 dc.l FuncLineFlt	;	$1bf
 dc.l GrClipLine	;	$1c0
 dc.l CptDeltax	;	$1c1
 dc.l CptDeltay	;	$1c2
 dc.l CkValidDelta	;	$1c3
 dc.l GR_Pan	;	$1c4
 dc.l FindFunc	;	$1c5
 dc.l FindGrFunc	;	$1c6
 dc.l grFuncName	;	$1c7
 dc.l gr_initCondName	;	$1c8
 dc.l CptIndep	;	$1c9
 dc.l gr_CptIndepInc	;	$1ca
 dc.l gr_del_locals	;	$1cb
 dc.l gr_DelFolder	;	$1cc
 dc.l gr_openFolder	;	$1cd
 dc.l setup_more_graph_fun	;	$1ce
 dc.l unlock_more_graph_fun	;	$1cf
 dc.l execute_graph_func	;	$1d0
 dc.l cpt_gr_fun	;	$1d1
 dc.l cpt_gr_param	;	$1d2
 dc.l cpt_gr_polar	;	$1d3
 dc.l gr_execute_seq	;	$1d4
 dc.l CountGrFunc	;	$1d5
 dc.l FirstSeqPlot	;	$1d6
 dc.l cleanup_seq_mem	;	$1d7
 dc.l time_loop	;	$1d8
 dc.l InitTimeSeq	;	$1d9
 dc.l seqWebInit	;	$1da
 dc.l run_one_seq	;	$1db
 dc.l gr_seq_value	;	$1dc
 dc.l StepCk	;	$1dd
 dc.l seqStepCk	;	$1de
 dc.l rngLen	;	$1df
 dc.l gdb_len	;	$1e0
 dc.l gdb_store	;	$1e1
 dc.l gdb_recall	;	$1e2
 dc.l gr_DispLabels	;	$1e3
 dc.l GraphOrTableCmd	;	$1e4
 dc.l ck_valid_float	;	$1e5
 dc.l CreateEmptyList	;	$1e6
 dc.l QSkipGraphErr	;	$1e7
 dc.l gr_find_de_result	;	$1e8
 dc.l InitDEAxesRng	;	$1e9
 dc.l InitDEMem	;	$1ea
 dc.l de_loop	;	$1eb
 dc.l cleanup_de_mem	;	$1ec
 dc.l gr_de_value	;	$1ed
 dc.l gr_find_func_index	;	$1ee
 dc.l CptLastIndepDE	;	$1ef
 dc.l de_initRes	;	$1f0
 dc.l gr_del_vars_in_folder	;	$1f1
 dc.l gr_de_axes_lbl	;	$1f2
 dc.l gr_execute_de	;	$1f3
 dc.l gr_delete_fldpic	;	$1f4
 dc.l gr_remove_fldpic	;	$1f5
 dc.l gr_add_fldpic	;	$1f6
 dc.l gr_stopic	;	$1f7
 dc.l gr_find_el	;	$1f8
 dc.l deStepCk	;	$1f9
 dc.l gr_ck_solvergraph	;	$1fa
 dc.l GR3_addContours	;	$1fb
 dc.l GraphActivate	;	$1fc
 dc.l GR3_freeDB	;	$1fd
 dc.l GR3_handleEvent	;	$1fe
 dc.l GR3_paint3d	;	$1ff
 dc.l GR3_xyToWindow	;	$200
 dc.l GS_PlotTrace	;	$201
 dc.l GS_PlotAll	;	$202
 dc.l PlotDel	;	$203
 dc.l PlotPut	;	$204
 dc.l PlotGet	;	$205
 dc.l PlotInit	;	$206
 dc.l PlotDup	;	$207
 dc.l PlotSize	;	$208
 dc.l PlotLookup	;	$209
 dc.l QActivePlots	;	$20a
 dc.l QPlotActive	;	$20b

 dc.l GT_BackupToScr	;	$20c
 dc.l GT_CalcDepVals	;	$20d
 dc.l GT_CenterGraphCursor	;	$20e
 dc.l GT_CursorKey	;	$20f
 dc.l GT_DspFreeTraceCoords	;	$210
 dc.l GT_DspTraceCoords	;	$211
 dc.l GT_DspMsg	;	$212
 dc.l GT_Error	;	$213
 dc.l GT_Format	;	$214
 dc.l GT_FreeTrace	;	$215
 dc.l GT_IncXY	;	$216
 dc.l GT_KeyIn	;	$217
 dc.l GT_QFloatCursorsInRange	;	$218
 dc.l GT_Regraph	;	$219
 dc.l GT_Regraph_if_neccy	;	$21a
 dc.l GT_Open	;	$21b
 dc.l GT_SaveAs	;	$21c
 dc.l GT_SelFunc	;	$21d
 dc.l GT_SetGraphRange	;	$21e
 dc.l GT_SetCursorXY	;	$21f
 dc.l GT_ShowMarkers	;	$220
 dc.l GT_Trace	;	$221
 dc.l GT_ValidGraphRanges	;	$222
 dc.l GT_WinBound	;	$223
 dc.l GT_WinCursor	;	$224

 dc.l GYcoord	;	$225
 dc.l GXcoord	;	$226

 dc.l round12_err	;	$227
 dc.l GT_Set_Graph_Format	;	$228
 dc.l GT_PrintCursor	;	$229
 dc.l GT_DE_Init_Conds	;	$22a

 dc.l GZ_Box	;	$22b
 dc.l GZ_Center	;	$22c
 dc.l GZ_Decimal	;	$22d
 dc.l GZ_Fit	;	$22e
 dc.l GZ_InOut	;	$22f
 dc.l GZ_Integer	;	$230
 dc.l GZ_Previous	;	$231
 dc.l GZ_Recall	;	$232
 dc.l GZ_SetFactors	;	$233
 dc.l GZ_Square	;	$234
 dc.l GZ_Standard	;	$235
 dc.l GZ_Stat	;	$236
 dc.l GZ_Store	;	$237
 dc.l GZ_Trig	;	$238

 dc.l HeapGetHandle	;	$239
 dc.l HeapPtrToHandle	;	$23a
 dc.l FreeHandles	;	$23b

 dc.l HS_chopFIFO	;	$23c
 dc.l HS_countFIFO	;	$23d
 dc.l HS_deleteFIFONode	;	$23e
 dc.l HS_freeAll	;	$23f
 dc.l HS_freeFIFONode	;	$240
 dc.l HS_getAns	;	$241
 dc.l HS_getEntry	;	$242
 dc.l HS_getFIFONode	;	$243
 dc.l HS_popEStack	;	$244
 dc.l HS_newFIFONode	;	$245
 dc.l HS_pushFIFONode	;	$246
 dc.l HToESI	;	$247

 dc.l OSInitKeyInitDelay	;	$248
 dc.l OSInitBetweenKeyDelay	;	$249

 dc.l OSCheckSilentLink	;	$24a
 dc.l OSLinkCmd	;	$24b
 dc.l OSLinkReset	;	$24c
 dc.l OSLinkOpen	;	$24d
 dc.l OSLinkClose	;	$24e
 dc.l OSReadLinkBlock	;	$24f
 dc.l OSWriteLinkBlock	;	$250
 dc.l OSLinkTxQueueInquire	;	$251
 dc.l OSLinkTxQueueActive	;	$252
 dc.l LIO_SendProduct	;	$253

 dc.l MD5Init	;	$254
 dc.l MD5Update	;	$255
 dc.l MD5Final	;	$256
 dc.l MD5Done	;	$257

 dc.l convert_to_TI_92	;	$258
 dc.l gen_version	;	$259
 dc.l is_executable	;	$25a
 dc.l NG_RPNToText	;	$25b
 dc.l NG_approxESI	;	$25c
 dc.l NG_execute	;	$25d
 dc.l NG_graphESI	;	$25e
 dc.l NG_rationalESI	;	$25f
 dc.l NG_tokenize	;	$260
 dc.l NG_setup_graph_fun	;	$261
 dc.l NG_cleanup_graph_fun	;	$262
 dc.l push_END_TAG	;	$263
 dc.l push_LIST_TAG	;	$264
 dc.l tokenize_if_TI_92_or_text	;	$265

 dc.l setjmp	;	$266
 dc.l longjmp	;	$267

 dc.l VarGraphRefBitsClear	;	$268
 dc.l VarInit	;	$269

 dc.l memcpy	;	$26a
 dc.l memmove	;	$26b
 dc.l strcpy	;	$26c
 dc.l strncpy	;	$26d
 dc.l strcat	;	$26e
 dc.l strncat	;	$26f
 dc.l memcmp	;	$270
 dc.l strcmp	;	$271
 dc.l strncmp	;	$272
 dc.l memchr	;	$273
 dc.l strchr	;	$274
 dc.l strcspn	;	$275
 dc.l strpbrk	;	$276
 dc.l strrchr	;	$277
 dc.l strspn	;	$278
 dc.l strstr	;	$279
 dc.l strtok	;	$27a
 dc.l _memset	;	$27b
 dc.l memset	;	$27c
 dc.l strerror	;	$27d
 dc.l strlen	;	$27e

 dc.l SymAddTwin	;	$27f
 dc.l SymDelTwin	;	$280
 dc.l LoadSymFromFindHandle	;	$281
 dc.l MakeHsym	;	$282
 dc.l SymFindPtr	;	$283

 dc.l OSVRegisterTimer	;	$284
 dc.l OSVFreeTimer	;	$285

 dc.l sincos	;	$286
 dc.l asinh	;	$287
 dc.l acosh	;	$288
 dc.l atanh	;	$289
 dc.l itrig	;	$28a
 dc.l trig	;	$28b

 dc.l VarOpen	;	$28c
 dc.l VarSaveAs	;	$28d
 dc.l VarNew	;	$28e
 dc.l VarCreateFolderPopup	;	$28f
 dc.l VarSaveTitle	;	$290

 dc.l WinWidth	;	$291
 dc.l WinHeight	;	$292
 dc.l XR_stringPtr	;	$293

 dc.l OSReset	;	$294
 dc.l SumStoChkMem	;	$295
 dc.l OSContrastUp	;	$296
 dc.l OSContrastDn	;	$297
 dc.l OSKeyScan	;	$298
 dc.l OSGetStatKeys	;	$299

 dc.l off	;	$29a
 dc.l idle	;	$29b
 dc.l OSSetSR	;	$29c

 dc.l AB_prodid	;	$29d		; renvoie l'ID du produit, a0 = le 0 de cette chaine
 dc.l AB_prodname	;	$29e	; a0 : nom du produit
 dc.l AB_serno	;	$29f		; je pense que c le serial, mais je peux pas tester sur emu

 dc.l cgetcertrevno	;	$2a0
 dc.l cgetsn	;	$2a1

 dc.l de_rng_no_graph	;	$2a2

 dc.l EV_hook	;	$2a3

 dc.l _ds16u16	;	$2a4
 dc.l _ms16u16	;	$2a5
 dc.l _du16u16	;	$2a6
 dc.l _mu16u16	;	$2a7
 dc.l _ds32s32	;	$2a8
 dc.l _ms32s32	;	$2a9
 dc.l _du32u32	;	$2aa
 dc.l _mu32u32	;	$2ab

 dc.l assign_between	;	$2ac
 dc.l did_push_var_val	;	$2ad
 dc.l does_push_fetch	;	$2ae
 dc.l delete_list_element	;	$2af
 dc.l push_ans_entry	;	$2b0
 dc.l index_after_match_endtag	;	$2b1
 dc.l push_indir_name	;	$2b2
 dc.l push_user_func	;	$2b3
 dc.l store_func_def	;	$2b4
 dc.l store_to_subscripted_element	;	$2b5
 dc.l index_below_display_expression_aux	;	$2b6
 dc.l get_key_ptr	;	$2b7
 dc.l get_list_indices	;	$2b8
 dc.l get_matrix_indices	;	$2b9
 dc.l init_list_indices	;	$2ba
 dc.l init_matrix_indices	;	$2bb
 dc.l push_float_qr_fact	;	$2bc
 dc.l push_lu_fact	;	$2bd
 dc.l push_symbolic_qr_fact	;	$2be
 dc.l are_expressions_identical	;	$2bf
 dc.l compare_expressions	;	$2c0
 dc.l find_error_message	;	$2c1
 dc.l check_estack_size	;	$2c2
 dc.l delete_between	;	$2c3
 dc.l deleted_between	;	$2c4
 dc.l delete_expression	;	$2c5
 dc.l deleted_expression	;	$2c6
 dc.l estack_to_short	;	$2c7
 dc.l estack_to_ushort	;	$2c8
 dc.l factor_base_index	;	$2c9
 dc.l factor_exponent_index	;	$2ca
 dc.l GetValue	;	$2cb
 dc.l im_index	;	$2cc
 dc.l index_numeric_term	;	$2cd
 dc.l index_of_lead_base_of_lead_term	;	$2ce
 dc.l index_main_var	;	$2cf
 dc.l is_advanced_tag	;	$2d0
 dc.l is_antisymmetric	;	$2d1
 dc.l is_complex_number	;	$2d2
 dc.l is_complex0	;	$2d3
 dc.l is_free_of_tag	;	$2d4
 dc.l is_independent_of	;	$2d5
 dc.l is_independent_of_de_seq_vars	;	$2d6
 dc.l is_independent_of_tail	;	$2d7
 dc.l is_independent_of_elements	;	$2d8
 dc.l is_monomial	;	$2d9
 dc.l is_monomial_in_kernel	;	$2da
 dc.l is_narrowly_independent_of	;	$2db
 dc.l is_symmetric	;	$2dc
 dc.l is_tail_independent_of	;	$2dd
 dc.l lead_base_index	;	$2de
 dc.l lead_exponent_index	;	$2df
 dc.l lead_factor_index	;	$2e0
 dc.l lead_term_index	;	$2e1
 dc.l main_gen_var_index	;	$2e2
 dc.l map_unary_over_comparison	;	$2e3
 dc.l min_quantum	;	$2e4
 dc.l move_between_to_top	;	$2e5
 dc.l moved_between_to_top	;	$2e6
 dc.l numeric_factor_index	;	$2e7
 dc.l push_between	;	$2e8		; push_between(end_EXPR *, end_EXPR*)
 dc.l push_expr_quantum	;	$2e9
 dc.l push_expr2_quantum	;	$2ea
 dc.l push_next_arb_int	;	$2eb
 dc.l push_next_arb_real	;	$2ec
 dc.l push_next_internal_var	;	$2ed
 dc.l push_quantum	;	$2ee
 dc.l push_quantum_pair	;	$2ef
 dc.l reductum_index	;	$2f0
 dc.l remaining_factors_index	;	$2f1
 dc.l re_index	;	$2f2
 dc.l reset_estack_size	;	$2f3
 dc.l reset_control_flags	;	$2f4
 dc.l can_be_approxed	;	$2f5
 dc.l compare_complex_magnitudes	;	$2f6
 dc.l compare_Floats	;	$2f7
 dc.l did_push_cnvrt_Float_to_integer	;	$2f8
 dc.l estack_number_to_Float	;	$2f9
 dc.l float_class	;	$2fa
 dc.l frexp10	;	$2fb
 dc.l gcd_exact_whole_Floats	;	$2fc
 dc.l init_float	;	$2fd
 dc.l is_Float_exact_whole_number	;	$2fe
 dc.l is_float_infinity	;	$2ff
 dc.l is_float_negative_zero	;	$300
 dc.l is_float_positive_zero	;	$301
 dc.l is_float_signed_infinity	;	$302
 dc.l is_float_transfinite	;	$303
 dc.l is_float_unsigned_inf_or_nan	;	$304
 dc.l is_float_unsigned_zero	;	$305
 dc.l is_nan	;	$306
 dc.l likely_approx_to_complex_number	;	$307
 dc.l likely_approx_to_number	;	$308
 dc.l norm1_complex_Float	;	$309
 dc.l push_Float	;	$30a
 dc.l push_Float_to_nonneg_int	;	$30b
 dc.l push_Float_to_rat	;	$30c
 dc.l push_cnvrt_integer_if_whole_nmb	;	$30d
 dc.l push_overflow_to_infinity	;	$30e
 dc.l push_pow	;	$30f
 dc.l push_round_Float	;	$310
 dc.l should_and_did_push_approx_arg2	;	$311
 dc.l signum_Float	;	$312
 dc.l did_push_to_polar	;	$313
 dc.l push_degrees	;	$314
 dc.l push_format	;	$315
 dc.l push_getkey	;	$316
 dc.l push_getfold	;	$317
 dc.l push_getmode	;	$318
 dc.l push_gettype	;	$319
 dc.l push_instring	;	$31a
 dc.l push_mrow_aux	;	$31b
 dc.l push_part	;	$31c
 dc.l push_pttest	;	$31d
 dc.l push_pxltest	;	$31e
 dc.l push_rand	;	$31f
 dc.l push_randpoly	;	$320
 dc.l push_setfold	;	$321
 dc.l push_setgraph	;	$322
 dc.l push_setmode	;	$323
 dc.l push_settable	;	$324
 dc.l push_str_to_expr	;	$325
 dc.l push_string	;	$326
 dc.l push_switch	;	$327
 dc.l push_to_cylin	;	$328
 dc.l push_to_sphere	;	$329

 dc.l cmd_andpic	;	$32a
 dc.l cmd_blddata	;	$32b
 dc.l cmd_circle	;	$32c
 dc.l cmd_clrdraw	;	$32d
 dc.l cmd_clrerr	;	$32e
 dc.l cmd_clrgraph	;	$32f
 dc.l cmd_clrhome	;	$330
 dc.l cmd_clrio	;	$331
 dc.l cmd_clrtable	;	$332
 dc.l cmd_copyvar	;	$333
 dc.l cmd_cubicreg	;	$334
 dc.l cmd_custmoff	;	$335
 dc.l cmd_custmon	;	$336
 dc.l cmd_custom	;	$337
 dc.l cmd_cycle	;	$338
 dc.l cmd_cyclepic	;	$339
 dc.l cmd_delfold	;	$33a
 dc.l cmd_delvar	;	$33b
 dc.l cmd_dialog	;	$33c
 dc.l cmd_disp	;	$33d
 dc.l cmd_dispg	;	$33e
 dc.l cmd_disphome	;	$33f
 dc.l cmd_disptbl	;	$340
 dc.l cmd_drawfunc	;	$341
 dc.l cmd_drawinv	;	$342
 dc.l cmd_drawparm	;	$343
 dc.l cmd_drawpol	;	$344
 dc.l cmd_else	;	$345
 dc.l cmd_endfor	;	$346
 dc.l cmd_endloop	;	$347
 dc.l cmd_endtry	;	$348
 dc.l cmd_endwhile	;	$349
 dc.l cmd_exit	;	$34a
 dc.l cmd_expreg	;	$34b
 dc.l cmd_fill	;	$34c
 dc.l cmd_fnoff	;	$34d
 dc.l cmd_fnon	;	$34e
 dc.l cmd_for	;	$34f
 dc.l cmd_get	;	$350
 dc.l cmd_getcalc	;	$351
 dc.l cmd_goto	;	$352
 dc.l cmd_graph	;	$353
 dc.l cmd_if	;	$354
 dc.l cmd_ifthen	;	$355
 dc.l cmd_input	;	$356
 dc.l cmd_inputstr	;	$357
 dc.l cmd_line	;	$358
 dc.l cmd_linehorz	;	$359
 dc.l cmd_linetan	;	$35a
 dc.l cmd_linevert	;	$35b
 dc.l cmd_linreg	;	$35c
 dc.l cmd_lnreg	;	$35d
 dc.l cmd_local	;	$35e
 dc.l cmd_lock	;	$35f
 dc.l cmd_logistic	;	$360
 dc.l cmd_medmed	;	$361
 dc.l cmd_movevar	;	$362
 dc.l cmd_newdata	;	$363
 dc.l cmd_newfold	;	$364
 dc.l cmd_newpic	;	$365
 dc.l cmd_newplot	;	$366
 dc.l cmd_newprob	;	$367
 dc.l cmd_onevar	;	$368
 dc.l cmd_output	;	$369
 dc.l cmd_passerr	;	$36a
 dc.l cmd_pause	;	$36b
 dc.l cmd_plotsoff	;	$36c
 dc.l cmd_plotson	;	$36d
 dc.l cmd_popup	;	$36e
 dc.l cmd_powerreg	;	$36f
 dc.l cmd_printobj	;	$370
 dc.l cmd_prompt	;	$371
 dc.l cmd_ptchg	;	$372
 dc.l cmd_ptoff	;	$373
 dc.l cmd_pton	;	$374
 dc.l cmd_pttext	;	$375
 dc.l cmd_pxlchg	;	$376
 dc.l cmd_pxlcircle	;	$377
 dc.l cmd_pxlhorz	;	$378
 dc.l cmd_pxlline	;	$379
 dc.l cmd_pxloff	;	$37a
 dc.l cmd_pxlon	;	$37b
 dc.l cmd_pxltext	;	$37c
 dc.l cmd_pxlvert	;	$37d
 dc.l cmd_quadreg	;	$37e
 dc.l cmd_quartreg	;	$37f
 dc.l cmd_randseed	;	$380
 dc.l cmd_rclgdb	;	$381
 dc.l cmd_rclpic	;	$382
 dc.l cmd_rename	;	$383
 dc.l cmd_request	;	$384
 dc.l cmd_return	;	$385
 dc.l cmd_rplcpic	;	$386
 dc.l cmd_send	;	$387
 dc.l cmd_sendcalc	;	$388
 dc.l cmd_sendchat	;	$389
 dc.l cmd_shade	;	$38a
 dc.l cmd_showstat	;	$38b
 dc.l cmd_sinreg	;	$38c
 dc.l cmd_slpline	;	$38d
 dc.l cmd_sorta	;	$38e
 dc.l cmd_sortd	;	$38f
 dc.l cmd_stogdb	;	$390
 dc.l cmd_stopic	;	$391
 dc.l cmd_style	;	$392
 dc.l cmd_table	;	$393
 dc.l cmd_text	;	$394
 dc.l cmd_toolbar	;	$395
 dc.l cmd_trace	;	$396
 dc.l cmd_try	;	$397
 dc.l cmd_twovar	;	$398
 dc.l cmd_unlock	;	$399
 dc.l cmd_while	;	$39a
 dc.l cmd_xorpic	;	$39b
 dc.l cmd_zoombox	;	$39c
 dc.l cmd_zoomdata	;	$39d
 dc.l cmd_zoomdec	;	$39e
 dc.l cmd_zoomfit	;	$39f
 dc.l cmd_zoomin	;	$3a0
 dc.l cmd_zoomint	;	$3a1
 dc.l cmd_zoomout	;	$3a2
 dc.l cmd_zoomprev	;	$3a3
 dc.l cmd_zoomrcl	;	$3a4
 dc.l cmd_zoomsqr	;	$3a5
 dc.l cmd_zoomstd	;	$3a6
 dc.l cmd_zoomsto	;	$3a7
 dc.l cmd_zoomtrig	;	$3a8

 dc.l OSenqueue	;	$3a9
 dc.l OSdequeue	;	$3aa
 dc.l OSqinquire	;	$3ab
 dc.l OSqhead	;	$3ac
 dc.l OSqclear	;	$3ad

 dc.l did_push_divide_units	;	$3ae
 dc.l has_unit_base	;	$3af
 dc.l init_unit_system	;	$3b0
 dc.l is_units_term	;	$3b1
 dc.l push_auto_units_conversion	;	$3b2
 dc.l push_unit_system_list	;	$3b3
 dc.l setup_unit_system	;	$3b4
 dc.l all_tail	;	$3b5
 dc.l any_tail	;	$3b6
 dc.l is_matrix	;	$3b7
 dc.l is_square_matrix	;	$3b8
 dc.l is_valid_smap_aggregate	;	$3b9
 dc.l last_element_index	;	$3ba
 dc.l map_tail	;	$3bb
 dc.l map_tail_Int	;	$3bc
 dc.l push_list_plus	;	$3bd
 dc.l push_list_times	;	$3be
 dc.l push_reversed_tail	;	$3bf
 dc.l push_sq_matrix_to_whole_number	;	$3c0
 dc.l push_transpose_aux	;	$3c1
 dc.l push_zero_partial_column	;	$3c2
 dc.l remaining_element_count	;	$3c3
 dc.l push_offset_array	;	$3c4
 dc.l push_matrix_product	;	$3c5
 dc.l is_pathname	;	$3c6
 dc.l next_token	;	$3c7
 dc.l nonblank	;	$3c8
 dc.l push_parse_prgm_or_func_text	;	$3c9
 dc.l push_parse_text	;	$3ca
 dc.l INVALID_ROMCALL	; What is $3CB ?
 dc.l INVALID_ROMCALL	; What is $3CC ?
ROMCALLS_TABLE_END:
		