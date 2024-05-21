;============================================================================
;	proplayer.a
;	~~~~~~~~~~~
; $VER: proplayer 6.3 (19.06.1995)
;
; The music player routine for MMD0/MMD1/MMD2 MED/OctaMED
; four-channel modules.
;
; Copyright � 1995 Teijo Kinnunen AND RBF Software.
;
; Written by Teijo Kinnunen.
; Comments/questions/bug repoRTS can be sent to:
;	Teijo Kinnunen
;	Oksantie 19
;	FIN-86300  OULAINEN
;	FINLAND
;	email: Teijo.Kinnunen@oulu.fi
;
; See OctaMED docs for conditions about using these routines.
; Comments/questions about distribution AND usage conditions
; should be directed to RBF Software. (Email: rbfsoft@cix.compulink.co.uk)
;============================================================================
; REFACTOR, DEBUG, EXTENDED FEATURES BY KONEY | koney.org | github.com/KONEY
; KONEY Version 1.0 | 30.10.2021
; KONEY Version 1.2 | 30.04.2022 | NO MORE AURA/MIDI CODE | BIG REFACTOR
;============================================================================
	;SECTION	"Code",CODE
	IFNE EASY
		XDEF	_startmusic,_endmusic
		
_startmusic:
	LEA	MED_MODULE,A2
	BSR.S	_RelocModule
	BSR.W	_InitPlayer
	LEA	MED_MODULE,A0
	BRA.W	_PlayModule

_endmusic:
	BRA.W	_RemPlayer
; ***** The relocation routine *****
; NOTE: The module pointer is passed in register A0 (FALSE). Use stubs with C.
reloci:			; ** RELOC SAMPLES **
	MOVE.L	mmd_smplarr(A2),D0
	BEQ.S	.xloci
	MOVEA.L	D0,A0
	MOVEQ	#0,D0
	MOVE.B	msng_numsamples(A1),D0	; number of samples
	SUBQ.B	#1,D0
	IFNE SPLIT_RELOCS
	MOVE.L	#MED_SAMPLES,D7		; NEW POINTER
	SUB.L	(A0),D7			; NEW OFFSET
	ENDC
	.relocs:
	IFNE SPLIT_RELOCS
	BSR.S	relocSample		; FOR SAMPLES ONLY
	ENDC
	IFEQ SPLIT_RELOCS
	BSR.S	relocentr			; original SUBroutine
	ENDC
	MOVE.L	-4(A0),D3			; sample ptr
	BEQ.S	.nosyn
	MOVE.L	D3,A3
	TST.W	4(A3)
	BPL.S	.nosyn			; type >= 0
	MOVE.W	20(A3),D2			; number of waveforms
	LEA	278(A3),A3		; ptr to wf ptrs
	SUBQ.W	#1,D2
	.relsyn:
	ADD.L	D3,(A3)+
	DBF	D2,.relsyn
	.nosyn:
	DBF	D0,.relocs
	.xloci:
	RTS

	IFNE SPLIT_RELOCS
relocSample:
	TST.L	(A0)
	BEQ.S	.norel
	ADD.L	D7,(A0)+
	RTS
	.norel:
	ADDQ.L	#4,A0
	RTS
	ENDC
relocentr:
	TST.L	(A0)
	BEQ.S	.norel
	ADD.L	D1,(A0)+
	RTS
	.norel:
	ADDQ.L	#4,A0
	RTS

_RelocModule:
	MOVEM.L	A2-A4/D2-D4,-(SP)
	MOVE.L	A2,D1			; D1 = ptr to start of module
	BSR.S	.relocp
	MOVEA.L	mmd_songinfo(A2),A1
	BSR.S	reloci
	MOVE.B	mmd_songsleft(A2),D4
	.rel_lp:
	BSR.S	.relocb
	CMP.B	#'2',3(A2)		; MMD2?
	BNE.S	.norelmmD2
	BSR.W	.relocmmD2sng
	.norelmmD2:
	MOVE.L	mmd_expdata(A2),D0
	BEQ.S	.rel_ex
	MOVE.L	D0,A0
	BSR.S	relocentr
	BSR.S	relocentr
	ADDQ.L	#4,A0
	; We reloc the pointers of MMD0exp, so anybody who needs them can easily read theM.
	BSR.S	relocentr			; annotxt
	ADDQ.L	#4,A0			; annolen
	BSR.S	relocentr			; InstrInfo
	ADDQ.L	#8,A0
	BSR.S	relocentr			; rgbtable (not useful for most people)
	ADDQ.L	#4,A0			; skip channelSPlit
	BSR.S	relocentr			; NotationInfo
	BSR.S	relocentr			; songname
	ADDQ.L	#4,A0			; skip song name length
	BSR.S	relocentr			; MIDI dumps
	BSR.S	.relocmdd
	SUBQ.B	#1,D4			; songs left..?
	BCS.S	.rel_ex
	MOVE.L	D0,A0
	MOVE.L	(A0),D0
	BEQ.S	.rel_ex
	MOVE.L	D0,A2
	BSR.S	.relocp
	MOVEA.L	8(A2),A1
	BRA.S	.rel_lp
	.rel_ex:
	MOVEM.L	(SP)+,D2-D4/A2-A4
	RTS
	.relocp:
	LEA	mmd_songinfo(A2),A0
	BSR.S	relocentr
	ADDQ.L	#4,A0
	BSR.S	relocentr
	ADDQ.L	#4,A0
	BSR.S	relocentr
	ADDQ.L	#4,A0
	BRA.S	relocentr
	.relocb:
	MOVE.L	mmd_blockarr(A2),D0
	BEQ.S	.xlocb
	MOVEA.L	D0,A0
	MOVE.W	msng_numblocks(A1),D0
	SUBQ.B	#1,D0
	.rebl:
	BSR	relocentr
	DBF	D0,.rebl
	CMP.B	#'T',3(A2)		; MMD0 (= MCNT)
	BEQ.S	.xlocb
	CMP.B	#'1',3(A2)		; test MMD type
	BGE.S	.relocbi
	.xlocb:
	RTS
	.relocmdd:
	MOVE.L	D0,-(SP)
	TST.L	-(A0)
	BEQ.S	.xlocmdd
	MOVEA.L	(A0),A0
	MOVE.W	(A0),D0			; # of msg dumps
	ADDQ.L	#8,A0
	.mddloop:
	BEQ.S	.xlocmdd
	BSR	relocentr
	BSR.S	.relocdmp
	SUBQ.W	#1,D0
	BRA.S	.mddloop
	.xlocmdd:
	MOVE.L	(SP)+,D0
	RTS
	.relocdmp:
	MOVE.L	-4(A0),D3
	BEQ.S	.xlocdmp			; save
	exg.L	A0,D3
	ADDQ.L	#4,A0
	BSR	relocentr	 		; reloc data pointer
	MOVE.L	D3,A0			; restore
	.xlocdmp:
	RTS
	.relocbi:
	MOVE.W	msng_numblocks(A1),D0
	MOVE.L	A0,A3
	.Biloop:
	SUBQ.W	#1,D0
	BMI.S	.xlocdmp
	MOVE.L	-(A3),A0
	ADDQ.L	#4,A0
	BSR	relocentr			; BlockInfo ptr
	TST.L	-(A0)
	BEQ.S	.Biloop
	MOVE.L	(A0),A0
	BSR	relocentr			; hldata
	BSR	relocentr			; block name
	ADDQ.L	#4,A0			; skip blocknamelen
	BSR	relocentr			; pagetable
	TST.L	-(A0)
	BNE.S	.relocpgtbl
	BRA.S	.Biloop
	.relocmmD2sng:			; take care of the new features of MMD2s
	MOVE.L	mmd_songinfo(A2),A0
	LEA	msng_pseqs(A0),A0
	BSR	relocentr			; playseqtable
	BSR	relocentr			; sectiontable
	BSR	relocentr			; trackvols
	MOVE.W	2(A0),D0			; numpseqs
	MOVE.L	-12(A0),A0		; get back to playseqtable
	SUBQ.W	#1,D0
	.psqtblloop:
	BSR	relocentr
	DBF	D0,.psqtblloop
	RTS
	.relocpgtbl:
	MOVEA.L	(A0),A4			; page table list hdr
	MOVE.W	(A4),D2
	SUBQ.W	#1,D2
	LEA	4(A4),A0
	.pgtblloop:
	BSR	relocentr
	DBF	D2,.pgtblloop
	BRA	.Biloop
	RTS
	ENDC

; -------- _ChannelOff: Turn off a channel -------------------------------
_ChannelOff:	;D0 = channel #
		LEA	DB,A0
		LEA	trackdataptrs-DB(A0),A1
		LSL.W	#2,D0
		ADDA.W	D0,A1
		LSR.W	#2,D0
		MOVEA.L	(A1),A1
		MOVE.B	trk_outputdev(A1),D1
		BNE.S	.notamigatrk
		.notcomidi:
		CMP.B	#4,D0
		BGE.S	.notamigatrk
	; -------- TURN OFF AMIGA-CHANNEL ----------------------------------------
	IFNE SYNTH
		CLR.L	trk_synthptr(A1)
		CLR.B	trk_synthtype(A1)
	ENDC
		CLR.W	trk_soffset(A1)
		MOVEQ	#1,D1
		LSL.W	D0,D1
		MOVE.W	D1,$dff096
		.notamigatrk:
		RTS

; -------- SoundOff: Turn off all channels -------------------------------
SoundOff:
		MOVE.L	D2,-(SP)
		MOVEQ	#MAX_NUMTRACKS-1,D2
		.SO_loop0:
		MOVE.L	D2,D0
		BSR.S	_ChannelOff
		DBF	D2,.SO_loop0
		CLR.L	_module			;play nothing
		MOVE.L	(SP)+,D2
		.SO_RTS:
		RTS

; -------- _PlayNote: The note playing routine ---------------------------
_PlayNote:	;D7(w) = trk #, D1 = note #, D3(w) = instr # A3 = ADDr of instr
	; -------- CHECK INSTRUMENT (existence, type) ----------------------------
		MOVE.L	A3,D4
		BEQ.S	SoundOff\.SO_RTS
		MOVEQ	#0,D4
		BSET	D7,D4			;D4 is mask for this channel
		MOVEA.L	mmd_smplarr(A2),A0
		ADD.W	D3,D3			;D3 = instr.num << 2
		ADD.W	D3,D3
		MOVE.L	0(A0,D3.W),D5		;get ADDress of instrument
	IFNE CHECK
		BEQ.W	pnote_RTS			; NO!!!
	ENDC
	; -------- ADD TRANSPOSE -------------------------------------------------
		.inmem:
		ADD.B	msng_playtransp(A4),D1	;ADD play tranSPose
		ADD.B	inst_strans(A3),D1		;AND instr. tranSPose
		MOVE.B	trk_outputdev(A5),D3
		;BEQ.S	.pn_offami
		;BRA.S	noprevmidi		;dunno.. unsupported type
	; -------- TURN OFF CHANNEL DMA, IF REQUIRED -----------------------------
		.pn_offami:
		CMP.B	#4,D7
		BGE.S	.noDMAoff			;track #�>= 4: not an Amiga channel
		MOVE.L	D5,A1
	IFNE SYNTH
		TST.L	D5
		BEQ.S	.StpDMA
		TST.B	trk_synthtype(A5)
		BLE.S	.StpDMA			;prev. type = sample/hybrid
		CMP.W	#-1,4(A1)			;type == SYNTHETIC??
		BEQ.S	.noStpDMA
	ENDC
		.StpDMA:
		MOVE.W	D4,$dff096		;stop this channel (DMAcon)
		.noStpDMA:
	IFNE SYNTH
		CLR.L	trk_synthptr(A5)
	ENDC
		.noDMAoff:
		SUBQ.B	#1,D1
	; -------- TEST OUTPUT DEVICE AND BRANCH IF NOT STD ----------------------
		TST.B	trk_outputdev(A5)
		BNE.W	handlenonstdout
	; -------- SET SOME AMIGA-CHANNEL PARAMETERS -----------------------------
	IFNE CHECK
		CMP.W	#4,D7			;track > 3???
		BGE.W	pnote_RTS			;no Amiga instruments here!!!
	ENDC
	; hANDle decay (for tracks 0 - 3 only!!)
	IFNE HOLD
		CLR.B	trk_fadespd(A5)		;no fade yet..
		MOVE.B	trk_initdecay(A5),trk_decay(A5) ;set decay
	ENDC
		CLR.W	trk_vibroffs(A5)		;CLR viBRAto/tremolo offset
		OR.W	D4,DMAonmsk-DB(A6)
		MOVE.L	D5,A0
	IFNE SYNTH
	; -------- IF SYNTH NOTE, CALL SYNTH ROUTINE -----------------------------
		TST.W	4(A0)
		BMI.W	handleSynthnote
		CLR.B	trk_synthtype(A5)
	ENDC
	; -------- CHECK NOTE RANGE ----------------------------------------------
		.tlwtst0:
		TST.B	D1
		BPL.S	.notenot2low
		ADD.B	#12,D1			;note was too low, octave up
		BRA.S	.tlwtst0
		.notenot2low:
		CMP.B	#62,D1
		BLE.S	.endpttest
		SUB.B	#12,D1			;note was too high, octave down
		.endpttest:
		MOVEQ	#0,D2
		MOVEQ	#0,D3
		MOVEQ	#6,D4			;skip (stereo+hdr) offset
		LEA	_periodtable+32-DB(A6),A1
		MOVE.B	trk_finetune(A5),D2		;finetune value
		ADD.B	D2,D2
		ADD.B	D2,D2			;multiply by 4...
		EXT.W	D2			;extend
		MOVEA.L	0(A1,D2.W),A1		;period table ADDress
		MOVE.W	4(A0),D0			;(Instr hdr in A0)
		BTST	#5,D0
		BEQ.S	.gid_nostereo
		MOVE.B	D7,D5
		AND.B	#3,D5
		BEQ.S	.gid_nostereo		;ch 0/4 = play left (norM.)
		CMP.B	#3,D5
		BEQ.S	.gid_nostereo		;also for ch 3/7
		ADD.L	(A0),D4			;play right channel
		.gid_nostereo:
	IFNE IFFMOCT
		AND.W	#$F,D0
		BNE.S	.gid_notnormal		;note # in D1 (0 - ...)
	ENDC
		.gid_cont_ext:
		MOVE.L	A1,trk_periodtbl(A5)
		ADD.B	D1,D1
		MOVE.W	0(A1,D1.W),D5		;put period to D5
		MOVE.L	A0,D0
		MOVE.L	(A0),D1			;length
		ADD.L	D4,D0			;skip hdr AND stereo
		ADD.L	D0,D1			;sample end pointer
		MOVE.W	inst_repeat(A3),D2
		MOVE.W	inst_replen(A3),D3
	IFNE IFFMOCT
		BRA	.gid_setrept
		.gid_addtable:
		DC.B	0,6,12,18,24,30
		.gid_divtable:
		DC.B	31,7,3,15,63,127
		.gid_notnormal:
		CMP.W	#7,D0
		BLT.S	.gid_not_ext
		SUBA.W	#48,A1
		BRA.S	.gid_cont_ext
		.gid_not_ext:
		MOVE.L	D7,-(SP)
		MOVEQ	#0,D7
		MOVE.W	D1,D7
		DIVU	#12,D7			;octave #
		MOVE.L	D7,D5
		CMP.W	#6,D7			;if oct > 5, oct = 5
		BLT.S	.nohioct
		MOVEQ	#5,D7
		.nohioct:
		SWAP	D5			;note number in this oct (0-11) is in D5
		MOVE.L	(A0),D1
		CMP.W	#6,D0
		BLE.S	.nounrecit
		MOVEQ	#6,D0
		.nounrecit:
		ADD.B	.gid_addtable-1(PC,D0.W),D7
		MOVE.B	.gid_divtable-1(PC,D0.W),D0
		DIVU	D0,D1			;get length of the highest octave
		SWAP	D1
		CLR.W	D1
		SWAP	D1
		MOVE.L	D1,D0			;D0 AND D1 = length of the 1st oct
		MOVE.W	inst_repeat(A3),D2
		MOVE.W	inst_replen(A3),D3
		MOVEQ	#0,D6
		MOVE.B	.Shiftcnt(PC,D7.W),D6
		LSL.W	D6,D2
		LSL.W	D6,D3
		LSL.W	D6,D1
		MOVE.B	.mullencnt(PC,D7.W),D6
		MULU	D6,D0			;offset of this oct from 1st oct
		ADD.L	A0,D0			;ADD base ADDress to offset
		ADD.L	D4,D0			;skip header + stereo
		ADD.L	D0,D1
		MOVE.L	A1,trk_periodtbl(A5)
		ADD.B	.octstart(PC,D7.W),D5
		ADD.B	D5,D5
		MOVE.W	0(A1,D5.W),D5
		MOVE.L	(SP)+,D7
		BRA.S	.gid_setrept
		.Shiftcnt:
		DC.B	4,3,2,1,1,0,2,2,1,1,0,0,1,1,0,0,0,0
		DC.B	3,3,2,2,1,0,5,4,3,2,1,0,6,5,4,3,2,1
		.mullencnt:	
		DC.B	15,7,3,1,1,0,3,3,1,1,0,0,1,1,0,0,0,0
		DC.B	7,7,3,3,1,0,31,15,7,3,1,0,63,31,15,7,3,1
		.octstart:		
		DC.B	12,12,12,12,24,24,0,12,12,24,24,36,0,12,12,24,36,36
		DC.B	0,12,12,24,24,24,12,12,12,12,12,12,12,12,12,12,12,12
	ENDC
		.gid_setrept:
		ADD.L	D2,D2
		ADD.L	D0,D2			;rep. start pointer
		CMP.W	#1,D3
		BHI.S	.gid_noreplen2
		MOVEQ	#0,D3			;no repeat
		BRA.S	.gid_cont
		.gid_noreplen2:
		ADD.L	D3,D3
		ADD.L	D2,D3			;rep. end pointer
	; -------- CALCULATE START/END ADDRESSES ---------------------------------
		.gid_cont:
		MOVEQ	#0,D4
		MOVE.W	trk_soffset(A5),D4
		ADD.L	D4,D0
		CMP.L	D0,D1
		BHI.S	.pn_nooffsovf
		SUB.L	D4,D0
		.pn_nooffsovf:
		MOVEA.L	trk_audioaddr(A5),A1	;base of this channel's regs
		MOVE.L	D0,(A1)+			;push ac_ptr
		MOVEQ	#0,D4
		MOVE.B	trk_previnstr(A5),D4
		LEA	flags-DB(A6),A0
		BTST	#0,0(A0,D4.W)		;test flags.SSFLG_LOOP
		BNE.S	.repeat
		MOVE.L	#_chipzero,trk_sampleptr(A5)	;pointer of zero word
		MOVE.W	#1,trk_samplelen(A5)	;length: 1 word
		SUB.L	D0,D1
		LSR.L	#1,D1			;shift length right
		MOVE.W	D1,(A1)+			;AND push to ac_len
		BRA.S	.retsn1
		.repeat:
		MOVE.L	D2,trk_sampleptr(A5)
		MOVE.L	D3,D1
		SUB.L	D0,D1
		LSR.L	#1,D1
		MOVE.W	D1,(A1)+			;ac_len
		SUB.L	D2,D3
		LSR.L	#1,D3
		MOVE.W	D3,trk_samplelen(A5)
		.retsn1:
		MOVE.W	D5,trk_prevper(A5)
	IFNE SYNTH
		TST.B	trk_synthtype(A5)
		BNE.W	handleSynthnote\.hSn2
	ENDC
		.pnote_RTS: RTS

		handlenonstdout: RTS

	IFNE SYNTH

; -------- TRIGGER SYNTH NOTE, CLEAR PARAMETERS --------------------------
handleSynthnote:
		MOVE.B	D1,trk_prevnote2(A5)
		MOVE.L	A0,trk_synthptr(A5)
		CMP.W	#-2,4(A0)			;HYBRID??
		BNE.S	.hSn_nossn
		ST	trk_synthtype(A5)
		MOVEA.L	278(A0),A0		;yep, get the waveform pointer
		BRA.W	_PlayNote\.tlwtst0		;go AND play it
		.hSn_nossn:
		MOVE.B	#1,trk_synthtype(A5)
		LEA	_periodtable+32-DB(A6),A1
		MOVE.B	trk_finetune(A5),D0		;finetune value
		ADD.B	D0,D0
		ADD.B	D0,D0			;multiple by 4...
		EXT.W	D0			;extend
		MOVEA.L	0(A1,D0.W),A1		;period table ADDress
		SUBa.W	#48,A1
		MOVE.L	A1,trk_periodtbl(A5)	;save table ptr for synth periods
		ADD.W	D1,D1
		MOVE.W	0(A1,D1.W),D1
		MOVE.W	D1,trk_prevper(A5)
		CLR.L	trk_sampleptr(A5)
		.hSn2:
		LEA	trk_arpgoffs(A5),A1
		CLR.L	(A1)+
		CLR.L	(A1)+
		BTST	#0,trk_miscflags(A5)
		BNE.S	.hSn_cmdE			;cmd E given, don't cLEAr trk_wfcmd!
		CLR.W	(A1)
		.hSn_cmdE:
		ADDQ.L	#2,A1
		CLR.W	(A1)+
		CLR.L	(A1)+
		CLR.L	(A1)+
		CLR.L	(A1)+
		MOVE.L	#sinetable,(A1)+
		CLR.W	(A1)+
		MOVEA.L	trk_synthptr(A5),A0
		MOVE.W	18(A0),(A1)+
		CLR.B	(A1)
		MOVEQ	#64,D4
		RTS

synth_start:
		MOVE.W	trk_prevper(A5),D5
		.Synth_start2:
		MOVE.L	A3,-(SP)			;D0 = SynthPtr
		MOVE.L	D0,A0
		MOVEA.L	trk_audioaddr(A5),A3	;audio channel base ADDress
	; -------- SYNTHSOUND VOLUME SEQUENCE HANDLING ---------------------------
		SUBQ.B	#1,trk_volxcnt(A5)		;decrease execute counter..
		BGT.W	.Synth_wftbl		;not 0...go to waveform
		MOVE.B	trk_initvolxspd(A5),trk_volxcnt(A5) ;reset counter
		MOVE.B	trk_volchgspd(A5),D0	;volume change??
		BEQ.S	.Synth_nochgvol		;no.
		ADD.B	trk_synvol(A5),D0		;ADD previous volume
		BPL.S	.Synth_voln2l		;not negative
		MOVEQ	#0,D0			;was negative => 0
		.Synth_voln2l:
		CMP.B	#$40,D0			;too high??
		BLE.S	.Synth_voln2h		;not 2 high.
		MOVEQ	#$40,D0			;was 2 high => 64
		.Synth_voln2h:
		MOVE.B	D0,trk_synvol(A5)		;remember new...
		.Synth_nochgvol:
		MOVE.L	trk_envptr(A5),D1		;envelope pointer
		BEQ.S	.Synth_novolenv
		MOVEA.L	D1,A1
		MOVE.B	(A1)+,D0
		ADD.B	#128,D0
		LSR.B	#2,D0
		MOVE.B	D0,trk_synvol(A5)
		ADDQ.B	#1,trk_envcount(A5)
		BPL.S	.Synth_endenv
		CLR.B	trk_envcount(A5)
		MOVE.L	trk_envrestart(A5),A1
		.Synth_endenv:
		MOVE.L	A1,trk_envptr(A5)
		.Synth_novolenv:
		MOVE.W	trk_volcmd(A5),D0		;get table position ptr
		TST.B	trk_volwait(A5)		;WAI(t) active
		BEQ.S	.Synth_getvolcmd		;no
		SUBQ.B	#1,trk_volwait(A5)		;yep, decr wait ctr
		BLE.S	.Synth_getvolcmd		;0 => continue
		BRA.W	.Synth_wftbl		;> 0 => still wait
		.Synth_inccnt:
		ADDQ.B	#1,D0
		.Synth_getvolcmd:
		ADDQ.B	#1,D0			;advance pointer
		MOVE.B	21(A0,D0.W),D1		;get commAND
		BMI.S	.Synth_cmd		;negative = commAND
		MOVE.B	D1,trk_synvol(A5)		;set synthvol
		BRA.W	.Synth_endvol		;end of volume executing
		.Synth_cmd:
		AND.W	#$000f,D1
		ADD.B	D1,D1
		MOVE.W	.Synth_vtbl(PC,D1.W),D1
		JMP	.Syv(PC,D1.W)
		.Synth_vtbl:
		DC.W	.Syv_f0-.Syv,.Syv_f1-.Syv,.Syv_f2-.Syv,.Syv_f3-.Syv
		DC.W	.Syv_f4-.Syv,.Syv_f5-.Syv,.Syv_f6-.Syv
		DC.W	.Synth_endvol-.Syv,.Synth_endvol-.Syv,.Synth_endvol-.Syv
		DC.W	.Syv_fa-.Syv,.Syv_ff-.Syv,.Synth_endvol-.Syv
		DC.W	.Synth_endvol-.Syv,.Syv_fe-.Syv,.Syv_ff-.Syv
	; -------- VOLUME SEQUENCE COMMANDS --------------------------------------
		.Syv:
		.Syv_fe:
		MOVE.B	22(A0,D0.W),D0		;JMP
		BRA.S	.Synth_getvolcmd
		.Syv_f0:
		MOVE.B	22(A0,D0.W),trk_initvolxspd(A5) ;change volume ex. SPeed
		BRA.S	.Synth_inccnt
		.Syv_f1:
		MOVE.B	22(A0,D0.W),trk_volwait(A5)	;WAI(t)
		ADDQ.B	#1,D0
		BRA.S	.Synth_endvol
		.Syv_f3:
		MOVE.B	22(A0,D0.W),trk_volchgspd(A5)	;set volume slide up
		BRA.S	.Synth_inccnt
		.Syv_f2:
		MOVE.B	22(A0,D0.W),D1
		NEG.B	D1
		MOVE.B	D1,trk_volchgspd(A5)	;set volume slide down
		BRA.S	.Synth_inccnt
		.Syv_fa:
		MOVE.B	22(A0,D0.W),trk_wfcmd+1(A5)	;JWS (jump wform sequence)
		CLR.B	trk_wfwait(A5)
		BRA.S	.Synth_inccnt
		.Syv_f4:
		MOVE.B	22(A0,D0.W),D1
		BSR.S	.Synth_getwf
		CLR.L	trk_envrestart(A5)
		.Syv_f4end:
		MOVE.L	A1,trk_envptr(A5)
		CLR.B	trk_envcount(A5)
		BRA.W	.Synth_inccnt
		.Syv_f5:
		MOVE.B	22(A0,D0.W),D1
		BSR.S	.Synth_getwf
		MOVE.L	A1,trk_envrestart(A5)
		BRA.S	.Syv_f4end
		.Syv_f6:
		CLR.L	trk_envptr(A5)
		BRA.W	.Synth_getvolcmd
		.Synth_getwf:
		EXT.W	D1			;D1 = wform number, returns ptr in A1
		ADD.W	D1,D1			;create index
		ADD.W	D1,D1
		LEA	278(A0),A1
		ADDA.W	D1,A1
		MOVEA.L	(A1),A1			;get wform ADDress
		ADDQ.L	#2,A1			;skip length
		RTS
		.Syv_ff:
		SUBQ.B	#1,D0
		.Synth_endvol:
		MOVE.W	D0,trk_volcmd(A5)
		.Synth_wftbl:
		MOVEQ	#0,D0
		MOVE.B	trk_synvol(A5),D0
		MOVEQ	#0,D1
		MOVE.B	trk_prevvol(A5),D1
		MULU	D0,D1
		ASR.W	#6,D1
		MOVE.B	D1,trk_tempvol(A5)
		ADDA.W	#158,A0
	; -------- SYNTHSOUND WAVEFORM SEQUENCE HANDLING -------------------------
		SUBQ.B	#1,trk_wfxcnt(A5)			;decr. wf SPeed counter
		BGT.W	.Synth_arpeggio			;not yet...
		MOVE.B	trk_initwfxspd(A5),trk_wfxcnt(A5)	;restore SPeed counter
		MOVE.W	trk_wfcmd(A5),D0			;get table pos offset
		MOVE.W	trk_wfchgspd(A5),D1			;CHU/CHD ??
		BEQ.S	.Synth_TSTwfwai			;0 = no change
		.Wytanwet:
		ADD.W	trk_perchg(A5),D1			;ADD value to current change
		MOVE.W	D1,trk_perchg(A5)			;remember amount of change
		.Synth_TSTwfwai:
		TST.B	trk_wfwait(A5)			;WAI ??
		BEQ.S	.Synth_getwfcmd			;not waiting...
		SUBQ.B	#1,trk_wfwait(A5)			;decr wait counter
		BEQ.S	.Synth_getwfcmd			;waiting finished
		BRA.W	.Synth_arpeggio			;still sleep...
		.Synth_incwfc:
		ADDQ.B	#1,D0
		.Synth_getwfcmd:
		ADDQ.B	#1,D0				;advance position counter
		MOVE.B	-9(A0,D0.W),D1			;get commAND
		BMI.S	.Synth_wfcmd			;negative = commAND
		EXT.W	D1
		ADD.W	D1,D1
		ADD.W	D1,D1
		MOVEA.L	120(A0,D1.W),A1
		MOVE.W	(A1)+,ac_len(A3)			;push waveform length
		MOVE.L	A1,ac_ptr(A3)			;AND the new pointer
		BRA.W	.Synth_wfend			;no new commANDs now...
		.Synth_wfcmd:
		AND.W	#$000f,D1				;get the right nibble
		ADD.B	D1,D1				;* 2
		MOVE.W	.Synth_wfctbl(PC,D1.W),D1
		JMP	.Syw(PC,D1.W)			;jump to commAND
		.Synth_wfctbl:
		DC.W	.Syw_f0-.Syw,.Syw_f1-.Syw,.Syw_f2-.Syw,.Syw_f3-.Syw,.Syw_f4-.Syw
		DC.W	.Syw_f5-.Syw,.Syw_f6-.Syw,.Syw_f7-.Syw,.Synth_wfend-.Syw
		DC.W	.Synth_wfend-.Syw,.Syw_fa-.Syw,.Syw_ff-.Syw
		DC.W	.Syw_fc-.Syw,.Synth_getwfcmd-.Syw,.Syw_fe-.Syw,.Syw_ff-.Syw
	; -------- WAVEFORM SEQUENCE COMMANDS ------------------------------------
		.Syw:
		.Syw_f7:
		MOVE.B	-8(A0,D0.W),D1
		EXT.W	D1
		ADD.W	D1,D1
		ADD.W	D1,D1
		MOVEA.L	120(A0,D1.W),A1
		ADDQ.L	#2,A1
		MOVE.L	A1,trk_synvibwf(A5)
		BRA.S	.Synth_incwfc
		.Syw_fe:
		MOVE.B	-8(A0,D0.W),D0			;jump (JMP)
		BRA.S	.Synth_getwfcmd
		.Syw_fc:
		MOVE.W	D0,trk_arpsoffs(A5)			;new arpeggio begin
		MOVE.W	D0,trk_arpgoffs(A5)
		.Synth_findare:
		ADDQ.B	#1,D0
		TST.B	-9(A0,D0.W)
		BPL.S	.Synth_findare
		BRA.S	.Synth_getwfcmd
		.Syw_f0:
		MOVE.B	-8(A0,D0.W),trk_initwfxspd(A5)	;new waveform SPeed
		BRA	.Synth_incwfc
		.Syw_f1:
		MOVE.B	-8(A0,D0.W),trk_wfwait(A5)		;wait waveform
		ADDQ.B	#1,D0
		BRA.S	.Synth_wfend
		.Syw_f4:
		MOVE.B	-8(A0,D0.W),trk_synvibdep+1(A5)	;set viBRAto depth
		BRA.W	.Synth_incwfc
		.Syw_f5:
		MOVE.B	-8(A0,D0.W),trk_synthvibspd+1(A5)	;set viBRAto SPeed
		ADDQ.B	#1,trk_synthvibspd+1(A5)
		BRA.W	.Synth_incwfc
		.Syw_f2:
		MOVEQ	#0,D1				;set slide down
		MOVE.B	-8(A0,D0.W),D1
		.Synth_setsld:
		MOVE.W	D1,trk_wfchgspd(A5)
		BRA.W	.Synth_incwfc
		.Syw_f3:
		MOVE.B	-8(A0,D0.W),D1			;set slide up
		NEG.B	D1
		EXT.W	D1
		BRA.S	.Synth_setsld
		.Syw_f6:
		CLR.W	trk_perchg(A5)			;reset period
		MOVE.W	trk_prevper(A5),D5
		BRA.W	.Synth_getwfcmd
		.Syw_fa:
		MOVE.B	-8(A0,D0.W),trk_volcmd+1(A5)		;JVS (jump volume sequence)
		CLR.B	trk_volwait(A5)
		BRA.W	.Synth_incwfc
		.Syw_ff:
		SUBQ.B	#1,D0				;pointer = END - 1
		.Synth_wfend:
		MOVE.W	D0,trk_wfcmd(A5)
	; -------- HANDLE SYNTHSOUND ARPEGGIO ------------------------------------
		.Synth_arpeggio:
		MOVE.W	trk_arpgoffs(A5),D0
		BEQ.S	.Synth_viBRAto
		MOVEQ	#0,D1
		MOVE.B	-8(A0,D0.W),D1
		ADD.B	trk_prevnote2(A5),D1
		MOVEA.L	trk_periodtbl(A5),A1	;get period table
		ADD.W	D1,D1
		MOVE.W	0(A1,D1.W),D5
		ADDQ.B	#1,D0
		TST.B	-8(A0,D0.W)
		BPL.S	.Synth_noarpres
		MOVE.W	trk_arpsoffs(A5),D0
		.Synth_noarpres:
		MOVE.W	D0,trk_arpgoffs(A5)
	; -------- HANDLE SYNTHSOUND VIBRATO -------------------------------------
		.Synth_viBRAto:
		MOVE.W	trk_synvibdep(A5),D1	;get viBRAto depth
		BEQ.S	.Synth_RTS		;0 => no viBRAto
		MOVE.W	trk_synviboffs(A5),D0	;get offset
		LSR.W	#4,D0			;/ 16
		AND.W	#$1f,D0			;sinetable offset (0-31)
		MOVEA.L	trk_synvibwf(A5),A0
		MOVE.B	0(A0,D0.W),D0		;get a byte
		EXT.W	D0			;to word
		MULS	D1,D0			;amplify (* depth)
		ASR.W	#8,D0			;AND divide by 64
		ADD.W	D0,D5			;ADD viBRAto...
		MOVE.W	trk_synthvibspd(A5),D0	;viBRAto SPeed
		ADD.W	D0,trk_synviboffs(A5)	;ADD to offset
		.Synth_RTS:
		ADD.W	trk_perchg(A5),D5
		CMP.W	#113,D5			;overflow??
		BGE.S	.Synth_pern2h
		MOVEQ	#113,D1
		.Synth_pern2h:
		MOVE.L	(SP)+,A3
		RTS
	ENDC

sinetable:	DC.B	0,25,49,71,90,106,117,125,127,125,117,106,90,71,49
		DC.B	25,0,-25,-49,-71,-90,-106,-117,-125,-127,-125,-117
		DC.B	-106,-90,-71,-49,-25,0
		EVEN

_IntHandler:	;MOVE.W	#$0F0,$DFF180		; show rastertime left down to $12c
		MOVEM.L	D2-D7/A2-A6,-(SP)
	IFNE CIAB|VBLANK
		MOVEA.L	A1,A6			;get data base ADDress (int_Data)
	ENDC
	IFEQ CIAB|VBLANK
		LEA	DB,A6			;don't expect A1 to contain DB ADDress
	ENDC
		TST.B	bpmcounter-DB(A6)
		BMI.S	.plr_nobpm
		SUBQ.B	#1,bpmcounter-DB(A6)
		BLE.S	.plr_bpmcnt0
		BRA.W	.plr_exit
		.plr_bpmcnt0:
		MOVE.B	#4,bpmcounter-DB(A6)
		.plr_nobpm:
		MOVEA.L	_module-DB(A6),A2
		MOVE.L	A2,D0
		BEQ.W	.plr_exit
		TST.W	mmd_pstate(A2)
		BEQ.W	.plr_exit
		CLR.W	DMAonmsk-DB(A6)
		MOVEA.L	mmd_songinfo(A2),A4
		MOVEQ	#0,D3
		MOVE.B	mmd_counter(A2),D3
		ADDQ.B	#1,D3
		CMP.B	msng_tempo2(A4),D3
		BGE.S	.plr_pnewnote		;play new note
		MOVE.B	D3,mmd_counter(A2)
		BNE.W	.nonewnote		;do just fx
	; --- new note!!
		.plr_pnewnote:
		CLR.B	mmd_counter(A2)
		TST.W	blkdelay-DB(A6)
		BEQ.S	.plr_noblkdelay
		SUBQ.W	#1,blkdelay-DB(A6)
		BNE.W	.nonewnote
	; --- now start to play it
	; -------- GET ADDRESS OF NOTE DATA --------------------------------------
		.plr_noblkdelay:
		MOVE.W	mmd_pblock(A2),D0
		BSR.W	GetNoteDataAddr
		MOVEQ	#0,D7			;number of track
		MOVEQ	#0,D4
	IFNE PLAYMMD0
		CMP.B	#'1',3(A2)
		SGE	D5			;D5 set -> >= MMD1
	ENDC
		LEA	trackdataptrs-DB(A6),A1
	; -------- TRACK LOOP (FOR EACH TRACK) -----------------------------------
	IFNE INSTR_TRACKING				; ## KONEY MOD ##
		MOVEM.L	A2,-(SP)
		LEA	MED_TRK_0_INST,A2		; KONEY
	ENDC					; ## KONEY MOD ##
		.plr_loop0:
		MOVEA.L	(A1)+,A5			; get ADDress of this track's struct
	; ---------------- get the note numbers
		MOVEQ	#0,D3
	IFNE PLAYMMD0
		TST.B	D5
		BNE.S	.plr_mmd1_1
		MOVE.B	(A3)+,D0
		MOVE.B	(A3),D3
		ADDQ.L	#2,A3
		LSR.B	#4,D3
		BCLR	#7,D0
		BEQ.S	.plr_bseti4
		BSET	#4,D3
		.plr_bseti4:
		BCLR	#6,D0
		BEQ.S	.plr_bseti5
		BSET	#5,D3
		.plr_bseti5:
		MOVE.B	D0,trk_currnote(A5)
		BEQ.S	.plr_nngok
		MOVE.B	D0,(A5)
		BRA.S	.plr_nngok
		.plr_mmd1_1:
	ENDC
		MOVE.B	(A3)+,D0			;get the number of this note
		BPL.S	.plr_nothinote
		MOVEQ	#0,D0
		.plr_nothinote:
		MOVE.B	D0,trk_currnote(A5)
		BEQ.S	.plr_nosetprevn
		MOVE.B	D0,(A5)
		.plr_nosetprevn:
		MOVE.B	(A3),D3			;instrument number
		ADDQ.L	#3,A3			;adv. to next track
	; ---------------- check if there's an instrument number
		.plr_nngok:
		AND.W	#$3F,D3
		BEQ.S	.noinstnum
	; ---------------- finally, save the number
	IFNE INSTR_TRACKING				; ## KONEY MOD ##
		MOVE.B	D3,(A2)+			; KONEY: TRACK INSTR#
		MOVE.B	D0,(A2)			; KONEY: TRACK INSTR#
		SUBQ	#1,A2			; KONEY: TRACK INSTR#
	ENDC					; ## KONEY MOD ##
		SUBQ.B	#1,D3
		MOVE.B	D3,trk_previnstr(A5)	;remember instr. number!
	; ---------------- get the pointer of data's of this sample in Song-struct
		MOVE.W	D3,D0
		ASL.W	#3,D3
		LEA	0(A4,D3.W),A0		;A0 contains now ADDress of it
		MOVE.L	A0,trk_previnstra(A5)
	; ---------------- get volume
		MOVE.B	inst_svol(A0),trk_prevvol(A5)	;vol of this instr
		MOVE.B	inst_strans(A0),trk_stransp(A5)
	; ---------------- remember some values of this instrument
		LEA	holdvals-DB(A6),A0
		ADDA.W	D0,A0
	IFNE HOLD
		MOVE.B	(A0),trk_inithold(A5)	;hold
		MOVE.B	63(A0),trk_initdecay(A5)	;decay
	ENDC
		MOVE.B	2*63(A0),trk_finetune(A5)	;finetune
		MOVE.B	6*63(A0),trk_outputdev(A5)	;output dev
	; ---------------- remember tranSPose
		CLR.W	trk_soffset(A5)		;sample offset
		CLR.B	trk_miscflags(A5)		;misc.
		.noinstnum:
		ADDQ.W	#1,D7
	IFNE INSTR_TRACKING				; ## KONEY MOD ##
		ADDQ	#2,A2			; KONEY: TRACK INSTR#
	ENDC					; ## KONEY MOD ##
		CMP.W	numtracks-DB(A6),D7
		BLT	.plr_loop0
	IFNE INSTR_TRACKING				; ## KONEY MOD ##
		MOVEM.L	(SP)+,A2
	ENDC					; ## KONEY MOD ##
		BSR.W	DoPreFXLoop
	; -------- NOTE PLAYING LOOP ---------------------------------------------
		MOVEQ	#0,D7
		LEA	trackdataptrs-DB(A6),A1
		.plr_loop2:
		MOVEA.L	(A1)+,A5
		TST.B	trk_fxtype(A5)
		BNE.S	.plr_loop2_end
		MOVE.B	trk_currnote(A5),D1
		BEQ.S	.plr_loop2_end
	; ---------------- play
		MOVE.L	A1,-(SP)
		EXT.W	D1
		MOVEQ	#0,D3
		MOVE.B	trk_previnstr(A5),D3		;instr #
		MOVEA.L	trk_previnstra(A5),A3		;instr data ADDress
		MOVE.B	trk_inithold(A5),trk_noteoffcnt(A5)	;initialize hold
		BNE.S	.plr_nohold0			;not 0 -> OK
		ST	trk_noteoffcnt(A5)			;0 -> hold = 0xff (-1)
	; ---------------- AND finally:
		.plr_nohold0:
		BSR	_PlayNote				;play it
		MOVE.L	(SP)+,A1
		.plr_loop2_end:
		ADDQ.W	#1,D7
		CMP.W	numtracks-DB(A6),D7
		BLT.S	.plr_loop2
	; -------- THE REST... ---------------------------------------------------
	IFNE SONG_POS_TRACKING
		MOVE.W	mmd_pseqnum(A2),MED_SONG_POS	;SONG POSITION | KONEY
	ENDC
	IFNE BLOCK_LINE_TRACKING
		MOVE.W	mmd_pline(A2),MED_BLOCK_LINE	;LINE POSITION | KONEY
	ENDC
		BSR.S	AdvSngPtr
		.nonewnote:
		BSR.W	DoFX
		.plr_endfx:
		BSR	_StartDMA			;turn on DMA
		.plr_exit:
		MOVEM.L	(SP)+,D2-D7/A2-A6
	IFNE VBLANK
		MOVEQ	#0,D0
	ENDC
		RTS

	; AND advance song pointers
AdvSngPtr:
		MOVE.L	mmd_pblock(A2),fxplineblk-DB(A6) ;store pline/block for fx
		MOVE.W	nextblockline-DB(A6),D1
		BEQ.S	.plr_advlinenum
		CLR.W	nextblockline-DB(A6)
		SUBQ.W	#1,D1
		BRA.S	.plr_linenumset
		.plr_advlinenum:
		MOVE.W	mmd_pline(A2),D1		;get current line #
		ADDQ.W	#1,D1			;advance line number
	IFNE STEP_SEQ
		MOVE.W	MED_STEPSEQ_POS,D0		; UPDATE STEPSEQUENCER
		ADDQ.W	#1,D0			; INCREASE STEPSEQ | KONEY
		ANDI.W	#$F,D0			; POSITION (0-15 = 16 LEDS)
		MOVE.W	D0,MED_STEPSEQ_POS		; QUICKER TO DO HERE
	ENDC
		.plr_linenumset:
		CMP.W	numlines-DB(A6),D1 		;advance block?
		BHI.S	.plr_chgblock		;yes.
		TST.B	nextblock-DB(A6)		;commAND F00/1Dxx?
		BEQ.W	.plr_nochgblock		;no, don't change block
	; -------- CHANGE BLOCK? -------------------------------------------------
		.plr_chgblock:
		TST.B	nxtnoclrln-DB(A6)
		BNE.S	.plr_noclrln
		MOVEQ	#0,D1			;cLEAr line number
		.plr_noclrln:
		TST.W	mmd_pstate(A2)		;play block or play song
		BPL.W	.plr_nonewseq		;play block only...
		CMP.B	#'2',3(A2)		;MMD2?
		BNE.S	.plr_noMMD2_0
	; ********* BELOW CODE FOR MMD2 ONLY ************************************
	; -------- CHANGE SEQUENCE -----------------------------------------------
		.plr_skipseq:
		MOVE.W	mmd_pseq(A2),D0		;actually stored as << 2
		MOVEA.L	msng_pseqs(A4),A1		;ptr to playseqs
		MOVEA.L	0(A1,D0.W),A0		;A0 = ptr to curr PlaySeq
		MOVE.W	mmd_pseqnum(A2),D0		;get play sequence number
		TST.B	nextblock-DB(A6)
		BMI.S	.plr_noadvseq		;Bxx sets nextblock to -1
		ADDQ.W	#1,D0			;advance sequence number
		.plr_noadvseq:
		CMP.W	40(A0),D0			;is this the highest seq number??
		BLT.S	.plr_notagain		;no.
	; -------- CHANGE SECTION ------------------------------------------------
		MOVE.W	mmd_psecnum(A2),D0		;get section number
		ADDQ.W	#1,D0			;increase..
		CMP.W	msng_songlen(A4),D0		;highest section?
		BLT.S	.plr_nohisec
		MOVEQ	#0,D0			;yes.
		.plr_nohisec:
		MOVE.W	D0,mmd_psecnum(A2)		;push back.
		ADD.W	D0,D0
		MOVEA.L	msng_sections(A4),A0	;section table
		MOVE.W	0(A0,D0.W),D0		;new playseqlist number
		ADD.W	D0,D0
		ADD.W	D0,D0
		MOVE.W	D0,mmd_pseq(A2)
		MOVEA.L	0(A1,D0.W),A0		;A0 = ptr to new PlaySeq
		MOVEQ	#0,D0			;playseq OFFSET = 0
	; -------- FETCH BLOCK NUMBER FROM SEQUENCE ------------------------------
		.plr_notagain:
		MOVE.W	D0,mmd_pseqnum(A2)		;remember new playseq pos
	IFNE START_POS
		CMP.W	MED_START_POS,D0		;START_POS REACHED? | KONEY
		BLO.S	.plr_chgblock		;GO INCREMENT AGAIN | KONEY
		MOVE.W	#0,MED_START_POS		;SAVE START POSITION | KONEY
	ENDC
		ADD.W	D0,D0
		MOVE.W	42(A0,D0.W),D0		;get number of the block
		BPL.S	.plr_changeblk		;NEG. values for future expansion
		BRA.S	.plr_skipseq		;(skip them)
	; ********* BELOW CODE FOR MMD0/MMD1 ONLY *******************************
		.plr_noMMD2_0:
		MOVE.W	mmd_pseqnum(A2),D0		;get play sequence number
		TST.B	nextblock-DB(A6)
		BMI.S	.plr_noadvseq_b		;Bxx sets nextblock to -1
		ADDQ.W	#1,D0			;advance sequence number
		.plr_noadvseq_b:
		CMP.W	msng_songlen(A4),D0		;is this the highest seq number??
		BLT.S	.plr_notagain_b		;no.
		MOVEQ	#0,D0			;yes: restart song
		.plr_notagain_b:
		MOVE.B	D0,mmd_pseqnum+1(A2)	;remember new playseq-#
	IFNE START_POS
		CMP.W	MED_START_POS,D0		;START_POS REACHED? | KONEY
		BLO.W	.plr_chgblock		;GO INCREMENT AGAIN | KONEY
		MOVE.W	#0,MED_START_POS		;SAVE START POSITION | KONEY
	ENDC
		LEA	msng_playseq(A4),A0		;offset of sequence table
		MOVE.B	0(A0,D0.W),D0		;get number of the block
	; ********* BELOW CODE FOR BOTH FORMATS *********************************
		.plr_changeblk:
	IFNE CHECK
		CMP.W	msng_numblocks(A4),D0	;beyond last block??
		BLT.S	.plr_nolstblk		;no..
		MOVEQ	#0,D0			;play block 0
	;IFNE STOP_AT_END
		;BRA.W	_RemPlayer		;STOP MUSIC | KONEY
	;ENDC
	ENDC
		.plr_nolstblk:
		MOVE.W	D0,mmd_pblock(A2)		;store block number
		.plr_nonewseq:
		CLR.W	nextblock-DB(A6)		;cLEAr this if F00 set it
	; ------------------------------------------------------------------------
		.plr_nochgblock:
		MOVE.W	D1,mmd_pline(A2)		;set new line number
	IFNE HOLD
		LEA	trackdataptrs-DB(A6),A5
		MOVE.W	mmd_pblock(A2),D0		;pblock
		BSR.W	GetBlockAddr
		MOVE.W	mmd_pline(A2),D0		;play line
		MOVE.B	msng_tempo2(A4),D3		;interrupts/note
	IFNE PLAYMMD0
		CMP.B	#'1',3(A2)
		BGE.S	.plr_mmD1_2
		MOVE.B	(A0),D7			;# of tracks
		MOVE.W	D0,D1
		ADD.W	D0,D0			;D0 * 2
		ADD.W	D1,D0			;+ D0 = D0 * 3
		MULU	D7,D0
		LEA	2(A0,D0.W),A3
		SUBQ.B	#1,D7
		.plr_chkholdb:
		MOVEA.L	(A5)+,A1			;track data
		TST.B	trk_noteoffcnt(A1)		;hold??
		BMI.S	.plr_holdendb		;no.
		MOVE.B	(A3),D1			;get the 1st byte..
		BNE.S	.plr_holD1b
		MOVE.B	1(A3),D1
		AND.B	#$f0,D1
		BEQ.S	.plr_holdendb		;don't hold
		BRA.S	.plr_holD2b
		.plr_holD1b:
		AND.B	#$3f,D1			;note??
		BEQ.S	.plr_holD2b		;no, cont hold..
		MOVE.B	1(A3),D1
		AND.B	#$0f,D1			;get cmd
		SUBQ.B	#3,D1			;is there commAND 3 (slide)
		BNE.S	.plr_holdendb		;no -> end holding
		.plr_holD2b:
		ADD.B	D3,trk_noteoffcnt(A1)	;continue holding...
		.plr_holdendb:
		ADDQ.L	#3,A3			;next note
		DBF	D7,.plr_chkholdb
		RTS
		.plr_mmD1_2:
	ENDC
		MOVE.W	(A0),D7			;# of tracks
		ADD.W	D0,D0
		ADD.W	D0,D0			;D0 = D0 * 4
		MULU	D7,D0
		LEA	8(A0,D0.L),A3
		SUBQ.B	#1,D7
		.plr_chkhold:
		MOVEA.L	(A5)+,A1			;track data
		TST.B	trk_noteoffcnt(A1)		;hold??
		BMI.S	.plr_holdend		;no.
		MOVE.B	(A3),D1			;get the 1st byte..
		BNE.S	.plr_holD1
		MOVE.B	1(A3),D0
		AND.B	#$3F,D0
		BEQ.S	.plr_holdend		;don't hold
		BRA.S	.plr_holD2
		.plr_holD1:
		AND.B	#$7f,D1			;note??
		BEQ.S	.plr_holD2		;no, cont hold..
		MOVE.B	2(A3),D1
		SUBQ.B	#3,D1			;is there commAND 3 (slide)
		BNE.S	.plr_holdend		;no -> end holding
		.plr_holD2:
		ADD.B	D3,trk_noteoffcnt(A1)	;continue holding...
		.plr_holdend:
		ADDQ.L	#4,A3			;next note
		DBF	D7,.plr_chkhold
	ENDC
		RTS

; *******************************************************************
; DoPreFXLoop:	Loop AND call DoPreFX
; *******************************************************************
DoPreFXLoop:
	; -------- PRE-FX COMMAND HANDLING LOOP ----------------------------------
		MOVEQ	#0,D5			;commAND page count
		.plr_loop1:
		MOVE.W	mmd_pblock(A2),D0
		BSR.W	GetBlockAddr
		MOVE.W	D5,D1
		MOVE.W	mmd_pline(A2),D2
		BSR.W	GetCmdPointer
		MOVEA.L	A0,A3
		MOVEQ	#0,D7			;cLEAr track count
		LEA	trackdataptrs-DB(A6),A1
		.plr_loop1_1:
		MOVEA.L	(A1)+,A5
		CLR.B	trk_fxtype(A5)
		MOVE.B	(A3),D0			;commAND #
		BEQ.S	.plr_loop1_end
		MOVEQ	#0,D4
		MOVE.B	1(A3),D4			;data byte
	IFNE PLAYMMD0
		CMP.B	#3,D6			;if adv == 3 -> MMD0
		BNE.S	.doprefx_mmD12mask
		AND.W	#$0F,D0
		BRA.S	.doprefx_mmD0maskd
		.doprefx_mmD12mask:
	ENDC
	IFNE SKIP_TO_NEXT
		BTST	#6,$BFE001		;IF LMB		| KONEY
		BNE.S	.doprefx_mmD0maskd
		MOVE.W	#$0F,D0			;MOCK A F00 CMD	| KONEY
		MOVE.B	#0,D4			;TO SKIP TO NEXT	| KONEY
		BRA.S	DoPreFX			;BLOCK IN SEQ	| KONEY
	ENDC

		AND.W	#$1F,D0
		.doprefx_mmD0maskd:
		BSR.S	DoPreFX
		OR.B	D0,trk_fxtype(A5)
		.plr_loop1_end:
		ADDA.W	D6,A3			;next track...
		ADDQ.W	#1,D7
		CMP.W	numtracks-DB(A6),D7
		BLT.S	.plr_loop1_1
		ADDQ.W	#1,D5
		CMP.W	numpages-DB(A6),D5
		BLS.S	.plr_loop1
		RTS

; *******************************************************************
; DoPreFX: Perform effects that must be handled before note playing
; *******************************************************************
; args:		A6 = DB		D0 = command number (w)
;		A5 = track data	D5 = note number
;		A4 = song		D4 = data
;				D7 = track #
; returns:	D0 = 0: play -	D0 = 1: don't play
rtplay:		MACRO
		MOVEQ	#0,D0
		RTS
		ENDM
rtnoplay:		MACRO
		MOVEQ	#1,D0
		RTS
		ENDM
DoPreFX:
		ADD.B	D0,D0			;* 2
		MOVE.W	.f_table(PC,D0.W),D0
		JMP	.fst(PC,D0.W)
		.f_table:	
		DC.W	.fx-.fst,.fx-.fst,.fx-.fst,.f_03-.fst,.fx-.fst,.fx-.fst,.fx-.fst,.fx-.fst
		DC.W	.f_08-.fst,.f_09-.fst,.fx-.fst,.f_0b-.fst,.f_0c-.fst,.fx-.fst,.f_0e-.fst,.f_0f-.fst
		DC.W	.fx-.fst,.fx-.fst,.fx-.fst,.fx-.fst,.fx-.fst,.f_15-.fst,.f_16-.fst,.fx-.fst
		DC.W	.fx-.fst,.f_19-.fst,.fx-.fst,.fx-.fst,.f_1c-.fst,.f_1d-.fst,.f_1e-.fst,.f_1f-.fst
		.fst:
	; ---------------- tempo (F)
		.f_0f:
		TST.B	D4			;test effect qual..
		BEQ	.fx0fchgblck		;if effect qualifier (last 2 #'s)..
		CMP.B	#$f0,D4			;..is zero, go to next block
		BHI.S	.fx0fSPecial		;if it's F1-FF something SPecial
	; ---------------- just an ordinary "change tempo"-request
	IFNE CIAB
		MOVEQ	#0,D0			;will happen!!!
		MOVE.B	D4,D0
		BSR	_SetTempo			;change The Tempo
	ENDC
		.fx:	rtplay
	; ---------------- no, it was FFx, something SPecial will happen!!
		.fx0fSPecial:
		CMP.B	#$f2,D4
		BEQ.S	.f_1f
		CMP.B	#$f4,D4
		BEQ.S	.f_1f
		CMP.B	#$f5,D4
		BNE.S	.isfxfe
	; ---------------- FF2 (or 1Fxx)
		.f_1f:
	IFNE HOLD
		MOVE.B	trk_inithold(A5),trk_noteoffcnt(A5) ;initialize hold
		BNE.S	.f_1fRTS			;not 0 -> OK
		ST	trk_noteoffcnt(A5)		;0 -> hold = 0xff (-1)
	ENDC
		.f_1fRTS:	rtnoplay
		.isfxfe:
		CMP.B	#$fe,D4
		BNE.S	.notcmdfe
	; ---------------- it was FFE, stop playing
		CLR.W	mmd_pstate(A2)
	IFNE CIAB
		MOVEA.L	craddr-DB(A6),A0
		BCLR	#0,(A0)
	ENDC
		BSR.W	SoundOff
		ADDA.W	#8,SP			;2 SUBroutine levels
		BRA.W	_IntHandler\.plr_exit
		.f_ffe_no8: rtplay
		.notcmdfe:
		CMP.B	#$fd,D4			;change period
		BNE.S	.isfxff
	; ---------------- FFD, change the period, don't replay the note
	IFNE CHECK
		CMP.W	#4,D7			;no tracks above 4, thank you!!
		BGE.S	.f_ff_RTS
	ENDC
		MOVE.L	trk_periodtbl(A5),D1	;period table
		BEQ.S	.f_1fRTS
		MOVEA.L	D1,A0
		MOVE.B	trk_currnote(A5),D0
		SUBQ.B	#1,D0			;SUB 1 to make "real" note number
	IFNE CHECK
		BMI.S	.f_1fRTS
	ENDC
		ADD.B	msng_playtransp(A4),D0
		ADD.B	trk_stransp(A5),D0
		ADD.W	D0,D0
		BMI.S	.f_1fRTS
		MOVE.W	0(A0,D0.W),trk_prevper(A5)	;get & push the period
		rtnoplay
		.isfxff:
		CMP.B	#$ff,D4			;note off??
		BNE.S	.f_ff_RTS
		MOVE.W	D7,D0
		MOVE.L	A1,-(SP)
		BSR.W	_ChannelOff
		MOVE.L	(SP)+,A1
		.f_ff_RTS: rtplay
	; ---------------- F00, called Pattern Break in ST
		.fx0fchgblck:
		MOVE.B	#1,nextblock-DB(A6)		;next block????...YES!!!! (F00)
		BRA.S	.f_ff_RTS
	; ---------------- was not Fxx, then it's something else!!
		.f_0e:
	IFNE SYNTH	; NO E00 CMD IF SYNTH DISABLED	| KONEY
	IFNE CHECK
		CMP.B	#4,D7
		BGE.S	.f_0e_RTS
	ENDC
		BSET	#0,trk_miscflags(A5)	; THIS IS FOR SYNTHSOUNDS
		MOVE.B	D4,trk_wfcmd+1(A5)		; set waveform commAND position ptr
		.f_0e_RTS: rtplay
	ENDC		; NO E00 CMD IF SYNTH DISABLED	| KONEY
	; ---------------- change volume
		.f_0c:
		MOVE.B	D4,D0
		BPL.S	.plr_nosetdefvol
		AND.B	#$7F,D0
	IFNE CHECK
		CMP.B	#64,D0
		BGT.S	.go_nocmd
	ENDC
		MOVEQ	#0,D1
		MOVE.B	trk_previnstr(A5),D1
		ASL.W	#3,D1
		MOVE.B	D0,inst_svol(A4,D1.W)	;set new svol
		BRA.S	.plr_setvol
		.plr_nosetdefvol:
		BTST	#4,msng_flags(A4)		;look at flags
		BNE.S	.volhex
		LSR.B	#4,D0			;get number from left
		MULU	#10,D0			;number of tens
		MOVE.B	D4,D1			;get again
		AND.B	#$0f,D1			;this time don't get tens
		ADD.B	D1,D0			;ADD them
		.volhex:
	IFNE CHECK
		CMP.B	#64,D0
		BHI.S	.go_nocmd
	ENDC
		.plr_setvol:
		MOVE.B	D0,trk_prevvol(A5)
		.go_nocmd: rtplay
	; ---------------- tempo2 change??
		.f_09:
	IFNE CHECK
		AND.B	#$1F,D4
		BNE.S	.fx9chk
		MOVEQ	#$20,D4
	ENDC
		.fx9chk:
		MOVE.B	D4,msng_tempo2(A4)
		.f_09_RTS: rtplay
	; ---------------- block delay
		.f_1e:
		TST.W	blkdelay-DB(A6)
		BNE.S	.f_1e_RTS
		ADDQ.W	#1,D4
		MOVE.W	D4,blkdelay-DB(A6)
		.f_1e_RTS: rtplay
	; ---------------- finetune
		.f_15:
	IFNE CHECK
		CMP.B	#7,D4
		BGT.S	.f_15_RTS
		CMP.B	#-8,D4
		BLT.S	.f_15_RTS
	ENDC
		MOVE.B	D4,trk_finetune(A5)
		.f_15_RTS: rtplay
	; ---------------- repeat loop
		.f_16:
		TST.B	D4
		BNE.S	.plr_dorpt
		MOVE.W	mmd_pline(A2),rptline-DB(A6)
		BRA.S	.f_16_RTS
		.plr_dorpt:
		TST.W	rptcounter-DB(A6)
		BEQ.S	.plr_newrpt
		SUBQ.W	#1,rptcounter-DB(A6)
		BEQ.S	.f_16_RTS
		BRA.S	.plr_setrptline
		.plr_newrpt:
		MOVE.B	D4,rptcounter+1-DB(A6)
		.plr_setrptline:
		MOVE.W	rptline-DB(A6),D0
		ADDQ.W	#1,D0
		MOVE.W	D0,nextblockline-DB(A6)
		.f_16_RTS: rtplay
	; ---------------- preset change
		.f_1c:
		CMP.B	#$80,D4
		BHI.S	.f_1c_RTS
		MOVEQ	#0,D1
		MOVE.B	trk_previnstr(A5),D1
		ADD.W	D1,D1
		LEA	ext_midipsets-DB(A6),A0
		EXT.W	D4
		MOVE.W	D4,0(A0,D1.W)		;set MIDI preset
		.f_1c_RTS: rtplay
	; ---------------- note off time set??
		.f_08:
	IFNE HOLD
		MOVE.B	D4,D0
		LSR.B	#4,D4			;extract left  nibble
		AND.B	#$0f,D0			; "   "  right  "  "
		MOVE.B	D4,trk_initdecay(A5)	;left = decay
		MOVE.B	D0,trk_inithold(A5)		;right = hold
	ENDC
		rtplay
	; ---------------- sample begin offset
		.f_19:
		LSL.W	#8,D4
		MOVE.W	D4,trk_soffset(A5)
		.f_19_RTS: rtplay
	; ---------------- cmd Bxx, "position jump"
		.f_0b:
	IFNE CHECK
		CMP.B	#'2',3(A2)
		BEQ.S	.chk0b_mmD2
		CMP.W	msng_songlen(A4),D4
		BHI.S	.f_0b_RTS
		BRA.S	.chk0b_end
		.chk0b_mmD2:
		MOVE.W	mmd_pseq(A2),D0		;get seq number
		MOVEA.L	msng_pseqs(A4),A0		;ptr to playseqs
		MOVEA.L	0(A0,D0.W),A0		;A0 = ptr to curr PlaySeq
		CMP.W	40(A0),D4			;test song length
		BHI.S	.f_0b_RTS
		.chk0b_end:
	ENDC
		MOVE.W	D4,mmd_pseqnum(A2)
		ST	nextblock-DB(A6)		; = 1
		.f_0b_RTS: rtplay
	; ---------------- cmd 1Dxx, jump to next seq, line # SPecified
		.f_1d:
		MOVE.W	#$1ff,nextblock-DB(A6)
		ADDQ.W	#1,D4
		MOVE.W	D4,nextblockline-DB(A6)
		rtplay
	; ---------------- try portamento (3)
		.f_03:
	IFNE CHECK
		CMP.W	#4,D7
		BGE.S	.f_03_RTS
	ENDC
		MOVEQ	#0,D0
		MOVE.B	trk_currnote(A5),D0
		SUBQ.B	#1,D0			;SUBtract note number
		BMI.S	.plr_setfx3SPd		;0 -> set new SPeed
		MOVE.L	trk_periodtbl(A5),D1
		BEQ.S	.f_03_RTS
		MOVEA.L	D1,A0
		ADD.B	msng_playtransp(A4),D0	;play tranSPose
		ADD.B	trk_stransp(A5),D0		;AND instrument tranSPose
		BMI.S	.f_03_RTS			;again.. too low
		ADD.W	D0,D0
		MOVE.W	0(A0,D0.W),trk_porttrgper(A5)	;period of this note is the target
		.plr_setfx3SPd:
		TST.B	D4			;qual??
		BEQ.S	.f_03_RTS			;0 -> do nothing
		MOVE.B	D4,trk_prevportspd(A5)	;store SPeed
		.f_03_RTS: rtnoplay

; *******************************************************************
; DoFX: HANDle effects, hold/fade etc.
; *******************************************************************
DoFX:
		MOVEQ	#0,D3
		MOVE.B	mmd_counter(A2),D3
	IFNE HOLD
		LEA	trackdataptrs-DB(A6),A1
	; Loop 1: Hold/Fade hANDling
		MOVEQ	#0,D7			;cLEAr track count
		.dofx_loop1:
		MOVEA.L	(A1)+,A5
		BSR.W	HoldAndFade
		ADDQ.W	#1,D7
		CMP.W	numtracks-DB(A6),D7
		BLT.S	.dofx_loop1
	ENDC
	; Loop 2: Track commAND hANDling
		MOVEQ	#0,D5			;commAND page count
		.dofx_loop2:
		MOVE.W	fxplineblk-DB(A6),D0
		BSR.W	GetBlockAddr
		MOVEA.L	A0,A3
	IFNE PLAYMMD0
		CMP.B	#'1',3(A2)
		BGE.S	.dofx_sbd_nommD0
		BSR.W	StoreBlkDimsMMD0
		BRA.S	.dofx_sbd_mmD0
		.dofx_sbd_nommD0:
	ENDC
		BSR.W	StoreBlockDims
		.dofx_sbd_mmD0:
		MOVE.W	D5,D1
		MOVE.W	fxplineblk+2-DB(A6),D2
		MOVEA.L	A3,A0
		BSR.S	GetCmdPointer
		MOVEA.L	A0,A3
		MOVEQ	#0,D7			;cLEAr track count
		LEA	trackdataptrs-DB(A6),A1
		.dofx_loop2_1:
		MOVEA.L	(A1)+,A5
		MOVEQ	#0,D4
		MOVE.B	(A3),D0			;commAND #
		MOVE.B	1(A3),D4			;data byte
	IFNE PLAYMMD0
		CMP.B	#3,D6			;if adv == 3 -> MMD0
		BNE.S	.dofx_mmD12mask
		AND.W	#$0F,D0
		BRA.S	.dofx_mmD0maskd
		.dofx_mmD12mask:
	ENDC
		AND.W	#$1F,D0
		.dofx_mmD0maskd:
		TST.B	trk_fxtype(A5)
		BGT.S	.dofx_lenD2_1		;1 = skip
		BNE.S	.dofx_lenD2_1
		.dofx_chfx:
		BSR.W	ChannelFX
		.dofx_lenD2_1:
		ADDA.W	D6,A3			;next track...
		ADDQ.W	#1,D7
		CMP.W	numtracks-DB(A6),D7
		BLT.S	.dofx_loop2_1
		ADDQ.W	#1,D5
		CMP.W	numpages-DB(A6),D5
		BLS.S	.dofx_loop2
	; Loop 3: Updating audio hardware
		MOVEQ	#0,D7			;cLEAr track count
		LEA	trackdataptrs-DB(A6),A1
		.dofx_loop3:
		MOVEA.L	(A1)+,A5
	IFNE HOLD
		TST.B	trk_fxtype(A5)
		BNE.S	.dofx_lenD3		;only in case 0 (norm)
	ENDC
	IFEQ HOLD
		CMP.W	#4,D7
		BGE.S	.dofx_stopl3
	ENDC
		BSR.W	UpdatePerVol
		.dofx_lenD3:
		ADDQ.W	#1,D7
		CMP.W	numtracks-DB(A6),D7
		BLT.S	.dofx_loop3
		.dofx_stopl3:
		RTS

; *******************************************************************
; GetCmdPointer: Return commAND pointer for track 0
; *******************************************************************
; args:		A0 = block pointer
;		D1 = page number
;		D2 = line number
;		A2 = module
; result:		A0 = commAND pointer (i.e. trk 0 note + 2)
;		D6 = track advance (bytes)
; scratches:	D0, D1, D2, A0
; Note: no num_pages check! If numpages > 0 it can be assumed that
; extra pages exist.
GetCmdPointer:
	IFNE PLAYMMD0
		CMP.B	#'1',3(A2)
		BLT.S	.GetCmdPtrMMD0
	ENDC
		MULU	(A0),D2			;D2 = line # * numtracks
		ADD.L	D2,D2			;D2 *= 2...
		SUBQ.W	#1,D1
		BMI.S	.gcp_page0
		MOVEA.L	4(A0),A0
		MOVEA.L	12(A0),A0
		ADD.W	D1,D1
		ADD.W	D1,D1
		MOVEA.L	4(A0,D1.W),A0		;commAND data
		ADDA.L	D2,A0
		MOVEQ	#2,D6
		RTS
		.gcp_page0:
		ADD.L	D2,D2			;D2 *= 4
		LEA	10(A0,D2.L),A0		;offs: 4 = header, 2 = note
		MOVEQ	#4,D6			;track advance (bytes)
		RTS
	IFNE PLAYMMD0
		.GetCmdPtrMMD0:
		MOVEQ	#0,D0
		MOVE.B	(A0),D0			;get numtracks
		MULU	D0,D2			;line # * numtracks
		MOVE.W	D2,D0
		ADD.W	D2,D2
		ADD.W	D0,D2			; *= 3...
		LEA	3(A0,D2.L),A0		;offs: 2 = header, 1 = note
		MOVEQ	#3,D6
		RTS
	ENDC

; *******************************************************************
; GetBlockAddr: Return pointer to block
; *******************************************************************
; args:		D0 = block number
; result:		A0 = block pointer
; scratches:	D0, A0
GetBlockAddr:
		MOVEA.L	mmd_blockarr(A2),A0
		ADD.W	D0,D0
		ADD.W	D0,D0
		MOVEA.L	0(A0,D0.W),A0
		RTS

; *******************************************************************
; GetNoteDataAddr: Check & return ADDr. of current note
; *******************************************************************
;args:		D0 = pblock	A6 = DB
;returns:		A3 = ADDress
;scratches:	D0, A0, D1
GetNoteDataAddr:
		BSR.W	GetBlockAddr
		MOVEA.L	A0,A3
	IFNE PLAYMMD0
		CMP.B	#'1',3(A2)
		BLT.S	.GetNDAddrMMD0
	ENDC
		BSR.W	StoreBlockDims
		MOVE.W	numlines-DB(A6),D1
		MOVE.W	mmd_pline(A2),D0
		CMP.W	D1,D0			;check if block end exceeded...
		BLS.S	.plr_nolinex
		MOVE.W	D1,D0
		.plr_nolinex:
		ADD.W	D0,D0
		ADD.W	D0,D0			;D0 = D0 * 4
		MULU	numtracks-DB(A6),D0
		LEA	8(A3,D0.L),A3		;ADDress of current note
		RTS

	IFNE PLAYMMD0
		.GetNDAddrMMD0:	BSR.W	StoreBlkDimsMMD0
		MOVE.W	numlines-DB(A6),D1
		MOVE.W	mmd_pline(A2),D0
		CMP.W	D1,D0			;check if block end exceeded...
		BLS.S	.plr_nolinex2
		MOVE.W	D1,D0
		.plr_nolinex2:
		MOVE.W	D0,D1
		ADD.W	D0,D0
		ADD.W	D1,D0			;D0 = D0 * 3
		MULU	numtracks-DB(A6),D0
		LEA	2(A3,D0.L),A3		;ADDress of current note
		RTS
	ENDC

; *******************************************************************
; StoreBlockDims: Store block dimensions
; *******************************************************************
; args:		A0 = block ptr, A6 = DB
StoreBlockDims:
		MOVE.L	(A0)+,numtracks-DB(A6)	;numtracks & lines
		TST.L	(A0)			;BlockInfo
		BEQ.S	.Sbd_1page
		MOVEA.L	(A0),A0
		MOVE.L	12(A0),D0			;BlockInfo.pagetable
		BEQ.S	.Sbd_1page
		MOVEA.L	D0,A0
		MOVE.W	(A0),numpages-DB(A6)	;num_pages
		RTS
		.Sbd_1page:
		CLR.W	numpages-DB(A6)
		RTS

	IFNE PLAYMMD0
StoreBlkDimsMMD0:
		CLR.W	numpages-DB(A6)
		MOVEQ	#0,D0
		MOVE.B	(A0)+,D0			;numtracks
		MOVE.W	D0,numtracks-DB(A6)
		MOVE.B	(A0),D0			;numlines
		MOVE.W	D0,numlines-DB(A6)
		RTS
	ENDC

; *******************************************************************
; HoldAndFade: HANDle hold/fade
; *******************************************************************
; args:		A5 = track data
;		A6 = DB
;		D7 = track #
; scratches:	D0, D1, A0

	IFNE HOLD
HoldAndFade:
	IFNE CHECK
		CMP.W	#4,D7
		BGE.W	.plr_haf_midi		;no non-MIDI effects in tracks 4 - 15
	ENDC
		TST.B	trk_noteoffcnt(A5)
		BMI.S	.plr_haf_noholdexp
		SUBQ.B	#1,trk_noteoffcnt(A5)
		BPL.S	.plr_haf_noholdexp
	IFNE SYNTH
		TST.B	trk_synthtype(A5)		;synth/hybrid??
		BEQ.S	.plr_nosyndec
		MOVE.B	trk_decay(A5),trk_volcmd+1(A5) ;set volume commAND pointer
		CLR.B	trk_volwait(A5)		;abort WAI
		BRA.S	.plr_haf_noholdexp
	ENDC
		.plr_nosyndec:
		MOVE.B	trk_decay(A5),trk_fadespd(A5)	;set fade...
		BNE.S	.plr_haf_noholdexp		;if > 0, don't stop sound
		MOVEQ	#0,D0
		BSET	D7,D0
		MOVE.W	D0,$dff096		;shut DMA...
		.plr_haf_noholdexp:
		MOVE.B	trk_fadespd(A5),D0		;fade??
		BEQ.S	.plr_haf_dofx		;no.
		SUB.B	D0,trk_prevvol(A5)
		BPL.S	.plr_nofade2low
		CLR.B	trk_prevvol(A5)
		CLR.B	trk_fadespd(A5)		;fade no more
		.plr_nofade2low:
		.plr_haf_dofx:
		CLR.B	trk_fxtype(A5)
		.plr_haf_RTS:
		RTS
		.plr_haf_midi:
		ST	trk_fxtype(A5)
		RTS
;hold
	ENDC

; *******************************************************************
; ChannelFX:	Do an effect on a channel
; *******************************************************************
;args:		D3 = counter
;		A4 = song struct	D4 = commAND qual (long, byte used)
;		A5 = track data ptr	
;		A6 = DB		D0 = commAND (long, byte used)
;		D7 = track (channel) number
;scratches:	D0, D1, D4, A0

ChannelFX:
		ADD.B	D0,D0	;* 2
		MOVE.W	.fx_table(PC,D0.W),D0
		JMP	.fxs(PC,D0.W)
		.fx_table:
		DC.W	.fx_00-.fxs,.fx_01-.fxs,.fx_02-.fxs,.fx_03-.fxs,.fx_04-.fxs
		DC.W	.fx_05-.fxs,.fx_06-.fxs,.fx_07-.fxs,.fx_xx-.fxs,.fx_xx-.fxs
		DC.W	.fx_0a-.fxs,.fx_xx-.fxs,.fx_0c-.fxs,.fx_0d-.fxs,.fx_xx-.fxs
		DC.W	fx_0f-.fxs
		DC.W	.fx_10-.fxs,.fx_11-.fxs,.fx_12-.fxs,.fx_13-.fxs,.fx_14-.fxs
		DC.W	.fx_xx-.fxs,.fx_xx-.fxs,.fx_xx-.fxs,.fx_18-.fxs,.fx_xx-.fxs
		DC.W	.fx_1a-.fxs,.fx_1b-.fxs,.fx_xx-.fxs,.fx_xx-.fxs,.fx_xx-.fxs
		DC.W	.fx_1f-.fxs
		.fxs:
	; **************************************** Effect 01 ******
		.fx_01:
		TST.B	D3
		BNE.S	.fx_01nocnt0
		BTST	#5,msng_flags(A4)		;FLAG_STSLIDE??
		BNE.S	.fx_01RTS
		.fx_01nocnt0:
		MOVE.W	trk_prevper(A5),D0
		SUB.W	D4,D0
		CMP.W	#113,D0
		BGE.S	.fx_01noovf
		MOVE.W	#113,D0
		.fx_01noovf:
		MOVE.W	D0,trk_prevper(A5)
		.fx_xx:			;.fx_xx is just a RTS
		.fx_01RTS: RTS
	; **************************************** Effect 11 ******
		.fx_11:
		TST.B	D3
		BNE.S	.fx_11RTS
		SUB.W	D4,trk_prevper(A5)
		.fx_11RTS: RTS
	; **************************************** Effect 02 ******
		.fx_02:
		TST.B	D3
		BNE.S	.fx_02nocnt0
		BTST	#5,msng_flags(A4)
		BNE.S	.fx_02RTS
		.fx_02nocnt0:
		ADD.W	D4,trk_prevper(A5)
		.fx_02RTS: RTS
	; **************************************** Effect 12 ******
		.fx_12:
		TST.B	D3
		BNE.S	.fx_12RTS
		ADD.W	D4,trk_prevper(A5)
		.fx_12RTS: RTS
	; **************************************** Effect 00 ******
		.fx_00:
		TST.B	D4			;both fxqualifiers are 0s: no arpeggio
		BEQ.S	.fx_00RTS
		MOVE.L	D3,D0
		DIVU	#3,D0
		SWAP	D0
		SUBQ.B	#1,D0
		BGT.S	.fx_arp2
		BLT.S	.fx_arp0
		AND.B	#$0f,D4
		BRA.S	.fx_doarp
		.fx_arp0:
		LSR.B	#4,D4
		BRA.S	.fx_doarp
		.fx_arp2:
		MOVEQ	#0,D4
		.fx_doarp:
		MOVE.B	(A5),D0
		SUBQ.B	#1,D0			;-1 to make it 0 - 127
		ADD.B	msng_playtransp(A4),D0	;ADD play tranSPose
		ADD.B	trk_stransp(A5),D0		;ADD instrument tranSPose
		ADD.B	D0,D4
		MOVE.L	trk_periodtbl(A5),D1
		BEQ.S	.fx_00RTS
		MOVEA.L	D1,A0
		ADD.B	D0,D0
		MOVE.W	0(A0,D0.W),D0		;base note period
		ADD.B	D4,D4
		SUB.W	0(A0,D4.W),D0		;calc difference from base note
		MOVE.W	D0,trk_arpadjust(A5)
		.fx_00RTS: RTS
	; **************************************** Effect 04 ******
		.fx_14:
		MOVE.B	#6,trk_vibshift(A5)
		BRA.S	.vib_cont
		.fx_04:
		MOVE.B	#5,trk_vibshift(A5)
		.vib_cont:
		TST.B	D3
		BNE.S	.nonvib
		MOVE.B	D4,D1
		BEQ.S	.nonvib
		AND.W	#$0f,D1
		BEQ.S	.plr_chgvibSPd
		MOVE.W	D1,trk_vibrsz(A5)
		.plr_chgvibSPd:
		AND.B	#$f0,D4
		BEQ.S	.nonvib
		LSR.B	#3,D4
		AND.B	#$3e,D4
		MOVE.B	D4,trk_vibrspd(A5)
		.nonvib:
		MOVE.B	trk_vibroffs(A5),D0
		LSR.B	#2,D0
		AND.W	#$1f,D0
		MOVEQ	#0,D1
		LEA	sinetable(PC),A0
		MOVE.B	0(A0,D0.W),D0
		EXT.W	D0
		MULS	trk_vibrsz(A5),D0
		MOVE.B	trk_vibshift(A5),D1
		ASR.W	D1,D0
		MOVE.W	D0,trk_vibrAdjust(A5)
		MOVE.B	trk_vibrspd(A5),D0
		ADD.B	D0,trk_vibroffs(A5)
		.fx_04RTS: RTS
	; **************************************** Effect 06 ******
		.fx_06:
		TST.B	D3
		BNE.S	.fx_06nocnt0
		BTST	#5,msng_flags(A4)
		BNE.S	.fx_04RTS
		.fx_06nocnt0:
		BSR.S	.plr_voLSLide		;Volume slide
		BRA.S	.nonvib			;+ ViBRAto
	; **************************************** Effect 07 ******
		.fx_07:
		TST.B	D3
		BNE.S	.nontre
		MOVE.B	D4,D1
		BEQ.S	.nontre
		AND.W	#$0f,D1
		BEQ.S	.plr_chgtreSPd
		MOVE.W	D1,trk_tremsz(A5)
		.plr_chgtreSPd:
		AND.B	#$f0,D4
		BEQ.S	.nontre
		LSR.B	#2,D4
		AND.B	#$3e,D4
		MOVE.B	D4,trk_tremspd(A5)
		.nontre:
		MOVE.B	trk_tremoffs(A5),D0
		LSR.B	#3,D0
		AND.W	#$1f,D0
		LEA	sinetable(PC),A0
		MOVE.B	0(A0,D0.W),D1
		EXT.W	D1
		MULS	trk_tremsz(A5),D1
		ASR.W	#7,D1
		MOVE.B	trk_tremspd(A5),D0
		ADD.B	D0,trk_tremoffs(A5)
		ADD.B	trk_prevvol(A5),D1
		BPL.S	.tre_pos
		MOVEQ	#0,D1
		.tre_pos:
		CMP.B	#64,D1
		BLE.S	.tre_no2hi
		MOVEQ	#64,D1
		.tre_no2hi:
		MOVE.B	D1,trk_tempvol(A5)
		RTS
	; ********* VOLUME SLIDE FUNCTION *************************
		.plr_voLSLide:
		MOVE.B	D4,D0
		MOVEQ	#0,D1
		MOVE.B	trk_prevvol(A5),D1		;MOVE previous vol to D1
		AND.B	#$f0,D0
		BNE.S	.crescendo
		SUB.B	D4,D1			;SUB from prev. vol
		.voltest0:
		BPL.S	.novolover64
		MOVEQ	#0,D1			;volumes under zero not accepted
		BRA.S	.novolover64
		.crescendo:
		LSR.B	#4,D0
		ADD.B	D0,D1
		.voltest:
		CMP.B	#64,D1
		BLE.S	.novolover64
		MOVEQ	#64,D1
		.novolover64:
		MOVE.B	D1,trk_prevvol(A5)
		.voLSL_RTS: RTS
	; **************************************** Effect 0D/0A ***
		.fx_0a:
		.fx_0d:
		TST.B	D3
		BNE.S	.plr_voLSLide
		BTST	#5,msng_flags(A4)
		BEQ.S	.plr_voLSLide
		RTS
	; **************************************** Effect 05 ******
		.fx_05:
		TST.B	D3
		BNE.S	.fx_05nocnt0
		BTST	#5,msng_flags(A4)
		BNE.S	.fx_05RTS
		.fx_05nocnt0:
		BSR.S	.plr_voLSLide
		BRA.S	.fx_03nocnt0
		.fx_05RTS: RTS
	; **************************************** Effect 1A ******
		.fx_1a:
		TST.B	D3
		BNE.S	.voLSL_RTS
		MOVE.B	trk_prevvol(A5),D1
		ADD.B	D4,D1
		BRA.S	.voltest
	; **************************************** Effect 1B ******
		.fx_1b:
		TST.B	D3
		BNE.S	.voLSL_RTS
		MOVE.B	trk_prevvol(A5),D1
		SUB.B	D4,D1
		BRA.S	.voltest0
	; **************************************** Effect 03 ******
		.fx_03:
		TST.B	D3
		BNE.S	.fx_03nocnt0
		BTST	#5,msng_flags(A4)
		BNE.S	.fx_03RTS
		.fx_03nocnt0:
		MOVE.W	trk_porttrgper(A5),D0	;D0 = target period
		BEQ.S	.fx_03RTS
		MOVE.W	trk_prevper(A5),D1		;D1 = curr. period
		MOVE.B	trk_prevportspd(A5),D4	;get prev. SPeed
		CMP.W	D0,D1
		BHI.S	.Subper			;curr. period > target period
		ADD.W	D4,D1			;ADD the period
		CMP.W	D0,D1
		BGE.S	.targreached
		BRA.S	.targnreach
		.Subper:
		SUB.W	D4,D1			;SUBtract
		CMP.W	D0,D1			;compare current period to target period
		BGT.S	.targnreach
		.targreached:
		MOVE.W	trk_porttrgper(A5),D1	;EVENtually push target period
		CLR.W	trk_porttrgper(A5)		;now we can forget everything
		.targnreach:
		MOVE.W	D1,trk_prevper(A5)
		.fx_03RTS: RTS
	; **************************************** Effect 13 ******
		.fx_13:
		CMP.B	#3,D3
		BGE.S	.fx_13RTS			;if counter < 3
		NEG.W	D4
		MOVE.W	D4,trk_vibrAdjust(A5)	;SUBtract effect qual...
		.fx_13RTS: RTS
	; *********************************************************
		.fx_0c:
		TST.B	D3
		BNE.S	.fx_13RTS
		.dvc_0:
		MOVE.B	trk_prevvol(A5),D1
		RTS
	; **************************************** Effect 10 ******
		.fx_10:
		RTS
	; **************************************** Effect 18 ******
		.fx_18:
		CMP.B	D4,D3
		BNE.S	.fx_18RTS
		CLR.B	trk_prevvol(A5)
		.fx_18RTS: RTS
	; **************************************** Effect 1F ******
		.fx_1f:
		MOVE.B	D4,D1
		LSR.B	#4,D4			;note delay
		BEQ.S	.nonotedelay
		CMP.B	D4,D3			;compare to counter
		BLT.S	.fx_18RTS			;tick not reached
		BNE.S	.nonotedelay
		BRA	fx_0f\.playfxnote		;trigger note
		.nonotedelay:
		AND.W	#$0f,D1			;retrig?
		BEQ.S	.fx_18RTS
		MOVEQ	#0,D0
		MOVE.B	D3,D0
		DIVU	D1,D0
		SWAP	D0			;get modulo of counter/tick
		TST.W	D0
		BNE.S	.fx_18RTS
		BRA	fx_0f\.playfxnote		;retrigger
	; **************************************** Effect 0E - SYNTHSOUND
	;	.fx_0e:
	;	MOVE.W	#$0F0F,$DFF180		; show rastertime left down to $12c
	;	.fx_0eRTS: RTS
	; **************************************** Effect 0F ******
	; see below...
	; *********************************************************

; *******************************************************************
; UpdatePerVol:	Update audio registers (period & volume) after FX
; *******************************************************************
; args:		A6 = DB	D7 = channel #
;		A5 = track data
; scratches:	D0, D1, A0, D5
UpdatePerVol:
		MOVE.W	trk_prevper(A5),D5
	IFNE SYNTH
		MOVE.L	trk_synthptr(A5),D0
		BEQ.S	.plr_upv_nosynth
		MOVE.L	A1,-(SP)
		BSR.W	synth_start
		MOVE.L	(SP)+,A1
	ENDC
		.plr_upv_nosynth:
		ADD.W	trk_vibrAdjust(A5),D5
		SUB.W	trk_arpadjust(A5),D5
		CLR.L	trk_vibrAdjust(A5)		;CLR both adjusts
		MOVEA.L	trk_audioaddr(A5),A0
		MOVE.W	D5,ac_per(A0)		;push period
		MOVEQ	#0,D0
		MOVE.B	trk_tempvol(A5),D0
		BPL.S	.plr_upv_setvol
		MOVE.B	trk_prevvol(A5),D0
		.plr_upv_setvol:
		ST	trk_tempvol(A5)
	; -------- GetRelVol: Calculate track volume -----------------------------
	; track # = D7, note vol = D0, song = A4
	IFNE RELVOL
		MULU	trk_trackvol(A5),D0		;D0 = master v. * track v. * volume
		LSR.W	#8,D0
	ENDC
		MOVE.B	D0,ac_vol+1(A0)
		RTS

; **** a separate routine for hANDling commAND 0F
fx_0f:
		CMP.B	#$f1,D4
		BNE.S	.no0ff1
		CMP.B	#3,D3
		BEQ.S	.playfxnote
		RTS
		.no0ff1:
		CMP.B	#$f2,D4
		BNE.S	.no0ff2
		CMP.B	#3,D3
		BEQ.S	.playfxnote
		RTS
		.no0ff2:
		CMP.B	#$f3,D4
		BNE.S	.no0ff3
		MOVE.B	D3,D0
		BEQ.S	.cF_RTS
		AND.B	#1,D0			;is 2 or 4
		BNE.S	.cF_RTS
		.playfxnote:
		MOVEQ	#0,D1
		MOVE.B	trk_currnote(A5),D1		;get note # of curr. note
		BEQ.S	.cF_RTS
		MOVE.B	trk_noteoffcnt(A5),D0	;get hold counter
		BMI.S	.pfxn_nohold		;no hold, or hold over
		ADD.B	D3,D0			;increase by counter val
		BRA.S	.pfxn_hold
		.pfxn_nohold:
		MOVE.B	trk_inithold(A5),D0		;get initial hold
		BNE.S	.pfxn_hold
		ST	D0
		.pfxn_hold:
		MOVE.B	D0,trk_noteoffcnt(A5)
		MOVEM.L	A1/A3/D3/D6,-(SP)
		MOVEQ	#0,D3
		MOVE.B	trk_previnstr(A5),D3	;AND prev. sample #
		MOVEA.L	trk_previnstra(A5),A3
		BSR	_PlayNote
		.pndone_0ff:
		MOVEM.L	(SP)+,A1/A3/D3/D6
		.cF_RTS:	RTS
		.no0ff3:
		CMP.B	#$f4,D4			;triplet cmd 1
		BNE.S	.no0ff4
		MOVEQ	#0,D0
		MOVE.B	msng_tempo2(A4),D0
		DIVU	#3,D0
		CMP.B	D0,D3
		BEQ.S	.playfxnote
		RTS
		.no0ff4:
		CMP.B	#$f5,D4			;triplet cmd 2
		BNE.S	.no0ff5
		MOVEQ	#0,D0
		MOVE.B	msng_tempo2(A4),D0
		DIVU	#3,D0
		ADD.W	D0,D0
		CMP.B	D0,D3
		BEQ.S	.playfxnote
		RTS
		.no0ff5:
		CMP.B	#$f8,D4			;f8 = filter off
		BEQ.S	.plr_filteroff
		CMP.B	#$f9,D4			;f9 = filter on
		BNE.S	.cF_RTS
		BCLR	#1,$bfe001
		BSET	#0,msng_flags(A4)
		RTS
		.plr_filteroff:
		BSET	#1,$bfe001
		BCLR	#0,msng_flags(A4)
		RTS

; -------- HANDLE DMA WAIT (PROCESSOR-INDEPENDENT) -----------------------
_Wait1line:
		MOVE.W	D0,-(SP)
		.Wl0:
		MOVE.B	$dff007,D0
		.Wl1:
		CMP.B	$dff007,D0
		BEQ.S	.Wl1
		DBF	D1,.Wl0
		MOVE.W	(SP)+,D0
		RTS
		.pushnewvals:
		MOVEA.L	(A1)+,A5
		LSR.B	#1,D0
		bcc.S	.rpnewv
		MOVE.L	trk_sampleptr(A5),D1
		BEQ.S	.rpnewv
		MOVEA.L	trk_audioaddr(A5),A0
		MOVE.L	D1,ac_ptr(A0)
		MOVE.W	trk_samplelen(A5),ac_len(A0)
		.rpnewv:	RTS

; -------- AUDIO DMA ROUTINE ---------------------------------------------
_StartDMA:	;This small routine turns on audio DMA
		MOVE.W	DMAonmsk-DB(A6),D0		;DMAonmsk contains the mask of
		BEQ.S	.SDMA_noDMAon		;the channels that must be turned on
	IFNE INSTR_TRACKING
		LEA	MED_TRK_0_COUNT,A4		; #KONEY# RESET AUDIO LEVELS
		BTST	#$0,D0
		BEQ.S	.noCh0
		MOVE.W	#$0,(A4)
		.noCh0:
		BTST	#$1,D0
		BEQ.S	.noCh1
		MOVE.W	#$0,2(A4)
		.noCh1:
		BTST	#$2,D0
		BEQ.S	.noCh2
		MOVE.W	#$0,4(A4)
		.noCh2:
		BTST	#$3,D0
		BEQ.S	.noCh3
		MOVE.W	#$0,6(A4)
		.noCh3:
	ENDC
		BSET	#15,D0			;DMAF_SETCLR: set these bits in DMAcon
		MOVEQ	#80,D1
	; The following line makes the playroutine one scanline slower. If your
	; song works well without the following instruction, you can LEAve it out.
	IFNE SYNTH
		ADD.W	D1,D1			;sometimes double wait time is required
	ENDC
		BSR.S	_Wait1line
		MOVE.W	D0,$dff096		;do that!!!
		MOVEQ	#80,D1
		BSR.S	_Wait1line
		LEA	trackdataptrs-DB(A6),A1
		BSR.S	_Wait1line\.pushnewvals
		BSR.S	_Wait1line\.pushnewvals
		BSR.S	_Wait1line\.pushnewvals
		BRA.S	_Wait1line\.pushnewvals
		.SDMA_noDMAon:
		RTS

_SetTempo:
	IFNE CIAB
		MOVE.L	_module-DB(A6),D1
		BEQ.S	.ST_x
		MOVE.L	D1,A0
		MOVEA.L	mmd_songinfo(A0),A0
		BTST	#5,msng_flags2(A0)
		BNE.S	.ST_bpm
		CMP.W	#10,D0			;If tempo <= 10, use SoundTracker tempo
		BHI.S	.calctempo
		SUBQ.B	#1,D0
		ADD.W	D0,D0
		MOVE.W	.Sttempo+2(PC,D0.W),D1
		BRA.S	.pushtempo
		.calctempo:	
		MOVE.L	timerdiv-DB(A6),D1
		DIVU	D0,D1
		.pushtempo:
		MOVEA.L	craddr+4-DB(A6),A0
		MOVE.B	D1,(A0)			;AND set the CIA timer
		LSR.W	#8,D1
		MOVEA.L	craddr+8-DB(A6),A0
		MOVE.B	D1,(A0)
	ENDC
		.ST_x:	RTS			; vv-- These values are the SoundTracker tempos (approx.)
		.Sttempo:	
		DC.W	$0f00
	IFNE CIAB
		DC.W	2417,4833,7250,9666,12083,14500,16916,19332,21436,24163
		.ST_bpm:	
		MOVE.B	msng_flags2(A0),D1
		AND.W	#$1F,D1
		ADDQ.B	#1,D1
		MULU	D1,D0
		MOVE.L	bpmdiv-DB(A6),D1
		DIVU	D0,D1
		BRA.S	.pushtempo
	ENDC

; *************************************************************************
; *************************************************************************
; ***********	P U B L I C   F U N C T I O N S	***************
; *************************************************************************
; *************************************************************************

	IFEQ EASY
		XDEF	_InitModule,_PlayModule
		XDEF	_InitPlayer,_RemPlayer,_StopPlayer
		XDEF	_ContModule
	ENDC

; *************************************************************************
; InitModule(A0 = module) -- extract expansion data etc.. from V3.xx module
; *************************************************************************
_InitModule:
		MOVEM.L	A2-A3/D2,-(SP)
		MOVE.L	A0,-(SP)
	IFNE RELVOL
		MOVEA.L	mmd_songinfo(A0),A1		;MMD0song
		MOVE.B	msng_mastervol(A1),D0	;D0 = mastervol
		EXT.W	D0
		LEA	trackdataptrs,A2
		CMP.B	#'2',3(A0)		;MMD2?
		BNE.S	.IM_mmD01
		MOVE.W	msng_numtracks(A1),D1
		SUBQ.W	#1,D1
		MOVEA.L	msng_trkvoltbl(A1),A1
		BRA.S	.IM_loop0
		.IM_mmD01:
		LEA	msng_trkvol(A1),A1		;A1 = trkvol
		MOVEQ	#MAX_MMD1_TRACKS-1,D1
		.IM_loop0:
		MOVE.B	(A1)+,D2			;get vol...
		EXT.W	D2
		MOVE.L	(A2)+,A3			;pointer to track data
		MULU	D0,D2			;mastervol * trackvol
		LSR.W	#4,D2
		MOVE.W	D2,trk_trackvol(A3)
		DBF	D1,.IM_loop0
	ENDC
	IFNE SYNTH
		LEA	trackdataptrs,A2
		MOVEQ	#3,D1
		.IM_loop1:
		MOVE.L	(A2)+,A3
		CLR.L	trk_synthptr(A3)
		CLR.B	trk_synthtype(A3)
		DBF	D1,.IM_loop1
	ENDC
		LEA	holdvals,A2
		MOVEA.L	A0,A3
		MOVE.L	mmd_expdata(A0),D0		;expdata...
		BEQ.W	.IM_CLRhlddec
		MOVE.L	D0,A1
		MOVE.L	4(A1),D0			;exp_smp
		BEQ.S	.IM_CLRhlddec		;again.. nothing
		MOVE.L	D0,A0			;InstrExt...
		MOVE.W	8(A1),D2			;# of entries
		BEQ.S	.IM_CLRhlddec
		BEQ.W	.IM_CLRhlddec
		SUBQ.W	#1,D2			;-1 (for DBF)
		MOVE.W	10(A1),D0			;entry size
		MOVEA.L	mmd_songinfo(A3),A3		;MMD0song
		.IM_loop2:
		CLR.B	2*63(A2)			;cLEAr finetune
		CMP.W	#3,D0
		BLE.S	.IM_noftune
		MOVE.B	3(A0),126(A2)		;InstrExt.finetune -> finetune
		.IM_noftune:
		CLR.B	3*63(A2)			;cLEAr flags
		CMP.W	#6,D0
		BLT.S	.IM_noflags
		MOVE.B	5(A0),3*63(A2)		;InstrExt.flags -> flags
		BRA.S	.IM_gotflags
		.IM_noflags:
		CMP.W	#1,inst_replen(A3)
		BLS.S	.IM_gotflags
		BSET	#0,3*63(A2)
		.IM_gotflags:
		CLR.B	6*63(A2)			;Initally OUTPUT_STD
		CMP.W	#9,D0
		BLT.S	.IM_noopdev
		MOVE.B	8(A0),6*63(A2)		;get InstrExt.output_device
		.IM_noopdev:
		MOVE.B	1(A0),63(A2)		;InstrExt.decay -> decay
		MOVE.B	(A0),(A2)+		;InstrExt.hold -> holdvals
		ADDA.W	D0,A0			;ptr to next InstrExt
		ADDQ.L	#8,A3			;next instrument...
		DBF	D2,.IM_loop2
		BRA.S	.IM_exit
		.IM_CLRhlddec:
		MOVE.W	#3*63-1,D0		;no InstrExt => cLEAr holdvals/decays
		.IM_loop3:
		CLR.W	(A2)+			;..AND finetunes/flags/ext_psets
		DBF	D0,.IM_loop3
		MOVEA.L	(SP),A0
		; -------- For (very old) MMDs, with no InstrExt, set flags/SSFLG_LOOP,
		; -------- also copy inst_midipreset to ext_midipsets.
		MOVEA.L	mmd_songinfo(A0),A3
		LEA	flags,A2
		MOVEQ	#62,D0
		.IM_loop4:
		CMP.W	#1,inst_replen(A3)
		BLS.S	.IM_noreptflg
		BSET	#0,(A2)
		.IM_noreptflg:
		ADDQ.L	#1,A2
		ADDQ.L	#8,A3			;next inst
		DBF	D0,.IM_loop4
		.IM_exit:
		ADDQ.L	#4,SP
		MOVEM.L	(SP)+,A2-A3/D2
		RTS

; *************************************************************************
; InitPlayer() -- allocate interrupt, audio, serial port etc...
; *************************************************************************
_InitPlayer:
		BSR.W	_AudioInit
		TST.L	D0
		BNE.S	.IP_error
		RTS
		.IP_error:
		BSR.S	_RemPlayer
		MOVEQ	#-1,D0
		RTS

; *************************************************************************
; RemPlayer() -- free interrupt, audio, serial port etc..
; *************************************************************************
_RemPlayer:
		MOVE.B	_timeropen,D0
		BEQ.S	.RP_notimer		;timer is not ours
		BSR.S	_StopPlayer
		.RP_notimer:
		BSR.W	_AudioRem
		RTS

; *************************************************************************
; StopPlayer() -- stop the music
; *************************************************************************
_StopPlayer:
		LEA	DB,A1
		MOVE.B	_timeropen-DB(A1),D0
		BEQ.S	.SP_end			;res. alloc fail.
	IFNE CIAB
		MOVEA.L	craddr-DB(A1),A0
		BCLR	#0,(A0)			;stop timer
	ENDC
		MOVE.L	_module-DB(A1),D0
		BEQ.S	.SP_nomod
		MOVE.L	D0,A0
		CLR.W	mmd_pstate(A0)
		CLR.L	_module-DB(A1)
		.SP_nomod:
		BRA.W	SoundOff
		.SP_end:	RTS

_ContModule:
		TST.B	_timeropen
		BEQ.S	_StopPlayer\.SP_end
		MOVEA.L	craddr,A1
		BCLR	#0,(A1)
		MOVE.L	A0,-(SP)
		BSR.W	SoundOff
		MOVE.L	(SP)+,A0
		MOVEQ	#0,D0
		BRA.S	_PlayModule\.contpoint

; *************************************************************************
; PlayModule(A0 = module)  -- initialize & play it!
; *************************************************************************
_PlayModule:
		ST	D0
		.contpoint:
		MOVEM.L	A0/D0,-(SP)
		BSR	_InitModule
		MOVEM.L	(SP)+,A0/D0
		MOVE.L	A6,-(SP)
		LEA	DB,A6
		TST.B	_timeropen-DB(A6)
		BEQ	.PM_end			;resource allocation failure
		MOVE.L	A0,D1
		BEQ	.PM_end			;module failure
	IFNE CIAB
		MOVEA.L	craddr-DB(A6),A1
		BCLR	#0,(A1)			;stop timer...
	ENDC
		CLR.L	_module-DB(A6)
		MOVE.W	_modnum,D1
		BEQ.S	.PM_modfound
		.PM_nextmod:
		TST.L	mmd_expdata(A0)
		BEQ.S	.PM_modfound
		MOVE.L	mmd_expdata(A0),A1
		TST.L	(A1)
		BEQ.S	.PM_modfound		;no more modules here!
		MOVE.L	(A1),A0
		SUBQ.W	#1,D1
		BGT.S	.PM_nextmod
		.PM_modfound:
		CMP.B	#'T',3(A0)
		BNE.S	.PM_nomodT
		MOVE.B	#'0',3(A0)		;change MCNT to MCN0
		.PM_nomodT:
		MOVEA.L	mmd_songinfo(A0),A1		;song
		MOVE.B	msng_tempo2(A1),mmd_counter(A0) ;init counter
		BTST	#0,msng_flags(A1)
		BNE.S	.PM_filon
		BSET	#1,$bfe001
		BRA.S	.PM_filset
		.PM_filon:
		BCLR	#1,$bfe001
		.PM_filset:
		TST.B	D0
		BEQ.S	.PM_noclr
		CLR.L	mmd_pline(A0)
		CLR.L	rptline-DB(A6)
		CLR.W	blkdelay-DB(A6)
		; ---------- Set 'pblock' AND 'pseq' to correct values...
		.PM_noclr:
		CMP.B	#'2',3(A0)
		BNE.S	.PM_oldpbset
		MOVE.W	mmd_psecnum(A0),D1
		MOVE.L	A2,-(SP)			;need extra register
		MOVEA.L	msng_sections(A1),A2
		ADD.W	D1,D1
		MOVE.W	0(A2,D1.W),D1		;get sequence number
		ADD.W	D1,D1
		ADD.W	D1,D1
		MOVE.W	D1,mmd_pseq(A0)
		MOVEA.L	msng_pseqs(A1),A2
		MOVEA.L	0(A2,D1.W),A2		;PlaySeQ...
		MOVE.W	mmd_pseqnum(A0),D1
		ADD.W	D1,D1
		MOVE.W	42(A2,D1.W),D1		;AND the correct block..
		MOVE.L	(SP)+,A2
		BRA.S	.PM_setblk
		.PM_oldpbset:
		MOVE.W	mmd_pseqnum(A0),D1
		ADD.W	#msng_playseq,D1
		MOVE.B	0(A1,D1.W),D1		;get first playseq entry
		EXT.W	D1
		.PM_setblk:
		MOVE.W	D1,mmd_pblock(A0)
		MOVE.W	#-1,mmd_pstate(A0)
		MOVE.L	A0,_module-DB(A6)
		BTST	#5,msng_flags2(A1)		;BPM?
		SEQ	bpmcounter-DB(A6)
	IFNE CIAB
		MOVE.W	msng_deftempo(A1),D0	;get default tempo
		MOVEA.L	craddr-DB(A6),A1
		BSR.W	_SetTempo			;set default tempo
		BSET	#0,(A1)			;start timer => PLAY!!
	ENDC
		.PM_end:
		MOVE.L	(SP)+,A6
		RTS

; *************************************************************************
_AudioInit:
		MOVEM.L	A4/A6/D2-D3,-(SP)
		LEA	DB,A4
		MOVEQ	#0,D2
		MOVEA.L	4.W,A6
		;+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+ alloc signal bit
	IFNE AUDDEV
		MOVEQ	#1,D2
		MOVEQ	#-1,D0
		JSR	-$14a(A6)			;AllocSignal()
		TST.B	D0
		BMI.W	initerr
		MOVE.B	D0,sigbitnum-DB(A4)
		;+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+ prepare IORequest
		LEA	allocport-DB(A4),A1
		MOVE.B	D0,15(A1)			;set mp_SigBit
		MOVE.L	A1,-(SP)
		SUBa.L	A1,A1
		JSR	-$126(A6)			;FindTask(0)
		MOVE.L	(SP)+,A1
		MOVE.L	D0,16(A1)			;set mp_SigTask
		LEA	reqlist-DB(A4),A0
		MOVE.L	A0,(A0)			;NEWLIST begins...
		ADDQ.L	#4,(A0)
		CLR.L	4(A0)
		MOVE.L	A0,8(A0)			;NEWLIST ends...
		;+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+ open audio.device
		MOVEQ	#2,D2
		LEA	allocreq-DB(A4),A1
		LEA	audiodevname-DB(A4),A0
		MOVEQ	#0,D0
		MOVEQ	#0,D1
		MOVEA.L	4.W,A6
		JSR	-$1bc(A6)			;OpenDevice()
		TST.B	D0
		BNE.W	initerr
		ST	audiodevopen-DB(A4)
		;+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+ open cia resource
		MOVEQ	#3,D2
	ENDC
	IFNE CIAB
		CMP.B	#50,$212(A6)		;ExecBase->VBlankFrequency
		BEQ.S	.init_pal
		MOVE.L	#474326,timerdiv-DB(A4)	;Assume that CIA freq is 715 909 Hz
		MOVE.L	#3579545/2,bpmdiv-DB(A4)
		.init_pal:
		MOVEQ	#0,D3
		LEA	cianame-DB(A4),A1
		MOVE.B	#'a',3(A1)
		.open_ciares:
		MOVEQ	#0,D0
		MOVEA.L	4.W,A6
		JSR	-$1f2(A6)			;OpenResource()
		MOVE.L	D0,_ciaresource
		BEQ.S	.try_CIAB
		MOVEQ	#4,D2
		MOVE.L	D0,A6
		LEA	timerinterrupt-DB(A4),A1
		MOVEQ	#0,D0			;Timer A
		JSR	-$6(A6)			;AddICRVector()
		TST.L	D0
		BEQ.S	.got_timer
		ADDQ.L	#4,D3			;ADD base ADDr index
		LEA	timerinterrupt-DB(A4),A1
		MOVEQ	#1,D0			;Timer B
		JSR	-$6(A6)			;AddICRVector()
		TST.L	D0
		BEQ.S	.got_timer
		.try_CIAB:
		LEA	cianame-DB(A4),A1
		CMP.B	#'a',3(A1)
		BNE.S	.initerr
		ADDQ.B	#1,3(A1)
		MOVEQ	#8,D3			;CIAB base ADDr index = 8
		BRA.W	.open_ciares
		;+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+ attach interrupt
		.got_timer:
		LEA	craddr+8-DB(A4),A6
		MOVE.L	.cia_addr(PC,D3.W),D0
		MOVE.L	D0,(A6)
		SUB.W	#$100,D0
		MOVE.L	D0,-(A6)
		MOVEQ	#2,D3			;assume timer B
		BTST	#9,D0			;timer A or B ?
		BNE.S	.got_timerB
		SUBQ.B	#1,D3			;not timer B -> SUBtract 1
		ADD.W	#$100,D0			;calc offset to timer contROL reg
		.got_timerB:
		ADD.W	#$900,D0
		MOVE.L	D0,-(A6)
		MOVE.L	D0,A0			;get ContROL Register
		AND.B	#%10000000,(A0)		;cLEAr CtrlReg bits 0 - 6
		MOVE.B	D3,_timeropen-DB(A4)	;D3: 1 = TimerA 2 = TimerB
	ENDC
	IFNE VBLANK
		MOVEQ	#5,D0			;INTB_VERTB
		LEA	timerinterrupt-DB(A4),A1
		JSR	-$a8(A6)			;AddIntServer
		ST	_timeropen-DB(A4)
	ENDC
		MOVEQ	#0,D0
		.initret:
		MOVEM.L	(SP)+,A4/A6/D2-D3
		RTS
		.initerr:
		MOVE.L	D2,D0
		BRA.S	.initret
		.cia_addr:
		DC.L	$BFE501,$BFE701,$BFD500,$BFD700

_AudioRem:
		MOVEM.L	A5-A6,-(SP)
		LEA	DB,A5
		MOVEQ	#0,D0
		MOVE.B	_timeropen,D0
		BEQ.S	.rem1
	;+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+ reMOVE interrupt
		CLR.B	_timeropen
	IFNE CIAB
		MOVE.L	_ciaresource,A6
		LEA	timerinterrupt-DB(A5),A1
		SUBQ.B	#1,D0
		JSR	-$c(A6)			;RemICRVector
	ENDC
	IFNE VBLANK
		MOVEA.L	4.W,A6
		LEA	timerinterrupt(PC),A1
		MOVEQ	#5,D0
		JSR	-$ae(A6)			;RemIntServer
	ENDC
		.rem1:
	IFNE AUDDEV
		MOVEA.L	4.W,A6
		TST.B	audiodevopen-DB(A5)
		BEQ.S	.rem2
		MOVE.W	#$000f,$dff096		;stop audio DMA
	;+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+ close audio.device
		LEA	allocreq-DB(A5),A1
		JSR	-$1c2(A6)			;CloseDevice()
		CLR.B	audiodevopen-DB(A5)
		.rem2:
		MOVEQ	#0,D0
		MOVE.B	sigbitnum-DB(A5),D0
		BMI.S	.rem3
	;+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+ free signal bit
		JSR	-$150(A6)			;FreeSignal()
		ST	sigbitnum-DB(A5)
		.rem3:
	ENDC
		MOVEM.L	(SP)+,A5-A6
		RTS

	;DATA:
DB:		;Data base pointer
miscresbase:	DC.L 0
timerdiv:		DC.L 470000
	IFNE AUDDEV
audiodevopen:	DC.B 0
sigbitnum:	DC.B -1
	ENDC
		EVEN
_module:		DC.L 0
DMAonmsk:		DC.W 0 ;\_May not be
	IFNE CIAB
_ciaresource:	DC.L 0
	ENDC
craddr:		DC.L 0
		DC.L 0		;tloADDr
		DC.L 0		;thiADDr

timerinterrupt:	DC.W 0,0,0,0,0
		DC.L timerintname,DB
		DC.L _IntHandler
	IFNE AUDDEV
allocport:	DC.L 0,0		;succ, pred
		DC.B 4,0		;NT_MSGPORT
		DC.L 0		;name
		DC.B 0,0		;flags = PA_SIGNAL
		DC.L 0		;task
reqlist:		DC.L 0,0,0	;list head, tail AND tailpred
		DC.B 5,0
allocreq:		DC.L 0,0
		DC.B 0,127	;NT_UNKNOWN, use maximum priority (127)
		DC.L 0,allocport	;name, replyport
		DC.W 68		;length
		DC.L 0		;io_Device
		DC.L 0		;io_Unit
		DC.W 0		;io_CommAND
		DC.B 0,0		;io_Flags, io_ErROR
		DC.W 0		;ioa_AllocKey
		DC.L sttempo	;ioa_Data
		DC.L 1		;ioa_Length
		DC.W 0,0,0	;ioa_Period, Volume, Cycles
		DC.W 0,0,0,0,0,0,0,0,0,0 ;ioa_WriteMsg
audiodevname:	DC.B 'audio.device',0
	ENDC
	IFNE CIAB
cianame:		DC.B 'ciax.resource',0
	ENDC
_timeropen:	DC.B 0
timerintname:	DC.B 'OMEDTimerInterrupt',0
		EVEN
; TRACK-data structures (see definitions at the end of this file)
t03d:		DS.B TAAOFFS
		DC.L $dff0A0
		DS.B TTMPVOLOFFS-(TAAOFFS+4)
		DC.B $ff
t03de:		DS.B T03SZ-(t03de-t03d)
		DS.B TAAOFFS
		DC.L $dff0b0
		DS.B TTMPVOLOFFS-(TAAOFFS+4)
		DC.B $ff
		DS.B T03SZ-(t03de-t03d)
		DS.B TAAOFFS
		DC.L $dff0c0
		DS.B TTMPVOLOFFS-(TAAOFFS+4)
		DC.B $ff
		DS.B T03SZ-(t03de-t03d)
		DS.B TAAOFFS
		DC.L $dff0D0
		DS.B TTMPVOLOFFS-(TAAOFFS+4)
		DC.B $ff
		DS.B T03SZ-(t03de-t03d)
t463d:		DS.B (MAX_NUMTRACKS-4)*T415SZ
trackdataptrs:	DC.L t03d,t03d+T03SZ,t03d+2*T03SZ,t03d+3*T03SZ
; Build pointer table. This works on Devpac assembler, other assemblers
; may need modifications.
TRKCOUNT		SET 0
		REPT (MAX_NUMTRACKS-4)
		DC.L t463d+TRKCOUNT
TRKCOUNT		SET TRKCOUNT+T415SZ
		ENDR
; MODIFICATION FOR ASM-PRO
;	DC.L t463d+0
nextblock:	DC.B 0 ;\ DON'T SEPARATE
nxtnoclrln:	DC.B 0 ;/
numtracks:	DC.W 0 ;\ DON'T SEPARATE
numlines:		DC.W 0 ;/
numpages:		DC.W 0
nextblockline:	DC.W 0
rptline:		DC.W 0 ;\ DON'T SEPARATE
rptcounter:	DC.W 0 ;/
blkdelay:		DC.W 0		;block delay (PT PatternDelay)
bpmcounter:	DC.W 0
bpmdiv:		DC.L 3546895/2
fxplineblk:	DC.L 0		;for reading effects

; ##### KONEY MOD ######
	IFNE INSTR_TRACKING
MED_TRK_0_INST:	DC.B 0		; sample# note...
MED_TRK_0_NOTE:	DC.B 0
MED_TRK_1_INST:	DC.B 0
MED_TRK_1_NOTE:	DC.B 0
MED_TRK_2_INST:	DC.B 0
MED_TRK_2_NOTE:	DC.B 0
MED_TRK_3_INST:	DC.B 0
MED_TRK_3_NOTE:	DC.B 0
MED_TRK_0_COUNT:	DC.W $4000
MED_TRK_1_COUNT:	DC.W $4000
MED_TRK_2_COUNT:	DC.W $4000
MED_TRK_3_COUNT:	DC.W $4000
	ENDC
	IFNE START_POS
MED_START_POS:	DC.W 0		; staRTS at...
	ENDC
	IFNE SONG_POS_TRACKING
MED_SONG_POS:	DC.W 0		; Well the position...
	ENDC
	IFNE BLOCK_LINE_TRACKING
MED_BLOCK_LINE:	DC.W 0		; Line of block
	ENDC
	IFNE STEP_SEQ
MED_STEPSEQ_POS:	DC.W -1		; Pos of the step sequencer 0-15 | FIX for start=1
	ENDC
; ##### KONEY MOD ######
; Fields in struct InstrExt (easier to access this way rather than
; searching through the module).
holdvals:		DS.B 63
decays:		DS.B 63
finetunes:	DS.B 63
flags:		DS.B 63
ext_midipsets:	DS.W 63
outputdevs:	DS.B 63
		EVEN

; Below are the period tables. There's one table for each finetune position.
	IFNE SYNTH|IFFMOCT
	DC.W 3424,3232,3048,2880,2712,2560,2416,2280,2152,2032,1920,1812
	DC.W 1712,1616,1524,1440,1356,1280,1208,1140,1076,1016,960,906
	ENDC
per0:	DC.W 856,808,762,720,678,640,604,570,538,508,480,453
	DC.W 428,404,381,360,339,320,302,285,269,254,240,226
	DC.W 214,202,190,180,170,160,151,143,135,127,120,113
	DC.W 214,202,190,180,170,160,151,143,135,127,120,113
	DC.W 214,202,190,180,170,160,151,143,135,127,120,113
	DC.W 214,202,190,180,170,160,151,143,135,127,120,113
	IFNE SYNTH|IFFMOCT
	DC.W 3400,3209,3029,2859,2699,2547,2404,2269,2142,2022,1908,1801
	DC.W 1700,1605,1515,1430,1349,1274,1202,1135,1071,1011,954,901
	ENDC
per1:	DC.W 850,802,757,715,674,637,601,567,535,505,477,450
	DC.W 425,401,379,357,337,318,300,284,268,253,239,225
	DC.W 213,201,189,179,169,159,150,142,134,126,119,113
	DC.W 213,201,189,179,169,159,150,142,134,126,119,113
	DC.W 213,201,189,179,169,159,150,142,134,126,119,113
	DC.W 213,201,189,179,169,159,150,142,134,126,119,113
	IFNE SYNTH|IFFMOCT
	DC.W 3376,3187,3008,2839,2680,2529,2387,2253,2127,2007,1895,1788
	DC.W 1688,1593,1504,1419,1340,1265,1194,1127,1063,1004,947,894
	ENDC
per2:	DC.W 844,796,752,709,670,632,597,563,532,502,474,447
	DC.W 422,398,376,355,335,316,298,282,266,251,237,224
	DC.W 211,199,188,177,167,158,149,141,133,125,118,112
	DC.W 211,199,188,177,167,158,149,141,133,125,118,112
	DC.W 211,199,188,177,167,158,149,141,133,125,118,112
	DC.W 211,199,188,177,167,158,149,141,133,125,118,112
	IFNE SYNTH|IFFMOCT
	DC.W 3352,3164,2986,2819,2660,2511,2370,2237,2112,1993,1881,1776
	DC.W 1676,1582,1493,1409,1330,1256,1185,1119,1056,997,941,888
	ENDC
per3:	DC.W 838,791,746,704,665,628,592,559,528,498,470,444
	DC.W 419,395,373,352,332,314,296,280,264,249,235,222
	DC.W 209,198,187,176,166,157,148,140,132,125,118,111
	DC.W 209,198,187,176,166,157,148,140,132,125,118,111
	DC.W 209,198,187,176,166,157,148,140,132,125,118,111
	DC.W 209,198,187,176,166,157,148,140,132,125,118,111
	IFNE SYNTH|IFFMOCT
	DC.W 3328,3141,2965,2799,2641,2493,2353,2221,2097,1979,1868,1763
	DC.W 1664,1571,1482,1399,1321,1247,1177,1111,1048,989,934,881
	ENDC
per4:	DC.W 832,785,741,699,660,623,588,555,524,495,467,441
	DC.W 416,392,370,350,330,312,294,278,262,247,233,220
	DC.W 208,196,185,175,165,156,147,139,131,124,117,110
	DC.W 208,196,185,175,165,156,147,139,131,124,117,110
	DC.W 208,196,185,175,165,156,147,139,131,124,117,110
	DC.W 208,196,185,175,165,156,147,139,131,124,117,110
	IFNE SYNTH|IFFMOCT
	DC.W 3304,3119,2944,2778,2622,2475,2336,2205,2081,1965,1854,1750
	DC.W 1652,1559,1472,1389,1311,1238,1168,1103,1041,982,927,875
	ENDC
per5:	DC.W 826,779,736,694,655,619,584,551,520,491,463,437
	DC.W 413,390,368,347,328,309,292,276,260,245,232,219
	DC.W 206,195,184,174,164,155,146,138,130,123,116,109
	DC.W 206,195,184,174,164,155,146,138,130,123,116,109
	DC.W 206,195,184,174,164,155,146,138,130,123,116,109
	DC.W 206,195,184,174,164,155,146,138,130,123,116,109
	IFNE SYNTH|IFFMOCT
	DC.W 3280,3096,2922,2758,2603,2457,2319,2189,2066,1950,1841,1738
	DC.W 1640,1548,1461,1379,1302,1229,1160,1095,1033,975,920,869
	ENDC
per6:	DC.W 820,774,730,689,651,614,580,547,516,487,460,434
	DC.W 410,387,365,345,325,307,290,274,258,244,230,217
	DC.W 205,193,183,172,163,154,145,137,129,122,115,109
	DC.W 205,193,183,172,163,154,145,137,129,122,115,109
	DC.W 205,193,183,172,163,154,145,137,129,122,115,109
	DC.W 205,193,183,172,163,154,145,137,129,122,115,109
	IFNE SYNTH|IFFMOCT
	DC.W 3256,3073,2901,2738,2584,2439,2302,2173,2051,1936,1827,1725
	DC.W 1628,1537,1450,1369,1292,1220,1151,1087,1026,968,914,862
	ENDC
per7:	DC.W 814,768,725,684,646,610,575,543,513,484,457,431
	DC.W 407,384,363,342,323,305,288,272,256,242,228,216
	DC.W 204,192,181,171,161,152,144,136,128,121,114,108
	DC.W 204,192,181,171,161,152,144,136,128,121,114,108
	DC.W 204,192,181,171,161,152,144,136,128,121,114,108
	DC.W 204,192,181,171,161,152,144,136,128,121,114,108
	IFNE SYNTH|IFFMOCT
	DC.W 3628,3424,3232,3051,2880,2718,2565,2421,2285,2157,2036,1922
	DC.W 1814,1712,1616,1525,1440,1359,1283,1211,1143,1079,1018,961
	ENDC
per_8:	DC.W 907,856,808,762,720,678,640,604,570,538,508,480
	DC.W 453,428,404,381,360,339,320,302,285,269,254,240
	DC.W 226,214,202,190,180,170,160,151,143,135,127,120
	DC.W 226,214,202,190,180,170,160,151,143,135,127,120
	DC.W 226,214,202,190,180,170,160,151,143,135,127,120
	DC.W 226,214,202,190,180,170,160,151,143,135,127,120
	IFNE SYNTH|IFFMOCT
	DC.W 3588,3387,3197,3017,2848,2688,2537,2395,2260,2133,2014,1901
	DC.W 1794,1693,1598,1509,1424,1344,1269,1197,1130,1067,1007,950
	ENDC
per_7:	DC.W 900,850,802,757,715,675,636,601,567,535,505,477
	DC.W 450,425,401,379,357,337,318,300,284,268,253,238
	DC.W 225,212,200,189,179,169,159,150,142,134,126,119
	DC.W 225,212,200,189,179,169,159,150,142,134,126,119
	DC.W 225,212,200,189,179,169,159,150,142,134,126,119
	DC.W 225,212,200,189,179,169,159,150,142,134,126,119
	IFNE SYNTH|IFFMOCT
	DC.W 3576,3375,3186,3007,2838,2679,2529,2387,2253,2126,2007,1894
	DC.W 1788,1688,1593,1504,1419,1339,1264,1193,1126,1063,1003,947
	ENDC
per_6:	DC.W 894,844,796,752,709,670,632,597,563,532,502,474
	DC.W 447,422,398,376,355,335,316,298,282,266,251,237
	DC.W 223,211,199,188,177,167,158,149,141,133,125,118
	DC.W 223,211,199,188,177,167,158,149,141,133,125,118
	DC.W 223,211,199,188,177,167,158,149,141,133,125,118
	DC.W 223,211,199,188,177,167,158,149,141,133,125,118
	IFNE SYNTH|IFFMOCT
	DC.W 3548,3349,3161,2984,2816,2658,2509,2368,2235,2110,1991,1879
	DC.W 1774,1674,1580,1492,1408,1329,1254,1184,1118,1055,996,940
	ENDC
per_5:	DC.W 887,838,791,746,704,665,628,592,559,528,498,470
	DC.W 444,419,395,373,352,332,314,296,280,264,249,235
	DC.W 222,209,198,187,176,166,157,148,140,132,125,118
	DC.W 222,209,198,187,176,166,157,148,140,132,125,118
	DC.W 222,209,198,187,176,166,157,148,140,132,125,118
	DC.W 222,209,198,187,176,166,157,148,140,132,125,118
	IFNE SYNTH|IFFMOCT
	DC.W 3524,3326,3140,2963,2797,2640,2492,2352,2220,2095,1978,1867
	DC.W 1762,1663,1570,1482,1399,1320,1246,1176,1110,1048,989,933
	ENDC
per_4:	DC.W 881,832,785,741,699,660,623,588,555,524,494,467
	DC.W 441,416,392,370,350,330,312,294,278,262,247,233
	DC.W 220,208,196,185,175,165,156,147,139,131,123,117
	DC.W 220,208,196,185,175,165,156,147,139,131,123,117
	DC.W 220,208,196,185,175,165,156,147,139,131,123,117
	DC.W 220,208,196,185,175,165,156,147,139,131,123,117
	IFNE SYNTH|IFFMOCT
	DC.W 3500,3304,3118,2943,2778,2622,2475,2336,2205,2081,1964,1854
	DC.W 1750,1652,1559,1472,1389,1311,1237,1168,1102,1041,982,927
	ENDC
per_3:	DC.W 875,826,779,736,694,655,619,584,551,520,491,463
	DC.W 437,413,390,368,347,328,309,292,276,260,245,232
	DC.W 219,206,195,184,174,164,155,146,138,130,123,116
	DC.W 219,206,195,184,174,164,155,146,138,130,123,116
	DC.W 219,206,195,184,174,164,155,146,138,130,123,116
	DC.W 219,206,195,184,174,164,155,146,138,130,123,116
	IFNE SYNTH|IFFMOCT
	DC.W 3472,3277,3093,2920,2756,2601,2455,2317,2187,2064,1949,1839
	DC.W 1736,1639,1547,1460,1378,1301,1228,1159,1094,1032,974,920
	ENDC
per_2:	DC.W 868,820,774,730,689,651,614,580,547,516,487,460
	DC.W 434,410,387,365,345,325,307,290,274,258,244,230
	DC.W 217,205,193,183,172,163,154,145,137,129,122,115
	DC.W 217,205,193,183,172,163,154,145,137,129,122,115
	DC.W 217,205,193,183,172,163,154,145,137,129,122,115
	DC.W 217,205,193,183,172,163,154,145,137,129,122,115
	IFNE SYNTH|IFFMOCT
	DC.W 3448,3254,3072,2899,2737,2583,2438,2301,2172,2050,1935,1827
	DC.W 1724,1627,1536,1450,1368,1292,1219,1151,1086,1025,968,913
	ENDC
per_1:	DC.W 862,814,768,725,684,646,610,575,543,513,484,457
	DC.W 431,407,384,363,342,323,305,288,272,256,242,228
	DC.W 216,203,192,181,171,161,152,144,136,128,121,114
	DC.W 216,203,192,181,171,161,152,144,136,128,121,114
	DC.W 216,203,192,181,171,161,152,144,136,128,121,114
	DC.W 216,203,192,181,171,161,152,144,136,128,121,114

_periodtable:
	DC.L per_8,per_7,per_6,per_5,per_4,per_3,per_2,per_1,per0
	DC.L per1,per2,per3,per4,per5,per6,per7
	IFND __G2
		;section "ChipData",data_c	;,chip ;for A68k
	ENDC
	IFD __G2
		;section "ChipData",data_c	;this is for Devpac 2
	ENDC
		XDEF	_modnum
	IFNE EASY
	;easymod:	INCBIN	"med/octamed_test.med"	;<<<<< MODULE NAME HERE!
	ENDC
	;IFEQ SPLIT_RELOCS
;_chipzero: DC.L 0	; MOVE this to chip ram after SAMPLES!
	;ENDC
_modnum:	DC.W 0	; number of module to play

; macros for entering offsets
DEFWORD	MACRO
\1	EQU OFFS
OFFS	SET OFFS+2
	ENDM
DEFBYTE	MACRO
\1	EQU OFFS
OFFS	SET OFFS+1
	ENDM
DEFLONG	MACRO
\1	EQU OFFS
OFFS	SET OFFS+4
	ENDM

OFFS	SET 0
	; the track-data structure definition:
	DEFBYTE	trk_prevnote	;previou	s note number (0 = none, 1 = C-1..)
	DEFBYTE	trk_previnstr	;previous instrument number
	DEFBYTE	trk_prevvol	;previous volume
	DEFBYTE	trk_prevmidich	;previous MIDI channel
	DEFBYTE	trk_prevmidin	;previous MIDI note
	DEFBYTE	trk_noteoffcnt	;note-off counter (hold)
	DEFBYTE	trk_inithold	;default hold for this instrument
	DEFBYTE	trk_initdecay	;default decay for....
	DEFBYTE	trk_stransp	;instrument tranSPose
	DEFBYTE	trk_finetune	;finetune
	DEFWORD	trk_soffset	;new sample offset | don't sep this AND 2 below!
	DEFBYTE	trk_miscflags	;bit: 7 = cmd 3 exists, 0 = cmd E exists
	DEFBYTE	trk_currnote	;note on CURRENT line (0 = none, 1 = C-1...)
	DEFBYTE	trk_outputdev	;output device
	DEFBYTE	trk_fxtype	;fx type: 0 = norm, 1 = none, -1 = MIDI
	DEFLONG	trk_previnstra	;ADDress of the previous instrument data
	DEFWORD	trk_trackvol
	; the following data only on tracks 0 - 3
	DEFWORD	trk_prevper	;previous period
	DEFLONG	trk_audioaddr	;hardware audio channel base ADDress
	DEFLONG	trk_sampleptr	;pointer to sample
	DEFWORD	trk_samplelen	;length (>> 1)
	DEFWORD	trk_porttrgper	;portamento (cmd 3) target period
	DEFBYTE	trk_vibshift	;viBRAto shift for ASR instruction
	DEFBYTE	trk_vibrspd	;viBRAto SPeed/size (cmd 4 qualifier)
	DEFWORD	trk_vibrsz	;viBRAto size
	DEFLONG	trk_synthptr	;pointer to synthetic/hybrid instrument
	DEFWORD	trk_arpgoffs	;SYNTH: current arpeggio offset
	DEFWORD	trk_arpsoffs	;SYNTH: arpeggio restart offset
	DEFBYTE	trk_volxcnt	;SYNTH: volume execute counter
	DEFBYTE	trk_wfxcnt	;SYNTH: waveform execute counter
	DEFWORD	trk_volcmd	;SYNTH: volume commAND pointer
	DEFWORD	trk_wfcmd		;SYNTH: waveform commAND pointer
	DEFBYTE	trk_volwait	;SYNTH: counter for WAI (volume list)
	DEFBYTE	trk_wfwait	;SYNTH: counter for WAI (waveform list)
	DEFWORD	trk_synthvibspd	;SYNTH: viBRAto SPeed
	DEFWORD	trk_wfchgspd	;SYNTH: period change
	DEFWORD	trk_perchg	;SYNTH: curr. period change from trk_prevper
	DEFLONG	trk_envptr	;SYNTH: envelope waveform pointer
	DEFWORD	trk_synvibdep	;SYNTH: viBRAto depth
	DEFLONG	trk_synvibwf	;SYNTH: viBRAto waveform
	DEFWORD	trk_synviboffs	;SYNTH: viBRAto pointer
	DEFBYTE	trk_initvolxspd	;SYNTH: volume execute SPeed
	DEFBYTE	trk_initwfxspd	;SYNTH: waveform execute SPeed
	DEFBYTE	trk_volchgspd	;SYNTH: volume change
	DEFBYTE	trk_prevnote2	;SYNTH: previous note
	DEFBYTE	trk_synvol	;SYNTH: current volume
	DEFBYTE	trk_synthtype	;>0 = synth, -1 = hybrid, 0 = no synth
	DEFLONG	trk_periodtbl	;pointer to period table
	DEFWORD	trk_prevportspd	;portamento (cmd 3) SPeed
	DEFBYTE	trk_decay		;decay
	DEFBYTE	trk_fadespd	;decay SPeed
	DEFLONG	trk_envrestart	;SYNTH: envelope waveform restart point
	DEFBYTE	trk_envcount	;SYNTH: envelope counter
	DEFBYTE	trk_split		;0 = this channel not SPlitted (OctaMED V2)
	DEFWORD	trk_newper	;new period (for synth use)
	DEFBYTE	trk_vibroffs	;viBRAto table offset \ DON'T SEPARATE
	DEFBYTE	trk_tremoffs	;tremolo table offset /
	DEFWORD	trk_tremsz	;tremolo size
	DEFBYTE	trk_tremspd	;tremolo SPeed
	DEFBYTE	trk_tempvol	;temporary volume (for tremolo)
	DEFWORD	trk_vibrAdjust	;viBRAto +/- change from base period \ DON'T SEPARATE
	DEFWORD	trk_arpadjust	;arpeggio +/- change from base period/
	;END
