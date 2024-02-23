;*** WORKING PLAY MED MUSIC
;*** MiniStartup by Photon ***
	INCDIR	"NAS:AMIGA/CODE/octamed_playroutines_amiga/"
	SECTION	"Code",CODE
	INCLUDE	"custom-registers.i"
	INCLUDE	"med/med_feature_control.i"	; MED CFGs
	INCLUDE	"PhotonsMiniWrapper1.04.S"
	IFNE MED_PLAY_ENABLE
	INCLUDE	"med/MED_PlayRoutine.i"
	ENDC
;********** Constants **********
WI	EQU 320		;screen width, height, depth
HE	EQU 256
BPLS	EQU 1		;handy values:
BYPL	EQU WI/16*2	;byte-width of 1 bitplane line (40)
BWID	EQU BPLS*BYPL	;byte-width of 1 pixel line (all bpls)

;********** Demo **********	; Demo-specific non-startup code below.
Demo:	;a4=VBR, a6=Custom Registers Base addr
	;*--- init ---*
	MOVE.L	#VBint,$6C(A4)
	MOVE.W	#%1110000000000000,INTENA	; Master and lev6	; NO COPPER-IRQ!
	MOVE.W	#%1000011111100000,DMACON
	;MOVE.W	#$C020,INTENA
	;MOVE.W	#$87C0,DMACON
	;*--- clear screens ---*
	LEA	SCREEN1,a1
	;bsr.w	ClearScreen
	LEA	SCREEN2,a1
	;bsr.w	ClearScreen
	;bsr.w	WaitBlitter
	;*--- start copper ---*
	LEA	SCREEN1,a0
	MOVEQ	#BYPL,d0
	LEA	Copper\.BplPtrs+2,a1
	MOVEQ	#BPLS-1,d1
	BSR.W	PokePtrs

	; #### CPU INTENSIVE TASKS BEFORE STARTING MUSIC
	IFNE STEP_SEQ
	BSR.W	__POINT_SPRITES		; #### Point sprites
	ENDC
	; #### CPU INTENSIVE TASKS BEFORE STARTING MUSIC
	IFNE MED_PLAY_ENABLE
	; in photon's wrapper comment:;move.w d2,$9a(a6) ;INTENA
	;MOVE.W	#2,MED_START_POS	 ; skip to pos# after first block
	JSR	_startmusic
	ENDC

	MOVE.L	#Copper\.Palette,COP2LC
	MOVE.L	#Copper,COP1LC
;********************  main loop  ********************
MainLoop:
	move.w	#$12C,D0		;No buffering, so wait until raster
	bsr.w	WaitRaster	;is below the Display Window.
	;*--- swap buffers ---*
	MOVEM.L	DrawBuffer,A2-A3
	EXG	A2,A3
	MOVEM.L	A2-A3,DrawBuffer	;draw into a2, show a3
	;*--- show one... ---*
	MOVE.L	A3,A0
	MOVE.L	#BYPL*HE,D0
	LEA	Copper\.BplPtrs+2,A1
	MOVEQ	#BPLS-1,D1
	BSR.W	PokePtrs
	;*--- ...draw into the other(a2) ---*
	MOVE.L	A2,A1
	;bsr.w	ClearScreen
	;bsr.w	WaitBlitter
	MOVE.L	KONEYBG,DrawBuffer
	BSR.W	__FILLRNDBG	; SOME DUMMY OPERATION...
	; do stuff here :)

	;*--- main loop end ---*
	;BTST	#6,$BFE001
	;BNE.S	.DontShowRasterTime
	;MOVE.W	#$0F0,$180(A6)	; show rastertime left down to $12c
	;.DontShowRasterTime:

	IFNE STEP_SEQ		; MOVED INSIDE PLAYROUTINE
	;MOVE.W	MED_STEPSEQ_POS,D0	; UPDATE STEPSEQUENCER
	;ANDI.W	#$F,D0		; POSITION (0-15 = 16 LEDS)
	;MOVE.W	D0,MED_STEPSEQ_POS
	ENDC

	IFNE INSTR_TRACKING
	LEA	MED_TRK_0_COUNT(PC),A0
	LEA	Copper\.LEVELWAITS+6,A1
	LEA	AUDIOCHLEV_0,A2
	LEA	MED_TRK_0_INST,A3
	;CLR.L	D0
	MOVEQ	#3,D7
	.loop:
	MOVEQ	#15,D0		; maxvalue
	SUB.W	(A0)+,D0		; -#frames/irqs since instrument trigger
	BPL.S	.ok		; below minvalue?
	MOVEQ	#0,D0		; then set to minvalue
	MOVE.W	D0,(A3)		; RESET TWO BYTES (INST+NOTE)
	.ok:
	MOVE.W	D0,(A2)+		; LEVEL VALUE TO USE IN CODE
	ROL.L	#$4,D0		; expand bits to green
	ROL.L	#$4,D0		; expand bits to green
	MOVE.W	D0,(A1)		; poke color
	LEA	16(A1),A1
	LEA	2(A3),A3
	DBF	D7,.loop

	ADDQ.W	#1,MED_TRK_0_COUNT	; inc elapsed #calls since last
	ADDQ.W	#1,MED_TRK_1_COUNT
	ADDQ.W	#1,MED_TRK_2_COUNT
	ADDQ.W	#1,MED_TRK_3_COUNT

	LEA	Copper\.INSTRWAITS+6,A1
	CLR.L	D0
	MOVE.B	MED_TRK_0_INST,D0
	ROL.L	#$2,D0		; expand bits to green
	MOVE.W	D0,(A1)
	MOVE.L	D0,D3
	ROL.L	#$4,D3		; expand bits to green
	ADD.L	D3,D0
	ROL.L	#$4,D3
	ADD.L	D3,D0		; expand bits to red

	LEA	16(A1),A1
	CLR.L	D0
	MOVE.B	MED_TRK_1_INST,D0
	ROL.L	#$2,D0		; expand bits to green
	MOVE.W	D0,(A1)
	MOVE.L	D0,D3
	ROL.L	#$4,D3		; expand bits to green
	ADD.L	D3,D0
	ROL.L	#$4,D3
	ADD.L	D3,D0		; expand bits to red

	LEA	16(A1),A1
	CLR.L	D0
	MOVE.B	MED_TRK_2_INST,D0
	ROL.L	#$2,D0		; expand bits to green
	MOVE.W	D0,(A1)
	MOVE.L	D0,D3
	ROL.L	#$4,D3		; expand bits to green
	ADD.L	D3,D0
	ROL.L	#$4,D3
	ADD.L	D3,D0		; expand bits to red

	LEA	16(A1),A1
	CLR.L	D0
	MOVE.B	MED_TRK_3_INST,D0
	ROL.L	#$2,D0		; expand bits to green
	MOVE.W	D0,(A1)
	MOVE.L	D0,D3
	ROL.L	#$4,D3		; expand bits to green
	ADD.L	D3,D0
	ROL.L	#$4,D3
	ADD.L	D3,D0		; expand bits to red

	LEA	Copper\.NOTEWAITS+6,A1
	CLR.L	D0
	MOVE.B	MED_TRK_0_NOTE,D0
	ROL.L	#$2,D0		; expand bits to green
	MOVE.W	D0,(A1)
	MOVE.L	D0,D3
	ROL.L	#$4,D3		; expand bits to green
	ADD.L	D3,D0
	ROL.L	#$4,D3
	ADD.L	D3,D0		; expand bits to red

	LEA	16(A1),A1
	CLR.L	D0
	MOVE.B	MED_TRK_1_NOTE,D0
	ROL.L	#$2,D0		; expand bits to green
	MOVE.W	D0,(A1)
	MOVE.L	D0,D3
	ROL.L	#$4,D3		; expand bits to green
	ADD.L	D3,D0
	ROL.L	#$4,D3
	ADD.L	D3,D0		; expand bits to red

	LEA	16(A1),A1
	CLR.L	D0
	MOVE.B	MED_TRK_2_NOTE,D0
	ROL.L	#$2,D0		; expand bits to green
	MOVE.W	D0,(A1)
	MOVE.L	D0,D3
	ROL.L	#$4,D3		; expand bits to green
	ADD.L	D3,D0
	ROL.L	#$4,D3
	ADD.L	D3,D0		; expand bits to red

	LEA	16(A1),A1
	CLR.L	D0
	MOVE.B	MED_TRK_3_NOTE,D0
	ROL.L	#$2,D0		; expand bits to green
	MOVE.W	D0,(A1)
	MOVE.L	D0,D3
	ROL.L	#$4,D3		; expand bits to green
	ADD.L	D3,D0
	ROL.L	#$4,D3
	ADD.L	D3,D0		; expand bits to red
	.skip2:

	;## CODE TO CHECK FOR SPECIFIC INTSRUMENT+NOTE
	MOVE.W	MED_TRK_0_INST,D0	; 1 WORD TO TAKE 2 BYTES FROM CH0
	CMP.W	#$71E,D0		; CH0 INST 07 NOTE F-3 = SNARE	; 0000011100011110
	BNE.S	.noNote
	MOVE.W	AUDIOCHLEV_0,$DFF1AE
	BRA.S	.skip
	.noNote:
	ENDC

	IFNE STEP_SEQ
	BSR.W	__SET_SEQUENCER_LEDS
	ENDC

	;## DEBUG VALUES ##
	MOVE.L	AUDIOCHLEV_0,D0
	MOVE.L	AUDIOCHLEV_2,D1
	IFNE SONG_POS_TRACKING
	MOVE.L	MED_SONG_POS,D2
	ENDC
	IFNE INSTR_TRACKING
	MOVE.L	MED_TRK_0_INST,D3
	MOVE.L	MED_TRK_2_INST,D4
	ENDC
	;## DEBUG VALUES ##

	; # CODE FOR BUTTON PRESS ##
	BTST	#6,$BFE001
	BNE.S	.skip
	MOVE.W	#$000F,$DFF180		; show rastertime left down to $12c
	IFNE MED_PLAY_ENABLE
	;#### TESTS FOR TRANSPOSE ####
	;LEA	DB,A6			;don't expect A1 to contain DB ADDress
	;MOVEA.L	_module-DB(A6),A2
	;MOVEA.L	mmd_songinfo(A2),A4		; 03D5 009B 0000 4001 0301 006E 0000 4000
	;MOVE.W	#$10,D3			;D3 = instr.num << 2
	;MOVE.L	0(A4,D3.W),A0		;get ADDress of instrument
	;MOVE.B	inst_strans(A0),D0		;AND instr. tranSPose
	;MOVE.B	D0,inst_strans(A0)		;AND instr. tranSPose
	ENDC
	.skip:

	;BTST	#6,$BFE001
	;BNE.S	.DontShowRasterTime
	;TST.W	LMBUTTON_STATUS
	;BNE.S	.DontShowRasterTime
	;MOVE.W	#1,LMBUTTON_STATUS
	;ADD.W	#16,MED_SONG_POS
	;.DontShowRasterTime:
	;BTST	#6,$BFE001
	;BEQ.S	.DontResetStatus
	;MOVE.W	#0,LMBUTTON_STATUS
	;.DontResetStatus:

	;BSR.S	WaitRasterCopper	; is below the Display Window.

	BTST	#2,$DFF016	; POTINP - RMB pressed?
	BNE.W	MainLoop		; then loop
	;*--- exit ---*
	IFNE MED_PLAY_ENABLE
	; --- quit MED code ---
	MOVEM.L	D0-A6,-(SP)
	JSR	_endmusic
	MOVEM.L	(SP)+,D0-A6
	ENDC
	RTS
;********** Demo Routines **********

WaitRasterCopper:
	;MOVE.W	#$0223,$DFF180	; show rastertime left down to $12c
	BTST	#4,INTENAR+1
	BNE.S	WaitRasterCopper
	;MOVE.W	#$0000,$DFF180	; show rastertime left down to $12c
	MOVE.W	#$8010,INTENA
	RTS
PokePtrs:				; Generic, poke ptrs into copper list
	.bpll:	
	move.l	a0,d2
	swap	d2
	move.w	d2,(a1)		;high word of address
	move.w	a0,4(a1)		;low word of address
	addq.w	#8,a1		;skip two copper instructions
	add.l	d0,a0		;next ptr
	dbf	d1,.bpll
	RTS
ClearScreen:			; a1=screen destination address to clear
	BSR.W	WaitBlitter
	CLR.W	BLTDMOD			; destination modulo
	MOVE.L	#$01000000,BLTCON0		; set operation type in BLTCON0/1
	MOVE.L	A1,BLTDPTH		; destination address
	MOVE.W	#HE*BPLS*64+BYPL/2,BLTSIZE	; blitter operation size
	RTS
VBint:				; Blank template VERTB interrupt
	BTST	#5,INTREQR+1	; check if it's our vertb int.
	BEQ.S	.notvb
	MOVE.W	D0,-(SP)		; SAVE USED REGISTERS
	MOVE.W	INTREQR,D0
	OR.W	#$20,D0		; BSET 5,D0 but quicker :)
	MOVE.W	D0,INTREQ		; poll irq bit
	MOVE.W	D0,INTREQ		; KONEY REFACTOR
	MOVE.W	(SP)+,D0		; RESTORE
	.notvb:
	RTE

__FILLRNDBG:
	MOVEM.L	D0-A6,-(SP)	; SAVE TO STACK
	IFNE SONG_POS_TRACKING
	MOVE.W	MED_SONG_POS,D2
	ENDC
	MULU.W	#BYPL*2,D2
	MOVE.L	KONEYBG,A4	; SOURCE DATA
	ADD.L	D2,A4
	CLR	D4
	MOVE.B	#4-1,D4		; QUANTE LINEE
	.OUTERLOOP:		; NUOVA RIGA
	CLR	D6
	MOVE.B	#BYPL/2-1,D6	; RESET D6
	.INNERLOOP:
	BSR.S	_RandomByte
	MOVE.B	D5,(A4)+
	DBRA	D6,.INNERLOOP
	DBRA	D4,.OUTERLOOP
	MOVEM.L	(SP)+,D0-A6	; FETCH FROM STACK
	RTS
_RandomWord:	BSR	_RandomByte
		ROL.W	#8,D5
_RandomByte:	MOVE.B	$DFF007,D5 ;$dff00a $dff00b for mouse pos
		MOVE.B	$BFD800,D3
		EOR.B	D3,D5
		RTS

	IFNE STEP_SEQ
__POINT_SPRITES:
	LEA	Copper\.SpritePointers,A1

	MOVE.L	#0,D0		; sprite 0
	MOVE.W	D0,6(A1)
	SWAP	D0
	MOVE.W	D0,2(A1)

	ADDQ.W	#8,A1
	MOVE.L	#0,D0		; sprite 1
	MOVE.W	D0,6(A1)
	SWAP	D0
	MOVE.W	D0,2(A1)

	ADDQ.W	#8,A1
	MOVE.L	#LED_OFF,D0	; sprite 2
	MOVE.W	D0,6(A1)
	SWAP	D0
	MOVE.W	D0,2(A1)

	ADDQ.W	#8,A1
	MOVE.L	#LED_ON,D0	; sprite 3
	MOVE.W	D0,6(A1)
	SWAP	D0
	MOVE.W	D0,2(A1)

	ADDQ.W	#8,A1
	MOVE.L	#0,D0		; sprite 4
	MOVE.W	D0,6(A1)
	SWAP	D0
	MOVE.W	D0,2(A1)

	ADDQ.W	#8,A1
	MOVE.L	#0,D0		; sprite 5
	MOVE.W	D0,6(A1)
	SWAP	D0
	MOVE.W	D0,2(A1)

	ADDQ.W	#8,A1
	MOVE.L	#0,D0		; sprite 6
	MOVE.W	D0,6(A1)
	SWAP	D0
	MOVE.W	D0,2(A1)

	ADDQ.W	#8,A1
	MOVE.L	#0,D0		; sprite 7
	MOVE.W	D0,6(A1)
	SWAP	D0
	MOVE.W	D0,2(A1)
	RTS
__SET_SEQUENCER_LEDS:
	MOVE.W	MED_STEPSEQ_POS,D0	; UPDATE STEPSEQUENCER
	LEA	SEQ_POS_ON,A0
	MOVE.B	(A0,D0.W),LED_ON\.HPOS
	LEA	SEQ_POS_OFF,A0
	MOVE.B	(A0,D0.W),LED_OFF\.HPOS
	RTS
	ENDC

;********** Fastmem Data **********
LMBUTTON_STATUS:	DC.W 0
AUDIOCHLEV_0:	DC.W 0
AUDIOCHLEV_1:	DC.W 0
AUDIOCHLEV_2:	DC.W 0
AUDIOCHLEV_3:	DC.W 0
FRAMESINDEX:	DC.W 4

SEQ_POS_ON:	DC.B $00,$61,$69,$71,$00,$81,$89,$91,$00,$A1,$A9,$B1,$00,$C1,$C9,$D1
SEQ_POS_OFF:	DC.B $59,$00,$00,$00,$79,$00,$00,$00,$99,$00,$00,$00,$B9,$00,$00,$00

KONEYBG:		DC.L BG1		; INIT BG
DrawBuffer:	DC.L SCREEN2	; pointers to buffers to be swapped
ViewBuffer:	DC.L SCREEN1

*******************************************************************************
	SECTION	"ChipData",DATA_C	;declared data that must be in chipmem
*******************************************************************************

MED_MODULE:	INCBIN "med/mammagamma.med"	;<<<<< MODULE NAME HERE!
	;IFNE	SPLIT_RELOCS
_chipzero:	DC.L 0
	;ENDC
		DC.L 0,0	 		; DUMMY

BG1:		INCBIN "GFX_MEDPLAYER2.raw"
		DS.B BYPL*HE	

	IFNE STEP_SEQ
LED_ON:	
	.VPOS:
	DC.B $9B
	.HPOS:
	DC.B $47
	DC.B $9F
	.CTRL:
	DC.B $00
	DC.W $FF00,$FF00,$FF00,$FF00,$FF00,$FF00
	DC.L 0
LED_OFF:	
	.VPOS:
	DC.B $9B
	.HPOS:
	DC.B $47
	DC.B $9F
	.CTRL:
	DC.B $00
	DC.W $FF00,$FF00,$FF00,$FF00,$FF00,$FF00
	DC.L 0
	ENDC

Copper:
	DC.W $1FC,0	;Slow fetch mode, remove if AGA demo.
	DC.W $8E,$2C81	;238h display window top, left
	DC.W $90,$2CC1	;and bottom, right.
	DC.W $92,$38	;Standard bitplane dma fetch start
	DC.W $94,$D0	;and stop for standard screen.

	DC.W $106,$0C00	;(AGA compat. if any Dual Playf. mode)
	DC.W $108,0	;bwid-BYPL	;modulos
	DC.W $10A,0	;bwid-BYPL	;RISULTATO = 80 ?
	DC.W $102,0	;SCROLL REGISTER (AND PLAYFIELD PRI)

	.BplPtrs:
	DC.W $E0,0
	DC.W $E2,0
	DC.W $E4,0
	DC.W $E6,0
	DC.W $E8,0
	DC.W $EA,0
	DC.W $EC,0
	DC.W $EE,0
	DC.W $F0,0
	DC.W $F2,0
	DC.W $F4,0
	DC.W $F6,0		;full 6 ptrs, in case you increase bpls
	DC.W $100,BPLS*$1000+$200	;enable bitplanes

	.Palette:		;Some kind of palette (3 bpls=8 colors)
	DC.W $0180,$0111,$0182,$0FFF,$0184,$0111,$0186,$0122
	DC.W $0188,$0F00,$018A,$0F00,$018C,$0F00,$018E,$0F00
	DC.W $0190,$0F00,$0192,$0F00,$0194,$0F00,$0196,$0F00
	DC.W $0198,$0F00,$019A,$0F00,$019C,$0F00,$019E,$0F00
	DC.W $01A0,$0555,$01A2,$0444,$01A4,$0FF0,$01A6,$0EEF
	DC.W $01A8,$0BBC,$01AA,$099A,$01AC,$0F0F,$01AE,$0F00

	.SpritePointers:
	DC.W $120,0,$122,0	; 0
	DC.W $124,0,$126,0	; 1
	DC.W $128,0,$12A,0	; 2
	DC.W $12C,0,$12E,0	; 3
	DC.W $130,0,$132,0	; 4
	DC.W $134,0,$136,0	; 5
	DC.W $138,0,$13A,0	; 6
	DC.W $13C,0,$13E,0	; 7

	.NOTEINSTRWAIT:
	;DC.W $A207,$FFFE
	;DC.W $0182,$0AAA
	;DC.W $A407,$FFFE
	;DC.W $0182,$0FFF

	.STEPSEQWAITS:
	DC.W $9A07,$FFFE
	DC.W $0182,$0000

	DC.W $9F07,$FFFE
	DC.W $0182,$0FFF

	.LEVELWAITS:
	DC.W $B207,$FFFE
	DC.W $0182,$0000
	DC.W $B407,$FFFE
	DC.W $0182,$0111

	DC.W $B507,$FFFE
	DC.W $0182,$0000
	DC.W $B707,$FFFE
	DC.W $0182,$0111

	DC.W $B807,$FFFE
	DC.W $0182,$0000
	DC.W $BA07,$FFFE
	DC.W $0182,$0111

	DC.W $BB07,$FFFE
	DC.W $0182,$001
	DC.W $BD07,$FFFE
	DC.W $0182,$0FFF

	.INSTRWAITS:
	DC.W $D207,$FFFE
	DC.W $0182,$0000
	DC.W $D407,$FFFE
	DC.W $0182,$0111

	DC.W $D507,$FFFE
	DC.W $0182,$0000
	DC.W $D707,$FFFE
	DC.W $0182,$0111

	DC.W $D807,$FFFE
	DC.W $0182,$0000
	DC.W $DA07,$FFFE
	DC.W $0182,$0111

	DC.W $DB07,$FFFE
	DC.W $0182,$0000
	DC.W $DD07,$FFFE
	DC.W $0182,$0FFF

	.NOTEWAITS:
	DC.W $F207,$FFFE
	DC.W $0182,$0000
	DC.W $F407,$FFFE
	DC.W $0182,$0111

	DC.W $F507,$FFFE
	DC.W $0182,$0000
	DC.W $F707,$FFFE
	DC.W $0182,$0111

	DC.W $F807,$FFFE
	DC.W $0182,$0000
	DC.W $FA07,$FFFE
	DC.W $0182,$0111

	DC.W $FB07,$FFFE
	DC.W $0182,$0000
	DC.W $FD07,$FFFE
	DC.W $0182,$0FFF

	DC.W $FFDF,$FFFE		; allow VPOS>$ff

	DC.W $3501,$FF00		; ## RASTER END ## #$12C?
	;DC.W $009A,$0010		; CLEAR RASTER BUSY FLAG

	DC.W $FFFF,$FFFE		; magic value to end copperlist

*******************************************************************************
	SECTION "ChipBuffers",BSS_C	;BSS doesn't count toward exe size
*******************************************************************************

SCREEN1:		DS.W 1		; Define storage for buffer 1
SCREEN2:		DS.W 1		; two buffers
GLITCHBUFFER:	DS.B 0		; some free space for glitch
	END