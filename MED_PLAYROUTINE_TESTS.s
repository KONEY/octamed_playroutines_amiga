;*** WORKING PLAY MED MUSIC
;*** MiniStartup by Photon ***
	INCDIR	"NAS:AMIGA/CODE/octamed_playroutines_amiga/"
	SECTION	"Code",CODE
	INCLUDE	"custom-registers.i"
	INCLUDE	"PhotonsMiniWrapper1.04!.S"
	INCLUDE	"med/med_feature_control.i"		; MED CFGs
;********** Constants **********
w=320		;screen width, height, depth
h=256
bpls=3		;handy values:
bpl=w/16*2	;byte-width of 1 bitplane line (40)
bwid=bpls*bpl	;byte-width of 1 pixel line (all bpls)

;********** Demo **********	; Demo-specific non-startup code below.
	;CLR.W	$100			; DEBUG | w 0 100 2
Demo:	;a4=VBR, a6=Custom Registers Base addr
	;*--- init ---*
	move.l	#VBint,$6C(A4)
	MOVE.W	#%1110000000000000,INTENA	; Master and lev6	; NO COPPER-IRQ!
	MOVE.W	#%1000011111100000,DMACON
	;*--- clear screens ---*
	lea	Screen1,a1
	;bsr.w	ClearScreen
	lea	Screen2,a1
	;bsr.w	ClearScreen
	;bsr.w	WaitBlitter
	;*--- start copper ---*
	lea	Screen1,a0
	moveq	#bpl,d0
	lea	Copper\.BplPtrs+2,a1
	moveq	#bpls-1,d1
	bsr.w	PokePtrs

	; #### CPU INTENSIVE TASKS BEFORE STARTING MUSIC
	; #### CPU INTENSIVE TASKS BEFORE STARTING MUSIC

	; in photon's wrapper comment:;move.w d2,$9a(a6) ;INTENA
	JSR	_startmusic

	MOVE.L	#Copper,COP1LC
;********************  main loop  ********************
MainLoop:
	move.w	#$12C,D0		;No buffering, so wait until raster
	bsr.w	WaitRaster	;is below the Display Window.
	;*--- swap buffers ---*
	movem.l	DrawBuffer,a2-a3
	exg	a2,a3
	movem.l	a2-a3,DrawBuffer	;draw into a2, show a3
	;*--- show one... ---*
	move.l	a3,a0
	move.l	#bpl*h,d0
	lea	Copper\.BplPtrs+2,a1
	moveq	#bpls-1,d1
	bsr.w	PokePtrs
	;*--- ...draw into the other(a2) ---*
	move.l	a2,a1
	;bsr	ClearScreen
	;bsr.w	WaitBlitter
	MOVE.L	KONEYBG,DrawBuffer
	; do stuff here :)

	MOVE.W	MED_STEPSEQ_POS,D0
	ANDI.W	#15,D0
	MOVE.W	D0,MED_STEPSEQ_POS
	MOVE.W	MED_SONG_POS,D0

	;*--- main loop end ---*
	;BTST	#6,$BFE001
	;BNE.S	.DontShowRasterTime
	;MOVE.W	#$0F0,$180(A6)	; show rastertime left down to $12c
	;.DontShowRasterTime:

	IFNE	INSTR_TRACKING

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

	;CLR.W	$100		; DEBUG | w 0 100 2
	;BRA.W	.skip2

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

	lea 16(a1),a1
	CLR.L	D0
	MOVE.B	MED_TRK_1_INST,D0
	ROL.L	#$2,D0		; expand bits to green
	MOVE.W	D0,(A1)
	MOVE.L	D0,D3
	ROL.L	#$4,D3		; expand bits to green
	ADD.L	D3,D0
	ROL.L	#$4,D3
	ADD.L	D3,D0		; expand bits to red

	lea 16(a1),a1
	CLR.L	D0
	MOVE.B	MED_TRK_2_INST,D0
	ROL.L	#$2,D0		; expand bits to green
	MOVE.W	D0,(A1)
	MOVE.L	D0,D3
	ROL.L	#$4,D3		; expand bits to green
	ADD.L	D3,D0
	ROL.L	#$4,D3
	ADD.L	D3,D0		; expand bits to red

	lea 16(a1),a1
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

	lea 16(a1),a1
	CLR.L	D0
	MOVE.B	MED_TRK_1_NOTE,D0
	ROL.L	#$2,D0		; expand bits to green
	MOVE.W	D0,(A1)
	MOVE.L	D0,D3
	ROL.L	#$4,D3		; expand bits to green
	ADD.L	D3,D0
	ROL.L	#$4,D3
	ADD.L	D3,D0		; expand bits to red

	lea 16(a1),a1
	CLR.L	D0
	MOVE.B	MED_TRK_2_NOTE,D0
	ROL.L	#$2,D0		; expand bits to green
	MOVE.W	D0,(A1)
	MOVE.L	D0,D3
	ROL.L	#$4,D3		; expand bits to green
	ADD.L	D3,D0
	ROL.L	#$4,D3
	ADD.L	D3,D0		; expand bits to red

	lea 16(a1),a1
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
	ENDC

	; # CODE FOR BUTTON PRESS ##
	BTST	#6,$BFE001
	BNE.S	.skip
	MOVE.W	#$0FF0,$DFF180	; show rastertime left down to $12c
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

	BTST	#2,$DFF016	; POTINP - RMB pressed?
	BNE.W	MainLoop		; then loop
	;*--- exit ---*
	; ---  quit MED code  ---
	MOVEM.L	D0-A6,-(SP)
	JSR	_endmusic
	MOVEM.L	(SP)+,D0-A6
	RTS
;********** Demo Routines **********

PokePtrs:				; Generic, poke ptrs into copper list
	.bpll:	
	move.l	a0,d2
	swap	d2
	move.w	d2,(a1)		;high word of address
	move.w	a0,4(a1)		;low word of address
	addq.w	#8,a1		;skip two copper instructions
	add.l	d0,a0		;next ptr
	dbf	d1,.bpll
	rts
ClearScreen:			; a1=screen destination address to clear
	bsr.W	WaitBlitter
	clr.w	$66(a6)		; destination modulo
	move.l	#$01000000,$40(a6)	; set operation type in BLTCON0/1
	move.l	a1,$54(a6)	; destination address
	move.l	#h*bpls*64+bpl/2,$58(a6)	;blitter operation size
	rts
VBint:				; Blank template VERTB interrupt
	btst	#5,$DFF01F	; check if it's our vertb int.
	beq.s	.notvb
	move.w	#$20,$DFF09C	; poll irq bit
	move.w	#$20,$DFF09C	; KONEY REFACTOR
	.notvb:	
	rte

;********** Fastmem Data **********
LMBUTTON_STATUS:	DC.W 0
AUDIOCHLEV_0:	DC.W 0
AUDIOCHLEV_1:	DC.W 0
AUDIOCHLEV_2:	DC.W 0
AUDIOCHLEV_3:	DC.W 0
FRAMESINDEX:	DC.W 4

KONEYBG:		DC.L BG1	; INIT BG
DrawBuffer:	DC.L SCREEN2	; pointers to buffers to be swapped
ViewBuffer:	DC.L SCREEN1

;*******************************************************************************
	INCLUDE	"med/MED_PlayRoutine.i"
;*******************************************************************************
	SECTION	"ChipData",DATA_C	;declared data that must be in chipmem
;*******************************************************************************
;MED_MODULE:	INCBIN "med/playroutine_test.MED"		;<<<<< MODULE NAME HERE!
MED_MODULE:	INCBIN "med/mammagamma.med"		;<<<<< MODULE NAME HERE!
	;IFNE	SPLIT_RELOCS
_chipzero:	DC.L 0
	;ENDC
		DC.L 0,0	 				; DUMMY

BG1:		INCBIN "GFX_MEDPLAYER.raw"
		DS.B bpl*h*2	

Copper:
	DC.W $1FC,0	;Slow fetch mode, remove if AGA demo.
	DC.W $8E,$2C81	;238h display window top, left
	DC.W $90,$2CC1	;and bottom, right.
	DC.W $92,$38	;Standard bitplane dma fetch start
	DC.W $94,$D0	;and stop for standard screen.

	DC.W $106,$0C00	;(AGA compat. if any Dual Playf. mode)
	DC.W $108,0	;bwid-bpl	;modulos
	DC.W $10A,0	;bwid-bpl	;RISULTATO = 80 ?
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

	.Palette:			;Some kind of palette (3 bpls=8 colors)
	DC.W $0180,$0111,$0182,$0FFF,$0184,$0111,$0186,$0122
	DC.W $0188,$0333,$018A,$0444,$018C,$0555,$018E,$0455
	DC.W $0190,$0666,$0192,$0888,$0194,$0999,$0196,$0AAA
	DC.W $0198,$09AA,$019A,$0FFF,$019C,$0FFF,$019E,$0FFF

	.SpritePointers:
	DC.W $120,0,$122,0	; 0
	DC.W $124,0,$126,0	; 1
	DC.W $128,0,$12A,0	; 2
	DC.W $12C,0,$12E,0	; 3
	DC.W $130,0,$132,0	; 4
	DC.W $134,0,$136,0	; 5
	DC.W $138,0,$13A,0	; 6
	DC.W $13C,0,$13E,0	; 7

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
	DC.W $0182,$0111

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
	DC.W $0182,$0111

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
	DC.W $0182,$0111

	DC.W $FFDF,$FFFE		; allow VPOS>$ff

	DC.W $FFFF,$FFFE		;magic value to end copperlist
_Copper:

;*******************************************************************************
	SECTION "ChipBuffers",BSS_C	;BSS doesn't count toward exe size
;*******************************************************************************

SCREEN1:		DS.B h*bwid	; Define storage for buffer 1
SCREEN2:		DS.B 0		; two buffers
GLITCHBUFFER:	DS.B 0		; some free space for glitch

	END
