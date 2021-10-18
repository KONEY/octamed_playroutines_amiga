;*** ATTEMPTS TO PLAY MED MUSIC
;*** MiniStartup by Photon ***
	INCDIR	"NAS:AMIGA/CODE/octamed_playroutines_amiga/"
	SECTION	"Code",CODE
	INCLUDE	"PhotonsMiniWrapper1.04!.S"
	INCLUDE	"Blitter-Register-List.S"
;********** Constants **********
w=320		;screen width, height, depth
h=256
bpls=4		;handy values:
bpl=w/16*2	;byte-width of 1 bitplane line (40)
bwid=bpls*bpl	;byte-width of 1 pixel line (all bpls)

POS_TOP=124*bpl
POS_LEFT=16
POS_MID=4
POS_RIGHT=20
POS_BOTTOM=122*bpl
BAND_OFFSET=86*bpl
bltx	=0	;BLITTER CONSTANTS
bltoffs	=210*(w/8)+bltx/8
;********** Demo **********	; Demo-specific non-startup code below.
	;CLR.W	$100			; DEBUG | w 0 100 2
Demo:	;a4=VBR, a6=Custom Registers Base addr
	;*--- init ---*
	move.l	#VBint,$6c(a4)
	move.w	#%1110000000100000,INTENA
	;** SOMETHING INSIDE HERE IS NEEDED TO MAKE MOD PLAY! **
	move.w	#%1000001111000000,DMACON
	;*--- clear screens ---*
	lea	Screen1,a1
	bsr.w	ClearScreen
	lea	Screen2,a1
	bsr.w	ClearScreen
	bsr.w	WaitBlitter
	;*--- start copper ---*
	lea	Screen1,a0
	moveq	#bpl,d0
	lea	BplPtrs+2,a1
	moveq	#bpls-1,d1
	bsr.w	PokePtrs

	; #### CPU INTENSIVE TASKS BEFORE STARTING MUSIC
	BSR.W	__CREAPATCH	; FILL THE BUFFER
	BSR.W	__PRINT2X
	; #### CPU INTENSIVE TASKS BEFORE STARTING MUSIC

	;CLR.W	$100		; DEBUG | w 0 100 2
	; in photon's wrapper comment:;move.w d2,$9a(a6) ;INTENA
	JSR	_startmusic

	MOVE.L	#Copper,$80(a6)

;********************  main loop  ********************
MainLoop:
	move.w	#$12c,d0		;No buffering, so wait until raster
	bsr.w	WaitRaster	;is below the Display Window.
	;*--- swap buffers ---*
	movem.l	DrawBuffer,a2-a3
	exg	a2,a3
	movem.l	a2-a3,DrawBuffer	;draw into a2, show a3
	;*--- show one... ---*
	move.l	a3,a0
	move.l	#bpl*256,d0
	lea	BplPtrs+2,a1
	moveq	#bpls-1,d1
	bsr.w	PokePtrs
	;*--- ...draw into the other(a2) ---*
	move.l	a2,a1
	;bsr	ClearScreen
	bsr.w	WaitBlitter
	MOVE.L	KONEYBG,DrawBuffer
	; do stuff here :)

	MOVE.W	MED_STEPSEQ_POS,D0
	ANDI.W	#15,D0
	MOVE.W	D0,MED_STEPSEQ_POS
	MOVE.W	MED_SONG_POS,D0

	;BSR.W	__CREATESCROLLSPACE	; NOW WE USE THE BLITTER HERE!
	;BSR.W	__BLITINPLACE		; FIRST BLITTATA
	;BSR.W	__SHIFTTEXT		; SHIFT DATI BUFFER?
	;BSR.W	__POPULATETXTBUFFER	; PUT SOMETHING

	;*--- main loop end ---*
	;BTST	#6,$BFE001
	;BNE.S	.DontShowRasterTime
	;MOVE.W	#$0F0,$180(A6)	; show rastertime left down to $12c
	;.DontShowRasterTime:

	LEA	COPPERWAITS+6,A1
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

	; # CODE FOR BUTTON PRESS ##
	BTST	#6,$BFE001
	BNE.S	.skip
	MOVE.W	#$0FF0,$DFF180	; show rastertime left down to $12c
	.skip:

	BTST	#6,$BFE001
	BNE.S	.DontShowRasterTime
	TST.W	LMBUTTON_STATUS
	BNE.S	.DontShowRasterTime
	MOVE.W	#1,LMBUTTON_STATUS
	ADD.W	#16,MED_SONG_POS
	.DontShowRasterTime:
	BTST	#6,$BFE001
	BEQ.S	.DontResetStatus
	MOVE.W	#0,LMBUTTON_STATUS
	.DontResetStatus:

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
	movem.l	d0/a6,-(sp)	; Save used registers
	lea	$dff000,a6
	btst	#5,$1f(a6)	; check if it's our vertb int.
	beq.s	.notvb
	;*--- do stuff here ---*
	moveq	#$20,d0		; poll irq bit
	move.w	d0,$9c(a6)
	move.w	d0,$9c(a6)
	.notvb:	
	movem.l	(sp)+,d0/a6	; restore
	rte

__PRINT2X:
	MOVEM.L	D0-A6,-(SP)	; SAVE TO STACK
	MOVEQ	#bpls-1,D1	; UGUALI PER TUTTI I BITPLANE
	MOVE.L	KONEYBG,A4
	LEA	DISPLACETABLE,A3
	LEA	PATCH,A0
	.OUTERLOOP:
	LEA	KONEY2X,A5
	MOVEQ	#0,D6		; RESET D6
	MOVE.B	#9,D6			
	ADD.W	#POS_TOP,A4	; POSITIONING
	.INNERLOOP:
	MOVE.W	DISPLACEINDEX,D7
	ADD.W	#POS_LEFT,A4	; POSITIONING
	MOVE.L	(A0)+,D2		; SALVO SFONDO
	MOVE.L	(A5)+,D3		
	MOVE.L	(A3,D7.W),D5	; FX 1

	OR.L	D2,D3		; KOMBINO SFONDO+SKRITTA
	MOVE.L	D3,(A4)		
	ADD.W	#POS_MID,A4	; POSITIONING

	MOVE.L	(A0)+,D2		; SALVO SFONDO
	MOVE.L	(A5)+,D3

	OR.L	D2,D3		; KOMBINO SFONDO+SKRITTA
	MOVE.L	D3,(A4)		
	ADD.W	#POS_RIGHT,A4	; POSITIONING
	DBRA	D6,.INNERLOOP
	ADD.W	#POS_BOTTOM,A4	; POSITIONING
	DBRA	D1,.OUTERLOOP
	MOVEM.L	(SP)+,D0-A6	; FETCH FROM STACK
	RTS

__CREAPATCH:
	MOVEM.L	D0-A6,-(SP)	; SAVE TO STACK
	MOVEQ	#bpls-1,D1	; UGUALI PER TUTTI I BITPLANE
	MOVE.L	KONEYBG,A4
	LEA	PATCH,A5
	.OUTERLOOP:
	MOVEQ	#0,D6		; RESET D6
	MOVE.B	#9,D6
	ADD.W	#POS_TOP,A4	; POSITIONING
	.INNERLOOP:
	ADD.W	#POS_LEFT,A4	; POSITIONING
	MOVE.L	(A4),(A5)+	
	ADD.W	#POS_MID,A4	; POSITIONING
	MOVE.L	(A4),(A5)+	
	ADD.W	#POS_RIGHT,A4	; POSITIONING
	DBRA	D6,.INNERLOOP
	ADD.W	#POS_BOTTOM,A4	; POSITIONING
	DBRA	D1,.OUTERLOOP
	MOVEM.L	(SP)+,D0-A6	; FETCH FROM STACK
	RTS

__CREATESCROLLSPACE:
	MOVEM.L	D0-D7/A0-A6,-(SP)	; SAVE TO STACK
	MOVEQ	#bpls-1,D1	; UGUALI PER TUTTI I BITPLANE
	MOVE.L	KONEYBG,A4
	.OUTERLOOP:
	MOVEQ	#0,D6		; RESET D6
	MOVE.B	#10*11-1,D6
	ADD.W	#POS_TOP+BAND_OFFSET,A4	; POSITIONING
	.INNERLOOP:
	MOVE.L	#0,(A4)+	
	DBRA	D6,.INNERLOOP
	ADD.W	#POS_BOTTOM-BAND_OFFSET-bpl,A4	; POSITIONING
	DBF	D1,.OUTERLOOP
	MOVEM.L	(SP)+,D0-D7/A0-A6	; FETCH FROM STACK
	RTS

__BLITINPLACE:
	MOVEM.L	D0-A6,-(SP)	; SAVE TO STACK
	MOVE.L	KONEYBG,A4
	ADD.W	#bltoffs+40,A4

	BTST.B	#6,DMACONR	; for compatibility
	bsr	WaitBlitter

	MOVE.L	A4,BLTDPTH
	MOVE.W	#$FFFF,BLTAFWM	; BLTAFWM lo spiegheremo dopo
	MOVE.W	#$FFFF,BLTALWM	; BLTALWM lo spiegheremo dopo
	MOVE.W	#$09F0,BLTCON0	; BLTCON0 (usa A+D)
	MOVE.W	#%0000000000000000,BLTCON1	; BLTCON1 lo spiegheremo dopo
	MOVE.W	#0,BLTAMOD	; BLTAMOD =0 perche` il rettangolo

	MOVE.W	#0,BLTDMOD	; BLTDMOD 40-4=36 il rettangolo

	MOVE.L	#TXTSCROLLBUF,BLTAPTH	; BLTAPT  (fisso alla figura sorgente)

	MOVE.W	#8*64+320/16,BLTSIZE	; BLTSIZE (via al blitter !)
				; adesso, blitteremo una figura di
				; 2 word X 6 linee con una sola
				; blittata coi moduli opportunamente
				; settati per lo schermo.
				; BLTSIZE = (Altezza in righe)
				; * 64 + (Larghezza in pixel)/16 
	MOVEM.L	(SP)+,D0-A6	; FETCH FROM STACK
	RTS

__SHIFTTEXT:
	MOVEM.L	D0-A6,-(SP)	; SAVE TO STACK
	BTST.B	#6,DMACONR	; for compatibility
	bsr	WaitBlitter

	MOVE.W	#$FFFF,BLTAFWM	; BLTAFWM lo spiegheremo dopo
	MOVE.W	#$FFFF,BLTALWM	; BLTALWM lo spiegheremo dopo
	MOVE.W	#%0010100111110000,BLTCON0	; BLTCON0 (usa A+D); con shift di un pixel
	MOVE.W	#%0000000000000010,BLTCON1	; BLTCON1 BIT 12 DESC MODE
	MOVE.W	#0,BLTAMOD	; BLTAMOD =0 perche` il rettangolo
				; sorgente ha le righe consecutive
				; in memoria.

	MOVE.W	#0,BLTDMOD	; BLTDMOD 40-4=36 il rettangolo
				; destinazione e` all'interno di un
				; bitplane largo 20 words, ovvero 40
				; bytes. Il rettangolo blittato
				; e` largo 2 words, cioe` 4 bytes.
				; Il valore del modulo e` dato dalla
				; differenza tra le larghezze

	MOVE.L	#_TXTSCROLLBUF-2,BLTAPTH	; BLTAPT  (fisso alla figura sorgente)
	MOVE.L	#_TXTSCROLLBUF-2,BLTDPTH

	MOVE.W	#8*64+320/16,BLTSIZE	; BLTSIZE (via al blitter !)
				; adesso, blitteremo una figura di
				; 2 word X 6 linee con una sola
				; blittata coi moduli opportunamente
				; settati per lo schermo.
				; BLTSIZE = (Altezza in righe)
				; * 64 + (Larghezza in pixel)/16 
	MOVEM.L	(SP)+,D0-A6	; FETCH FROM STACK
	RTS

__POPULATETXTBUFFER:
	MOVEM.L	D0-D7/A0-A6,-(SP)	; SAVE TO STACK
	MOVE.W	FRAMESINDEX,D7
	CMP.W	#4,D7
	BNE.W	.SKIP
	LEA	TXTSCROLLBUF,A4
	LEA	FONT,A5
	LEA	TEXT,A6

	ADD.W	TEXTINDEX,A6
	CMP.L	#_TEXT,A6	; Siamo arrivati all'ultima word della TAB?
	BNE.S	.PROCEED
	MOVE.W	#0,TEXTINDEX	; Riparti a puntare dalla prima word
	.PROCEED:
	MOVE.B	(A6),D2		; Prossimo carattere in d2
	SUB.B	#$20,D2		; TOGLI 32 AL VALORE ASCII DEL CARATTERE, IN
	MULU.W	#8,D2		; MOLTIPLICA PER 8 IL NUMERO PRECEDENTE,
	ADD.W	D2,A5
	MOVEQ	#0,D6		; RESET D6
	MOVE.B	#8-1,D6
	.LOOP:
	ADD.W	#38,A4		; POSITIONING
	MOVE.B	(A5)+,(A4)+
	;ADD.W	#1,A4		; POSITIONING
	;ADD.W	#38,A4		; POSITIONING
	MOVE.B	#%00000000,(A4)+
	;ADD.W	#2,A4		; POSITIONING
	DBRA	D6,.LOOP
	.SKIP:
	SUB.W	#1,D7
	CMP.W	#0,D7
	BEQ.W	.RESET
	MOVE.W	D7,FRAMESINDEX
	MOVEM.L	(SP)+,D0-D7/A0-A6	; FETCH FROM STACK
	RTS
	.RESET:
	ADD.W	#1,TEXTINDEX
	MOVE.W	#4,D7
	MOVE.W	D7,FRAMESINDEX	; OTTIMIZZABILE
	MOVEM.L	(SP)+,D0-D7/A0-A6	; FETCH FROM STACK
	RTS

;********** Fastmem Data **********
LMBUTTON_STATUS:	DC.W 0
AUDIOCHANLEVEL0:	DC.W 0
AUDIOCHANLEVEL1:	DC.W 0
AUDIOCHANLEVEL2:	DC.W 0
AUDIOCHANLEVEL3:	DC.W 0
FRAMESINDEX:	DC.W 4

KONEYBG:		DC.L SCREEN1	; INIT BG
DrawBuffer:	DC.L SCREEN2	; pointers to buffers to be swapped
ViewBuffer:	DC.L SCREEN1

DISPLACEINDEX:	DC.W 0
DISPLACETABLE:
	DC.W 4,3,0,1,0,0,0,0,2,0,0,0,0,3,0,2
	DC.W 0,0,0,5,0,1,0,0,0,0,3,0,1,0,1
	DC.W 0,1,0,2,0,0,0,0,0,2,0,3,0,0,6,1
	DC.W 0,0,0,3,0,0,0,0,3,0,0,7,0,0,0,0
	DC.W 1,3,5,2,0,1,0,3,0,0,4,0,1,0,7,8
	DC.W 0,0,3,0,0,0,0,1,4,0,0,0,0,8,1
	DC.W 2,1,0,1,0,3,0,3,0,0,0,1,2,1,0,0
	DC.W 0,2,0,0,3,0,0,0,0,1,0,0,0,2,1,4

PATCH:		DS.B 10*64*bpls	;I need a buffer to save trap BG

TEXTINDEX:	DC.W 0
TEXT:
	DC.B "  OCTAMED ASSEMBLY PLAYROUTINES... AND IT WORKS!!! PROBLEM WAS ON PHOTON'S WRAPPER WHERE INTENA BITS WERE ALL RESET... "
	DC.B "THIS WORKS INSIDE INTRO/DEMOS OR ANY OTHER CODE WHICH DIRECTLY BANGS THE AMIGA "
	DC.B "HARDWARE. THIS IS EVEN MORE COMPLICATED THAN I SUSPECTED AND I NEED HELP :)  "
	DC.B "                                                                  "
	EVEN
_TEXT:

;*******************************************************************************
	INCLUDE	"med/MED_PlayRoutine.i"

;*******************************************************************************
	SECTION	"ChipData",DATA_C	;declared data that must be in chipmem
;*******************************************************************************

TXTSCROLLBUF:	DS.B	(bpl)*8
_TXTSCROLLBUF:

MED_MODULE:	INCBIN	"med/LOST_OCTAMED_FILES_1.MED"	;<<<<< MODULE NAME HERE!
KONEY2X:		INCBIN	"koney10x64.raw"
BG1:		INCBIN	"BG_METAL2_320256_4.raw"

FONT:		DC.L	0,0			; SPACE CHAR
		INCBIN	"scummfnt_8x752.raw",0

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

Palette:						;Some kind of palette (3 bpls=8 colors)
	DC.W $0180,$0000,$0182,$0000,$0184,$0111,$0186,$0122
	DC.W $0188,$0333,$018A,$0444,$018C,$0555,$018E,$0455
	DC.W $0190,$0666,$0192,$0888,$0194,$0999,$0196,$0AAA
	DC.W $0198,$09AA,$019A,$0FFF,$019C,$0FFF,$019E,$0FFF

BplPtrs:
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

COPPERWAITS:
	DC.W $F207,$FFFE
	DC.W $180,$001
	DC.W $F407,$FFFE
	DC.W $180,$000

	DC.W $F507,$FFFE
	DC.W $180,$001
	DC.W $F707,$FFFE
	DC.W $180,$000

	DC.W $F807,$FFFE
	DC.W $180,$001
	DC.W $FA07,$FFFE
	DC.W $180,$000

	DC.W $FB07,$FFFE
	DC.W $180,$001
	DC.W $FD07,$FFFE
	DC.W $180,$000

	;DC.W $FFDF,$FFFE	; allow VPOS>$ff

	DC.W $FFFF,$FFFE	;magic value to end copperlist
_Copper:

;*******************************************************************************
	SECTION "ChipBuffers",BSS_C	;BSS doesn't count toward exe size
;*******************************************************************************

SCREEN1:		DS.B h*bwid	; Define storage for buffer 1
SCREEN2:		DS.B 0		; two buffers
GLITCHBUFFER:	DS.B 0		; some free space for glitch

	END
