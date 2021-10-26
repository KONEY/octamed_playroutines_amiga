;********** relevant blitter registers **********
;base reg $dff000

BLTDDAT	= 0	;result of the last word. used for bob collision detection and 
		;MFM decoding
DMACONR	= $DFF002	;bit 14=blitter busy flag

;blitter operation setup
BLTCON0	= $DFF040
BLTCON1	= $DFF042
BLTAFWM	= $DFF044
BLTALWM	= $DFF046

;sources, destination, and size
BLTCPTH	= $DFF048
BLTCPTL	= $DFF04A
BLTBPTH	= $DFF04C
BLTBPTL	= $DFF04E
BLTAPTH	= $DFF050
BLTAPTL	= $DFF052
BLTDPTH	= $DFF054
BLTDPTL	= $DFF056

BLTSIZE	= $DFF058

;ECS/AGA registers
BLTCON0L = $DFF05A
BLTSIZV	= $DFF05C
BLTSIZH	= $DFF05E

;modulos
BLTCMOD	= $DFF060	
BLTBMOD	= $DFF062
BLTAMOD	= $DFF064
BLTDMOD	= $DFF066

;data to replace sources
BLTCDAT	= $DFF070	
BLTBDAT	= $DFF072
BLTADAT	= $DFF074

BPLCON0	= $DFF100
BPLCON1	= $DFF102
BPLCON2	= $DFF104

;bit 6: enable blitter DMA - bit 10: give blitter priority over the CPU
DMACON	= $DFF096

;Interrupt enable bits (clear or set bits)
INTENA	= $DFF09A
INTREQ	= $DFF09C

VPOSR	= $DFF004
VPOSW	= $DFF02A

COP1LC	= $DFF080
COP2LC	= $DFF084

COPJMP1	= $0088
COPJMP2	= $008A

COP1LCH	= $0080
COP1LCL	= $0082
COP2LCH	= $0084
COP2LCL	= $0086