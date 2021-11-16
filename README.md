# octamed_playroutines_amiga
MED playroutines in ASM. Refactored, fixed and extended by KONEY 2021
Based on Routines for programers utilising the OctaMED Program: http://m68k.aminet.net/package/dev/src/OctaMED-R
I managed to make this code work with Photon's mini Wrapper and I'm adding more things.
Any help would be appreciated :)

![Preview](https://github.com/KONEY/octamed_playroutines_amiga/blob/main/preview.png)

# Release notes for V1.0
Fixed a few thing but mostly added support for visualizations. In orded to do that I got inspiration from P61 player
but added some interesting things like tracking of what instrument number is playing and what note.
There in my opinion are much more useful values to sync events to music, better than using special commands therefore
for the moment I'm not going to implement those.

Magic values:
AUDIOCHLEV_0:	DC.W 0	; Audio level of CH 0, 1 etc.

MED_TRK_0_INST:	DC.B 0	; Instrument # being played on CH 0, 1 etc.
MED_TRK_0_NOTE:	DC.B 0	; As above but note number

KNOWN BUGS:
For now level tracking is not working for synthsounds but I'll try to fix this in next release.
On my A600 with Vampire some notes are triggered higher in pitch, this is very random.

.EOF
