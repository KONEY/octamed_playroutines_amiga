# octamed_playroutines_amiga
MED playroutines in ASM. Refactored, fixed and extended by KONEY 2021.
Based on Routines for programmers utilising the OctaMED Program: http://m68k.aminet.net/package/dev/src/OctaMED-R
I managed to make this code work with Photon's mini Wrapper and I'm adding more things.
Any help would be appreciated :)

![Preview](https://github.com/KONEY/octamed_playroutines_amiga/blob/main/preview.png)

# I'm maintaining this because I want to make OctaMED SoundStudio known to the scene.
Actually most releases use Protracker .MOD and .P61 player but I've understood there's a lack of knowledge of how powerful OctaMED SS is. First, it can load and save PT format so its already a good thing because OctaMED is a full Intuition Amiga application, not a toy with a custom GUI. Not to mention its ARexx interface and useful scripts! It also has unique features like special commands, command pages (one of the most powerful), SynthSounds and Hybrid sounds and more. When saving for Protracker if some of these feature were used within the song a requester would notify the user, so it's always clear what's going on. But to use those feature in a Demo we need playroutines mantained, optimized and improved so here they are.

# Resources
http://m68k.aminet.net/package/mus/edit/OctamedSS1.03c  - The latest and best version for Amiga 68k
https://github.com/KONEY/octamed-arexx-repo - My ARexx Scripts
https://aminet.net/package/mods/instr/bc-medsounds  - SynthSounds DOC in .txt
https://docs.google.com/document/d/1l_4wyp5bhGYkN5EHM8PFY6nHXuFM0In--8Uf_R8kh1A/edit?usp=sharing - SynthSounds DOC Google Docs version
https://github.com/KONEY/lost_octamed_files_amiga - First production using this player.

# Release notes for V1.2 - 30.04.2022
Got rid of code to handle Aura and MIDI. As a matter of fact, these routines are here for demos development and MIDI
or Aura are never used. Old code can be found on a separate branch, not maintained.
Having less code to deal with a refactor became possible, so now everything is divided using sublabels so now
code isn't a mess anymore.

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
