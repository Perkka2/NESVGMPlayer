This is a VGM Player and Parser for the NES,
It parses NES APU, Sunsoft 5B, YM2608, YM2610, YM2612, YM2203 VGM files
Currently (2020-10-01) There are no emulator support for Yamaha OPN Derivatives but basic functionality has been tested with a NES with a OPNA Attached.
There is currently no *DPCM support.

The php script inputs a VGM file and the output data is intended to be written into the BANK00 segment at the end of nesvgmplay.s

This is configured for the MMC3 mapper and a 256kb PRG ROM, for Sunsoft 5B audio a modified emulator with Sunsoft audio on MMC3 is needed.

This depends on the CC65 toolchain, though it only uses the ca65 assembler, not the C compiler:

https://cc65.github.io/

Base code to get me started came from:

Brad Smith
http://rainwarrior.ca
