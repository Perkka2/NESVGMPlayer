@del nesvgmplay.o
@del nesvgmplay.nes
@del nesvgmplay.map.txt
@del nesvgmplay.labels.txt
@del nesvgmplay.nes.ram.nl
@del nesvgmplay.nes.0.nl
@del nesvgmplay.nes.1.nl
@del nesvgmplay.nes.dbg
@echo.
@echo Compiling...
cc65\bin\ca65 nesvgmplay.s -g -o nesvgmplay.o
@IF ERRORLEVEL 1 GOTO failure
@echo.
@echo Linking...
cc65\bin\ld65 -o nesvgmplay.nes -C nesvgmplay.cfg nesvgmplay.o -m nesvgmplay.map.txt -Ln nesvgmplay.labels.txt --dbgfile nesvgmplay.nes.dbg
@IF ERRORLEVEL 1 GOTO failure
@echo.
@echo Generating FCEUX debug symbols...
python nesvgmplay_fceux_symbols.py
@echo.
@echo Success!
@pause
@GOTO endbuild
:failure
@echo.
@echo Build error!
@pause
:endbuild
