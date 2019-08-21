@REM @SET PATH=C:/MinGW/32/bin/;C:/MinGW/msys/bin/;
@SET PATH=C:/MinGW/64/bin/;C:/MinGW/msys/bin/;

make -f Makefile clean COMP=mingw
@REM make -f Makefile clean COMP=gcc

@PAUSE
