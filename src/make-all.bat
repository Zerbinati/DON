@REM @SET PATH=C:/MinGW/32/bin/;C:/MinGW/msys/bin/;

@REM make -f Makefile build         ARCH=general-32 COMP=mingw
@REM make -f Makefile build         ARCH=x86-32-old COMP=mingw
@REM make -f Makefile build         ARCH=x86-32     COMP=mingw

@REM make -f Makefile profile-build ARCH=x86-32     COMP=mingw

@REM -----------------------------------------------------

@SET PATH=C:/MinGW/64/bin/;C:/MinGW/msys/bin/;

@REM make -f Makefile build         ARCH=general-64 COMP=mingw
@REM make -f Makefile build         ARCH=x86-64     COMP=mingw
@REM make -f Makefile profile-build ARCH=general-64 COMP=mingw
@REM make -f Makefile profile-build ARCH=x86-64     COMP=mingw

make -f Makefile build         ARCH=x86-64     COMP=mingw debug=yes

@PAUSE
