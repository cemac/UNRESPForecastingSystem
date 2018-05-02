REM Compiling and linking with TERREL using Lahey LF95 for Windows

lf95 terrel.for -o0 -co -sav -trap doi -out terrel.exe >cpl.txt

del *.obj
del *.map

rem Switch settings ------------------------------
rem -o0             No optimization
rem -co             Display the compiler options that are used
rem -sav            Save local variables
rem -trap doi       Trap NDP divide-by-zero (d), overflow (o), and invalid operation (i) 
rem -out            Name the compiled executable to "terrel.exe"
rem >               Send compiler screen output to file "cpl.txt"
REM Compiling and linking with LF95


