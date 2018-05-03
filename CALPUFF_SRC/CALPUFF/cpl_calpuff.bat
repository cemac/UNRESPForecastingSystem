REM Compiling and linking with CALPUFF using Lahey LF95 for Windows

lf95 modules.for calpuff.for -o0 -co -sav -out calpuff.exe >cpl_calpuff.txt

pause

Rem Do not use trap(doi) for programs that include ISORROPIA
rem lf95 modules.for calpuff.for -o0 -co -sav -trap doi -out calpuff.exe >cpl_calpuff.txt

del *.obj
del *.map
del *.mod

rem Switch settings ------------------------------
rem -o0             No optimization
rem -co             Display the compiler options that are used
rem -sav            Save local variables
rem -trap doi       Trap NDP divide-by-zero (d), overflow (o), and invalid operation (i) 
rem -out            Name the compiled executable to "calpuff.exe"
rem >               Send compiler screen output to file "cpl_calpuff.txt"
