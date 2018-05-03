# Example settings for compiling on Linux with Portland Group 64-bit Compiler (make 32-bit executable!)

pgf90 -O0 -Kieee -Msave -tp k8-32 -L/opt/pgi/linux86/11.5/liblf modules.for calpuff.for -o calpuff.x


# Do not use Ktrap(fp) for programs that include ISORROPIA
# pgf90 -O0 -Kieee -Ktrap=fp -Msave -tp k8-32 -L/opt/pgi/linux86/11.5/liblf modules.for calpuff.for -o calpuff.x


# Switch settings ------------------------------
# pgf90           Portland Group Fortran 90 compiler (64-bit library here)
# -O0             Set the optimization level at Level 0
# -Kieee          Request  special  compilation semantics from the compiler.  Perform float and double divides in
#                    conformance with the IEEE 754 standard
# -Ktrap=fp     
# -Msave          All  local  variables  are  subject to the SAVE statement
# -tp k8-32       Create 32-bit executable
# -L              Library path that is installation-specific
# -o calpuff.exe  Use file as the name of the executable program, rather than the default a.out 
