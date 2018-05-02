#!/usr/bin/bash

#Setup environment
module load intel/17.0.0

###TERREL###
#Compile TERREL:
cd CALPUFF_EXE
ifort -O0 -fltconsistency ../CALPUFF_SRC/TERREL/terrel.for -o terrel_intel.exe
cd ..
#Remove any old files:
rm -rf ./CALPUFF_OUT/TERREL/*
rm -rf *.dat *.grd *.lst *.sav
#Run TERREL:
./CALPUFF_EXE/terrel_intel.exe ./CALPUFF_INP/terrel.inp
#Move output files:
mv *.dat *.grd *.lst *.sav ./CALPUFF_OUT/TERREL/.