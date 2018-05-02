#!/usr/bin/bash

#Setup environment
module load intel/17.0.0

###TERREL###
#Compile TERREL if required:
cd CALPUFF_EXE
if [ ! -f ./terrel_intel.exe ]; then
    echo "### COMPILING TERREL"
    ifort -O0 -fltconsistency ../CALPUFF_SRC/TERREL/terrel.for -o terrel_intel.exe 
    echo " ---> FINISHED ###"
else
    echo "### TERREL ALREADY COMPILED ###"
fi
cd ..
#Remove any old files before running:
echo -n "### DELETING OLD TERREL OUTPUT FILES"
rm -rf *.dat *.grd *.lst *.sav
cd CALPUFF_OUT/TERREL
shopt -s extglob
rm -- !(README)
cd ../..
echo " ---> FINISHED ###"
#Run TERREL:
echo "### RUNNING TERREL"
./CALPUFF_EXE/terrel_intel.exe ./CALPUFF_INP/terrel.inp > terrel.log
echo " ---> FINISHED ###"
#Move output files:
echo -n "### MOVING TERREL OUTPUT FILES"
mv *.dat *.grd *.lst *.sav ./CALPUFF_OUT/TERREL/.
echo " ---> FINISHED ###"