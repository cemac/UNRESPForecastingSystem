#!/usr/bin/bash

#Setup environment
module load intel/17.0.0

#Set flags
runTERREL=true
runCTGPROC=true

###TERREL###
if [ "$runTERREL" = true ]; then
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
  rm -rf *.dat *.grd *.lst *.sav *.log
  cd CALPUFF_OUT/TERREL
  find . ! -name 'README' -type f -exec rm -f {} +
  cd ../..
  echo " ---> FINISHED ###"
  #Run TERREL:
  echo "### RUNNING TERREL"
  ./CALPUFF_EXE/terrel_intel.exe ./CALPUFF_INP/terrel.inp > terrel.log
  echo " ---> FINISHED ###"
  #Move output files:
  echo -n "### MOVING TERREL OUTPUT FILES"
  mv *.dat *.grd *.lst *.sav *.log ./CALPUFF_OUT/TERREL/.
  echo " ---> FINISHED ###"
fi

###CTGPROC###
if [ "$runCTGPROC" = true ]; then
  #Compile CTGPROC if required:
  cd CALPUFF_EXE
  if [ ! -f ./ctgproc_intel.exe ]; then
      echo "### COMPILING CTGPROC"
      ifort -O0 -fltconsistency ../CALPUFF_SRC/CTGPROC/ctgproc.for -o ctgproc_intel.exe
      echo " ---> FINISHED ###"
  else
      echo "### CTGPROC ALREADY COMPILED ###"
  fi
  cd ..
  #Remove any old files before running:
  echo -n "### DELETING OLD CTGPROC OUTPUT FILES"
  rm -rf *.dat *.lst *.log
  cd CALPUFF_OUT/CTGPROC
  find . ! -name 'README' -type f -exec rm -f {} +
  cd ../..
  echo " ---> FINISHED ###"
  #Run CTGPROC:
  echo "### RUNNING CTGPROC"
  ./CALPUFF_EXE/ctgproc_intel.exe ./CALPUFF_INP/ctgproc.inp > ctgproc.log
  echo " ---> FINISHED ###"
  #Move output files:
  echo -n "### MOVING CTGPROC OUTPUT FILES"
  mv *.dat *.lst *.log ./CALPUFF_OUT/CTGPROC/.
  echo " ---> FINISHED ###"
fi