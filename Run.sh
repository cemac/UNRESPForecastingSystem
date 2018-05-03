#!/usr/bin/bash

#Setup environment
set -e #stop at first error
module load intel/17.0.0
module load python2 python-libs

#Read in command line arguments
rundate=$1
echo "### RUNNING FORECAST SYSTEM FOR DATE "${rundate}" ###"

#Set flags
runTERREL=false
runCTGPROC=false
runMAKEGEO=false
run3DDAT=true

###TERREL###
if [ "$runTERREL" = true ]; then
  #Compile TERREL if required:
  cd CALPUFF_EXE
  if [ ! -f ./terrel_intel.exe ]; then
      echo -n "### COMPILING TERREL"
      ifort -O0 -fltconsistency -w ../CALPUFF_SRC/TERREL/terrel.for -o terrel_intel.exe 
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
      echo -n "### COMPILING CTGPROC"
      ifort -O0 -fltconsistency -w ../CALPUFF_SRC/CTGPROC/ctgproc.for -o ctgproc_intel.exe
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

###MAKEGEO###
if [ "$runMAKEGEO" = true ]; then
  #Compile MAKEGEO if required:
  cd CALPUFF_EXE
  if [ ! -f ./makegeo_intel.exe ]; then
      echo -n "### COMPILING MAKEGEO"
      ifort -O0 -fltconsistency -w ../CALPUFF_SRC/MAKEGEO/makegeo.for -o makegeo_intel.exe
      echo " ---> FINISHED ###"
  else
      echo "### MAKEGEO ALREADY COMPILED ###"
  fi
  cd ..
  #Copy data files from TERREL and CTGPROC across to the data directory
  echo -n "### COPYING GEO DATA FILES ACROSS"
  cp -f ./CALPUFF_OUT/TERREL/masaya.dat data/.
  cp -f ./CALPUFF_OUT/CTGPROC/lulc1km_masaya.dat data/.
  echo " ---> FINISHED ###"
  #Remove any old files before running:
  echo -n "### DELETING OLD MAKEGEO OUTPUT FILES"
  rm -rf *.dat *.lst *.clr *.log *.grd
  cd CALPUFF_OUT/MAKEGEO
  find . ! -name 'README' -type f -exec rm -f {} +
  cd ../..
  echo " ---> FINISHED ###"
  #Run MAKEGEO:
  echo "### RUNNING MAKEGEO"
  ./CALPUFF_EXE/makegeo_intel.exe ./CALPUFF_INP/makegeo.inp > makegeo.log
  echo " ---> FINISHED ###"
  #Move output files:
  echo -n "### MOVING MAKEGEO OUTPUT FILES"
  mv *.dat *.lst *.clr *.log *.grd ./CALPUFF_OUT/MAKEGEO/.
  echo " ---> FINISHED ###"
fi

###NAM data###
##Download NAM data if required:
#How many files downloaded already?:
if [ -d ./NAM_data/${rundate} ]; then
  eval numfiles=$(ls ./NAM_data/${rundate} | wc -l)
else
  numfiles=0
fi
#if not 17 files, need to download more:
if [ ${numfiles} != 17 ]; then
  echo "### ATTEMPTING TO DOWNLOAD NAM DATA ###"
  #Make data directory if required:
  if [ ! -d ./NAM_data/${rundate}  ]; then
    mkdir NAM_data/${rundate}
  fi
  cd NAM_data/${rundate}
  #Download each file if required:
  for i in `seq 0 3 48`; do
    hour=`printf "%02d" $i`
    if [ ! -f nam.t00z.afwaca${hour}.tm00.grib2 ]; then
      wget http://www.ftp.ncep.noaa.gov/data/nccf/com/nam/prod/nam.${rundate}/nam.t00z.afwaca${hour}.tm00.grib2
    fi
  done
  cd ../..
fi
#Extract data into CALMET inpt file format:
if [ "$run3DDAT" = true ]; then
  rm -f data/3D.DAT
  cd Python
  ./Create3DDAT.py ${rundate}
  cd ..
fi
