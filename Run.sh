#!/usr/bin/bash

#Setup environment
set -e #stop at first error
module load intel/17.0.0
module load python2 python-libs

#Read in command line arguments
rundate=$1

#Set flags
runTERREL=false
runCTGPROC=false
runMAKEGEO=false
run3DDAT=false
runCALMET=true

echo "### RUNNING FORECAST SYSTEM FOR DATE "${rundate}" ###"

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
  echo "### ATTEMPTING TO DOWNLOAD NAM DATA"
  #Make data directory if required:
  if [ ! -d ./NAM_data/${rundate}  ]; then
    mkdir NAM_data/${rundate}
  fi
  cd NAM_data/${rundate}
  #Download each NAM data file if required:
  for i in `seq 0 3 48`; do
    hour=`printf "%02d" $i`
    if [ ! -f nam.t00z.afwaca${hour}.tm00.grib2 ]; then
      echo "### DOWNLOADING DATA FOR FORECAST HOUR "${hour}" ###"
      #Entire GRIB file:
      #wget http://www.ftp.ncep.noaa.gov/data/nccf/com/nam/prod/nam.${rundate}/nam.t00z.afwaca${hour}.tm00.grib2 
      #Subset of GRIB file using GRIB filter (http://nomads.ncep.noaa.gov/cgi-bin/filter_nam_crb.pl):
      curl "http://nomads.ncep.noaa.gov/cgi-bin/filter_nam_crb.pl?file=nam.t00z.afwaca"${hour}".tm00.grib2&"\
"lev_1000_mb=on&lev_100_mb=on&lev_10_mb=on&lev_150_mb=on&lev_200_mb=on&lev_20_mb=on&lev_250_mb=on&"\
"lev_2_mb=on&lev_300_mb=on&lev_30_mb=on&lev_400_mb=on&lev_500_mb=on&lev_50_mb=on&lev_5_mb=on&"\
"lev_600_mb=on&lev_700_mb=on&lev_75_mb=on&lev_7_mb=on&lev_800_mb=on&lev_850_mb=on&lev_900_mb=on&"\
"lev_925_mb=on&lev_950_mb=on&lev_mean_sea_level=on&var_HGT=on&var_PRMSL=on&var_RH=on&var_TMP=on&var_UGRD=on&var_VGRD=on&"\
"var_DZDT=on&subregion=&leftlon=272&rightlon=278&toplat=16&bottomlat=10&dir=%2Fnam."${rundate} \
-o nam.t00z.afwaca${hour}.tm00.grib2
    fi
  done
  cd ../..
  echo " ---> FINISHED ###"
fi
#Extract NAM data into CALMET input file format:
if [ "$run3DDAT" = true ]; then
  echo "### EXTRACTING NAM DATA INTO 3D.DAT FILE"
  rm -f data/3d.dat
  cd Python
  ./Create3DDAT.py ${rundate}
  cd ..
  echo " ---> FINISHED ###"
fi

###CALMET###
if [ "$runCALMET" = true ]; then
  #Compile CALMET if required:
  cd CALPUFF_EXE
  if [ ! -f ./calmet_intel.exe ]; then
      echo -n "### COMPILING CALMET"
      ifort -O0 -fltconsistency -w ../CALPUFF_SRC/CALMET/calmet.for -o calmet_intel.exe
      echo " ---> FINISHED ###"
  else
      echo "### CTGPROC ALREADY COMPILED ###"
  fi
  cd ..
  #Copy data file from MAKEGEO across to the data directory
  echo -n "### COPYING GEO DATA FILE ACROSS"
  cp -f ./CALPUFF_OUT/MAKEGEO/geo_masaya.dat data/.
  echo " ---> FINISHED ###"
  #Remove any old files before running:
  echo -n "### DELETING OLD CALMET OUTPUT FILES"
  rm -rf *.dat *.DAT *.bna *.lst *.aux
  cd CALPUFF_OUT/CALMET
  find . ! -name 'README' -type f -exec rm -f {} +
  cd ../..
  echo " ---> FINISHED ###"
  #Run CALMET:
  echo "### RUNNING CALMET"
  ./CALPUFF_EXE/calmet_intel.exe ./CALPUFF_INP/calmet.inp
  echo " ---> FINISHED ###"
  #Move output files:
  echo -n "### MOVING CALMET OUTPUT FILES"
  mv *.dat *.DAT *.bna *.lst *.aux ./CALPUFF_OUT/CALMET/.
  echo " ---> FINISHED ###"
fi
echo "### SUCCESSFULLY COMPLETED FORECAST ###"