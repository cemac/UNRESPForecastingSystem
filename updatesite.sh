#!/bin/bash
# This script was created by CEMAC (University of Leeds) as part of the UNRESP
# Project to RUN VIZ SEPARATELY
 #Setup environment
set -e #stop at first error
module load intel/17.0.0
module load python2 python-libs

# Defaults
rundate=$(date +%Y%m%d)
vizhome=~earunres
runVIS=true

print_usage() {
  echo "Usage:
 -d date YMD defaults to today
 -n -home name of viz defaults to ~earunres
 -p turn viz off
 long options are currently not avaible"
}

while getopts 'd:n:p:h' flag; do
  case "${flag}" in
    d) rundate="${OPTARG}" ;;
    n) vizhome="${OPTARG}" ;;
    p) runVIS="${OPTARG}" ;;
    h) print_usage
       exit 1 ;;
    *) print_usage
       exit 1 ;;
  esac
done

echo date=$rundate
echo vizhome=$vizhome
echo viz=$runVIS

runffmpeg=false
cwd=$(pwd)
FNAME=$rundate
VIZPATH=$vizhome/public_html/UNRESP_VIZ/

if [ "$runVIS" = true ]; then
  echo "### RUNNING VISUALISATION TOOLS"
  rm -rf ./vis/${rundate}
  mkdir ./vis/${rundate}
  cd Python
  ./generateMaps.py ${rundate}
  cd ..
  cd vis/${rundate}
  if [ $runffmpeg = true ]; then
    ffmpeg -f image2 -r 4 -i static_concrec0100%02d.png -vcodec mpeg4 -y -s 7680x4320 movie_${rundate}.mp4
  fi
  cd ../..
  echo " ---> FINISHED ###"
  cd vis/$FNAME
  echo "### REFORMATIING AND MOVING TO "$VIZPATH
  mogrify -format jpg *.png
  rm -f *.png
  setfacl -m other:r-x *
  chmod og+rx *
  if [ ! -e $VIZPATH$FNAME ]
  then
    mkdir $VIZPATH$FNAME
  fi
  mv *.jpg *.html $VIZPATH$FNAME
  cd $VIZPATH
  ln -sf $FNAME Today
  cd $cwd
  fi
echo "DONE"
