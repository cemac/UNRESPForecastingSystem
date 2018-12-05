#!/bin/bash
rundate=$(date +%Y%m%d)
# If the vis hasn't been done yet
runVIS=false
if [ "$runVIS" = true ]; then
  echo "### RUNNING VISUALISATION TOOLS"
  rm -rf ./vis/${rundate}
  mkdir ./vis/${rundate}
  cd Python
  ./generateMaps.py ${rundate}
  cd ..
  cd vis/${rundate}
  ffmpeg -f image2 -r 4 -i static_concrec0100%02d.png -vcodec mpeg4 -y -s 7680x4320 movie_${rundate}.mp4
  cd ../..
  echo " ---> FINISHED ###"
fi
cwd=$(pwd)
FNAME=$(date +%Y%m%d)
VIZPATH=~/public_html/UNRESP/UNRESP_VIZ/
cd vis/$FNAME
# mogrify -format jpg *.png
setfacl -m other:r-x *
chmod og+rx *
if [ ! -e $VIZPATH$FNAME ]
then
  mkdir $VIZPATH$FNAME
fi
mv *.jpg *.html $VIZPATH$FNAME
cd $VIZPATH
ln -sf $VIZPATH$FNAME/*.jpg .
ln -sf $VIZPATH$FNAME/*.html .
cd $cwd
