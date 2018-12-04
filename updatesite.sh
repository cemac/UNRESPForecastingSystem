#!/bin/bash
cwd=$(pwd)
FNAME=$(date +%Y%m%d)
VIZPATH=~/public_html/UNRESP/UNRESP_VIZ/
cd vis/$FNAME
mogrify -format jpg *.png
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
