#!/bin/bash

cwd=$(pwd)
FNAME=$(date +%Y%m%d)
VIZPATH=$'~/public_html/UNRESP/UNRESP_VIZ/'
cd vis/$FNAME
mogrify -format jpg *.png
if [ ! -e $VIZPATH$FNAME ]
then
  mkdir $VIZPATH$FNAME
fi
mv *.jpg $VIZPATH$FNAME
cd $VIZPATH
ln -sf $VIZPATH$FNAME/*.jpg .
cd $cwd
