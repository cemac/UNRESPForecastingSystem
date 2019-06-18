#!/bin/bash -
#title          :gribchopper.sh
#description    :Discard unwanted vars from grib files
#author         :CEMAC - Helen
#date           :20190617
#version        :1.0
#usage          :./gribchopper.sh
#notes          :
#bash_version   :4.2.46(2)-release
#============================================================================


# NAM FILES contain 712 Vars accross the whole of central america
# We want just 139 for a specific region
yearmnt=201712
location="/nfs/earcemac/projects/unresp/nam_data"
cd $location
mkdir ${yearmnt}_small
echo "belt and braces copying data before chopping"
echo "this may take a few mins"
cp -rp $yearmnt/* ${yearmnt}_small
cd ${yearmnt}_small
d=$(ls)
for line in $d ; do
  echo $line
  cd $line
  list=$(ls)
  for f in $list ; do
    echo "shrinking to region " $f
    wgrib2 $f -small_grib 272.096000:277.928000 10.073999:15.905999 small.grb
    echo "removing unwated variables"
    wgrib2 small.grb -s | egrep '(:PRMSL:|:HGT:|:TMP:|:RH:|:DZDT:|:UGRD:|:VGRD:)' | wgrib2 -i small.grb -grib tiny.grb
    echo "removing unwanted pressure levels"
    wgrib2 tiny.grb -s | egrep -e '(:1000 mb:|:100 mb:|:150 mb:|:1000 mb:|:10 mb:|:200 mb:|:20 mb:|:250 mb:|:2 mb:|:300 mb:|:30 mb:|:400 mb:|:500 mb:|:50 mb:|:5 mb:|:600 mb:|:700 mb:|:75 mb:|:7 mb:|:800 mb:|:850 mb:|:900 mb:|:925 mb:|:950 mb:|:PRMSL:)' | wgrib2 -i tiny.grb  -grib min.grb
    rm -f $f
    rm -f tiny.grb
    rm -f small.grb
    mv min.grb $f
  done
  cd ..
done
echo "complete.."
echo "check and delete"
