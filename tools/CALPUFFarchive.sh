#!/bin/bash -
#title          :CALPUFFarchive.sh
#description    :Archive CALPUFF data
#author         :CEMAC - Helen
#date           :20190404
#version        :0.1-beta
#usage          :./CALPUFFarchive.sh
#notes          :
#bash_version   :4.2.46(2)-release
#============================================================================
# Set current and archive location
current=~earunres/public_html/UNRESP_VIZ/
#archive="/ds/shared/Earth&Environment/Research/SEE/Research-1/UNRESP/Image_current/"
archive="/ds/shared/Earth&Environment/Research/SEE/Research-1/UNRESP/UNRESPForecastingSystem/VIZ_SITE_CODE/public_html/UNRESP_VIZ"
# Set year to archive
year=2019
# Create the year folder in the archive space
cd $archive
if [ ! -e  $year ]
then
  mkdir $year
fi
# Create a folder for each month
cd $year
for i in $(seq -f "%02g" 1 12)
  do
    mkdir m$year$i
  done
cd $current/
for d in $year*/; do
    md=${d:(-5)}
    m=${md:(0):(2)}
    folder=$archive/$year/m$year$m/$d
    if [ ! -e $folder ]
    then
      mkdir $folder
    fi
    cp -p $d* $folder
done
