#!/bin/bash -
#title          :linker.sh
#description    :link grib files in correct format for processing
#author         :CEMAC - Helen
#date           :20190620
#version        :1.0
#usage          :./linker.sh
#notes          :
#bash_version   :4.2.46(2)-release
#============================================================================

month=201703
root=/nfs/earcemac/projects/unresp/nam_data/${month}_small
namarea=/nfs/earcemac/projects/unresp/ForecastVisualized/UNRESPForecastingSystem/NAM_data/raw/
cd $namarea
for d in $month*/; do
  d1=${d:(-3):(-1)}
  if [ $d1 == "31" ] ; then
    echo "end"
    exit 1
  fi
  d1=${d:(-3):(-1)}
  d1=$(( ${d1#0} ))
  d2=$(( $d1 + 1 ))
  d2="$( printf '%02d' "$d2" )"
  ln -sf  ${root}/$month$d2/nam.t00z.afwaca00.grb2.tm00 ${namarea}${d}nam.t24z.afwaca00.grb2.tm00
done
