#!/bin/bash -
#title          :processgribs.sh
#description    :proccess marhc 2017
#author         :CEMAC - helen
#date           :20190620
#version        :1.0
#usage          :./processgribs.sh
#notes          :
#bash_version   :4.2.46(2)-release
#============================================================================


month=201703
namarea=/nfs/earcemac/projects/unresp/ForecastVisualized/UNRESPForecastingSystem/NAM_data/raw/
pythonarea=/nfs/earcemac/projects/unresp/ForecastVisualized/UNRESPForecastingSystem/Python/
cd $namarea
for d in $month*/; do
  cd $pythonarea
  d1=${d:0:(-1)}
  ./nam23ddat.py $d1
done
