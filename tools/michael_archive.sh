#!/bin/bash -
#title          :michael_archive.sh
#description    :Archive IMO output for michael
#author         :CEMAC - Helen
#date           :20190304
#version        :0.1-beta
#usage          :./michael_archive.sh
#notes          :
#bash_version   :4.2.46(2)-release
#============================================================================
# A stand alone archiving script to extract and reformat the UNRESP data
archive="/ds/shared/Earth&Environment/Research/SEE/Research-1/UNRESP/Image_archive/"
new=/nfs/earcemac/projects/unresp_for_michael
cd $new
mkdir 2017
mkdir 2018
cd 2017
for i in $(seq -f "%02g" 1 12)
  do
    mkdir m2017$i
  done
cd ../2018
for i in $(seq -f "%02g" 1 12)
  do
    mkdir m2018$i
  done
cd $archive/2018
for d in */; do
    md=${d:(-5)}
    m=${md:(0):(2)}
    folder=$new/2018/m2018$m/20$d
    if [ ! -e $folder ]
    then
      mkdir $folder
    fi
    cp -p $d/calpuff_so2_masaya/concrec*.dat $folder
done
cd $archive/2017
for d in */; do
    md=${d:(-5)}
    m=${md:(0):(2)}
    folder=$new/2017/m2017$m/20$d
    if [ ! -e $folder ]
    then
      mkdir $folder
    fi
    cp -p $d/calpuff_so2_masaya/concrec*.dat $folder
done
