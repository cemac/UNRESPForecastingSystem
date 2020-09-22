#!/bin/bash -   
#title          :compact.sh
#description    :tar up concrec files in (folder saves a large amount of space!)
#author         :CEMAC - helen
#date           :20200921
#version        :1      
#usage          :./compact.sh
#notes          :       
#bash_version   :4.2.46(2)-release
#============================================================================

# Give it a year and this script will match folders mYYYYmm e.g. m201901 and compress
# them to a unresp_concrec_mYYYYmm.tbz file
year=2019
for n in `seq 1 1 12`; do
    no=`printf "%02d" $n`
    if [ -e ${year}${no} ]; then
	echo "compressing m"${year}${no}
	tar jcvf unresp_concrec_m${year}${no}.tbz m${year}${no}
    fi
done 
