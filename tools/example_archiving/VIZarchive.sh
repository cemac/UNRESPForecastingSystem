#!/bin/bash -
#title          :generic_archiving.sh
#description    :Archive tool
#author         :CEMAC - Helen
#date           :20190404
#version        :0.1-beta
#usage          :./generic_archiving.sh
#notes          :
#bash_version   :4.2.46(2)-release
#============================================================================

# Archive RAW
echo "Archiving Images"
in=~earunres/public_html/UNRESP_VIZ/
out="/ds/shared/Earth&Environment/Research/SEE/Research-1/UNRESP/UNRESPForecastingSystem/VIZ_SITE_CODE/public_html/UNRESP_VIZ/"
./generic_archiving.sh -i $in -o $out
echo "Archived Images"
