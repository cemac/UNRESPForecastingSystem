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

# Archive CALPUFF_OUT
echo "Archiving CALPUFF OUTPUT"
in=~earmgr/CEMAC/UNRESPForecastingSystem/CALPUFF_OUT/CALPUFF/
out="/ds/shared/Earth&Environment/Research/SEE/Research-1/UNRESP/UNRESPForecastingSystem/CALPUFF_OUT/CALPUFF/"
./generic_archiving.sh -i $in -o $out
echo "Archived CALPUFF OUTPUT"

# Archive CALMET
echo "Archiving CALMET OUTPUT"
in=~earmgr/CEMAC/UNRESPForecastingSystem/CALPUFF_OUT/CALMET/
out="/ds/shared/Earth&Environment/Research/SEE/Research-1/UNRESP/UNRESPForecastingSystem/CALPUFF_OUT/CALMET/"
./generic_archiving.sh -i $in -o $out
echo "Archived CALMET OUTPUT"
