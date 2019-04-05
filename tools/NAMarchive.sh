#!/bin/bash -
#title          :NAMarchive.sh
#description    :Archive NAM data
#author         :CEMAC - Helen
#date           :20190404
#version        :0.1-beta
#usage          :./NAMarchive.sh
#notes          :
#bash_version   :4.2.46(2)-release
#============================================================================

# Archive RAW
echo "Archiving Raw NAM data"
in=~earmgr/CEMAC/UNRESPForecastingSystem/NAM_data/raw
out="/ds/shared/Earth&Environment/Research/SEE/Research-1/UNRESP/UNRESPForecastingSystem/NAM_data/raw"
./generic_archiving.sh -i $in -o $out -b
echo "Archived Raw NAM data"

# Archive Processed
echo "Archiving Processed NAM data"
in=~earmgr/CEMAC/UNRESPForecastingSystem/NAM_data/processed
out="/ds/shared/Earth&Environment/Research/SEE/Research-1/UNRESP/UNRESPForecastingSystem/NAM_data/processed"
./generic_archiving.sh -i $in -o $out -b -n
echo "Archived Processed NAM data"
