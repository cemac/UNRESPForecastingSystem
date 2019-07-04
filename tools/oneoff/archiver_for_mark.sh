#!/bin/bash -
#title          :archiver_for_mark.sh
#description    :A sctript to archive everything in a cron job
#author         :CEMAC - Helen
#date           :20190703
#version        :1.0
#usage          :./archiver_for_mark.sh
#notes          :
#bash_version   :4.2.46(2)-release
#============================================================================


cd ${HOME}/CEMAC/UNRESPForecastingSystem/tools/example_archiving/
./CALPUFFarchive.sh
./NAMarchive.sh
./VIZarchive.sh
printf '%s\n %s\n %s\n %s\n' 'Hello Mark,' 'Please Check last months Unresp data has been archived' 'Cheers,' 'Cron Job'| mail -s "UNRESP Archiver" "M.G.Richardson@leeds.ac.uk"
