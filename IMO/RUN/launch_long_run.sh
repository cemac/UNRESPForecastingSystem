#!/bin/bash
# Updates the general_input file for the current date
# you need to enter the date in the format yyyymmdd 
# 20171212
currentdate=$1 
#currentdate=$(date +"%Y%m%d")
loopenddate=$(date -d "$currentdate + 1 days" +%Y%m%d)
echo $loopenddate
echo $end
#
until [ "$currentdate" == "$loopenddate" ]
do
#
currentdate2=$(date -d "$currentdate + 2 days" +%Y%m%d)
currentdate1=$(date -d "$currentdate + 1 days" +%Y%m%d)
year=${currentdate:0:4}
month=${currentdate:4:2}
day=${currentdate:6:2}
#
yeare=${currentdate2:0:4}
monthe=${currentdate2:4:2}
daye=${currentdate2:6:2}
#
#
yearei=${currentdate1:0:4}
monthei=${currentdate1:4:2}
dayei=${currentdate1:6:2}
#
timeforfile=$day$month$year
#
####mkdir /home/sara/LAVORO/VOLC_PROCEDURE/TEST_CASES/MASAYA/RUN_FILE/$currentdate
# MRI
IMO_ROOT=${HOME}/CEMAC/UNRESP_tech/IMO
mkdir $IMO_ROOT/RUN/$currentdate
echo $currentdate
echo $loopenddate
#!/bin/bash
cd ${IMO_ROOT}/Meteo
./extract_one_to_multiple_file.sh $timeforfile 
cd $IMO_ROOT/RUN
echo "extracted meteo files"
#
# This script allows to produce daily plot with the correlation between 
echo $timeforfile > timestmp.dat
${IMO_ROOT}/EXEC/caleta_masaya
#
#
# First we need to process the meteo data with the calmet
sed -e "s/YYYYb/$year/g" -e "s/MMb/$month/g" -e "s/DDb/$day/g" -e "s/YYYYe/$yeare/g" -e "s/MMe/$monthe/g" -e "s/DDe/$daye/g" calmet_temp.inp > calmet.inp
#
echo 'starting calmet'
${IMO_ROOT}/EXEC/calmet_sub_ifort
###~/LAVORO/VOLC_PROCEDURE/VAR/EXE/calmet_sub_ifort
#
echo 'Meteorological data have been processed'
#
# MRI artificial stop processing met data for one day
###mriexit 4
# Executes the forecasts over the 48hours 
#
                echo 'starting sed'
                sed -e "s/YYYYb/$year/g" -e "s/MMb/$month/g" -e "s/DDb/$day/g" -e "s/YYYYe/$yeare/g" -e "s/MMe/$monthe/g" -e "s/DDe/$daye/g" calpuff_temp.inp > calpuff.inp
#
                echo 'starting calpuff'
                ${IMO_ROOT}/EXEC/calpuff
#
                cp ./concrec* ./$currentdate/
#
# makes the data accessible from outside on brunnur
#
# Originally wrote to FTp site DISABLE 
###scp $IMO_ROOT/RUN/concrec* pub@brunnur.vedur.is:/srv/www/brunnur/pub/sara/calpuff_so2_masaya
rm -f $IMO_ROOT/RUN/concrec*
#
#prepare the restart over the next 24hours only
#
                echo 'starting sed'
                sed -e "s/YYYYb/$year/g" -e "s/MMb/$month/g" -e "s/DDb/$day/g" -e "s/YYYYe/$yearei/g" -e "s/MMe/$monthei/g" -e "s/DDe/$dayei/g" calpuff_temp_reinit.inp > calpuff.inp
#
                echo 'starting calpuff'
                ${IMO_ROOT}/EXEC/calpuff
#

                cp RESTART_SO2_E.DAT RESTART_SO2_B.DAT
#currentdate=/bin/date -d "$currentdate + 1 day" +%d%m%Y
currentdate=$(date -d "$currentdate + 1 day" +%Y%m%d)
echo $currentdate
# copy the model results on brunnur

done
