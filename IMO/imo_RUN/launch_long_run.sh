#!/bin/bash
# Updates the general_input file for the current date
# you need to enter the date in the format yyyymmdd 
#currentdate=$1 
currentdate=$(date +"%Y%m%d")
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
mkdir /home/sara/LAVORO/VOLC_PROCEDURE/TEST_CASES/MASAYA/RUN_FILE/$currentdate
echo $currentdate
echo $loopenddate
#!/bin/bash
cd /home/sara/DATA/ECMWF/EC0125/
./extract_one_to_multiple_file.sh $timeforfile 
cd /home/sara/LAVORO/VOLC_PROCEDURE/TEST_CASES/MASAYA/RUN_FILE/
echo "extracted meteo files"
#
# This script allows to produce daily plot with the correlation between 
echo $timeforfile > timestmp.dat
./caleta_masaya
#
#
# First we need to process the meteo data with the calmet
sed -e "s/YYYYb/$year/g" -e "s/MMb/$month/g" -e "s/DDb/$day/g" -e "s/YYYYe/$yeare/g" -e "s/MMe/$monthe/g" -e "s/DDe/$daye/g" calmet_temp.inp > calmet.inp
#
echo 'starting calmet'
~/LAVORO/VOLC_PROCEDURE/VAR/EXE/calmet_sub_ifort
#
echo 'Meteorological data have been processed'
#
# Executes the forecasts over the 48hours 
#
                echo 'starting sed'
                sed -e "s/YYYYb/$year/g" -e "s/MMb/$month/g" -e "s/DDb/$day/g" -e "s/YYYYe/$yeare/g" -e "s/MMe/$monthe/g" -e "s/DDe/$daye/g" calpuff_temp.inp > calpuff.inp
#
                echo 'starting calpuff'
                ~/LAVORO/VOLC_PROCEDURE/VAR/EXE/calpuff
#
                cp ./concrec* ./$currentdate/
#
# makes the data accessible from outside on brunnur
#
scp /home/sara/LAVORO/VOLC_PROCEDURE/TEST_CASES/MASAYA/RUN_FILE/concrec* pub@brunnur.vedur.is:/srv/www/brunnur/pub/sara/calpuff_so2_masaya
rm -f /home/sara/LAVORO/VOLC_PROCEDURE/TEST_CASES/MASAYA/RUN_FILE/concrec*
#
#prepare the restart over the next 24hours only
#
                echo 'starting sed'
                sed -e "s/YYYYb/$year/g" -e "s/MMb/$month/g" -e "s/DDb/$day/g" -e "s/YYYYe/$yearei/g" -e "s/MMe/$monthei/g" -e "s/DDe/$dayei/g" calpuff_temp_reinit.inp > calpuff.inp
#
                echo 'starting calpuff'
                ~/LAVORO/VOLC_PROCEDURE/VAR/EXE/calpuff
#

                cp RESTART_SO2_E.DAT RESTART_SO2_B.DAT
#currentdate=/bin/date -d "$currentdate + 1 day" +%d%m%Y
currentdate=$(date -d "$currentdate + 1 day" +%Y%m%d)
echo $currentdate
# copy the model results on brunnur

done
