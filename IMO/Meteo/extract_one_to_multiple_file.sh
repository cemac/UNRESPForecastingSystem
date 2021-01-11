#!/bin/bash
#
#this script helps in creating multiple files each three hours from the single file produced by MARS archive
# it needs to read the date if it is working not on the current date files
# in case it will be run ./extract_one_to_multiple_file.sh ddmmyyyy ddmmyyyy
#
currentdate=$1

# create file name
filname00Z=VOLC_MAS00$currentdate.grib
filname12Z=VOLC_MAS12$currentdate.grib
#
# to check if it is a 00Z or 12Z run
#
if [ -f $filname00Z ];
then
filnamest3=VOLC_MAS03$currentdate
filnamest6=VOLC_MAS06$currentdate
filnamest9=VOLC_MAS09$currentdate
filnamest12=VOLC_MAS12$currentdate
filnamest15=VOLC_MAS15$currentdate
filnamest18=VOLC_MAS18$currentdate
filnamest21=VOLC_MAS21$currentdate
filnamest24=VOLC_MAS24$currentdate
filnamest27=VOLC_MAS27$currentdate
filnamest30=VOLC_MAS30$currentdate
filnamest33=VOLC_MAS33$currentdate
filnamest36=VOLC_MAS36$currentdate
filnamest39=VOLC_MAS39$currentdate
filnamest42=VOLC_MAS42$currentdate
filnamest45=VOLC_MAS45$currentdate
filnamest48=VOLC_MAS48$currentdate
#filnamest51=VOLC_MAS51$currentdate
#filnamest54=VOLC_MAS54$currentdate
#filnamest57=VOLC_MAS57$currentdate
#filnamest60=VOLC_MAS60$currentdate
#filnamest63=VOLC_MAS63$currentdate
#filnamest66=VOLC_MAS66$currentdate
#filnamest69=VOLC_MAS69$currentdate
#filnamest72=VOLC_MAS72$currentdate
filname=$filname00Z
echo "File meteo 00Z  exists"
fi
#if [ -f $filname12Z ];
#then
#filnamest3=VOLC_MAS15$currentdate
#filnamest6=VOLC_MAS18$currentdate
#filnamest9=VOLC_MAS21$currentdate
#filnamest12=VOLC_MAS24$currentdate
#filnamest15=VOLC_MAS27$currentdate
#filnamest18=VOLC_MAS30$currentdate
#filnamest21=VOLC_MAS33$currentdate
#filnamest24=VOLC_MAS36$currentdate
#filnamest27=VOLC_MAS39$currentdate
#filnamest30=VOLC_MAS42$currentdate
#filnamest33=VOLC_MAS45$currentdate
#filnamest36=VOLC_MAS48$currentdate
#filnamest39=VOLC_MAS51$currentdate
#filnamest42=VOLC_MAS54$currentdate
#filnamest45=VOLC_MAS57$currentdate
#filnamest48=VOLC_MAS60$currentdate
#filnamest51=VOLC_MAS63$currentdate
#filnamest54=VOLC_MAS66$currentdate
#filnamest57=VOLC_MAS69$currentdate
#filnamest60=VOLC_MAS72$currentdate
#filnamest63=VOLC_MAS75$currentdate
#filnamest66=VOLC_MAS78$currentdate
#filnamest69=VOLC_MAS81$currentdate
#filnamest72=VOLC_MAS84$currentdate
#filname=$filname12Z
#echo "File meteo 12Z  exists"
#fi
#filnamest0=VOLC_MAS00b$currentdate
#
#grib_copy -w step=0 $filname.grib $filnamest0.grib
echo $filname
grib_copy -w step=3 $filname $filnamest3.grib
grib_copy -w step=6 $filname $filnamest6.grib
grib_copy -w step=9 $filname $filnamest9.grib
grib_copy -w step=12 $filname $filnamest12.grib
grib_copy -w step=15 $filname $filnamest15.grib
grib_copy -w step=18 $filname $filnamest18.grib
grib_copy -w step=21 $filname $filnamest21.grib
grib_copy -w step=24 $filname $filnamest24.grib
grib_copy -w step=27 $filname $filnamest27.grib
grib_copy -w step=30 $filname $filnamest30.grib
grib_copy -w step=33 $filname $filnamest33.grib
grib_copy -w step=36 $filname $filnamest36.grib
grib_copy -w step=39 $filname $filnamest39.grib
grib_copy -w step=42 $filname $filnamest42.grib
grib_copy -w step=45 $filname $filnamest45.grib
grib_copy -w step=48 $filname $filnamest48.grib
#/usr/local/bin/grib_copy -w step=51 $filname $filnamest51.grib
#/usr/local/bin/grib_copy -w step=54 $filname $filnamest54.grib
#/usr/local/bin/grib_copy -w step=57 $filname $filnamest57.grib
#/usr/local/bin/grib_copy -w step=60 $filname $filnamest60.grib
#/usr/local/bin/grib_copy -w step=63 $filname $filnamest63.grib
#/usr/local/bin/grib_copy -w step=66 $filname $filnamest66.grib
#/usr/local/bin/grib_copy -w step=69 $filname $filnamest69.grib
#/usr/local/bin/grib_copy -w step=72 $filname $filnamest72.grib
#
#mv VOLC_MAS00$currentdate.grib VOLC_MAS$currentdate.grib
#mv VOLC_MAS00b$currentdate.grib VOLC_MAS00$currentdate.grib
#
