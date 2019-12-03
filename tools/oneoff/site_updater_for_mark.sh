#!/bin/bash -
#title          :site_updater_for_mark.sh
#description    :Implement changes to unresp site
#author         :CEMAC - Helen
#date           :20191021
#version        :1.0
#usage          :./site_updater_for_mark.sh
#notes          :
#bash_version   :4.2.46(2)-release
#============================================================================

newcodehome='/nfs/earcemac/projects/unresp/ForecastVisualized/UNRESPForecastingSystem/VIZ_SITE_CODE/public_html/'
oldcodehome='/home/earunres/public_html'

cp ${newcodehome}/_includes/* ${oldcodehome}/_includes/
cp ${newcodehome}/css/custom4.css ${oldcodehome}/css/custom4.css
cp ${newcodehome}/css/s*.css ${oldcodehome}/css/
cp ${newcodehome}/js/anime.js ${oldcodehome}/js/
cp ${newcodehome}/UNRESP_VIZ/*html ${oldcodehome}/UNRESP_VIZ/
cp  ${newcodehome}/UNRESP_VIZ/AQSensor/*html ${oldcodehome}/UNRESP_VIZ/AQSensor/

cd $oldcodehome

chmod -R ogu+rX *
