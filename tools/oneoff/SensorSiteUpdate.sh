#!/bin/bash
cwd=$(pwd)
cd ~earunres/public_html/UNRESP_VIZ/AQSensor
rm -f 785150_index.html
cp -p/nfs/earcemac/projects/unresp/ForecastVisualized/UNRESPForecastingSystem/VIZ_SITE_CODE/public_html/UNRESP_VIZ/AQSensor/785150_index.html .
rm -f 861150_index.html
cp -p/nfs/earcemac/projects/unresp/ForecastVisualized/UNRESPForecastingSystem/VIZ_SITE_CODE/public_html/UNRESP_VIZ/AQSensor/861150_index.html .
rm -f ElCrucero_index.html
cp -p/nfs/earcemac/projects/unresp/ForecastVisualized/UNRESPForecastingSystem/VIZ_SITE_CODE/public_html/UNRESP_VIZ/AQSensor/ElCrucero_index.html .
rm -f ElPanama_index.html
cp -p/nfs/earcemac/projects/unresp/ForecastVisualized/UNRESPForecastingSystem/VIZ_SITE_CODE/public_html/UNRESP_VIZ/AQSensor/ElPanama_index.html .
rm -f Pacaya_index.html
cp -p/nfs/earcemac/projects/unresp/ForecastVisualized/UNRESPForecastingSystem/VIZ_SITE_CODE/public_html/UNRESP_VIZ/AQSensor/Pacaya_index.html .
rm -f Rigoberto_index.html
cp -p/nfs/earcemac/projects/unresp/ForecastVisualized/UNRESPForecastingSystem/VIZ_SITE_CODE/public_html/UNRESP_VIZ/AQSensor/Rigoberto_index.html .
rm -f SanJu1_index.html
cp -p/nfs/earcemac/projects/unresp/ForecastVisualized/UNRESPForecastingSystem/VIZ_SITE_CODE/public_html/UNRESP_VIZ/AQSensor/SanJu1_index.html .
rm -f SanJuan2_index.html
cp -p/nfs/earcemac/projects/unresp/ForecastVisualized/UNRESPForecastingSystem/VIZ_SITE_CODE/public_html/UNRESP_VIZ/AQSensor/SanJuan2_index.html .
cd $cwd
