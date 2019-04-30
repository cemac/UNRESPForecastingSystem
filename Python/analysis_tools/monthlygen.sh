#!/bin/bash -
#title          :monthlygen.sh
#description    :generated average conc files
#author         :CEMAC - Helen
#date           :20190429
#version        :1.0
#usage          :./monthlygen.sh
#notes          :
#bash_version   :4.2.46(2)-release
#============================================================================

python monthly.py --start 20180601 --end 20180630
echo "June 2018"
python monthly.py --start 20180701 --end 20180731
echo "July 2018"
python monthly.py --start 20180801 --end 20180831
echo "August 2018"
python monthly.py --start 20180901 --end 20180930
echo "Sept 2018"
python monthly.py --start 20181001 --end 20181031
echo "Oct 2018"
python monthly.py --start 20181101 --end 20181130
echo "Nov 2018"
python monthly.py --start 20181201 --end 20181231
echo "Dec 2018"
python monthly.py --start 20190101 --end 20190131
echo "Jan 2019"
python monthly.py --start 20190201 --end 20190228
echo "Feb 2019"
python monthly.py --start 20190301 --end 20190331
echo "March 2019"
