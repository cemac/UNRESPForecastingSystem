#!/bin/bash -
#title          :unittest.sh
#description    :test archive add
#author         :CEMAC - helen
#date           :20190508
#version        :0
#usage          :./unittest.sh
#notes          :
#bash_version   :4.2.46(2)-release
#============================================================================

day=`date '+%d'`
if [[ "$day" == 08 ]];
then
  echo "1 identified day as 08"
  if [[ "$USER"==earhbu ]];
  then
    echo '1 PASSED earhbu'
  else
    echo '1 FAILED for user lock accept'
  fi
fi

if [[ "$day" == 01 ]];
then
  echo "2 FAILED to lock for day"
  if [ $USER=='earhbu' ];
  then
    echo '2 FAIL shoulnt make it here'
  else
    echo '2 FAILED for user lock accept'
  fi
fi

if [[ "$day"==08 ]];
then
  echo "3 PASS day"
  if [[ "$USER" == 'earmgr' ]];
  then
    echo '3 FAILED for user lock deny'
  else
    echo '3 PASSED for user lock'
  fi
fi
