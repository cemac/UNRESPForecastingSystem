#!/bin/bash -
#title          :generic_archiving.sh
#description    :Archive tool
#author         :CEMAC - Helen
#date           :20190404
#version        :0.1-beta
#usage          :./generic_archiving.sh
#notes          :
#bash_version   :4.2.46(2)-release
#============================================================================

print_usage() {
  echo "
 generic_archiving.sh

 A CEMAC script to create folder and move to
 Usage:
  .\generic_archiving.sh -i <IN> -o <OUT> <opts>
 Defaults to take current month in IN and archive to OUT
 Write protected and metadata preserved
 Optional arguments can adjust dates for archiving

 Options:
  -i <starting loaction>
  -o <output location>
  -b sets bulk archiving
  -n set for nam processed (no daily folders) Req. for bulk setup
  -m override month for month archive e.g 201801
  -y override year for bulk archive e.g. 2018
  "
}
##
##  DEFAULTS
##
# Set current and archive location defaults
year=$(date +%Y)
# find previous month YYYYmm
m=$(date --date="$(date +%Y%m15) -1 month" +%Y%m)
# Bulk archiving for year
bulk=false
set_bulk() {
  bulk=true
}
# A flag for NAM processed (files not folders)
nam=false
set_nam() {
  nam=true
}
# GET ARGUMENTS
while getopts 'i:o:y:m:bnh' flag; do
  case "${flag}" in
    i) in="${OPTARG}" ;;
    o) out="${OPTARG}" ;;
    y) year="${OPTARG}" ;;
    m) m="${OPTARG}" ;;
    n) set_nam ;;
    b) set_bulk ;;
    h) print_usage
      exit 1 ;;
    *) print_usage
      exit 1 ;;
  esac
done
#
# IN and OUT are required
#
if [ "x" == "x$in" ]; then
  echo "-i [input location] is required"
  exit 1
fi
if [ "x" == "x$out" ]; then
  echo "-o [output location] is required"
  exit 1
fi
#
# DEFAULT Monthly Archiving
#
if [ "$bulk" = false ]; then
  # Extract year
  year=${m:(0):(-2)}
  # Extract month
  m=${m:(-2)}
  echo "Archiving month: " $m " Year: " $year
  # Check and create the year folder in the archive space
  cd $out
  if [ ! -e  $year ]
  then
    echo $year "does not exist, creating folder"
    mkdir $year
  fi
  cd $year
  # Check and create the year month folders in the archive space
  if [ ! -e  m$year$m ]
  then
    echo $year$m "does not exist, creating folder"
    mkdir m$year$m
  fi
  cd $in/
  for d in *$year$m*; do
    # output folder full path
    folder=$out/$year/m$year$m/
    # Don't overwrite files
    if [ ! -e $folder/$d ];
    then
      # If the days file or folder isn't there already put it there
      rsync -a $d $folder/$d
    fi
  done
  echo "data copied to " $out
  echo "write protecting chmod -R ogu-w " $folder
  chmod -R ogu-w $folder
  echo "run checks and delete duplicates from " $in
fi

if [ "$bulk" = true ]; then
  echo "Archiving year:" $year
  # Check and create the year folder in the archive space
  cd $out
  if [ ! -e $year ]
  then
    echo $year "does not exist, creating folder"
    mkdir $year
  fi
  cd $in/
  for d in *$year*; do
    md=${d:(-5)}
    m=${md:(1):(2)}
    if [ "$nam" = true ]; then
      m=${d:(8):(2)}
    fi
    folder=$out/$year/m$year$m
    # Check and creating monthly folders in year folder
    if [ ! -e $folder ]
    then
      mkdir $folder
    fi
    # If the days file or folder isn't there already put it there
    if [ ! -e $folder/$d ];
    then
      rsync -a $d $folder
    fi
  done
  echo "data copied to " $out
  echo "run checks and delete duplicates from " $in
  echo "*Consider* using chmod -R ogu-w " $out/$year/m$year"*"
  echo "To write protect completed archives"
fi
