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
  .\generic_archiving.sh <opts>

 Options:
  -d <directory> directory
  -f <file> filename or pattern
 "
}

while getopts 'd:f:h' flag; do
  case "${flag}" in
    d) folder="${OPTARG}" ;;
    f) fpat="${OPTARG}" ;;
    h) print_usage
       exit 1 ;;
    *) print_usage
       exit 1 ;;
  esac
done


if [ ! -e $folder ]
then
  mkdir $folder
fi

mv $fpat $folder
