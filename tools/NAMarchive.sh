#!/bin/bash -
#title          :NAMarchive.sh
#description    :Archive NAM data
#author         :CEMAC - Helen
#date           :20190404
#version        :0.1-beta
#usage          :./NAMarchive.sh
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
  -d <date> YYYYMMDD
  -i <starting loaction>
  -o <output location>
 "
}

while getopts 'd:i:o:h' flag; do
  case "${flag}" in
    d) date="${OPTARG}" ;;
    i) in="${OPTARG}" ;;
    o) out="${OPTARG}" ;;
    h) print_usage
       exit 1 ;;
    *) print_usage
       exit 1 ;;
  esac
done
