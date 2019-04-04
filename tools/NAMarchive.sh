<<<<<<< HEAD
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
=======
#! /bin/bash

# CEMAC SCRIPT to archive name date
>>>>>>> eee31c4436db1b8c93e0247c92d141aa2d1738e5

print_usage() {
  echo "
 generic_archiving.sh

 A CEMAC script to create folder and move to
 Usage:
  .\generic_archiving.sh <opts>

 Options:
  -d <date> YYYYMMDD
  -i <starting loaction>
<<<<<<< HEAD
  -o <output location>
=======
  -o <output location>   
>>>>>>> eee31c4436db1b8c93e0247c92d141aa2d1738e5
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
