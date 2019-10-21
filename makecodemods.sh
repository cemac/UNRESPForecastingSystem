#!/bin/bash -
#title          :makecodemods.sh
#description    :Make code modifications to CALPUF SRC code
#author         :CEMAC - Helen
#date           :20190501
#version        :1.0
#usage          :./makecodemods.sh
#notes          :
#bash_version   :4.2.46(2)-release
#============================================================================


read -r -p "Do you want to implement CALPUFF code modificiation? [Y/n] " input

case $input in
    [yY][eE][sS]|[yY])
 echo "Yes"
 ;;
    [nN][oO]|[nN])
 echo "No"
       ;;
    *)
 echo "Invalid input..."
 exit 1
 ;;
esac

echo 'Linking CALPUFF_MODS to CALPUFF_SRC'

cd CALPUFF_SRC/CALMET
ln -sf ../../CALPUFF_MODS/CALMET/* .
cd ..
cd CALPUFF
ln -sf ../../CALPUFF_MODS/CALPUFF/* .
cd ..
cd CTGPROC
ln -sf ../../CALPUFF_MODS/CTGPROC/* .
cd ..
cd MAKEGEO
ln -sf ../../CALPUFF_MODS/MAKEGEO/* .
cd ..
cd TERREL
ln -sf ../../CALPUFF_MODS/TERREL/* .
cd ../..

echo 'COMPLETED:'
echo '*************************************************'
echo ' Please make future code mods in CALPUFF_MODS to keep track of changes'
echo ' Rerun makecodemods.sh to include new files after making changes'
echo '*************************************************'
echo 'Please Agree to CALPUFF License Agreements'
echo 'http://www.src.com/calpuff/calpuff_eula.htm'
