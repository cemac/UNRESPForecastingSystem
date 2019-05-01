#!/bin/bash -
#title          :installcalpuff.sh
#description    :This scripts installs CALPUFF system version 7 in the appropriate location for the CALPUFF MODEL
#author         :CEMAC - Helen
#date           :20190501
#version        :1.0
#usage          :./installcalpuff.sh
#notes          :
#bash_version   :4.2.46(2)-release
#============================================================================


echo 'Running installcalpuff.sh'
cd CALPUFF_SRC
echo 'Downloading and unzipping CALMET'
wget www.src.com/calpuff/download/Mod7_Files/CALMET_v6.5.0_L150223.zip
mv CALMET_v6.5.0_L150223.zip CALMET.zip
unzip CALMET.zip
rm -f CALMET.zip CALMET_v6.5.0_L150223.zip
mv CALMET_v6.5.0_L150223 CALMET
cd CALMET
for i in *; do mv $i `echo $i | tr [:upper:] [:lower:]`; done
cd ..
echo 'Downloading and unzipping CALPUFF'
wget http://www.src.com/calpuff/download/Mod7_Files/CALPUFF_v7.2.1_L150618.zip
unzip CALPUFF_v7.2.1_L150618.zip
mv CALPUFF_v7.2.1_L150618 CALPUFF
rm -f CALPUFF_v7.2.1_L150618.zip
cd CALPUFF
for i in *; do mv $i `echo $i | tr [:upper:] [:lower:]`; done
cd ..
echo 'Downloading and unzipping CTGPROC'
wget http://www.src.com/calpuff/download/Mod7_Files/CTGPROC_v7.0.0_L150211.zip
unzip CTGPROC_v7.0.0_L150211.zip
mv CTGPROC_v7.0.0_L150211 CTGPROC
rm -f CTGPROC_v7.0.0_L150211.zip
cd CTGPROC
for i in *; do mv $i `echo $i | tr [:upper:] [:lower:]`; done
cd ..
echo 'Downloading and unzipping MAKEGEO'
wget http://www.src.com/calpuff/download/Mod7_Files/MAKEGEO_V3.2_L110401.zip
unzip MAKEGEO_V3.2_L110401.zip
mv MAKEGEO_V3.2_L110401 MAKEGEO
rm -f MAKEGEO_V3.2_L110401.zip
cd MAKEGEO
for i in *; do mv $i `echo $i | tr [:upper:] [:lower:]`; done
cd ..
echo 'Downloading and unzipping TERREL'
wget http://www.src.com/calpuff/download/Mod7_Files/TERREL_v7.0.0_L141010.zip
unzip TERREL_v7.0.0_L141010.zip
mv TERREL_v7.0.0_L141010 TERREL
rm -f TERREL_v7.0.0_L141010.zip
cd TERREL
for i in *; do mv $i `echo $i | tr [:upper:] [:lower:]`; done
cd ..
cd ..
echo 'complete - ready to compile executables'

./makecodemods.sh
