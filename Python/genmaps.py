"""genmaps
.. module:: genmaps
    :platform: Unix
    :synopis:
.. moduleauther: CEMAC (UoL)
.. description: This module was developed by CEMAC as part of the UNRESP
   Project. This Script plots CALPUFF concrec data on a map.
   :copyright: © 2019 University of Leeds.
   :license: BSD-2 Clause.
Example:
    To use::
        ./genemaps.py <date>
        <date> - Date string, format YYYYMMDD, of the current CALPUFF run.
                 Used to locate directory containing the SO2 output files
                 (with assumed naming convention 'concrec0100**.dat',
                  where '**' goes from '01' through to '48')
.. CEMAC_UNRESPForcastingSystem:
   https://github.com/cemac/UNRESPForcastingSystem
"""
from dateutil.parser import parse
import maptoolkit as mtk
# Flags
StaticMaps = True
GoogleMaps = True
# Not yet Functioning
diffMaps = False
windfield = False
Layers = False
# Standalone Functions
# READ IN COMMAND LINE ARGUMENTS
parser = argparse.ArgumentParser(description="Used to generate a series (48hrs) of static and interactive (google) maps \
         showing SO2 concentrations around the Masaya volcano, as predicted by the CALPUFF dispersion model")
parser.add_argument("date", help="Date string, format YYYYMMDD, of the current CALPUFF run. Used to locate \
                    directory containing the SO2 output files (with assumed naming convention 'concrec0100**.dat', \
                    where '**' goes from '01' through to '48'", type=str)
args = parser.parse_args()
date = args.date
mpt = mtk(date)
