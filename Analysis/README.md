# Analysis Tools

## Overview

As part of developing this tool, the CALPUFF model was forced with NAM and ECMWF data, the results showed large discrepancies. An experiment was set up (IMO) to compare identical CALPUFF runs over a 1 month period with known observations, and evaluate if the one meteological dataset is inferior. An overview of the results will be hosted in the wiki.

Summary of meteological data:

### NAM

* Resolution:
 *
 *
* available to non academic: yes


### ECMWF

* Resolution
  * Spatial
  * Temporal
* available to non-academic: **no**

## Description of Analysis

1. extract the model results for the location closest to the stations (or the four closest points and then the average)
2. converting the unit to make the comparison reliable (the unit from the model al g/m3 whereas the obs are in micrograms/m3)
3. clean for the data gaps in the obs
4. plot all together

Other possible steps:

* calculate: daily/hourly/month max, min, mean (for area around station), see if it's comparable to data
* calculate other useful statistics e.g. RMS and std to see variability and error

## Acknowledgements

Sara Barsotti
