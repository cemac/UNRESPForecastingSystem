# Analysis Tools

:warning: This README is incomplete :warning:

These tools can be used to aid in writing further analysis tools and transparency in model configuration. They will not be available in release archives

## Overview

As part of developing this tool, the CALPUFF model was forced with NAM and ECMWF data, the results showed large discrepancies. An experiment was set up (IMO) to compare identical CALPUFF runs over a 1 month period with known observations, and evaluate if the one meteological dataset is inferior. An overview of the results will be hosted in the wiki.

Summary of meteological data:

### NAM

* Resolution:
 * Spatial -
 * Temporal -
* available to non academic: yes


### ECMWF

* Resolution:
  * Spatial -
  * Temporal -
* available to non-academic: **no**

## Description of Analysis

1. Extract Observational data
2. Fill in missing values with ML alogrithm , but keep raw incomplete data set
3. Extract model data, nearest point and surrounding points (mean, min max) accept that the model accuracy is not such that the exact station point will be well defined.
4. Coallate data sets and perform statistical analysis
5. Plot obervations vs model data

### Other possible steps:

* calculate: daily/hourly/month max, min, mean (for area around station), see if it's comparable to data
* calculate other useful statistics e.g. RMS and std to see variability and error
* Normalise the data sets to see if there's any peak correleation.

## Acknowledgements

Sara Barsotti
