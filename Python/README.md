# Python TOOLS

* Create3DDAT.py - process the grib files
* generateMaps.py - visualisation of concrec files
    * soon to be superseded by:
    * maptoolkit.py - potting module
    * genmaps.py - plotter (uses the plotting module)
* analysis tools - useful analysis tools (under development)
  * Probability tools pmap etc. (Probability tool (initial version))

# Usage:

`maptoolkit.py` and `genmaps.py` Can be ran from python 2 and 3 and be used as stand alone or as part of the Forecasting visualisation.

default:

```bash
./genmaps.py <date>
```

Will search CALPUFF_OUT for data and produce images for googlemaps

# Google Maps

As of June 2018 Google maps requires an API Key to plot on google maps.
To use these tools to plot google maps plot you must get an API Key:
https://github.com/cemac/cemac_generic/wiki/Google-API
A file GM_API_KEY.txt must contain only the API key
