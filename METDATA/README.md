# Analysis For Comparing met data

NAM and ECMWF runs are quite different. We need to figure out why?

1. Check preprocessing
2. Check archive DATA
3. Validate against met data

<hr>

# preprocessing

preprocessing is done via `Create3DDAT.py` following Sara's method
for injesting the ECMWF data

1. check scripts
2. Comparte grib to dat file produced
3. Compare ECMWF dat file to NAM
4. Check Calmet files

# Check Archive data

the 2017 data was from a random source

1. Check grib raw and interpolated
2. Check against 2019 data

## Plot time series of met data vs NAM data

* pick a date with not archive data
