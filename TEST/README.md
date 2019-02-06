# Fixing Error #


# Error

```bash
 ---> FINISHED ###
### EXTRACTING NAM DATA INTO CALMET INPUT FILE FORMAT Traceback (most recent call last):
  File "./Create3DDAT.py", line 230, in <module>
    gidPRMSL = varNames.index("prmsl")+1  # Pressure reduced to mean sea level
ValueError: 'prmsl' is not in list

```

# Problem

NAM data not downloading

# Testing

changes

```bash
curl "http://nomads.ncep.noaa.gov/c...."
```
to



```bash
curl "https://nomads.ncep.noaa.gov/c...."
```
