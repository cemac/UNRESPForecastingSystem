In terrel.for, delete the `flen` argument in the line `inquire(file=datafil(k),exist=lexist,flen=isize)`.
This is a Lahey-specific argument.

* params.trl, comment out the ‘Lahey F95 Compiler’ block and
uncomment the ‘Compaq DF Compiler’ block.
* In `setsrtm()` (terrel.for), remove ‘access=caccess’ from the open statement
(otherwise TERREL crashes when trying to read in the first
line of an SRTM3 data file with a "sequential-access I/O to unit open
for direct access" error)
