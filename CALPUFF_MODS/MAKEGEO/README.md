If using a 100m resolution, increase `mxnx` and `mxny` to `901` and `541` respectively
in params.geo

* In subroutine `comline` (calutils.for), comment out the call to `getcl` and
uncomment the Sun compiler block (this seems to work for both the intel
and pgi compilers).
