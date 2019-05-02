If using a 100m resolution, increase `mxnx` and `mxny` to `901` and `541` respectively
in params.ctg

* In params.ctg, comment out the ‘Lahey F95 Compiler’ block and
uncomment the ‘INTEL Compiler’ block.
* In control.ctg, add the missing variable `lll` to the `/CONTROL/` common
block.
