* If using a 100m resolution, increase `mxnx` and `mxny` to `901` and `541` respectively
in params.puf
* In params.puf, change `mxnz` from `12` to `27` and `mxrec` from `10000` to `20000`.
* In calpuff.for, near the end of subroutine `comp` (e.g. just after the call
to FOGOUT), insert the following lines of code to write out the hourly
concentrations to individual file:
```fortran
do ispec=1,nspec
write ( infile ,10000 ) ispec , nn
10000 format (’concrec’ ,I2.2,I4.4, ’.dat’)
open (50,file=infile)
do i=1,nrec
write (50, *) chirec(i,ispec)
end do
end do
close (50)
```
