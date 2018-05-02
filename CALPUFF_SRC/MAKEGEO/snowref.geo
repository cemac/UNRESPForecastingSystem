c-----------------------------------------------------------------------
c --- COMMON BLOCK /SNOWREF/                                     MAKEGEO
c-----------------------------------------------------------------------
c --- Snow arrays
      parameter (mxsng=10,mxtype=10)

      integer nland(mxnx,mxny),ntype(mxtype,mxnx,mxny)
      integer ngsnow(mxtype,mxnx,mxny)
      integer idsnow(mxsng,mxtype,mxnx,mxny)
      real    rsnow(mxsng,mxtype,mxnx,mxny)
      character*524 buffl,blankl

      common/SNOWREF/nland,ntype,ngsnow,idsnow,rsnow

c     MXSNG     - maximum snow grids in one calmet grid
c     MXTYPE    - maximum landuse catagories in snow grids
c
c     NLAND     - Number of landuse type in calmet grid
c     NTYPE     - Landuse type for NLAND 
c     NGSNOW    - Number of snow grids
C     RSNOW     - Percentage of snow grid
