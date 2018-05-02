c-----------------------------------------------------------------------
c --- COMMON BLOCK /SNOW/                                       CTGCOMP
c-----------------------------------------------------------------------
      parameter(isfill=1,nwds=2)   ! S-Fill window in grids
      parameter(itfill=1,nwdt=3)   ! T-Fill window in days

      parameter(nvars=8)
      parameter(nmx=32767,ipmiss=32768,fmiss=-9999.)
      parameter(ndsnowb=14)
c
      character*120 fsngrid,fsgeo
      character*16 datasets,datavers,datetimes
      character*64 datamods
      character*80 comment

      character*8 cmaps,datums,timezones
      character*10 datens
      character*4 unitss

      character*12 cactions
      character*4 c4dums

      real*8 vectis(9),vectos(9)

      common/SNOW/datasets,datavers,datetimes,datamods,fsgeo
     &  ,fsnow,fsngrid,cmaps,datums,timezones,datens,unitss
     &  ,flonorg,flatorg,flonbs,flatbs,nxp,nyp
     &  ,nib,nie,njb,nje,npk,isoff,jsoff
     &  ,nxt,nyt,nxs,nys,dlon,dlat,xl,xh,yl,yh,xyoff,nijs
     &  ,cactions,vectis,vectos,feastis,fnortis,feasts,fnorths
     &  ,c4dums,ngref,ngipp,istart,ifmt
     &  ,iyrs,imns,idys,ihre,iyr,imn,idy,nhrtot,idatc
     &  ,idateold,idatenew,idatenext,idateb,idatee,ndtot
     &  ,sdpmin,hfact,msrl,hscl,msal
     &  ,nages(mxcat),ralb(mxcat)

c-----------------------------------------------------------------------
c     DEFINITIONS  [i]=integer   [r]=real   [l]=logical   [c]=character
c-----------------------------------------------------------------------
c fsngrid        Snow grid reference file name                       [c]
c datasets       Snow data set name                                  [c]
c datavers       Snow data version name                              [c]
c datetime       Snow data time                                      [c]
c cmap           Snow data map projection                            [c]
C datumsn        Snow data datum                                     [c]
C timezones      Snow data time zone                                 [c]
C datens         Snow data daten                                     [c]
C unitss         Snow data units                                     [c]

c idatebeg       Beginning date (YYYYMMDD) of varying geo.dat        [i]
c idateend       Ending date (YYYYMMDD) of varying geo.dat           [i]
c idate          Current date (YYYYMMDD) of varying geo.dat          [i]
c-----------------------------------------------------------------------
