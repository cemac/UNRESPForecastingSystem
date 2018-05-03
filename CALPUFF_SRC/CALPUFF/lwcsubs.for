c----------------------------------------------------------------------
      subroutine rdr2daux(io,dout,dinp,mxnx,mxny,nx,ny,lcmprs,clab12)
c----------------------------------------------------------------------
c
c --- CALPUFF    Version: TNG-7.0.0    Level: 140521          RDR2DAUX
c                D. Strimaitis
c
c --- PURPOSE:  Read NX*NY words of 2-D real array
c               (possibly with compression)
c
c --- INPUTS:
c               IO - integer     - Unit number of input file
c      DINP(nx,ny) - real array  - Data array from file
c        MXNX,MXNY - integers    - Dimensions of output data array
c            NX,NY - integers    - Dimensions of input data array
c           LCMPRS - logocal     - Compression flag
c
c --- OUTPUT:
c  DOUT(mxnx,mxny) - real array  - Data array to calling routine
c           CLAB12 - character*12- Data label
c
c --- RDR2DAUX called by:  AUX1, RDAUX
c --- RDR2DAUX calls:      UNCOMPRS
c----------------------------------------------------------------------
      real dinp(nx,ny)
      real dout(mxnx,mxny)
      character*12 clab12
      character*15 clab15
      logical lcmprs

      nxy=nx*ny
      mxnxy=mxnx*mxny
      nchar=12

      if(mxnxy.LT.nxy) then
         write(*,*)'ERROR in RDR2DAUX:  actual input array will not ',
     &             'fit in output array!'
         write(*,*)'Input  array NX, NY:  ',nx,ny
         write(*,*)'Output array NX, NY:  ',mxnx,mxny
         stop

      elseif(nx.EQ.mxnx .AND. ny.EQ.mxny) then
c ---    Input and output arrays have same shape so simple read if not
c ---    compressed.   Use DINP as the work array if compressed.
         if(lcmprs) then
            read(io) n
            call UNCOMPRS(dinp,n,io,nxy,nchar,clab12,clab15,dout)
         else
            read(io) clab12,dout
         endif

      else
c ---    Input array smaller than output array, so read and then
c ---    transfer by element.  Use output array as the work array
c ---    if compressed.
         if(lcmprs) then
            read(io) n
            call UNCOMPRS(dout,n,io,nxy,nchar,clab12,clab15,dinp)
         else
            read(io) clab12,dinp
         endif

         do j=1,ny
            do i=1,nx
               dout(i,j)=dinp(i,j)
            enddo
         enddo

      endif

      return
      end

c----------------------------------------------------------------------
      subroutine rdi2daux(io,iout,iinp,mxnx,mxny,nx,ny,lcmprs,clab12)
c----------------------------------------------------------------------
c
c --- CALPUFF    Version: TNG-7.0.0    Level: 140521          RDI2DAUX
c                D. Strimaitis
c
c --- PURPOSE:  Read NX*NY words of 2-D integer array
c               (no compression allowed)
c
c --- INPUTS:
c               IO - integer     - Unit number of input file
c      IINP(nx,ny) - int. array  - Data array from file
c        MXNX,MXNY - integers    - Dimensions of output data array
c            NX,NY - integers    - Dimensions of input data array
c           LCMPRS - logocal     - Compression flag
c
c --- OUTPUT:
c  IOUT(mxnx,mxny) - int. array  - Data array to calling routine
c           CLAB12 - character*12- Data label
c
c --- RDI2DAUX called by:  AUX1, RDAUX
c --- RDI2DAUX calls:      none
c----------------------------------------------------------------------
      integer iinp(nx,ny)
      integer iout(mxnx,mxny)
      character*12 clab12
      character*15 clab15
      logical lcmprs

      nxy=nx*ny
      mxnxy=mxnx*mxny
      nchar=12

      if(lcmprs) then
         write(*,*)'ERROR in RDI2DAUX: Data compression option ',
     &             'is NOT implemented'
         stop
      endif

      if(mxnxy.LT.nxy) then
         write(*,*)'ERROR in RDI2DAUX:  actual input array will not ',
     &             'fit in output array!'
         write(*,*)'Input  array NX, NY:  ',nx,ny
         write(*,*)'Output array NX, NY:  ',mxnx,mxny
         stop

      elseif(nx.EQ.mxnx .AND. ny.EQ.mxny) then
c ---    Input and output arrays have same shape so simple read
         read(io) clab12,iout

      else
c ---    Input array smaller than output array, so read and then
c ---    transfer by element.
         read(io) clab12,iinp
         do j=1,ny
            do i=1,nx
               iout(i,j)=iinp(i,j)
            enddo
         enddo

      endif

      return
      end

c----------------------------------------------------------------------
      subroutine avgcldmr(qcz,tkz,patmz,qcup,zupbot,zuptop,zface,nzp1,
     &                    zbot,ztop,ldb,cldamr,fzcld,cldt,cldp)
c----------------------------------------------------------------------
c
c --- CALPUFF    Version: TNG-7.0.0    Level: 140521          AVGCLDMR
c                D. Strimaitis
c
c --- PURPOSE:  Obtain average LWC from vertical profile data
c               Average is "in-cloud", so only LWC>0 contributes
c               T,P "in-cloud" average is calculated the same way
c               and assumes that T,P are constant in each layer
c
c --- INPUTS:
c       QCZ(mxnz) - real array - Cloud water mixing ratio (g/kg) profile
c       TKZ(mxnz) - real array - Temperature (K) profile @ layer heights
c     PATMZ(mxnz) - real array - Pressure (atm) profile @ layer heights
c            QCUP - real       - Cloud water mixing ratio (g/kg) aloft
c         (ZUPBOT - real       - Bottom (mAGL) of cloud layers aloft)
c         (ZUPTOP - real       - Top (mAGL) of cloud layers aloft)
c                                ZUPBOT/ZUPTOP not currently used
c   ZFACE(mxnzp1) - real array - Cell face heights (m) for each layer
c            NZP1 - integer    - Number of cell face heights (NZ + 1)
c            ZBOT - real       - Bottom (mAGL) of layer to be averaged
c            ZTOP - real       - Top (mAGL) of layer to be averaged
c             LDB - logical    - Debug output flag
c
c     Parameters:
c           MXNZ, MXNZP1, IO6
c
c --- OUTPUT:
c          CLDAMR - real       - Average in-cloud liquid water mixing
c                                ratio (g/kg) for layer
c           FZCLD - real       - Fraction of interval ZTOP-ZBOT with
c                                LWC>0
c            CLDT - real       - Associated temperature (K)
c            CLDP - real       - Associated pressure (atm)
c
c --- AVGCLDMR called by:  CHEM
c --- AVGCLDMR calls:      ZFIND
c----------------------------------------------------------------------
c --- Include parameters
      include 'params.puf'

      real qcz(mxnz),tkz(mxnz)
      real zface(mxnzp1),patmz(mxnz)
      logical ldb

c --- Averaging height range
c --- Set averaging limits to model domain faces (keep orig range)
      zabot=zbot
      zatop=ztop
      zabot=MAX(zabot,zface(1))
      zabot=MIN(zabot,zface(nzp1))
      zatop=MAX(zatop,zface(1))
      zatop=MIN(zatop,zface(nzp1))

c --- Find the grid layers containing the bottom, average, and top
      zbar=0.5*(zabot+zatop)
      call ZFIND(zabot,zface,nzp1,ibot)
      call ZFIND(zbar, zface,nzp1,ibar)
      call ZFIND(zatop,zface,nzp1,itop)

c --- Initially no cloud water
      cldamr=0.0
      fzcld=0.0
      cldt=tkz(ibar)
      cldp=patmz(ibar)

      if(ibot.EQ.itop) then
c ---    Just 1 layer for average
         cldamr=qcz(ibot)
         if(cldamr.GT.0.0) fzcld=1.0
         cldt=tkz(ibot)
         cldp=patmz(ibot)

      else
c ---    2 or more layers
c ---    Get average in-cloud mixing ratio (do not average zeroes)
         sumqc=0.0
c ---    Cloud thickness
         sumdz=0.0
c ---    In-cloud T,P weighted by LWC
         sumtq=0.0
         sumpq=0.0

c ---    Partial layer from ZABOT to ZFACE(ibot+1)
         if(qcz(ibot).GT.0.0) then
            dz=zface(ibot+1)-zabot
            sumdz=sumdz+dz
            dzq=dz*qcz(ibot)
            sumqc=sumqc+dzq
            sumtq=sumtq+dzq*tkz(ibot)
            sumpq=sumpq+dzq*patmz(ibot)
         endif

c ---    Partial layer from ZFACE(itop) to ZATOP
         if(qcz(itop).GT.0.0) then
            dz=zatop-zface(itop)
            sumdz=sumdz+dz
            dzq=dz*qcz(itop)
            sumqc=sumqc+dzq
            sumtq=sumtq+dzq*tkz(itop)
            sumpq=sumpq+dzq*patmz(itop)
         endif

c ---    Remaining are full layers between IBOT+1 and ITOP-1
         i1=ibot+1
         i2=itop-1
         if(i2.GE.i1) then
            do i=i1,i2
               if(qcz(i).GT.0.0) then
                  dz=zface(i+1)-zface(i)
                  sumdz=sumdz+dz
                  dzq=dz*qcz(i)
                  sumqc=sumqc+dzq
                  sumtq=sumtq+dzq*tkz(i)
                  sumpq=sumpq+dzq*patmz(i)
               endif
            enddo
         endif

c ---    Compute average
         if(sumdz.GT.0.0) then
            cldamr=sumqc/sumdz
            cldt=sumtq/sumqc
            cldp=sumpq/sumqc
            fzcld=sumdz/(ztop-zbot)
         endif
      endif

c *** Not Active ***
cc --- Treatment for interaction with clouds at top of model domain
cc --- Averaging layer must extend into the top model layer
cc --- (use linear weight to soften transition)
      weight=0.0
c      if(qcup.GT.0.0) then
c         zlo=zface(nzp1-1)
c         if(ztop.GT.zlo) then
c            weight=(ztop-zlo)/(zface(nzp1)-zlo)
c            weight=AMIN1(weight,1.0)
c         endif
cc ---    Bottom of any cloud layer aloft must touch the model-top
c         if(zupbot.LE.zface(nzp1)) then
cc ---       Use the maximum of the average computed above and the
cc ---       weighted average of the cloud water aloft
c            cldamr=AMAX1(cldamr,weight*qcup)
c         endif
c      endif
c *** Not Active ***

      if(ldb) then
         write(io6,*) 'AVGCLDMR: zbot,ztop,cldamr = ',zbot,ztop,cldamr
         write(io6,*) '          weight,qcup      = ',weight,qcup
         write(io6,*) '          T(K),P(atm)      = ',cldt,cldp
         write(io6,*) '          zabot,zatop      = ',zabot,zatop
         write(io6,*) '          ibot,itop        = ',ibot,itop
         write(io6,*) '          sumqc,sumdz,fzcld= ',sumqc,sumdz,fzcld
      endif

      return
      end

c----------------------------------------------------------------------
      subroutine makep3d
c----------------------------------------------------------------------
c
c --- CALPUFF    Version: TNG-7.0.0    Level: 140521           MAKEP3D
c                D. Strimaitis
c
c --- PURPOSE:  Estimate the 3D pressure (atm) at the face heights
c
c --- INPUTS:
c     Common block /GRIDNEST/ variables:
c         ngrid
c     Common block /METHD/ variables:
c         nxm(mxmetdom),nym(mxmetdom),nzm,zfacem(mxnzp1)
c     Common block /METHR/ variables:
c         temp2d(mxnx,mxny,mxmetdom),rho2d(mxnx,mxny,mxmetdom),
c         tmet(mxnx,mxny,mxnz,mxmetdom)
c
c     Parameters:
c           MXNZ, MXNZP1, MXNX, MXNY, MXMETDOM
c
c --- OUTPUT:
c     Common block /METHR/ variables:
c         pmet(mxnx,mxny,mxnzp1,mxmetdom)
c
c --- MAKEP3D called by:  COMP
c --- MAKEP3D calls:      none
c----------------------------------------------------------------------
c --- Include parameters
      include 'params.puf'

      include 'gridnest.puf'
      include 'methd.puf'
      include 'methr.puf'

c --- P2=P1* EXP(-(g/R)(z2-z1)/Tv)
c --- g=9.81 m2/s2   R=287.0 J/(kg K)
c --- Tv is average virtual temperature for layer z2-z1 (use layer T)
      data gbyr/0.0341812/

c --- Loop over met domains
      do im=1,ngrid

c ---    Set surface pressure
         kz=1
         do jy=1,nym(im)
            do ix=1,nxm(im)
c ---          kg-Molar volume(m3) at ambient T,P computed from density
               vkgmol = 28.97/rho2d(ix,jy,im)
               pmet(ix,jy,kz,im)=(22.4141/vkgmol)*
     &                           (temp2d(ix,jy,im)/273.15)
            enddo
         enddo

c ---    Set pressure profiles from temperature profiles
         do kz=2,nzm+1
            kzm1=kz-1
            do jy=1,nym(im)
               do ix=1,nxm(im)
                  f=-gbyr*(zfacem(kz)-zfacem(kzm1))
                  pmet(ix,jy,kz,im)=pmet(ix,jy,kzm1,im)*
     &                              EXP(f/tmet(ix,jy,kzm1,im))
               enddo
            enddo
         enddo

      enddo

      return
      end
