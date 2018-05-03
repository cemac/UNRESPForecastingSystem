c-----------------------------------------------------------------------
      subroutine outauxhd
c-----------------------------------------------------------------------
c
c --- CALMET   Version: 6.4.2         Level: 121203            OUTAUXHD
c
c --- PURPOSE:  Write header records to CALMET AUXiliary output file
c
c --- INPUTS:
c      common block /AUXVAR/
c         naux2d, naux3d,
c         auxnam2d(mxaux), auxnam3d(mxaux),
c         auxunit2d(mxaux), auxunit3d(mxaux),
c         auxtyp2d(mxaux), auxtyp3d(mxaux),
c      common block /GEN/
c         ibyrn,ibmon,ibdyn,ibhrn,ibsecn,ieyrn,iemon,iedyn,
c         iehrn,iesecn,axtz,irlg
c      common block /GRID/
c         nx, ny, nz, dgrid, zface(mxnz+1), xorigr, yorigr
c      common block /MAP/
c         iutmzn,feast,fnorth,
c         rnlat0,relon0,xlat1,xlat2,
c         pmap,utmhem,datum,daten
c      common block /MM4HDO/
c         ioutmm5
c      common block /QA/
c         ver, level, ncommout
c      common block /OUTPT/
c         ioaux
c      Parameters: MXNX, MXNY, MXNZ, MXNZP1, MXLEV, IOAUX, IO6, IOX
c
c --- OUTPUT:
c      common block /AUXVAR/
c         naux2d, naux3d,
c         auxnam2d(mxaux), auxnam3d(mxaux),
c         auxunit2d(mxaux), auxunit3d(mxaux),
c         auxtyp2d(mxaux), auxtyp3d(mxaux),
c         lauxout
c
c --- OUTAUXHD called by:  SETUP
c --- OUTAUXHD calls:      WRTR1D, WRTR2D, WRTI2D
c-----------------------------------------------------------------------
c
c --- include parameters
      include 'params.met'

      include 'auxvar.met'
      include 'gen.met'
      include 'grid.met'
      include 'map.met'
      include 'mm4hdo.met'
      include 'qa.met'
      include 'outpt.met'

c --- Local Variables
      character*8 clabel
      character*16 dataset,dataver
      character*33 blank33
      character*64 datamod
      character*132 comment1,blank

      data idum/0/
      data blank33/'                                 '/

c --- Configure output documentation
c --- Note:  Dataset version 1.0 did NOT have compression option
c ---        Timestamp moved before CNAME+Data to be consistent
c ---        with CALPUFF compression subroutine
      data dataset/'CALMET.AUX'/, dataver/'1.1'/
      data datamod/'Generic file structure with compression'/
      data comment1/'Produced by CALMET Version: '/

c --- Set blank (132 characters)
      blank(1:33)=blank33
      blank(34:66)=blank33
      blank(67:99)=blank33
      blank(100:132)=blank33


c --- Create specific configuration placing cloud water mixing ratio
c --- in both 2D and 3D arrays, with compression
      naux2d=3

      auxnam2d(1)='CLDMRUP'
      auxunit2d(1)='G/KG'
      auxtyp2d(1)='RC4'

      auxnam2d(2)='ZCLDBOT'
      auxunit2d(2)='MAGL'
      auxtyp2d(2)='RC4'

      auxnam2d(3)='ZCLDTOP'
      auxunit2d(3)='MAGL'
      auxtyp2d(3)='RC4'

      naux3d=1
      auxnam3d(1)='CLDMR3D'
      auxunit3d(1)='G/KG'
      auxtyp3d(1)='RC4'

c --- Determine if cloud water is available
      if(ioutmm5.EQ.81 .OR. ioutmm5.EQ.82 .OR.
     &   ioutmm5.EQ.91 .OR. ioutmm5.EQ.92) then
         lauxout=.false.
         return
      else
         lauxout=.true.
      endif

c --- Construct the version-level comment string
      j=29
      do i=1,12
         if(ver(i:i).NE.' ') then
            comment1(j:j)=ver(i:i)
            j=j+1
         endif
      enddo
      j=j+1
      comment1(j:j+7)=' Level: '
      j=j+8
      do i=1,8
         if(level(i:i).NE.' ') then
            comment1(j:j)=level(i:i)
            j=j+1
         endif
      enddo

c --- Record #1 - File Declaration -- 24 words
      write(ioaux) dataset,dataver,datamod

c --- Record #2 - Number of comment lines -- 1 word
      ncom=ncommout+1
      write(ioaux) ncom

c --- Record #3 to NCOM+2 (Comment record section) -- 33 words each
      write(ioaux) comment1
c --- Go to beginning of the scratch file with the control file image
      REWIND(iox)
c --- Loop over records
      do i=1,ncommout
         comment1=blank
         read(iox,'(a132)') comment1
         write(ioaux) comment1
      enddo

c --- record #NCOM+3 - run control parameters -- 36 words
      write(ioaux) ibyrn,ibmon,ibdyn,ibhrn,ibsecn,
     1 ieyrn,iemon,iedyn,iehrn,iesecn,axtz,irlg,
     2 nx, ny, nz, dgrid, xorigr, yorigr,
     3 pmap,datum,daten,feast,fnorth,utmhem,iutmzn,
     4 rnlat0,relon0,xlat1,xlat2,naux2d,naux3d

c --- record #NCOM+4 - cell face heights (NZ + 1 words)
      nzp1=nz+1
      clabel='ZFACE'
      call wrtr1d(ioaux,zface,nzp1,clabel,idum,idum,idum,idum)

c --- record #NCOM+5 - Names, units and types of 2D variables
c ---                                (2+naux2d*5 words)
      clabel='2D_VARS'
      write(ioaux) clabel,(auxnam2d(k),k=1,naux2d),
     &                    (auxunit2d(k),k=1,naux2d),
     &                    (auxtyp2d(k),k=1,naux2d)

c --- record #NCOM+6 - Names, units and types of 3D variables
c ---                                (2+naux3d*5 words)
      clabel='3D_VARS'
      write(ioaux) clabel,(auxnam3d(k),k=1,naux3d),
     &                    (auxunit3d(k),k=1,naux3d),
     &                    (auxtyp3d(k),k=1,naux3d)

      return
      end

c-----------------------------------------------------------------------
      subroutine auxout(ndathrb,nsecb,ndathre,nsece,
     1                  nx,ny,nz,ifull,io)
c-----------------------------------------------------------------------
c
c --- CALMET   Version: 6.4.2         Level: 121203              AUXOUT
c
c --- PURPOSE:  Write auxiliary meteorological output fields
c
c --- INPUTS:
c       NDATHRB - integer     - Beginning Date and hour (YYYYJJJHH)
c                               in LST (explicit)
c         NSECB - integer     - Beginning seconds in LST (explicit)
c       NDATHRE - integer     - Ending Date and hour (YYYYJJJHH)
c                               in LST (explicit)
c         NSECE - integer     - Ending seconds in LST (explicit)
c        NX, NY - integers    - No.  X, Y grid cells
c            NZ - integer     - No. vertical layers
c         IFULL - integer     - Flag for size of arrays
c                                  0 = nx,ny < mxnx,mxny
c                                  1 = nx,ny = mxnx,mxny
c            IO - integer     - Fortran unit no. of output file
c               
c       Common block /AUXDAT/
c          QCUP(mxnx,mxny),ZUPTOP(mxnx,mxny),ZUPBOT(mxnx,mxny)
c          QC3D(mxnx,mxny,mxnz)
c       Common block /AUXVAR/
c          naux2d, naux3d,
c          auxnam2d(mxaux), auxnam3d(mxaux),
c          auxtyp2d(mxaux), auxtyp3d(mxaux)
c          lauxout
c       Parameters: MXNX, MXNY, MXNZ, MXAUX
c
c --- OUTPUT:  none
c
c --- AUXOUT called by:  COMP
c --- AUXOUT calls:      XTRACT, COMPRS, 
c-----------------------------------------------------------------------
c
c --- include parameters and commons
      include 'params.met'
      parameter(mxnxy=mxnx*mxny)

      include 'auxdat.met'
      include 'auxvar.met'

      character*12 clab12
      real xbuf1(mxnx,mxny),xbuf2(mxnx,mxny)

c --- Test for Output
      if(.NOT.lauxout) return

c --- Timestamp is written before each variable, followed by the
c --- variable name and then the data array

      nxy=nx*ny

c --- Write 2D fields
      iout=0
      do k=1,naux2d

         if(auxnam2d(k).EQ.'CLDMRUP ') then
c ---       Average in-cloud LWC above model top:  QCUP()
c ---       Expect real*4
            if(auxtyp2d(k).NE.'RC4 ' .AND. auxtyp2d(k).NE.'R_4 ') then 
               write(io6,*) 'ERROR in AUXOUT -- Invalid 2D type'
               write(io6,*) 'Variable: ',auxnam2d(k)
               write(io6,*) 'Expected: type RC4 or R_4'
               write(io6,*) 'Found   : ',auxtyp2d(k)
            endif
            iout=iout+1
c ---       Timestamp
            write(io) ndathrb,nsecb,ndathre,nsece
c ---       Data
            clab12='            '
            clab12(1:8)=auxnam2d(k)
            if(ifull.EQ.1)then
               if(auxtyp2d(k).EQ.'RC4 ')then
c ---             Compressed real*4 data records
                  call COMPRS(qcup,mxnxy,xbuf2,mxnxy,
     &                        clab12,io,io6)
               elseif(auxtyp2d(k).EQ.'R_4 ')then
c ---             Uncompressed real*4 data record
                  call WRDAT(io,clab12,qcup,nx,ny)
               endif
            else
               call XTRACT(io6,qcup,mxnx,mxny,nx,ny,xbuf1)
               if(auxtyp2d(k).EQ.'RC4 ')then
c ---             Compressed real*4 data records
                  call COMPRS(xbuf1,nxy,xbuf2,mxnxy,
     &                        clab12,io,io6)
               elseif(auxtyp2d(k).EQ.'R_4 ')then
c ---             Uncompressed real*4 data record
                  call WRDAT(io,clab12,xbuf1,nx,ny)
               endif
            endif

         elseif(auxnam2d(k).EQ.'ZCLDBOT ') then
c ---       Bottom of cloud LWC above model top:  ZUPBOT()
c ---       Expect real*4
            if(auxtyp2d(k).NE.'RC4 ' .AND. auxtyp2d(k).NE.'R_4 ') then 
               write(io6,*) 'ERROR in AUXOUT -- Invalid 2D type'
               write(io6,*) 'Variable: ',auxnam2d(k)
               write(io6,*) 'Expected: type RC4 or R_4'
               write(io6,*) 'Found   : ',auxtyp2d(k)
            endif
            iout=iout+1
c ---       Timestamp
            write(io) ndathrb,nsecb,ndathre,nsece
c ---       Data
            clab12='            '
            clab12(1:8)=auxnam2d(k)
            if(ifull.EQ.1)then
               if(auxtyp2d(k).EQ.'RC4 ')then
c ---             Compressed real*4 data records
                  call COMPRS(zupbot,mxnxy,xbuf2,mxnxy,
     &                        clab12,io,io6)
               elseif(auxtyp2d(k).EQ.'R_4 ')then
c ---             Uncompressed real*4 data record
                  call WRDAT(io,clab12,zupbot,nx,ny)
               endif
            else
               call XTRACT(io6,zupbot,mxnx,mxny,nx,ny,xbuf1)
               if(auxtyp2d(k).EQ.'RC4 ')then
c ---             Compressed real*4 data records
                  call COMPRS(xbuf1,nxy,xbuf2,mxnxy,
     &                        clab12,io,io6)
               elseif(auxtyp2d(k).EQ.'R_4 ')then
c ---             Uncompressed real*4 data record
                  call WRDAT(io,clab12,xbuf1,nx,ny)
               endif
            endif

         elseif(auxnam2d(k).EQ.'ZCLDTOP ') then
c ---       Bottom of cloud LWC above model top:  ZUPTOP()
c ---       Expect real*4
            if(auxtyp2d(k).NE.'RC4 ' .AND. auxtyp2d(k).NE.'R_4 ') then 
               write(io6,*) 'ERROR in AUXOUT -- Invalid 2D type'
               write(io6,*) 'Variable: ',auxnam2d(k)
               write(io6,*) 'Expected: type RC4 or R_4'
               write(io6,*) 'Found   : ',auxtyp2d(k)
            endif
            iout=iout+1
c ---       Timestamp
            write(io) ndathrb,nsecb,ndathre,nsece
c ---       Data
            clab12='            '
            clab12(1:8)=auxnam2d(k)
            if(ifull.EQ.1)then
               if(auxtyp2d(k).EQ.'RC4 ')then
c ---             Compressed real*4 data records
                  call COMPRS(zuptop,mxnxy,xbuf2,mxnxy,
     &                        clab12,io,io6)
               elseif(auxtyp2d(k).EQ.'R_4 ')then
c ---             Uncompressed real*4 data record
                  call WRDAT(io,clab12,zuptop,nx,ny)
               endif
            else
               call XTRACT(io6,zuptop,mxnx,mxny,nx,ny,xbuf1)
               if(auxtyp2d(k).EQ.'RC4 ')then
c ---             Compressed real*4 data records
                  call COMPRS(xbuf1,nxy,xbuf2,mxnxy,
     &                        clab12,io,io6)
               elseif(auxtyp2d(k).EQ.'R_4 ')then
c ---             Uncompressed real*4 data record
                  call WRDAT(io,clab12,xbuf1,nx,ny)
               endif
            endif
         endif

      enddo

      if(iout.NE.naux2d) then
         write(io6,*) 'ERROR in AUXOUT -- 2D variable names not found'
         write(io6,*) 'Expected: ',naux2d
         write(io6,*) 'Found   : ',iout
         stop 'Halted in AUXOUT -- see list file.'
      endif

c --- Write 3D fields
      iout=0
      do k=1,naux3d
         if(auxnam3d(k).EQ.'CLDMR3D ') then
c ---       Layered in-cloud LWC as 3D field:  QC3D()
c ---       Expect real*4
            if(auxtyp3d(k).NE.'RC4 ' .AND. auxtyp3d(k).NE.'R_4 ') then 
               write(io6,*) 'ERROR in AUXOUT -- Invalid 3D type'
               write(io6,*) 'Variable: ',auxnam3d(k)
               write(io6,*) 'Expected: type RC4 or R_4'
               write(io6,*) 'Found   : ',auxtyp3d(k)
            endif
            iout=iout+1
c ---       Timestamp
            write(io) ndathrb,nsecb,ndathre,nsece
c ---       Data
            do i=1,nz
               clab12='            '
               clab12(1:8)=auxnam3d(k)
               write(clab12(9:12),'(i4.4)')i
               if(ifull.EQ.1)then
                  if(auxtyp3d(k).EQ.'RC4 ')then
c ---                Compressed real*4 data records
                     call COMPRS(qc3d(1,1,i),mxnxy,xbuf2,mxnxy,
     &                           clab12,io,io6)
                  elseif(auxtyp3d(k).EQ.'R_4 ')then
c ---                Uncompressed real*4 data record
                     call WRDAT(io,clab12,qc3d(1,1,i),nx,ny)
                  endif
               else
                  call XTRACT(io6,qc3d(1,1,i),mxnx,mxny,nx,ny,xbuf1)
                  if(auxtyp3d(k).EQ.'RC4 ')then
c ---                Compressed real*4 data records
                     call COMPRS(xbuf1,nxy,xbuf2,mxnxy,
     &                           clab12,io,io6)
                  elseif(auxtyp3d(k).EQ.'R_4 ')then
c ---                Uncompressed real*4 data record
                     call WRDAT(io,clab12,xbuf1,nx,ny)
                  endif
               endif
           enddo
         endif
      enddo

      if(iout.NE.naux3d) then
         write(io6,*) 'ERROR in AUXOUT -- 3D variable names not found'
         write(io6,*) 'Expected: ',naux3d
         write(io6,*) 'Found   : ',iout
         stop 'Halted in AUXOUT -- see list file.'
      endif

      return
      end

c-----------------------------------------------------------------------
      subroutine xtract(iolst,datarr,nxmax,nymax,nxact,nyact,outarr)
c-----------------------------------------------------------------------
c
c --- CALMET   Version: 6.4.2         Level: 121203              XTRACT
c              J. Scire
c
c --- PURPOSE:  Extract the active portion of a 2-D array
c
c --- INPUTS:
c                 IOLST - integer - File unit for list-file
c   DATARR(nxmax,nymax) - real    - Full data array
c                 NXMAX - integer - First dimension of data array
c                 NYMAX - integer - Second dimension of data array
c                 NXACT - integer - Number of active elements of the
c                                   array (first dimension)
c                 NYACT - integer - Number of active elements of the
c                                   array (second dimension)
c
c --- OUTPUT:
c   OUTARR(nxact,nyact) - real    - Output array consisting only
c                                   of the active elements of the
c                                   full input array
c
c --- XTRACT called by:  AUXOUT
c --- XTRACT calls:      none
c-----------------------------------------------------------------------
c
      real datarr(nxmax,nymax),outarr(nxact,nyact)
c
c --- Check that values of array dimensions are reasonable
      if(nxact.le.0.or.nxact.gt.nxmax.or.
     1   nyact.le.0.or.nyact.gt.nymax)then
            write(iolst,*)'ERROR in subr. XTRACT -- Invalid values ',
     1      'of array dimensions input -- NXACT = ',nxact,' NYACT = ',
     2      nyact,' NXMAX = ',nxmax,' NYMAX = ',nymax
            write(*,*)
            stop 'Halted in XTRACT -- see list file.'
      endif
c
c --- Extract the active portion of the input data array
      do j=1,nyact
         do i=1,nxact
            outarr(i,j)=datarr(i,j)
         enddo
      enddo
c
      return
      end

c-----------------------------------------------------------------------
      subroutine comprs(xdat,nwords,xwork,nwork,clab12,io,io6)
c-----------------------------------------------------------------------
c
c --- CALMET   Version: 6.4.2         Level: 121203              COMPRS
c              J. Scire
c
c --- PURPOSE:  Compress an array reals by replacing strings of zero
c               values with a negative code indicating the number of
c               zero values
c
c --- INPUTS:
c        XDAT(nwords) - real array - Array of uncompressed data to be
c                                    output
c              NWORDS - integer    - Number of values in data array
c        XWORK(nwork) - real array - Work array to temporarily store
c                                    compressed array
c               NWORK - integer    - Dimension of work array - NWORK
c                                    must be >= NWORDS
c              CLAB12 - char*12    - Character record header
c                  IO - integer    - Unit number of output file
c                 IO6 - integer    - Unit number of output list file
c
c --- OUTPUT:  none
c
c --- COMPRS called by: AUXOUT
c --- COMPRS calls:     WRDAT
c-----------------------------------------------------------------------
c
      real xdat(nwords),xwork(nwork)
      character*12 clab12
c
c --- Check that work array is sized large enough
      if(nwork.lt.nwords)then
         write(io6,*)'ERROR in Subr. COMPRS -- Work array ',
     1   'dimension is too small -- NWORK = ',nwork,' NWORDS = ',
     2   nwords
         write(*,*)
         stop 'Halted in COMPRS -- see list file.'
      endif
c
c --- Replace all zeroes with negative coded integer
      nzero=0
      ii=0
      do 100 i=1,nwords
c
         if(xdat(i).eq.0.0)then
            nzero=nzero+1
            go to 100
         else if(xdat(i).lt.0.0)then
            write(io6,*)'ERROR in Subr. COMPRS -- Negative value ',
     1      'encountered with COMPRESS option on -- I = ',i,
     2      ' XDAT(i) = ',xdat(i)
            write(io6,*)'COMPRESS option cannot be used when data ',
     1      'values are negative'
            write(*,*)
            stop 'Halted in COMPRS -- see list file.'
         endif
c
         if(nzero.eq.0)then
            ii=ii+1
            xwork(ii)=xdat(i)
         else
            ii=ii+1
            xwork(ii)=-(float(nzero)+0.0001)
            nzero=0
            ii=ii+1
            xwork(ii)=xdat(i)
         endif
100   continue
c
      if(nzero.gt.0)then
         ii=ii+1
         xwork(ii)=-(float(nzero)+0.0001)
      endif
c
c --- Write the data records (header, compressed data record)
      write(io)ii
      call WRDAT(io,clab12,xwork,ii,1)
c
      return
      end

c-----------------------------------------------------------------------
      subroutine wrdat(iounit,cnam12,outarr,nx,ny)
c-----------------------------------------------------------------------
c
c --- CALMET   Version: 6.4.2         Level: 121203               WRDAT
c              J. Scire
c
c --- PURPOSE:  Write a gridded 2D real array
c               (one 12-character identifier and a 2-D data array)
c
c --- INPUTS:
c          IOUNIT - integer      - Fortran unit no. of output file
c          CNAM12 - character*12 - Variable name
c   OUTARR(nx,ny) - real array   - Array of data
c              NX - integer      - Number of grid points in the
c                                  X direction
c              NY - integer      - Number of grid points in the
c                                  Y direction
c
c --- OUTPUT:  none
c
c --- WRDAT called by:  AUXOUT, COMPRS
c --- WRDAT calls:      none
c-----------------------------------------------------------------------
c
      real outarr(nx,ny)
      character*12 cnam12
c
      write(iounit)cnam12,outarr
c
      return
      end

c-----------------------------------------------------------------------
      subroutine wrint(iounit,cnam12,ioutarr,nx,ny)
c-----------------------------------------------------------------------
c
c --- CALMET   Version: 6.4.2         Level: 121203               WRINT
c              J. Scire
c
c --- PURPOSE:  Write a gridded 2D integer array
c               (one 12-character identifier and a 2-D data array)
c
c --- INPUTS:
c          IOUNIT - integer      - Fortran unit no. of output file
c          CNAM12 - character*12 - Variable name
c  IOUTARI(nx,ny) - int. array   - Array of data
c              NX - integer      - Number of grid points in the
c                                  X direction
c              NY - integer      - Number of grid points in the
c                                  Y direction
c
c --- OUTPUT:  none
c
c --- WRINT called by:  AUXOUT
c --- WRINT calls:      none
c-----------------------------------------------------------------------
c
      integer ioutarr(nx,ny)
      character*12 cnam12
c
      write(iounit)cnam12,ioutarr
c
      return
      end

c-----------------------------------------------------------------------
      subroutine prfvar(iolst,ldb,nz1,nz2,array1,zface1,zface2,thresh,
     &                  array2,zupbot,zuptop,avgup)
c-----------------------------------------------------------------------
c
c --- CALMET   Version: 6.4.2         Level: 121203              PRFVAR
c              D. Strimaitis
c
c --- PURPOSE:  Process vertical profile of a real variable from one set
c               of layers to another set of layers.
c               The array variable is assumed to be constant across each
c               layer explicitly defined by a set of interface heights.
c               Values in input layers that overlap an output layer are
c               averaged.
c               Above the last output layer, a threshold screen is
c               applied to the input values and the min/max heights
c               where the threshold is exceeded are determined.  The
c               average of the variable between these heights is then
c               computed.
c
c --- INPUTS:
c          IOLIST - integer      - Fortran unit no. of list file
c             LDB - logical      - Debug output flag
c             NZ1 - integer      - Number of layers in input profile
c             NZ2 - integer      - Number of layers in output profile
c     ARRAY1(nz1) - real array   - Profile of input data
c   ZFACE1(nz1+1) - real array   - Interface heights in input (m)
c   ZFACE2(nz2+1) - real array   - Interface heights in output (m)
c          THRESH - real         - Threshold for reporting average aloft
c
c --- OUTPUT:
c     ARRAY2(nz2) - real array   - Profile of output data
c          ZUPBOT - real         - Bottom of layer aloft (m)
c          ZUPTOP - real         - Top of layer aloft (m)
c           AVGUP - real         - Average of variable aloft
c
c --- PRFVAR called by:  
c --- PRFVAR calls:      none
c-----------------------------------------------------------------------
c
      real array1(*), zface1(*)
      real array2(*), zface2(*)
      logical ldb

c --- Step through output profile layers and fill output array2

      do kout=1,nz2
         zbot=zface2(kout)
         ztop=zface2(kout+1)
         array2(kout)=0.0
         dz2=ztop-zbot

c ---    Loop over input profile layers
         do k=1,nz1
c ---       Process layers that are above local ground
            if(zface1(k+1).GT.0.0) then
               zhi=zface1(k+1)
               zlo=zface1(k)
               if(zlo.LE.0.0) zlo=0.0
c ---          Use Input layer if it overlaps output layer
               if(zhi.GT.zbot .AND. zlo.LT.ztop) then
                  hupper=AMIN1(ztop,zhi)
                  hlower=AMAX1(zbot,zlo)
                  array2(kout)=array2(kout)+
     &                         array1(k)*(hupper-hlower)/dz2
                endif
            endif
         enddo
      enddo

c --- Construct layer information aloft
      zupbot=zface1(nz1+1)
      zuptop=0.0
      avgup=0.0
      zwt=0.0
c --- Loop over layers in input profile
      do k=1,nz1
         if(zface1(k+1).GT.zface2(nz2+1) .AND.
     &      array1(k).GE.thresh) then
            deltaz=zface1(k+1)-zface1(k)
            avgup=avgup+array1(k)*deltaz
            zwt=zwt+deltaz
            zupbot=AMIN1(zupbot,zface1(k))
            zuptop=AMAX1(zuptop,zface1(k+1))
         endif
      enddo
      if(zwt.GT.0.0) avgup=avgup/zwt
c --- Restore zero markers if threshold is not exceeded (compression)
      if(avgup.LT.thresh) then
         zupbot=0.0
         zuptop=0.0
         avgup=0.0
         zwt=0.0
      endif

      if(LDB) then
         write(iolst,*)
         write(iolst,*)'PRFVAR: Configuration Data'
         write(iolst,*)'        nz1,nz2,thresh = ',nz1,nz2,thresh
         write(iolst,*)'PRFVAR: Input Profile Data'
         write(iolst,*)'    zface1(bottom) = ',zface1(1)
         do k=1,nz1
            write(iolst,*)'    zface1, array1 = ',zface1(k+1), array1(k)
         enddo
         write(iolst,*)'PRFVAR: Output Profile Data'
         write(iolst,*)'    zface2(bottom) = ',zface2(1)
         do k=1,nz2
            write(iolst,*)'    zface2, array2 = ',zface2(k+1), array2(k)
         enddo
         write(iolst,*)'PRFVAR: Layers Aloft'
         write(iolst,*)'     zbot,ztop,avg = ',zupbot,zuptop,avgup
      endif

      return
      end

c-----------------------------------------------------------------------
      subroutine z2face(iolst,ldb,nz,z,zface)
c-----------------------------------------------------------------------
c
c --- CALMET   Version: 6.4.2         Level: 121203              Z2FACE
c              D. Strimaitis
c
c --- PURPOSE:  Construct interface heights from layer heights assuming
c               interfaces lie midway between the layer heights.
c
c --- INPUTS:
c          IOLIST - integer      - Fortran unit no. of list file
c             LDB - logical      - Debug output flag
c              NZ - integer      - Number of layers
c           Z(nz) - real array   - Layer heights (m)
c
c --- OUTPUT:
c     ZFACE(nz+1) - real array   - Interface heights (m)
c
c --- Z2FACE called by:  
c --- Z2FACE calls:      none
c-----------------------------------------------------------------------
      real z(*), zface(*)

c --- First layer
      if(z(1).LE.0) then
         zface(1)=z(1)-1.0
      else
         zface(1)=0.0
      endif

c --- Loop over intermediate layer heights (AGL)
      do k=2,nz
c ---    Approximate layer faces as halfway between points
         zface(k)=0.5*(z(k-1)+z(k))
      enddo

c --- Top face
      zface(nz+1)=2.0*z(nz)-zface(nz)

      return
      end
