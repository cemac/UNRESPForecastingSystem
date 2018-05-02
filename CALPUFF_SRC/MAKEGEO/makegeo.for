c----------------------------------------------------------------------
c --- MAKEGEO -- Geophysical Data Preprocessor
c----------------------------------------------------------------------
c
c --- MAKEGEO   Version: 3.2      Level: 110401                  MAIN
c
c     Copyright (c) 1996-2011 by Exponent, Inc.
c
c --- Written by:  J. Scire
c --- With changes by: Y. Zhuang, E. Insley, E. Chang and M. Fernau
c
c --- PURPOSE:
c      Takes gridded fractional land use information and
c      associated micrometeorological information and creates
c      a CALMET GEO.DAT file by 1) calculating dominant land
c      use category, 2) calculating weighted micrometeorological
c      parameters, and 3) remapping to new categories if desired.
c      Terrain information must be derived separately and is either
c      read from a TERREL output file, or inserted manually into the
c      GEO.DAT.  Will also make a UAM "TERRAIN" file if requested.
c
c --- Updates
c     Ver 3.2, Level 110401 from Ver 3.12, Level 110225 (DGS)
c              - LU.DAT Dataset Version 2.1 with additional 
c                header record containing the land use codes
c              - Demand that the input categories specified in the
c                control file match those read from the file
c                Modified: LUCAT.GEO
c                          READHD
c
c     Ver 3.12, Level 110225 from Ver 3.11, Level 100301 (DGS)
c            Updated CALUTILS.FOR to version v2.58 (110225)
c            - Add control file variable type 5 (character array) and
c              retain commas in string returned so that array of
c              variable values can be parsed
c              Modified: READIN, ALTONU, SETVAR
c     Ver 3.11, Level 100301 from Ver 3.1, Level 090714 (DGS)
c              - Revise control file processing to accept both new
c                and old formats (add control file version record)
c              - Revise QA checks on snow inputs
c                Modified: READCF, SETUP
c
c     Ver 3.1, Level 090714 from Ver 3.0, Level 090526 (minor)
c              - Fix a bug for albedo weighting for no-snow land use types
c                
c                Zhong-Xiang Wu
c                7/14/2009

c     Ver 3.0, Level 090526 from Ver 2.292, Level 080511 (Major)
c              - Add snow data processing:
c                New     : RDSNHD,RDSN,GETHOURS,WTSW,WTSWZ0,WTSWALB,
c                          ROLLBK,GETDATE,TIMESTMP,CHGTIM,FILLT,FILLS
c                Modified: SETUP,COMP,READCF
c                
c                Zhong-Xiang Wu
c                5/26/2009
c
c     Ver 2.292, Level 090511 from Ver 2.291, Level 080407
c              - CALUTILS from v2.56 Level 080407 to v2.571 Level 090511
c                Increase control file line length to 200 characters
c                Activate CPU clock using F95 system routine
c                Add routine to reformat a date string
c                New     : FMT_DATE
c                Modified: PARAMS.CAL, READIN, DATETM
c              - Reformat date reported to list file
c                Modified: FIN
c
c     Ver 2.291, Level 080407 from Ver 2.29, Level 070327 
c              - CALUTILS from v2.55 Level 070327 to v2.56 Level 080407
c                Control file entries in exponential notation were not
c                correct if decimal point was missing (2e-02 was read
c                as 0.2e-02).
c                Modified: ALTONU
c
c     Ver 2.29, Level 070327 from Ver 2.28, Level 060519 (DGS) 
c              - CALUTILS from v2.52 Level 060519 to v2.55 Level 070327
c                Move GLOBE1 to COORDLIB
c                Allow negative increments in INCRS
c                Fixed format bug in subroutine BASRUTC for the case of
c                time zone zero (output string was 'UTC+0  0' instead
c                of 'UTC+0000'
c                Modified:  INCRS, UTCBASR, BASRUTC
c                Removed:   GLOBE1
c
c     Ver 2.28, Level 060519 from Ver 2.27, Level 060309 (DGS) 
c              - CALUTILS from v2.51 Level 051019 to v2.52 Level 060519
c                Variable names in control file are not processed
c                correctly if there are too many characters (including
c                blanks) to the left of the "=" sign (run stops in
c                setup phase).
c                Modified:  READIN
c
c     Ver 2.27, Level 060309 from Ver 2.26, Level 041230 (DGS) 
c              - Updated to CALUTILS V2.51 (051019) from V2.2 (0030528)
c              - Filnames changed from c*70 to c*132 (for CALUTILS V2.3
c                and later)
c                Modified:  FILNAM.GEO
c                           READCF, SETUP, READHD, XTRCTX
c
c     Ver 2.26, Level 041230 from Ver 2.25, Level 041013 (DGS) 
c              - Modify treatment of cells with incomplete land use
c                (cells with a fractional LU total of less than 1.0) so
c                that the replacement LU category for missing (IMISS) is
c                used only if the total LU fraction is less than 0.001.
c                Allow non-missing cells to have a total LU fraction as
c                small as FLUMIN.  Set FLUMIN in BlockData and add
c                FLUMIN to input group 4a.
c                Note that fractional LU for all cells are renormalized
c                in WT.
c                Modified: CONTROL.GEO, BLOCKDATA, READCF, SETUP, COMP
c              - Add the LU2.DAT fractional land use file.  This is used
c                only for empty cells in the primary LU.DAT file (no
c                fractional land use data in LU.DAT).
c                Modified: PARAMS.GEO, FILNAM.GEO, CONTROL.GEO,
c                          BLOCKDATA, READCF, SETUP, COMP
c              - Add filename to READHD argument list so that subroutine
c                can be used for either LU.DAT or LU2.DAT files.
c                Modified: READHD
c              - Modify the LUSE.CLR file to change wetland color.
c              - Allow the land use assigned to IMISS to be invalid (not
c                an input or output LU category).  This will provoke the
c                following actions:
c
c                1.  NO GEO.DAT file is created, and if the file exists
c                    from an earlier application, it is deleted.
c                2.  QALUSE.GRD and QATERR.GRD are created for display.
c                3.  Warnings are written to the screen and list file.
c
c                This feature should be used only to identify cells with
c                no land use.
c                Modified: CONTROL.GEO, SETUP, READCF, COMP
c
c     Ver 2.25, Level 041013 from Ver 2.24, Level 040920 (DGS) 
c              - Add GRD format control as a hidden option in the
c                control file.  This applies to GRD files that are
c                plotted as image maps (not contours) -- Landuse and
c                PGT.  SURFER 7 required a range adjustment to properly
c                register the grid cell blocks that make up the image.
c                SURFER 8 registers cells properly without the
c                adjustment, so the standard GRD works for both image
c                and contour maps.  The default for the control is the
c                SURFER 8 convention in which all GRD files are alike.
c                (We presume that the SURFER 7 format will seldom be
c                needed.)
c                Modified: CONTROL.GEO, BLOCKDATA, READCF, COMP
c
c     Ver 2.24, Level 040920 from Ver 2.23, Level 030905 (DGS) 
c              - Enlarge format for QATERR.DAT plot-file to allow
c                terrain elevations below sea level.
c
c     Ver 2.23, Level 030905 from Ver 2.22, Level 030709 (DGS) 
c              - Change default DATUM code
c
c     Ver 2.22, Level 030709 from Ver 2.21, Level 030528 (DGS) 
c              - Fix type assigned to LCFILES in READCF
c
c     Ver 2.21, Level 030528 from Ver 2.2 Level 030402 
c                J. Scire, D. Strimaitis
c              - CALUTILS (Version: 2.2, Level: 030528)
c
c     Ver 2.2, Level 030402 from Ver 2.1 Level 020828   D. Strimaitis          
c              - Rename Surfer 'CLR' file to LUSE.CLR
c              - Add terrain GRD file output (tegrd=QATERR.GRD)
c              - Default land use GRD file renamed (lugrd=QALUSE.DAT)
c              - Updated CALUTILS (Version 2.1, Level 030402)
c              - Add TYPE argument to XTRACTLL
c              - New header for input LU.DAT and TERR.DAT files
c              - New header for output GEO.DAT file
c
c     Ver 2.1 Level 020828 from Ver 2.0, Level 011003   D. Strimaitis
c              - Updated CALUTILS (Version 1.1, Level 020828)
c
c     Ver 2.0, Level 011003 from Ver 1.1, Level 010206   D. Strimaitis          
c              - Restructure inputs for CALPUFF system control file
c              - Restructure main program as subroutine COMP
c              - Place system-wide utilities into an include module
c                (calutil.for)
c              - Change UAM option text output to a character string,
c                and also remove option from control file
c                (UAM file option deprecated)
c
c --- Version: 1.1      Level: 010206
c     1) Added GRD output file and Surfer 'CLR' file for making LU
c        plot (DGS)
c     2) Implement new CTGPROC output file format (i.e., with three
c        header records containing dataset name and coordinate/
c        projection data) (JSS)
c     3) Miscellaneous clean up of code (JSS)
c         - add version and level number definition to main program
c           (remove from parameter file)
c         - fix read(3... statement (to write(io3...))
c         - use standard convention for parameter names (MXNX, MXNY,
c           MXCAT, MXOCAT, file unit numbers)
c         - add new routine (XTRCTX) to extract text (filename) from
c           a character string
c         - reformat output to list file
c     4) Introduce map projection flag defining coordinate system used
c     5) Restructure input file separating optional UAM output options
c
c --- Version: 1.0       Level: 990130           DGS
c     1) Updated OUT to allow subgrid to be printed, and use (I3)
c        format for cell index
c     2) Report grid of LU values to list file with 2 digits, and
c        change percent labels to fraction for coverage output
c     3) Report landuse/surface parameters as table in list file
c
c --- Version: 1.0       Level: 961031           EMI
c     1) Reaaranged input file structure -- added option to create
c        UAM terrain file, if N then the user doesn't need to include
c        the input data lines associated with that.
c
c --- Version: 1.0       Level: 960622           EMI
c     1) Changed code to use include files for paramters
c     2) Allows input of fractional coverage used to define a cell as
c        being water  (CFRACT)
c     3) Added option to read in a file of terrain elevation data
c        if available to make up a complete geo.dat file.
c     4) If a file of terrain elevation data is read in, HTFAC is also
c        read in and then passed to GEO.DAT file (it used to always use
c        1.0)
c----------------------------------------------------------------------
      program makegeo
c
c --- Include parameters
      include 'params.geo'
c --- Include common blocks
      include 'qa.geo'
c
c --- Set version and level number of program (stored in /QA/ and
c --- checked against values set in PARAMS.GEO)
      ver='3.2'
      level='110401'
c
c --- SETUP PHASE -- read control file information
      call SETUP
c
c --- COMPUTATIONAL PHASE -- process data files
      call COMP
c
c --- TERMINATION PHASE -- program termination functions
      call FIN
c
      stop
      end
c----------------------------------------------------------------------
      BLOCK DATA
c----------------------------------------------------------------------
c
c --- MAKEGEO   Version: 3.2          Level: 090526        BLOCK DATA
c               D. Strimaitis
c
c----------------------------------------------------------------------
c
c --- Include parameter statements
      include 'params.geo'
c
c --- Include common blocks
      include 'filnam.geo'
      include 'control.geo'
      include 'grid.geo'
      include 'lucat.geo'
      include 'snow.geo'

c --- FILNAM common block
      data runinp/'makegeo.inp'/,runlst/'makegeo.lst'/,
     1     terrdat/'terr.dat'/,ludat/'lu.dat'/,lu2dat/'lu2.dat'/,
     2     geodat/'geo.dat'/,lugrd/'qaluse.grd'/,tegrd/'qaterr.grd'/,
     3     snowdat/'snow.dat'/

c --- FILLOG common block
      data lcfiles/.true./

c --- CONTROL common block
c --- Primary variables
      data lterr/.true./, lqacell/.false./,lsnow/.false./
      data iflip/1/,nlx/0/,nly/0/,ifmtgeo/1/
      data htfac/1.0/
      data image/0/
      data flumin/0.96/

c --- Derived variables
      data lutm/.false./, llcc/.false./, lps/.false./
      data lem/.false./, llaza/.false./, lttm/.false./

c --- GRID common block
      data pmap/'UTM     '/
      data utmhem/'N   '/, datum/'WGS-84'/
      data reflat/-999./,reflon/-999./,xlat1/-999./,xlat2/-999./
      data feast/0.0/, fnorth/0.0/

c --- LUCAT common block
      data noutcat/14/, iwat1/50/, iwat2/55/
      data nincat/38/, numwat/5/, nsplit/0/, imiss/55/
      data cfract/0.5/

C --- Snow data common bloc
      data flonorg,flatorg,nxt,nyt,dlon,dlat/-124.729584, 52.8704185
     &       ,6935, 3351, 8.33333377E-03, -8.33333377E-03/
      data cmaps,datums,timezones,datens,unitss,datetimes/
     &       'LL      ','WGS-84  ','02-21-2003','DEG '
     &      ,'UTC+0000','GREGORIAN_YMDHS '/
      data ifmt/1/
      data HSCL,SDPMIN/10.,0.01/
      data MSRL,MSAL/1,1/

      end

c----------------------------------------------------------------------
c --- BRING IN CALPUFF SYSTEM UTILITY SUBROUTINES
      include 'calutils.for'
c----------------------------------------------------------------------

c-----------------------------------------------------------------------
      subroutine setup
c-----------------------------------------------------------------------
c
c --- MAKEGEO   Version: 3.2          Level: 100301             SETUP
c               D. Strimaitis
c
c PURPOSE:     SETUP calls routines to read and check the control data
c              provided, to set logicals, and it reports the control
c              data, and opens the data files if inputs are valid.
c
c --- Updates
c     Ver 3.11, Level 100301 from Ver 3.0, Level 090526      DGS
c              - Edit list-file output
c     Ver 3.0, Level 090526 from Ver 2.27, Level 060309      ZWU
c              - Add snow data processing variables
c     Ver 2.27 Level 060309 from Ver 2.26 Level 041230       DGS
c              Filenames from c*70 to c*132 for CALUTILS V2.3 and later
c     Ver 2.26 Level 041230 from Ver 2.2  Level 030402       DGS
c              Add list file report of FLUMIN value
c              Report LU2.DAT file variables
c              Modify IMISS documentation for list file
c     Ver 2.2  Level 030402 from Ver 2.1  Level 011003       DGS
c              New COORDS/DATUM, and QA landuse/terrain output files
c              New header records in landuse/terrain input files
c              New header records in GEO.DAT ouitput file
c              IFLIP and HTFAC removed from iputs
c
c ARGUMENTS:
c    PASSED:  none
c
c  RETURNED:  none
c
c CALLING ROUTINES:   MAIN
c
c EXTERNAL ROUTINES:  DATETM, COMLINE, READCF, READHD, WRTHEAD
c-----------------------------------------------------------------------
c --- Include file of parameters and commons
      include 'params.geo'
      include 'control.geo'
      include 'filnam.geo'
      include 'grid.geo'
      include 'lucat.geo'
      include 'qa.geo'
      include 'snow.geo'

c --- Get date and time from system
      call DATETM(rdate,rtime,rcpu)

c --- Get the name of the control file from the command line
      call COMLINE(runinp)

c --- Open the control file
      open(io5,file=runinp,status='old')

c --- Report progress
      write(iomesg,*)'SETUP PHASE'

c --- Check that the version and level number in the parameter
c --- file matches those in the code itself
      if(ver.ne.mver.or.level.ne.mlevel)then
         write(iomesg,10) ver,level,mver,mlevel
10       format(/1x,'ERROR in SUBR. SETUP -- The MAKEGEO version ',
     1   'and level numbers do not match those in the parameter file'/
     2   5x,'    Model Code - Version: ',a12,' Level: ',a12/
     3   5x,'Parameter File - Version: ',a12,' Level: ',a12)
         stop
      endif

c --- Read control file (open list file)
      call READCF

c --- Write header lines to list-file

      write(io6,*)
      write(io6,*) '--------------------------'
      write(io6,*) '    SETUP Information'
      write(io6,*) '--------------------------'
      write(io6,*)

c -----------------------------
c --- Report control data
c -----------------------------

      write(io6,*)
      write(io6,*) 'Application -----'
      write(io6,*) ctitle

      write(io6,*)
      write(io6,*) 'Control File Used -----'
      write(io6,*) runinp

      write(io6,*)
      write(io6,*) 'Processing Options -----'
      write(io6,*) 'Terrain Data File Used? :  ',lterr
      write(io6,*) 'Snow Data File Used?    :  ',lsnow
      if(LSNOW) then
         write(io6,*) 'Snow Processing Dates   :  ',
     &                 idatebeg,idateend
      endif
      if(nlx.EQ.0 .OR. nly.EQ.0) then
         write(io6,*) 'QA output for 1 cell is NOT requested'
      else
         write(io6,*) 'QA Cell located at      :  ',nlx,nly
      endif

      write(io6,*)
      write(io6,*) 'Input Land Use File -----'
      write(io6,'(a10,a)') 'ludat   : ',trim(ludat)
      if(LLU2) then
         write(io6,'(a10,a)') 'lu2dat  : ',trim(lu2dat)
      endif
      if(LTERR) then
         write(io6,*) 'Input Terrain File -----'
         write(io6,'(a10,a)') 'terrdat : ',trim(terrdat)
      endif
      if(LSNOW) then
         write(io6,*) 'Input SNOW File -----'
         write(io6,'(a10,a)') 'snowdat : ',trim(snowdat)
      endif

      write(io6,*)
      write(io6,*) 'Output GEO.DAT File -----'
      write(io6,'(a10,a)') 'geodat  : ',trim(geodat)
      if(LSNOW .AND. ifmtgeo.EQ.1) then
         write(io6,*) '(Daily files created with date added)'
      endif

      write(io6,*) 'Output List File -----'
      write(io6,'(a10,a)') 'runlst  : ',trim(runlst)
      write(io6,*) 'Output Plot Files -----'
      write(io6,'(a10,a)') 'lugrd   : ',trim(lugrd)
      write(io6,'(a10,a)') 'tegrd   : ',trim(tegrd)
      write(io6,'(a18)') 'luclr   : LUSE.CLR'

      write(io6,*)
      write(io6,*) 'Grid data (for output) ---------------------'
      write(io6,*) 'datum  : ' ,datum
      write(io6,*) 'pmap   : ' ,pmap
      if(LUTM) then
         write(io6,*) 'Hemisphere : ',utmhem
         write(io6,*) 'UTM zone   : ',izone
      endif
      write(io6,*) 'xorgn  : ' ,xorg
      write(io6,*) 'yorgn  : ' ,yorg
      write(io6,*) 'izone  : ' ,izone
      write(io6,*) 'delx   : ' ,delx
      write(io6,*) 'nx     : ' ,nx
      write(io6,*) 'ny     : ' ,ny
      if(LLCC.or.LPS.or.LEM.or.LLAZA.or.LTTM) then
         write(io6,*) 'rlat(N): ' ,reflat
         write(io6,*) 'rlon(E): ' ,reflon
         if(LLCC.or.LPS)write(io6,*) 'xlat1  : ' ,xlat1
         if(LLCC)write(io6,*) 'xlat2  : ' ,xlat2
      endif

      write(io6,*)
      write(io6,*) 'Land Use Processing Data -----'
      write(io6,*)
      write(io6,*)' Supplemental fractional land use file provided ',
     &             'for missing data?: ',llu2
      if(LLU2) write(io6,*)' (Read from LU2.DAT file) '
      write(io6,*)
      write(io6,*)' Land use category used for missing data: ',imiss
      write(io6,*)' (Must be listed in input categories for valid run)'
      write(io6,*)' (May be set to invalid LU to produce only QA files)'
      write(io6,*)
      write(io6,*)' Total fractional land use for cell may be greater',
     &            ' than or equal to ',flumin
      write(io6,*)' or less than or equal to .001 (missing)'
      write(io6,*)
      write(io6,*)' Water coverage threshold fraction (CFRACT) = ',
     1 cfract
      write(io6,*)' Number of input water categories: ',numwat
      write(io6,*)' Water categories: ',(iwat(i),i=1,numwat)

      if(LSNOW) then
        write(io6,*)
        write(io6,*)' Snow processing configuration:'
        if(msrl.EQ.1) then
           write(io6,*)' Surface roughness with snow computed'
           write(io6,*)' Height Scale(m): ',hscl
        else
           write(io6,*)' Surface roughness with snow from User Table'
        endif
        write(io6,*)' Min. snow depth(m) for roughness: ',sdpmin
        if(msal.EQ.1) then
           write(io6,*)' Surface albedo computed with snow age'
        else
           write(io6,*)' Surface albedo with snow from User Table'
        endif
      endif

      write(io6,*)
      write(io6,105)
105   format(/1x,'Number',2x,'Input Category',2x,'Redefined?'/
     1 25x,'(0=No,0<Yes)')
      do i = 1,nincat
         write(io6,106)i,incat(i),iredef(i)
         if(iredef(i).GT.0) then
            j=iredef(i)
            do k=1,nrec(j)
               write(io6,*) prec(j,k),' % redefined to category ',
     &                      incat(irec(j,k))
            enddo
         endif
      enddo
106   format(1x,i4,4x,i7,11x,i3)
      write(io6,*)
      write(io6,*)' Number of output categories: ',noutcat
      write(io6,*)' Values of output categories: ',
     &             (outcat(i),i=1,noutcat)
      write(io6,*)' New range of water categories: ',iwat1,iwat2
      write(io6,*)
      write(io6,*)' QA Check Option = ',lqacell
      if(LQACELL) then
         write(io6,207) nlx,nly
207      format(1x,'Check at grid cell (i,j) = (',i4,',',i4,')')
      endif

c --- Report properties assigned to each LU category
      write(io6,*)
      write(io6,*)' Properties Assigned to each LU Category'
      write(io6,*)' ----- Without Snow Cover --------------'
      write(io6,*)
      write(io6,'(a,a)')
     &   '  Input                 Bowen   Soil   Anthro   Leaf ',
     &   ' Mapped'
      write(io6,'(a,a)')
     &   '   LU   Z0(m)  Albedo   Ratio   HFlux   HFlux   Index',
     &   '   LU'
      write(io6,*)
      do i = 1,nincat
         write(io6,'(i5,6f8.3,i5)') incat(i),z0lu(i),alblu(i),
     &         bowlu(i),soillu(i),qflu(i),xlailu(i),mapcat(i)
      end do
      write(io6,*)
      write(io6,*)' Any input category that has been redefined will',
     &            ' not actually map because it has been zeroed'
      write(io6,*)
      write(io6,*)

      if(.NOT. LSNOW) goto 1500

      write(io6,*)
      write(io6,*)' Properties Assigned to each LU Category'
      write(io6,*)' ---- With Snow Cover ------------------'
      write(io6,*)
      write(io6,'(a,a)')
     &   '  Input                 Bowen   Soil   Anthro   Leaf ',
     &   ' Mapped'
      write(io6,'(a,a)')
     &   '   LU   Z0(m)  Albedo   Ratio   HFlux   HFlux   Index',
     &   '   LU'
      write(io6,*)
      do i = 1,nincat
         write(io6,'(i5,6f8.3,i5)')incats(i),z0lus(i),alblus(i),
     &         bowlus(i),soillus(i),qflus(i),xlailus(i),mapcats(i)
      end do
      write(io6,*)
      write(io6,*)' Any input category that has been redefined will',
     &            ' not actually map because it has been zeroed'
      write(io6,*)
      write(io6,*)
      write(io6,*)'Albedo adjustment parameters for snow-age'
      write(io6,'(a)')
     &   '   LU   Snow-Age    Albedo Reduction'
      write(io6,'(a)')
     &   '        Time Scale      Factor'
      write(io6,'(a)')
     &   '         (days)'
      do i = 1,nincat
         write(io6,205)incats(i),nages(i),ralb(i)
 205     format(i5,i8,4x,f13.3)
      enddo

 1500 continue

c --- Open fractional land use data file and process header
      open(io2,file=ludat, status='old')
      call READHD(io2,ludat)

c --- Open supplemental fractional LU data file and process header
      if(LLU2) then
         open(io3,file=lu2dat, status='old')
         call READHD(io3,lu2dat)
      endif

c --- Open terrain data file and process header (conditional)
      if(LTERR) then
         open(io4,file=terrdat, status='old')
         call READHD(io4,terrdat)
      endif

c --- Open output GEO.DAT data file and write header
      if ( .NOT. LSNOW) then
         open(io7,file=geodat, status='unknown')
         call WRTHEAD
      endif

c --- Open output QA plot files
      open(io8,file=lugrd,status='unknown')
      open(io9,file=tegrd,status='unknown')
      open(io53,file='luse.clr',status='unknown')

      return
      end
c----------------------------------------------------------------------
      subroutine readcf
c----------------------------------------------------------------------
c
c --- MAKEGEO   Version: 3.2          Level: 100301            READCF
c               J. Scire, D. Strimaitis
c
c --- PURPOSE:  Read the control file containing the file names of
c               the input and output files of the run, and associated
c               control variables, place into program variables,
c               and QA information.
c
c --- UPDATES:
c --- ver 3.0, Level 090526 to ver 3.11, Level 100301 - D. Strimaitis
c              - Accept both new and old control file formats
c              - Introduce file version record
c              - Move QA for snow data into QA section
c --- ver 2.27 Level 060309 to ver 3.0, Level 090526  - Zhong Wu
c              - Add snow data processing
c --- ver 2.26 Level 041230 to ver 2.27 Level 060309  - D. Strimaitis
c               - Filenames from c*70 to c*132 for CALUTILS V2.3
c                 and later
c --- ver 2.25 Level 041013 to ver 2.26 Level 041230  - D. Strimaitis
c               - Add FLUMIN control variable to input group 4a
c               - Add LU2.DAT file variables
c               - Allow invalid IMISS with warnings
c --- ver 2.22 Level 030709 to ver 2.25 Level 041013  - D. Strimaitis
c               - Add IMAGE control variable for GRD format to input
c                 group 1 (hidden)
c --- Ver 2.21 Level 030402 to ver 2.22 Level 030709  - D. Strimaitis
c              - Fix type assigned to LCFILES
c --- Ver 2.2  Level 011003 to Level 030402    - D. Strimaitis
c              - Add DATUM
c              - PMAP = UTM, TTM, LCC, PS, EM, or LAZA (imap removed)
c              - LSOHEM (L: T/F) replaced by UTMHEM (C*4: N/S)
c              - Add TYPE argument to XTRACTLL
c              - Add false Easting and Northing
c
c --- INPUTS:
c
c ---    Common block /QA/ variables:
c           VER, LEVEL
c
c        Parameters: io5, IO6, IOMESG, MXVAR
c
c --- OUTPUT:
c
c ---    Common block /FILNAM/ variables:
c           runinp,runlst,ludat,lu2dat,terrdat,geodat,lugrd,tegrd
c           lcfiles
c ---    Common block /CONTROL/ variables:
c           lterr, iflip, htfac, image, flumin,
c           lutm,llcc,lps,lem,llaza,lttm, llu2, lqamiss
c ---    Common block /GRID/ variables:
c           nx,ny,delx,xorg,yorg,
c           izone,reflat,reflon,xlat1,xlat2,feast,fnorth,
c           pmap,utmhem,datum,clat0,clon0,clat1,clat2
c ---    Common block /LUCAT/ variables:
c
c --- READCF called by:  SETUP
c --- READCF calls:      READIN, FILCASE, XTRACTLL
c----------------------------------------------------------------------
c
c --- Include parameter statements and commons
      include 'params.geo'
      include 'params.cal'
c
c --- Include common blocks
      include 'control.geo'
      include 'filnam.geo'
      include 'grid.geo'
      include 'lucat.geo'
      include 'qa.geo'
      include 'snow.geo'
c
c --- Local variables
      parameter(ndic=11)
      real x8(8),xsplit(3),x3(3)
      character*4 ctemp(132,8)
      character*4 clatlon(16,4)
      character*4 cpmap(8),cdatum(8)
      character*12 cvdic(mxvar,ndic)
      integer ivleng(mxvar,ndic),ivtype(mxvar,ndic)
      integer iwork(mxocat)
      integer incatsa(mxcat)
      logical lecho,lerrcf
      logical lmore

c --- Initialize local variables
      data lecho/.false./, lerrcf/.false./
      data names/8/

c --- Set Dictionary

      data cvdic/
     a  'RUNLST','LUDAT','TERRDAT','GEODAT','LUGRD','TEGRD','LU2DAT',
     a  'SNOWDAT','LCFILES',51*' ',
     b  'LTERR','IXQA','IYQA','IMAGE','LLU2',
     b  'LSNOW','IFMTGEO','IDATEBEG','IDATEEND',51*' ',
     c  'PMAP','IUTMZN','UTMHEM','RLAT0','RLON0','RLAT1','RLAT2',
     c  'DATUM','XREFKM','YREFKM','NX','NY','DGRIDKM',
     c  'FEAST','FNORTH',  45*' ',
     d  'NOUTCAT','IWAT1','IWAT2',  57*' ',
     e  'OUTCAT',  59*' ',
     f  'NINCAT','NUMWAT','NSPLIT','CFRACT','IMISS','FLUMIN',
     f  'MSRL','HSCL','MSAL','SDPMIN',  50*' ',
     g  'X',  59*' ',
     h  'XS',  59*' ',
     i  'XF',  59*' ',
     j  'IWAT',  59*' ',
     k  'XSPLIT',  59*' '/

      data ivleng/
     a  8*132,1, 51*0,
     b  9*1, 51*0,
     c  8,2*1,4*16,8,7*1, 45*0,
     d  3*1, 57*0,
     e  mxocat, 59*0,
     f  10*1, 50*0,
     g  8, 59*0,
     h  8, 59*0,
     i  3, 59*0,
     j  1, 59*0,
     k  3, 59*0/

c --- Variable types (ivtype) are coded as:
c          0 = null
c          1 = real
c          2 = integer
c          3 = logical
c          4 = character
      data ivtype/
     a  8*4,3, 51*0,
     b  3,3*2,2*3,3*2, 51*0,
     c  4,2,6*4,1,1,2,2,3*1, 45*0,
     d  3*2, 57*0,
     e  2, 59*0,
     f  3*2,1,2,1,2,1,2,1, 50*0,
     g  1, 59*0,
     h  1, 59*0,
     i  1, 59*0,
     j  2, 59*0,
     k  1, 59*0/

c ---------------
c --- Title Line
c ---------------
c --- File version identification record is introduced
c --- but not currently used so do not process
c --- If present, read second record for title
c --- Read title for GEO.DAT file
      read(io5,'(a80)')ctitle
      if(ctitle(1:11).EQ.'MAKEGEO.INP') then
         read(io5,'(a80)')ctitle
      endif

c ------------------
c --- Input Group 0
c ------------------

c --- Initialize the temporary arrays
      do i=1,names
         do j=1,132
            ctemp(j,i)(1:1)=' '
         enddo
      enddo

c --- Read the group data
       call READIN(cvdic(1,1),ivleng(1,1),ivtype(1,1),io5,iomesg,
     & lecho,
     1 ctemp(1,1),ctemp(1,2),ctemp(1,3),ctemp(1,4),ctemp(1,5),
     2 ctemp(1,6),ctemp(1,7),ctemp(1,8),lcfiles,
     3 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     4 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     7 idum,idum,idum)

c --- Prepare any filenames included in the I/O file by erasing
c --- the default filename set above
      if(ctemp(1,1)(1:1).ne.' ')runlst=' '
      if(ctemp(1,2)(1:1).ne.' ')ludat=' '
      if(ctemp(1,3)(1:1).ne.' ')terrdat=' '
      if(ctemp(1,4)(1:1).ne.' ')geodat=' '
      if(ctemp(1,5)(1:1).ne.' ')lugrd=' '
      if(ctemp(1,6)(1:1).ne.' ')tegrd=' '
      if(ctemp(1,7)(1:1).ne.' ')lu2dat=' '
      if(ctemp(1,8)(1:1).ne.' ')snowdat=' '

c --- Transfer the char*4 data into the char*132 variables
      do j=1,132
         if(ctemp(j,1)(1:1).ne.' ')runlst(j:j)=ctemp(j,1)(1:1)
         if(ctemp(j,2)(1:1).ne.' ')ludat(j:j)=ctemp(j,2)(1:1)
         if(ctemp(j,3)(1:1).ne.' ')terrdat(j:j)=ctemp(j,3)(1:1)
         if(ctemp(j,4)(1:1).ne.' ')geodat(j:j)=ctemp(j,4)(1:1)
         if(ctemp(j,5)(1:1).ne.' ')lugrd(j:j)=ctemp(j,5)(1:1)
         if(ctemp(j,6)(1:1).ne.' ')tegrd(j:j)=ctemp(j,6)(1:1)
         if(ctemp(j,7)(1:1).ne.' ')lu2dat(j:j)=ctemp(j,7)(1:1)
         if(ctemp(j,8)(1:1).ne.' ')snowdat(j:j)=ctemp(j,8)(1:1)
      enddo

c --- Convert the file names to the proper case
      call FILCASE(lcfiles,runlst)
      call FILCASE(lcfiles,ludat)
      call FILCASE(lcfiles,terrdat)
      call FILCASE(lcfiles,geodat)
      call FILCASE(lcfiles,lugrd)
      call FILCASE(lcfiles,tegrd)
      call FILCASE(lcfiles,lu2dat)
      call FILCASE(lcfiles,snowdat)

c --- Open listfile
      open(io6,file=runlst,status='unknown')

c --- Write banner to list file
      write(io6,5) ver,level
5     format(///,26x,'MAKEGEO OUTPUT SUMMARY',/,19x,'VERSION:  ',A12,
     1       ' LEVEL:  ',A12///)

c -----------------
c --- Input Group 1
c -----------------

c --- Set initial QA cell flag to BLOCK DATA values
      ixqa=nlx
      iyqa=nly

      call READIN(cvdic(1,2),ivleng(1,2),ivtype(1,2),io5,io6,lecho,
     1 LTERR,IXQA,IYQA,IMAGE,LLU2,LSNOW,IFMTGEO,IDATEBEG,IDATEEND,
     2 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     3 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     4 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6 idum,idum,idum)

c --- Assign QA cell flag and index names
      if(ixqa.GT.0 .AND. iyqa.GT.0) lqacell=.TRUE.
      nlx=ixqa
      nly=iyqa

c -----------------
c --- Input Group 2
c -----------------

c --- Initialize the temporary arrays for the character lat/lon fields
      do i=1,4
         do j=1,16
            clatlon(j,i)(1:1)=' '
         enddo
      enddo
      do j=1,16
         clat0(j:j)=' '
         clon0(j:j)=' '
         clat1(j:j)=' '
         clat2(j:j)=' '
      enddo

c --- Initialize the temporary array for the Datum-Region name and 
c --- map projection
      do j=1,8
         cpmap(j)(1:1)=' '
         cdatum(j)(1:1)=' '
      enddo

c --- Initialize input false easting and northing with defaults
      feastin=feast
      fnorthin=fnorth

      call READIN(cvdic(1,3),ivleng(1,3),ivtype(1,3),io5,io6,lecho,
     1 CPMAP,IUTMZN,UTMHEM,
     2 CLATLON(1,1),CLATLON(1,2),CLATLON(1,3),CLATLON(1,4),
     3 CDATUM,XREFKM,YREFKM,NX,NY,DGRIDKM,FEASTIN,FNORTHIN,
     4 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     7 idum,idum,idum,idum,idum,idum,idum,idum,idum)

c --- Transfer the char*4 data into the char*16 variables
      do j=1,16
         if(clatlon(j,1)(1:1).ne.' ')clat0(j:j)=clatlon(j,1)(1:1)
         if(clatlon(j,2)(1:1).ne.' ')clon0(j:j)=clatlon(j,2)(1:1)
         if(clatlon(j,3)(1:1).ne.' ')clat1(j:j)=clatlon(j,3)(1:1)
         if(clatlon(j,4)(1:1).ne.' ')clat2(j:j)=clatlon(j,4)(1:1)
      enddo

c --- Transfer the char*4 data into the char*8 variables
      if(cpmap(1)(1:1).ne.' ') then
         do j=1,8
            pmap(j:j)=cpmap(j)(1:1)
         enddo
      endif
      if(cdatum(1)(1:1).ne.' ') then
         do j=1,8
            datum(j:j)=cdatum(j)(1:1)
         enddo
      endif

c --- Pad the char*4 UTM Hemisphere
      utmhem(2:4)='   '

c --------------------------------------------------
c --- Translate selected inputs to MAKEGEO variables
c --------------------------------------------------

c --- Grid variables
      xorg=xrefkm
      yorg=yrefkm
      delx=dgridkm
      izone=iutmzn

c --- Translate character lat/lon to real NLat/ELon
      if(clat0(1:1).NE.' ') call XTRACTLL(io6,'LAT ',clat0,reflat)
      if(clon0(1:1).NE.' ') call XTRACTLL(io6,'LON ',clon0,reflon)
      if(clat1(1:1).NE.' ') call XTRACTLL(io6,'LAT ',clat1,xlat1)
      if(clat2(1:1).NE.' ') call XTRACTLL(io6,'LAT ',clat2,xlat2)

c --- Set logicals for map projection PMAP
      if(pmap.EQ.'UTM     ')  lutm =.TRUE.
      if(pmap.EQ.'LCC     ')  llcc =.TRUE.
      if(pmap.EQ.'PS      ')  lps  =.TRUE.
      if(pmap.EQ.'EM      ')  lem  =.TRUE.
      if(pmap.EQ.'LAZA    ')  llaza=.TRUE.
      if(pmap.EQ.'TTM     ')  lttm =.TRUE.

c --- Adjust projection information if needed
      if(LEM) then
c ---    Equatorial Mercator projection matches at 0.0N, 
c ---    and places the northing origin at 0.0N
         reflat=0.0
         xlat1=0.0
         xlat2=0.0
      endif

c --- Transfer input false easting and northing if the projection
c --- can use it
      if(LLCC.or.LTTM.or.LLAZA) then
         feast=feastin
         fnorth=fnorthin
      endif

c ------------------
c --- Input Group 3a
c ------------------

      call READIN(cvdic(1,4),ivleng(1,4),ivtype(1,4),io5,io6,lecho,
     1 NOUTCAT,IWAT1,IWAT2,
     2 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     3 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     4 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6 idum,idum,idum,idum,idum,idum,idum,idum,idum)

c ------------------
c --- Input Group 3b
c ------------------

c --- Set counters
      ilast =0
      ioutcat=0

c --- Initialize contents of OUTCAT work array to zero
10    do i=1,mxocat
         iwork(i)=0
      enddo

      call READIN(cvdic(1,5),ivleng(1,5),ivtype(1,5),io5,io6,lecho,
     1 iwork,
     2 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     3 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     4 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum)

c --- Place non-zero values into OUTCAT array
      do i=1,mxocat
         if(iwork(i).NE.0) then
            ioutcat=ioutcat+1
            ilast=ilast+1
c ---       Limit to maximum array dimension
            ilast=MIN(ilast,mxocat)
            outcat(ilast)=iwork(i)
         endif
      enddo
c --- Continue OUTCAT reads until NOUTCAT values are found
      if(ioutcat.LT.noutcat) goto 10

c ------------------
c --- Input Group 4a
c ------------------

      call READIN(cvdic(1,6),ivleng(1,6),ivtype(1,6),io5,io6,lecho,
     1 NINCAT,NUMWAT,NSPLIT,CFRACT,IMISS,FLUMIN,
     2 MSRL,HSCL,MSAL,SDPMIN,
     3 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     4 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     7 idum,idum)

c ------------------
c --- Input Group 4b
c ------------------

c --- Expect NINCAT records
      do i=1,nincat
         do j=1,8
            x8(j)=0.0
         enddo
         call READIN(cvdic(1,7),ivleng(1,7),ivtype(1,7),io5,io6,lecho,
     1    X8,
     2    idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     3    idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     4    idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5    idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6    idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum)

c ---    Place values into arrays (limit index to mxcat)
         icat=MIN(i,mxcat)
         incat(icat)=NINT(x8(1))
         z0lu(icat)=x8(2)
         alblu(icat)=x8(3)
         bowlu(icat)=x8(4)
         soillu(icat)=x8(5)
         qflu(icat)=x8(6)
         xlailu(icat)=x8(7)
         mapcat(icat)=NINT(x8(8))

      enddo
      
c --- Assign array pointer for missing LU class
      imiss2=0
      do i=1,nincat
        if(imiss.EQ.incat(i))imiss2=i
      enddo

c ------------------
c --- Input Group 4c - for Snow processing (read if LSNOW=T)
c ------------------
    
      if(LSNOW) then
c ---   Expect NINCAT records
        do i=1,nincat
           do j=1,8
              x8(j)=0.0
           enddo
         call READIN(cvdic(1,8),ivleng(1,8),ivtype(1,8),io5,io6,lecho,
     1    X8,
     2    idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     3    idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     4    idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5    idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6    idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum)

c ---      Place values into arrays (limit index to mxcat)
           icat=MIN(i,mxcat)
           incats(icat)=NINT(x8(1))
           z0lus(icat)=x8(2)
           alblus(icat)=x8(3)
           bowlus(icat)=x8(4)
           soillus(icat)=x8(5)
           qflus(icat)=x8(6)
           xlailus(icat)=x8(7)
           mapcats(icat)=NINT(x8(8))
        enddo
      endif

c ------------------
c --- Input Group 4d - for Snow processing (read if LSNOW=T)
c ------------------

      if(LSNOW) then
c ---   Expect NINCAT records
        do i=1,nincat
           do j=1,3
              x3(j)=0.0
           enddo
         call READIN(cvdic(1,9),ivleng(1,9),ivtype(1,9),io5,io6,lecho,
     1    X3,
     2    idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     3    idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     4    idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5    idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6    idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum)

c ---      Place values into arrays (limit index to mxcat)
c ---      Note: incatsa() is local and must match incats()
           icat=MIN(i,mxcat)
           incatsa(icat)=NINT(x3(1))
           nages(icat)=nint(x3(2))
           ralb(icat)=x3(3)
        enddo
      endif

c ------------------
c --- Input Group 4e
c ------------------

c --- Expect NUMWAT records
      do i=1,numwat
         call READIN(cvdic(1,10),ivleng(1,10),ivtype(1,10),io5,io6,
     1    lecho,IWAT(i),
     2    idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     3    idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     4    idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5    idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6    idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum)
      enddo

c ------------------
c --- Input Group 4f
c ------------------

c --- Set initial values
      nredef=0

      if(NSPLIT.GT.0) then
c ---    NSPLIT land use classes are redefined (split)
20       do j=1,3
            xsplit(j)=0.0
         enddo
         call READIN(cvdic(1,11),ivleng(1,11),ivtype(1,11),io5,io6,
     1    lecho,XSPLIT,
     2    idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     3    idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     4    idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5    idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6    idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum)

c ---    Convert land use IDs to integer
         luin=NINT(xsplit(1))
         luout=NINT(xsplit(2))
c ---    Find corresponding index in input land use array
         idin=0
         idout=0
         do i=1,nincat
            if(luin.EQ.incat(i)) idin=i
            if(luout.EQ.incat(i)) idout=i
         enddo
c ---    Stop reading control file if either index is invalid
         if(idin.EQ.0 .OR. idout.EQ.0) then
            write(io6,*)
            write(io6,*) 'READCF:  Error in Input Group 4d'
            write(io6,*) 'Invalid land use found = ',luin,luout
            write(io6,*) 'Split option uses LU categories from the'
            write(io6,*) 'input categories: ',(incat(i),i=1,nincat)
            lerrcf=.TRUE.
            goto 101
         endif
         if(iredef(idin).EQ.0) then
c ---       This input ID has not been redefined yet, increment the 
c ---       count of IDs redefined, and assign to iredef
            nredef=nredef+1
            iredef(idin)=nredef
            nnrec(nredef)=idin
         endif
c ---    Set the index for this definition
         kredef=iredef(idin)
c ---    Update the number of receiving IDs for this split
         nrec(kredef)=nrec(kredef)+1
c ---    Set data for this ID
         irec(kredef,nrec(kredef))=idout
         prec(kredef,nrec(kredef))=xsplit(3)

c ---    Should more records be provided?
         lmore=.FALSE.
         if(nredef.LT.nsplit) lmore=.TRUE.
         do i=1,nredef
            psum=0
            do j=1,nrec(i)
               psum=psum+prec(i,j)
            enddo
            if(NINT(psum).LT.100) lmore=.TRUE.
         enddo
         if(LMORE) goto 20         
      endif

c ---------------------
c --- Perform QA checks
c ---------------------
101   continue

c --- Test for valid QA cell location
      if(ixqa.LT.0 .OR. ixqa.GT.mxnx) then
         write(io6,*)
         write(io6,*) 'READCF:  Error in Input Group 1'
         write(io6,*) 'IXQA out of range      = ',ixqa
         write(io6,*) 'IXQA should be positive, and less than ',mxnx
         lerrcf=.TRUE.
      endif
      if(iyqa.LT.0 .OR. iyqa.GT.mxny) then
         write(io6,*)
         write(io6,*) 'READCF:  Error in Input Group 1'
         write(io6,*) 'IYQA out of range      = ',iyqa
         write(io6,*) 'IYQA should be positive, and less than ',mxny
         lerrcf=.TRUE.
      endif

c --- Test for valid GEO.DAT output file format
c --- (only 1 format is currently supported)
      if(ifmtgeo.NE.1) then
         write(io6,*)
         write(io6,*) 'READCF:  Error in Input Group 1'
         write(io6,*) 'IFMTGEO out of range   = ',ifmtgeo
         write(io6,*) 'Only IFMTGEO = 1 is supported'
         lerrcf=.TRUE.
      endif

c --- Test for valid output day range
      if(idatebeg.GT.idateend) then
         write(io6,*)
         write(io6,*) 'READCF:  Error in Input Group 1'
         write(io6,*) 'Begin date comes after End date'
         write(io6,*) 'IDATEBEG = ',idatebeg
         write(io6,*) 'IDATEEND = ',idateend
         lerrcf=.TRUE.
      endif

c --- Test for valid PMAP
      if(lutm.OR.llcc.OR.lps.OR.lem.OR.llaza.OR.lttm) then
c        OK
      else
         write(io6,*)
         write(io6,*) 'READCF:  Error in Input Group 2'
         write(io6,*) 'Unknown PMAP             = ',pmap
         write(io6,*) 'PMAP must be UTM,LCC,PS,EM,LAZA, or TTM'
         lerrcf=.TRUE.
      endif

c --- Test for valid IUTMZN
      if((iutmzn.LT.1 .OR. iutmzn.GT.60) .AND. LUTM) then
         write(io6,*)
         write(io6,*) 'READCF:  Error in Input Group 2'
         write(io6,*) 'IUTMZN out of range      = ',iutmzn
         write(io6,*) 'IUTMZN should be 1 to 60'
         lerrcf=.TRUE.
      endif

c --- Test for valid UTMHEM
      if((utmhem.NE.'N   '.AND.utmhem.NE.'S   ') .AND. LUTM) then
         write(io6,*)
         write(io6,*) 'READCF:  Error in Input Group 2'
         write(io6,*) 'UTMHEM out of range      = ',utmhem
         write(io6,*) 'UTMHEM should be N or S'
         lerrcf=.TRUE.
      endif

c --- Test for lat/lon of origin for LCC/PS/EM/LAZA/TTM map projection
      if(LLCC .or. LPS .or. LEM .or. LLAZA .or. LTTM) then
         if(reflat .LT. -900.0 .AND. reflon .LT. -900.0) then
            write(io6,*)
            write(io6,*) 'READCF:  Error in Input Group 2'
            write(io6,*) 'Missing lat/lon of origin for ',pmap
            lerrcf=.TRUE.
         endif
      endif

c --- Test for matching latitudes for LCC map projection
      if(LLCC) then
         if(xlat1 .LT. -900.0 .OR. xlat2 .LT. -900.0) then
            write(io6,*)
            write(io6,*) 'READCF:  Error in Input Group 2'
            write(io6,*) 'Missing matching lats for ',pmap
            lerrcf=.TRUE.
         endif
      endif

c --- Test for matching latitudes for PS/EM map projection
      if(LPS .or. LEM) then
         if(xlat1 .LT. -900.0) then
            write(io6,*)
            write(io6,*) 'READCF:  Error in Input Group 2'
            write(io6,*) 'Missing matching lats for ',pmap
            lerrcf=.TRUE.
         endif
      endif

c --- Test for valid NX,NY
      if(nx.GT.mxnx) then
         write(io6,*)
         write(io6,*) 'READCF:  Error in Input Group 2'
         write(io6,*) 'NX exceeds the parameter MXNX '
         write(io6,*) 'NX, MXNX = ',nx,mxnx
         write(io6,*) 'Increase MXNX in PARAMS.geo and recompile'
         write(io6,*) 'or reduce the number of X-grid cells'
         lerrcf=.TRUE.
      endif
      if(ny.GT.mxny) then
         write(io6,*)
         write(io6,*) 'READCF:  Error in Input Group 2'
         write(io6,*) 'NY exceeds the parameter MXNY '
         write(io6,*) 'NY, MXNY = ',ny,mxny
         write(io6,*) 'Increase MXNY in PARAMS.geo and recompile'
         write(io6,*) 'or reduce the number of Y-grid cells'
         lerrcf=.TRUE.
      endif

c --- Test for valid DGRIDKM
      if(dgridkm.LE.0.0) then
         write(io6,*)
         write(io6,*) 'READCF:  Error in Input Group 2'
         write(io6,*) 'DGRIDKM must be positive '
         write(io6,*) 'DGRIDKM = ',dgridkm
         lerrcf=.TRUE.
      endif

c --- Test for valid NOUTCAT
      if(noutcat.LE.0 .OR. noutcat.GT.mxocat) then
         write(io6,*)
         write(io6,*) 'READCF:  Error in Input Group 3'
         write(io6,*) 'NOUTCAT out of range          = ',noutcat
         write(io6,*) 'NOUTCAT should be 1 to MXOCAT = ',mxocat
         lerrcf=.TRUE.
      endif

c --- Test for valid NINCAT
      if(nincat.LE.0 .OR. nincat.GT.mxcat) then
         write(io6,*)
         write(io6,*) 'READCF:  Error in Input Group 4'
         write(io6,*) 'NINCAT out of range         = ',nincat
         write(io6,*) 'NINCAT should be 1 to MXCAT = ',mxcat
         lerrcf=.TRUE.
      endif

c --- Test for valid NSPLIT
      if(nsplit.GE.nincat) then
         write(io6,*)
         write(io6,*) 'READCF:  Error in Input Group 4'
         write(io6,*) 'NSPLIT out of range               = ',nsplit
         write(io6,*) 'NSPLIT should be less than NINCAT = ',nincat
         lerrcf=.TRUE.
      endif
      if(nsplit.NE.nredef) then
         write(io6,*)
         write(io6,*) 'READCF:  Error in Input Group 4'
         write(io6,*) 'NSPLIT does not match number provided'
         write(io6,*) 'NSPLIT, NREDEF  = ',nsplit,nredef
         lerrcf=.TRUE.
      endif

c --- Test for valid IMISS
      lqamiss=.FALSE.
      if(imiss2.EQ.0) then
         lqamiss=.TRUE.
         write(io6,*)
         write(io6,*)'WARNING (Input Group 4)'
         write(io6,*)' IMISS is not an input category: ',imiss
         write(io6,*)' Input categories: ',(incat(i),i=1,nincat)
         write(io6,*)
         write(io6,*)' NO GEO.DAT file is created'
         write(io6,*)' Existing GEO.DAT file is deleted'
         write(io6,*)' QALUSE.GRD and QATERR.GRD are created'
         write(io6,*)
         write(*,*)
         write(*,*)' WARNING issued for CONTROL file input'
         write(*,*)' IMISS is not an input category: ',imiss
         write(*,*)' Review messages written to the LIST file'
         write(*,*)' --- NO GEO.DAT file is created ---'
         write(*,*)
      endif

c --- Test for valid snow processing variables (conditional)
      if(LSNOW) then
c ---    Test for valid MSRL
         if(msrl.LT.1 .OR. msrl.GT.2) then
            write(io6,*)
            write(io6,*) 'READCF:  Error in Input Group 4'
            write(io6,*) 'MSRL out of range     = ',msrl
            write(io6,*) 'Expected MSRL = 1 or 2'
            lerrcf=.TRUE.
         endif
c ---    Test for valid MSAL
         if(msal.LT.1 .OR. msal.GT.2) then
            write(io6,*)
            write(io6,*) 'READCF:  Error in Input Group 4'
            write(io6,*) 'MSAL out of range     = ',msal
            write(io6,*) 'Expected MSAL = 1 or 2'
            lerrcf=.TRUE.
         endif
c ---    Test for matching land use categories
         n=0
         do i=1,nincat
            if(incats(i).NE.incat(i))   n=n+1
            if(mapcats(i).NE.mapcat(i)) n=n+1
            if(incatsa(i).NE.incat(i))  n=n+1
         enddo
         if(n.GT.0) then
            write(io6,*)
            write(io6,*) 'READCF:  Error in Input Group 4'
            write(io6,*) 'Land use categories for snow-processing'
            write(io6,*) 'inputs do not match other land use inputs'
            write(io6,*) '  No Snow        Snow       Snow'
            write(io6,*) '  Group 4b     Group 4c   Group 4d'
            write(io6,*) ' ----------   ----------  --------'
            write(io6,*) ' Input Output Input Output Input '
            write(io6,*) '   ID    ID     ID    ID     ID'
            do i=1,nincat
               write(io6,'(3(1x,i6))') incat(i),mapcat(i),
     &                                 incats(i),mapcats(i),incatsa(i)
            enddo
            lerrcf=.TRUE.
         endif
      endif

c --- STOP now if error exists in the control file
      if(LERRCF) then
         write(*,*)'ERRORS are found in the CONTROL file'
         write(*,*)'Review messages written to the LIST file'
         stop
      endif

      return
      end
c-----------------------------------------------------------------------
      subroutine readhd(io,fname)
c-----------------------------------------------------------------------
c
c --- MAKEGEO   Version: 3.2          Level: 110401            READHD
c               D. Strimaitis
c
c PURPOSE:     READHD reads the header records of an input data file
c              and screens coordinate and grid parameters
c
c --- UPDATES:
c --- ver 2.27 Level 060309 to ver 3.2 Level 110401   - D. Strimaitis
c               - Read input categories from LU.DAT file v2.1
c               - Add QA that they must match control file categories
c --- ver 2.26 Level 041230 to ver 2.27 Level 060309  - D. Strimaitis
c               - Filenames from c*70 to c*132 for CALUTILS V2.3
c                 and later
c --- ver 2.2 Level 030402 to ver 2.26 Level 041230  - D. Strimaitis
c               - Add file name to argument list
c
c ARGUMENTS:
c --- INPUTS:
c            io - integer    - File unit number
c         fname - char*132   - File name
c
c     Common blocks
c             /CONTROL/   logicals
c             /GRID/      data
c             /LUCAT/     nincat, incat()
c
c  RETURNED:  
c             /LUCAT/     nincat0, incat0()
c
c CALLING ROUTINES:   SETUP
c
c EXTERNAL ROUTINES:  ALLCAP, XTRACTLL, LRSAME
c-----------------------------------------------------------------------
c --- Include file of parameters and commons
      include 'params.geo'
      include 'params.cal'
      include 'control.geo'
      include 'grid.geo'
      include 'lucat.geo'

c --- Local Variables
      character*4 xyunitin,zunitin,EWorder,NSorder,utmhemin
      character*8 lutype,pmapin,datumin
      character*16 dataset,dataver,blank16
      character*16 clat0in,clon0in,clat1in,clat2in
      character*64 datamod
      character*132 fname
      character*80 comment1

      logical lutmin,llccin,lpsin,lemin,llazain,lttmin
      logical llu,lte
      logical LRSAME
      logical lerror

      data nlim/1/
      data blank16/'                '/

      lutmin =.FALSE.
      llccin =.FALSE.
      lpsin  =.FALSE.
      lemin  =.FALSE.
      llazain=.FALSE.
      lttmin =.FALSE.

      llu =.FALSE.
      lte =.FALSE.

      lerror=.FALSE.

      clat0in=blank16
      clon0in=blank16
      clat1in=blank16
      clat2in=blank16


c --- Read header information
c ---------------------------
      write(io6,*)
      write(io6,*)
      write(io6,*)'Header records from input data file:'
      write(io6,*) fname
      write(io6,*)'------------------------------------'

c --- Dataset, Version, Modifier
      read(io,'(2a16,a64)') dataset,dataver,datamod
      write(io6,'(2a16,a64)') dataset,dataver,datamod
c --- Convert Dataset to upper case
      do i=1,16
         call ALLCAP(dataset(i:i),nlim)
      enddo

c --- Identify dataset type
      if(dataset.EQ.'LU.DAT') then
         llu=.TRUE.
      elseif(dataset.EQ.'TERREL.DAT' .OR. dataset.EQ.'TERRAIN.DAT') then
         lte=.TRUE.
      else
c ---    FATAL ERROR
         write(io6,*)
         write(io6,*)'RDHEAD: Invalid input file DATASET type: ',dataset
         write(io6,*)'        Expected LU.DAT, TERREL.DAT, TERRAIN.DAT'
         lerror=.TRUE.
         goto 999
      endif

c --- Number of comment records
      read(io,*) ncomment
      write(io6,*) ncomment
c --- Comments (optional/repeatable)
      do k=1,ncomment
         read(io,'(a80)') comment1
         write(io6,'(a80)') comment1
      enddo

      if(LLU) then
c ---    LU File Type
         read(io,'(a8)') lutype
         write(io6,'(a8)') lutype
         do i=1,8
            call ALLCAP(lutype(i:i),nlim)
         enddo
      endif

c --- Map projection
      read(io,'(a8)') pmapin
      write(io6,'(a8)') pmapin
      do i=1,8
         call ALLCAP(pmapin(i:i),nlim)
      enddo

      if(pmapin.EQ.'UTM     ')  lutmin =.TRUE.
      if(pmapin.EQ.'LCC     ')  llccin =.TRUE.
      if(pmapin.EQ.'PS      ')  lpsin  =.TRUE.
      if(pmapin.EQ.'EM      ')  lemin  =.TRUE.
      if(pmapin.EQ.'LAZA    ')  llazain=.TRUE.
      if(pmapin.EQ.'TTM     ')  lttmin =.TRUE.

c --- Map projection parameters
      if(LUTMIN) then
         read(io,'(i4,a4)') izonein,utmhemin
         write(io6,'(i4,a4)') izonein,utmhemin
      elseif(LLCCIN) then
         read(io,'(4a16)') clat0in,clon0in,clat1in,clat2in
         write(io6,'(4a16)') clat0in,clon0in,clat1in,clat2in
      elseif(LPSIN) then
         read(io,'(3a16)') clat0in,clon0in,clat1in
         write(io6,'(3a16)') clat0in,clon0in,clat1in
      elseif(LEMIN.or.LLAZAIN.or.LTTMIN) then
         read(io,'(2a16)') clat0in,clon0in
         write(io6,'(2a16)') clat0in,clon0in
      endif
c --- Map false Easting/Northing
      if(LLCCIN.or.LLAZAIN.or.LTTMIN) then
         read(io,*) feastin,fnorthin
         write(io6,*) feastin,fnorthin
      else
         feastin=feast
         fnorthin=fnorth
      endif
c --- Map DATUM
      read(io,'(a8,a12)') datumin,daten
      write(io6,'(a8,a12)') datumin,daten
      do i=1,8
         call ALLCAP(datumin(i:i),nlim)
      enddo

      if(LLU) then
c ---    Grid
         read(io,*) nxin,nyin,xoin,yoin,dgridin,dgridin,
     &              mxcatin
         write(io6,'(2i8,4f12.3,i6)') nxin,nyin,xoin,yoin,
     &                            dgridin,dgridin,mxcatin
c ---    XYUNIT
         read(io,'(a4)') xyunitin
         write(io6,'(a4)') xyunitin
         do i=1,4
            call ALLCAP(xyunitin(i:i),nlim)
         enddo
c ---    LU categories (dataset v2.1)
         nincat0=MIN(mxcatin,mxcat)
         read(dataver,'(f)') rver
         if(rver.GT.2.01) then
            read(io,*) (incat0(n),n=1,nincat0)
            write(io6,*) (incat0(n),n=1,nincat0)
         else
            nincat0=0
         endif
      elseif(LTE) then
c ---    Grid
         read(io,*) nxin,nyin,xoin,yoin,dgridin,dgridin
         write(io6,'(2i8,4f12.3)') nxin,nyin,xoin,yoin,
     &                             dgridin,dgridin
c ---    XYUNIT,ZUNIT
         read(io,'(2a4)') xyunitin,zunitin
         write(io6,'(2a4)') xyunitin,zunitin
c ---    Record structure
         read(io,'(2a4)') EWorder,NSorder
         write(io6,'(2a4)') EWorder,NSorder
c ---    Convert to upper case
         do i=1,4
            call ALLCAP(xyunitin(i:i),nlim)
            call ALLCAP(zunitin(i:i),nlim)
            call ALLCAP(EWorder(i:i),nlim)
            call ALLCAP(NSorder(i:i),nlim)
         enddo
      endif

c --- QA header information
c -------------------------

c --- Grid information
      if(nx.NE.nxin.OR.ny.NE.nyin) then
         write(io6,*)
         write(io6,*)'RDHEAD:  Problem in input file type: ',dataset
         write(io6,*)'         Number of grid cells does not match'
         write(io6,*)'         Input file NX,NY  : ',nxin,nyin
         write(io6,*)'         Control file NX,NY: ',nx,ny
         lerror=.TRUE.
      endif
      if(xyunitin.NE.'KM  ') then
         write(io6,*)
         write(io6,*)'RDHEAD:  Problem in input file type: ',dataset
         write(io6,*)'         XY units must be KM'
         write(io6,*)'         Input file: ',xyunitin
         lerror=.TRUE.
      endif
      if(.not.LRSAME(0.0001,dgridin,delx))then
         write(io6,*)
         write(io6,*)'RDHEAD:  Problem in input file type: ',dataset
         write(io6,*)'         Grid cell size does not match'
         write(io6,*)'         Input file  : ',dgridin
         write(io6,*)'         Control file: ',delx
         lerror=.TRUE.
      endif
      if((.not.LRSAME(0.0001,xoin,xorg)).or.
     1   (.not.LRSAME(0.0001,yoin,yorg)))then
         write(io6,*)
         write(io6,*)'RDHEAD:  Problem in input file type: ',dataset
         write(io6,*)'         SW corner coordinates do not match'
         write(io6,*)'         Input file xkm,ykm  : ',xoin,yoin
         write(io6,*)'         Control file xkm,ykm: ',xorg,yorg
         lerror=.TRUE.
      endif

c --- Land Use Checks
c --- Check for a match of the number of (input) land use categories
      if(LLU .AND. mxcatin.NE.nincat) then
         write(io6,*)
         write(io6,*)'RDHEAD:  Problem in input file type: ',dataset
         write(io6,*)'         Number of land use categories: ',mxcatin
         write(io6,*)'         Control file categories      : ',nincat
         lerror=.TRUE.
      endif
      if(LLU .AND. lutype.NE.'FRACTION')then
         write(io6,*)
         write(io6,*)'RDHEAD:  Problem in input file type: ',dataset
         write(io6,*)'         Incorrect dataset type: ',lutype
         write(io6,*)'         Type required         : FRACTION'
         lerror=.TRUE.
      endif
      if(LLU .AND. nincat0.GT.0) then
c ---    LU categories (dataset v2.1) must match control file
         incatbad=0
         if(nincat0.EQ.nincat) then
            do n=1,nincat
               if(incat(n).NE.incat0(n)) incatbad=1
            enddo
         else
            incatbad=1
         endif
         if(incatbad.GT.0) then
           write(io6,*)
           write(io6,*)'RDHEAD:  Problem in input file type: ',dataset
           write(io6,*)'         Control file categories   : ',nincat
           write(io6,*) (incat(n),n=1,nincat)
           write(io6,*)'         LU file categories        : ',nincat0
           write(io6,*) (incat0(n),n=1,nincat0)
           lerror=.TRUE.
         endif
      endif

c --- Projection checks
      if(pmap.NE.pmapin) then
         write(io6,*)
         write(io6,*)'RDHEAD:  Problem in input file type: ',dataset
         write(io6,*)'         Map projection PMAP does not match'
         write(io6,*)'         Input file  : ',pmapin
         write(io6,*)'         Control file: ',pmap
         lerror=.TRUE.
      endif
      if(LUTMIN .AND. LUTM) then
         if(izonein.NE.izone)then
            write(io6,*)
            write(io6,*)'RDHEAD:  Problem in input file type: ',dataset
            write(io6,*)'         UTM zone does not match'
            write(io6,*)'         Input file  : ',izonein
            write(io6,*)'         Control file: ',izone
            lerror=.TRUE.
         endif
         if(utmhemin.NE.utmhem)then
            write(io6,*)
            write(io6,*)'RDHEAD:  Problem in input file type: ',dataset
            write(io6,*)'         UTM Hemisphere does not match'
            write(io6,*)'         Input file  : ',utmhemin
            write(io6,*)'         Control file: ',utmhem
            lerror=.TRUE.
         endif
      endif

c --- False Easting/Northing checks
      if(.not.LRSAME(0.0001,feastin,feast))then
         write(io6,*)
         write(io6,*)'RDHEAD:  Problem in input file type: ',dataset
         write(io6,*)'         False Easting does not match'
         write(io6,*)'         Input file  : ',feastin
         write(io6,*)'         Control file: ',feast
         lerror=.TRUE.
      endif
      if(.not.LRSAME(0.0001,fnorthin,fnorth))then
         write(io6,*)
         write(io6,*)'RDHEAD:  Problem in input file type: ',dataset
         write(io6,*)'         False Northing does not match'
         write(io6,*)'         Input file  : ',fnorthin
         write(io6,*)'         Control file: ',fnorth
         lerror=.TRUE.
      endif

c --- Check lat/lon variables
      if(clat0in(1:1).NE.' ') then
        call XTRACTLL(io6,'LAT ',clat0in,reflatin)
        if(.not.LRSAME(0.0001,reflatin,reflat))then
          write(io6,*)
          write(io6,*)'RDHEAD:  Problem in input file type: ',dataset
          write(io6,*)'         REFLAT does not match'
          write(io6,*)'         Input file  : ',reflatin
          write(io6,*)'         Control file: ',reflat
          lerror=.TRUE.
        endif
      endif
      if(clon0in(1:1).NE.' ') then
        call XTRACTLL(io6,'LON ',clon0in,reflonin)
        if(.not.LRSAME(0.0001,reflonin,reflon))then
          write(io6,*)
          write(io6,*)'RDHEAD:  Problem in input file type: ',dataset
          write(io6,*)'         REFLON does not match'
          write(io6,*)'         Input file  : ',reflonin
          write(io6,*)'         Control file: ',reflon
          lerror=.TRUE.
        endif
      endif
      if(clat1in(1:1).NE.' ') then
        call XTRACTLL(io6,'LAT ',clat1in,xlat1in)
        if(.not.LRSAME(0.0001,xlat1in,xlat1))then
          write(io6,*)
          write(io6,*)'RDHEAD:  Problem in input file type: ',dataset
          write(io6,*)'         XLAT1 does not match'
          write(io6,*)'         Input file  : ',xlat1in
          write(io6,*)'         Control file: ',xlat1
          lerror=.TRUE.
        endif
      endif
      if(clat2in(1:1).NE.' ') then
        call XTRACTLL(io6,'LAT ',clat2in,xlat2in)
        if(.not.LRSAME(0.0001,xlat2in,xlat2))then
          write(io6,*)
          write(io6,*)'RDHEAD:  Problem in input file type: ',dataset
          write(io6,*)'         XLAT2 does not match'
          write(io6,*)'         Input file  : ',xlat2in
          write(io6,*)'         Control file: ',xlat2
          lerror=.TRUE.
        endif
      endif

c --- DATUM
      if(datumin.NE.datum)then
         write(io6,*)
         write(io6,*)'RDHEAD:  Problem in input file type: ',dataset
         write(io6,*)'         DATUM does not match'
         write(io6,*)'         Input file  : ',datumin
         write(io6,*)'         Control file: ',datum
         lerror=.TRUE.
      endif

c --- Set/QA Terain data conversions
      if(LTE) then
         if(zunitin.EQ.'M   ') then
            htfac=1.0
         elseif(zunitin.EQ.'KM  ') then
            htfac=1000.
         elseif(zunitin.EQ.'FT  ') then
            htfac=0.3048
         elseif(zunitin.EQ.'YD  ') then
            htfac=0.9144
         else
            write(io6,*)
            write(io6,*)'RDHEAD:  Problem in input file type: ',dataset
            write(io6,*)'         ZUNIT must be M, KM, FT, or YD'
            write(io6,*)'         Input file  : ',zunitin
            lerror=.TRUE.
         endif
         if(EWorder.NE.'W_E ') then
            write(io6,*)
            write(io6,*)'RDHEAD:  Problem in input file type: ',dataset
            write(io6,*)'         Data order within record must be W_E'
            write(io6,*)'         Input file  : ',EWorder
            lerror=.TRUE.
         endif
         if(NSorder.EQ.'S_N ') then
            iflip=0
         elseif(NSorder.EQ.'N_S ') then
            iflip=1
         else
            write(io6,*)
            write(io6,*)'RDHEAD:  Problem in input file type: ',dataset
            write(io6,*)'         Records must be ordered N_S or S_N'
            write(io6,*)'         Input file  : ',NSorder
            lerror=.TRUE.
         endif
      endif

c --- STOP now if error exists in the input file
999   if(LERROR) then
         write(*,*)'ERRORS are found in an input file'
         write(*,*)'Review messages written to the LIST file'
         stop
      endif

      return
      end
c----------------------------------------------------------------------
      subroutine comp
c----------------------------------------------------------------------
c
c --- MAKEGEO   Version: 3.2          Level: 090526              COMP
c ---           J. Scire, D. Strimaitis  Earth Tech, Inc.
c
c --- PURPOSE:  Main computational routine
c
c --- Updates
c     Ver 3.0, Level 090526 from Ver 2.26, Level 041230      ZWU
c              - Add snow data processing
c     Ver 2.26 Level 041230 from Ver 2.25 Level 041013        DGS
c              - Add the IMISS category to a cell only when total
c                fractional LU .LE. FREPLACE (0.001).
c              - Allow valid total fractional LU to be .GE. FLUMIN.
c              - Stop if fractional LU is between 0.001 and FLUMIN.
c              - Add the LU2.DAT fractional land use file.  Used
c                only for empty cells in the primary LU.DAT file
c                (no fractional land use data in LU.DAT).
c              - Add feature to process QA files for identifying
c                missing LU cells (no GEO.DAT produced)
c     Ver 2.25 Level 041013 from Ver 2.24  Level 040920       DGS
c              - Add control file flag for Image GRD format
c                (SURFER 7 or SURFER 8 convention)
c     Ver 2.24 Level 040920 from Ver 2.2  Level 030402        DGS
c              - Enlarge format for QATERR.DAT plot-file to allow
c                terrain elevations below sea level.
c
c     Ver 2.2  Level 030402 from Ver 2.1  Level 011003        DGS
c              - Header records in landuse/terrain input files are 
c                processed in SETUP/READHD (new format:  old format
c                is NOT supported)
c              - Write terain data to GRD file for QA plotting
c
c --- INPUTS:
c       Parameters: IO6, IOMESG, mxcat,mxocat,mxnx,mxny
c
c --- OUTPUT:  none
c
c --- COMP called by:  MAIN
c --- COMP calls:      
c----------------------------------------------------------------------
c
c --- include parameters
      include 'params.geo'

c --- include common blocks
      include 'control.geo'
      include 'grid.geo'
      include 'lucat.geo'
      include 'filnam.geo'
      include 'snow.geo'
      include 'snowref.geo'

      dimension pland(mxnx,mxny,mxcat+1),dum2d(mxnx,mxny)
      dimension pland2(mxcat)
      dimension ipercal(mxnx,mxny)
      real percal(mxnx,mxny)
      real xwork(mxcat)
      real vflu(mxcat)
      real z0(mxnx,mxny),albedo(mxnx,mxny),bowen(mxnx,mxny),
     1     soilcg(mxnx,mxny),qf(mxnx,mxny),xlai(mxnx,mxny),
     2     elev(mxnx,mxny),vf(mxnx,mxny)
      character*1 ccomma,cuamter
      character*70 messag
      character*10 tname1,tname2
      dimension ifile(10),note(60)
      logical ldate
      integer water

      real, allocatable,dimension(:,:,:) :: xsnow
      real, allocatable,dimension(:,:) :: xsnowc
      integer, allocatable,dimension(:,:):: ndsnow
c
      data elev/ncell*0.0/
      data ldate/.false./
      data ccomma/','/

c --- Set threshold for replacing a cell
      data freplace/0.001/

c --- Set data for UAM output option (not active here)
      data cuamter/'N'/
      data tname1/'TERRAIN   '/
      data tname2/'VEG FACTOR'/
      data ifile/10*0/,note/60*0/
      data idate/0/,begtim/0/,jdate/0/,endtim/0/
      data orgx/0./,orgy/0./,xxorg/0./,yyorg/0./,ddelx/0./,dely/0./
      data iizone/0/,nnx/0/,nny/0/,nz/0/,nzlow/0/,nzup/0/
      data zhtsfc/0/,zminlw/0/,zminup/0/,ix/0/,iy/0/,nxcll/0/,nycll/0/

c --- Report progress
      write(iomesg,*)'COMPUTATIONAL PHASE'

      do i=1,524
         blankl=' '
      enddo

c --- Read in the preprocessed terrain data if available
      if(LTERR)then
        if(iflip.EQ.1)then
          do 1305 j = ny,1,-1
            Read(io4,*)(elev(i,j),i = 1,nx)
            do 1304 i = 1,nx
              elev(i,j) = elev(i,j)*htfac
1304        Continue
1305      Continue
        Endif
        if(iflip.EQ.0)then
          do 1307 j = 1,ny
            Read(io4,*)(elev(i,j),i = 1,nx)
            do 1306 i = 1,nx
              elev(i,j) = elev(i,j)*htfac
1306        Continue
1307      Continue
        Endif
      Endif
c
c --- Initialize LU arrays to zero
      do j=1,mxny
         do i=1,mxnx
            ipercal(i,j)=0
            do l=1,mxcat+1
               pland(i,j,l) = 0.
            enddo
         enddo
      enddo
c
c --- Read in the fractional areas for each grid cell
c
      nbad=0
      nfill2=0
      do j = 1,ny
      do i = 1,nx
        read(io2,*) iindex,jindex,(pland(i,j,k),k=1,nincat)

c ---   Check that the CTG LU data cells are read in the right order
        if(i.NE.iindex .OR. j.NE.jindex) then
          write(io6,*) ' ERROR: Index of CTG LU data does not match',
     1                 ' expected: Expected(I,J) = ',i,j,'   CTG(I,J)',
     2                 ' = ', iindex,jindex
          write(*,*) ' ERROR occurs --- See Run LIST file'
          stop
        endif

        if(LLU2) then
c ---      Read corresponding fractional land use for this cell from
c ---      the supplementary LU2.DAT file
           read(io3,*) iindex2,jindex2,(pland2(k),k=1,nincat)
c ---      Check that the LU data cells are read in the right order
           if(i.NE.iindex2 .OR. j.NE.jindex2) then
             write(io6,*) ' ERROR: LU2.DAT cell does not match',
     1                 ' expected: Expected(I,J) = ',i,j,'   LU2(I,J)',
     2                 ' = ', iindex2,jindex2
             write(*,*) ' ERROR occurs --- See Run LIST file'
             stop
           endif
c ---      Check primary data for a blank cell
           flutotal=0.0
           do k = 1,nincat
             flutotal=flutotal+pland(i,j,k)
           enddo
c ---      Replace with supplental LU if missing
           if(flutotal.LE.freplace) then
              nfill2=nfill2+1
              do k = 1,nincat
                pland(i,j,k) = pland2(k)
              enddo
           endif
        endif
c
c ---   Redefine any categories if necessary
c
        do k = 1,nsplit
          sum = 0.
          do kk = 1,nrec(k)
c
c --- Move appropriate fraction from one percent to the other
c
            pland(i,j,irec(k,kk)) = pland(i,j,irec(k,kk)) +
     &      prec(k,kk) * pland(i,j,nnrec(k))
            sum = sum + prec(k,kk)
          end do
c
c --- Subtract the splitting category to reflect new percent
c
          pland(i,j,nnrec(k)) = pland(i,j,nnrec(k)) -
     &                          sum * pland(i,j,nnrec(k))
        end do
c
c ---   Sum total fractional LU into array after last input category
        do k = 1,nincat
          pland(i,j,nincat+1) = pland(i,j,nincat+1) + pland(i,j,k)
        end do

c ---   QA total
        if(pland(i,j,nincat+1).GT.freplace .AND.
     &     pland(i,j,nincat+1).LT.flumin) then
           nbad=nbad+1
        endif
      end do
      end do

C --- Read in Snow cross reference
      if(.NOT. LSNOW) goto 1000

      read(io2,'(a)')buffl

      do j = 1,ny
      do i = 1,nx
         read(io2,'(a)')buffl

         read(buffl,202)ii,jj,ntp
 202     format(2i4,i3)
c         print *,' ii,jj,ntp:',ii,jj,ntp

         if(ii.ne.i .or. jj.ne.j) then
            write(io6,203)ii,i,jj,j
            print 203,ii,i,jj,j
 203        format(' Error: I/J mis-match for snow:',4i5)
            stop
         endif

C        Too many land use type in one snow grids
         if(ntp.gt.mxtype) then
            write(io6,206)ntp,mxtype
            print 206,ntp,mxtype
 206        format(' Error: Landuse type outrange for snow:',2i5)
            stop
         endif

         nland(i,j)=ntp
         nt=12
         do k=1,ntp
            read(buffl(nt:),212)land,np
 212        format(i3,i4)
c            print *,' land,np=',land,np

C           Too many snow grids in one calmet grid
            if(np.gt.mxsng) then
               write(io6,207)np,mxsng,i,j,k
               print 207,np,mxsng,i,j,k
 207           format(' Error: Snow grid outrange:',5i5)
               stop
            endif

            ntype(k,i,j)=land
            ngsnow(k,i,j)=np

            nt=nt+7+1
            do m=1,np
               read(buffl(nt:),208)ijs,rr
 208           format(i9,f6.3)
               idsnow(m,k,i,j)=ijs
               rsnow(m,k,i,j)=rr

               nt=nt+15+1
               if(nt.gt.524) then
                  write(io6,*)' Error: buffl is too long:',nt
                  print *,' Error: buffl is too long:',nt
                  stop
               endif
            enddo
         enddo

      enddo  ! end of i
      enddo  ! end of j

C --- QA snow reference array
      do j=1,ny
         do i=1,nx
            nn=nland(i,j)
            do k=1,nn
               land=ntype(k,i,j)
               np=ngsnow(k,i,j)

               do m=1,np
                  ijs=idsnow(m,k,i,j)
                  rr=rsnow(m,k,i,j)
               enddo
            enddo
         enddo
      enddo

c --- Finish reading LU.DAT ----
 1000 continue

      nadj=0
      nmiss=0
      do 3 i=1,nx
      do 3 j=1,ny
c
c ---   Fill in low land use coverage with replacement LU (usually
c ---   water)
        if(pland(i,j,nincat+1) .LT. freplace) then
          if(LQAMISS) then
c ---       Special treatment if invalid IMISS was assigned for QA
            nmiss = nmiss+1
            do kk = 1,nincat+1
              pland(i,j,kk) = 0.0
            enddo
          else
            pland(i,j,imiss2) = pland(i,j,imiss2)
     &                        + (1.0 - pland(i,j,nincat+1))
            nadj = nadj+1
            pland(i,j,nincat+1)=1.0
          endif
        end if
c
c ---   Keep track of percentages of PRIMARY land use in each cell
c
c ---   First see if water makes up > cfract*100% of the cell
c
        watsum = 0.
        if (numwat .gt. 0) then
          do k = 1,numwat
            do kk = 1,nincat
              if (iwat(k) .eq. incat(kk)) then
                watsum = watsum + pland(i,j,kk)
                goto 65
              end if
            end do
 65         continue
          end do
        end if
        water = 0
c***    if (watsum .gt. 0.5) then
c        if (watsum .gt. cfract) then
        if (watsum .gt. cfract+0.0001) then
          water = 1
        end if

        percal(i,j)=-99.9
        do 32 k=1,nincat
c
c --- Water is only eligible for dominant category if it makes up
c --- more than cfract*100 percent of the cell (water = 1)
c

          water2 = 0
          do kk = 1,numwat
c
c --- Is this a water category?
c
            if (iwat(kk) .eq. incat(k)) then
              water2 = 1
              goto 67
            end if
          end do
 67       continue

          if (water2 .eq. 0 .and. water .eq. 1) goto 32
          if (water2 .eq. 1 .and. water .eq. 0) goto 32
          if(pland(i,j,k) .gt. percal(i,j))then
            percal(i,j)=pland(i,j,k)
            ipercal(i,j)= incat(k)
          endif

32      continue

c ---   Special treatment if invalid IMISS was assigned for QA
        if(pland(i,j,nincat+1) .LT. freplace .AND. LQAMISS) then
          percal(i,j)=0.0
          ipercal(i,j)= imiss
        endif

3     continue
      write(io6,*)
      write(io6,*)
c
c --- Print the percentage of each primary land use type within each
c --- cell
      messag='Fraction of primary LU '

c --- Use the percal(i,j) array already filled above ...
       call out(percal,idum,1,5,ldate,messag,1,1,nx,ny)  

c      do 42 i=1,nx
c      do 42 j=1,ny
c        do k = 1,nincat
c          if (ipercal(i,j) .eq. incat(k)) then
c            kk = k
c            goto 69
c          end if
c        end do
c 69     continue
c        dum2d(i,j)=pland(i,j,kk)
c42    continue
c      call out(dum2d,idum,1,5,ldate,messag,1,1,nx,ny)

c --- Print the dominant land use types
      messag='Dominant land use type in each cell before mapping'
      call out(orgx,ipercal,2,3,ldate,messag,1,1,nx,ny)      
c
c --- Perform mapping of land use types from input to output
c
      do i = 1, nx
      do j = 1, ny
        do k = 1,nincat
          if (ipercal(i,j) .eq. incat(k)) then
            ipercal(i,j) = mapcat(k)
            goto 68
          end if
        end do
 68     continue
      end do
      end do
c
c --- Print the dominant land use types
      messag='Dominant land use type in each cell after mapping'
      call out(orgx,ipercal,2,3,ldate,messag,1,1,nx,ny)
c
c --- Print the total percentage of each cell with LU data
      messag='Fraction of each cell with LU data in the data base'
      do 34 i=1,nx
      do 34 j=1,ny
        dum2d(i,j)=pland(i,j,nincat+1)
34    continue
      call out(dum2d,idum,1,5,ldate,messag,1,1,nx,ny)

c --- Write messages about incomplete cell coverage

      if(nfill2.GT.0) then
         write(io6,'(/,a,i10)')
     &    'Total number of cells replaced using LU2.DAT file: ',nfill2
         write(*,'(2x,a,i10)')
     &    'Total number of cells replaced using LU2.DAT file: ',nfill2
      endif

      if(nadj.GT.0) then
         write(io6,'(/,a,f6.3,a,i10,a)')
     1        'Warning -- LU coverage is less than ',freplace,' in ',
     2        nadj,' cell(s)'
         write(io6,'(a,i4)')
     &        'Missing LU fraction filled with LU =',imiss
         write(*,'(2x,a,f6.3,a,i10,a)')
     1        'Warning -- LU coverage is less than ',freplace,
     2        ' in ',nadj,' cell(s)'
         write(*,'(2x,a,i4)')
     &        'Missing LU fraction filled with LU =',imiss
      endif

      if(nmiss.GT.0) then
         write(io6,'(/,a,f6.3,a,i10,a)')
     1        'Warning -- LU coverage is less than ',freplace,' in ',
     2        nmiss,' cell(s)'
         write(io6,'(a,i4)')
     &        'LU for cell set to ',imiss
         write(*,*)
         write(*,'(2x,a,f6.3,a,i10,a)')
     1        'Warning -- LU coverage is less than ',freplace,
     2        ' in ',nmiss,' cell(s)'
         write(*,'(2x,a,i4)')
     &        'LU for cell set to ',imiss
         write(*,*)
      endif

      if(nbad.GT.0) then
         write(io6,'(/,a,f6.3,a,f6.3,a,i10,a)')
     1        'FATAL -- LU coverage is between ',freplace,' and ',
     2         flumin,' in ',nbad,' cell(s)'
         write(io6,'(a)')'Check for problems in LU processing'
         write(io6,'(a)')'No GEO.DAT file is created'
         write(io6,'(a)')'HALTED in COMP'

         write(*,'(2x,a,f6.3,a,f6.3,a,i10,a)')
     1        'FATAL -- LU coverage is between ',freplace,' and ',
     2         flumin,' in ',nbad,' cell(s)'
         write(*,'(2x,a)')'Check for problems in LU processing'
         write(*,'(2x,a)')'No GEO.DAT file is created'
         write(*,'(2x,a)')'HALTED in COMP'

c ---    Remove output file and terminate
         close(io7,status='delete')
         stop

      endif
c
c -------------------------------------------------------
c --- Write LU and ELEV fields to the GRD files (for map)
c -------------------------------------------------------
c
c --- Coordinates of upper-right corner of domain
      xur=xorg+nx*delx
      yur=yorg+ny*delx
c --- Coordinates of cell centers of the corners
      dxby2=0.5*delx
      x1=xorg+dxby2
      y1=yorg+dxby2
      xnx=xur-dxby2
      yny=yur-dxby2

c --- Process landuse [GRD format]
c --------------------------------
      if(image.EQ.0) then
c ---    Place data points at cell centers (SURFER 8 Image)
         xlo=x1
         xhi=xnx
         ylo=y1
         yhi=yny
      elseif(image.EQ.1) then
c ---    Identify data with entire cell  (SURFER 7 Image)
         xlo=xorg
         xhi=xur
         ylo=yorg
         yhi=yur
      else
         write(*,*)'HALTED in COMP:  Image format = 0,1'
         write(*,*)'Found Image = ',image
         stop
      endif

c --- Set min/max landuse to 10 - 100 range
      lmin=10
      lmax=100
c --- Header records
      write(io8,'(a4)') 'DSAA'
      write(io8,'(2i12)') nx,ny
      write(io8,'(2f12.4)') xlo,xhi
      write(io8,'(2f12.4)') ylo,yhi
      write(io8,'(2i12)') lmin,lmax
c --- Data, in rows of constant Y
      do j=1,ny
        write(io8,'(10000(i3,2x))')(IABS(ipercal(i,j)),i=1,nx)
      enddo
      close(io8)
c
c --- Process terrain elevations [GRD format]
c -------------------------------------------
c --- Obtain min/max elevations
      emin=elev(1,1)
      emax=elev(1,1)
      do j=1,ny
         do i=1,nx
            if(elev(i,j).GT.emax) emax=elev(i,j)
            if(elev(i,j).LT.emin) emin=elev(i,j)
         enddo
      enddo
      if(emax.EQ.emin) then
         close(io9, status='DELETE')
      else
c ---    Header records
c ---    Place data points at cell centers for contouring
         write(io9,'(a4)') 'DSAA'
         write(io9,'(2i12)') nx,ny
         write(io9,'(2f12.4)') x1,xnx
         write(io9,'(2f12.4)') y1,yny
         write(io9,'(2e12.4)') emin,emax
c ---    Data, in rows of constant Y
         do j=1,ny
           write(io9,'(10000(1pe11.4,1x))') (elev(i,j),i=1,nx)
         enddo
         close(io9)
      endif
c
c --- Write the default colors to the CLR file (for map)
c
      write(io53,'(a)') 'ColorMap 1 1'
      write(io53,'(a)') '    0.000000 255 255   0'
      write(io53,'(a)') '   11.000000 255 255   0'
      write(io53,'(a)') '   11.000000 204 255 102'
      write(io53,'(a)') '   22.100000 204 255 102'
      write(io53,'(a)') '   22.100000 160 255 160'
      write(io53,'(a)') '   33.100000 160 255 160'
      write(io53,'(a)') '   33.100000  80 255  80'
      write(io53,'(a)') '   44.200000  80 255  80'
      write(io53,'(a)') '   44.200000 153 255 255'
      write(io53,'(a)') '   50.000000 153 255 255'
      write(io53,'(a)') '   50.000000  60 204 255'
      write(io53,'(a)') '   55.400000  60 204 255'
      write(io53,'(a)') '   55.400000 204 153 204'
      write(io53,'(a)') '   66.500000 204 153 204'
      write(io53,'(a)') '   66.500000 255 204 153'
      write(io53,'(a)') '   77.600000 255 204 153'
      write(io53,'(a)') '   77.600000 255 255 204'
      write(io53,'(a)') '   88.700000 255 255 204'
      write(io53,'(a)') '   88.700000 255 255 255'
      write(io53,'(a)') '   99.029557 255 255 255'
      write(io53,'(a)') '   99.029557 255   0   0'
      write(io53,'(a)') '  100.000000 255   0   0'
      close(io53)

      if(LQAMISS) then
c ---    Completed QA processing
         write(io6,*)
         write(io6,*)' NOTE --- IMISS is not an input LU category '
         write(io6,*)' NO GEO.DAT file is created'
         write(io6,*)' Existing GEO.DAT file is deleted'
         write(io6,*)' Review list file and QALUSE.GRD file'
c ---    Remove output file and return
         close(io7,status='delete')
         return
      endif

c ---------------------------------------------
c --- Compute the GEO.DAT parameters for CALMET
c ---------------------------------------------
c
c --- Print the percentage of those land use types needed for
c --- calculation of GEO.DAT parameters
c
c --- Compute area-weighted values of each GEO.DAT parameter
c      print *, ' call z0'
      call wt(pland,z0lu,  xwork,1,nx,ny,nincat,z0)
c     print *, ' call alb'
      call wt(pland,alblu, xwork,0,nx,ny,nincat,albedo)
c     print *, ' call bow'
      call wt(pland,bowlu, xwork,0,nx,ny,nincat,bowen)
c     print *, ' call sil'
      call wt(pland,soillu,xwork,0,nx,ny,nincat,soilcg)
c     print *, ' call qf'
      call wt(pland,qflu,  xwork,0,nx,ny,nincat,qf)
c     print *, ' call la'
      call wt(pland,xlailu,xwork,0,nx,ny,nincat,xlai)
c     print *, ' call vflu'
      call wt(pland,vflu,xwork,0,nx,ny,nincat,vf)
c     print *,'Finish weighting calculation!'
c
      if(cuamter.EQ.'Y' .OR. cuamter.EQ.'y') then
c...    Write UAM terrain file
        write(io52)ifile,note,1,25,idate,begtim,jdate,endtim
        write(io52)orgx,orgy,iizone,xxorg,yyorg,ddelx,dely,nnx,nny,
     *         nz,nzlow,nzup,zhtsfc,zminlw,zminup
        write(io52)ix,iy,nxcll,nycll
        write(io52)1,tname1,((z0(ii,jj),ii=1,nx),jj=1,ny)
        write(io52)1,tname2,((vf(ii,jj),ii=1,nx),jj=1,ny)
      endif

c --- Write the QA check at the user-specified grid cell
      if(LQACELL)then
        write(io6,989) nlx,nly
989     format(//,'Contents of QA check cell - (I,J) = (',i3,',',i3,')')
        write(io6,1900)ipercal(nlx,nly),' - dominant land use class    '
        write(io6,1902)   elev(nlx,nly),' - terrain height (m)         '
        write(io6,1901)     z0(nlx,nly),' - roughness length (m)       '
        write(io6,1901)     vf(nlx,nly),' - UAM vegfactor              '
        write(io6,1901) albedo(nlx,nly),' - albedo (fraction)          '
        write(io6,1901)  bowen(nlx,nly),' - bowen ratio                '
        write(io6,1901) soilcg(nlx,nly),' - soil heat flux             '
        write(io6,1901)     qf(nlx,nly),' - anthropogenic heat flux    '
        write(io6,1901)   xlai(nlx,nly),' - leaf area index            '
1900    format(1x,i6,a30)
1901    format(1x,f6.3,a30)
1902    format(1x,f6.1,a30)
      endif

      if(LSNOW) goto 2000

c --- Part I: Without Snow Processing ----------------

c --- Write the gridded GEO.DAT fields
c --- Write the land use data
      write(io7,1222)
1222  format(1x,'1          - LAND USE DATA - (1=new categories)')
      write(io7,1224)noutcat,iwat1,iwat2
1224  format(1x,3i4,' - NLU, IWAT1, IWAT2')
      write(io7,1226)(outcat(n),n=1,noutcat)
1226  format(1x,100(1x,i3),'  - new LU categories')
      nxm1=nx-1
      do 1230 j=ny,1,-1
        write(io7,1228)(ipercal(i,j),ccomma,i=1,nxm1),ipercal(nx,j)
c1228    format(100(5x,i3,a1))
1228    format(10(4x,i3,a1))
1230  continue

      messag='TERRAIN heights - HTFAC (Conversion to meters)'

c --- If gridded terrain data is not read in, default conversion is 1.0
      if(.not.LTERR) htfac = 1.0
      call wredat(elev,nx,ny,1,htfac,messag)
c
      messag='gridded z0 field'
      call wrrdat(z0,nx,ny,2,messag)
c
      messag='gridded albedo field'
      call wrrdat(albedo,nx,ny,1,messag)
c
      messag='gridded Bowen ratio field'
      call wrrdat(bowen,nx,ny,1,messag)
c
      messag='gridded soil heat flux parameters'
      call wrrdat(soilcg,nx,ny,1,messag)
c
      messag='gridded anthropogenic heat flux field'
      call wrrdat(qf,nx,ny,1,messag)
c
      messag='gridded leaf area index field'
      call wrrdat(xlai,nx,ny,1,messag)
c
C --- Complete no-snow processing
      return

 2000 continue
c --- Part II: With Snow Processing ----------------
      if( .NOT. LSNOW) then
         print *,' Error: Snow processing only:',LSNOW
         stop
      endif

C --- Read snow file header
      open(isnw,file=snowdat,status='old',action='read')
      call rdsnhd

      allocate(xsnow(nxp,nyp,nwdt))
      allocate(xsnowc(nxp,nyp),ndsnow(nxp,nyp))

C --- Setup time stamp
      idateb=idatebeg*100
      idatee=idateend*100

      call gethours(idateb,idatee,nhrtot)
      ndtot=nhrtot/24
      if(mod(nhrtot,24).ne.0) ndtot=ndtot+1
      print *,' Total days: ',ndtot

      call getdate(idateb,iy,im,id,ih)
      call chgtim(iy,im,id,ih,24)
      call timestmp(iy,im,id,ih,idatenext)

      xsnow=fmiss
      ndsnow=ndsnowb

C --- Loop over all datas
      idatgeo=idateb
      nt=index(geodat,'.',back=.TRUE.)-1
      nl=len_trim(geodat)

      iloop=1

 3000 continue

      nn=idatgeo/100
      write(fsgeo,310)geodat(1:nt),nn,geodat(nt+1:nl)
 310  format(a,i8.8,a)

      print *,' Create GEODAT file:  ',trim(fsgeo)

      open(io7,file=fsgeo, status='unknown')
      call WRTHEAD

C --- Read snow data record
      call rdsn(xsnow,ndsnow,xsnowc)

C --- Process GEO.DAT for one day ------

c --- Compute area-weighted values of each GEO.DAT parameter
c      print *, ' call z0'
      if(msrl.eq.1) then
         call wtswz0(pland,z0lu,xwork,1,nx,ny,nincat,z0,xsnowc)
      elseif(msrl.eq.2) then
         call wtsw(pland,z0lu,xwork,1,nx,ny,nincat,z0,xsnowc,z0lus)
      else
         write(io6,*)' Error: Illegal MSRL:',msrl
         write(*,*)' Error: Illegal MSRL:',msrl
         stop
      endif

c     print *, ' call alb'
      if(msal.eq.1) then
         call wtswalb(pland,alblu,xwork,0,nx,ny,nincat,albedo,ndsnow
     &        ,alblus,xsnowc)
      elseif(msal.eq.2) then
         call wtsw(pland,alblu,xwork,0,nx,ny,nincat,albedo,ndsnow
     &        ,alblus)
      else
         write(io6,*)' Error: Illegal MSAL:',msal
         write(*,*)' Error: Illegal MSAL:',msal
         stop
      endif
c     print *, ' call bow'
      call wtsw(pland,bowlu,xwork,0,nx,ny,nincat,bowen,xsnowc,bowlus)
c     print *, ' call sil'
      call wtsw(pland,soillu,xwork,0,nx,ny,nincat,soilcg,xsnowc,soillus)
c     print *, ' call qf'
      call wtsw(pland,qflu,xwork,0,nx,ny,nincat,qf,xsnowc,qflus)
c     print *, ' call la'
      call wtsw(pland,xlailu,xwork,0,nx,ny,nincat,xlai,xsnowc,xlailus)

c     print *,'Finish weighting calculation with snow info!'

c --- Write the gridded GEO.DAT fields
c --- Write the land use data
      write(io7,1222)
      write(io7,1224)noutcat,iwat1,iwat2
      write(io7,1226)(outcat(n),n=1,noutcat)
      nxm1=nx-1
      do j=ny,1,-1
        write(io7,1228)(ipercal(i,j),ccomma,i=1,nxm1),ipercal(nx,j)
      enddo

c --- If gridded terrain data is not read in, default conversion is 1.0
      messag='TERRAIN heights - HTFAC (Conversion to meters)'
      if(.not.LTERR) htfac = 1.0
      call wredat(elev,nx,ny,1,htfac,messag)
c
      messag='gridded z0 field'
      call wrrdat(z0,nx,ny,2,messag)
c
      messag='gridded albedo field'
      call wrrdat(albedo,nx,ny,1,messag)
c
      messag='gridded Bowen ratio field'
      call wrrdat(bowen,nx,ny,1,messag)
c
      messag='gridded soil heat flux parameters'
      call wrrdat(soilcg,nx,ny,1,messag)
c
      messag='gridded anthropogenic heat flux field'
      call wrrdat(qf,nx,ny,1,messag)
c
      messag='gridded leaf area index field'
      call wrrdat(xlai,nx,ny,1,messag)

      close(io7)

C --- Finish geo.dat for one day 
c     Nextday snow date      
      iloop=iloop+1
      call getdate(idatc,iy,im,id,ih)
      call chgtim(iy,im,id,ih,24)
      call timestmp(iy,im,id,ih,idatenext)

c     Nextday geo date      
      call getdate(idatgeo,iy,im,id,ih)
      call chgtim(iy,im,id,ih,24)
      call timestmp(iy,im,id,ih,idatgeo)

      if(iloop.le.ndtot) then
         call rollbk(xsnow,nxp,nyp,nwdt,fmiss)
         goto 3000
      endif

      deallocate(xsnow,xsnowc,ndsnow)

      return
      end
c----------------------------------------------------------------------
      subroutine wt(xlupcnt,xlutab,xref,ilog,nx,ny,nlu,xdata)
c----------------------------------------------------------------------
c
c --- MAKEGEO  Version: 3.2     Level: 960215                      WT
c ---          J. Scire, SRC
c
c --- PURPOSE:  Compute area-weighted values of gridded land use
c               parameters using either arithmetic or log weights
c
c --- INPUTS:
c  XLUPCNT(nx,ny,nlu+1) - real array  - Percentage of land use type in
c                                       each cell
c           XLUTAB(nlu) - real array  - Value of parameter (e.g., z0,
c                                       LAI, etc.) for each land use
c             XREF(nlu) - real array  - Work array dimensioned NLU
c                  ILOG - integer     - Weighting type (0=arithmetic
c                                       weighting, 1=log weighting)
c                    NX - integer     - No. grid cells in X direction
c                    NY - integer     - No. grid cells in Y direction
c                   NLU - integer     - No. land use categories
c
c --- OUTPUT:
c          XDATA(nx,ny) - real array  - Gridded parameter values
c                                       weighted by land use area
c                                       within each cell
c
c       Parameters: MXNX, MXNY, MXCAT, IO6
c
c --- WT called by:  MAIN
c --- WT calls:      none
c----------------------------------------------------------------------
c
c --- Set parameters
      include 'params.geo'

      real xlupcnt(mxnx,mxny,mxcat+1)
      real xlutab(mxcat),xref(mxcat)
      real xdata(mxnx,mxny)
c
c --- Arithmetic or log weights
      if(ilog.eq.0)then
c
c ---    Arithmetic weighting factors
         do 10 i=1,nlu
         xref(i)=xlutab(i)
10       continue
      else if(ilog.eq.1)then
c
c ---    Logarithmic weighting factors
         do 20 i=1,nlu
         xref(i)=alog(xlutab(i))
20       continue
      else
c
c ---    Invalid value of ILOG passed into subr.
         write(io6,*)'Error in subr. WT -- Invalid value of ILOG -- ',
     1   'ILOG = ',ilog
         write(*,*) ' ERROR in SUBR. WT -- See Run LIST file'
         stop
      endif
c
c --- Compute area-weighted gridded values
      do 50 i=1,nx
      do 50 j=1,ny
c
         xnum=0.0
         xden=0.0
c
         do 40 k=1,nlu
            xnum=xnum+xlupcnt(i,j,k)*xref(k)
            xden=xden+xlupcnt(i,j,k)
40       continue
c
         if(xden.eq.0.0)then
            write(io6,*)'Error in subr. WT -- XDEN = 0.0 -- I = ',i,
     1      ' J = ',j
            write(*,*) ' ERROR in SUBR. WT -- See Run LIST file'
            stop
         else
            xdata(i,j)=xnum/xden
         endif
c
50    continue
c
c --- Adjust back from log if using log weighting factors
      if(ilog.eq.1)then
         do 60 i=1,nx
         do 60 j=1,ny
            xdata(i,j)=exp(xdata(i,j))
60       continue
      endif
c
      return
      end

c----------------------------------------------------------------------
      subroutine wtsw(xlupcnt,xlutab,xref,ilog,nx,ny,nlu,xdata
     &               ,xsnowc,xlutabs)
c----------------------------------------------------------------------
c
c --- MAKEGEO  Version: 3.2     Level: 090526                     WTSW
c ---          J. Scire, SRC
c
c --- PURPOSE:  Compute area-weighted values of gridded land use
c               parameters using either arithmetic or log weights,
c               and consider snow coverage for individul landuse
c               type.
c
c --- INPUTS:
c  XLUPCNT(nx,ny,nlu+1) - real array  - Percentage of land use type in
c                                       each cell
c           XLUTAB(nlu) - real array  - Value of parameter (e.g., z0,
c                                       LAI, etc.) for each land use
c             XREF(nlu) - real array  - Work array dimensioned NLU
c                  ILOG - integer     - Weighting type (0=arithmetic
c                                       weighting, 1=log weighting)
c                    NX - integer     - No. grid cells in X direction
c                    NY - integer     - No. grid cells in Y direction
c                   NLU - integer     - No. land use categories

c          XLUTABS(nlu) - real array  - Same as XLUTAB but in snow covered 
c                                       land use
c        XSNOWC(nxp,nyp) - real array  - Daily snow depth
c                   NXP - integer     - Snow No. grid cells in X direction
c                   NYP - integer     - Snow No. grid cells in Y direction
c
c --- OUTPUT:
c          XDATA(nx,ny) - real array  - Gridded parameter values
c                                       weighted by land use area
c                                       within each cell
c
c       Parameters: MXNX, MXNY, MXCAT, IO6
c
c --- WT called by:  MAIN
c --- WT calls:      none
c----------------------------------------------------------------------
c
c --- Set parameters
      include 'params.geo'
      include 'snow.geo'
      include 'snowref.geo'

c --- No snow data
      real xlupcnt(mxnx,mxny,mxcat+1)
      real xlutab(mxcat),xref(mxcat)
c --- Snow data
      real xsnowc(nxp,nyp)
      real xlutabs(mxcat)
C --- Output
      real xdata(mxnx,mxny)
c     XREF2(nlu) - real array  - SNOW Work array dimensioned NLU
      dimension xref2(nlu)

c --- Arithmetic or log weights
      if(ilog.eq.0)then
c
c ---    Arithmetic weighting factors
         do 10 i=1,nlu
            xref(i)=xlutab(i)
            xref2(i)=xlutabs(i)
 10      continue
      else if(ilog.eq.1)then
c
c ---    Logarithmic weighting factors
         do 20 i=1,nlu
            xref(i)=alog(xlutab(i))
            xref2(i)=alog(xlutabs(i))
 20      continue
      else
c
c ---    Invalid value of ILOG passed into subr.
         write(io6,*)'Error in subr. WTSW - Invalid value of ILOG -',
     1   'ILOG = ',ilog
         write(*,*) ' ERROR in SUBR. WTSW - See Run LIST file'
         stop
      endif

c --- Compute area-weighted gridded values
      do 50 i=1,nx
      do 50 j=1,ny

c         xnum=0.0
c         xden=0.0

c         do 40 k=1,nlu
c            xnum=xnum+xlupcnt(i,j,k)*xref(k)
c            xden=xden+xlupcnt(i,j,k)
c40       continue

C ---    Snow count ----
         xnum=0.0
         xden=0.0

         nn=nland(i,j)
         do k=1,nn
            land=ntype(k,i,j)
            np=ngsnow(k,i,j)
            do m=1,np
               ijs=idsnow(m,k,i,j)
               rr=rsnow(m,k,i,j)
               dd=dd+rr

               js1=(ijs-1)/nxt+1
               is1=ijs-(js1-1)*nxt
               
               is=is1-isoff
               js=js1-jsoff

               if(is.lt.1 .or. is.gt.nxp) then
                  write(io6,410)is,nxp,isoff
                  print 410,is,nxp,isoff
 410              format(' Error: Snow grid out-range in I:',3i6)
                  stop
               endif

               if(js.lt.1 .or. js.gt.nyp) then
                  write(io6,411)js,nyp,jsoff
                  print 410,js,nyp,jsoff
 411              format(' Error: Snow grid out-range in J:',3i6)
                  stop
               endif

               sdp=xsnowc(is,js)

               if(sdp.gt.0) then
                  xf=xref2(land)
               else
                  xf=xref(land)
               endif

               xnum=xnum+rr*xf
               xden=xden+rr

            enddo   ! end of m
         enddo   ! end of k

         if(xden.eq.0.0)then
            write(io6,*)'Error in subr. WT -- XDEN = 0.0 -- I = ',i,
     1      ' J = ',j
            write(*,*) ' ERROR in SUBR. WT -- See Run LIST file'
            stop
         else
            xdata(i,j)=xnum/xden
         endif

 50   continue

c --- Adjust back from log if using log weighting factors
      if(ilog.eq.1)then
         do 60 i=1,nx
         do 60 j=1,ny
            xdata(i,j)=exp(xdata(i,j))
60       continue
      endif

      return
      end

c----------------------------------------------------------------------
      subroutine wtswz0(xlupcnt,xlutab,xref,ilog,nx,ny,nlu,xdata
     &               ,xsnowc)
c----------------------------------------------------------------------
c
c --- MAKEGEO  Version: 3.2     Level: 090526                     WTSW
c ---          J. Scire, SRC
c
c --- PURPOSE:  Compute area-weighted z0 values of gridded land use
c               parameters using log weights, and considering snow 
c               coverage for individul landuse type.

c     Note: z0 only, do not use for other property values
c
c --- INPUTS:
c  XLUPCNT(nx,ny,nlu+1) - real array  - Percentage of land use type in
c                                       each cell
c           XLUTAB(nlu) - real array  - Value of parameter (e.g., z0,
c                                       LAI, etc.) for each land use
c             XREF(nlu) - real array  - Work array dimensioned NLU
c                  ILOG - integer     - Weighting type (0=arithmetic
c                                       weighting, 1=log weighting)
c                    NX - integer     - No. grid cells in X direction
c                    NY - integer     - No. grid cells in Y direction
c                   NLU - integer     - No. land use categories

c          XLUTABS(nlu) - real array  - Same as XLUTAB but in snow covered 
c                                       land use
c        XSNOWC(nxp,nyp) - real array  - Daily snow depth
c                   NXP - integer     - Snow No. grid cells in X direction
c                   NYP - integer     - Snow No. grid cells in Y direction
c
c --- OUTPUT:
c          XDATA(nx,ny) - real array  - Gridded parameter values
c                                       weighted by land use area
c                                       within each cell
c
c       Parameters: MXNX, MXNY, MXCAT, IO6
c
c --- WT called by:  MAIN
c --- WT calls:      none
c----------------------------------------------------------------------
c
c --- Set parameters
      include 'params.geo'
      include 'snow.geo'
      include 'snowref.geo'

c --- No snow data
      real xlupcnt(mxnx,mxny,mxcat+1)
      real xlutab(mxcat),xref(mxcat)
c --- Snow data
      real xsnowc(nxp,nyp)
C --- Output 
      real xdata(mxnx,mxny)

c --- Locals
      dimension hz0(nlu)

c --- Arithmetic or log weights
      if(ilog.ne.1) then
         write(io6,*)' Error: WTSWZ0 only uses log weights:',ilog
         print *,' Error: WTSWZ0 only uses log weights:',ilog
         stop
      endif

c --- Recove obstacle height for no-snow landuse
      do i=1,nlu
         zs=xlutab(i)
         hz0(i)=zs*hscl
      enddo
      hzmin=hz0(nlu)  !ice

c --  Logarithmic weighting factors
      do i=1,nlu
         xref(i)=alog(xlutab(i))
      enddo

c --- Compute area-weighted gridded values
      do 50 i=1,nx
      do 50 j=1,ny

C ---    Snow count ----
         xnum=0.0
         xden=0.0

         nn=nland(i,j)
         do k=1,nn
            land=ntype(k,i,j)
            np=ngsnow(k,i,j)
            do m=1,np
               ijs=idsnow(m,k,i,j)
               rr=rsnow(m,k,i,j)
               dd=dd+rr

               js1=(ijs-1)/nxt+1
               is1=ijs-(js1-1)*nxt
               
               is=is1-isoff
               js=js1-jsoff

               if(is.lt.1 .or. is.gt.nxp) then
                  write(io6,410)is,nxp,isoff
                  print 410,is,nxp,isoff
 410              format(' Error: Snow grid out-range in I:',3i6)
                  stop
               endif

               if(js.lt.1 .or. js.gt.nyp) then
                  write(io6,411)js,nyp,jsoff
                  print 410,js,nyp,jsoff
 411              format(' Error: Snow grid out-range in J:',3i6)
                  stop
               endif

               sdp=xsnowc(is,js)

               if(sdp.lt.sdpmin) then
                  xf=xref(land)
               else
                  hh=hz0(land)-sdp
                  hh=max(hh,hzmin)
                  hz=hh/hscl
                  xf=alog(hz)
               endif

               xnum=xnum+rr*xf
               xden=xden+rr

            enddo   ! end of m
         enddo   ! end of k

         if(xden.eq.0.0)then
            write(io6,*)'Error in subr. WT -- XDEN = 0.0 -- I = ',i,
     1      ' J = ',j
            write(*,*) ' ERROR in SUBR. WT -- See Run LIST file'
            stop
         else
            xdata(i,j)=xnum/xden
         endif

 50   continue

c --- Adjust back from log if using log weighting factors
      if(ilog.eq.1)then
         do 60 i=1,nx
         do 60 j=1,ny
            xdata(i,j)=exp(xdata(i,j))
60       continue
      endif

      return
      end

c----------------------------------------------------------------------
      subroutine wtswalb(xlupcnt,xlutab,xref,ilog,nx,ny,nlu,xdata
     &               ,ndsnow,xlutabs,xsnowc)
c----------------------------------------------------------------------
c
c --- MAKEGEO  Version: 3.2     Level: 090714                     WTSW
c ---          J. Scire, SRC
c
c --- PURPOSE:  Compute area-weighted albedo values of gridded land use
c               parameters using arithmetic weights, and consider snow 
c               ages of individul landuse type.

c     Note: Albedo only, do not use for other property values
c
c --- INPUTS:
c  XLUPCNT(nx,ny,nlu+1) - real array  - Percentage of land use type in
c                                       each cell
c           XLUTAB(nlu) - real array  - Value of parameter (e.g., z0,
c                                       LAI, etc.) for each land use
c             XREF(nlu) - real array  - Work array dimensioned NLU
c                  ILOG - integer     - Weighting type (0=arithmetic
c                                       weighting, 1=log weighting)
c                    NX - integer     - No. grid cells in X direction
c                    NY - integer     - No. grid cells in Y direction
c                   NLU - integer     - No. land use categories

c          XLUTABS(nlu) - real array  - Same as XLUTAB but in snow covered 
c                                       land use
c        NDSNOW(nxp,nyp) - integer array - Snow history in days
c        XSNOWC(nxp,nyp) - real array - Daily snow depth
c                   NXP - integer     - Snow No. grid cells in X direction
c                   NYP - integer     - Snow No. grid cells in Y direction
c
c --- OUTPUT:
c          XDATA(nx,ny) - real array  - Gridded parameter values
c                                       weighted by land use area
c                                       within each cell
c
c       Parameters: MXNX, MXNY, MXCAT, IO6
c
c --- WT called by:  MAIN
c --- WT calls:      none
c----------------------------------------------------------------------
c
c --- Set parameters
      include 'params.geo'
      include 'snow.geo'
      include 'snowref.geo'

c --- No snow data
      real xlupcnt(mxnx,mxny,mxcat+1)
      real xlutab(mxcat),xref(mxcat)
c --- Snow Data: Snow history (in days), Snow depth (in meter)
      integer ndsnow(nxp,nyp)
      real xsnowc(nxp,nyp)

      real xlutabs(mxcat),xref2(mxcat)

C --- Output
      real xdata(mxnx,mxny)

C --- Locals
      real rdcy(mxcat)

c --- Arithmetic or log weights
      if(ilog.ne.0) then
         write(io6,*)' Error: WTSWZ0 only uses log weights:',ilog
         print *,' Error: WTSWZ0 only uses log weights:',ilog
         stop
      endif

      if(nlu.gt.mxcat) then
         write(io6,*)' Error: LandUse Type mis-match:',mxcat,nlu
         print *,' Error: LandUse Type mis-match:',mxcat,nlu
         stop
      endif

      do i=1,nlu
         xref(i)=xlutab(i)   ! without snow
         xref2(i)=xlutabs(i) ! with snow
         dd=xref2(i)-xref(i)
         rdcy(i)=dd*ralb(i)  
      enddo

c --- Compute area-weighted gridded values
      do 50 i=1,nx
      do 50 j=1,ny

C ---    Snow count ----
         xnum=0.0
         xden=0.0

         nn=nland(i,j)
         do k=1,nn
            land=ntype(k,i,j)
            np=ngsnow(k,i,j)
            do m=1,np
               ijs=idsnow(m,k,i,j)
               rr=rsnow(m,k,i,j)
               dd=dd+rr

               js1=(ijs-1)/nxt+1
               is1=ijs-(js1-1)*nxt
               
               is=is1-isoff
               js=js1-jsoff

               if(is.lt.1 .or. is.gt.nxp) then
                  write(io6,410)is,nxp,isoff
                  print 410,is,nxp,isoff
 410              format(' Error: Snow grid out-range in I:',3i6)
                  stop
               endif

               if(js.lt.1 .or. js.gt.nyp) then
                  write(io6,411)js,nyp,jsoff
                  print 410,js,nyp,jsoff
 411              format(' Error: Snow grid out-range in J:',3i6)
                  stop
               endif

               sdp=xsnowc(is,js)
               nd=ndsnow(is,js)
               nds=nages(land)
               rdc=rdcy(land)
               
               if(nd.gt.nds) nd=nds
               age=float(nd)/nds

               if(sdp.lt.sdpmin) then
                  xf=xref(land)
               else
                  ww=1.0-rdc*age
                  xf=xref2(land)*ww
               endif

               xnum=xnum+rr*xf
               xden=xden+rr

            enddo   ! end of m
         enddo   ! end of k

         if(xden.eq.0.0)then
            write(io6,*)'Error in subr. WT -- XDEN = 0.0 -- I = ',i,
     1      ' J = ',j
            write(*,*) ' ERROR in SUBR. WT -- See Run LIST file'
            stop
         else
            xdata(i,j)=xnum/xden
         endif

 50   continue

      return
      end

c----------------------------------------------------------------------
      subroutine xtrctx(ctxtin,ctxtout)
c----------------------------------------------------------------------
c
c --- MAKEGEO  Version: 3.2     Level: 060309                  XTRCTX
c ---          J. Scire, Earth Tech, Inc.
c
c --- PURPOSE: Extract from a character string the text starting
c              with the first non-blank character up to the
c              last non-blank character before next blank
c              i.e., strip off leading blanks & stop at first
c              blank space after the text has started.
c              ("   filenam.dat   " is extracted as "filename.dat")
c
c --- Updates
c     Ver 2.27 Level 060309 from Ver 1.1 Level 010206        DGS
c            - Filenames from c*70 to c*132 for CALUTILS V2.3 and later
c
c --- INPUTS:
c               CTXTIN - char*132    - Input text (with blanks)
c
c --- OUTPUT:
c               CTXTOUT- char*132    - Output text (no blanks)
c
c --- XTRCTX called by:  MAIN
c --- XTRCTX calls:      none
c----------------------------------------------------------------------
c
      character*132 ctxtin,ctxtout
c
c --- Extract filename from 132 character string
      do i=1,132
        if(ctxtin(i:i).NE.' ')then
          ibeg=i
          go to 11
        endif
      enddo
c --- If all characters are blank, return a blank
      ibeg=132
11    continue
c
      do i=ibeg,132
        if(ctxtin(i:i).EQ.' ')then
          iend=i
          go to 12
        endif
      enddo
      iend=132
12    continue
c
c --- Transfer text between ibeg and iend to output variable
      read(ctxtin(ibeg:iend),'(a)')ctxtout
c
      return
      end
c----------------------------------------------------------------------
      subroutine wrrdat(xdata,nx,ny,iform,messag)
c----------------------------------------------------------------------
c
c --- MAKEGEO  Version: 3.2     Level: 960215                  WRRDAT
c ---          J. Scire, SRC
c
c --- PURPOSE:  Write real gridded data in the GEO.DAT format used
c               by CALMET
c
c --- INPUTS:
c          XDATA(nx,ny) - real array  - Gridded data field
c                    NX - integer     - No. grid cells in X direction
c                    NY - integer     - No. grid cells in Y direction
c                 IFORM - integer     - Output format
c                                       (1=f7.2, 2=f7.4)
c                MESSAG - char*70     - Text label for initial record
c
c --- OUTPUT: none
c
c       Parameters: MXNX, MXNY, IO6, IO7
c
c --- WRRDAT called by:  MAIN
c --- WRRDAT calls:      none
c----------------------------------------------------------------------
c
c --- Set parameters
      include 'params.geo'

      real xdata(mxnx,mxny)
      character*1 ccomma
      character*70 messag
c
      data ccomma/','/
c
c --- Write the header record for this field
c --- NOTE: 2 is the code for a gridded field
      write(io7,12)messag
12    format(1x,'2',3x,' - ',a70)
c
c --- Write a gridded field in GEO.DAT format
      nxm1=nx-1
      if(iform.eq.1)then
         do 100 j=ny,1,-1
            write(io7,95)(xdata(n,j),ccomma,n=1,nxm1),xdata(nx,j)
95          format(100(f7.2,a1))
100      continue
      else if(iform.eq.2)then
         do 200 j=ny,1,-1
            write(io7,195)(xdata(n,j),ccomma,n=1,nxm1),xdata(nx,j)
195         format(100(f7.4,a1))
200      continue
      else
         write(io6,*)'Error in subr. WRRDAT -- Invalid value of IFORM ',
     1   '-- IFORM = ',iform
         write(*,*) ' ERROR in SUBR. WRRDAT -- See Run LIST file'
         stop
      endif
c
      return
      end
c----------------------------------------------------------------------
      subroutine wredat(xdata,nx,ny,iform,htfac,messag)
c----------------------------------------------------------------------
c
c --- MAKEGEO  Version: 3.2     Level: 960622                  WREDAT
c ---          J. Scire, SRC
c
c --- PURPOSE:  Write gridded ELEVATION data in the GEO.DAT format used
c               by CALMET
c
c --- INPUTS:
c          XDATA(nx,ny) - real array  - Gridded elevtaion data field (m)
c                    NX - integer     - No. grid cells in X direction
c                    NY - integer     - No. grid cells in Y direction
c                 IFORM - integer     - Output format
c                                       (1=f7.2, 2=f7.4)
c                 HTFAC - real        - Factor to convert elevations
c                                       to meters
c                MESSAG - char*70     - Text label for initial record
c
c --- OUTPUT: none
c
c       Parameters: MXNX, MXNY, IO6, IO7
c
c --- WREDAT called by:  MAIN
c --- WREDAT calls:      none
c----------------------------------------------------------------------
c
c --- Set parameters
      include 'params.geo'

      real xdata(mxnx,mxny)
      character*1 ccomma
      character*70 messag
c
      data ccomma/','/
c
c --- Write the header record for this field
c --- NOTE: htfac is the conversion factor to meters
      write(io7,12) htfac,messag
12    format(1x,f6.4,1x,' - ',a70)
c
c --- Write a gridded field in GEO.DAT format
      nxm1=nx-1
      if(iform.eq.1)then
         do 100 j=ny,1,-1
            write(io7,95)(xdata(n,j),ccomma,n=1,nxm1),xdata(nx,j)
95          format(100(f7.2,a1))
100      continue
      else if(iform.eq.2)then
         do 200 j=ny,1,-1
            write(io7,195)(xdata(n,j),ccomma,n=1,nxm1),xdata(nx,j)
195         format(100(f7.4,a1))
200      continue
      else
         write(io6,*)'Error in subr. WREDAT -- Invalid value of IFORM ',
     1   '-- IFORM = ',iform
         write(*,*) ' ERROR in SUBR. WREDAT -- See Run LIST file'
         stop
      endif
c
      return
      end
c----------------------------------------------------------------------
      subroutine out(rarray,iarray,ityp,nsigd,ldate,messag,nbx,nby,
     1 nex,ney)

c----------------------------------------------------------------------
c
c --- MAKEGEO  Version: 3.2     Level: 990130                     OUT
c ---          J. Scire, Earth Tech, Inc.
c
c --- PURPOSE:  Write a gridded field of real or integer numbers
c
c --- Update
c     Ver 1.0  Level 960215 to 990130       DGS
c              Allow subgrid to be printed, and use (I3) cell index
c
c --- INPUTS:
c       RARRAY(MXNX,MXNY) - Real array- Array of real numbers to print
c                                       (used only if ITYP = 1)
c       IARRAY(MXNX,MXNY) - Int. array- Array of integer numbers to
c                                       print (used only if ITYP = 2)
c                  ITYP - Integer     - Array type (1=real, 2=integer)
c                 NSIGD - Integer     - No. digits to print (valid range
c                                       for NSIGD is 1 to 5)
c                 LDATE - Logical     - Control variable for printing
c                                       of date (.true. = print date in
c                                       common /GEN/, .false. = do not
c                                       print date)
c                MESSAG - Char.*70    - Label of table
c                   NBX - Integer     - Starting X cell to print
c                   NBY - Integer     - Starting Y cell to print
c                   NEX - Integer     - Ending X cell to print
c                   NEY - Integer     - Ending Y cell to print
c       Common block /GEN/ variables:
c          NYR, NMO, NDAY, NJUL, NHR  - (Used only if LDATE=.true.)
c       Parameters: MXNX, MXNY, IO6
c
c --- OUTPUT:  none
c
c --- OUT    called by:  MAIN
c --- OUT    calls:      WRT, WRT2
c----------------------------------------------------------------------
c
c --- include parameters
      include 'params.geo'
c
c --- Set dummy common for "date" variables (not used here)
      common/gen/nyr,nmo,nday,njul,nhr
c
      real rarray(mxnx,mxny)
c
      integer iarray(mxnx,mxny),icol(5)
      integer iout(mxnx)
c
      logical ldate
c
      character*70 messag
      character*1 sign(mxnx),plus,minus
      character*24 form1(5)
      character*21 form2(5)
      character*18 form3(5)
c
c     include 'gen.met'
c
      data icol /40,40,30,25,20/
      data plus,minus /'+','-'/
      data form1 /'(1x,i3,1x,1hI,40(i3,1x))',
     1            '(1x,i3,1x,1hI,40(i3,1x))',
     2            '(1x,i3,1x,1hI,40(i3,1x))',
     3            '(1x,i3,1x,1hI,40(i4,1x))',
     4            '(1x,i3,1x,1hI,40(i5,1x))'/
      data form2 /'(5x,1hI,40(2x,a1,1x))',
     1            '(5x,1hI,40(2x,a1,1x))',
     2            '(5x,1hI,40(2x,a1,1x))',
     3            '(5x,1hI,40(3x,a1,1x))',
     4            '(5x,1hI,40(4x,a1,1x))'/
      data form3 /'(6x,40(i3,1x))',
     1            '(6x,40(i3,1x))',
     2            '(6x,40(i3,1x))',
     3            '(6x,40(i4,1x))',
     4            '(6x,40(i5,1x))'/

c --- check that valid values of array type (ityp) and print digits
c --- (nsigd) have been passed to routine
      if(ityp.ne.1.and.ityp.ne.2)then
         write(io6,*)'ERROR in SUBR. OUT -- invalid value of ITYP -- ',
     1   'ITYP = ',ityp
         write(*,*) 'ERROR in SUBR. OUT  --  See Run LIST File'
         stop
      endif
      if(nsigd.lt.1.or.nsigd.gt.5)then
         write(io6,*)'ERROR in SUBR. OUT -- invalid value of NSIGD -- ',
     1   'NSIGD = ',nsigd
         write(*,*) 'ERROR in SUBR. OUT  --  See Run LIST File'
         stop
      endif
c
c --- compute no. X cells to print
      nx=nex-nbx+1
c
      icr=2
      if(nsigd.eq.1)icr=1
      if(mod(nx,icol(nsigd)).eq.0)then
         npass=nx/icol(nsigd)
      else
         npass=nx/icol(nsigd)+1
      endif
c
c --- real array -- find min. & max. values
      if(ityp.ne.1)go to 50
      xmax=-1.e-25
      xmin=1.e25
      do 10 i=nbx,nex
      do 10 j=nby,ney
      if(rarray(i,j).gt.xmax)xmax=rarray(i,j)
      if(rarray(i,j).lt.xmin)xmin=rarray(i,j)
10    continue
      if(xmin.ne.0.0.or.xmax.ne.0.0)go to 12
      if(ldate)write(io6,94)messag,nyr,nmo,nday,njul,nhr
      if(.not.ldate)write(io6,95)messag
      write(io6,11)
11    format(1x,'GRID NOT PRINTED -- all values zero')
      return
c
12    continue
      xexp=xmax
      if(abs(xmin).gt.xmax)xexp=abs(xmin)
      iexp=alog10(xexp)
      if(xexp.lt.1.0)iexp=iexp-1
      nexp=iexp-(nsigd-icr)
      xscale=10.**(-nexp)
c
      ic1=nbx
      ic2=ic1+icol(nsigd)-1
      if(ic2.gt.nex)ic2=nex
c
      do 30 ipass=1,npass
c
      if(ldate)write(io6,94)messag,nyr,nmo,nday,njul,nhr
94    format(/1x,a70,2x,'year: ',i2,2x,'month: ',i2,2x,'day: ',i2,2x,
     1 'Julian day: ',i3,2x,'hour: ',i2/)
      if(.not.ldate)write(io6,95)messag
95    format(/1x,a70/)
      write(io6,109)nexp
109   format(1x,'Multiply all values by 10 ** ',i3/)
c
      do 20 jj=ney,nby,-1
         icnt=0
c
         do 18 i=ic1,ic2
            icnt=icnt+1
            if(rarray(i,jj).lt.0)then
               iout(icnt)=-(rarray(i,jj)*xscale-0.5)
               sign(icnt)=minus
            else
               iout(icnt)=rarray(i,jj)*xscale+0.5
               sign(icnt)=plus
            endif
18       continue
         call wrt(form1(nsigd),form2(nsigd),jj,iout,sign,icnt,io6)
20    continue
c --- Set underline (minimum space per cell is 4 characters)
      minund=4     
      nund=icnt*MAX((nsigd+1),minund)
      write(io6,101)(minus,n=1,nund)
101   format(5x,160a1)
      call wrt2(form3(nsigd),ic1,ic2,io6)
c
      ic1=ic1+icol(nsigd)
      ic2=ic2+icol(nsigd)
      if(ic2.gt.nex)ic2=nex
30    continue
      return
c
c --- integer array -- find min. & max. values
50    continue
      kmax=-9999999
      kmin=9999999
      do 110 i=nbx,nex
      do 110 j=nby,ney
      if(iarray(i,j).gt.kmax)kmax=iarray(i,j)
      if(iarray(i,j).lt.kmin)kmin=iarray(i,j)
110   continue
      if(kmin.ne.0.or.kmax.ne.0)go to 102
      if(ldate)write(io6,94)messag,nyr,nmo,nday,njul,nhr
      if(.not.ldate)write(io6,95)messag
      write(io6,11)
      return
c
102   continue
      xexp=kmax
      if(iabs(kmin).gt.kmax)xexp=iabs(kmin)
      iexp=alog10(xexp)
      if(xexp.lt.1.0)iexp=iexp-1
      nexp=iexp-(nsigd-icr)
      xscale=10.**(-nexp)
c
      ic1=nbx
      ic2=ic1+icol(nsigd)-1
      if(ic2.gt.nex)ic2=nex
c
      do 130 ipass=1,npass
c
      if(ldate)write(io6,94)messag,nyr,nmo,nday,njul,nhr
      if(.not.ldate)write(io6,95)messag
      write(io6,109)nexp
c
      do 120 jj=ney,nby,-1
         icnt=0
c
         do 118 i=ic1,ic2
            icnt=icnt+1
            if(iarray(i,jj).lt.0)then
               iout(icnt)=-(iarray(i,jj)*xscale-0.5)
               sign(icnt)=minus
            else
               iout(icnt)=iarray(i,jj)*xscale+0.5
               sign(icnt)=plus
            endif
118      continue
         call wrt(form1(nsigd),form2(nsigd),jj,iout,sign,icnt,io6)
120   continue
c --- Set underline (minimum space per cell is 4 characters)
      minund=4     
      nund=icnt*MAX((nsigd+1),minund)
      write(io6,101)(minus,n=1,nund)
      call wrt2(form3(nsigd),ic1,ic2,io6)
c
      ic1=ic1+icol(nsigd)
      ic2=ic2+icol(nsigd)
      if(ic2.gt.nex)ic2=nex
130   continue
c
      return
      end
c----------------------------------------------------------------------
      subroutine wrt(form1,form2,jj,iout,sign,n,io6)
c----------------------------------------------------------------------
c
c --- MAKEGEO  Version: 3.2     Level: 920905                     WRT
c ---          J. Scire, SRC
c
c --- PURPOSE:  Write one Y row of gridded data
c
c --- INPUTS:
c              FORM1 - Char.*24    - Format field for Y label and data
c                                    to be printed
c              FORM2 - Char.*21    - Format field for sign of data
c                 JJ - Integer     - Y grid cell number
c            IOUT(N) - Int. array  - Array of data to be printed
c                                    (one Y row)
c            SIGN(N) - Char.*1     - Array containing sign of data
c                                    ('+' or '-')
c                  N - Integer     - Number of cells in this row
c                IO6 - Integer     - Fortran unit no. of output
c
c --- OUTPUT:  none
c
c --- WRT called by:  OUT
c --- WRT calls:      none
c----------------------------------------------------------------------
      integer iout(n)
c
      character*1 sign(n)
      character*24 form1
      character*21 form2
c
      write(io6,form1)jj,iout
      write(io6,form2)sign
c
      return
      end
c----------------------------------------------------------------------
      subroutine wrt2(form,n1,n2,io6)
c----------------------------------------------------------------------
c
c --- MAKEGEO  Version: 3.2     Level: 920905                    WRT2
c ---          J. Scire, SRC
c
c --- PURPOSE:  Write a line labeling grid cell numbers
c
c --- INPUTS:
c               FORM - Char.*18    - Format field of data to be printed
c                 N1 - Integer     - Starting grid cell number
c                 N2 - Integer     - Ending grid cell number
c                IO6 - Integer     - Fortran unit no. of output
c
c --- OUTPUT:  none
c
c --- WRT2 called by:  OUT
c --- WRT2 calls:      none
c----------------------------------------------------------------------
      character*18 form
c
      write(io6,form)(i,i=n1,n2)
      return
      end
c----------------------------------------------------------------------
      logical function lrsame(r0,r1,r2)
c----------------------------------------------------------------------
c
c --- MAKEGEO    Version: 3.2    Level: 991104c                LRSAME
c                D. Strimaitis,    Earth Tech, Inc.
c ---            From CALPOST V5.2, L991104c
c
c --- PURPOSE:  Compare 2 real numbers (r1,r2) to determine if their
c               fractional difference exceeds r0
c
c --- INPUTS:
c            r0 - real       - Fractional difference allowed
c            r1 - real       - Real value 1
c            r2 - real       - Real value 2
c
c
c --- OUTPUT:
c        lrsame - logical    - Key indicating result of test
c                              .TRUE.  -- values are 'same'
c                              .FALSE. -- values are NOT 'same'
c
c
c --- LRSAME called by:   MAIN
c --- LRSAME calls:       none
c----------------------------------------------------------------------
c
      data half/0.5/

      lrsame=.TRUE.

c --- Direct comparison
      if(r1.EQ.r2) return

      rdif=ABS(r1-r2)
      ravg=half*ABS(r1+r2)

      if(rdif.GE.ravg) then
c ---    Fractional difference greater than one!
         lrsame=.FALSE.
      else
         ftest=rdif/ravg
         if(ftest.GT.r0) lrsame=.FALSE.
      endif

      return
      end
c-----------------------------------------------------------------------
      subroutine wrthead
c-----------------------------------------------------------------------
c
c --- MAKEGEO   Version: 3.2          Level: 090526           WRTHEAD
c               D. Strimaitis, Earth Tech, Inc.
c
c PURPOSE:     WRTHEAD constructs the header records for the output
c              data file GEO.DAT
c
C
C Update:
C    From Version: 3.2 Level: 030402 to Veriosn: 3.0  Level: 090526
C    Change GEO.DAT Version numder to 2.1 from 2.0 to indicate 
C           possible snow informat processing.
C
C Zhong-Xiang Wu
C 5/26/2009
 
c ARGUMENTS:
c    PASSED:  /CONTROL/   logicals
c             /GRID/      data
c             /QA/        ver,level
c
c  RETURNED:  none
c
c CALLING ROUTINES:   SETUP
c
c EXTERNAL ROUTINES:  none
c-----------------------------------------------------------------------
c --- Include file of parameters and commons
      include 'params.geo'
      include 'control.geo'
      include 'grid.geo'
      include 'qa.geo'

c --- Local Variables
      character*16 dataset,dataver
      character*64 datamod
      character*80 comment1

c --- Configure output variables
      data dataset/'GEO.DAT'/, dataver/'2.1'/
c      data dataset/'GEO.DAT'/, dataver/'2.0'/
      data datamod/'Header structure with coordinate parameters'/
      data ncomment/2/
      data comment1/'Produced by MAKEGEO Version: '/

c --- Construct the version-level comment string
      j=30
      do i=1,12
         if(ver(i:i).NE.' ') then
            comment1(j:j)=ver(i:i)
            j=j+1
         endif
      enddo
      j=j+1
      comment1(j:j+7)=' Level: '
      j=j+8
      do i=1,12
         if(level(i:i).NE.' ') then
            comment1(j:j)=level(i:i)
            j=j+1
         endif
      enddo

c --- Record 1:  Dataset, Version, Modifier
      write(io7,'(2a16,a64)') dataset,dataver,datamod
c --- Record 2:  Number of comment records
      write(io7,'(i4)') ncomment
c --- Record 3:  Comment (optional/repeatable)
      write(io7,'(a80)') comment1
      write(io7,'(a80)') ctitle
c --- Record 5:  Map projection
      write(io7,'(a8)') pmap
c --- Record 6:  Map projection parameters
      if(LUTM) then
         write(io7,'(i4,a4)') izone,utmhem
      elseif(LLCC) then
         write(io7,'(4a16)') clat0,clon0,clat1,clat2
      elseif(LPS) then
         write(io7,'(3a16)') clat0,clon0,clat1
      elseif(LEM.or.LLAZA.or.LTTM) then
         write(io7,'(2a16)') clat0,clon0
      endif
c --- Record 7:  Map false Easting/Northing
      if(LLCC.or.LLAZA.or.LTTM) then
         write(io7,*) feast,fnorth
      endif
c --- Record 8:  Map DATUM
      write(io7,'(a8,a12)') datum,daten
c --- Record 9:  Grid
      write(io7,'(2i8,4f12.3)') nx,ny,xorg,yorg,delx,delx
c --- Record 10:  XYUNIT,ZUNIT
      write(io7,'(2a4)') 'KM  ','M   '

      return
      end
c----------------------------------------------------------------------
      subroutine fin
c----------------------------------------------------------------------
c
c --- MAKEGEO  Version: 3.2           Level: 090511               FIN
c ---          J. Scire
c
c --- PURPOSE:  Run termination routine -- compute runtime
c
c --- V2.0 (011003) to V6.327 (090511) (DGS)
c               - Reformat date reported at end of run
c
c --- INPUTS:
c       Common block /QA/
c          rdate, rtime, rcpu
c       Parameters: IO6, IOMESG
c
c --- OUTPUT:  none
c
c --- FIN called by:  MAIN
c --- FIN calls:      DATETM, JULDAY, DELTT, FMT_DATE
c----------------------------------------------------------------------
c
c --- include parameters
      include 'params.geo'
      include 'qa.geo'
c
      character*8 rtime2
      character*10 rdate2
      character*12 rdate12
c
      write(iomesg,*)'TERMINATION PHASE'
c
c --- get system date & time at end of run
      call datetm(rdate2,rtime2,rcpu)
c
c --- compute runtime
      read(rtime(1:2),10)ihr1
      read(rtime(4:5),10)imin1
      read(rtime(7:8),10)isec1
10    format(i2)
      t1=ihr1*3600.+imin1*60.+isec1
c
      read(rtime2(1:2),10)ihr2
      read(rtime2(4:5),10)imin2
      read(rtime2(7:8),10)isec2
      t2=ihr2*3600.+imin2*60.+isec2
c
      if(rdate.eq.rdate2)then
         delt=t2-t1
      else
         read(rdate(1:2),10)imo1
         read(rdate(4:5),10)iday1
         read(rdate(7:10),'(i4)')iyr1
         call julday(io6,iyr1,imo1,iday1,ijul1)
c
         read(rdate2(1:2),10)imo2
         read(rdate2(4:5),10)iday2
         read(rdate2(7:10),'(i4)')iyr2
         call julday(io6,iyr2,imo2,iday2,ijul2)
c
c ---    compute no. hours from beg. of first hour of run to
c ---    ending hour of ending day of the run
         call deltt(iyr1,ijul1,ihr1,iyr2,ijul2,ihr2,idelhr)
c
c ---    adjust for minutes and seconds
         delt=idelhr*3600.-imin1*60.-isec1+imin2*60.+isec2
      endif

c --- On the PC, the runtime and CPU time are the same
c --- (DATETM provides RCPU = 0.0 on the PC)
      if(rcpu.EQ.0.0)rcpu=delt

c --- Report current date
      rdate12=rdate2(1:10)//'  '
      call FMT_DATE(io6,'MM-DD-YYYY','DD-MMM-YYYY',rdate12)
      write(io6,1402)rtime2,rdate12,NINT(delt),NINT(rcpu)
1402  format(//2x,'End of run -- Clock time: ',a8/
     1         2x,'                    Date: ',a12//
     2         2x,'      Elapsed Clock Time: ',i12,' (seconds)'//
     3         2x,'                CPU Time: ',i12,' (seconds)')

c
      return
      end

c----------------------------------------------------------------------
      subroutine rdsnhd
c----------------------------------------------------------------------
c
c --- MAKEGEO   Version: 3.2          Level: 090526            RDSNHD
c               J. Scire, D. Strimaitis   Earth Tech, Inc.
c
c --- PURPOSE:  
C         Read the file header from a snow.dat file
C
c --- Zhong-Xiang Wu
C     5/26/2009
      
      include 'params.geo'
      include 'control.geo'
      include 'snow.geo'

      parameter(mxcmm=100)

      character*16 dataseti,dataveri,datetimei
      character*64 datamodi
      character*8 cmapi,datumi,timezonei
      character*10 dateni
      character*4 unitsi
      character*80 buff

C --- Header Record #1, #2
      if(ifmt.eq.1) then
         read(isnw,101)dataseti,dataveri,datamodi
         read(isnw,102)ncomm
 101     format(2a16,a64)
 102     format(i4)
      else
         read(isnw)dataseti,dataveri,datamodi
         read(isnw)ncomm
      endif

C --- Header Record #3
      if(ncomm.gt.mxcmm) then
         write(io6,*)' Error: Comment lines out range:',ncomm,mxcmm
         print *,' Error: Comment lines out range:',ncomm,mxcmm
         stop
      endif

      do i=1,ncomm
         if(ifmt.eq.1) then
            read(isnw,'(a)')comment
         else
            read(isnw)comment
         endif
      enddo

C --- Read Standard CALXX file header (from DS)
      if(ifmt.eq.1) then
         read(isnw,121)cmapi
         read(isnw,121)datumi,dateni
         read(isnw,122)unitsi
 121     format(a8,a10)
 122     format(a4)
      else
         read(isnw)cmapi
         read(isnw)datumi,dateni
         read(isnw)unitsi
      endif

      if(ifmt.eq.1) then
         read(isnw,*)flonbs,flatbs,nxp,nyp,dx,dy
         read(isnw,121)timezonei
         read(isnw,302)datetimei
 302     format(a16)
      else
         read(isnw)flonbs,flatbs,nxp,nyp,dx,dy
         read(isnw)timezonei
         read(isnw)datetimei
      endif

C --- Raw snow data config. info
      if(ifmt.eq.1) then
         read(isnw,*)flon0,flat0,nx0,ny0,dx0,dy0
         read(isnw,*)nib,nie,njb,nje,npk
      else
         read(isnw)flon0,flat0,nx0,ny0,dx0,dy0
         read(isnw)nib,nie,njb,nje,npk
      endif

      isoff=nib-1
      jsoff=njb-1

C --- Snow file header QA --------------
      if(nxt.ne.nx0 .or. nyt.ne.ny0) then
         write(io6,201)nxt,nyt,nx0,ny0
         write(*,201)nxt,nyt,nx0,ny0
         stop
      endif
 201  format(' Error: nx/ny mis-match:',4i8)

      dd1=abs(flonorg-flon0)+abs(flatorg-flat0)
      dd2=abs(dlon-dx0)+abs(dlat-dy0)
      if(dd1.gt.0.001) then
         write(io6,202)dd1,flonorg,flon0,flatorg,flat0
         stop
      endif
 202  format(' Error: Corner lon/lat mis-match:',4f12.6)

      if(dd2.gt.0.00001) then
         write(io6,203)dd2,dlon,dx0,dlat,dy0
         stop
      endif
 203  format(' Error: Grid size mis-match:',4f12.6)

      return
      end

c----------------------------------------------------------------------
      subroutine rdsn(xsnow,ndsnow,xsnowc)
c----------------------------------------------------------------------
c
c --- MAKEGEO   Version: 3.2          Level: 090526            RDSN
c               J. Scire, D. Strimaitis   Earth Tech, Inc.
c
c --- PURPOSE:  
C         Read the file header from a snow.dat file
C
c --- Zhong-Xiang Wu
C     5/26/2009
      
      include 'params.geo'
      include 'control.geo'
      include 'snow.geo'

      real xsnow(nxp,nyp,nwdt)
      integer ndsnow(nxp,nyp)
      real xsnowc(nxp,nyp)

C --- Local 
      integer*2 ix(nxp*nyp),ix2(nxp*nyp)
      integer*2 isml2,ibig2,irange2,ipmiss2,ia
      integer*2 it0,it1,it2,it3,it4,it5,it6,it7,it8,it9

      character*8 vns,csndp,cprec
      data csndp,cprec/'SNOWDEP','PRECIPS'/
      data precmin/0.1/

C --- Loop over variables in snow data
      call getdate(idatenext,iy,im,id,ih)
      call chgtim(iy,im,id,ih,-48)
      call timestmp(iy,im,id,ih,idatelmt)

 4000 continue
      ifound1=0
      ifound2=0

      do 5000 ivar=1,npk

C --- Data header
      if(ifmt.eq.1) then
         read(isnw,111,err=6000,end=6000)it1,it2,it3,it4,it0,it5,it6
     &     ,it7,it8,it0,vns,isml2,ibig2,ipmiss2,scale,ins
 111     format(2(i4,3i3,1x,i4.4,1x),a8,1x,3i7,f8.1,i10)
      else
         read(isnw,err=6000,end=6000)it1,it2,it3,it4,it0,it5,it6
     &     ,it7,it8,it0,vns,isml2,ibig2,ipmiss2,scale,ins
      endif

      isml=isml2
      ibig=ibig2
      irange=ibig-isml

      if(irange.gt.nmx) then
         write(io6,112)isml,ibig,irange
         print 112,isml,ibig,irange
 112     format(' Error: TRANSDATA failed: data outrange :',4i10)
         stop
      endif

      iyr=it1
      imn=it2
      idy=it3
      ihr=it4

C --- Read packed Data 
      if(ins.lt.0) then
         write(io6,201)ins
         print 201,ins
 201     format(' Error: Illegal packed snow data:',2i7)
         stop
      endif

      if(ifmt.eq.1) then
         read(isnw,301)(ix2(ij),ij=1,ins)
 301     format(10i7)
      else
         read(isnw)(ix2(ij),ij=1,ins)
      endif

C --- Finish reading one variable from  one-day data
 2000 continue
      if(vns.ne.csndp .and. vns.ne.cprec) goto 5000
      
C --- Check time stamps
      ihr0=0

      call timestmp(iyr,imn,idy,ihr0,idatc)

      if(idatc.lt.idatelmt) goto 5000

C --- Find selected variable

C --- Expand to full nx*ny array 
      if(vns.eq.csndp) ifound1=1
      if(vns.eq.cprec) ifound2=1

      ipos=1
      ij=0

 1000 ia=ix2(ipos)
      if(ia.ge.0 .or. ia.eq.ipmiss2) then
         ij=ij+1
         ix(ij)=ia
      else
         np=-ia
         ipos=ipos+1
         ia=ix2(ipos)
         do i=1,np
            ij=ij+1
            ix(ij)=ia
         enddo
      endif

      ipos=ipos+1

      if(ipos.le.ins) goto 1000
      
      if(ij.ne.nxp*nyp) then
         write(io6,102)ij,nxp*nyp
         print 102,ij,nxp*nyp
 102     format(' Error: Expanding failed:',3i10)
         stop
      endif

C --- Recover original values to 2D array
      do ij=1,nxp*nyp
         ia=ix(ij)
         j=(ij-1)/nxp+1
         i=ij-(j-1)*nxp

         if(ia.ne.ipmiss2) then
            ia=ia+isml2
            aa=ia/scale
         else
            aa=fmiss
         endif

         if(vns.eq.csndp) then
            xsnow(i,j,nwdt)=aa
         elseif(vns.eq.cprec) then
            if(aa.ge.precmin) then 
               ndsnow(i,j)=0
            else
               ndsnow(i,j)=ndsnow(i,j)+1
            endif
         endif
      enddo

 5000 continue

      if(idatc.lt.idatelmt) goto 4000

      if(ifound1.eq.0) then
         write(io6,311)ifound1,csndp
         print 311,ifound1,csndp
 311     format(' Error: Missing Required Variable:',i6,1x,a) 
         stop
      endif

      if(ifound2.eq.0) then
         write(io6,311)ifound2,cprec
         print 311,ifound2,cprec
         stop
      endif

C --- Fill missing in spatial if required for snow depth
      if(isfill.eq.1) then
         call fills(xsnow,nxp,nyp,nwdt,nwdt,fmiss,nwds)
      endif

C --- Read 2nd day at beginning date
      if(idatc.lt.idatenext) then
         call rollbk(xsnow,nxp,nyp,nwdt,fmiss)
         goto 4000
      endif

      if(idatc.ne.idatenext) then
         write(io6,*)' Error: Snow data not found:',idatc,idatenext
         print *,' Error: Snow data not found:',idatc,idatenext
         stop
      endif

      goto 7000
 6000 continue

      ioff=1
      print *,' Snow Data End at ',idatc

C --- Fill missing in spatial if required
 7000 continue

      nn=nwdt-1
      if(itfill.eq.1) then
         call fillt(xsnow,xsnowc,nxp,nyp,nn,nwdt,fmiss)
      else
C        Pass the middle one for GEO processing use
         do j=1,nyp
            do i=1,nxp
               xsnowc(i,j)=xsnow(i,j,nn)
            enddo
         enddo
      endif

      return
      end

c----------------------------------------------------------------------
      subroutine fills(xdata,nx,ny,ivar,nvar,fmiss,nwds)
c----------------------------------------------------------------------
c
c --- MAKEGEO   Version: 3.2          Level: 090526            FILL
c               J. Scire, D. Strimaitis   Earth Tech, Inc.
c
c --- PURPOSE:  
C         Fill missing data within a spatial window
C
c --- Zhong-Xiang Wu
C     5/26/2009
      
      dimension xdata(nx,ny,nvar)
      dimension xtmp(nx,ny)

      do j=1,ny
         do i=1,nx
            xtmp(i,j)=xdata(i,j,ivar)
         enddo
      enddo

      ims=0
      do j=1,ny
         do i=1,nx
            aa=xtmp(i,j)
            if(aa.ne.fmiss) goto 1000
            
            do m=1,nwds
               i1=i-m
               i2=i+m
               j1=j-m
               j2=j+m
               
               i1=max(i1,1)
               j1=max(j1,1)
               i2=min(i2,nx)
               j2=min(j2,ny)

               dd=0
               ip=0
               do jj=j1,j2
                  do ii=i1,i2
                     aa=xtmp(ii,jj)
                     if(aa.ne.fmiss) then
                        ip=ip+1
                        dd=dd+aa
                     endif
                  enddo
               enddo

               if(ip.ge.1) then
                  dd=dd/ip
                  xdata(i,j,ivar)=dd
                  ims=ims+1
                  goto 1000
               endif
            enddo               ! end of m
            
 1000       continue
            
         enddo                  ! end of i
      enddo                     ! end of j

      return
      end

c----------------------------------------------------------------------
      subroutine fillt(xdata,xdatac,nx,ny,ivar,nwdt,fmiss)
c----------------------------------------------------------------------
c
c --- MAKEGEO   Version: 3.2          Level: 090526            FILL
c               J. Scire, D. Strimaitis   Earth Tech, Inc.
c
c --- PURPOSE:  
C         Fill missing data within a spatial window
C
c --- Zhong-Xiang Wu
C     5/26/2009
      
      dimension xdata(nx,ny,nwdt),xdatac(nx,ny)

      ims=0
      
      do j=1,ny
         do i=1,nx
            aa=xdata(i,j,ivar)
            if(aa.ne.fmiss) then
               xdatac(i,j)=aa
            else
               dd=0
               ip=0
               do m=1,nwdt
                  aa=xdata(i,j,m)
                  if(aa.ne.fmiss) then
                     ip=ip+1
                     dd=dd+aa               
                  endif
               enddo

               if(ip.ge.1) then
                  dd=dd/ip
                  ims=ims+1
               else
                  dd=fmiss
               endif
               xdatac(i,j)=dd
            endif
         enddo                  ! end of i
      enddo                     ! end of j

      return
      end
               
! **********************************************************************
      subroutine chgtim(iyr,imon,iday,ihour,idt)

c --- RUCDECODE    Version: 2.6       Level: 060823               
! --- Zhong-Xiang Wu           
!
! --- PURPOSE:
!     Increase or decrease hours. 
!     Used to convert Local Stand Time to GMT or to setup time stamp for loop

      parameter(nmonth=12)

      dimension ndays(nmonth)

      data ndays/31,28,31,30,31,30,31,31,30,31,30,31/

      if(mod(iyr,4).eq.0) then
         ndays(2)=29 
      else
         ndays(2)=28
      endif

      ihour=ihour+idt

      if(idt.lt.0) goto 2000

 1000 if(ihour.gt.23) then
         ihour=ihour-24
         iday=iday+1
         
         if(iday.gt.ndays(imon)) then
            iday=1
            imon=imon+1
            
            if(imon.gt.12) then
               imon=1
               iyr=iyr+1
               if(iyr/4 .eq. iyr/4.0) then
                  ndays(2)=29 
               else
                  ndays(2)=28
               endif
            endif
         endif
         goto 1000
      else
         return
      endif

 2000 continue

 3000 if(ihour.lt.0) then
         ihour=ihour+24
         iday=iday-1
         
         if(iday.le.0) then
            imon=imon-1
            if(imon.le.0) then
               iyr=iyr-1
               imon=12
               if(iyr/4 .eq. iyr/4.0) then
                  ndays(2)=29 
               else
                  ndays(2)=28
               endif
            endif
   
            iday=ndays(imon)
         endif
         goto 3000
      else
         return
      endif

      end
! --------------------------------------------------
      Subroutine timestmp(iyr,imn,idy,ihr,idate)

c --- RUCDECODE    Version: 2.6       Level: 060823               

      idate=ihr+idy*100+imn*10000+iyr*1000000

      return
      end

c ---------------------------------------------------------------------
      subroutine getdate(ndate,iyr,imon,iday,ihour)
c ---------------------------------------------------------------------
c --- RUCDECODE    Version: 2.6       Level: 050507             GETDATE

      iyr=ndate/1000000
      imon=ndate/10000-iyr*100
      iday=ndate/100-iyr*10000-imon*100
      ihour=ndate-iyr*1000000-imon*10000-iday*100
      
      return
      end

c----------------------------------------------------------------------
      subroutine rollbk(xdata,nx,ny,nwdt,fmiss)
c----------------------------------------------------------------------
c
c --- MAKEGEO   Version: 3.2          Level: 090526            FILL
c               J. Scire, D. Strimaitis   Earth Tech, Inc.
c
c --- PURPOSE:  
C         Save one day snow data
C
c --- Zhong-Xiang Wu
C     5/26/2009
      
      dimension xdata(nx,ny,nwdt)

      do j=1,ny
         do i=1,nx
            do k=1,nwdt-1
               k1=k+1
               xdata(i,j,k)=xdata(i,j,k1)
            enddo
            xdata(i,j,nwdt)=fmiss
         enddo
      enddo

      return
      end

c --------------------------------------------------------------
	subroutine gethours(ndate1,ndate2,nhrs)

c --- RUCDECODE    Version: 2.6       Level: 061228               
C Purpose:
c       Get hours between two dates
C Zhong Wu
C (3/22/1999)

C Changes
C  1. Fix an error for total hours when beginning year is 
C     leap year
C  Zhong Wu      
C  5/23/2003

c	Get hours between to dates

	if(ndate2.lt.ndate1) then
	   print *,'Error in gethours: ndate2 < ndate1'
	   print *,ndate1,ndate2
	   stop
	endif

	call getdate(ndate1,iyr1,imon1,iday1,ihour1)
	call julday(io6,iyr1,imon1,iday1,jday1)

	call getdate(ndate2,iyr2,imon2,iday2,ihour2)
	call julday(io6,iyr2,imon2,iday2,jday2)

        call deltt(iyr1,jday1,ihour1,iyr2,jday2,ihour2,nhrs)

        nhrs=nhrs+1   ! deltt is one hour less total hours
 
	return
	end
