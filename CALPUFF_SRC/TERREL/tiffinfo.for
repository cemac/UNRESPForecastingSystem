c
c --- TIFFINFO  Version: 1.03           Level: 090123         
c               K. Morrison, Hatch
c
c     This file contains subroutines for extracting geographic
c     information from GeoTIFF files.  Externally, only GET_IFD and
c     READTIFF are called, the other routines only being used internally
c     to read information within the GeoTIFF, allowing for byte
c     flipping if needed based on the byte order in the file and on
c     the host machine.
c
c-----------------------------------------------------------------------
      subroutine get_ifd(ioinp,nxi,nyi,dxi,dyi,cdatumi,rlati,rloni,
     &  xlat1i,xlat2i,feasti,fnorthi,cproji,xorg,yorg,utmhemi,iutmzni,
     &  tmscalei,itifftype,ltiffreset)
c-----------------------------------------------------------------------
c      
c --- TIFFINFO  Version: 1.03           Level: 090123         GET_IFD
c               K. Morrison, Hatch
c
c PURPOSE:      GET_IFD reads all the tags and geokeys from the Image
c               File Directory of a GeoTIFF file, passes useful fields
c               back to the calling subroutine for projection and datum
c               identification, and puts variables into the TIFF_TAGS
c               common for subsequent use by READTIFF.
c
c UPDATES:      
c            Version 1.03 Level 090123 from Version 1.02 Level 070706
c               - Change resets for datum and projection
c            Version 1.02 Level 070706 from Version 1.01 Level 061214
c               - Change test for UTMs
c               - Include half-pixel shift in origin if GeoTIFF values 
c                 are areas (assumed default) and not points
c
c INPUTS:       IOINP (i) - input unit for the GeoTIFF file (already
c                           opened in the calling program)
c           LTIFFRESET(l) - logical to reset the projection and datum 
c
c OUTPUTS:   NXI, NYI (i) - number of columns and rows in the file
c            DXI, DYI (r) - spacing of pixels in X and Y
c           CDATUMI (c*8) - datum of data in the file
c        RLATI, RLONI (r) - reference/origin latitude and longitude
c                           of the projection of the data
c      XLAT1I, XLAT2I (r) - equator-ward and pole-ward parallels
c     FEASTI, FNORTHI (r) - false easting and northing of the data
c            CPROJI (c*8) - projection of the data
c          XORG, YORG (r) - coordinates of the origin of the data
c           UTMHEMI (c*4) - hemisphere if data are in UTM
c             IUTMZNI (i) - UTM zone if data are in UTM
c            TMSCALEI (r) - TM scaling factor
c           ITIFFTYPE (i) - type of TIFF (1-values,2-RGB,3-palette)
c-----------------------------------------------------------------------
      logical lflip,loffset,lbig,lbig_end,lutm,lprojerr,lerror,
     & ltiffreset,lpixel
      integer*1 ival1(200000)
      integer*2 ival2(200000),ntag,ittag,numbytes(12),ired(200000),
     &  igreen(200000),iblue(200000)
      integer*4 itag,iltag,ivtag,ival4(200000),idum1,idum2
      real val4(200000)
      real*8 val8(200000)
      character*8 cdatumi,cproji
      character*8 char8(1000)
      character*1 char1(1000)
      character*1000 char1000
      character*4 utmhemi
      character*8 ctproj(16)/
     & 'LL      ','TM      ','        ','        ','        ',
     & '        ','        ','EM      ','LCC     ','        ',
     & 'LAZA    ','ACEA    ','        ','        ','        ',
     & 'PS      '/
      character*8 ctdatum(132)/
     & 'ADI-M   ','AUA     ','AUG     ','AIN-A   ','AFG     ',
     & '        ','        ','        ','        ','        ',
     & '        ','        ','        ','        ','        ',
     & 'BER     ','        ','BOO     ','BUR     ','        ',
     & 'CAI     ','CAP     ','CGE     ','CHU     ','COA     ',
     & '        ','        ','        ','OEG     ','EUR-M   ',
     & 'EUS     ','        ','        ','        ','        ',
     & 'HTN     ','        ','IDN     ','INF-A   ','INH-A   ',
     & '        ','        ','        ','KAN     ','KEA     ',
     & '        ','        ','PRP-M   ','        ','LEH     ',
     & 'LIB     ','        ','LUZ-A   ','        ','HEN     ',
     & 'MIK     ','        ','        ','        ','        ',
     & 'MER     ','MAS     ','MIN-A   ','        ','        ',
     & 'MPO     ','NAS-C   ','        ','NAR-C   ','NAH-A   ',
     & 'NAP     ','        ','        ','        ','        ',
     & '        ','        ','        ','        ','        ',
     & '        ','PTN     ','WGS-84  ','PUK     ','QAT     ',
     & '        ','QUO     ','        ','        ','        ',
     & 'SAN-M   ','SAP     ','SCK     ','        ','        ',
     & '        ','TAN     ','TIL     ','        ','        ',
     & 'TOY-M   ','        ','        ','VOI     ','VOR     ',
     & '        ','NSD     ','        ','YAC     ','        ',
     & 'ZAN     ','        ','        ','        ','        ',
     & '        ','        ','        ','        ','        ',
     & '        ','WGS-72  ','        ','        ','        ',
     & 'WGS-84  ','        ','        ','        ','        ',
     & '        ','        '/
      common /tiff_tags/ nbitsi,iptypei,idtypei,
     & idstarti,iorienti,numpixv,zorgtif,
     & icmodel,icordunit,iangunit,
     & lbig,lflip,zorg,zmul
c
      data numbytes/1,1,2,4,8,1,1,2,4,4,4,8/
      equivalence(char1000,char1(1))
c
c     set or reset defaults for data characteristics, units, projections
c
      if(ltiffreset) then
        cdatumi=' '
        cproji=' '
      endif
      idatum=0
      lpixel=.true.
      dxi=0.
      dyi=0.
      rlati=-999.
      rloni=-999.
      feasti=0.
      fnorthi=0.
      xorg=0.
      yorg=0.
      zorg=0.
      utmhemi=' '
      iutmzni=0
      tmscalei=1.
      icmodel=1
      icordunt=1
      nxi=0
      nyi=0
      nbitsi=1
      iptypei=0
      iptypidx=0
      idtypei=1
      idstart=0
      iorienti=1
      numpixv=1
      ixorgtif=1
      iyorgtif=1
      zorgtif=0.
      iangunt=2
      ielevunt=1
      ctrlat=-999.
      ctrlon=-999.
      xlat1i=0.
      xlat2i=0.
      tmscalec=1.
      icompress=1
      ired=0
      igreen=0
      iblue=0
      iproj=-99
      lutm=.false.
      lerror=.false.
      lprojerr=.false.
      indxpar1=0
      indxpar2=0
      indxorlon=0
      indxorlat=0
      indxfalse=0
      indxfalsn=0
      indxctrlon=0
      indxctrlat=0
      indxtmscalo=0
      indxtmscalc=0
c
c     check byte order of current platform
c
      lflip=.false.
      lbig=lbig_end(1)
c
c     read the byte order of the file, and if it's not the same as the
c     current platform, set logical for byte flipping
c     first byte = 'I' (73) (INTEL) or 'M' (77) (MOTOROLA)
c
      read(ioinp,rec=1) ival1(1)
      if((ival1(1).eq.77.and..not.lbig).or.(ival1(1).eq.73.and.lbig))
     & lflip=.true.
c
c     check the answer to the ultimate question of life, the universe,
c     and everything
c
      ipos=3
      call read2b(ioinp,ipos,ival2(1),lflip)
      if(ival2(1).ne.42) then
        write(*,*) 'Invalid TIFF ID - ',ival2
        stop
      endif
c
c     get the offset of the IFD and set the position to read it
c
      ipos=5
      call read4b(ioinp,ipos,ival4(1),lflip)
      ipos=ival4(1)+1
c
c     get the number of TAGS
c
      call read2b(ioinp,ipos,ntag,lflip)
c
c     read and decode the 12-byte tags
c
      do i=1,ntag
c
c       retain the start of the next tag
c
        iopos=ipos+12
c
c       read the tag ID
c
        call read24b(ioinp,ipos,itag,lflip,lbig)
c
c       read the tag type:
c         1 - 1-byte unsigned integer
c         2 - 1-byte character
c         3 - 2-byte unsigned integer
c         4 - 4-byte unsigned integer
c         5 - 8-byte ratio, 2 type-4s, numerator and then denominator
c         6 - 1-byte signed integer
c         7 - 1-byte undefined 
c         8 - 2-byte signed integer
c         9 - 4-byte signed integer
c        10 - 8-byte ratio, 2 type-9s, numerator and then denominator
c        11 - 4-byte real
c        12 - 8-byte real
c
        call read2b(ioinp,ipos,ittag,lflip)
c
c       check for end of tags
c
        if(itag.eq.0.and.ittag.eq.0) exit
c
c       read the number of values in this tag
c
        call read4b(ioinp,ipos,iltag,lflip)
c
c       if the number of bytes for the tag value(s) is too large for
c       the 4-byte field, the value instead is an offset to where
c       the values are stored
c
        loffset=.false.
        if(iltag*numbytes(ittag).gt.4) then
          loffset=.true.
          call read4b(ioinp,ipos,ioffset,lflip)
          ipos=ioffset+1
        endif
c
c       read the tag value(s) according to type
c
        do j=1,iltag
          if(ittag.eq.1) call read12b(ioinp,ipos,ival2(j),lbig)
          if(ittag.eq.2) call read1c(ioinp,ipos,char1(j))
          if(ittag.eq.3) call read24b(ioinp,ipos,ival4(j),lflip,lbig)
          if(ittag.eq.4) call read4b(ioinp,ipos,ival4(j),lflip)
          if(ittag.eq.5) then
            call read4b(ioinp,ipos,idum1,lflip)
            call read4b(ioinp,ipos,idum2,lflip)
            val4(j)=float(idum1)/float(idum2)
          endif
          if(ittag.eq.6) call read1b(ioinp,ipos,ival1(j))
          if(ittag.eq.7) call read1c(ioinp,ipos,char1(j))
          if(ittag.eq.8) call read2b(ioinp,ipos,ival2(j),iflip)
          if(ittag.eq.9) call read4b(ioinp,ipos,ival4(j),lflip)
          if(ittag.eq.10) then
            call read4b(ioinp,ipos,idum1,lflip)
            call read4b(ioinp,ipos,idum2,lflip)
            val4(j)=float(idum1)/float(idum2)
          endif
          if(ittag.eq.11) call read4r(ioinp,ipos,val4(j))
          if(ittag.eq.12) call read8r(ioinp,ipos,val8(j),lflip)
        enddo
c
c       put the useful tags into appropriate variables
c
c       number of columns and rows
c
        if(itag.eq.256) nxi=ival4(1)
        if(itag.eq.257) nyi=ival4(1)
c
c       number of bits per value - assume additional values are the same
c       as the first (RGB)
c
        if(itag.eq.258) nbitsi=ival4(1)
c
c       compression type - only 1 (uncompressed) supported
c
        if(itag.eq.259) icompress=ival4(1)
c
c       photometric interpretation
c         1 - B&W - black is zero
c         2 - RGB
c         3 - Palette
c
        if(itag.eq.262) iptypei=ival4(1)
c
c       strip offsets - only first value used (assumes sequential)
c
        if(itag.eq.273) idstarti=ival4(1)
c
c       image orientation
c         1 - left to right, top to bottom
c         4 - left to right, bottom to top
c
        if(itag.eq.274) iorienti=ival4(1)
c
c       number of values per pixel - only 1 or 3 supported
c
        if(itag.eq.277) numpixv=ival4(1)
c
c       read the palette - reduce to 0-255 from 0-65535
c
        if(itag.eq.320) then
          ncrclass=iltag/3
          do kk=1,ncrclass
            icount=(kk-1)*3+1
            ired(kk)=ival4(icount)/256
            igreen(kk)=ival4(icount+ncrclass)/256
            iblue(kk)=ival4(icount+2*ncrclass)/256
          enddo
        endif
c
c       data types for the image data
c         1 - unsigned integer
c         2 - signed integer
c         3 - real
c
        if(itag.eq.339) idtypei=ival4(1)
c
c       read indexing (equivalent to palette type)
c
        if(itag.eq.346) iptypidx=ival4(1)
c
c       read and decode GeoTIFF keys in extended tags
c
c       read the pixel scale (spacing)
c
        if(itag.eq.33550) then
          dxi=val8(1)
          dyi=val8(2)
        endif
c
c       read the reference point
c
        if(itag.eq.33922) then
          ixorgtif=int(val8(1))+1
          iyorgtif=int(val8(2))+1
          zorgtif=sngl(val8(3))
          xorg=sngl(val8(4))
          yorg=sngl(val8(5))
          zorg=sngl(val8(6))
        endif
c
c       GeoKey Directory
c
        if(itag.eq.34735) then
c
c         read the keys, starting with #2 (#1 is simply a header)
c
          do kk=5,iltag-3,4
c
c           get the ID, the TAG containg the key (0 if the value fits
c           in the 4th field), the number of values, and the element
c           offet in the containing tag (0 means first) or the actual
c           key value if it will fit
c
            keyid=ival4(kk)
            intag=ival4(kk+1)
            nkeyval=ival4(kk+2)
            keyval=ival4(kk+3)
            if(keyval.eq.32767) cycle
c
c           model type - 1 is projection coordinates, 2 is lat-lon
c
            if(keyid.eq.1024) icmodel=keyval
c
c           pixel type - 1 is area, 2 is point
c
            if(keyid.eq.1025.and.keyval.eq.2) lpixel=.false.
c
c           datum may be in 1 of 3 keys, the last for UTMs
c
            if(keyid.eq.2048) idatum=keyval-4200
            if(keyid.eq.2050) idatum=keyval-6200
            if(keyid.eq.3072) then
              iproj=1
              lutm=.true.
              idatum=keyval/100-200
              if(idatum.eq.67.or.idatum.eq.69) then
                iutmzni=mod(keyval,100)
                if(iutmzni.ge.3.and.iutmzni.le.23) then
                  utmhemi='N   '
                else
                  iutmzni=0
                endif
              endif
              if(idatum.gt.121.and.idatum.lt.127) then
                utmhemi='N   '
                if(mod(idatum,2).eq.1) then
                  utmhemi='S   '
                  idatum=idatum-1
                endif
              endif
              iutmzni=mod(keyval,100)
c
c             check for arbitrary zoning
c
              if(iutmzni.gt.60) iutmzni=0
            endif
c
c           coordinate, angle, elevation units
c             coords: 1 - meters, 2 - feet, 35 - mile,  36 - kilometers
c             angles: 2 - degrees, 3 - arc-minutes, 4 - arc-seconds
c              elevs: 1 - meters, 2 - feet
c
            if(keyid.eq.2052) icordunt=keyval-9000
            if(keyid.eq.2054) iangunt=keyval-9100
            if(keyid.eq.4099) ielevunt=keyval-9000
c
c           get the projection
c
            if(keyid.eq.3075) iproj=keyval
c
c           indices for various projection parameters in TAG 34736
c
            if(keyid.eq.3078) indxpar1=keyval+1
            if(keyid.eq.3079) indxpar2=keyval+1
            if(keyid.eq.3080) indxorlon=keyval+1
            if(keyid.eq.3081) indxorlat=keyval+1
            if(keyid.eq.3082) indxfalse=keyval+1
            if(keyid.eq.3083) indxfalsn=keyval+1
            if(keyid.eq.3088) indxctrlon=keyval+1
            if(keyid.eq.3089) indxctrlat=keyval+1
            if(keyid.eq.3092) indxtmscalo=keyval+1
            if(keyid.eq.3093) indxtmscalc=keyval+1
          enddo
        endif
c
c       extract projection parameters:
c         - equator-ward ref lat
c         - pole-ward reference latitude
c         - projection origin longitude
c         - projection origin latitude
c         - false easting
c         - false northing
c         - projection center longitude (overridden by origin)
c         - projection center latitude (overridden by origin)
c         - TM scaling at origin
c         - TM scaling at center (overridden by origin)
c
        if(itag.eq.34736) then
          if(indxpar1.gt.0) xlat1i=val8(indxpar1)
          if(indxpar2.gt.0) xlat2i=val8(indxpar2)
          if(indxorlon.gt.0) rloni=val8(indxorlon)
          if(indxorlat.gt.0) rlati=val8(indxorlat)
          if(indxfalse.gt.0) feasti=val8(indxfalse)
          if(indxfalsn.gt.0) fnorthi=val8(indxfalsn)
          if(indxctrlon.gt.0) ctrlon=val8(indxctrlon)
          if(indxctrlat.gt.0) ctrlat=val8(indxctrlat)
          if(indxtmscalo.gt.0) tmscalei=val8(indxtmscalo)
          if(indxtmscalc.gt.0) tmscalec=val8(indxtmscalc)
        endif
c
c       reset position to read next tag
c
        ipos=iopos
      enddo
c
c     select the datum and projection strings
c
      if(iproj.eq.-99.and.icmodel.eq.2) iproj=0
      if(idatum.gt.0) cdatumi=ctdatum(idatum)
      if(iproj.gt.-99.and.iproj.lt.15) cproji=ctproj(iproj+1)
      if(lutm) then
        cproji='UTM     '
        if(iutmzni.eq.0) lprojerr=.true.
      endif
c
c     equate indexing with palette
c
      if(iptypei.eq.0.and.iptypidx.eq.1) iptypei=3
c
c     handle overrides
c
      if(rloni.lt.-998..and.ctrlon.gt.-998.) rloni=ctrlon
      if(rlati.lt.-998..and.ctrlat.gt.-998.) rlati=ctrlat
      if(tmscalei.eq.1..and.tmscalec.ne.1.) tmscalei=tmscalec
c
c     test for supported TIFF values
c
      if(icompress.ne.1) lerror=.true.
c     note: iptypei value of 0 or 1 might occur for DEM files 
      if(iptypei.gt.3) lerror=.true.
      if(numpixv.ne.1.and.numpixv.ne.3) lerror=.true.
      if(idtypei.gt.3) lerror=.true.
      if(iorienti.ne.1.and.iorienti.ne.4) lerror=.true.
c
c     test for supported datum/projection
c
      if(cproji.eq.'        '.or.cdatumi.eq.'        ') lprojerr=.true.
c
c     print error messages and stop
c
      if(lerror.or.lprojerr) then
        write(*,*) 'ERROR: GEOTIFF PROCESSING STOPPED'
        if(lerror) then
          write(*,*) 'TIFF tag values for one of the following are'
          write(*,*) ' not supported in this application:'
          write(*,*) 'compression  : must be 1 (uncompressed)'
          write(*,*) 'photometric  : must be 2 or 3 or indexed'
          write(*,*) 'pixel number : must be 1 or 3'
          write(*,*) 'sample type  : must be 1 or 2 (integer)',
     &               ' or 3 (real)'
          write(*,*) 'orientation  : must be 1 or 4'
        endif
        if(lprojerr) then
          write(*,*) 'GEOTIFF datum or projection are not supported'
          write(*,*) ' in this application'
        endif
        stop
      endif
c
c     convert X-Y units if necessary, starting with lat-lon and then
c     coordinates - output units are decimal degrees or kilometers
c
      xmul=1.
      if(iangunt.eq.2) xmul=1.
      if(iangunt.eq.3) xmul=1./60.
      if(iangunt.eq.4) xmul=1./3600.
      rlati=rlati*xmul
      rloni=rloni*xmul
      xlat1i=xlat1i*xmul
      xlat2i=xlat2i*xmul
      if(icmodel.eq.1.and.ltiffreset) then
        xmul=0.001
        if(icordunt.eq.1) xmul=0.001
        if(icordunt.eq.2) xmul=0.0003048
        if(icordunt.eq.35) xmul=1.609344
        if(icordunt.eq.36) xmul=1.
      endif
      dxi=dxi*xmul
      dyi=dyi*xmul
      xorg=xorg*xmul
      yorg=yorg*xmul
      dyi=abs(dyi)
      if(iorienti.eq.1) dyi=-dyi
c
c     if pixel is an area, offset the origin to the center of the pixel
c
      if(lpixel) then
        xorg=xorg+dxi/2.
        yorg=yorg+dyi/2.
      endif
c
c     if origin is not pixel 1, offset the origin to the center of
c     pixel 1
c
      if(ixorgtif.ne.1.or.iyorgtif.ne.1) then
        xorg=xorg-(dxi*float(ixorgtif-1))
        yorg=yorg-(dyi*float(iyorgtif-1))
      endif
c
      zmul=1.
      if(ielevunt.eq.2) zmul=0.3048
c        
      itifftype=max(1,iptypei)
      return
      end
c-----------------------------------------------------------------------
      subroutine read1b(ioinp,ipos,ival1)
c-----------------------------------------------------------------------
c      
c --- TIFFINFO  Version: 1.03           Level: 061214          READ1B
c               K. Morrison, Hatch
c
c PURPOSE:      READ1B reads a single-byte integer in a direct-access
c               file and updates the pointer to the next byte
c
c INPUTS:  IOINP - the i/o unit of the data file
c           IPOS - the position in the file to read
c
c OUTPUTS:  IPOS - the position in the file incremented for the next read
c          IVAL1 - the integer*1 value as read
c
c-----------------------------------------------------------------------

      integer*1 ival
      read(ioinp,rec=ipos) ival
      ipos=ipos+1
      return
      end
c-----------------------------------------------------------------------
      subroutine read12b(ioinp,ipos,ival2,lbig)
c-----------------------------------------------------------------------
c      
c --- TIFFINFO  Version: 1.03           Level: 061214          READ12B
c               K. Morrison, Hatch
c
c PURPOSE:      READ12B reads a single-byte unsigned integer in a 
c               direct-access file into a signed two-byte integer,
c               and updates the pointer to the next byte
c
c INPUTS:  IOINP - the i/o unit of the data file
c           IPOS - the position in the file to read
c           LBIG - logical indicating if the machine is big-endian
c
c OUTPUTS:  IPOS - the position in the file incremented for the next read
c          IVAL2 - the integer*1 value as read and assigned to integer*2
c
c-----------------------------------------------------------------------

      integer*1 ival(2)
      integer*2 ival2,ivalt
      logical lbig
      equivalence (ivalt,ival(1))
      ivalt=0
      ind=1
      if(lbig) ind=2
      read(ioinp,rec=ipos) ival(ind)
      ipos=ipos+1
      ival2=ivalt
      return
      end
c-----------------------------------------------------------------------
      subroutine read1c(ioinp,ipos,char1)
c-----------------------------------------------------------------------
c      
c --- TIFFINFO  Version: 1.03           Level: 061214          READ1C
c               K. Morrison, Hatch
c
c PURPOSE:      READ1B reads a single-byte character in a direct-access
c               file and updates the pointer to the next byte
c
c INPUTS:  IOINP - the i/o unit of the data file
c           IPOS - the position in the file to read
c
c OUTPUTS:  IPOS - the position in the file incremented for the next read
c          CHAR1 - the characterr*1 value as read
c
c-----------------------------------------------------------------------

      character*1 char1
      read(ioinp,rec=ipos) char1
      ipos=ipos+1
      return
      end
c-----------------------------------------------------------------------
      subroutine read2b(ioinp,ipos,ival2,lflip)
c-----------------------------------------------------------------------
c      
c --- TIFFINFO  Version: 1.03           Level: 061214          READ2B
c               K. Morrison, Hatch
c
c PURPOSE:      READ2B reads a two-byte integer in a direct-access
c               file and updates the pointer to the next byte
c
c INPUTS:  IOINP - the i/o unit of the data file
c           IPOS - the position in the file to read
c          LFLIP - logical indicating if the file and the current machine
c                  have the same byte order
c
c OUTPUTS:  IPOS - the position in the file incremented for the next read
c          IVAL2 - the integer*1 value as read and assigned to integer*2
c
c-----------------------------------------------------------------------

      logical lflip
      integer*2 ival2,itmp2
      integer*1 itmp1(2)
      equivalence (itmp2,itmp1(1))
      if(lflip) then
        do i=2,1,-1
          read(ioinp,rec=ipos) itmp1(i)
          ipos=ipos+1
        enddo
      else
        do i=1,2
          read(ioinp,rec=ipos) itmp1(i)
          ipos=ipos+1
        enddo
      endif
      ival2=itmp2
      return
      end
c-----------------------------------------------------------------------
      subroutine read4b(ioinp,ipos,ival4,lflip)
c-----------------------------------------------------------------------
c      
c --- TIFFINFO  Version: 1.03           Level: 061214          READ4B
c               K. Morrison, Hatch
c
c PURPOSE:      READ4B reads a four-byte integer in a direct-access
c               file and updates the pointer to the next byte
c
c INPUTS:  IOINP - the i/o unit of the data file
c           IPOS - the position in the file to read
c          LFLIP - logical indicating if the file and the current machine
c                  have the same byte order
c
c OUTPUTS:  IPOS - the position in the file incremented for the next read
c          IVAL4 - the integer*4 value as read 
c
c-----------------------------------------------------------------------

      logical lflip
      integer*4 ival4,itmp4
      integer*1 itmp1(4)
      equivalence (itmp4,itmp1(1))
      if(lflip) then
        do i=4,1,-1
          read(ioinp,rec=ipos) itmp1(i)
          ipos=ipos+1
        enddo
      else
        do i=1,4
          read(ioinp,rec=ipos) itmp1(i)
          ipos=ipos+1
        enddo
      endif
      ival4=itmp4
      return
      end
c-----------------------------------------------------------------------
      subroutine read24b(ioinp,ipos,ival4,lflip,lbig)
c-----------------------------------------------------------------------
c      
c --- TIFFINFO  Version: 1.03           Level: 061214          READ24B
c               K. Morrison, Hatch
c
c PURPOSE:      READ24B reads a two-byte unsigned integer in a 
c               direct-access file into a signed four-byte integer,
c               and updates the pointer to the next byte
c
c INPUTS:  IOINP - the i/o unit of the data file
c           IPOS - the position in the file to read
c          LFLIP - logical indicating if the file and the current machine
c                  have the same byte order
c           LBIG - logical indicating if the machine is big-endian
c
c OUTPUTS:  IPOS - the position in the file incremented for the next read
c          IVAL4 - the integer*2 value as read and assigned to integer*4
c
c-----------------------------------------------------------------------

      logical lflip,lbig
      integer*4 ival4,itmp4
      integer*1 itmp1(4)
      equivalence (itmp4,itmp1(1))
      itmp4=0
      idi=1
      if(lflip) then
        idi=-1
        istart=2
        if(lbig) istart=4
      else
        istart=1
        if(lbig) istart=3
      endif
      iend=istart+idi
      do i=istart,iend,idi
        read(ioinp,rec=ipos) itmp1(i)
        ipos=ipos+1
      enddo
      ival4=itmp4
      return
      end
c-----------------------------------------------------------------------
      subroutine read4r(ioinp,ipos,val,lflip)
c-----------------------------------------------------------------------
c      
c --- TIFFINFO  Version: 1.03           Level: 061214          READ4R
c               K. Morrison, Hatch
c
c PURPOSE:      READ4R reads a four-byte real in a direct-access
c               file and updates the pointer to the next byte
c
c INPUTS:  IOINP - the i/o unit of the data file
c           IPOS - the position in the file to read
c          LFLIP - logical indicating if the file and the current machine
c                  have the same byte order
c
c OUTPUTS:  IPOS - the position in the file incremented for the next read
c            VAL - the real*4 value as read 
c
c-----------------------------------------------------------------------

      logical lflip
      integer*1 ival(4)
      real*4 val,valt
      equivalence (ival(1),valt)
      istart=1
      iend=4
      indx=1
      if(lflip) then
        istart=4
        iend=1
        indx=-1
      endif
      do i=istart,iend,indx
        read(ioinp,rec=ipos) ival(i)
        ipos=ipos+1
      enddo
      val=valt
      return
      end
c-----------------------------------------------------------------------
      subroutine read8r(ioinp,ipos,val,lflip)
c-----------------------------------------------------------------------
c      
c --- TIFFINFO  Version: 1.03           Level: 061214          READ8R
c               K. Morrison, Hatch
c
c PURPOSE:      READ8R reads an eight-byte real in a direct-access
c               file and updates the pointer to the next byte
c
c INPUTS:  IOINP - the i/o unit of the data file
c           IPOS - the position in the file to read
c          LFLIP - logical indicating if the file and the current machine
c                  have the same byte order
c
c OUTPUTS:  IPOS - the position in the file incremented for the next read
c            VAL - the real*8 value as read 
c
c-----------------------------------------------------------------------

      logical lflip
      integer*1 ival(8)
      real*8 val,valt
      equivalence (ival(1),valt)
      istart=1
      iend=8
      indx=1
      if(lflip) then
        istart=8
        iend=1
        indx=-1
      endif
      do i=istart,iend,indx
        read(ioinp,rec=ipos) ival(i)
        ipos=ipos+1
      enddo
      val=valt
      return
      end

c-----------------------------------------------------------------------
      subroutine readtiff(ioinp,kcnt,iclass,rval4,ipos)
c-----------------------------------------------------------------------
c      
c --- TIFFINFO  Version: 1.03           Level: 061214         READTIFF
c               K. Morrison, Hatch
c
c PURPOSE:      READTIFF reads the actual data in a GeoTIFF file,
c               pixel by pixel
c
c INPUTS:  IOINP - the i/o unit of the data file
c           KCNT - the position relative to the first value in the file
c                  to read
c          LFLIP - logical indicating if the file and the current machine
c                  have the same byte order
c
c OUTPUTS:  IPOS - the position in the file incremented for the next read
c         ICLASS - the integer*4 value as read
c            VAL - the real*4 value as read 
c
c-----------------------------------------------------------------------

      common /tiff_tags/ nbitsi,iptypei,idtypei,
     & idstarti,iorienti,numpixv,izorgtif,
     & icmodel,icordunit,iangunit,
     & lbig,lflip,zadd,zmul
c
      integer*2 ival2(3)
      integer ival4
      logical lflip,lbig
      real rval4
      real*8 rval8

      ival2=0
      ival4=0
      rval4=-999.
      rval8=-999.d0
c
c     branch based on the file type
c
c     RGB
c
      if(iptypei.eq.2) then
        if(numpixv.ne.3) then
          write(*,*) 'ERROR: Invalid number of values per pixel for'
          write(*,*) '       RGB GeoTIFF - only 3 values supported'
          stop
        endif
        ipos=idstarti + 1 + (kcnt-1)*3
c
c       read the triad RGB, and if 0-65535, reduce to 0-255
c
        do i=1,3
          if(nbitsi.eq.8) then
            call read12b(ioinp,ipos,ival2(i),lbig)
          else
            call read24b(ioinp,ipos,ival4,lflip,lbig)
            ival2(i)=ival4/256
          endif
        enddo
c
c       return value as 9 digits - RRRGGGBBB
c
        iclass=(ival2(1)*1000+ival2(2)) * 1000 + ival2(3)
        return
c
      else
c
c     palette or other
c
        if(idtypei.eq.1.or.idtypei.eq.2) then
          if(nbitsi.le.8) then
            ipos=idstarti + 1 + (kcnt-1)
            call read12b(ioinp,ipos,ival2(1),lbig)
            ival4=ival2(1)
          elseif(nbitsi.gt.8.and.nbitsi.le.16) then
            ipos=idstarti + 1 + (kcnt-1)*2
            call read24b(ioinp,ipos,ival4,lflip,lbig)
          else
            ipos=idstarti + 1 + (kcnt-1)*4
            call read4b(ioinp,ipos,ival4,lflip)
          endif
          iclass=ival4
c
c         DEM may have integer elevation
c
          if(iptypei.eq.1) rval4=(float(iclass)+zadd)*zmul
c
          return
c
c       real value - probably DEM
c
        elseif(idtypei.eq.3) then
          if(nbitsi.eq.32) then
            ipos=idstarti + 1 + (kcnt-1)*4
            call read4r(ioinp,ipos,rval4,lflip)
          else
            ipos=idstarti + 1 + (kcnt-1)*8
            call read8r(ioinp,ipos,rval8,lflip)
            rval4=sngl(rval8)
          endif
          rval4=(rval4+zadd)*zmul
          return
        else
          write(*,*) 'ERROR: Undefined image values not supported'
          stop
        endif
c
        return
c
      endif
      end
