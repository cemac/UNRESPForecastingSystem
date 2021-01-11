c CALETA source 
      include 'eta2m3d.par' 
      include 'eta2m3d.cm1' 
 
c ---------------------------------------------------------------- 
c      print *,'Input date (Such as 01092000 for 00 Sep 21, 2001): ' 
c      read(*,*)ipdate 
c      ipdate=02071506 
      integer type_input 
c 
 
      open(ict,file='timestmp.dat',status='old',action='read') 
      read(ict,*)ipdate 
c      print *,'Required date (YYMMDDHH. e.g. 01101200): ',ipdate 
      close(ict) 
 
c     Open user input control file 
 
      open(ict, file='eta2m3d.inp',status='old',action='read') 
 
c --- Read control file for title,input,output,log file names 
      read(ict,101) title  
      read(ict,*) type_input  
      read(ict,101) inbase 
      read(ict,101) outbase 
      read(ict,101) logfile  
 101  format (a) 
 
      call getflname(inbase,nti)     
      call getflname(outbase,nto)     
      call getflname(logfile,ntl) 
 
c     Open log file and output grib file 
      open (ilg,file=logfile,status='unknown') 
c      open (iout,file='readfile.dat',status='unknown') 
 
c     Echo control inputs to log file 
      write(ilg,1019)cver,clevel 
1019  format(1x,'ETA2M3D - Version: ',a8,3x,'Level: ',a8//) 
 
      write(ilg,101)title(1:nti) 
      write(ilg,1022)outbase(1:nto) 
      write(ilg,1023)logfile(1:ntl) 
      write(ilg,*) 
 1022 format('Output CALMM5/M3D file: ',a) 
 1023 format('ETA2M3D log file:     ',a) 
       
c     Selected domain range for CALMM5 data:  
C     (Lat1/Lat2, Lon1/Lon2 in decimal, < 0 for SH or WH) 
C     (Vertical levels: 1-27, 21 is 400 hPa) 
      read(ict,*)rlatmin,rlatmax         
      read(ict,*)rlonmin,rlonmax 
      read(ict,*)nz1,nz2 
 
      write(ilg,377)rlatmin,rlatmax 
      write(ilg,378)rlonmin,rlonmax 
      write(ilg,379)nz1,nz2 
 377  format('latitude  range:',3(4x,f7.2)) 
 378  format('longitude range:',3(2x,f9.2)) 
 379  format('Vertical Levels:',2(2x,i7)) 
 
      write(ilg,*) 
      if (rlatmin.ge.rlatmax) then 
         write(ilg,*)' Error: latitude min > latitude max' 
         write(ilg,*)' stop' 
         stop 
      endif 
      if (rlonmin.ge.rlonmax) then 
         write(ilg,*)' Error: longitude min > longitude max' 
         write(ilg,*)' stop' 
         stop 
      endif 
      if (nz1.ge.nz2) then 
         write(ilg,*)' Error: NZ min > NZ max' 
         write(ilg,*)' stop' 
         stop 
      endif 
C     Read in grid configuration for specified Grid  
C     from a look-up table 
      infile='grid.dat' 
      open(1,file=infile,status='old',action='read') 
 
      read(1,*)nx212,ny212 
      read(1,*)flats,flonv,re,conf 
c 
      do j=1,ny212
         do i=1,nx212
            read(1,*)ii,jj,(xy212(i1,i,j),i1=1,4)  
                        !order:x,y,flon,flat,elev,fm 
            if(ii.ne.i .or. jj.ne.j) then 
               print *,'Error in I/J: ',ii,jj,i,j 
               stop 
            endif 
c            ter(i,j)=xy212(5,i,j) 
         enddo 
      enddo 
 
      close(1) 
 
C     Read in terrain elevation in Texas area from a look up table 
 
C     Select I/J ranges based on required lat/lon 
 
      call getrange 
 
C     Open output file name based on required date 
      write(outfile,501) outbase(1:nto) 
 501  format(a,'out_caleta','.m3d') 
      write(outfile2,504) outbase(1:nto) 
 504  format(a,'out_caleta','.m2d') 
      open (im3d,file=outfile,status='unknown') 
      if(iosrf.eq.1)  
     &   open (im2d,file=outfile2,status='unknown') 
C --- Determine the number of GRIB files needed 
      nfhr=ipdate-(ipdate/100)*100 
      idhr=nfhr/6+1 
c --- What follows is for forcing to read monthly mean data 
c      idhr=4 
 
      if(idhr.lt.1 .or. idhr.gt.4) then 
         write(ilg,*)'Error in IDHR:',ipdate,nfhr,idhr 
         print *,'Error in IDHR:',ipdate,nfhr,idhr 
         stop 
      endif 
 
      nfile=nfiles(1,idhr) 
      nfile1=nfiles(2,idhr) 
      nfile2=nfiles(3,idhr) 
 
      write(ilg,*)'Grib files:',nfile,nfile1,nfile2 
      print *,'Grib files:',nfile,nfile1,nfile2 
C --- Loop over AWIPS grib files ------ 
      do 5000 ifile=nfile1, nfile2 
 
c --- ihrfst depends on the temporal resolution of meteo data
      ihrfst=(ifile-1)*3 
c      ihrfst=(ifile-1)*6 
c      if(ihrfst.le.9) then
      write(infile,502)inbase(1:nti),ihrfst,ipdate 
c       write(infile,502)inbase(1:nti), ihrfst 
 502  format(a,'VOLC_MAS',I2.2,I8.8,'.grib') 
c 502  format(a,'piton',I2.2,I8.8,'.grib') 
c 502   format(a,'hm_',I1.1,'.grib') 
c502    format(a,'gfs.t00z.master.grbf',I2.2)
c502    format(a,'output_etna_06122006_tot_',I2.2,'.grib')
c      else
c       write(infile,503)inbase(1:nti), ihrfst
c 503   format(a,'hm_',I2.2,'.grib') 
c      endif
c      
c  
      open(in,err=5500,file=infile,form='binary', 
     &    access='sequential'
     &   ,status='old',action='read') 
 
      nt=index(infile, ' ')-1 
      write(*,1021)ifile,infile(1:nt) 
      write(ilg,1021)ifile,infile(1:nt) 
 1021 format(i3,'th Input ETA GRIB file:   ',a) 
 
C     Initialize check arrays 
      do i=1,n3d 
         do k=1,mxnz 
            ichk3d(k,i)=0 
         enddo 
      enddo 
 
      do i=1,n2d 
         ichk2d(i)=0 
      enddo 
 
      ihr_fst=(ifile-1)*3 
 
c --- Decode ETA grib file 

      call degrib(type_input)    ! read/decode one grib file (for one model hour) 
 
      goto 5600 
 
 5500 print *,'Error to open file: ' 
      print *,infile 
      print *,'Processing stopped' 
      print *,'Get this file and restart the program' 
 
      write(ilg,*)'Error to open file: ' 
      write(ilg,*)infile 
      write(ilg,*)'Processing stopped' 
      write(ilg,*)'Get this file and restart the program' 
      stop 
 
 5600 close(in) 
 
      if(ifile.eq.nfile1) call wrthd 
 
C     Check whether all variables needed are there 
      do i=1,n3d 
         do k=1,mxnz 
            id=ichk3d(k,i) 
            if(id.eq.0) then 
               if(i.ne.3) then 
                  print *,'Warning: Zero 3D Var -',i,k 
c                  stop 
               elseif(k.le.23 .and. k.ne.1) then 
                  print *,'Warning: Zero 3D Var -',i,k 
c                  stop 
               endif 
            endif 
         enddo 
      enddo 
 
      do i=1,n2d 
         id=ichk2d(i) 
         if(id.eq.0) then 
            print *,'Warning: Zero 2D Var -',i 
c            stop 
         endif 
      enddo 
 
C     All variables are there 
C      Print *,'Find all variable for ',idate 
 
C     Output one-hour data in MM53D format 
      print *,'Output to M3D at ',idate 
      write(ilg,*)'Output to M3D at ',idate 
 
      do j=ny1,ny2 
      do i=nx1,nx2 
       if(type_input.eq.0) then  
          slp=1013.0 
       else  
         slp=x2d(i,j,1)/100. 
       endif 
c --- Sara 13/12/2004: if ip7 is equal to 61 then it is 
c --- reading the value for the total precipitation(kg/m^2) 
c --- so to have the value in cm it is necessary to divide 
c --- for the density of the rain that here is assumed  
c --- 1000kg/m^3 multiplied for 10^-1 
c 
         pre=x2d(i,j,2)*10E-01 
 
c         if(pre.ne.0) pre=pre/10.0    ! change unit to cm 
         if(pre.ne.0) pre=pre/3.0 ! change unit to cm/hr 
                           ! Precip is 3-hour accumulation 
         write(*,*) 'precipitation', pre
         snow=x2d(i,j,3) 
         if(snow.gt.0) then 
            isnow=1 
         else 
            isnow=0 
         endif 
 
         rswd=0 
         rlwd=0 
 
         write(im3d,98)idate,i,j,slp,pre,isnow,rswd,rlwd 
 98      format(i10,2i3,f7.1,1x,f8.4,i2,4f8.1) 
 
c        Loop over vertical levels (from surface to top) 
         do k=nz1,nz2 
c --- Sara 14/12/2004: data provided by lami (AM) contain values 
c --- for the specific humidity(sh) and not for the relative one 
c --- so it is necessary to compute the saturated vapour tension  
c --- with tetens formula (ECMWF) to obtain the relative humidity  
c 
          if(type_input.eq.1.or.type_input.eq.3) then 
            sh=x3d(i,j,k,3) 
            pp=pres(k) 
            ips=nint(pp) 
            izh=(nint(x3d(i,j,k,1)))/9.8 
            atk=x3d(i,j,k,2) 
            rh=relhum(atk,sh,pp) 
c            rh=x3d(i,j,k,3) 
          elseif(type_input.eq.2) then 
            pp=pres(k) 
            ips=nint(pp) 
            izh=(nint(x3d(i,j,k,1)))/9.8 
            atk=x3d(i,j,k,2) 
            rh=x3d(i,j,k,3) 
          elseif(type_input.eq.0) then 
            pp=pres(k) 
            ips=nint(pp) 
            izh=(nint(x3d(i,j,k,1))) 
            atk=x3d(i,j,k,2) 
            rh=x3d(i,j,k,3) 
          elseif(type_input.eq.4) then 
            sh=x3d(i,j,k,3) 
            pp=pres(k) 
            ips=nint(pp) 
            izh=(nint(x3d(i,j,k,1)))/9.8 
            atk=x3d(i,j,k,2) 
            rh=relhum(atk,sh,pp) 
          endif 
c            if(k.eq.1 .and. rh.eq.0) rh=x3d(i,j,k+1,3) 
            irh=nint(rh) 
            irh=min(100,irh) 
c 
          if(type_input.eq.2) then 
            uur=x3d(i,j,k,4) 
            vvr=x3d(i,j,k,5) 
            lats=xy212(4,i,j) 
            lons=xy212(3,i,j) 
c 
            stpho=sind(tlm0d) 
            ctpho=cosd(tlm0d) 
c 
            call rltlwd(lons,lats,uur,vvr,tlm0d,ctpho,stpho,uu,vv) 
          else 
            uu=x3d(i,j,k,4) 
            vv=x3d(i,j,k,5) 
          endif 
c 
            call uv2ws(uu,vv,ws,wd) 
 
            flon=xy212(3,i,j) 
            flat=xy212(4,i,j) 
 
            call rotate(ws,wd,flat,flon) 
 
            iwd=nint(wd) 
            if(iwd.eq.0) iwd=360 
 
            wp=x3d(i,j,k,6) 
            call getw(wp,pp,atk,rh,ww,aq) 
c --- SOLO PER IL RUN LAMI 2001 
c            ww=x3d(i,j,k,6) 
 
            write(im3d,92)ips,izh,atk,iwd,ws,ww,irh,aq 
 92         format(i4,i6,f6.1,i4,f5.1,f6.3,i3,f5.2) 
         enddo 
 
      enddo 
      enddo 
 
 5000 continue 
 
      close(ict) 
      close(ilg) 
      close(im3d) 
c      close(iout) 
      if(iosrf.eq.1) close(im2d) 
 
      print *,'Processing succeeded' 
      print *,'--------------------' 
 
      stop 
      end 
 
      include 'eta2m3d.blk' 
 
 
c *********************************************************************** 
 
      subroutine degrib(type_input) 
 
c Obtained from NOAA/NCEP Office Note 338 - March 10, 1998 version 
c Modified by Zhong-Xiang Wu to fit the gribtyc.f (8/5/99) 
c 
c New main program to solve 'GRIB' in BDS problem   19971105 
c 
c This GRIB decoder has been tested on SGI/iris4, HP/ux, SUN/sunOS4.1.3 and 
c     DEC/Alpha. 
c 
c maximum byte array expected: 60000*8 
c 
      include 'eta2m3d.par' 
      include 'eta2m3d.cm1' 
 
      parameter (nnx=720,nny=361,nmax=nnx*nny)  ! global, 0.5 degree 
      parameter (mxrd=1024)           ! number of bytes in each read, was 1024 
      parameter (nbit=32)             ! number of bits in a word, 
      parameter (nword=nbit/8)        ! number of bytes in a word, 
c      parameter (mxcray=60000)        ! max number of words for a GRIB on Cray 
      parameter (mxcray=300000)        ! max number of words for a GRIB on Cray 
      parameter (mxlen=mxcray*8/nword) ! max number of words for a GRIB on Sun 
      parameter (mxone=mxrd/nword)     ! max length of ione 
      dimension ingrib(mxlen), ione(mxone) 
c     character one*(mxrd),inf*72, two*(mxcray) 
      character one*1024,inf*80, two*4000000 
      character*132 asum 
      character*1 str1 
      integer*4 laspos 
      integer*4 ivarout 
      integer*4 iliv 
      integer*4 idati 
      integer*4 ibloc(4),lengrib,gribedi 
      integer type_input 
      logical   newgrib 
      equivalence (one,ione),(two,ingrib) 
      save 
 
c --------------------------------------------------------------- 
 
      isel=1 
      isum=1 
      iprnt=0  ! Print (1) or not (0) detail info 
      mgrib=0 
 
      istart=1 
c 
      ngrib=0 
      laspos=0 
      one=' ' 
      newgrib=.false. 
      do i=1,3 
c        ist=fgetc(in,one(i:i)) 
c        if(ist.ne.0) go to 9998 
         read(in,err=9998)str1 
         one(i:i)=str1 
         laspos=laspos+1 
      enddo 
      two(1:3)=one(1:3) 
 
   10 continue 
      one(1:3)=two(1:3) 
      indxt=0 
c      ist=fgetc(in,one(4:4)) 
c      if(ist.ne.0) go to 9998 
      read(in,err=9998,end=9999)str1 
      one(4:4)=str1 
      laspos=laspos+1 
      if(one(1:4).eq.'GRIB') then 
        ibeg=laspos-4 
        if(isel.ne.1) write(*,*) '....found one GRIB, starts ',ibeg 
        do ka=5,8 
c          ist=fgetc(in,one(ka:ka)) 
c          if(ist.ne.0) go to 9998 
           read(in,err=9998)str1 
           one(ka:ka)=str1 
           laspos=laspos+1 
        enddo 
        two(1:8)=one(1:8) 
 
        indxt=8 
c 
c GRIB x x x  1 -------> GRIB Edition 1 
c GRIB 0 0 24 0 -------> ECMWF GRIB Edition 0 
c GRIB 98       -------> ECMWF GRIB Edition X, no lenGRIB 
c 
c swap one after 'GRIB' since GRIB is in ascii       ! DEC_ALPHA 
        call swap4(ione(2),ione(2),4)                ! DEC_ALPHA 
        call gbytes(one,ibloc,32,8,0,4) 
        if(iprnt.ne.0) write(ilg,*) ' ibloc 4: ',(ibloc(jk),jk=1,4) 
        if(ibloc(1).ne.98 .and. ibloc(4).eq.1) then 
          call gbyte(one,lengrib,32,24) 
          if(iprnt.ne.0) write(ilg,*) ' GRIB Edition 1 ',lengrib 
          gribedi=1 
          newgrib=.true. 
        elseif(ibloc(1).eq.0 .and. ibloc(2).eq.0 .and.  
     &    ibloc(3).eq.24 .and. ibloc(4).eq.0 ) then 
          call gbyte(one,lenpds,32,24) 
          if(iprnt.ne.0) write(ilg,*) ' ECMWF GRIB Edition 0 ',lenpds 
          gribedi=0 
          newgrib=.true. 
        elseif(ibloc(1).eq.98) then 
          if(iprnt.ne.0) write(ilg,*) ' ECMWF GRIB Edition X ' 
          gribedi=-1 
          lenpds=20 
          newgrib=.true. 
        else              !wrong GRIB at wrong place, probably data 
          newgrib=.false. 
        endif 
 
        if(gribedi.eq.1 .and. indxt.lt.lengrib) then 
          do i=indxt+1,lengrib 
c            ist=fgetc(in,two(i:i)) 
c            if(ist.ne.0) go to 9998 
             read(in,err=9998)str1 
             two(i:i)=str1 
             laspos=laspos+1 
          enddo 
 
          ngrib=ngrib+1 
          if(ngrib.lt.mgrib) go to 10 
 
          indxt=(lengrib+3)/4 
 
          asum=' ' 
          if(isum.ne.0) write(asum(1:6),'(i6)') ngrib 
c 
          call swap4(ingrib(2),ingrib(2),lengrib-4)     ! DEC_ALPHA 1998MAY06 
c 
c      write(*,*) 'sono qui' 
          call genGRIB1_decode(iprnt,isum,mxlen,nbit,nword,type_input, 
c          call genGRIB1_decode(iprnt,isum, 
     &                         ivarout,iliv,idati,indxt,asum,ingrib) 
          if(isum.ne.0) write(asum(117:131),'(i15)') ibeg 
c          if(isum.ne.0) write(*,'(a)') asum(1:132) 
          if(isel.ne.1) then 
c            write(*,*) 'enter 1 to continue?' 
c            read(*,*) imore 
            imore=1     !ZWU 
            if(imore.ne.1) call exit(0) 
          endif 
          two(1:3)=two(lengrib-2:lengrib) 
        else 
          two(1:3)=one(2:4) 
        endif 
      else 
        two(1:3)=one(2:4) 
      endif           
 
c      if(ngrib.lt.87) go to 10 
c      if(isetup.eq.1) then 
c         goto 9998 
c      else 
         go to 10 
c      endif 
 
 9998 continue 
      print *,'Read file Error' 
      goto 9997 
 
 9999 print *,'End of file' 
 
 9997 close(in) 
 
      return 
      end 
 
c -------------------------------------------------------------------- 
      subroutine genGRIB1_decode(iprnt,isum,mxlen,nbit,nword,type_input, 
     &                           ivarout,iliv,idati,lnin,asum,ingrib) 
c 
c This has been changed on 19971002 
c iprnt: 0, no debug 
c        1, debug mode, print extra information 
c isum:  0, no summary 
c        1, prepare one line summary in asum 
c 
c maximum byte array expected: 60000*8 
c 
c basic logic: 
c  (1) read one complete GRIB record       (machine dependent) 
c  (2) decode PDS 
c  (3) decode GDS 
c  (4) decode BMS if there 
c  (5) decode BDS 
c  if more processing, repeat steps (1) to (5) 
 
      include 'eta2m3d.par' 
      include 'eta2m3d.cm1' 
 
      parameter (nnx=980,nny=390,nmax=nnx*nny) ! global, 0.5 degree 
c      parameter (nbit=32)                       ! number of bits in a word, 
c      parameter (nword=nbit/8)                  ! number of bytes in a word, 
c      parameter (mxcray=60000)        ! max number of words for a GRIB on Cray 
c      parameter (mxlen=mxcray*8/nword)! max number of words for a GRIB on Sun 
 
      dimension ingrib(mxlen) 
      integer ione 
      integer type_input 
      integer*4 idum,ioff,iskp,ibloc(4) 
      character*4 one, short*5 
      character*132 asum 
      equivalence (ione,one) 
c PDS  
      integer*4 lengrib,gribedi,pds(22),kgds,kbms,lvl2 
c GDS used 
      integer*4 lengds 
c BMS used 
      integer*4 lenbms, mapuse 
      logical*1 mapb(nmax) 
c BDS used 
      integer*4 lenbds,npoints,ix(nmax) 
      dimension datan(nmax) 
c     dimension grd1(nmax), grd2(144,73)        ! for select a region use 
 
      save 
 
c -------------------------------------------------------------------- 
      do i=1,nmax 
         ix(i)=0 
         datan(i)=0. 
      enddo 
c 
c read in one GRIB record until the next 'GRIB' 
c 
c To handle blank records of "NCAR NMC 2.5 degree global analysis grids", 
c  use direct access and check for 'GRIB'. If no 'GRIB', ierr=-9. 
c 
      ioff=0 
      do i=1,lnin 
        ione=ingrib(i) 
        if(one.eq.'GRIB') go to 15 
      enddo 
      write(*,*) '****WARNING, no GRIB found in this grib record.' 
      return 
c 
   15 continue 
      ioff=i-1 
c ioff is the number of bytes before 'GRIB' 
      if(iprnt.ne.0) write(*,*) ' One GRIB starts at: ',ioff 
c 
c check to see if NMC or ECMWF 
c 
      iskp=(ioff+4)*8 
      call gbytes(ingrib,ibloc,iskp,8,0,4)   
      if(ibloc(4).eq.1 .and. ibloc(1).ne.98) then 
        call gbyte(ingrib,lengrib,iskp,24) 
        call gbyte(ingrib,gribedi,iskp+24,8) 
        if(iprnt.ne.0) write(*,*) '.. GRIB Edition 1 ' 
        ioff=ioff+8 
        iskp=ioff*8 
        call gbyte(ingrib,lenpds,iskp,24) 
      elseif(ibloc(1).eq.0 .and. ibloc(2).eq.0 .and. 
     &       ibloc(3).eq.24 .and. ibloc(4).eq.0 ) then 
        call gbyte(ingrib,lengrib,iskp,24) 
        call gbyte(ingrib,gribedi,iskp+24,8) 
        if(iprnt.ne.0) write(*,*) '.. ECMWF GRIB Edition 0 ' 
        lenpds=lengrib 
        ioff=ioff+4 
        iskp=ioff*8 
      elseif(ibloc(1).eq.98) then 
        if(iprnt.ne.0) write(*,*) '.. ECMWF GRIB Edition X ' 
        gribedi=-1 
        lenpds=20 
        ioff=4 
        iskp=ioff*8 
      else 
        write(*,*) '..unknown GRIB record, exit ',lengrib,idum 
        call exit(0) 
      endif 
      if(iprnt.ne.0) write(*,*) ' GRIB, length,edi= ',lengrib,gribedi 
      idum=lenpds+ioff 
c 
c at this time, ioff is the number of bytes before PDS 
      if(iprnt.ne.0) write(*,*) 'lengrib,lenpds,ioff: ',lengrib,lenpds,ioff 
c 
c begin to process GRIB 
c to check BLOK, loop through the whole ingrib array 
c 
c 
c ioff is the number of bytes before PDS 
c iskp is the number of bits before PDS 
c 
c process PDS 
c 
      call pds_GRIB1(iprnt,gribedi,iskp,idum, 
     &   kgds,kbms,dscalf,ierr,lvl2,pds,ingrib) 
 
 
c To handle old GRIB Edition from ECMWF 
c     if(gribedi.eq.-1) then 
c       call gbyte(ingrib,kgds,7*8+7,1) 
c       call gbyte(ingrib,kbms,7*8+6,1) 
c       write(*,*) '....... revised kgds,kbms .....' 
c       write(*,*) '.... kgds = ',kgds 
c       write(*,*) '.... kbms = ',kbms 
c       call gbytes(ingrib,ibloc,10*8,8,0,2) 
c       if(pds(8).eq.100 .or. pds(8).eq.103 .or. 
c    &     pds(8).eq.105 .or. pds(8).eq.107 .or. 
c    &     pds(8).eq.109 .or. pds(8).eq.111 .or. 
c    &     pds(8).eq.113 .or. pds(8).eq.125 .or. 
c    &     pds(8).eq.160 .or. pds(8).eq.200 .or. 
c    &     pds(8).eq.201 ) then 
c         pds(9)=ibloc(1)*32+ibloc(2) 
c         write(*,*) '....... revised level.....' 
c         write(*,*) '....level = ',pds(9) 
c       endif 
c     endif 
c 
 
 
c 
c     Determine time flag 
      ip7=pds(7)                ! var ID 
      ip8=pds(8)                ! level/layer type 
      ip9=pds(9)                ! pressur/height 
 
      icen=pds(21) 
      iyr=pds(10) 
      imon=pds(11) 
      iday=pds(12) 
      ihr=pds(13) 
      iunit=pds(15) 
      iforecast1=pds(16) 
      iforecast2=pds(17) 
      itrange=pds(18) 
      ips19=pds(19) 
      ips20=pds(20) 
 
C   ....Per convertire la tab.2 ecmwf a quella del NOAA.... 
c        write(*,*) pds(7),ip7 
        if(type_input.eq.3) then 
          if(ip7.eq.129.or.ip7.eq.156) then 
                    ip7=7 
          endif 
          if(ip7.eq.130)  ip7=11 
          if(ip7.eq.131)  ip7=33 
          if(ip7.eq.132)  ip7=34 
          if(ip7.eq.133)  ip7=51 
          if(ip7.eq.135)  ip7=39 
          if(ip7.eq.138)  ip7=43 
          if(ip7.eq.155)  ip7=44 
          if(ip7.eq.157)  ip7=52 
          if(ip7.eq.151)  ip7=2 
c          if(ip7.eq.134)  ip7=2 
          if(ip7.eq.228)  ip7=61 
        endif 
C.............SCRIVE I parametri del PDS sul log file..... 
      do i=1,20 
         write(ilg,*)'pds:',i,pds(i) 
      enddo 
       
 
      if(iforecast2.ne.-9999) then 
         nhrinc=iunit*(iforecast1+iforecast2) 
      else 
         nhrinc=iunit*(iforecast1) 
      endif 
 
      call julday(iyr,imon,iday,ijuldy) 
      call incr(iyr,ijuldy,ihr,nhrinc) 
      call grday(iyr,ijuldy,imon,iday) 
      call timestamp(iyr,imon,iday,ihr,idateout) 
 
      if(istart.eq.1) then 
         idate=idateout 
         istart=0 
      endif 
c
c process GDS 
c 
      if(kgds.eq.0) then 
        lengds=0 
        go to 40 
      endif 
      iskp=(pds(1)+ioff)*8 
      call gbyte(ingrib,lengds,iskp,24) 
      if(iprnt.ne.0) write(*,*) ' GDS length: ',lengds 
 
      call gds_GRIB1(iprnt,gribedi,lnin,iskp, 
     &    kgds,ierr,nx,ny,la1,lo1,pds,ingrib,lov,idx,idy,igds6) 
       
 
c      print *,'Valid time: ',idateout,pds(7),pds(9) 
 
c BMS section 
c 
 40   continue 
      mapuse=0 
      if(kbms.eq.0) then 
        lenbms=0 
c       go to 50 
      else 
        iskp=(lengds+pds(1)+ioff)*8 
        call gbyte(ingrib,lenbms,iskp,24) 
      endif 
      call bms_GRIB1(iprnt,lnin,nmax,iskp,kbms,nx, 
     &  ny,la1,lo1,ierr,numgd,mapuse,pds,ingrib,mapb) 
 
      if(iprnt.ne.0) write(*,*) ' BMS length: ',lenbms 
c 
c BDS section 
c 
   50 continue 
      iskp=(lenbms+lengds+pds(1)+ioff)*8 
      call gbyte(ingrib,lenbds,iskp,24) 
      if(iprnt.ne.0) write(*,*) '     bdslen: ',lenbds 
c 
c check the total length with GRIB header 
c 
      if(pds(3).eq.98 .and. gribedi.ne.1) then      ! ECMWF edition 0 or X ' 
        lenall=(4+lenpds+lengds+lenbms+lenbds+4)/1  ! 'GRIB+...+7777' 
      else 
        lenall=(8+pds(1)+lengds+lenbms+lenbds+4)/1 
      endif 
      if(iprnt.ne.0) write(*,*) 'lenall,pds(1),gds,bms,bds: ', 
     &     ioff,pds(1),lengds,lenbms,lenbds,lenall 
      short=' ' 
      if((lnin*nword).lt.lenall) then 
        write(*,*) ' *** SHORT GRIB record ',lnin*nword,lenall 
        if((lenall-(lnin*nword)).gt.4) then 
          write(*,*) ' *** no bds_GRIB1, return ' 
          return 
        endif 
      endif 
      if(iprnt.ne.0) then 
c        write(*,*) 'enter 1 to continue ...' 
c        read(*,*) imore 
        imore=1                 !ZWU 
        if(imore.ne.1) go to 9998 
      endif 
      if(iprnt.ne.0) write(*,*) '..reading BDS, iskp = ',iskp 
c 
      call bds_GRIB1(iprnt,gribedi,ivarout,iliv,idati, 
     &   lnin,iskp,dscalf,ierr,nmax, 
     &  npoints,rc00,rmax,rmin,numgd,mapuse,mapb,pds,ingrib, 
     &  ix,datan) 
 
      if(ierr.eq.-3) then 
        write(*,*) '****Too many grids than specified..' 
        ierr=0 
        return 
      endif 
      if(ierr.eq.-8) then 
        write(*,*) '****Constant field, all grids = ',datan(1) 
        write(*,*) '    No number of grids can be computed from bds' 
        write(*,*) '    Use number of grids from gds section' 
        ierr=0 
        return 
      endif 
      if(ierr.eq.-11) then 
        write(*,*) '****UNKNOWN predefined grid type, ' 
        write(*,*) '    No grids decoded' 
        ierr=0 
        return 
      endif 
      if(ierr.eq.-12) then 
        write(*,*) '****COMPLEX PACKING of spherical harmonic ' 
        write(*,*) '    coefficients, to be developed!' 
        write(*,*) '    No grids decoded' 
        ierr=0 
        return 
      endif 
      if(iprnt.ne.0) write(*,83) (datan(k),k=1,15) 
 83   format(4(E15.8,1X),E15.8) 

C     Convert 1-D datan to a 2-D array and save it for selected variables.  
C     There are some variables occurring more than once. Using those only 
C     consistent with forecast-hour in file name 
      if(ip7.ge.61 .and. ip7.le.65) then 
         if(iforecast1.ne.ihr_fst) then 
            print 9,ihr_fst,iforecast2,ip7,ip8,ip9 
            goto 9998 
         endif 
c --- Sara 27/01/2005: to read the data coming form rianalysis 
 
      elseif(iforecast1.ne.ihr_fst) then 
         write(*,*) 'ifor1',iforecast1 
         write(*,*) 'ifor2', iforecast2 
         write(*,*) 'ihr', ihr_fst 
         print 9,ihr_fst,iforecast1,ip7,ip8,ip9 
         goto 9998 
      endif 
 9    format('Variable Not Needed:',6i8) 
 
C     3-D variable 
      do ivar=1,n3d 
         id=id3d(ivar) 
         if(ip7.ne.id) goto 2000 
         if(ip7.eq.7.and. ip8.eq.1 .and. ip9.eq.0) then   ! GEOPH at surface 
            call pass2d_ter(datan,nmax,npoints) 
            goto 2000 
         endif 
         if(mapuse.ne.0 .and. mapuse.ne.(nx*ny)) then 
            write(*,*) 'ERROR: bitmap is used:',mapuse,nx*ny,nx,ny 
            call exit(0) 
         endif 
 
         call getz(ip9,iz,ifound) 
          
 
         if(ifound.eq.0) goto 2000 
 
         call pass2d(datan,nmax,npoints,iz,ivar) 
 
         ichk3d(iz,ivar)=1 
 
         write(*,201)ip7,ip8,ip9,idateout 
c         write(*,*) 'ifor1',iforecast1 
c         write(*,*) 'ifor2', iforecast2 
c         write(*,*) 'ihr', ihr_fst 
         write(ilg,201)ip7,ip8,ip9,idateout 
 201     format('3D Variable selected: ',3i6,i12) 
 
 2000    continue 
 
      enddo 
 
C     2-D variable 
      do ivar=1,n2d 
         id=id2d(ivar) 
 
         if(ip7.ne.id) goto 3000 
 
         if(mapuse.ne.0 .and. mapuse.ne.(nx*ny)) then 
            write(*,*) 'ERROR: bitmap is used:',mapuse,nx*ny,nx,ny 
c            call exit(0) 
         endif 
 
         call pass2d_2(datan,nmax,npoints,ivar) 
         ichk2d(ivar)=1 
 
         write(*,202)ip7,ip8,ip9 
         write(ilg,202)ip7,ip8,ip9 
 202     format('2D Variable selected: ',5i6) 
 
 3000    continue 
 
      enddo 
 
C *************************************************************** 
 
 9998 continue 
      if(iprnt.ne.0) 
     &  write(*,*) ' finish reading input grib record....' 
c 
c make one line summary 
c 
      if(isum.ne.0) 
     &  write(asum(7:132),3213) (pds(i),i=1,5),(pds(j),j=7,9), 
     &    lvl2,(pds(i),i=10,13),(pds(k),k=15,18),pds(21), 
     &    pds(22),lenbds,npoints,rmax,rmin,short 
 3213 format(i3,i4,2i3,3i4,2i5,i3,3i2.2,2i4,i5,i4,2i5,i8,i6, 
     &       2f9.1,1x,a5,18x) 
c3213 format(6x,i3,i4,2i3,3i4,2i5,i3,3i2.2,4i4,2i5,i8,i6,2f9.1,1x,a5) 
c    nn 32   2  7 80  2   7 100 1000    0 91070600   1   0   0  10  20   0   13152 
c10512+999999.9+999999.9 short 
c123456iiijjjj1231231234iiiijjjj1234512345kkkiijjii12341234mmmmnnnnkkkkjjjj12345678123456fffffffffgggggggggxaaaaa 
c12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890 
c0        1         2         3         4         5         6         7         8         9 
c 
c------------------------------------- select a region, say 40N-20S, 130W-70W 
c 1. Check mapuse to detemine if a bitmap is used. 
c    If a bitmap is used, npoints will be less than (nx*ny), so fill to grd1(nx*ny). 
c 2. Remap grd1(1..npoints) to grd2(nx,ny).  If grd1 are gaussian grids, convert  
c    them to 2.5x2.5 lat,long grids. 
c 3. Print the location of grd1(1), i.e. la1,lo1 
c 4. Determine the starting,ending (i,j) on 2.5x2.5 
c 5. Save the regional grids ((grd2(i,j),i=ibeg,iend),j=jbeg,jend) 
c 
c step 1: 
c     If(mapuse.gt.0) then 
c       if(mapuse.ne.(nx*ny)) then 
c         write(*,*) '**** ERROR in select a region w/ bitmap ',mapuse,nx*ny,nx,ny 
c         call exit(0) 
c       endif 
c       kcnt=0 
c       do i=1,mapuse 
c         if(mapb(i).eq.1) then 
c            kcnt=kcnt+1 
c            grd1(i)=datan(kcnt) 
c         else 
c            grd1(i)=-9999. 
c         endif 
c       enddo 
c     else 
c       if(npoints.ne.(nx*ny)) then 
c         write(*,*) '**** ERROR in select a region, ',npoints,nx*ny,nx,ny 
c         call exit(0) 
c       endif 
c       do i=1,npoints 
c         grd1(i)=datan(i) 
c       enddo 
c     endif 
c 
c step 2: 
c     if((nx*ny).ne.(144*73)) then 
c       if((nx.ne.192) .or. (ny.ne.94)) then 
c         write(*,*) '****ERROR, select a region only works for ' 
c         write(*,*) '           144x73 (2.5x2.5 degree) grids, or' 
c         write(*,*) '           192x94 (gaussian) grids now......' 
c         call exit(0) 
c       endif 
c       call interpolation(nx,ny,grd2,grd1) 
c     else 
c       do j=1,ny 
c         do i=1,nx 
c           grd2(i,j)=grd1((j-1)*nx+i) 
c         enddo 
c       enddo 
c     endif 
c 
c step 3: 
c     xlat1=float(la1)/1000. 
c     xlon1=float(lo1)/1000. 
c     write(*,*) '....grd2(1,1) is located at lat,long ',xlat1,xlon1 
c 
c step 4:  
c     if((la1 .ne. 90000) .or. (lo1 .ne. 0)) then 
c       write(*,*) '****WARNING, adjust la1,lo1 to 90N,0E ' 
c       xlat1=90. 
c       xlon1=0. 
c     endif 
c     slat=40. 
c     elat=-20. 
c     slong=360.-130. 
c     elong=360.-70. 
c     ibeg=(slong-xlon1)/2.5+1 
c     iend=(elong-xlon1)/2.5+1 
c     jbeg=(xlat1-slat)/2.5+1 
c     jend=(xlat1-elat)/2.5+1 
c 
c step 5: save data 
c     write(*,*) ((grd2(i,j),i=ibeg,iend),j=jbeg,jend) 
c 
c------------------------------------- select a region 
 
      return 
      end 
 
c 
c 
c-- lines below are from ncep_int.f which is available in our anonymous ftp  
c   area on ncardata.ucar.edu in 'ftp/datasets/ds090.0/ncep_int.f' file. 
c 
      subroutine interpolation (nnx,nny,grdo2,grdi) 
c 
c This program demonstrates how to use the interpolation routines  
c (same as NCEPs) to get 2.5x2.5 grids from Gaussian (192x94) grids. 
c 
      parameter (mxx=192, mxy=94, imo=144,jmo=73) 
      dimension grdi(mxx*mxy), grdo(imo*jmo),grdo2(imo,jmo) 
      dimension gglat(mxy/2), bmapi(mxx*mxy),bmapo(mxx*mxy) 
      real PK(mxy/2),PKM1(mxy/2) 
      real SJN(mxy/2),WJN(mxy/2),CJN1(mxy/2) 
      real AJN(mxy+1),AJO(jmo+1),XIO(imo+1),XJO(jmo+1) 
      real WIO1(imo),WIO2(imo) 
      integer JPO(jmo) 
      real FPX(imo),FP0X(imo),WPX(imo),WTX(imo) 
c     character*72 inf, outf 
c 
c The file 'T62.gauss_lat' is available from anonymous ftp on ncardata.ucar.edu 
c under 'ftp/datasets/ds090.0/' directory.  It can be generated here as well. 
c 
c     open(unit=21,file='T62.gauss_lat') 
c     do i=1,14 
c       read(21,*) 
c     enddo 
c     do i=1,47 
c       read(21,*) idum,gglat(i) 
c     enddo 
c     close(unit=21) 
c 
      gglat(1)=0. 
c 
c     kin=1 
c     write(*,*) 'enter input data file?' 
c     read(*,'(a)') inf 
c     open(kin,file=inf) 
c     write(*,*) 'enter input data dimension? <192,94>' 
c     read(*,*) nnx,nny 
      if(nnx.gt.mxx .or. (nny.gt.mxy)) then 
        write(*,*) '****input dimension too large, exit' 
        write(*,*) nnx,nny,'   ',mxx,mxy 
        call exit(0) 
      endif 
      rmax=-999999.0 
      rmin= 999999.0 
      k=0 
      do j=1,nny 
        do i=1,nnx 
          k=k+1 
c         read(kin,*) idum,grdi(k) 
          if(grdi(k).ge.rmax) then 
            rmax=grdi(k) 
c           kmax=idum 
          endif 
          if(grdi(k).le.rmin) then 
            rmin=grdi(k) 
c           kmin=idum 
          endif 
        enddo 
      enddo 
c     close(kin) 
c     write(*,*) 'last idum = ',idum 
c     write(*,*) 'max: ',kmax,rmax,'  min: ',kmin,rmin 
c 
      mp=0 !0:scalar, 1:vector, 2:flag, 3:budget 
      mb=0 !0:no bitmap 
      imn=192 
      ixn=1 
      jmn=94 
      jxn=192 
      ixo=1 
      jxo=144 
      cio=0.    ! starting longitude in degrees East 
      cjo=90.   ! starting latitude in degrees North 
      dio=2.5   ! longitude increment in degrees East 
      djo=-2.5  ! latitude increment in degrees North 
      call gg2ll(mp,mb,nnx,ixn,nny,jxn,gglat,bmapi,grdi, 
     &           imo,ixo,jmo,jxo,cio,cjo,dio,djo,bmapo,grdo, 
     &           sjn,wjn,cjn1,ajn,ajo,xio,xjo,wio1,wio2,jpo, 
     &           fpx,wpx,wtx,fp0x,PK,PKM1) 
c 
c     do j=1,jmn/2 
c       write(*,*) ' Gaussian lat ',j,gglat(j) 
c     enddo 
      k=0 
      do j=1,jmo 
        do i=1,imo 
          k=k+1 
          grdo2(i,j)=grdo(k) 
        enddo 
      enddo 
c 
c save interpolated grids into a GrADS (or IEEE) data file 
c 
c     write(*,*) 'enter output data file?' 
c     read(*,'(a)') outf 
c     open(unit=23,file=outf,form='unformatted',access='direct', 
c    &     recl=imo*jmo*4) 
c     write(23,rec=1) ((grdo2(i,j),i=1,imo),j=jmo,1,-1) 
c     close(unit=23) 
c     stop 
c 
      return 
      end 
C----------------------------------------------------------------------- 
CFPP$ NOCONCUR R 
      SUBROUTINE GG2LL(MP,MB,IMN,IXN,JMN,JXN,CJN,LN,FN, 
     &                 IMO,IXO,JMO,JXO,CIO,CJO,DIO,DJO,LO,FO, 
     &                 sjn,wjn,cjn1,ajn,ajo,xio,xjo,wio1,wio2,jpo, 
     &                 fpx,wpx,wtx,fp0x,PK,PKM1) 
c 
C$$$  SUBPROGRAM DOCUMENTATION BLOCK 
C 
C SUBPROGRAM:    GG2LL       INTERPOLATE GAUSSIAN TO LAT-LON GRID. 
C   PRGMMR: IREDELL          ORG: W/NMC23    DATE: 92-10-31 
C 
C ABSTRACT: A HORIZONTAL FIELD IS BILINEARLY INTERPOLATED 
C   FROM A GLOBAL GAUSSIAN GRID TO A LATITUDE-LONGITUDE GRID. 
C   THE GAUSSIAN LATITUDES MAY BE PASSED OR CALCULATED. 
C   THE INPUT GLOBAL FIELD IS ASSUMED TO RUN EASTWARD AND SOUTHWARD, 
C   STARTING AT GREENWICH AND THE NORTHERNMOST LATITUDE, 
C   BUT THE OUTPUT FIELD ORIENTATION IS DEFINED BY PASSED ARGUMENTS. 
C   THE FIELD MAY BE TAGGED A SCALAR, A VECTOR, A FLAG OR A BUDGET. 
C   A FLAG FIELD SUCH AS THE LAND-SEA MASK IS NOT INTERPOLATED 
C   BUT TAKEN FROM THE VALUES AT THE CLOSEST INPUT GRIDPOINTS. 
C   A BUDGET FIELD SUCH AS PRECIPITATION IS INTERPOLATED 
C   WHILE PRESERVING THE AREA INTEGRALS OF THE ORIGINAL FIELD. 
C   POLAR SCALARS ARE THE AVERAGE OF THE CLOSEST LATITUDE CIRCLE VALUES. 
C   POLAR VECTOR COMPONENTS ARE TAKEN FROM THE WAVENUMBER 1 COMPONENT 
C   EXTRACTED FROM THE VALUES ON THE CLOSEST LATITUDE CIRCLE. 
C   A LOGICAL BITMAP MAY BE PASSED TO MASK OUT PART OF THE INPUT FIELD. 
C   IN THIS CASE, AN APPROPRIATE OUTPUT BITMAP IS CONSTRUCTED AND 
C   THE OUTPUT FIELD IS COMPUTED ONLY WHERE THE OUTPUT BITMAP IS TRUE 
C   AND ONLY USING DATA WHERE THE INPUT BITMAP IS TRUE. 
C   WHERE THE OUTPUT BITMAP IS FALSE, THE OUTPUT IS SET TO ZERO. 
C   THE BITMAP MAY ALTERNATIVELY BE CONSIDERED AS A TWO-WAY MASK, 
C   IN WHICH CASE OUTPUT FIELD IS ALSO COMPUTED WHERE THE OUTPUT BITMAP 
C   IS FALSE ONLY USING DATA WHERE THE INPUT BITMAP IS FALSE. 
C   THE TWO-WAY MASK OPTION SHOULD NOT BE USED WITH VECTOR COMPONENTS. 
C 
C PROGRAM HISTORY LOG: 
C   92-10-31  IREDELL 
C   93-10-21  IREDELL   ALLOW VECTORS, FLAGS, BITMAP, NORTH/SOUTH FLIP 
C   94-12-05  IREDELL   ALLOW BUDGETS, OPTIONALLY CALCULATE LATITUDES, 
C                       PRECOMPUTE INDICES, GENERALIZE OUTPUT DOMAIN. 
C   95-07-11  IREDELL   REWEIGHT VECTOR INTERPOLATION NEAR POLE. 
C 
C USAGE:    CALL GG2LL(MP,MB,IMN,IXN,JMN,JXN,CJN,LN,FN, 
C    &                 IMO,IXO,JMO,JXO,CIO,CJO,DIO,DJO,LO,FO) 
C   INPUT ARGUMENT LIST: 
C     MP       - INTEGER FIELD PARAMETER IDENTIFIER 
C                (0 FOR SCALAR, 1 FOR VECTOR, 2 FOR FLAG, 3 FOR BUDGET) 
C     MB       - INTEGER BITMAP IDENTIFIER 
C                (0 FOR NO BITMAP, 1 TO INTERPOLATE BITMAP, 
C                 2 TO ALSO INTERPOLATE DATA TO BITMAP FALSES) 
C     IMN      - INTEGER INPUT LONGITUDE DIMENSION 
C     IXN      - INTEGER NUMBER TO SKIP BETWEEN INPUT LONGITUDES 
C     JMN      - INTEGER INPUT LATITUDE DIMENSION (EVEN) 
C     JXN      - INTEGER NUMBER TO SKIP BETWEEN INPUT LATITUDES 
C     CJN      - REAL (JMN/2) GAUSSIAN LATITUDES IN DEGREES NORTH 
C                (IF CJN(1)=0., THE LATITUDES ARE CALCULATED.) 
C     LN       - LOGICAL ((IMN-1)*IXN+(JMN-1)*JXN+1) BITMAP IF MB=1 
C     FN       - REAL ((IMN-1)*IXN+(JMN-1)*JXN+1) FIELD TO INTERPOLATE 
C     IMO      - INTEGER OUTPUT LONGITUDE DIMENSION 
C     IXO      - INTEGER NUMBER TO SKIP BETWEEN OUTPUT LONGITUDES 
C     JMO      - INTEGER OUTPUT LATITUDE DIMENSION 
C     JXO      - INTEGER NUMBER TO SKIP BETWEEN OUTPUT LATITUDES 
C     CIO      - REAL START LONGITUDE IN DEGREES EAST 
C     CJO      - REAL START LATITUDE IN DEGREES NORTH 
C     DIO      - REAL LONGITUDE INCREMENT IN DEGREES EAST 
C     DJO      - REAL LATITUDE INCREMENT IN DEGREES NORTH 
C 
C   OUTPUT ARGUMENT LIST: 
C     LO       - LOGICAL ((IMO-1)*IXO+(JMO-1)*JXO+1) BITMAP IF MB=1 
C     FO       - REAL ((IMO-1)*IXO+(JMO-1)*JXO+1) INTERPOLATED FIELD 
C 
C SUBPROGRAMS CALLED: 
C   GLAT         COMPUTE GAUSSIAN LATITUDES 
C 
C ATTRIBUTES: 
C   LANGUAGE: CRAY FORTRAN 77 
C 
C$$$ 
      REAL CJN(JMN/2) 
      REAL FN((IMN-1)*IXN+(JMN-1)*JXN+1) 
      REAL FO((IMO-1)*IXO+(JMO-1)*JXO+1) 
      LOGICAL LN((IMN-1)*IXN+(JMN-1)*JXN+1) 
      LOGICAL LO((IMO-1)*IXO+(JMO-1)*JXO+1) 
      REAL SJN(JMN/2),WJN(JMN/2),CJN1(JMN/2) 
      REAL PK(JMN/2),PKM1(JMN/2) 
      REAL AJN(JMN+1),AJO(JMO+1),XIO(IMO+1),XJO(JMO+1) 
      REAL WIO1(IMO),WIO2(IMO) 
      INTEGER JPO(JMO) 
      REAL FPX(IMO),FP0X(IMO),WPX(IMO),WTX(IMO) 
      IJN(I,J)=(I-1)*IXN+(J-1)*JXN+1 
      IJO(I,J)=(I-1)*IXO+(J-1)*JXO+1 
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
C  PRECOMPUTE INDICES 
      PI=ACOS(-1.) 
      IF(CJN(1).EQ.0.) THEN 
        write(*,*) '****No Gaussian latitudes, calculate them...' 
        CALL GLAT(JMN/2,SJN,WJN,PK,PKM1) 
        DO J1=1,JMN/2 
          CJN1(J1)=(180/PI)*ASIN(SJN(J1)) 
          cjn(j1)=cjn1(j1) 
        ENDDO 
      ELSE 
        DO J1=1,JMN/2 
          CJN1(J1)=CJN(J1) 
        ENDDO 
      ENDIF 
      DO J=1,JMO 
        JPO(J)=0 
      ENDDO 
      IF(MP.NE.3) THEN 
C  SCALAR OR VECTOR OR FLAG 
        DO I=1,IMO 
          XIO(I)=MOD((CIO+(I-1)*DIO)/360*IMN+2*IMN,FLOAT(IMN))+1 
        ENDDO 
        IF(MP.EQ.0) THEN 
          DO I=1,IMO 
            I1=XIO(I) 
            WIO1(I)=I1+1-XIO(I) 
            WIO2(I)=XIO(I)-I1 
          ENDDO 
        ELSEIF(MP.EQ.1) THEN 
          DL=2*PI/IMN 
          DO I=1,IMO 
            I1=XIO(I) 
            WIO1(I)=SIN((I1+1-XIO(I))*DL)/SIN(DL) 
            WIO2(I)=SIN((XIO(I)-I1)*DL)/SIN(DL) 
          ENDDO 
        ENDIF 
CDIR$ IVDEP 
        DO J1=1,JMN/2 
          AJN(J1)=CJN1(J1) 
          AJN(JMN+1-J1)=-CJN1(J1) 
        ENDDO 
        DO J=1,JMO 
          AJO(J)=CJO+(J-1)*DJO 
        ENDDO 
        IF(DJO.LT.0.) THEN 
          JB=1 
          JE=JMO 
          JI=1 
        ELSE 
          JB=JMO 
          JE=1 
          JI=-1 
        ENDIF 
        J1=1 
        DO J=JB,JE,JI 
          IF(AJO(J).GE.AJN(1)) THEN 
            XJO(J)=1 
          ELSEIF(AJO(J).LE.AJN(JMN)) THEN 
            XJO(J)=JMN 
          ELSE 
            DOWHILE(AJO(J).LT.AJN(J1+1)) 
              J1=J1+1 
            ENDDO 
            XJO(J)=J1+(AJN(J1)-AJO(J))/(AJN(J1)-AJN(J1+1)) 
          ENDIF 
        ENDDO 
        IF(XJO(1).EQ.1.) THEN 
          JPO(1)=1 
        ELSEIF(XJO(1).EQ.FLOAT(JMN)) THEN 
          JPO(1)=JMN 
        ENDIF 
        IF(XJO(JMO).EQ.1.) THEN 
          JPO(JMO)=1 
        ELSEIF(XJO(JMO).EQ.FLOAT(JMN)) THEN 
          JPO(JMO)=JMN 
        ENDIF 
      ELSE 
C  BUDGET 
        DO I=1,IMO+1 
          XIO(I)=MOD((CIO+(I-1.5)*DIO)/360*IMN+2*IMN+0.5,FLOAT(IMN))+1 
        ENDDO 
        IBX=0 
        DO I=1,IMO 
          XIB=XIO(INT(I+0.5-SIGN(0.5,DIO))) 
          XIE=XIO(INT(I+0.5+SIGN(0.5,DIO))) 
          IB=XIB 
          IE=XIE 
          IF(IE.LT.IB) IE=IE+IMN 
          IBX=MAX(IBX,IE-IB+1) 
        ENDDO 
        AJN(1)=1. 
        AJN(JMN/2+1)=0. 
        AJN(JMN+1)=-1. 
CDIR$ IVDEP 
        DO J1=2,JMN/2 
          AJN(J1)=SIN((PI/180)*0.5*(CJN1(J1-1)+CJN1(J1))) 
          AJN(JMN+2-J1)=-AJN(J1) 
        ENDDO 
        DO J=1,JMO+1 
          PJO=CJO+(J-1.5)*DJO 
          IF(PJO.GE.90.) THEN 
            AJO(J)=1. 
          ELSEIF(PJO.LE.-90.) THEN 
            AJO(J)=-1. 
          ELSE 
            AJO(J)=SIN((PI/180)*PJO) 
          ENDIF 
        ENDDO 
        IF(DJO.LT.0.) THEN 
          JB=1 
          JE=JMO+1 
          JI=1 
        ELSE 
          JB=JMO+1 
          JE=1 
          JI=-1 
        ENDIF 
        J1=1 
        DO J=JB,JE,JI 
          IF(AJO(J).GE.AJN(1)) THEN 
            XJO(J)=1 
          ELSEIF(AJO(J).LE.AJN(JMN+1)) THEN 
            XJO(J)=JMN+1 
          ELSE 
            DOWHILE(AJO(J).LT.AJN(J1+1)) 
              J1=J1+1 
            ENDDO 
            XJO(J)=J1+(AJN(J1)-AJO(J))/(AJN(J1)-AJN(J1+1)) 
          ENDIF 
        ENDDO 
        IF(XJO(1).EQ.1.) THEN 
          JPO(1)=1 
        ELSEIF(XJO(1).EQ.FLOAT(JMN)) THEN 
          JPO(1)=JMN 
        ENDIF 
        IF(XJO(JMO+1).EQ.1.) THEN 
          JPO(JMO)=1 
        ELSEIF(XJO(JMO+1).EQ.FLOAT(JMN+1)) THEN 
          JPO(JMO)=JMN 
        ENDIF 
      ENDIF 
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
C  GET POLAR VALUES 
      DO J=1,JMO,JMO-1 
        IF(JPO(J).GT.0) THEN 
          IF(MB.EQ.0) THEN 
            IF(MP.EQ.0) THEN 
C  FULL SCALAR 
              J1=JPO(J) 
              FP=0. 
              DO I=1,IMN 
                FP=FP+FN(IJN(I,J1)) 
              ENDDO 
              FP=FP/IMN 
              DO I=1,IMO 
                FO(IJO(I,J))=FP 
              ENDDO 
            ELSEIF(MP.EQ.1) THEN 
C  FULL VECTOR 
              J1=JPO(J) 
              FPC=0. 
              FPS=0. 
              DO I=1,IMN 
                CI=COS(2*PI*(I-1)/IMN) 
                SI=SIN(2*PI*(I-1)/IMN) 
                FPC=FPC+CI*FN(IJN(I,J1)) 
                FPS=FPS+SI*FN(IJN(I,J1)) 
              ENDDO 
              FPC=2*FPC/IMN 
              FPS=2*FPS/IMN 
              DO I=1,IMO 
                CI=COS(2*PI*(I-1)/IMO) 
                SI=SIN(2*PI*(I-1)/IMO) 
                FO(IJO(I,J))=FPC*CI+FPS*SI 
              ENDDO 
            ELSEIF(MP.EQ.2) THEN 
C  FULL FLAG 
              J1=JPO(J) 
              DO I=1,IMO 
                FO(IJO(I,J))=FN(IJN(1,J1)) 
              ENDDO 
            ELSEIF(MP.EQ.3) THEN 
C  FULL BUDGET 
              XJB=XJO(INT(J+0.5+SIGN(0.5,DJO))) 
              XJE=XJO(INT(J+0.5-SIGN(0.5,DJO))) 
              JB=XJB 
              JE=XJE 
              WJB=JB+1-XJB 
              WJE=XJE-JE 
              FP=0. 
              WP=0. 
              DO J1=JB,JE 
                W=AJN(J1)-AJN(J1+1) 
                IF(J1.EQ.JB) W=W*WJB 
                IF(J1.EQ.JE) W=W*WJE 
                DO I=1,IMN 
                  FP=FP+W*FN(IJN(I,J1)) 
                ENDDO 
                WP=WP+W*IMN 
              ENDDO 
              DO I=1,IMO 
                FO(IJO(I,J))=FP/WP 
              ENDDO 
            ENDIF 
          ELSE 
            IF(MP.EQ.0) THEN 
C  BITMAP OR TWOWAY SCALAR 
              J1=JPO(J) 
              FP=0. 
              WP=0. 
              IF(MB.EQ.2) FP0=0. 
              DO I=1,IMN 
                IF(LN(IJN(I,J1))) THEN 
                  FP=FP+FN(IJN(I,J1)) 
                  WP=WP+1. 
                ELSEIF(MB.EQ.2) THEN 
                  FP0=FP0+FN(IJN(I,J1)) 
                ENDIF 
              ENDDO 
              DO I=1,IMO 
                LO(IJO(I,J))=WP.GE.0.5*IMN 
              ENDDO 
              IF(LO(IJO(1,J))) THEN 
                DO I=1,IMO 
                  FO(IJO(I,J))=FP/WP 
                ENDDO 
              ELSEIF(MB.EQ.2.AND.WP.LT.IMN) THEN 
                DO I=1,IMO 
                  FO(IJO(I,J))=FP0/(IMN-WP) 
                ENDDO 
              ELSE 
                DO I=1,IMO 
                  FO(IJO(I,J))=0. 
                ENDDO 
              ENDIF 
            ELSEIF(MP.EQ.1) THEN 
C  BITMAP VECTOR 
              J1=JPO(J) 
              IP=0 
              DO I=1,IMN 
                IF(LN(IJN(I,J1))) IP=IP+1 
              ENDDO 
              DO I=1,IMO 
                LO(IJO(I,J))=IP.EQ.IMN 
              ENDDO 
              IF(LO(IJO(1,J))) THEN 
                FPC=0. 
                FPS=0. 
                DO I=1,IMN 
                  CI=COS(2*PI*(I-1)/IMN) 
                  SI=SIN(2*PI*(I-1)/IMN) 
                  FPC=FPC+CI*FN(IJN(I,J1)) 
                  FPS=FPS+SI*FN(IJN(I,J1)) 
                ENDDO 
                FPC=2*FPC/IMN 
                FPS=2*FPS/IMN 
                DO I=1,IMO 
                  CI=COS(2*PI*(I-1)/IMO) 
                  SI=SIN(2*PI*(I-1)/IMO) 
                  FO(IJO(I,J))=FPC*CI+FPS*SI 
                ENDDO 
              ELSE 
                DO I=1,IMO 
                  FO(IJO(I,J))=0. 
                ENDDO 
              ENDIF 
            ELSEIF(MP.EQ.2) THEN 
C  BITMAP OR TWOWAY FLAG 
              J1=JPO(J) 
              DO I=1,IMO 
                LO(IJO(I,J))=LN(IJN(1,J1)) 
                IF(LN(IJN(1,J1)).OR.MB.EQ.2) THEN 
                  FO(IJO(I,J))=FN(IJN(1,J1)) 
                ELSE 
                  FO(IJO(I,J))=0. 
                ENDIF 
              ENDDO 
            ELSEIF(MP.EQ.3) THEN 
C  BITMAP OR TWOWAY BUDGET 
              XJB=XJO(INT(J+0.5+SIGN(0.5,DJO))) 
              XJE=XJO(INT(J+0.5-SIGN(0.5,DJO))) 
              JB=XJB 
              JE=XJE 
              WJB=JB+1-XJB 
              WJE=XJE-JE 
              FP=0. 
              WP=0. 
              WT=0. 
              IF(MB.EQ.2) THEN 
                FP0=0. 
              ENDIF 
              DO J1=JB,JE 
                W=AJN(J1)-AJN(J1+1) 
                IF(J1.EQ.JB) W=W*WJB 
                IF(J1.EQ.JE) W=W*WJE 
                DO I=1,IMN 
                  IF(LN(IJN(I,J1))) THEN 
                    FP=FP+W*FN(IJN(I,J1)) 
                    WP=WP+W 
                  ELSEIF(MB.EQ.2) THEN 
                    FP0=FP0+W*FN(IJN(I,J1)) 
                  ENDIF 
                ENDDO 
                WT=WT+W*IMN 
              ENDDO 
              DO I=1,IMO 
                LO(IJO(I,J))=WP.GE.0.5*WT 
              ENDDO 
              IF(LO(IJO(1,J))) THEN 
                DO I=1,IMO 
                  FO(IJO(I,J))=FP/WP 
                ENDDO 
              ELSEIF(MB.EQ.2.AND.WP.LT.WT) THEN 
                DO I=1,IMO 
                  FO(IJO(I,J))=FP0/(WT-WP) 
                ENDDO 
              ELSE 
                DO I=1,IMO 
                  FO(IJO(I,J))=0. 
                ENDDO 
              ENDIF 
            ENDIF 
          ENDIF 
        ENDIF 
      ENDDO 
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
C  INTERPOLATE THE REST OF THE FIELD 
      DO J=1,JMO 
        IF(JPO(J).EQ.0) THEN 
          IF(MB.EQ.0) THEN 
            IF(MP.EQ.0.OR.MP.EQ.1) THEN 
C  FULL SCALAR OR VECTOR 
              J1=XJO(J) 
              J2=MIN(J1+1,JMN) 
              WJ2=XJO(J)-J1 
              WJ1=1.-WJ2 
              DO I=1,IMO 
                I1=XIO(I) 
                I2=MOD(I1,IMN)+1 
                WI1=WIO1(I) 
                WI2=WIO2(I) 
                W11=WI1*WJ1 
                W21=WI2*WJ1 
                W12=WI1*WJ2 
                W22=WI2*WJ2 
                FO(IJO(I,J))=W11*FN(IJN(I1,J1))+W21*FN(IJN(I2,J1))+ 
     &                       W12*FN(IJN(I1,J2))+W22*FN(IJN(I2,J2)) 
              ENDDO 
            ELSEIF(MP.EQ.2) THEN 
C  FULL FLAG 
              J1=NINT(XJO(J)) 
              DO I=1,IMO 
                I1=MOD(NINT(XIO(I))-1,IMN)+1 
                FO(IJO(I,J))=FN(IJN(I1,J1)) 
              ENDDO 
            ELSEIF(MP.EQ.3) THEN 
C  FULL BUDGET 
              XJB=XJO(INT(J+0.5+SIGN(0.5,DJO))) 
              XJE=XJO(INT(J+0.5-SIGN(0.5,DJO))) 
              JB=XJB 
              JE=XJE 
              WJB=JB+1-XJB 
              WJE=XJE-JE 
              DO I=1,IMO 
                FPX(I)=0. 
                WPX(I)=0. 
              ENDDO 
              DO J1=JB,JE 
                WJ=AJN(J1)-AJN(J1+1) 
                IF(J1.EQ.JB) WJ=WJ*WJB 
                IF(J1.EQ.JE) WJ=WJ*WJE 
                DO I1X=1,IBX 
                  DO I=1,IMO 
                    XIB=XIO(INT(I+0.5-SIGN(0.5,DIO))) 
                    XIE=XIO(INT(I+0.5+SIGN(0.5,DIO))) 
                    IB=XIB 
                    IE=XIE 
                    WIB=IB+1-XIB 
                    WIE=XIE-IE 
                    IF(IE.LT.IB) IE=IE+IMN 
                    I1=IB+(I1X-1) 
                    IF(I1.LE.IE) THEN 
                      W=WJ 
                      IF(I1.EQ.IB) W=W*WIB 
                      IF(I1.EQ.IE) W=W*WIE 
                      I1M=MOD(I1-1,IMN)+1 
                      FPX(I)=FPX(I)+W*FN(IJN(I1M,J1)) 
                      WPX(I)=WPX(I)+W 
                    ENDIF 
                  ENDDO 
                ENDDO 
              ENDDO 
              DO I=1,IMO 
                FO(IJO(I,J))=FPX(I)/WPX(I) 
              ENDDO 
            ENDIF 
          ELSE 
            IF(MP.EQ.0.OR.MP.EQ.1) THEN 
C  BITMAP OR TWOWAY SCALAR OR VECTOR 
              J1=XJO(J) 
              J2=MIN(J1+1,JMN) 
              WJ2=XJO(J)-J1 
              WJ1=1.-WJ2 
              DO I=1,IMO 
                I1=XIO(I) 
                I2=MOD(I1,IMN)+1 
                WI1=WIO1(I) 
                WI2=WIO2(I) 
                W11=WI1*WJ1 
                W21=WI2*WJ1 
                W12=WI1*WJ2 
                W22=WI2*WJ2 
                WP=0. 
                IJ11=IJN(I1,J1) 
                IJ21=IJN(I2,J1) 
                IJ12=IJN(I1,J2) 
                IJ22=IJN(I2,J2) 
                IF(LN(IJ11)) WP=WP+W11 
                IF(LN(IJ21)) WP=WP+W21 
                IF(LN(IJ12)) WP=WP+W12 
                IF(LN(IJ22)) WP=WP+W22 
                WT=W11+W21+W12+W22 
                IJ=IJO(I,J) 
                LO(IJ)=WP.GE.0.5*WT 
                IF(LO(IJ)) THEN 
                  FP=0. 
                  IF(LN(IJ11)) FP=FP+W11*FN(IJ11) 
                  IF(LN(IJ21)) FP=FP+W21*FN(IJ21) 
                  IF(LN(IJ12)) FP=FP+W12*FN(IJ12) 
                  IF(LN(IJ22)) FP=FP+W22*FN(IJ22) 
                  FO(IJ)=FP*WT/WP 
                ELSEIF(MB.EQ.2.AND.WP.LT.WT) THEN 
                  FP=0. 
                  IF(.NOT.LN(IJ11)) FP=FP+W11*FN(IJ11) 
                  IF(.NOT.LN(IJ21)) FP=FP+W21*FN(IJ21) 
                  IF(.NOT.LN(IJ12)) FP=FP+W12*FN(IJ12) 
                  IF(.NOT.LN(IJ22)) FP=FP+W22*FN(IJ22) 
                  FO(IJ)=FP*WT/(WT-WP) 
                ELSE 
                  FO(IJ)=0. 
                ENDIF 
              ENDDO 
            ELSEIF(MP.EQ.2) THEN 
C  BITMAP OR TWOWAY FLAG 
              J1=NINT(XJO(J)) 
              DO I=1,IMO 
                I1=MOD(NINT(XIO(I))-1,IMN)+1 
                LO(IJO(I,J))=LN(IJN(I1,J1)) 
                IF(LO(IJO(I,J)).OR.MB.EQ.2) THEN 
                  FO(IJO(I,J))=FN(IJN(I1,J1)) 
                ELSE 
                  FO(IJO(I,J))=0. 
                ENDIF 
              ENDDO 
            ELSEIF(MP.EQ.3) THEN 
C  BITMAP OR TWOWAY BUDGET 
              XJB=XJO(INT(J+0.5+SIGN(0.5,DJO))) 
              XJE=XJO(INT(J+0.5-SIGN(0.5,DJO))) 
              JB=XJB 
              JE=XJE 
              WJB=JB+1-XJB 
              WJE=XJE-JE 
              DO I=1,IMO 
                FPX(I)=0. 
                IF(MB.EQ.2) FP0X(I)=0. 
                WPX(I)=0. 
                WTX(I)=0. 
              ENDDO 
              DO J1=JB,JE 
                WJ=AJN(J1)-AJN(J1+1) 
                IF(J1.EQ.JB) WJ=WJ*WJB 
                IF(J1.EQ.JE) WJ=WJ*WJE 
                DO I1X=1,IBX 
                  DO I=1,IMO 
                    XIB=XIO(INT(I+0.5-SIGN(0.5,DIO))) 
                    XIE=XIO(INT(I+0.5+SIGN(0.5,DIO))) 
                    IB=XIB 
                    IE=XIE 
                    WIB=IB+1-XIB 
                    WIE=XIE-IE 
                    IF(IE.LT.IB) IE=IE+IMN 
                    I1=IB+(I1X-1) 
                    IF(I1.LE.IE) THEN 
                      W=WJ 
                      IF(I1.EQ.IB) W=W*WIB 
                      IF(I1.EQ.IE) W=W*WIE 
                      I1M=MOD(I1-1,IMN)+1 
                      IF(LN(IJN(I1M,J1))) THEN 
                        FPX(I)=FPX(I)+W*FN(IJN(I1M,J1)) 
                        WPX(I)=WPX(I)+W 
                      ELSEIF(MB.EQ.2) THEN 
                        FP0X(I)=FP0X(I)+W*FN(IJN(I1M,J1)) 
                      ENDIF 
                      WTX(I)=WTX(I)+W 
                    ENDIF 
                  ENDDO 
                ENDDO 
              ENDDO 
              DO I=1,IMO 
                LO(IJO(I,J))=WPX(I).GE.0.5*WTX(I) 
                IF(LO(IJO(I,J))) THEN 
                  FO(IJO(I,J))=FPX(I)/WPX(I) 
                ELSEIF(MB.EQ.2.AND.WPX(I).LT.WTX(I)) THEN 
                  FO(IJO(I,J))=FP0X(I)/(WTX(I)-WPX(I)) 
                ELSE 
                  FO(IJO(I,J))=0. 
                ENDIF 
              ENDDO 
            ENDIF 
          ENDIF 
        ENDIF 
      ENDDO 
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      RETURN 
      END 
 
C----------------------------------------------------------------------- 
      SUBROUTINE GLAT(JH,SLAT,WLAT,PK,PKM1) 
C$$$  SUBPROGRAM DOCUMENTATION BLOCK 
C 
C SUBPROGRAM:    GLAT        COMPUTE GAUSSIAN LATITUDE FUNCTIONS 
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 92-10-31 
C 
C ABSTRACT: COMPUTES SINES OF GAUSSIAN LATITUDE BY ITERATION. 
C           THE GAUSSIAN WEIGHTS ARE ALSO COMPUTED. 
C 
C PROGRAM HISTORY LOG: 
C   91-10-31  MARK IREDELL 
C 
C USAGE:    CALL GLAT(JH,SLAT,WLAT) 
C 
C   INPUT ARGUMENT LIST: 
C     JH       - INTEGER NUMBER OF GAUSSIAN LATITUDES IN A HEMISPHERE 
C 
C   OUTPUT ARGUMENT LIST: 
C     SLAT     - REAL (JH) SINES OF (POSITIVE) GAUSSIAN LATITUDE 
C     WLAT     - REAL (JH) GAUSSIAN WEIGHTS FOR THE NH 
C 
C ATTRIBUTES: 
C   LANGUAGE: CRAY FORTRAN 
C 
C$$$ 
      DIMENSION SLAT(JH),WLAT(JH) 
c     PARAMETER(PI=3.14159265358979,C=(1.-(2./PI)**2)*0.25,EPS=1.E-14) 
      PARAMETER(PI=3.14159265358979,C=(1.-(2./PI)**2)*0.25,EPS=1.E-7) 
      PARAMETER(JBZ=50) 
      DIMENSION PK(JH),PKM1(JH),BZ(JBZ) 
      DATA BZ        / 2.4048255577,  5.5200781103, 
     $  8.6537279129, 11.7915344391, 14.9309177086, 18.0710639679, 
     $ 21.2116366299, 24.3524715308, 27.4934791320, 30.6346064684, 
     $ 33.7758202136, 36.9170983537, 40.0584257646, 43.1997917132, 
     $ 46.3411883717, 49.4826098974, 52.6240518411, 55.7655107550, 
     $ 58.9069839261, 62.0484691902, 65.1899648002, 68.3314693299, 
     $ 71.4729816036, 74.6145006437, 77.7560256304, 80.8975558711, 
     $ 84.0390907769, 87.1806298436, 90.3221726372, 93.4637187819, 
     $ 96.6052679510, 99.7468198587, 102.888374254, 106.029930916, 
     $ 109.171489649, 112.313050280, 115.454612653, 118.596176630, 
     $ 121.737742088, 124.879308913, 128.020877005, 131.162446275, 
     $ 134.304016638, 137.445588020, 140.587160352, 143.728733573, 
     $ 146.870307625, 150.011882457, 153.153458019, 156.295034268 / 
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
C  ESTIMATE LATITUDES USING BESSEL FUNCTION 
      R=1./SQRT((2*JH+0.5)**2+C) 
      DO J=1,MIN(JH,JBZ) 
        SLAT(J)=COS(BZ(J)*R) 
      ENDDO 
      DO J=JBZ+1,JH 
        SLAT(J)=COS((BZ(JBZ)+(J-JBZ)*PI)*R) 
      ENDDO 
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
C  CONVERGE UNTIL ALL SINES OF GAUSSIAN LATITUDE ARE WITHIN EPS 
      SPMAX=1. 
      DO WHILE(SPMAX.GT.EPS) 
        SPMAX=0. 
        DO J=1,JH 
          PKM1(J)=1. 
          PK(J)=SLAT(J) 
        ENDDO 
        DO N=2,2*JH 
          DO J=1,JH 
            PKM2=PKM1(J) 
            PKM1(J)=PK(J) 
            PK(J)=((2*N-1)*SLAT(J)*PKM1(J)-(N-1)*PKM2)/N 
          ENDDO 
        ENDDO 
        DO J=1,JH 
          SP=PK(J)*(1.-SLAT(J)**2)/(2*JH*(PKM1(J)-SLAT(J)*PK(J))) 
          SLAT(J)=SLAT(J)-SP 
          SPMAX=MAX(SPMAX,ABS(SP)) 
        ENDDO 
      ENDDO 
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
C  COMPUTE COSINES AND GAUSSIAN WEIGHTS 
      DO J=1,JH 
        WLAT(J)=2.*(1.-SLAT(J)**2)/(2*JH*PKM1(J))**2 
      ENDDO 
      RETURN 
      END 
 
C----------------------------------------------------------------------- 
      SUBROUTINE MPFDEF(IPTV,MPF) 
C$$$  SUBPROGRAM DOCUMENTATION BLOCK 
C 
C SUBPROGRAM: MPFDEF         SETS DEFAULT PARAMETER FLAGS 
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 92-10-31 
C 
C ABSTRACT: SETS FIELD IDENTIFIER DEFAULTS FOR VARIOUS PARAMETERS. 
C   A FLAG OF 0 IS SCALAR, 1 IS VECTOR, 2 IS FLAG AND 3 IS BUDGET. 
C   THESE IDENTIFIERS ARE USED IN INTERPOLATION. 
C 
C PROGRAM HISTORY LOG: 
C   93-10-21  IREDELL 
C   93-12-05  IREDELL   ADDED BUDGET FLAG 
C 
C USAGE:    CALL MPFDEF(IPTV,MPF) 
C   INPUT ARGUMENTS: 
C     IPTV         PARAMTER TABLE VERSION (ONLY 1 OR 2 IS RECOGNIZED) 
C   OUTPUT ARGUMENTS: 
C     MPF          INTEGER (255) FIELD PARAMETER IDENTIFIERS 
C 
C ATTRIBUTES: 
C   LANGUAGE: CRAY FORTRAN 
C 
C$$$ 
      DIMENSION MPF(255) 
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      do i=1,255 
        MPF(i)=0 
      enddo 
      IF(IPTV.EQ.1.OR.IPTV.EQ.2) THEN 
        MPF(033)=1 
        MPF(034)=1 
        MPF(049)=1 
        MPF(050)=1 
        MPF(095)=1 
        MPF(096)=1 
        MPF(124)=1 
        MPF(125)=1 
        MPF(147)=1 
        MPF(148)=1 
        MPF(181)=1 
        MPF(182)=1 
        MPF(183)=1 
        MPF(184)=1 
        MPF(247)=1 
        MPF(248)=1 
        MPF(081)=2 
        MPF(091)=2 
        MPF(140)=2 
        MPF(141)=2 
        MPF(142)=2 
        MPF(143)=2 
        MPF(173)=2 
        MPF(174)=2 
        MPF(175)=2 
        MPF(209)=2 
        MPF(054)=3      ! PRECIPITABLE WATER (KG/M2) 
        MPF(057)=3      ! EVAPORATION (KG/M2) 
        MPF(058)=3      ! CLOUD ICE (KG/M2) 
        MPF(059)=3      ! PRECIPITATION RATE (KG/M2/S) 
        MPF(061)=3      ! TOTAL PRECIPITATION (KG/M2) 
        MPF(062)=3      ! LARGE-SCALE PRECIPITATION (KG/M2) 
        MPF(063)=3      ! CONVECTIVE PRECIPITATION (KG/M2) 
        MPF(064)=3      ! WATER EQUIVALENT SNOWFALL RATE (KG/M2/S) 
        MPF(065)=3      ! WATER EQUIVALENT OF SNOW DEPTH (KG/M2) 
C       MPF(071)=3      ! TOTAL CLOUD COVER (PERCENT) 
C       MPF(072)=3      ! CONVECTIVE CLOUD COVER (PERCENT) 
        MPF(073)=3      ! LOW CLOUD COVER (PERCENT) 
        MPF(074)=3      ! MIDDLE CLOUD COVER (PERCENT) 
        MPF(075)=3      ! HIGH CLOUD COVER (PERCENT) 
        MPF(076)=3      ! CLOUD WATER (KG/M2) 
        MPF(079)=3      ! LARGE SCALE SNOW (KG/M2) 
        MPF(086)=3      ! SOIL WETNESS (KG/M2) 
        MPF(090)=3      ! RUNOFF (KG/M2) 
        MPF(111)=3      ! NET SOLAR RADIATIVE FLUX AT SURFACE (W/M2) 
        MPF(112)=3      ! NET LONGWAVE RADIATIVE FLUX AT SURFACE (W/M2) 
        MPF(113)=3      ! NET SOLAR RADIATIVE FLUX AT TOP (W/M2) 
        MPF(114)=3      ! NET LONGWAVE RADIATIVE FLUX AT TOP (W/M2) 
        MPF(115)=3      ! NET LONGWAVE RADIATIVE FLUX (W/M2) 
        MPF(116)=3      ! NET SOLAR RADIATIVE FLUX (W/M2) 
        MPF(117)=3      ! TOTAL RADIATIVE FLUX (W/M2) 
        MPF(121)=3      ! LATENT HEAT FLUX (W/M2) 
        MPF(122)=3      ! SENSIBLE HEAT FLUX (W/M2) 
        MPF(123)=3      ! BOUNDARY LAYER DISSIPATION (W/M2) 
        MPF(144)=3      ! VOLUMETRIC SOIL MOISTURE CONTENT (FRACTION) 
        MPF(145)=3      ! POTENTIAL EVAPORATION RATE (W/M2) 
        MPF(146)=3      ! CLOUD WORKFUNCTION (J/KG) 
        MPF(155)=3      ! GROUND HEAT FLUX (W/M2) 
        MPF(156)=3      ! CONVECTIVE INHIBITION (W/M2) 
        MPF(160)=3      ! CLEAR SKY UPWARD SOLAR FLUX (W/M2) 
        MPF(161)=3      ! CLEAR SKY DOWNWARD SOLAR FLUX (W/M2) 
        MPF(162)=3      ! CLEAR SKY UPWARD LONGWAVE FLUX (W/M2) 
        MPF(163)=3      ! CLEAR SKY DOWNWARD LONGWAVE FLUX (W/M2) 
        MPF(164)=3      ! CLOUD FORCING NET SOLAR FLUX (W/M2) 
        MPF(165)=3      ! CLOUD FORCING NET LONGWAVE FLUX (W/M2) 
        MPF(166)=3      ! VISIBLE BEAM DOWNWARD SOLAR FLUX (W/M2) 
        MPF(167)=3      ! VISIBLE DIFFUSE DOWNWARD SOLAR FLUX (W/M2) 
        MPF(168)=3      ! NEAR IR BEAM DOWNWARD SOLAR FLUX (W/M2) 
        MPF(169)=3      ! NEAR IR DIFFUSE DOWNWARD SOLAR FLUX (W/M2) 
        MPF(172)=3      ! MOMENTUM FLUX (N/M2) 
        MPF(204)=3      ! DOWNWARD SOLAR RADIATIVE FLUX (W/M2) 
        MPF(205)=3      ! DOWNWARD LONGWAVE RADIATIVE FLUX (W/M2) 
        MPF(211)=3      ! UPWARD SOLAR RADIATIVE FLUX (W/M2) 
        MPF(212)=3      ! UPWARD LONGWAVE RADIATIVE FLUX (W/M2) 
        MPF(213)=3      ! NON-CONVECTIVE CLOUD COVER (PERCENT) 
        MPF(214)=3      ! CONVECTIVE PRECIPITATION RATE (KG/M2/S) 
        MPF(223)=3      ! PLANT CANOPY SURFACE WATER (KG/M2) 
        MPF(228)=3      ! POTENTIAL EVAPORATION (KG/M2) 
        MPF(229)=3      ! SNOW PHASE-CHANGE HEAT FLUX (W/M2) 
        MPF(232)=3      ! DOWNWARD TOTAL RADIATION FLUX (W/M2) 
        MPF(233)=3      ! UPWARD TOTAL RADIATION FLUX (W/M2) 
        MPF(224)=3      ! BASEFLOW-GROUNDWATER RUNOFF (KG/M2) 
        MPF(225)=3      ! STORM SURFACE RUNOFF (KG/M2) 
        MPF(238)=3      ! SNOW COVER (PERCENT) 
      ENDIF 
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      RETURN 
      END 
c 
c 
      SUBROUTINE GBYTE (IN,IOUT,ISKIP,NBYTE) 
      CALL GBYTES (IN,IOUT,ISKIP,NBYTE,0,1) 
      RETURN 
      END 
 
      SUBROUTINE SBYTE (IOUT,IN,ISKIP,NBYTE) 
      CALL SBYTES (IOUT,IN,ISKIP,NBYTE,0,1) 
      RETURN 
      END 
 
      SUBROUTINE GBYTES (IN,IOUT,ISKIP,NBYTE,NSKIP,N) 
C          Get bytes - unpack bits:  Extract arbitrary size values from a 
C          packed bit string, right justifying each value in the unpacked 
C          array. 
      DIMENSION IN(*), IOUT(*) 
C            IN    = packed array input 
C            IO    = unpacked array output 
C            ISKIP = initial number of bits to skip 
C            NBYTE = number of bits to take 
C            NSKIP = additional number of bits to skip on each iteration 
C            N     = number of iterations 
C************************************** MACHINE SPECIFIC CHANGES START HERE 
C          Machine dependent information required: 
C            LMWD   = Number of bits in a word on this machine 
C            MASKS  = Set of word masks where the first element has only the 
C                     right most bit set to 1, the second has the two, ... 
C            LEFTSH = Shift left bits in word M to the by N bits 
C            RGHTSH = Shift right 
C            OR     = Logical OR (add) on this machine. 
C            AND    = Logical AND (multiply) on this machine 
C          This is for Sun UNIX Fortran, DEC Alpha, and RS6000 
      PARAMETER (LMWD=32) 
      DIMENSION MASKS(LMWD) 
      SAVE    MASKS 
      DATA    MASKS /'1'X,'3'X,'7'X,'F'X, '1F'X,'3F'X,'7F'X,'FF'X, 
     +'1FF'X,'3FF'X,'7FF'X,'FFF'X, '1FFF'X,'3FFF'X,'7FFF'X,'FFFF'X, 
     +'1FFFF'X,       '3FFFF'X,       '7FFFF'X,       'FFFFF'X, 
     +'1FFFFF'X,      '3FFFFF'X,      '7FFFFF'X,      'FFFFFF'X, 
     +'1FFFFFF'X,     '3FFFFFF'X,     '7FFFFFF'X,     'FFFFFFF'X, 
     +'1FFFFFFF'X,    '3FFFFFFF'X,    '7FFFFFFF'X,    'FFFFFFFF'X/ 
c     +'1FFFFFFFF',   '3FFFFFFFF',   '7FFFFFFFF'X,   'FFFFFFFFF'X, 
c     +'1FFFFFFFFF'X,  '3FFFFFFFFF',  '7FFFFFFFFF'X,  'FFFFFFFFFF'X, 
c     +'1FFFFFFFFFF'X, '3FFFFFFFFFF', '7FFFFFFFFFF'X, 'FFFFFFFFFFF'X, 
c     +'1FFFFFFFFFFF'X,'3FFFFFFFFFFF'X,'7FFFFFFFFFFF'X,'FFFFFFFFFFFF'X, 
c     +'1FFFFFFFFFFFF'X,   '3FFFFFFFFFFFF'X,   '7FFFFFFFFFFFF'X, 
c     +                                        'FFFFFFFFFFFFF'X, 
c     +'1FFFFFFFFFFFFF'X,  '3FFFFFFFFFFFFF'X,  '7FFFFFFFFFFFFF'X, 
c     +                                        'FFFFFFFFFFFFFF'X, 
c     +'1FFFFFFFFFFFFFF'X, '3FFFFFFFFFFFFFF'X, '7FFFFFFFFFFFFFF'X, 
c     +                                        'FFFFFFFFFFFFFFF'X, 
c     +'1FFFFFFFFFFFFFFF'X,'3FFFFFFFFFFFFFFF'X,'7FFFFFFFFFFFFFFF'X, 
c     +                                        'FFFFFFFFFFFFFFFF'X/ 
C          IBM PC using Microsoft Fortran uses different syntax: 
C     DATA MASKS/16#1,16#3,16#7,16#F,16#1F,16#3F,16#7F,16#FF, 
C    + 16#1FF,16#3FF,16#7FF,16#FFF,16#1FFF,16#3FFF,16#7FFF,16#FFFF, 
C    + 16#1FFFF,16#3FFFF,16#7FFFF,16#FFFFF,16#1FFFFF,16#3FFFFF, 
C    + 16#7FFFFF,16#FFFFFF,16#1FFFFFF,16#3FFFFFF,16#7FFFFFF,16#FFFFFFF, 
C    + 16#1FFFFFFF,16#3FFFFFFF,16#7FFFFFFF,16#FFFFFFFF/ 
      INTEGER RGHTSH, OR, AND 
      LEFTSH(M,N) = ISHFT(M,N) 
      RGHTSH(M,N) = ISHFT(M,-N) 
C     OR(M,N)  = M.OR.N 
C     AND(M,N) = M.AND.N 
C     OR(M,N)  = IOR(M,N) 
C     AND(M,N) = IAND(M,N) 
C************************************** MACHINE SPECIFIC CHANGES END HERE 
C          History:  written by Robert C. Gammill, jul 1972. 
 
 
C          NBYTE must be less than or equal to LMWD 
      ICON = LMWD-NBYTE 
      IF (ICON.LT.0) RETURN 
      MASK = MASKS (NBYTE) 
C          INDEX  = number of words into IN before the next "byte" appears 
C          II     = number of bits the "byte" is from the left side of the word 
C          ISTEP  = number of bits from the start of one "byte" to the next 
C          IWORDS = number of words to skip from one "byte" to the next 
C          IBITS  = number of bits to skip after skipping IWORDS 
C          MOVER  = number of bits to the right, a byte must be moved to be 
C                   right adjusted 
      INDEX = ISKIP/LMWD 
      II    = MOD (ISKIP,LMWD) 
      ISTEP = NBYTE+NSKIP 
      IWORDS= ISTEP/LMWD 
      IBITS = MOD (ISTEP,LMWD) 
 
      DO 6 I=1,N                                                                 
      MOVER = ICON-II 
      IF (MOVER) 2,3,4 
 
C          The "byte" is split across a word break. 
    2 MOVEL = -MOVER 
      MOVER = LMWD-MOVEL 
      NP1 = LEFTSH (IN(INDEX+1),MOVEL) 
      NP2 = RGHTSH (IN(INDEX+2),MOVER) 
      IOUT(I) = AND (OR (NP1,NP2) , MASK) 
      GO TO 5                                                                    
 
C          The "byte" is already right adjusted. 
    3 IOUT(I) = AND (IN (INDEX+1) , MASK) 
      GO TO 5                                                                    
 
C          Right adjust the "byte". 
    4 IOUT(I) = AND (RGHTSH (IN (INDEX+1),MOVER) , MASK) 
 
    5 II = II+IBITS 
      INDEX = INDEX+IWORDS 
      IF (II .LT. LMWD) GO TO 6 
      II = II-LMWD 
      INDEX = INDEX+1 
    6 CONTINUE                                                                   
 
      RETURN                                                                     
      END                                                                        
 
      SUBROUTINE SBYTES (IOUT,IN,ISKIP,NBYTE,NSKIP,N) 
C          Store bytes - pack bits:  Put arbitrary size values into a 
C          packed bit string, taking the low order bits from each value 
C          in the unpacked array. 
      DIMENSION IN(*), IOUT(*) 
C            IOUT  = packed array output 
C            IN    = unpacked array input 
C            ISKIP = initial number of bits to skip 
C            NBYTE = number of bits to pack 
C            NSKIP = additional number of bits to skip on each iteration 
C            N     = number of iterations 
C************************************** MACHINE SPECIFIC CHANGES START HERE 
C          Machine dependent information required: 
C            LMWD   = Number of bits in a word on this machine 
C            MASKS  = Set of word masks where the first element has only the 
C                     right most bit set to 1, the second has the two, ... 
C            LEFTSH = Shift left bits in word M to the by N bits 
C            RGHTSH = Shift right 
C            OR     = Logical OR (add) on this machine 
C            AND    = Logical AND (multiply) on this machine 
C            NOT    = Logical NOT (negation) on this machine 
C          This is for Sun UNIX Fortran 
      PARAMETER (LMWD=32) 
      DIMENSION MASKS(LMWD) 
      SAVE    MASKS 
      DATA    MASKS /'1'X,'3'X,'7'X,'F'X, '1F'X,'3F'X,'7F'X,'FF'X, 
     +'1FF'X,'3FF'X,'7FF'X,'FFF'X, '1FFF'X,'3FFF'X,'7FFF'X,'FFFF'X, 
     +'1FFFF'X,       '3FFFF'X,       '7FFFF'X,       'FFFFF'X, 
     +'1FFFFF'X,      '3FFFFF'X,      '7FFFFF'X,      'FFFFFF'X, 
     +'1FFFFFF'X,     '3FFFFFF'X,     '7FFFFFF'X,     'FFFFFFF'X, 
     +'1FFFFFFF'X,    '3FFFFFFF'X,    '7FFFFFFF'X,    'FFFFFFFF'X/ 
c     +'1FFFFFFFF',   '3FFFFFFFF',   '7FFFFFFFF'X,   'FFFFFFFFF'X, 
c     +'1FFFFFFFFF'X,  '3FFFFFFFFF',  '7FFFFFFFFF'X,  'FFFFFFFFFF'X, 
c     +'1FFFFFFFFFF'X, '3FFFFFFFFFF', '7FFFFFFFFFF'X, 'FFFFFFFFFFF'X, 
c     +'1FFFFFFFFFFF'X,'3FFFFFFFFFFF'X,'7FFFFFFFFFFF'X,'FFFFFFFFFFFF'X, 
c     +'1FFFFFFFFFFFF'X,   '3FFFFFFFFFFFF'X,   '7FFFFFFFFFFFF'X, 
c     +                                        'FFFFFFFFFFFFF'X, 
c     +'1FFFFFFFFFFFFF'X,  '3FFFFFFFFFFFFF'X,  '7FFFFFFFFFFFFF'X, 
c     +                                        'FFFFFFFFFFFFFF'X, 
c     +'1FFFFFFFFFFFFFF'X, '3FFFFFFFFFFFFFF'X, '7FFFFFFFFFFFFFF'X, 
c     +                                        'FFFFFFFFFFFFFFF'X, 
c     +'1FFFFFFFFFFFFFFF'X,'3FFFFFFFFFFFFFFF'X,'7FFFFFFFFFFFFFFF'X, 
c     +                                        'FFFFFFFFFFFFFFFF'X/ 
      INTEGER RGHTSH, OR, AND 
      LEFTSH(M,N) = ISHFT(M,N) 
      RGHTSH(M,N) = ISHFT(M,-N) 
C     OR(M,N)  = M.OR.N 
C     AND(M,N) = M.AND.N 
C     OR(M,N)  = IOR(M,N) 
C     AND(M,N) = IAND(M,N) 
C     NOT(M)   = .NOT.M 
C***********************************************************************         
 
C          NBYTE must be less than or equal to LMWD 
      ICON = LMWD-NBYTE 
      IF (ICON .LT. 0) RETURN 
      MASK = MASKS(NBYTE) 
C          INDEX  = number of words into IOUT the next "byte" is to be stored 
C          II     = number of bits in from the left side of the word to store it 
C          ISTEP  = number of bits from the start of one "byte" to the next 
C          IWORDS = number of words to skip from one "byte" to the next 
C          IBITS  = number of bits to skip after skipping IWORDS 
C          MOVER  = number of bits to the right, a byte must be moved to be 
C                   right adjusted 
      INDEX = ISKIP/LMWD 
      II    = MOD(ISKIP,LMWD) 
      ISTEP = NBYTE+NSKIP 
      IWORDS = ISTEP/LMWD 
      IBITS = MOD(ISTEP,LMWD) 
 
      DO 6 I=1,N                                                                 
      J = AND (MASK,IN(I)) 
      MOVEL = ICON-II 
      IF (MOVEL) 2,3,4 
 
C          The "byte" is to be split across a word break 
    2 MSK = MASKS (NBYTE+MOVEL) 
      IOUT(INDEX+1) = OR (AND(NOT(MSK),IOUT(INDEX+1)),RGHTSH(J,-MOVEL)) 
      ITEMP = AND (MASKS(LMWD+MOVEL),IOUT(INDEX+2)) 
      IOUT(INDEX+2) = OR(ITEMP,LEFTSH(J,LMWD+MOVEL)) 
      GO TO 5                                                                    
 
C          The "byte" is to be stored right-adjusted 
    3 IOUT(INDEX+1) = OR ( AND (NOT(MASK),IOUT(INDEX+1)) , J) 
      GO TO 5                                                                    
 
C          The "byte" is to be stored in middle of word, so shift left. 
    4 MSK = LEFTSH(MASK,MOVEL) 
      IOUT(INDEX+1) = OR(AND(NOT(MSK),IOUT(INDEX+1)),LEFTSH(J,MOVEL)) 
 
    5 II = II+IBITS 
      INDEX = INDEX+IWORDS 
      IF (II .LT. LMWD) GO TO 6 
      II = II-LMWD 
      INDEX = INDEX+1 
    6 CONTINUE 
 
      RETURN                                                                     
      END                                                                        
c 
c ************************************************************************ 
        subroutine swap4(in,io,nn) 
c swaps bytes in groups of 4 to compensate for byte swapping within 
c    words which occurs on DEC (VAX) and PC machines. 
c 
c in - input array to be swapped 
c io - ouput array with bytes swapped 
c nn - number of bytes to be swapped 
c       logical*1 in(1),io(1),ih          ! non_ALPHA 
        character*1 in(1),io(1),ih        ! DEC_ALPHA 
        do 10 i=1,nn,4 
        ih=in(i) 
        io(i)=in(i+3) 
        io(i+3)=ih 
        ih=in(i+1) 
        io(i+1)=in(i+2) 
        io(i+2)=ih 
   10   continue 
        return 
        end 
 
C ************************************************************************* 
        subroutine swap2(in,io,nn) 
c swaps bytes in groups of 2 to compensate for byte swapping within 
c    words which occurs on DEC (VAX) and PC machines. 
c 
c in - input array to be swapped 
c io - ouput array with bytes swapped 
c nn - number of bytes to be swapped 
c       logical*1 in(1),io(1),ih           ! non_ALPHA 
        character*1 in(1),io(1),ih         ! DEC_ALPHA 
        do 10 i=1,nn,2 
        ih=in(i) 
        io(i)=in(i+1) 
        io(i+1)=ih 
   10   continue 
        return 
        end 
 
c        subroutine filt(m,n,in) 
c        logical*1 m(1),n(1),l,u 
c        data l/32/,u/127/ 
c        do 10 i=1,in 
c        n(i)=m(i) 
c        if(n(i).lt.  l) n(i)=l 
c        if(n(i).gt.  u) n(i)=u 
c   10   continue 
c        return 
c        end 
 
c ************************************************************************** 
c 
c This version of anyGRIB1.f has changed the arguments passed to/from 
c the subroutine calls.  It works with the GRIB decoders in this directory. 
c 
c 
c Revision History: 
c  Jan 03, 1997 -- minor, fix P1,P2 unpacking in pds_GRIB1 
c  Nov 13, 1996 -- reverse the order of lines 1349,1350 
c  Sep 11, 1996 -- fix gds_GRIB1 subroutine for special grids 
c  Feb 12, 1996 -- initialize rmax,rmin to be -999999.0,999999.0 
c  Jan  5, 1996 -- fix iskp in BMS. 
c  Nov 30, 1995 -- fix BMS section when using a bitmap. 
c  Aug 31, 1995 -- major change, to unpack THINNED grids. 
c  Jul 10, 1995 -- minor change, to fix bit map section. 
c  Feb 17, 1995 -- minor change, to correct GDS LOV computation. 
c 
c Systems Tested: 
c  SUN,CRAY,PC/MS-DOS,DEC/ALPHA,IBM/RS6000 
c================================================================== 
c The subroutines below are to deocode GRIB format 
c 
      subroutine bds_GRIB1(iprnt,gribedi,ivarout,iliv,idati,nword, 
     &   inoff,dscalf,ierr, 
     &  nmax,npoints,rc00,rmax,rmin,numgd,mapuse,mapb,pds,ingrib, 
     &  ix,datan) 
c 
c iprnt:   print out flag, 0=NO printing 
c gribedi: GRIB edition number, 1, 0, -1 
c nword:   length of ingrib 
c inoff:   offset number of bits  
c dscalf:  decimal scale factor, from PDS 
c ierr: error code,  
c       0=no error,  
c      -1=number of bits per data value exceeds word length or maximum allowed 
c      -2=input array holds fewer coded values than expected, 
c      -3=output array too small to hold decoded data values 
c      -4=currently undefined GRIB Edition 1 option selected, 
c      -8=constant field, npoints cannot be computed, use number of grids from  
c         gds to fill datan, the reference value is in datan(1)  
c      -9=blank record 
c     -11=unknown predefined grid 
c     -12=complex packing of spher harmonic coeff, to be developed 
c       1=no '7777' at the end 
c nmax:    maximum dimension of datan 
c npoints: actual number of array unpacked 
c rc00: 
c rmax:    maximum value of array datan 
c rmin:    minimum value of array datan 
c numgd:   number of actual grids used, counted from BMP bitmap 
c mapuse:  if 0, do not use mapb; otherwise, mapb(mapuse) 
c mapb:    bit map from BMS or generated 
c pds:     PDS array 
c ingrib:  integer array containing GRIB record 
c ix:      integer array of unpacked array, of dimension (nnx*nny) 
c datan:   returned array of length (nmax=nnx*nny) 
c 
c bdslen: length of BDS 
c bdsflg: BDS flag (byte 4 bits 1-4 and byte 14) 
c bdsub:  number of unused bits in BDS 
c bdse:   BDS scale factor 
c bdsref: BDS reference value 
c bdsn:   number of bits used for packing 
c 
      dimension ingrib(nword),datan(nmax) 
      logical*1 mapb(nmax) 
      real*4    refval, bscalf 
      integer*4 bdslen,bdsflg(8),bdsub,bdse,bdsn,bdsn1 
      integer*4 pds(22),ix(nmax),refa,refb,gribedi 
      integer*4 N,P,JL,KL,ML 
      integer*4 ifov(361),kbdsn2(361)  ! 361 is the maximum nny 
      character*72 bdsnam(6) 
      data ifirst/0/ 
c 
c     save 
c 
      if(ifirst.eq.0) then 
        bdsnam(1)=' BDS length'                       ! byte 1-3 
        bdsnam(2)=' BDS flag'                         ! byte 4, bits 1-4 & byte 14 
        bdsnam(3)=' BDS number of unused bits'        ! byte 4, bits 5-8 
        bdsnam(4)=' BDS scale factor'                 ! byte 5-6 
        bdsnam(5)=' BDS reference value'              ! byte 7-10 
        bdsnam(6)=' BDS number of bits for packing'   ! byte 11 
        ifirst=9 
      endif 
      scalten = 10**dscalf 
c 
c unpack BDS section 
c 
      iskp=inoff 
      call gbyte(ingrib,bdslen,iskp,24) 
      if(iprnt.ne.0) write(*,*) '     bdslen: ',bdslen 
 
      do i=1,8 
        bdsflg(i)=0 
      enddo 
      iskp=iskp+24 
      if(gribedi.eq.-1) then 
        call gbyte(ingrib,bdsflg(1),iskp+3,1) 
        call gbyte(ingrib,bdsflg(2),iskp+2,1) 
        call gbyte(ingrib,bdsflg(3),iskp+1,1) 
        call gbyte(ingrib,bdsflg(4),iskp,1) 
        call gbyte(ingrib,bdsub,iskp+4,4) 
        if(bdsflg(4).eq.1) then 
          call gbyte(ingrib,bdsflg(5),iskp+10*8,1)       ! should be zero 
          call gbyte(ingrib,bdsflg(6),iskp+10*8+1,1) 
          call gbyte(ingrib,bdsflg(7),iskp+10*8+2,1) 
          call gbyte(ingrib,bdsflg(8),iskp+10*8+3,1) 
        endif 
      else 
        call gbyte(ingrib,bdsflg(1),iskp,1) 
        call gbyte(ingrib,bdsflg(2),iskp+1,1) 
        call gbyte(ingrib,bdsflg(3),iskp+2,1) 
        call gbyte(ingrib,bdsflg(4),iskp+3,1) 
        call gbyte(ingrib,bdsub,iskp+4,4) 
        if(bdsflg(4).eq.1) then 
          call gbyte(ingrib,bdsflg(5),iskp+10*8,1)       ! should be zero 
          call gbyte(ingrib,bdsflg(6),iskp+10*8+1,1) 
          call gbyte(ingrib,bdsflg(7),iskp+10*8+2,1) 
          call gbyte(ingrib,bdsflg(8),iskp+10*8+3,1) 
        endif 
      endif 
      if(iprnt.ne.0)  
     &  write(*,*) '     bdsflg: ',(bdsflg(i),i=1,8) 
      if(iprnt.ne.0) write(*,*) '     bdsub:  ',bdsub 
c 
c scale factor      ! byte 5-6 
c 
      iskp=iskp+8 
      call gbyte(ingrib,bdse,iskp,16) 
      if(bdse.gt.32768) bdse=32768-bdse 
      bscalf=float(bdse) 
      scaltwo=2**(bscalf) 
      if(iprnt.ne.0) write(*,*) '     bdse:   ',bdse 
c 
c reference value   ! byte 7-10 
c 
      iskp=iskp+16 
      call gbyte(ingrib,isign,iskp,1) 
      call gbyte(ingrib,refa,iskp+1,7) 
      call gbyte(ingrib,refb,iskp+8,24) 
      rdum=refb 
      refval=(-1)**isign * rdum * 16**(refa-64) / 2**24 
c 
c if bdsflg(3) == 1, refval needs to be round into an integer 
c 
      if(iprnt.ne.0)  
     &  write(*,*) '     bdsref: ',isign,refa,refb,refval 
c 
c number of bits used for packing   ! byte 11 
c if bdsn == 0, constatn field 
c 
      iskp=iskp+32 
      call gbyte(ingrib,bdsn,iskp,8) 
      if(iprnt.ne.0) then 
        write(*,*) '     bdsn:   ',bdsn 
        write(*,*) ' refval,bscalf,dscalf: ',refval,bscalf,dscalf 
      endif 
 
c 
c compute or find out how many points used 
c 
      if(bdsflg(4).eq.0) then        ! no flag in byte 14 
c 
c if (bdslen-11) is odd, the last byte may be padded 
c 
        idum=(bdslen-11)*8-bdsub 
        if(bdsn.eq.0) then           ! constant field when bdsn=0 
c 
c Ierr: -8=constant field,  
c          npoints cannot be computed, use number of grids from  
c          gds to fill datan, the reference value is in datan(1)  
c 
          npoints=1 
          datan(1)=refval 
          rmax=refval 
          rmin=refval 
          ijrmax=-9999 
          ijrmin=-9999 
          ierr=-8 
          return 
        else 
          if(bdsflg(7).eq.0 .and. mapuse.gt.0) then                   !951130 
            npoints=numgd                                             !951130 
          else                                                        !951130 
            npoints=idum/bdsn                                         !951130 
          endif                                                       !951130 
          if(mod(idum,bdsn).ne.0) then 
c                                                                     !950710 
c fix NMC GRIB encoder error for 1x1 degree (360x181=65160) grids     !950710 
c                                                                     !950710 
            if(abs(npoints-65160).le.2) then                          !950710 
              npoints=65160                                           !950710 
            else                                                      !950710 
c              write(*,*) 'WARNING! length of packed bits can not ',   !950710 
c     &         'be divided by bdsn ',idum,bdsn,npoints                !950710 
c              write(*,815)idum,bdsn,npoints,pds(7),pds(9)              !950710 
c 815          format('WARNING! length of packed bits can not ',        !950710 
c     &         'be divided by bdsn ',5i8)                              !950710 
            endif                                                     !950710 
          endif 
        endif 
      else                          ! extended flags in byte 14 
c 
c   Extended BDS flags: 
c                        NAME        BYTE LOCATION 
c                        bdsn1       11 
c                        n1          12-13 
c                                    14 
c                        n2          15-16 
c                        np1         17-18 
c                        np2         19-20 
c                                    21 
c                        bdsn2(np1)  22-(xx-1)  one byte each  xx=np1+22 
c                        bmap2(np2)  xx-(n1-1) 
c                        mp1(np1)    n1-(n2-1) 
c                        mp2(np2)    n2-... 
c 
        bdsn1=bdsn 
        iskp=iskp+8 
        call gbyte(ingrib,n1,iskp,16) 
        iskp=iskp+16+8 
        call gbyte(ingrib,n2,iskp,16) 
        iskp=iskp+16 
        call gbyte(ingrib,np1,iskp,16) 
        iskp=iskp+16 
        call gbyte(ingrib,np2,iskp,16) 
        iskp=iskp+16+8 
        if(iprnt.ne.0) then 
          write(*,*) '    bdsn1:   ',bdsn1 
          write(*,*) '       n1:   ',n1 
          write(*,*) '       n2:   ',n2 
          write(*,*) '      np1:   ',np1 
          write(*,*) '      np2:   ',np2 
        endif 
        if(bdsflg(7).eq.0 .and. mapuse.gt.0) then 
          npoints=numgd 
        else 
          npoints=-9999  ! to be determined counting secondary bit map 
        endif 
      endif 
c 
c All flags decoded, unpack the data 
c 
c bdsflg(1) = 0, grid point data 
c             1, spherical harmonic coefficients 
c bdsflg(2) = 0, simple packing 
c             1, second order ("complex") packing 
c bdsflg(3) = 0, original data were floating point values 
c             1, original data were integer values 
c bdsflg(4) = 0, no additional flags at oct 14 
c             1, octet 14 contains flag bits 5-12 
c bdsflg(5) = 0, reserved, should be 0 
c bdsflg(6) = 0, single datum at each grid point 
c             1, matrix of values at each grid point 
c bdsflg(7) = 0, no secondary bit maps 
c             1, secondary bit maps present 
c bdsflg(8) = 0, second order values have constant width 
c             1, second order values have different width 
c 
      if(bdsflg(2).eq.0) then        ! simple packing 
        if(iprnt.ne.0)  
     &    write(*,*) ' total number of grids: ',npoints, nmax 
        if(npoints.gt.nmax) then 
          ierr=-3 
          write(*,*) 'ERROR in bds_GRIB1, ierr = ',ierr 
          return 
        endif 
        if(bdsflg(1).eq.0) then      ! simple packing, grid point data 
          rc00=0. 
          iskp=11*8+inoff 
          call gbytes(ingrib,ix,iskp,bdsn,0,npoints) 
          do i=1,npoints 
            datan(i)=(refval+ix(i)*scaltwo)/scalten 
          enddo 
          if(ivarout.EQ.pds(7).AND.iliv.EQ.pds(8).AND. 
     &          idati.EQ.pds(9)) then 
            open(iout,file='readfile.dat',status='unknown') 
            write(iout,333) ivarout, iliv, idati 
 333        format(2X,I2.2,2X,I3.3,2X,I4.4)     
c            do i=1,npoints 
             write(iout,*) datan 
 444        format(10(2X,F8.4))             
c            end do 
            close(iout) 
          end if   
        elseif(bdsflg(1).eq.1) then  ! simple packing, spher harmonic coeff 
          iskp=11*8+inoff 
          call gbyte(ingrib,isign,iskp,1) 
          call gbyte(ingrib,refa,iskp+1,7) 
          call gbyte(ingrib,refb,iskp+8,24) 
          rdum=refb 
          rc00=(-1)**isign * rdum * 16**(refa-64) / 2**24 
          if(iprnt.ne.0) 
     &      write(*,*) ' real part of coeff (0,0): ',rc00 
          iskp=iskp+32 
          call gbytes(ingrib,ix,iskp,bdsn,0,npoints-1) 
          datan(1)=rc00 
          do i=2,npoints 
            datan(i)=(refval+ix(i-1)*scaltwo)/scalten 
          enddo 
        else 
        endif 
      elseif(bdsflg(2).eq.1) then    ! complex packing 
        if(bdsflg(1).eq.0) then      ! complex packing, grid point data 
c 
c octet 
c 22 -- (xx-1): widths in bits of the 2nd order packed values 
c xx -- (n1-1): secondary bit maps, padded to an even number octet 
c n1 -- (n2-1): np1 local minima, each is bdsn1 wide, padded 
c n2 -- bdslen: np2 second order packed values, padded at the end 
c 
c if bdsflg(7)==0, [22--(n1-1)]: widths in bits of 2nd order packed values 
c                  this is "row by row (or column by column) packing" 
c 
c if bdsflg(8)==0, [22--22]: width of 2nd order packed values 
c if bdsflg(8)==1, but width==0, this sub-section is constant 
c 
          if(bdsflg(7).ne.0) then 
            write(*,*) '** Secondary Bit Map in use, no unpacking yet!' 
            npoints=-9999 
            return 
          endif 
          k1off=inoff+(n1-1)*8 
          do k1=1,np1 
            call gbyte(ingrib,ifov(k1),k1off,bdsn1) 
c           if(k1.ge.(np1-10))write(*,*) ' ifov: ',k1,k1off,ifov(k1) 
            k1off=k1off+bdsn1 
          enddo 
c 
c unpack second order value 
c 
          k2off=inoff+21*8         ! width starts from byte 22 
          do k2=1,np1 
            call gbyte(ingrib,kbdsn2(k2),k2off,8) 
            k2off=k2off+8 
c           if(k2.ge.(np1-10)) 
c    &        write(*,*) ' kbdsn2: ',k2,k2off,kbdsn2(k2) 
          enddo 
c 
c         write(*,*) ' unpacking 2nd order value: ',numgd,mapuse 
          k3off=inoff+(n2-1)*8 
          nt=0 
          kone=mapuse/np1 
          do k2=1,mapuse 
            kk=((k2-1)/kone)+1 
            if(mapb(k2)) then 
              if(kbdsn2(kk).eq.0) then 
                isov=0 
              else 
                call gbyte(ingrib,isov,k3off,kbdsn2(kk)) 
                k3off=k3off+kbdsn2(kk) 
              endif 
              nt=nt+1 
              datan(nt)=(refval+float(ifov(kk)+isov)*scaltwo)/scalten 
c             if(nt.ge.130 .and. nt.le.140)  
c    &          write(*,*) ' isov: ',k2,nt,datan(nt),kk,ifov(kk),isov 
            endif 
          enddo 
c         write(*,*) ' unpacking 2nd done ',numgd,nt,k2off,k3off 
          if(numgd.ne.nt) write(*,*) '**WARNING, number of grids', 
     &      'mismatch in 2nd order unpacking! ',numgd,nt 
          npoints=nt 
        elseif(bdsflg(1).eq.1) then  ! complex packing, spher harmonic coeff 
          ierr=-12 
          if((pds(3).ne.98) .or. (gribedi.eq.1)) then 
            write(*,*) '**Complex packing of non-ECMWF spectral' 
            write(*,*) '**or GRIB Edition 1, NO unpacking yet!' 
            npoints=-9999 
            return 
          else 
c 
c                  Second Order Packing Formats: 
c 
c               GRIB 1              ECMWF(version 0, x) 
c              NAME  LOCATION      LOCATION   NAME 
c                N1   12-13         12-13     N 
c                     14            14-15     P 
c                N2   15-16         16        JL 
c                P1   17-18         17        KL 
c                P2   19-20         18        ML 
c                     21            19-(N-1)  binary data 
c         BIT WIDTH   22-(P1-1)     N-        packed binary data 
c Secondary BIT MAP   xx-(N1-1) 
c          MP1(NP1)   N1-(N2-1) 
c          MP2(NP2)   N2- 
c 
c ECMWF spherical harmonic complex packing 
c 
            iskp=11*8+inoff 
            call gbyte(ingrib,N,iskp,16) 
            iskp=iskp+16 
            call gbyte(ingrib,P,iskp,16) 
            iskp=iskp+16 
            call gbyte(ingrib,JL,iskp,8) 
            iskp=iskp+8 
            call gbyte(ingrib,KL,iskp,8) 
            iskp=iskp+8 
            call gbyte(ingrib,ML,iskp,8) 
            iskp=iskp+8 
c 
c compute how many values to unpack 
c 
            idum=bdslen-19 
            num=idum/4 
            if(mod(idum,4).ne.0) then 
              write(*,*) 'UNEVEN ECMWF spherical harmonic unpacking!' 
              write(*,*) '    ',bdslen,idum,num 
              return 
            endif 
            if(iprnt.ne.0) write(*,*) 'ECMWF spherical harmonic: ',num 
c 
c unpack every 4 bytes into real 
c 
            do i=1,num 
              iskp=iskp+(i-1)*32 
              call gbyte(ingrib,isign,iskp,1) 
              call gbyte(ingrib,refa,iskp+1,7) 
              call gbyte(ingrib,refb,iskp+8,24) 
              rdum=refb 
              datan(i)=(-1)**isign * rdum * 16**(refa-64) / 2**24 
            enddo 
            write(*,*) '**PARTIALLY DONE of ' 
            write(*,*) '**ECMWF spherical harmonic complex unpacking!' 
            write(*,*) '**DO NOT use yet!' 
            npoints=-9999 
            return 
          endif 
        else 
        endif 
      else 
        write(*,*) '****ERROR, unknown packing, grid/spectral!' 
        stop'UNKNOWN PACKING ERROR' 
      endif 
c 
c after unpacking, locate the maximum and minimum among datan 
c 
      rmax=-999999. 
      rmin=999999. 
      do i=1,npoints 
        if(datan(i).gt.rmax) then 
          ijrmax=i 
          rmax=datan(i) 
        endif 
        if(datan(i).lt.rmin) then 
          rmin=datan(i) 
          ijrmin=i 
        endif 
      enddo 
      if(iprnt.ne.0) then 
        write(*,*) ' rmax, at ij = ',rmax,ijrmax 
        write(*,*) ' rmin, at ij = ',rmin,ijrmin 
      endif 
      ierr=0 
 9998 return 
      end 
c 
c 
      subroutine bms_GRIB1(iprnt,nword,nmax,inoff, 
     &  kbms,nx,ny,la1,lo1,ierr,numb,mapuse,pds, 
     &  ingrib,mapb) 
c 
c This is a generic GRIB Edition 1 BMS decoder 
c 
c iprnt:  print out flag, 0=NO printing 
c lenpds: input, length of PDS in bytes 
c ipds:   input, integer array containing GRIB PDS 
c nword:  input, number of words used in ipds 
c kbms:   input, GDS exists if kbms=1 
c numb:   actual number of grids counted from bitmap 
c mapuse: number of grids expected from grid specification 
c ierr:   returned error code,  
c         0=no error, -11=unknown predefined grid 
c 
      dimension ingrib(nword),igrd37(73) 
      logical*1 mapb(nmax) 
      character*72 bmsnam(3) 
      integer*4 kbms,iskp,idum,pds(22) 
      integer*4 bmslen,bmsub,bmstbl 
      integer*4 nx,ny,la1,lo1,mapuse 
c 
      data ifirst/0/ 
c 
c igrd37 is good for grid_id 37-44 
      data igrd37/73, 73, 73,  
     &    73, 73, 73, 73, 73, 72, 72, 72, 71, 71, 
     &    71, 70, 70, 69, 69, 68, 67, 67, 66, 65,  
     &    65, 64, 63, 62, 61, 60, 60, 59, 58, 57, 
     &    56, 55, 54, 52, 51, 50, 49, 48, 47, 45,  
     &    44, 43, 42, 40, 39, 38, 36, 35, 33, 32,  
     &    30, 29, 28, 26, 25, 23, 22, 20, 19, 17, 
     &    16, 14, 12, 11,  9,  8,  6,  5,  3,  2/                  
c 
      ierr=0 
      if(ifirst.eq.0) then 
        bmsnam(1)=' 1 = BMS length'                               !byte 1-3 
        bmsnam(2)=' 2 = number of unused bits in BMS'             !byte 4 
        bmsnam(3)=' 3 = BMS Table Reference'                      !byte 5-6 
        ifirst=9 
      endif 
c 
c process BMS 
c 
      mapuse=0 
      numb=0 
      if(kbms.ne.0) then 
        iskp=inoff 
        call gbyte(ingrib,bmslen,iskp,24) 
        iskp=iskp+24 
        call gbyte(ingrib,bmsub,iskp,8) 
        iskp=iskp+8 
        call gbyte(ingrib,bmstbl,iskp,16)                         !950710 
        iskp=iskp+16                                           !960105 
        if(iprnt.ne.0) then 
          write(*,*) ' BMS length: ',bmslen 
          write(*,*) ' BMS number of unused bits: ',bmsub 
          write(*,*) ' BMS Table Reference: ',bmstbl 
          write(*,*) ' BMS LA1,LO1: ',la1,lo1 
        endif 
c                                                                 !950710 
c if bmstbl =0, a bit map follows, otherwise see Center Specs     !950710 
c                                                                 !950710 
        if(bmstbl.eq.0) then                                      !950710 
          mapuse=bmslen*8-6*8-bmsub                               !950710 
c         call gbytes(ingrib,mapb,iskp,1,0,numb)                  !950710 
          do i=1,mapuse                                           !951130 
            mapb(i)=.false. 
            call gbyte(ingrib,idum,iskp,1) 
            if(idum.eq.1) then                                    !951130 
              mapb(i)=.true.                                      !951130 
              numb=numb+1                                         !951130 
            endif                                                 !951130 
            iskp=iskp+1                                           !951130 
          enddo 
c         write(*,*) ' BMS Numb bitmap:     ',numb,mapuse 
          return 
        endif 
      endif 
c 
c pds(3) == 07, US NMC 
c           08, US NWS Telecomms Gateway 
c           09, US NWS Field Stations 
c           34, Japan Meteorological Agency 
c           52, US NHC, Miami 
c           54, Canadian 
c           57, US Air Forece, GWC 
c           58, US FNOC 
c           59, US NOAA FSL 
c           74, UKMET 
c           85, French Weather Service - Toulouse 
c           97, European Space Agency (ESA) 
c           98, ECMWF 
c           99, DeBilt, Netherlands 
c 
c     if(pds(3).ne.7) return 
c 
c NMC's predefined grids: make an rectangular mapb 
c 
      if(pds(5).eq.21 .or. pds(5).eq.22) then 
        mapuse=37*37 
        numb=1333 
        m=0 
        k=0 
        do j=1,ny+1 
          do i=1,nx 
            m=m+1 
            mapb(m)=.true. 
            if(j.eq.(ny+1) .and. i.ge.2) mapb(m)=.false. 
            if(mapb(m)) k=k+1 
          enddo 
        enddo 
        if(k.ne.numb) write(*,*) '**WARNING! in bms_GRIB1-1 ',k,numb 
        if(m.ne.mapuse) write(*,*) '**WARNING! in bms_GRIB1-2 ',m,mapuse 
      elseif(pds(5).eq.23 .or. pds(5).eq.24) then 
        mapuse=37*37 
        numb=1333 
        m=0 
        k=0 
        do j=1,ny+1 
          do i=1,nx 
            m=m+1 
            mapb(m)=.true. 
            if(j.eq.1 .and. i.ge.2) mapb(m)=.false. 
            if(mapb(m)) k=k+1 
          enddo 
        enddo 
        if(k.ne.numb) write(*,*) '**WARNING! in bms_GRIB1-3 ',k,numb 
        if(m.ne.mapuse) write(*,*) '**WARNING! in bms_GRIB1-4 ',m,mapuse 
      elseif(pds(5).eq.25) then 
        numb=1297 
        mapuse=72*19 
        m=0 
        k=0 
        do j=1,ny+1 
          do i=1,nx 
            m=m+1 
            mapb(m)=.true. 
            if(j.eq.(ny+1) .and. i.ge.2) mapb(m)=.false. 
            if(mapb(m)) k=k+1 
          enddo 
        enddo 
        if(k.ne.numb) write(*,*) '****WARNING! in bms_GRIB1-5 ',k,numb 
        if(m.ne.mapuse) write(*,*) '**WARNING! in bms_GRIB1-6 ',m,mapuse 
      elseif(pds(5).eq.26) then 
        numb=1297 
        m=0 
        k=0 
        do j=1,ny+1 
          do i=1,nx 
            m=m+1 
            mapb(m)=.true. 
            if(j.eq.1 .and. i.ge.2) mapb(m)=.false. 
            if(mapb(m)) k=k+1 
          enddo 
        enddo 
        if(k.ne.numb) write(*,*) '**WARNING! in bms_GRIB1-7 ',k,numb 
        if(m.ne.mapuse) write(*,*) '**WARNING! in bms_GRIB1-8 ',m,mapuse 
      elseif(pds(5).ge.37 .and. pds(5).le.44) then 
        numb=3447 
        mapuse=73*73 
        m=0 
        k=0 
        do j=1,73 
          jj=j 
          if(la1.eq.-90000) jj=74-j 
          do i=1,73 
            m=m+1 
            mapb(m)=.false. 
            if(i.le.igrd37(jj)) mapb(m)=.true. 
            if(mapb(m)) k=k+1 
          enddo 
        enddo 
        if(k.ne.numb) write(*,*) '**WARNING! in bms_GRIB1-37 ',k,numb 
        if(m.ne.mapuse)  
     &    write(*,*) '**WARNING! in bms_GRIB1-38 ',m,mapuse 
      elseif(pds(5).eq.50) then 
        numb=964 
        mapuse=36*33 
        m=0 
        k=0 
        do j=1,4 
          do i=1,36 
            m=m+1 
            mapb(m)=.false. 
            if(i.ge.8 .and. i.le.29) mapb(m)=.true. 
            if(mapb(m)) k=k+1 
          enddo 
        enddo 
        do j=5,8 
          do i=1,36 
            m=m+1 
            mapb(m)=.false. 
            if(i.ge.7 .and. i.le.30) mapb(m)=.true. 
            if(mapb(m)) k=k+1 
          enddo 
        enddo 
        do j=9,12 
          do i=1,36 
            m=m+1 
            mapb(m)=.false. 
            if(i.ge.6 .and. i.le.31) mapb(m)=.true. 
            if(mapb(m)) k=k+1 
          enddo 
        enddo 
        do j=13,16 
          do i=1,36 
            m=m+1 
            mapb(m)=.false. 
            if(i.ge.5 .and. i.le.32) mapb(m)=.true. 
            if(mapb(m)) k=k+1 
          enddo 
        enddo 
        do j=17,20 
          do i=1,36 
            m=m+1 
            mapb(m)=.false. 
            if(i.ge.4 .and. i.le.33) mapb(m)=.true. 
            if(mapb(m)) k=k+1 
          enddo 
        enddo 
        do j=21,24 
          do i=1,36 
            m=m+1 
            mapb(m)=.false. 
            if(i.ge.3 .and. i.le.34) mapb(m)=.true. 
            if(mapb(m)) k=k+1 
          enddo 
        enddo 
        do j=25,28 
          do i=1,36 
            m=m+1 
            mapb(m)=.false. 
            if(i.ge.2 .and. i.le.35) mapb(m)=.true. 
            if(mapb(m)) k=k+1 
          enddo 
        enddo 
        do j=29,33 
          do i=1,36 
            m=m+1 
            mapb(m)=.true. 
            k=k+1 
          enddo 
        enddo 
        if(k.ne.numb) write(*,*) '**WARNING! in bms_GRIB1-50 ',k,numb 
        if(m.ne.mapuse) write(*,*)'**WARNING! in bms_GRIB1-51 ',m,mapuse 
      elseif(pds(5).eq.61 .or. pds(5).eq.62) then 
        numb=4096 
        mapuse=91*46 
        m=0 
        k=0 
        do j=1,ny+1 
          do i=1,nx 
            m=m+1 
            mapb(m)=.true. 
            if(j.eq.(ny+1) .and. i.ge.2) mapb(m)=.false. 
            if(mapb(m)) k=k+1 
          enddo 
        enddo 
        if(k.ne.numb) write(*,*) '**WARNING! in bms_GRIB1-9 ',k,numb 
        if(m.ne.mapuse) write(*,*)'**WARNING! in bms_GRIB1-10 ',m,mapuse 
      elseif(pds(5).eq.63 .or. pds(5).eq.64) then 
        numb=4096 
        mapuse=91*46 
        m=0 
        k=0 
        do j=1,ny+1 
          do i=1,nx 
            m=m+1 
            mapb(m)=.true. 
            if(j.eq.1 .and. i.ge.2) mapb(m)=.false. 
            if(mapb(m)) k=k+1 
          enddo 
        enddo 
        if(k.ne.numb) write(*,*) '**WARNING! in bms_GRIB1-11 ',k,numb 
        if(m.ne.mapuse) write(*,*)'**WARNING! in bms_GRIB1-12 ',m,mapuse 
      else 
        numb=0 
        ierr=-11 
      endif 
      return 
      end 
c 
c -------------------------------------------------------------------- 
      subroutine gds_GRIB1(iprnt,gribedi,nword,inoff, 
     &  kgds,ierr,nx,ny,la1,lo1,pds,igds,lov,dx,dy,igds6) 
c 
c This is a generic GRIB Edition 1 GDS decoder 
c 
c iprnt:  print out flag, 0=NO printing 
c lenpds: input, length of PDS in bytes 
c igds:   input, integer array containing GRIB GDS section 
c nword:  input, number of words used in igds 
c kgds:   input, GDS exists if kgds=1 
c ierr:   returned error code, 0=no error, 
c 
      include 'eta2m3d.par' 
      include 'eta2m3d.cm1' 
 
      dimension igds(nword) 
      character*72 gdsnam(4), gds6name 
      integer*4 kgds,iskp,pds(22),gribedi 
      integer*4 gdslen,gdsnv,gdspv,gds6,shcrp,csmode 
      integer*4 nx,ny,la1,lo1,gds17(8) 
      integer*4 ksign,refa,refb 
c 
      integer*4 la2,lo2,di,dj,ngauss,latin,xssp,yssp,orient,nr 
      integer*4 gds27,gds28(3),lov,dx,dy,latin2,latin1,latsp,lonsp 
      integer*4 vertcoor(40)  
      integer*4 latpl,lonpl 
 
      data ifirst/0/ 
 
c -------------------------- 
c 
      if(ifirst.eq.0) then 
        gdsnam(1)=' 1 = length'                                       !byte 1-3 
        gdsnam(2)=' 2 = NV, number of vertical coordinate parameter'  !byte 4 
        gdsnam(3)=' 3 = PV, or PL or 255'                             !byte 5 
        gdsnam(4)=' 4 = data representation type (table 6)'           !byte 6 
        ifirst=9 
      endif 
c 
c process GDS 
c 
      if(kgds.eq.0) then 
        gdslen=0 
        return 
      endif 
      iskp=inoff 
      call gbyte(igds,gdslen,iskp,24) 
      iskp=iskp+24 
      call gbyte(igds,gdsnv,iskp,8) 
      iskp=iskp+8 
      call gbyte(igds,gdspv,iskp,8) 
      iskp=iskp+8 
      call gbyte(igds,gds6,iskp,8) 
      iskp=iskp+8 
      call GDS_TAB6(pds(3),gribedi,gds6,gds6name) 
      if(iprnt.ne.0) then 
        write(*,*) ' GDS length: ',gdslen 
        write(*,*) ' GDS NV: ',gdsnv 
        write(*,*) ' GDS PV: ',gdspv 
        write(*,*) ' GDS Data Repres Type:(table 6) ',gds6 
        write(*,*) '   ',gds6name 
      endif 
      igds6=gds6 
      write(ilg,*)  gds6 
 
c 
c bytes 7-17: either "spherical harmonic" or "the rest" 
c                      gds6 50-80 
c 
      if(gds6.ge.50 .and. gds6.le.80) then 
        call gbyte(igds,nj,iskp,16) 
        iskp=iskp+16 
        call gbyte(igds,nk,iskp,16) 
        iskp=iskp+16 
        call gbyte(igds,nm,iskp,16) 
        iskp=iskp+16 
        call gbyte(igds,shcrp,iskp,8) 
        iskp=iskp+8 
        call gbyte(igds,csmode,iskp,8) 
        iskp=iskp+8 
        if(iprnt.ne.0) then 
          write(*,*) ' GDS NJ: ',nj 
          write(*,*) ' GDS NK: ',nk 
          write(*,*) ' GDS NM: ',nm 
          write(*,*) ' GDS shcrp: (table 9)',shcrp 
          write(*,*) ' GDS Coeff Storage Mode: (table 10) ',csmode 
        endif 
      else 
        call gbyte(igds,nx,iskp,16) 
        if(nx.ge.65535) nx=-99999 
        iskp=iskp+16 
        call gbyte(igds,ny,iskp,16) 
        iskp=iskp+16 
        call gbyte(igds,ksign,iskp,1) 
        call gbyte(igds,la1,iskp+1,23) 
        if(ksign.eq.1) la1=-la1 
        iskp=iskp+24 
        call gbyte(igds,ksign,iskp,1) 
        call gbyte(igds,lo1,iskp+1,23) 
        if(ksign.eq.1) lo1=-lo1 
        iskp=iskp+24 
        call gbytes(igds,gds17,iskp,1,0,8) 
        if(iprnt.ne.0) then 
          write(ilg,*) ' GDS Ni: ',nx 
          write(ilg,*) ' GDS Nj: ',ny 
          write(ilg,*) ' GDS La1: ',la1 
          write(ilg,*) ' GDS Lo1: ',lo1 
          write(ilg,*) ' GDS 17: (table 7) ',(gds17(i),i=1,8) 
        endif 
 
      endif 
 
c        TABELLA_GDS 
C 
c pds(3)  gds6    bytes 18-  :  
c         0,4    (1) lat/long grids (including Gaussian lat/long)          7-32 
c        201     (2) Arakawa semi-staggered grid on rotated lat/long grids 7-32 
c        202     (3) Arakawa filled E-grid on rotated lat/long grids       7-32 
c          5     (4) Polar Stereographic grids                             7-32 
c ?        2     (5) Gnomonic Projection or Stereographic grids            7-32 
c          3     (6) Lambert Conformal secant or tangent cone grids        7-42 
c          1     (7) Mercator grids                                        7-42 
c         90     (8) Space View Perspective or Orthographic grids          7-42 
c         50     (9) Spherical Harmonic Coefficients                       7-32 
c  98     10    (10) ECMWF Rotated lat/long grids                          7-42 
c  98     14    (11) ECMWF Rotated Gaussian grids                          7-42 
c  98     60    (12) ECMWF Rotated Spherical Harmonics                     7-42 
c  98     20    (13) ECMWF Stretched lat/long grids                        7-42 
c  98     24    (14) ECMWF Stretched Gaussian grids                        7-42 
c  98     70    (15) ECMWF Stretched Spherical Harmonics                   7-42 
c  98     30    (16) ECMWF Stretched and Rotated lat/long grids            7-52 
c  98     34    (17) ECMWF Stretched and Rotated Gaussian grids            7-52 
c  98     80    (18) ECMWF Stretched and Rotated Spherical Harmonics       7-52 
c 
c bytes 18- 
c 
      iskp=iskp+8 
      if(gds6.eq.0 .or. gds6.eq.4 .or. 
     &   gds6.eq.10 .or. gds6.eq.14) then  !lat/long grids (including Gaussian) 
        call gbyte(igds,ksign,iskp,1) 
        call gbyte(igds,la2,iskp+1,23) 
        if(ksign.eq.1) la2=-la2 
        iskp=iskp+24 
        call gbyte(igds,ksign,iskp,1) 
        call gbyte(igds,lo2,iskp+1,23) 
        if(ksign.eq.1) lo2=-lo2 
        iskp=iskp+24 
        call gbyte(igds,ksign,iskp,1) 
        call gbyte(igds,di,iskp+1,15) 
        if(di.eq.32767) then 
          di=-99999 
        else 
          if(ksign.eq.1) di=-di 
        endif 
        if(iprnt.ne.0) then 
          write(ilg,*) ' GDS La2: ',la2 
          write(ilg,*) ' GDS Lo2: ',lo2 
          write(ilg,*) ' GDS Di: ',di 
        endif 
        iskp=iskp+16 
        if(gds6.eq.4 .or. gds6.eq.14) then 
          call gbyte(igds,ngauss,iskp,16) 
          if(iprnt.ne.0) write(*,*) ' GDS N gaussian: ',ngauss 
        else 
          call gbyte(igds,ksign,iskp,1) 
          call gbyte(igds,dj,iskp+1,15) 
          if(dj.eq.32767) then 
            dj=-99999 
          else 
            if(ksign.eq.1) dj=-dj 
          endif 
          if(iprnt.ne.0) write(*,*) ' GDS Dj: ',dj 
        endif 
        iskp=iskp+16 
        call gbytes(igds,gds28,iskp,1,0,3) 
        if(iprnt.ne.0)  
     &    write(*,*) ' GDS 28: (table 8) ',(gds28(i),i=1,3) 
c ---Qui di seguito faccio in modo che legga i livelli verticali       
c ---Ho copiato pari pari la parte del BDS quando legge i dati 
c        iskp=iskp+40 
c        call gbyte(igds,isign,iskp,1) 
c        call gbyte(igds,refa,iskp+1,7) 
c        call gbyte(igds,refb,iskp+8,24) 
c        rdum=refb 
c        vertcoor=(-1)**isign * rdum * 16**(refa-64) / 2**24 
c         if(iprnt.ne.0)  
c     &     write(ilg,*) vertcoor 
c         write(ilg,*) isign, refa, refb 
c ---         
      elseif(gds6.eq.201) then    !Arakawa semi-staggered rotated lat/long 
        call gbyte(igds,la2,iskp,24) 
        iskp=iskp+24 
        call gbyte(igds,lo2,iskp,24) 
        if(iprnt.ne.0) then 
          write(*,*) ' GDS La2: ',la2 
          write(*,*) ' GDS Lo2: ',lo2 
        endif 
        iskp=iskp+24 
        call gbyte(igds,ksign,iskp,1) 
        call gbyte(igds,di,iskp+1,15) 
        if(di.eq.32767) then 
          di=-99999 
        else 
          if(ksign.eq.1) di=-di 
        endif 
        if(iprnt.ne.0) write(*,*) ' GDS Di: ',di 
        iskp=iskp+16 
        call gbyte(igds,ksign,iskp,1) 
        call gbyte(igds,dj,iskp,15) 
        if(dj.eq.32767) then 
          dj=-99999 
        else 
          if(ksign.eq.1) dj=-dj 
        endif 
        if(iprnt.ne.0) write(*,*) ' GDS Dj: ',dj 
        iskp=iskp+16 
        call gbytes(igds,gds28,iskp,1,0,3) 
        if(iprnt.ne.0)  
     &    write(*,*) ' GDS 28: (table 8) ',(gds28(i),i=1,3) 
      elseif(gds6.eq.202) then    !Arakawa filled E-grid on rotated lat/long 
        call gbyte(igds,la2,iskp,24) 
        iskp=iskp+24 
        call gbyte(igds,lo2,iskp,24) 
        if(iprnt.ne.0) then 
          write(*,*) ' GDS La2: ',la2 
          write(*,*) ' GDS Lo2: ',lo2 
        endif 
        iskp=iskp+24 
        call gbyte(igds,ksign,iskp,1) 
        call gbyte(igds,di,iskp+1,15) 
        if(di.eq.32767) then 
          di=-99999 
        else 
          if(ksign.eq.1) di=-di 
        endif 
        if(iprnt.ne.0) write(*,*) ' GDS Di: ',di 
        iskp=iskp+16 
        call gbyte(igds,ksign,iskp,1) 
        call gbyte(igds,dj,iskp+1,15) 
        if(dj.eq.32767) then 
          dj=-99999 
        else 
          if(ksign.eq.1) dj=-dj 
        endif 
        if(iprnt.ne.0) write(*,*) ' GDS Dj: ',dj 
        iskp=iskp+16 
        call gbytes(igds,gds28,iskp,1,0,3) 
        if(iprnt.ne.0)  
     &    write(*,*) ' GDS 28: (table 8) ',(gds28(i),i=1,3) 
      elseif(gds6.eq.5) then      !Polar Stereographic grids 
c 
        call gbyte(igds,ksign,iskp,1)      ! added 02/17/95 
        call gbyte(igds,lov,iskp+1,23)     ! added 02/17/95 
        if(ksign.eq.1) lov=-lov            ! added 02/17/95 
c       call gbyte(igds,lov,iskp,24) 
c 
        iskp=iskp+24 
        call gbyte(igds,dx,iskp,24) 
        iskp=iskp+24 
        call gbyte(igds,dy,iskp,24) 
        iskp=iskp+24 
        call gbyte(igds,gds27,iskp,1) 
        iskp=iskp+8 
        call gbytes(igds,gds28,iskp,1,0,3) 
c       call gbyte(igds,gds28(1),iskp,1) 
c       call gbyte(igds,gds28(2),iskp+1,1) 
c       call gbyte(igds,gds28(3),iskp+2,1) 
        if(iprnt.ne.0) then 
          write(*,*) ' GDS Lov: ',lov 
          write(*,*) ' GDS Dx: ',dx 
          write(*,*) ' GDS Dy: ',dy 
          write(*,*) ' GDS 27: (note 5) ',gds27 
          write(*,*) ' GDS 28: (table 8) ',(gds28(i),i=1,3) 
        endif 
 
      elseif(gds6.eq.2) then    !Gnomonic Projection or Stereographic  
        write(*,*) '****Gnomonic Projection,  not ready yet.' 
      elseif(gds6.eq.3) then      !Lambert Conformal secant or tangent cone 
c 
        call gbyte(igds,ksign,iskp,1)      ! added 02/17/95 
        call gbyte(igds,lov,iskp+1,23)     ! added 02/17/95 
        if(ksign.eq.1) lov=-lov            ! added 02/17/95 
c       call gbyte(igds,lov,iskp,24) 
c 
        iskp=iskp+24 
        call gbyte(igds,dx,iskp,24) 
        iskp=iskp+24 
        call gbyte(igds,dy,iskp,24) 
        iskp=iskp+24 
        call gbyte(igds,gds27,iskp,1) 
        iskp=iskp+8 
        call gbytes(igds,gds28,iskp,1,0,3) 
        iskp=iskp+8 
        call gbyte(igds,ksign,iskp,1) 
        call gbyte(igds,latin1,iskp+1,23) 
        if(ksign.eq.1) latin1=-latin1 
        iskp=iskp+24 
        call gbyte(igds,ksign,iskp,1) 
        call gbyte(igds,latin2,iskp+1,23) 
        if(ksign.eq.1) latin2=-latin2 
        iskp=iskp+24 
        call gbyte(igds,ksign,iskp,1) 
        call gbyte(igds,latsp,iskp+1,23) 
        if(ksign.eq.1) latsp=-latsp 
        iskp=iskp+24 
        call gbyte(igds,ksign,iskp,1) 
        call gbyte(igds,lonsp,iskp+1,23) 
        if(ksign.eq.1) lonsp=-lonsp 
        iskp=iskp+24 
        if(iprnt.ne.0) then 
          write(*,*) ' GDS Lov: ',lov 
          write(*,*) ' GDS Dx: ',dx 
          write(*,*) ' GDS Dy: ',dy 
          write(*,*) ' GDS 27: (note 5) ',gds27 
          write(*,*) ' GDS 28: (table 8) ',(gds28(i),i=1,3) 
          write(*,*) ' GDS latin 1: ',latin1 
          write(*,*) ' GDS latin 2: ',latin2 
          write(*,*) ' GDS lat SP:  ',latsp 
          write(*,*) ' GDS lon SP:  ',lonsp 
        endif 
      elseif(gds6.eq.1) then      !Mercator grids 
        call gbyte(igds,ksign,iskp,1) 
        call gbyte(igds,la2,iskp+1,23) 
        if(ksign.eq.1) la2=-la2 
        iskp=iskp+24 
        call gbyte(igds,ksign,iskp,1) 
        call gbyte(igds,lo2,iskp+1,23) 
        if(ksign.eq.1) lo2=-lo2 
        iskp=iskp+24 
        call gbyte(igds,ksign,iskp,1) 
        call gbyte(igds,latin,iskp+1,23) 
        if(ksign.eq.1) latin=-latin 
        iskp=iskp+24 
        call gbyte(igds,gds27,iskp,1) 
        iskp=iskp+8 
        call gbytes(igds,gds28,iskp,1,0,3) 
        iskp=iskp+8 
        call gbyte(igds,di,iskp,24) 
        iskp=iskp+24 
        call gbyte(igds,dj,iskp,24) 
        if(iprnt.ne.0) then 
          write(*,*) ' GDS La2:   ',la2 
          write(*,*) ' GDS Lo2:   ',lo2 
          write(*,*) ' GDS Latin: ',latin 
          write(*,*) ' GDS 27: (note 5) ',gds27 
          write(*,*) ' GDS 28: (table 8) ',(gds28(i),i=1,3) 
          write(*,*) ' GDS Di: ',di 
          write(*,*) ' GDS Dj: ',dj 
        endif 
      elseif(gds6.eq.90) then     !Space View Perspective or Orthographic grids 
        call gbyte(igds,dx,iskp,24) 
        iskp=iskp+24 
        call gbyte(igds,dy,iskp,24) 
        iskp=iskp+24 
        call gbyte(igds,xssp,iskp,24) 
        iskp=iskp+24 
        call gbyte(igds,yssp,iskp,24) 
        iskp=iskp+24 
        call gbytes(igds,gds28,iskp,1,0,3) 
        iskp=iskp+8 
        call gbyte(igds,orient,iskp,24) 
        iskp=iskp+24 
        call gbyte(igds,nr,iskp,24) 
        iskp=iskp+24 
        if(iprnt.ne.0) then 
          write(*,*) ' GDS Dx:     ',dx 
          write(*,*) ' GDS Dy:     ',dy 
          write(*,*) ' GDS Xssp:   ',xssp 
          write(*,*) ' GDS Yssp:   ',yssp 
          write(*,*) ' GDS 28: (table 8) ',(gds28(i),i=1,3) 
          write(*,*) ' GDS Orient: ',orient 
          write(*,*) ' GDS Nr:     ',nr 
        endif 
      elseif(gds6.eq.50) then     !Spherical Harmonic Coefficients 
 
      else 
      endif 
C       
c 
      if(gds6.eq.10 .or. gds6.eq.14 .or. gds6.eq.60) then   !ECMWF Rotated only 
        if(pds(3).eq.98) then 
          call gbyte(igds,ksign,iskp,1) 
          call gbyte(igds,latsp,iskp+1,23) 
          if(ksign.eq.1) latsp=-latsp 
          iskp=iskp+24 
          call gbyte(igds,ksign,iskp,1) 
          call gbyte(igds,lonsp,iskp+1,23) 
          if(ksign.eq.1) lonsp=-lonsp 
          iskp=iskp+24 
          call gbyte(igds,isign,iskp,1) 
          call gbyte(igds,refa,iskp+1,7) 
          call gbyte(igds,refb,iskp+8,24) 
          rdum=refb 
          angrot=(-1)**isign * rdum * 16**(refa-64) / 2**24 
          if(iprnt.ne.0) then 
            write(*,*) ' GDS Lat SP:            ',latsp 
            write(*,*) ' GDS Lon SP:            ',lonsp 
            write(*,*) ' GDS Angle of Rotation: ',angrot 
          endif 
        endif 
      elseif(gds6.eq.20 .or. gds6.eq.24 .or. gds6.eq.70) then 
c                                                        !ECMWF Stretched only 
        if(pds(3).eq.98) then 
          call gbyte(igds,ksign,iskp,1) 
          call gbyte(igds,latpl,iskp+1,23) 
          if(ksign.eq.1) latpl=-latpl 
          iskp=iskp+24 
          call gbyte(igds,ksign,iskp,1) 
          call gbyte(igds,lonpl,iskp+1,23) 
          if(ksign.eq.1) lonpl=-lonpl 
          iskp=iskp+24 
          call gbyte(igds,isign,iskp,1) 
          call gbyte(igds,refa,iskp+1,7) 
          call gbyte(igds,refb,iskp+8,24) 
          rdum=refb 
          strfac=(-1)**isign * rdum * 16**(refa-64) / 2**24 
          if(iprnt.ne.0) then 
            write(*,*) ' GDS Lat Pole:          ',latpl 
            write(*,*) ' GDS Lon Pole:          ',lonpl 
            write(*,*) ' GDS Stretching Factor: ',strfac 
          endif 
        endif 
      elseif(gds6.eq.30 .or. gds6.eq.34 .or. gds6.eq.80) then 
c                                         !ECMWF Stretched & Rotated 
        if(pds(3).eq.98) then 
          call gbyte(igds,ksign,iskp,1) 
          call gbyte(igds,latsp,iskp+1,23) 
          if(ksign.eq.1) latsp=-latsp 
          iskp=iskp+24 
          call gbyte(igds,ksign,iskp,1) 
          call gbyte(igds,lonsp,iskp+1,23) 
          if(ksign.eq.1) lonsp=-lonsp 
          iskp=iskp+24 
          call gbyte(igds,isign,iskp,1) 
          call gbyte(igds,refa,iskp+1,7) 
          call gbyte(igds,refb,iskp+8,24) 
          rdum=refb 
          angrot=(-1)**isign * rdum * 16**(refa-64) / 2**24 
          if(iprnt.ne.0) then 
            write(*,*) ' GDS Lat SP:            ',latsp 
            write(*,*) ' GDS Lon SP:            ',lonsp 
            write(*,*) ' GDS Angle of Rotation: ',angrot 
          endif 
          call gbyte(igds,ksign,iskp,1) 
          call gbyte(igds,latpl,iskp+1,23) 
          if(ksign.eq.1) latpl=-latpl 
          iskp=iskp+24 
          call gbyte(igds,ksign,iskp,1) 
          call gbyte(igds,lonpl,iskp+1,23) 
          if(ksign.eq.1) lonpl=-lonpl 
          iskp=iskp+24 
          call gbyte(igds,isign,iskp,1) 
          call gbyte(igds,refa,iskp+1,7) 
          call gbyte(igds,refb,iskp+8,24) 
          rdum=refb 
          strfac=(-1)**isign * rdum * 16**(refa-64) / 2**24 
          if(iprnt.ne.0) then 
            write(*,*) ' GDS Lat Pole:          ',latpl 
            write(*,*) ' GDS Lon Pole:          ',lonpl 
            write(*,*) ' GDS Stretching Factor: ',strfac 
          endif 
        endif 
      else 
      endif 
C     Per vedere nel file log i dati della griglia.... 
C      ... nel caso di lambert conformal... 
          write(ilg,*) ' GDS Dx:     ',dx 
          write(ilg,*) ' GDS Dy:     ',dy 
          write(ilg,*) ' GDS Xssp:   ',xssp 
          write(ilg,*) ' GDS Yssp:   ',yssp 
          write(ilg,*) ' GDS 28: (table 8) ',(gds28(i),i=1,3) 
          write(ilg,*) ' GDS Orient: ',orient 
          write(ilg,*) ' GDS Nr:     ',nr 
cC      ....nel caso di lat/long......           
          write(ilg,*) ' GDS Ni: ',nx 
          write(ilg,*) ' GDS Nj: ',ny 
          write(ilg,*) ' GDS La1: ',la1 
          write(ilg,*) ' GDS Lo1: ',lo1 
          write(ilg,*) ' GDS La2: ',la2 
          write(ilg,*) ' GDS Lo2: ',lo2 
          write(ilg,*) ' GDS Di: ',di 
c 
      if(gds28(3).eq.0) then 
         iscan=0 
      else 
         iscan=1 
      endif 
 
      ierr=0 
      return 
      end 
c 
c --------------------------------------------------------------- 
 
      subroutine GDS_TAB6(idcenter,gribedi,indx,name) 
      parameter    (maxx=255) 
      character*72 code(maxx),name 
      integer*4    idcenter,gribedi 
      data ifirst/0/ 
c 
      if(ifirst.ne.0) go to 10 
      do i=1,maxx 
        code(i)=' ' 
      enddo 
c 
c               123456789012345678901234567890123456789012345678901234567890 
      code(1)='latitude/longitude grid' 
      code(2)='Mercator projection' 
      code(3)='Gnomonic Porjection grid' 
      code(4)='Lambert Conformal projection' 
      code(5)='Gaussian latitude/longitude grid' 
      code(6)='Polar stereographic' 
      code(14)='Oblique Lambert conformal projection' 
      code(51)='spherical harmonic coefficients' 
      code(91)='space view perspective or orthographic grid' 
      code(202)='Arakawa semi-stagg E-grid on rotated lat/long grids' 
      code(203)='Arakawa filled E-grid on rotated lat/long grids' 
c 
   10 continue 
      if(idcenter.eq.98 .and. (gribedi.eq.0 .or. gribedi.eq.-1)) then 
        code(3)='Stereographic projection' 
        code(11)='rotated latitude/longitude grids' 
        code(14)=' ' 
        code(15)='rotated Gaussian latitude/longitude grids' 
        code(21)='stretched latitude/longitude grids' 
        code(25)='stretched Gaussian latitude/longitude grids' 
        code(31)='stretched and rotated latitude/longitude grids' 
        code(35)='stretched and rotated Gaussian lat/long grids' 
        code(61)='rotated spherical harmonic coefficients' 
        code(71)='streched spherical harmonics' 
        code(81)='stretched and rotated spher harmonic coeff' 
        code(91)=' ' 
        code(202)=' ' 
        code(203)=' ' 
      endif 
      idum=indx+1 
      name=code(idum) 
      return 
      end 
c 
c 
C ---------------------------------------------------------------------- 
      subroutine pds_GRIB1(iprnt,gribedi,iskp,nword, 
     &  kgds,kbms,dscalf,ierr,lvl2,pds,ipds) 
c 
c This is a generic GRIB Edition 1 PDS decoder  
c 
c SUN FORTRAN version  
c 
c lenpds: input, length of PDS in bytes 
c iskp:   input, number of bits to skip from the beginning 
c ipds:   input, integer array containing GRIB PDS 
c nword:  input, number of words used in ipds 
c pds:    returned, PDS array 
c kgds:   returned, GDS exists if kgds=1 
c kbms:   returned, BMS exists if kbms=1 
c ierr:   returned error code, 0=no error,  
c 
      dimension ipds(nword) 
      character pdsnam(22)*72 
      integer*4 pds(22),kgds,kbms,iskp,gribedi,ibloc(2) 
      data ifirst/0/ 
 
c 
      save 
c 
C     TABELLA_PDS 
 
      if(ifirst.eq.0) then                                              ! pds bytes 
        pdsnam(1)=' 1=length'                                           !  1-3 
        pdsnam(2)=' 2=parameter table number'                           !  4 
        pdsnam(3)=' 3=center ID'                                        !  5 
        pdsnam(4)=' 4=process ID'                                       !  6 
        pdsnam(5)=' 5=grid ID'                                          !  7 
        pdsnam(6)=' 6=flag'                                             !  8 
        pdsnam(7)=' 7=parameter ID (table 2)'                           !  9  
        pdsnam(8)=' 8=type of level or layer (table 3 & 3a)'            ! 10 
        pdsnam(9)=' 9=pressure or height of level or layer'             ! 11-12 
        pdsnam(10)='10=year of centry'                                  ! 13 
        pdsnam(11)='11=month of year'                                   ! 14 
        pdsnam(12)='12=day of month'                                    ! 15 
        pdsnam(13)='13=hour of day'                                     ! 16 
        pdsnam(14)='14=minute of hour'                                  ! 17 
        pdsnam(15)='15=forecast time units (table 4)'                   ! 18 
        pdsnam(16)='16=P1'                                              ! 19 
        pdsnam(17)='17=P2'                                              ! 20 
        pdsnam(18)='18=time range indicator (table 5)'                  ! 21 
        pdsnam(19)='19=number included in averge when pds(18)<>0'       ! 22-23 
        pdsnam(20)='20=number missing from averages or accumulations'   ! 24 
        pdsnam(21)='21=century of initial (reference) time'             ! 25 
c                                                                       ! 26 
        pdsnam(22)='22=decimal scale factor D'                          ! 27-28 
c                                                                       ! 29-40 
c                                                                       ! 41- 
        ifirst=9 
      endif 
c 
      if(gribedi.eq.-1) then 
        pds(1)=20 
        pds(2)=128 
      else 
        call gbyte(ipds,pds(1),iskp,24) 
        iskp=iskp+24 
        call gbyte(ipds,pds(2),iskp,8) 
        iskp=iskp+8 
      endif 
      call gbyte(ipds,pds(3),iskp,8) 
      iskp=iskp+8 
      call gbyte(ipds,pds(4),iskp,8) 
      iskp=iskp+8 
      call gbyte(ipds,pds(5),iskp,8) 
      iskp=iskp+8 
      if(gribedi.eq.-1) then 
        call gbyte(ipds,kgds,7*8+7,1)                   ! ECMWF Edition X 
        call gbyte(ipds,kbms,7*8+6,1)                   ! ECMWF Edition X 
      else 
        call gbyte(ipds,kgds,iskp,1) 
        call gbyte(ipds,kbms,iskp+1,1) 
        pds(6)=(kgds*10+kbms)*1000000                   !950711 
      endif 
      iskp=iskp+8 
       
C         
      call gbyte(ipds,pds(7),iskp,8) 
      iskp=iskp+8 
      call gbyte(ipds,pds(8),iskp,8) 
      iskp=iskp+8 
      if(pds(8).eq.100 .or. pds(8).eq.103 .or. 
     &   pds(8).eq.105 .or. pds(8).eq.107 .or. 
     &   pds(8).eq.109 .or. pds(8).eq.111 .or. 
     &   pds(8).eq.113 .or. pds(8).eq.125 .or.  
     &   pds(8).eq.115 .or. pds(8).eq.117 .or.      !1998MAY14 
     &   pds(8).eq.119 .or.                         !1998MAY14 
     &   pds(8).eq.160 .or. pds(8).eq.200 .or.  
     &   pds(8).eq.201 ) then 
        if(gribedi.eq.-1) then                   ! ECMWF Edition X 
          call gbytes(ipds,ibloc,10*8,8,0,2) 
          pds(9)=ibloc(1)*32+ibloc(2) 
        else 
          call gbyte(ipds,pds(9),iskp,16) 
        endif 
        lvl2=-9999                               ! lvl2 is not used 
        iskp=iskp+16 
      else 
        call gbyte(ipds,pds(9),iskp,8) 
        iskp=iskp+8 
        call gbyte(ipds,lvl2,iskp,8) 
        iskp=iskp+8 
      endif 
      call gbyte(ipds,pds(10),iskp,8) 
      iskp=iskp+8 
      call gbyte(ipds,pds(11),iskp,8) 
      iskp=iskp+8 
      call gbyte(ipds,pds(12),iskp,8) 
      iskp=iskp+8 
      call gbyte(ipds,pds(13),iskp,8) 
      iskp=iskp+8 
      call gbyte(ipds,pds(14),iskp,8) 
      iskp=iskp+8 
      call gbyte(ipds,pds(15),iskp,8) 
      iskp=iskp+8 
      idum=iskp                                 !970103 
      call gbyte(ipds,pds(16),iskp,8) 
      iskp=iskp+8 
      call gbyte(ipds,pds(17),iskp,8) 
      iskp=iskp+8 
      call gbyte(ipds,pds(18),iskp,8) 
      iskp=iskp+8 
      if(pds(18).eq.10) then                    !970103 
        call gbyte(ipds,pds(16),idum,16)        !970103 
        pds(17)=-9999                           !970103 
      endif                                     !970103 
      call gbyte(ipds,pds(19),iskp,16) 
      iskp=iskp+16 
      call gbyte(ipds,pds(20),iskp,8) 
      iskp=iskp+8 
c 
c fix if pdslen < 28 
c 
      dscalf=0. 
      pds(22)=-9999 
      if(pds(1).le.24) pds(21)=-9999 
      if(pds(1).ge.28) then 
        call gbyte(ipds,pds(21),iskp,8) 
        iskp=iskp+8 
c 
c skip byte 26 
c 
        iskp=iskp+8 
        call gbyte(ipds,isign,iskp,1) 
        call gbyte(ipds,pds(22),iskp+1,15) 
        if(isign.eq.1) pds(22)=-pds(22)       !961113 
        dscalf=float(pds(22))                 !961113 
      endif 
c 
c print out PDS 
c 
      if(iprnt.ne.0) then 
         do i=1,22 
            if(i.eq.6) then 
               write(*,101) pds(i),kgds,kbms,pdsnam(i) 
 101           format(i10,i2,i2,a70) 
            elseif(i.eq.9) then 
               write(*,115) pds(i),lvl2,pdsnam(i) 
 115           format(i7,i6,1x,a70) 
            else 
               write(*,102) pds(i),pdsnam(i) 
 102           format(i10,4x,a70) 
            endif 
         enddo 
c         write(18,181)pds(7),pds(9),lvl2 
c 181     format('Find Variable: ',3i7) 
 
c        write(20,2010)pds(7),pds(9),lvl2 
c 2010   format('Var-Pres-Hgt: ',3i8) 
 
      endif 
 
c fill ib1par 
c 
c     ib1par(1)=pds(3) 
c     ib1par(2)=pds(4) 
c     ib1par(3)=pds(5) 
c     ib1par(4)=pds(6) 
c     ib1par(5)=pds(7) 
c     ib1par(6)=pds(8) 
c     ib1par(7)=pds(9) 
c     ib1par(8)=lvl2 
c     ib1par(9)=pds(10) 
c     ib1par(10)=pds(11) 
c     ib1par(11)=pds(12) 
c     ib1par(12)=pds(13) 
c     ib1par(13)=pds(14) 
c     ib1par(14)=pds(15) 
c     ib1par(15)=pds(16) 
c     ib1par(16)=pds(17) 
c     ib1par(17)=pds(18) 
c     ib1par(18)=pds(19) 
c     ib1par(19)=pds(20) 
c     ib1par(20)=pds(21) 
c 
      ierr=0 
      return 
      end 
c 
c 
c ************************************************************* 
 
      subroutine fill_ingrib(mxone,mxlen,nword,lenrd,lentot, 
     &                       ione,ingrib) 
      dimension ingrib(mxlen),ione(mxone) 
c 
      mword=lenrd/nword 
      if(mod(lenrd,nword).ne.0) mword=mword+1  
      do i=1,mword 
        lentot=lentot+1 
        ingrib(lentot)=ione(i) 
      enddo 
      return 
      end 
c 
c 
 
C --------------------------------------------------------- 
      subroutine fl360(flon) 
 
C     Purpose: Convert longitude to 0-360 convention 
       
      if(flon.le.0) flon=360.+ flon 
         
      return 
      end 
 
C ***************************************************************** 
 
      subroutine pass2d(datan,nmax,npoints,iz,ivar) 
 
C Pass 1-D data to a 2-D array area (one slice of a 3d array) 
       
      include 'eta2m3d.par' 
      include 'eta2m3d.cm1' 
 
      dimension datan(nmax) 
 
      if(iscan.ne.0) then 
         print *,'Error: Pass 2D only good for ISCAN=0' 
         print *,'       ISCAN=',iscan 
         stop 
      endif 
 
      nxlen=mxnx 
      do ii=1,npoints 
         j=(ii-1)/nxlen+1 
         i=ii-(j-1)*nxlen 
         x3d(i,j,iz,ivar)=datan(ii) 
c         write(*,*) x3d(i,j,iz,ivar)
      enddo 
 
      return 
      end 
C ***************************************************************** 
 
      subroutine pass2d_2(datan,nmax,npoints,ivar) 
 
C Pass 1-D data to a 2-D array area 
       
      include 'eta2m3d.par' 
      include 'eta2m3d.cm1' 
 
      dimension datan(nmax) 
 
      if(iscan.ne.0) then 
         print *,'Error: Pass 2D only good for ISCAN=0' 
         print *,'       ISCAN=',iscan 
         stop 
      endif 
 
      nxlen=mxnx 
      do ii=1,npoints 
         j=(ii-1)/nxlen+1 
         i=ii-(j-1)*nxlen 
         x2d(i,j,ivar)=datan(ii) 
      enddo 
 
      return 
      end 
 
C ***************************************************************** 
 
      subroutine pass2d_ter(datan,nmax,npoints) 
 
C Pass 1-D data to a 2-D array area 
       
      include 'eta2m3d.par' 
      include 'eta2m3d.cm1' 
 
      dimension datan(nmax) 
 
      if(iscan.ne.0) then 
         print *,'Error: Pass 2D only good for ISCAN=0' 
         print *,'       ISCAN=',iscan 
         stop 
      endif 
 
      nxlen=mxnx 
c     print *,'check terrain' 
      do ii=1,npoints 
         j=(ii-1)/nxlen+1 
         i=ii-(j-1)*nxlen 
         ter(i,j)=datan(ii) 
C        print *,i,j,ter(i,j) 
      enddo 
c     print *,'End of check terrain' 
 
      return 
      end 
 
c********************************************************************* 
      subroutine julday(iyr,imo,iday,ijuldy) 
c---------------------------------------------------------------------- 
c 
c --- calmm%   version: 1.0       level: 901130                  julday 
c ---          j. scire, src 
c 
c --- purpose:  compute the julian day number from the gregorian 
c               date (month, day) 
c 
c --- inputs: 
c           iyr - integer      - year 
c           imo - integer      - month 
c          iday - integer      - day 
c 
c --- output: 
c          ijul - integer      - julian day 
c 
c --- julday called by:  main 
c --- julday calls:      none 
c---------------------------------------------------------------------- 
      integer kday(12) 
      data kday/0,31,59,90,120,151,181,212,243,273,304,334/ 
 
      ijuldy=kday(imo)+iday 
      if(imo.le.2)return 
      if(mod(iyr,4).eq.0)ijuldy=ijuldy+1 
 
      return 
      end 
 
C -------------------------------------------------------- 
 
      Subroutine rotate(ws,wd,flat,flon) 
 
C     Rotate map wind to true north 
      include 'eta2m3d.par' 
      include 'eta2m3d.cm1' 
 
      xlonc=flonv 
      call fl360(xlonc) 
      call fl360(flon) 
 
      dlon=(flon-xlonc)*conf 
 
c      print 9,xlonc,conf,flon,flat,dlon,wd,ws 
c 9    format(7f10.2) 
 
      if(dlon .lt.-180.) dlon=dlon+360. 
      if(dlon .gt. 180.) dlon=dlon-360. 
 
      if(ws.gt.1.e-9) then 
         if (flat.lt.0.) then 
            wd=wd-dlon          !southern hemisphere 
         else    
            wd=wd+dlon          !northern hemisphere 
         endif 
      endif 
 
      if (wd.lt.  0.) wd=wd+360. 
      if (wd.gt.360.) wd=wd-360. 
 
      return 
      end 
 
C ******************************************************************** 
 
      Subroutine uv2ws(uu,vv,ws,wd) 
 
C     Purpose: Convert u,v (m/s) to wind speed (m/s) and wind direction(deg) 
      ws=sqrt(uu**2+vv**2) 
      if(ws.gt.1.e-9) then 
         xwd=270.-57.295778*atan2(vv,uu) 
         wd=amod(xwd,360.) 
      else 
         wd=0. 
      endif 
 
      return 
      end 
 
C -------------------------------------------------- 
      Subroutine getrange 
 
C Purpose: Select I/J ranges based required lat/lon, 
c          determine which gridpoints are within  
c          selected domain 
c Zhong Wu 
C 10/16/2001 
 
      include 'eta2m3d.par' 
      include 'eta2m3d.cm1' 
 
      i1=9999 
      i2=-9999 
      j1=9999 
      j2=-9999 
 
      do j=1,mxny 
         do i=1,mxnx 
 
            flat=xy212(4,i,j) 
            flon=xy212(3,i,j) 
 
            if ((flat.ge.rlatmin).and.(flat.le.rlatmax) .and.  
     &            (flon.ge.rlonmin) .and. (flon.le.rlonmax)) then 
 
               if(i.lt.i1) i1=i 
               if(i.gt.i2) i2=i 
               if(j.lt.j1) j1=j 
               if(j.gt.j2) j2=j 
            endif 
             
         enddo 
      enddo    
 
      if(i1.eq.9999 .or. i2.eq.-9999 .or. 
     &   j1.eq.9999 .or. j2.eq.-9999) then 
         print *,'Error in getrange:',i1,i2,j1,j2 
         stop 
      endif 
 
      nx1=i1 
      nx2=i2 
      ny1=j1 
      ny2=j2 
 
c      nz1=1 
c      nz2=mxnz 
 
C   ...calcola il numero dei punti griglia nel sottodominio.... 
 
      nxsub=(nx2-nx1)+1 
      nysub=(ny2-ny1)+1 
      nzsub=(nz2-nz1)+1 
 
c --- SW point LCC locations (km) for Selected SubDomain 
      rlatswsub=xy212(4,nx1,ny1) 
      rlonswsub=xy212(3,nx1,ny1) 
c 
      xorgsub=xy212(5,nx1,ny1) 
      yorgsub=xy212(6,nx1,ny1) 
 
      write(ilg,214)nx1,nx2,ny1,ny2,nxsub,nysub 
      write(ilg,215)rlatswsub,rlonswsub 
 214  format('Selected domain I: ',2i5,/, 
     &       '                J: ',2i5,/ 
     &       '  Number of Grids: ',2i5) 
 215  format('Selected domain SW lat/lon: ',2f12.3,/, 
     &       'Selected domain SW     X/Y: ',2f12.3) 
 
c --- SW point LCC locations (km) for AWIPS whole domain 
      rlatsw=xy212(4,1,1) 
      rlonsw=xy212(3,1,1) 
      xorg=xy212(5,1,1) 
      yorg=xy212(6,1,1) 
 
      write(ilg,314)mxnx,mxny 
      write(ilg,315)rlatsw,rlonsw,xorg,yorg 
 314  format('SWIPS domain Grids (NX/NY): ',2i5) 
 315  format('SWIPS domain SW lat/lon: ',2f12.3,/, 
     &       'SWIPS domain SW     X/Y: ',2f12.3) 
 
C     Domain Center: Assume center at I=185, J=129 
      ic=nint(mxnx/2.0+0.5) 
      jc=nint(mxny/2.0+0.5) 
 
      rlatc=xy212(4,ic,jc) 
      rlonc=xy212(3,ic,jc) 
      truelat1=25.0 
      truelat2=25.0 
      flonref=flonv 
      if(flonref.gt.180) flonref=360.0-flonref  ! WH if > 180 degree 
 
      dxym=(xy212(5,mxnx,1)-xy212(5,1,1))/(mxnx-1) 
 
      write(ilg,316)ic,jc,rlatc,rlonc 
      write(ilg,317)truelat1,truelat2,flonref 
      write(ilg,318)dxym 
 
 316  format('SWIPS Fake Center I/J & Latc/Lonc: ',2i5,2f12.4) 
 317  format('SWIPS True Lat1/Lat2 & Lon_ref: ',3f12.4) 
 318  format('SWIPS Grid size: ',f12.4) 
 
      return 
      end 
 
 
c*********************************************************************** 
 
       subroutine getflname(string,nt) 
 
c ---   Get file name from an input line  
c       The file name is at the begining of and before the first  
c       blank space in the line 
 
c ------------------------------------------------------------------- 
       character*80 string 
 
       nt=index(string,' ')-1 
       string=string(1:nt) 
 
       return 
       end 
 
c*********************************************************************** 
      subroutine noblank(string,nt) 
 
c ---   Get file name from an input line  
c       The file name is at the begining of and before the first  
c       blank space in the line 
 
c ------------------------------------------------------------------- 
      character*80 string 
 
      do i=80,1,-1 
         if(string(i:i).ne.' ') then 
            nt=i 
            return 
         endif 
      enddo 
         
      nt=10 
       
      return 
      end 
 
C ---------------------------------------------------------- 
      subroutine wrthd 
 
C     Output CALMM5 M3D and M2D headers 
      include 'eta2m3d.par' 
      include 'eta2m3d.cm1' 
 
C     CALMM5 M3D/M2D Record #1 and Record #2 
c      write(im3d,101)title,cset3d,cver,clevel 
c      write(im2d,101)title,cset2d,cver,clevel 
c 101  format(a80,/,3a12) 
 
      call noblank(title,nt) 
 
      write(im3d,101)title(1:nt) 
      write(im3d,1011)cset3d,cver,clevel 
      if(iosrf.eq.1) then       
         write(im2d,101)title(1:nt) 
         write(im2d,1011)cset3d,cver,clevel 
      endif 
c 101  format(a80) 
 101  format(a) 
 1011 format(3a12) 
       
C     Record #3 
      write (im3d,102)ioutw,ioutq,ioutc,iouti,ioutg,iosrf 
 102  format (6(i3)) 
 
      if(iosrf.eq.1)  
     &  write (im2d,102)ioutw,ioutq,ioutc,iouti,ioutg,iosrf 
 
      write(ilg,103) ioutw, ioutq, ioutc, iouti, ioutg, iosrf 
 103  format(3x,'ioutw:',i2/, 
     :     3x,'ioutq:',i2/, 
     :     3x,'ioutc:',i2/, 
     :     3x,'iouti:',i2/, 
     :     3x,'ioutg:',i2/, 
     :     3x,'iosrf:',i2/) 
 
C     Record #4 (Need Vertical level- NZ) 
      write(im3d,105)rlatc,rlonc,truelat1,truelat2, 
     &      xorg,yorg,dxym,mxnx,mxny,mxnz  
 105  format('LLC ',f9.4,f10.4,2f7.2,2f10.3,f8.3,2i4,i3) 
 
      if (iosrf.eq.1) 
     1     write(im2d,105)rlatc,rlonc,truelat1,truelat2, 
     2     xorg,yorg,dxym,mxnx,mxny,mxnz  
 
C     Record #5 
      write(im3d,106)inhyd,imphys,icupa,ibltyp,ifrad,isoil, 
     :     ifddaan,ifddaob,igrdt,ipbl,ishf,ilhf,iustr,iswdn, 
     :     ilwdn,ist1,ist2,ist3,ist4,ist5,ist6,iswou,ilwou 
      if (iosrf.eq.1) then 
         write(im2d,105)inhyd,imphys,icupa,ibltyp,ifrad,isoil, 
     &        ifddaan,ifddaob,igrdt,ipbl,ishf,ilhf,iustr,iswdn, 
     &        ilwdn,ist1,ist2,ist3,ist4,ist5,ist6,iswou,ilwou 
      endif 
 106  format (30(i3)) 
       
C     Record #6 
      nhours=nfile 
      write(im3d,96)idate,nhours,nxsub,nysub,nzsub 
      if(iosrf.eq.1) 
     &  write(im2d,96)idate,nhours,nxsub,nysub,nzsub 
 
c header record #7: 
      write(im3d,97)nx1,ny1,nx2,ny2,nz1,nz2 
     &      ,rlonmin,rlonmax,rlatmin,rlatmax 
      if(iosrf.eq.1) 
     & write(im2d,97)nx1,ny1,nx2,ny2,nz1,nz2 
     &      ,rlonmin,rlonmax,rlatmin,rlatmax 
 
c next nz records:sigma levels (normalized by 1013.0 hPa)  
c      dps=pres(1)-pres(mxnz) 
      dps=1013.0 
 
      do k=nz1,nz2 
c         sgm=(pres(k)-pres(mxnz))/dps 
         sgm=pres(k)/dps 
         write(im3d,98)sgm 
         if(iosrf.eq.1) write(im2d,98)sgm 
      enddo 
 
c geophysical records.  
c note: lat/lon and elev at dot point, land use at cross point 
      idum=-9 
 
      do j=ny1,ny2 
         do i=nx1,nx2 
            flat=xy212(4,i,j) 
            flon=xy212(3,i,j) 
c            ter(i,j)=xy212(5,i,j) 
            write(im3d,99)i,j,flat,flon,nint(ter(i,j)) 
     &            ,idum,fmiss,fmiss,imiss 
            if(iosrf.eq.1)  
     &          write(im2d,99)i,j,flat,flon,nint(ter(i,j)) 
     &            ,idum,fmiss,fmiss,imiss 
         enddo 
      enddo 
 96   format(i10,i5,4i4) 
 97   format(6i4,2f10.4,2f9.4) 
 98   format(f6.3) 
 99   format(2i4,f9.4,f10.4,i5,i3,1x,f9.4,f10.4,i5) 
 
      return 
      end 
 
C ------------------------------------------------ 
      Subroutine getz(ip9,iz,ifound) 
 
C     Get vertical level for a 3-D variable 
C     Note: GRIB variable may not be in order vertically 
      include 'eta2m3d.par' 
      include 'eta2m3d.cm1' 
 
      ifound=0 
      do k=1,mxnz 
         if(ip9.eq.nint(pres(k))) then 
            iz=k 
            ifound=1 
            return 
         endif 
      enddo 
 
c      print *,'Warning: This Vertical level does not exist ' 
c      print *,'         for all hours  - ',ip9 
 
      return 
 
      end 
c---------------------------------------------------------------------- 
      subroutine incr(iyr,ijul,ihr,nhrinc) 
c---------------------------------------------------------------------- 
c 
c --- CALMET   Version: 5.2       Level: 991104                    INCR 
c              J. Scire, SRC 
c 
c --- PURPOSE:  Increment the time and date by "NHRINC" hours 
c 
c --- UPDATE 
c --- V5.0-V5.1     991104  (DGS): Allow for a negative "increment" of 
c                                  up to 24 hours and an arbitrarily 
c                                  large positive nhrinc 
c                                  (adopt the CALPUFF 5.0 (980304) 
c                                   version of INCR) 
c 
c --- INPUTS: 
c       IYR    - integer - Current year 
c       IJUL   - integer - Current Julian day 
c       IHR    - integer - Current hour (00-23) 
c       NHRINC - integer - Time increment (hours) 
c 
c               NOTE: "NHRINC" must >= -24 
c                      Hour is between 00-23 
c 
c       Parameters: IO6 
c 
c --- OUTPUT: 
c       IYR    - integer - Updated year 
c       IJUL   - integer - Updated Julian day 
c       IHR    - integer - Updated hour (00-23) 
c 
c --- INCR called by: RDHD4, RDHD5 
c --- INCR calls:     none 
c---------------------------------------------------------------------- 
c 
c 
c --- Check nhrinc 
      if(nhrinc.lt.-24) then 
         write(*,*)'ERROR IN SUBR. INCR -- Invalid value of NHRINC ', 
     1   '-- NHRINC = ',nhrinc 
         stop 
      endif 
 
c --- Save increment remaining (needed if nhrinc > 8760) 
      nleft=nhrinc 
c 
c --- Process change in hour 
      if(nhrinc.gt.0)then 
c 
10       ninc=MIN0(nleft,8760) 
         nleft=nleft-ninc 
c 
c ---    Increment time 
         ihr=ihr+ninc 
         if(ihr.le.23)return 
c 
c ---    Increment day 
         ijul=ijul+ihr/24 
         ihr=mod(ihr,24) 
c 
c ---    ILEAP = 0 (non-leap year) or 1 (leap year) 
         if(mod(iyr,4).eq.0)then 
            ileap=1 
         else 
            ileap=0 
         endif 
c 
         if(ijul.gt.365+ileap) then 
c ---       Update year 
            iyr=iyr+1 
            ijul=ijul-(365+ileap) 
         endif 
c 
c ---    Repeat if more hours need to be added 
         if(nleft.GT.0) goto 10 
c 
      elseif(nhrinc.lt.0)then 
c ---    Decrement time 
         ihr=ihr+nhrinc 
         if(ihr.lt.0)then 
            ihr=ihr+24 
            ijul=ijul-1 
            if(ijul.lt.1)then 
               iyr=iyr-1 
               if(mod(iyr,4).eq.0)then 
                  ijul=366 
               else 
                  ijul=365 
               endif 
            endif 
         endif 
      endif 
c 
      return 
      end 
c---------------------------------------------------------------------- 
      subroutine grday(iyr,ijul,imo,iday) 
c---------------------------------------------------------------------- 
c 
c --- CALMET   Version: 5.2       Level: 901130                   GRDAY 
c ---          J. Scire, SRC 
c 
c --- Compute the Gregorian date (month, day) from the Julian day 
c 
c --- INPUTS: 
c           IYR - integer      - Year 
c          IJUL - integer      - Julian day 
c        Parameters: IO6 
c 
c --- OUTPUT: 
c           IMO - integer      - Month 
c          IDAY - integer      - Day 
c 
c --- GRDAY called by:  COMP 
c --- GRDAY calls:      none 
c---------------------------------------------------------------------- 
c 
c --- include parameters 
c 
      integer kday(12,2) 
      data kday/31,59,90,120,151,181,212,243,273,304,334,365, 
     1          31,60,91,121,152,182,213,244,274,305,335,366/ 
c 
      ileap=1 
      if(mod(iyr,4).eq.0)ileap=2 
      if(ijul.lt.1.or.ijul.gt.kday(12,ileap))go to 11 
c 
      do 10 i=1,12 
      if(ijul.gt.kday(i,ileap))go to 10 
      imo=i 
      iday=ijul 
      if(imo.ne.1)iday=ijul-kday(imo-1,ileap) 
      return 
10    continue 
c 
11    continue 
      write(*,12)iyr,ijul 
12    format(//2x,'ERROR in SUBR. GRDAY -- invalid Julian day '//2x, 
     1 'iyr = ',i5,3x,'ijul = ',i5) 
      stop 
      end 
 
c ******************************************************************* 
      subroutine timestamp(iyr,imon,iday,ihour,idate)  
 
c     Get time stamp 
 
      iyr2k=iyr 
      if(iyr.lt.100) then 
         if(iyr.lt.50) then 
            iyr2k=iyr+2000 
         else 
            iyr2k=iyr+1900 
         endif 
      endif 
      idate=ihour+iday*100+imon*10000+iyr2k*1000000 
 
      return 
      end 
 
 
C ------------------------------------------------- 
      Subroutine getw(wp,pp,atk,rh,ww,aq) 
 
C Purpose 
C    Convert omega velocity to w velocity 
C 
C Zhongxiang Wu 
 
      parameter(g=9.80616,Rd=287.0,alpha=Rd/g) 
 
      call gettv(pp,atk,rh,tv,aq) 
       
      ro=pp*100./Rd/tv 
      ww=-wp/ro/g 
 
c      print *,'ro:',ro,pp,atk,ww 
 
      return 
      end 
 
c---------------------------------------------------------------------- 
      function esat(tdegc) 
c---------------------------------------------------------------------- 
c 
c --- CALMET   Version: 5.2       Level: 901130                    ESAT 
c ---          J. Scire, SRC 
c 
c --- PURPOSE:  Compute the saturation water vapor pressure (mb) using 
c               the method of Lowe (1977) (JAM, 16, pp 100-103). 
c 
c --- INPUT: 
c                TDEGC - real          - Air temperature (deg. C) 
c 
c --- OUTPUT: 
c                 ESAT - real          - Saturation water vapor 
c                                        pressure (mb) 
c 
c --- ESAT called by: WATER 
c --- ESAT calls:     none 
c---------------------------------------------------------------------- 
      data a0/6.107799961/,a1/4.436518521e-1/,a2/1.428945805e-2/ 
      data a3/2.650648471e-4/,a4/3.031240396e-6/,a5/2.034080948e-8/ 
      data a6/6.136820929e-11/ 
c 
c --- compute saturation water vapor pressure (mb) 
c --- NOTE: temperature is in deg. C 
 
c     This equation gives negative esat at low temperature 
      esat=a0+tdegc*(a1+tdegc*(a2+tdegc*(a3+tdegc*(a4+tdegc* 
     1 (a5+tdegc*a6))))) 
 
c     Magnus equation 
c      aa=(17.67*tdegc)/(243.5+tdegc) 
c      esat=6.112*exp(aa) 
 
      if(esat.lt.0) then 
         print *,'esat < 0 : ',esat 
         print *,'set esat=0' 
         esat=0 
      endif 
c 
      return 
      end 
 
 
c ------------------------------------------------------------ 
      subroutine gettv(p,tk,rh,tv,aq) 
 
c Purpose: Get virtual temperature 
c  
c Zhongxiang Wu 
c 6/19/2001 
c 
c     p: pressure in mb 
c     tk: air temperature in K 
c     rh: relative humidity in % 
c     tv: return value for virtual temperature in K 
c     qq: specific humidity 
c     aq: mixing ratio 
 
      if(rh.gt.100) then 
         print *,'RH > 100 in gettv. Set it to 100' 
         write(*,*) rh 
         rh=100. 
      endif 
 
      es0=satvap(tk) 
      ee=es0*rh/100. 
 
      qq=0.622*ee/((p*100)-0.378*ee) 
      if(qq.lt.0) qq=0 
 
c      tv=tk*(1+0.608*qq) 
c --- Sara 16/12/2004: virtual temperature is calculated using the  
c --- formula proposed by Wallace (pg.52) in which all the pressures 
c --- are in Pascal 
      tv=tk/(1-(ee/(p*100))*(1-0.622)) 
      aq=qq/(1-qq) 
 
c      print *,'Sub gettv (tk/tv/dt):', tk,tv,tv-tk 
 
      return 
      end 
 
c ----------------------------------------------------------- 
 
c******************************************************************** 
        real function relhum(ta,sh,spp) 
c******************************************************************** 
c 
c-------------------------------------------------------------------- 
c --- input: 
c --- taa = AIR TEMPERATURE [deg K] 
c --- sh = specific humidity[kg/ Kg] 
c --- spp = MEAN SEA LEVEL PRESSURE [hPa]  ( 1hPa = 100 Pa = 1mb ) 
c 
c 
c --- It uses a formula used by ECMWF: 
c 
c                
c     Rh = spp*sh*(1/expsi)/((1+((1/expsi)-1)*sh))*esat(t) 
c                  
c 
c where: 
c T = air temperature 
c expsi = Rd / Rv = 0.622 
c 
c 
c 
c We use the TETENS formula: 
c 
c es(T) = C1 * exp(C3*(T - To)/(T - C4))     from ECMWF Manual 
c 
c 
c 
c-------------------------------------------------------------------- 
c 
        real ta,sh,spp, expsi 
        data expsi / 0.622 / 
c 
        spp =  spp*100     ! convert spp in [ Pa ] 
        aaa =  (spp*sh)/expsi 
        dff = satvap(ta)   ! in [ Pa ] 
        bbb = (1+((1-expsi)/expsi)*sh)*dff  
        relhum =  100*aaa / bbb 
c 
        return 
        end 
 
c******************************************************************** 
        real function satvap(t) 
c==================================================================== 
c 
c-------------------------------------------------------------------- 
c --- it calculates the SATURATION VAPOR PRESSURE [ Pa ] from the 
c --- TEMPERATURE [ deg K ] 
c-------------------------------------------------------------------- 
c 
        real c1,c3,c4,aa,bb,t00 
        data c1 /610.78/ 
        data t00 /273.16/ 
c 
        if(t.lt.t00) then 
           c3 = 21.875 
           c4 = 7.66 
        elseif(t.ge.t00) then 
           c3 = 17.269 
           c4 = 35.86 
        endif 
        aa = c3*(t - t00) 
        bb = t - c4 
        satvap = c1 * exp(aa/bb) 
c 
        return 
        end 
 
c-------------------------------------------------------------------- 
 
c    <HIBU, software di utilita per  grid> 
c    Copyright (C) 2000  <SMR ARPA> 
c 
c    $Date: 2005/03/09 11:25:00 $    $Revision: 1.3 $ 
c    $Id: caleta.f,v 1.3 2005/03/09 11:25:00 barsotti Exp $ 
 
c    Questo programma è software  libero; è lecito ridistribuirlo e/o 
c    modificarlo secondo i  termini della Licenza Pubblica Generica SMR  
c    come pubblicata  da ARPA SMR ; riferirsi alla versione 1 
c    della licenza o (a scelta) ad una versione successiva. 
 
c    Questo programma è distribuito  nella speranza che sia utile,  ma 
c    SENZA  ALCUNA GARANZIA;  senza  neppure la  garanzia  implicita di 
c    COMMERCIABILITA' o di APPLICABILITA' PER UN PARTICOLARE SCOPO.  Si 
c    veda la Licenza Pubblica Generica SMR per avere maggiori dettagli. 
 
c    Ognuno dovrebbe avere ricevuto una copia della Licenza Pubblica 
c    Generica SMR insieme a questo programma; in caso contrario, la si 
c    può ottenere da Agenzia Regionale Prevenzione e Ambiente (ARPA) 
c    Servizio Meteorologico Regionale (SMR), Viale Silvani 6, 40122 
c    Bologna, Italia 
c    http://www.smr.arpa.emr.it 
 
C ************************************************************** 
      subroutine rltlwd(almd,aphd,tpus,tpvs,tlm0d,ctpho,stpho     
     &                ,pus,pvs)                                  
C ************************************************************** 
c Routine per antiruotare il vento; usa funzioni goniom. in gradi. 
c 
c almd,aphd:	coordinate vere (NON ruotate) del punto 
c tpus,tpvs:	componenti ruotate del vento 
c	output: 
c pus,pvs:	componenti vere del vento 
 
 
comstart RLTLWD 
c 
c SUBROUTINE RLTLWD(ALMD,APHD,TPUS,TPVS,TLM0D,CTPH0,STPH0,PUS,PVS) 
c 
cidx trasforma le componenti del vento dal sistema ruotato al sistema geografico 
cvedi LTLWD TLLD RTLLD  rot_grib_LAMBO 
c 
c Trasforma le componeti u,v espresse nel sistema ruotato tipico dei modelli  
c meteo LAMBO e LOKAL a componenti u,v espresse nel sistema geografico 
c 
c input: 
c 
c  ALMD       real      longitudine sistema geografico (rapp. decimale) 
c  APHD       real      latitudine sistema geografico (rapp. decimale) 
c 
c  tpus       real      componente u nel sistema ruotato 
c  tpvs       real      componente v nel sistema ruotato 
c 
c  TLM0D      real      longitudine del punto intercetto del meridiano di  
c                       Greenwich con lequatore del sistema ruotato, nel  
c                       sistema geografico 
c                        
c  CTPH0      real      coseno della latitudine del punto intercetto del  
c                       meridiano di Greenwich con lequatore del sistema  
c                       ruotato nel sistema geografico (rapp. decimale) 
c  STPH0      real      seno della latitudine del punto intercetto del  
c                       meridiano di Greenwich con lequatore del sistema  
c                       ruotato nel sistema geografico 
c 
c output:  
c 
c  pus          real     componente u nel sistema geografico 
c  pvs          real     componente v nel sistema geografico 
c 
comend 
 
 
      relm=(almd-tlm0d)                                          
      srlm=sind(relm)                                            
      crlm=cosd(relm)                                            
C                                                                
      sph=sind(aphd)                                               
      cph=cosd(aphd)                                               
C                                                                
      cc=cph*crlm                                                
      tph=asin(ctpho*sph-stpho*cc)                               
C                                                                
      rctph=1./cos(tph)                                          
      cray=stpho*srlm*rctph                                      
      dray=(ctpho*cph+stpho*sph*crlm)*rctph                      
      dc=dray*dray+cray*cray                                     
      pus=(dray*tpus+cray*tpvs)/dc                               
      pvs=(dray*tpvs-cray*tpus)/dc                               
C                                                                
      return                                                     
      end                                                        
