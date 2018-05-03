c----------------------------------------------------------------------
c --- API_CHEM -- CALPUFF subroutines added for API (MCHEM=6,7)
c----------------------------------------------------------------------
c
c --- CALPUFF    Version: TNG-7.1.0    Level: 140521          API_CHEM
c
c --- PURPOSE: This collection of routines was prepared for API by
c              AER to add chemical transformation and aerosol options
c              to CALPUFF.  They are bundled as an include-file to
c              facilitate future modular upgrades.  Note that another
c              set of subroutines (isorropia.for) has not been placed
c              in api_chem.for and must be 'included' to complete
c              the code for the related options.
c
c --- IMPLEMENTATION:  Several modifications made to these new options
c              have been assigned either new user-selected modeling
c              option variables (changed via the control file) or
c              local logical operators that may be switched in an
c              individual subroutine to restore the original logic.
c              CALPUFF will need to be recompiled if a local logical
c              variable is changed.
c
c              The following variable settings will restore the original
c              features and logic of the API code (for testing):
c
c              Control File
c                     MLWC = 0 Do not read gridded cloud water from file
c                     MNH3 = 1 Read monthly ammonia vertical profiles
c                  MAVGNH3 = 0 Do not average ammonia vertical profiles
c                              across puff
c                   RNITE1 = 0.0 for no heterogeneous SO2 transformation
c              Local Logical in Subroutine
c                     L_KGPCM = .FALSE. in CHEMRIV6, CHEMRIV7
c                 L_TNO3FLOOR = .FALSE. in CHEMRIV6, CHEMRIV7 
c                 L_RAINCLOUD = .FALSE. in CHEMRIV6, CHEMRIV7
c                     L_SCAV6 = .FALSE. in WET
c
c -----------------------------
c --- CONTENT:
c -----------------------------
c --- AER routines based on existing CALPUFF routines
c      subroutine chemriv6
c      subroutine chemriv7
c      subroutine chmriv6
c      subroutine chmriv7
c      subroutine setbckoc
c
c --- AER ISORROPIA interface routine
c     subroutine isodriver
c     (include 'isorropia.for')
c
c --- AER CALTECH SOA routines
c     subroutine soadriver
c     subroutine caltech_soa
c     subroutine soasub
c
c --- AQUEOUS-CHEMISTRY ROUTINES BASED ON RADM/CMAQ
c     subroutine aqradm
c     function hlconst
c     function index1
c -----------------------------
c
c
c --- UPDATE
c
c **********************************************************************
c --- Exponent, Inc. Updates:
c **********************************************************************
c --- V6.41-V6.42_x1.1 140521  : Call ISORROPIA with the METASTABLE
c                                control mode ON
c                                Modified: ISODRIVER
c                      140521  : Use iterative procedure in calling
c                                CALTECH_SOA to revise absorbing
c                                organic mass based on current aerosol
c                                mass as equilibrium is sought
c                                Modified: CHEMRIV7, SOADRIVER
c                      140521  : Use cloud fraction to set puff mass
c                                altered when using AUX-file LWC
c                                Use local temperature, pressure for AQ
c                                Adjust mass transformed by fraction of
c                                puff that overlays cloud water layers
c                                Modified: CHEMRIV6, CHEMRIV7
c                      140521  : Enforce NO3<=TNO3 to avoid negative
c                                HNO3 concentrations due to precision in
c                                HNO3=TNO3-NO3 operation
c                                Also guard against NO3<0
c                                Modified: CHEMRIV6, CHEMRIV7
c                      140521  : Add minimum RH and SO4 for ISORROPIA
c                                Modified: CHEMRIV6, CHEMRIV7
c **********************************************************************
c
c --- V6.4-V6.41   110301 (DGS): Treat aqueous-phase transformation
c                                case of precip without cloud water
c                                (from AUX file) by using a default
c                                lwc=0.5g/m3 with at least 10% cloud
c                                cover
c                                Modified: CHEMRIV6, CHEMRIV7
c
c --- V6.302-V6.4  101025 (DGS): Update ISORROPIA to V2.1
c                                Replaced: (include file isorropia.for)
c                                          (include file isrpia.inc)
c                                Modified: ISODRIVER
c
c --- V5.8-V6.302  100917 (DGS): Restructure value**-n to value**(-n)
c                                to satisfy LF95 compiler
c                                Modified: CHMRIV6, CHMRIV7
c                  100917 (DGS): Place NH3 profiles into /CHEMDAT/
c                                and add chembkz to chembk logic
c                  100917 (DGS): pass RSHET(fraction/hr) for setting
c                                heterogeneous SO2 reaction rate
c                                Modified: CHEMRIV6, CHEMRIV7
c                                          CHMRIV6, CHMRIV7
c                  100917 (DGS): Pass local cloud water mixing ratio
c                                when available from CALMET 3D file
c                                Modified: CHEMRIV6, CHEMRIV7
c                  100917 (DGS): Add local logical to convert LWC
c                                passed to AQRADM from g/m3 to kg/m3
c                                Modified: CHEMRIV6, CHEMRIV7
c                  100917 (DGS): Add local logical to use the minimum
c                                TNO3 as a floor rather than as a
c                                breakpoint for NO3=0.0
c                                Modified: CHEMRIV6, CHEMRIV7
c                  100917 (DGS): Add local logical to set a minimum
c                                cloud cover (10%) whenever there is
c                                liquid precipitation, and
c                                add but do not activate code to
c                                change effective scavenging
c                                coefficients to limit their action
c                                in any timestep to just the cloud
c                                fraction affecting the puff mass.
c                                Modified: CHEMRIV6, CHEMRIV7
c----------------------------------------------------------------------
      subroutine chemriv6(delt,qin,coz,ctnh3,ch2o2,maqchem,rshet,temp,
     1                    rhum,rhoair,pivol,zlen,zpuf,cldamr,fzcld,
     2                    cldt,cldp,cloud,prate,zcoef,nspec,ldb1,io6,
     &                    rh_isrp,so4_isrp,
     3                    rate,scav)
c----------------------------------------------------------------------
c
c --- CALPUFF    Version: TNG-7.1.0    Level: 140521          CHEMRIV6
c                P. Karamchandani, AER (Adapted from chemriv)
c
c --- PURPOSE:  This routine sets up call to RIVAD chemical
c               transformation subroutines, and passes results to
c               calling program
c
c --- UPDATES:
c
c --- V6.41-V6.42_x1.1 140521  : Use cloud fraction to set puff mass
c                                altered when using AUX-file LWC
c                                Use local temperature, pressure for AQ
c                                Adjust mass transformed by fraction of
c                                puff that overlays cloud water layers
c                                Condition NO3 to be no larger than TNO3
c                                and no smaller than zero
c                                Add minimum RH and SO4 for ISORROPIA
c
c --- V6.302-V6.41 110301 (DGS): Treat aqueous-phase transformation
c                                case of precip without cloud water
c                                (from AUX file) by using a default
c                                lwc=0.5g/m3 with at least 10% cloud
c                                cover
c --- V5.8-V6.302  100917 (DGS): Pass RSHET(fraction/hr) for setting
c                                heterogeneous SO2 reaction rate
c                         (DGS): Add local cloud water mixing ratio
c                                to input argument list (MLWC option)
c                         (DGS): Add local logical to convert LWC
c                                passed to AQRADM from g/m3 to kg/m3
c                         (DGS): Add local logical to use the minimum
c                                TNO3 as a floor rather than as a
c                                breakpoint for NO3=0.0
c                         (DGS): Add local logical to set a minimum
c                                cloud cover (10%) whenever there is
c                                liquid precipitation for use in the
c                                aqueous phase reaction/scavenging,
c                                and add but do not activate code to
c                                change the effective scavenging
c                                coefficients by limiting their action
c                                in any timestep to just the cloud
c                                fraction affecting the puff mass.
c
c --- INPUTS:
c         DELT - real    - integration time interval (hours)
c  QIN(mxspec) - real    - Pollutant mass (g) in the puff
c                            QIN(1) = SO2
c                            QIN(2) = SO4
c                            QIN(3) = NO
c                            QIN(4) = NO2
c                            QIN(5) = TNO3 (HNO3 + NO3)
c                            QIN(6) = NH4NO3
c                            QIN(+) = Not Used Here
c
c          COZ - real    - puff ozone concentration (ppb)
c        CTNH3 - real    - background ammonia concentration (ppb)
c        CH2O2 - real    - puff H2O2 concentration (ppb)
c      MAQCHEM - integer - Aqueous phase transformation flag
c                            0 = aqueous phase transformation
c                                not modeled
c                            1 = transformation rates adjusted
c                                for aqueous phase reactions
c        RSHET - real    - SO2 heterogeneous loss rate (fraction/hr)
c         TEMP - real    - temperature (deg. K)
c         RHUM - real    - relative humidity (percent)
c       RHOAIR - real    - surface air density (kg/m**3)
c        PIVOL - real    - Reciprocal of puff volume (1/m**3)
C         ZLEN - real    - Puff vertical length scale (m)
c         ZPUF - real    - Puff/Slug elevation (m MSL)
c       CLDAMR - real    - Average cloud water mixing ratio (g/kg)
c
c --- 6.42_x1.1
c        FZCLD - real    - Fraction of puff layer in cloud
c         CLDT - real    - Average cloud temperature (K)
c         CLDP - real    - Average cloud pressure (atm)
c      RH_ISRP - real    - Minimum relative humidity (%)
c                          for ISORROPIA
c     SO4_ISRP - real    - Minimum SO4 (g/m3) for ISORROPIA
c
c        CLOUD - real    - Cloud cover (tenths)
c        PRATE - real    - Precip. rate (mm/hr)
c        ZCOEF - real    - Cosine of solar zenith angle
c        NSPEC - real    - number of species
c         LDB1 - logical - Control variable for printing of debug
c                          information
c          IO6 - integer - Fortran unit number of printed output
c
c
c --- OUTPUT:
c  QIN(mxspec) - real    - Pollutant mass (g) in the puff
c                            QIN(1) = SO2
c                            QIN(2) = SO4
c                            QIN(3) = NO
c                            QIN(4) = NO2
c                            QIN(5) = TNO3 (HNO3 + NO3)
c                            QIN(6) = NH4NO3
c                            QIN(+) = Not Used Here
c      RATE(4) - real    - Transformation rates (percent/hour)
c                            R(1) -- SO2 loss rate
c                            R(2) -- NOX loss rate
c                            R(3) -- TNO3 formation rate
c                            R(4) -- NO  loss rate
c     SCAV(6) - real    -  Scavenging coefficients (1/s)
c                            SCAV(1) -- SO2
c                            SCAV(2) -- SO4
c                            SCAV(3) -- NO
c                            SCAV(4) -- NO2
c                            SCAV(5) -- HNO3
c                            SCAV(6) -- NO3
c
c
c --- CHEMRIV6 called by: CHEM
c --- CHEMRIV6 calls:     PHOT, CHMRIV6, ISODRIVER, AQRADM
c----------------------------------------------------------------------
c
      implicit none

! --- Arguments
      integer nspec, maqchem, io6
      real delt, coz, ctnh3, ch2o2, temp, rhum, rhoair, pivol, zlen
      real zpuf, cloud, prate, zcoef, rshet
      real qin(nspec), scav(nspec)
      real rate(4)
      real cldamr

c --- 6.42_x1.1
      real cldt,cldp,fzcld,fcloud2
      real rh_isrp,so4_isrp
      real cldta,cldpa
      real confcta,rhoaira
      real test

      logical ldb1

! --- Locals
      real ppb(6),ppbi(6),q(6)
      real rmwt(6)
      real lwc    ! Liquid water content, g/m3
      real cozm, ch2o2m, ctnh3m  ! Concentrations in mols/mols air units
      real tno3floor
c --- 6.42_x1.1
      real tso4min
c --- Local controls
      logical l_kgpcm, l_tno3floor, l_raincloud

c --- Concentration array (moles/mole of air) for AQRADM
      real con(6)

c --- Note: TNO3 is weighted as NO3
      data rmwt/64.,96.,30.,46.,62.,62./

      real dt, vkgmol, confct, rk1, zcoefb, o3ppm
      real presur, f, ppbix,  ppbi4, ppbx
      real delno, tso4, tno3, tnh3, rhfrac, pno3
      real patm, taucld, rhoairm
      integer is, iss, j, ii, i

      real pcloud, fcloud

c ----------------------
c --- Set local controls
c ----------------------
c --- These enable 3 changes to the code to be reverted to their
c --- original condition.
c --- 1.  Selecting l_kgpcm=.TRUE. converts the LWC passed to AQRADM
c ---     from g/m3 to kg/m3, since AQRADM assumes it to be kg/m3.
c ---     Selecting l_kgpcm=.FALSE. retains the original code and
c ---     passes LWC to AQRADM in g/m3.
      data l_kgpcm/.TRUE./
c --- 2.  Selecting l_tno3floor=.TRUE. uses the cut-off TNO3
c ---     concentration as a floor, and computes NO3 corresponding
c ---     to this floor for all TNO3 less than this floor.  The
c ---     resulting ratio of NO3/TNO3(floor) is then multiplied by
c ---     the actual TNO3 to obtain the final NO3 concentration.
c ---     Selecting l_tno3floor=.FALSE. retains the original code
c ---     and NO3=0.0 for all TNO3<TNO3(floor).
      data l_tno3floor/.TRUE./
c --- 3.  Selecting l_raincloud=.TRUE. sets a minimum cloud cover
c ---     of 10% whenever liquid precipitation is non-zero.  This
c ---     only applies to the aqueous phase option and forces wet
c ---     removal whenever there is rain.  When cloud cover is zero,
c ---     aqueous conversion and wet removal are also zero.
c ---     (This also changes the effective scavenging coefficients
c ---     by limiting their action in any timestep to just the
c ---     cloud fraction affecting the puff mass.)--NA
      data l_raincloud/.TRUE./
c ----------------------

c --- Transfer mass data to local array
      iss = MIN(nspec,6)
      do is = 1, iss
         q(is) = qin(is)
      end do
      do is = (iss+1), 6
         q(is) = 0.0
      end do

c --- Compute initial concentrations as PPB
c --- kg-Molar volume (m^3) at ambient T,P computed from air density
      vkgmol = 28.97/rhoair
c --- Conversion factor to ppm in RIVAD
      confct = (1.0e-3)*vkgmol*pivol
      do is = 1, iss
         ppbi(is) = (1.0e09)*confct*q(is)/rmwt(is)
      end do

c --- Get NO2 photolysis rate
      call PHOT(zpuf,cloud,zcoef,rk1,zcoefb)

c --- Do conversions needed for RIVAD input arguments
c --- RIVAD weights TNO3 as HNO3: scale mass by 63/62=1.016129
      q(5) = q(5)*1.016129
c --- ozone in PPM rather than PPB
      o3ppm = 1.0e-03*coz
c --- Implied pressure (mb) -- use T0=273. P0=1013. as in RIVAD
      patm = (22.4/vkgmol)*(temp/273.)
      presur = 1013.*patm

      if(ldb1) then
         write(io6,*)
         write(io6,*)'CHEMRIV6:'
         write(io6,*)'    rk1,rhoair,O3ppb = ',rk1,rhoair,coz
         write(io6,*)'     TNH3ppb,H2O2ppb = ',ctnh3, ch2o2
         write(io6,*)'    presur,temp,rhum = ',presur,temp,rhum
         write(io6,*)'  cloud,prate,cldamr = ',cloud,prate,cldamr
         write(io6,*)'     zlen,zmsl,zcoef = ',zlen,zpuf,zcoef
         write(io6,*)' vkgmol,confct,pivol = ',vkgmol,confct,pivol

c --- 6.42_x1.1
         write(io6,*)'     fzcld,cldt,cldp = ',fzcld,cldt,cldp
         write(io6,*)'    rh_isrp,so4_isrp = ',rh_isrp,so4_isrp

         write(io6,*)
c         write(io6,101)
101      format('ppb =  SO2',7x,'SO4',7x,'NO',7x,'NO2',7x,'TNO3',
     &              6x,'NO3')
         write(io6,*)'Starting Concs -----'
         write(io6,101)
         write(io6,'(3x,6e10.2)') (ppbi(j),j=1,6)
         write(io6,*)'Starting Puff Mass (g) -----'
         write(io6,'(3x,6f10.2)') (qin(j),j=1,6)
      endif

c --- Call RIVAD module
      call CHMRIV6(q,temp,presur,rhum,o3ppm,rk1,zcoef,delt,confct,
     &             rshet)
c --- ozone in PPB
      coz = 1.0e03*o3ppm

c --- Re-weight TNO3 as NO3
      q(5) = q(5)*0.984127

      if(ldb1) then
         write(io6,*)'After CHMRIV6, Puff Mass (g) -----'
         write(io6,'(3x,6f10.2)') (q(j),j=1,6)
      endif

      if (maqchem.EQ.1) then
c
c --- initialize scavenging coefficients
         do ii = 1, 6
            scav(ii) = 0.
         end do

c --- Use local cloud water mixing ratio if valid, or
c --- Assign liquid water content if there is cloud cover and the
c --- temperature is above freezing
         fcloud = cloud * 0.1    ! fractional cloud cover
         pcloud = 0.0
         if(l_raincloud .AND. temp.GT.273.15) then
c ---       Force at least 10% cloud cover when there is rain
            if(prate.GT.0.0) fcloud=MAX(fcloud,0.1)
         endif

         if(fcloud.GT.0.0) pcloud = prate/fcloud

c --- 6.42_x1.1
         rhoaira=rhoair
         confcta=confct
         cldta=temp
         cldpa=patm
c ---    Fraction of puff within cloud horizontally and vertically
         fcloud2=fcloud
         if(cldamr.GE.0.0) then
c ---       Valid cloud water provided (use it)
c ---       Use average cloud temperature and pressure
            cldta=cldt
            cldpa=cldp
            rhoaira=rhoair*(cldpa/patm)*(temp/cldta)
            confcta=(1.0e-3)*(28.97/rhoaira)*pivol
c ---       Convert: g/kg(air) * rhoair(kg/m3) ==> g/m3(air)
            lwc=cldamr*rhoaira
c ---       Set minimum cloud fraction to 10%
            fcloud=MAX(fcloud,0.1)
            pcloud=prate/fcloud
c ---       Fraction of puff within cloud horizontally and vertically
            fcloud2=fcloud*fzcld

         elseif (fcloud.gt.0. .and. temp.gt.273.15) then
            if (prate .gt. 0.) then
               lwc = 0.5
            else
               lwc = 0.1
            end if
         else
            lwc = 0.
         end if

         if(l_kgpcm) then
c ---       Units for lwc in AQRADM are kg/m3
            lwc=0.001*lwc
         endif

         if (lwc .gt. 0.) then
c
c --- Calculate concs in moles/mole air units
            do ii = 1, 6

c --- 6.42_x1.1
               con(ii) = MAX(confcta*q(ii)/rmwt(ii),0.)

            end do
c
c --- Get HNO3 conc from total nitrate and PM nitrate
            con(5) = MAX(con(5) - con(6),0.)
c
            cozm = coz * 1.E-9
            ch2o2m = ch2o2 * 1.E-9
            ctnh3m = ctnh3 * 1.E-9
c
c --- Call aqueous-phase chemistry module
            taucld = delt * 3600.   ! timestep in seconds

c --- 6.42_x1.1
            rhoairm = rhoaira * 1.E3/28.97   ! air density in moles/m3
            if(ldb1) then
               write(io6,*)'AQRADM:cldta,cldpa,pcloud,lwc,cozm,'//
     &                     'ch2o2m,ctnh3m,con6= '
               write(io6,*)'called:   ',cldta,cldpa,pcloud,lwc,cozm,
     &                      ch2o2m,ctnh3m,con
            endif
            call AQRADM(cldta,cldpa,taucld,pcloud,lwc,cozm,ch2o2m,
     &                  ctnh3m,con,rhoairm,zlen,scav)
            if(ldb1) then
               write(io6,*)'returned: ',cldta,cldpa,pcloud,lwc,cozm,
     &                      ch2o2m,ctnh3m,con
               write(io6,*)'    scav: ',scav
            endif

c --- Assign adjusted SO2 and SO4 concs; adjust for cloud cover
            q(1) = q(1)*(1. - fcloud2) + fcloud2*con(1)*rmwt(1)/confcta
            q(2) = q(2)*(1. - fcloud2) + fcloud2*con(2)*rmwt(2)/confcta
c
c --- Adjusted oxidant concs
            coz = coz*(1. - fcloud2) + fcloud2*cozm*1.E9
            ch2o2 = ch2o2*(1. - fcloud2) + fcloud2*ch2o2m*1.E9

c
c --- Adjusted scavenging rates
            if (prate .gt. 0.) then
c ---          Possible alternate method (NA)
c ---           if(l_raincloud .AND. fcloud.LT.0.99) then
c ---              do ii = 1, 6
c ---                 scav(ii)=-ALOG(1.0 - fcloud*
c --- &                        (1.0 - EXP(-scav(ii)*taucld) ))/taucld
c ---              end do
c ---           else
                  do ii = 1, 6
                     scav(ii) = scav(ii) * fcloud
                  end do
c ---           endif
            end if
         end if 
      end if 

c --- Condition mass results
      do is = 1,iss
         q(is) = MAX(q(is),0.0)
      end do

c --- Compute ending concentrations as PPB
      do is = 1,iss
         ppb(is) = (1.0e09)*confct*q(is)/rmwt(is)
      end do

c --- Compute conversion rates (%/hr) for QA review
      do i = 1,4
         rate(i) = 0.0
      end do

c --- Compute these logs only if debug output is ON
      if(ldb1) then
         dt = .01*delt
c ---    SOX:
         if(ppbi(1).GT.0.0 .AND. ppb(1).GT.0.0) rate(1) = -ALOG(ppb(1)/
     &                                                   ppbi(1))/dt
c ---    NOX:
         ppbix = ppbi(4) + ppbi(3)
         ppbx = ppb(4) + ppb(3)
         if(ppbix.GT.0.0 .AND. ppbx.GT.0.0) rate(2) = -ALOG(ppbx/
     &                                                   ppbix)/dt
c ---    TNO3:  Conversion of NO2 after NO:NO2 process
         ppbi4 = ppbi(4) + ppbi(3) - ppb(3)
         if(ppbi4.GT.0.0 .AND. ppb(4).GT.0.0) rate(3) = -ALOG(ppb(4)/
     &                                                   ppbi4)/dt
c ---    NO:
         delno = ppb(3) - ppbi(3)
         if(delno.LT.0.0) then
            if(ppbi(3).GT.0.0 .AND. ppb(3).GT.0.0) rate(4) = 
     &                          ALOG(ppb(3)/ppbi(3))/dt
         elseif(delno.GT.0.0) then
            if(ppbi4.GT.0.0 .AND. ppb(4).GT.0.0) rate(4) = -ALOG(ppbi4/
     &                                                    ppbi(4))/dt
         endif
      endif

c --- Inorganic aerosol equilibrium with ISORROPIA
      if(q(5).gt.0.0)then
c
c --- concs in mols/m3

c --- 6.42_x1.1
c ---   Apply so4(g/m3) concentration constraint from control file
        tso4=q(2)*pivol
        tso4=MAX(tso4,so4_isrp)/rmwt(2)

        tso4 = MAX(tso4,1.E-12)
        tno3 = q(5) * pivol / rmwt(6)
        tnh3 = ctnh3 * rhoair / 28.97E6

c --- 6.42_x1.1
c ---   Apply RH(%) constraint from control file to compute RH fraction
        rhfrac=0.01*MAX(rhum,rh_isrp)

        if(tno3.gt.1.E-10)then
          if(ldb1) then

c --- 6.42_x1.1
             write(io6,*)'ISODRIVER:tso4,tno3,tnh3,rhfrac,temp     = '
             write(io6,*)'called:   ',tso4,tno3,tnh3,rhfrac,temp

          endif
          call isodriver(tso4,tno3,tnh3,rhfrac,temp,pno3)
          if(ldb1) then
             write(io6,*)'returned: ',tso4,tno3,tnh3,rhfrac,temp,pno3
          endif

        elseif(l_tno3floor) then
          tno3floor=1.E-10
          if(ldb1) then
             write(io6,*)
     &             'ISODRIVER:tso4,tno3floor,tnh3,rhfrac,temp,pno3= '

c --- 6.42_x1.1
             write(io6,*)'called:   ',
     &                        tso4,tno3floor,tnh3,rhfrac,temp

          endif
          call isodriver(tso4,tno3floor,tnh3,rhfrac,temp,pno3)
          if(ldb1) then
             write(io6,*)'returned: ',
     &                        tso4,tno3floor,tnh3,rhfrac,temp,pno3
          endif
          pno3=pno3*(tno3/tno3floor)

        else
          pno3 = 0.
        endif
        q(6) = pno3 * rmwt(6) / pivol

c --- 6.42_x1.1
c ---   Condition NO3 (q(6)) to be no smaller than zero
        if(q(6).LT.0.0) then
           write(io6,*)'CHEMRIV6 Warning: reset NO3(g) from ',
     &                 q(6),' to ZERO'
           q(6)=0.0
        endif
c ---   Condition NO3 to be no larger than TNO3 (q(5))
c ---   (q(5)>0.0 in this block)
        test=q(6)/q(5)-1.0
        if(test.GT.1.0e-06) then
           write(io6,*)'CHEMRIV6 Warning: reset NO3(g) from ',
     &                 q(6),' to ',q(5)
        endif
        q(6)=MIN(q(5),q(6))

      endif

c --- Transfer revised mass data to original array
      do is = 1,iss
         qin(is) = q(is)
      end do

      if(ldb1) then
         write(io6,*)'After ISODRIVER, Puff Mass (g) -----'
         write(io6,'(3x,6f10.2)') (qin(j),j=1,6)
         write(io6,*)
      endif

      return
      end

c----------------------------------------------------------------------
      subroutine chemriv7(delt,qin,coz,ctnh3,ch2o2,maqchem,rshet,temp,
     1                    rhum,rhoair,pivol,zlen,zpuf,cldamr,fzcld,
     2                    cldt,cldp,cloud,prate,zcoef,nspec,ldb1,io6,
     &                    rh_isrp,so4_isrp,
     3                    rate,scav)
c----------------------------------------------------------------------
c
c --- CALPUFF    Version: TNG-7.1.0    Level: 140521          CHEMRIV7
c                P. Karamchandani, AER (Adapted from chemriv)
c
c --- PURPOSE:  This routine sets up call to RIVAD chemical
c               transformation subroutines, and passes results to
c               calling program
c
c --- UPDATES:
c
c --- V6.41-V6.42_x1.1 140521  : Use cloud fraction to set puff mass
c                                altered when using AUX-file LWC
c                                Use local temperature, pressure for AQ
c                                Adjust mass transformed by fraction of
c                                puff that overlays cloud water layers
c                                Condition NO3 to be no larger than TNO3
c                                and no smaller than zero
c                                Add list file unit to SOADRIVER
c                                Add minimum RH and SO4 for ISORROPIA
c
c --- V6.302-V6.41 110301 (DGS): Treat aqueous-phase transformation
c                                case of precip without cloud water
c                                (from AUX file) by using a default
c                                lwc=0.5g/m3 with at least 10% cloud
c                                cover
c --- V5.8-V6.302  100917 (DGS): Pass RSHET(fraction/hr) for setting
c                                heterogeneous SO2 reaction rate
c                         (DGS): Add local cloud water mixing ratio
c                                to input argument list (MLWC option)
c                         (DGS): Add local logical to convert LWC
c                                passed to AQRADM from g/m3 to kg/m3
c                         (DGS): Add local logical to use the minimum
c                                TNO3 as a floor rather than as a
c                                breakpoint for NO3=0.0
c                         (DGS): Add local logical to set a minimum
c                                cloud cover (10%) whenever there is
c                                liquid precipitation for use in the
c                                aqueous phase reaction/scavenging,
c                                and add but do not activate code to
c                                change the effective scavenging
c                                coefficients by limiting their action
c                                in any timestep to just the cloud
c                                fraction affecting the puff mass.
c
c --- INPUTS:
c         DELT - real    - integration time interval (hours)
c  QIN(mxspec) - real    - Pollutant mass (g) in the puff
c                            QIN(1) = SO2
c                            QIN(2) = SO4
c                            QIN(3) = NO
c                            QIN(4) = NO2
c                            QIN(5) = TNO3 (HNO3 + NO3)
c                            QIN(6) = NH4NO3
c                            QIN(7)  = Primary OC (POC)
c                            QIN(8)  = TOL (Toluene)
c                            QIN(9)  = TOLAER1 (Condensable product)
c                            QIN(10) = TOLAER2 (Condensable product)
c                            QIN(11) = ATOLA1 (SOA 1 from TOL)
c                            QIN(12) = ATOLA2 (SOA 2 from TOL)
c                            QIN(13) = XYL (Xylene)
c                            QIN(14) = XYLAER1 (Condensable product)
c                            QIN(15) = XYLAER2 (Condensable product)
c                            QIN(16) = AXYLA1 (SOA 1 from XYL)
c                            QIN(17) = AXYLA2 (SOA 2 from XYL)
c                            QIN(18) = ALKH (Higher alkanes)
c                            QIN(19) = ALKHAER (Condensable product)
c                            QIN(20) = ALKHA (SOA 1 from ALKH)
c                            QIN(21) = PAH
c                            QIN(22) = PAHAER1 (Condensable product)
c                            QIN(23) = PAHAER2 (Condensable product)
c                            QIN(24) = APAHA1 (SOA 1 from PAH)
c                            QIN(25) = APAHA2 (SOA 2 from PAH)
c
c          COZ - real    - puff ozone concentration (ppb)
c        CTNH3 - real    - background ammonia concentration (ppb)
c        CH2O2 - real    - puff H2O2 concentration (ppb)
c      MAQCHEM - integer - Aqueous phase transformation flag
c                            0 = aqueous phase transformation
c                                not modeled
c                            1 = transformation rates adjusted
c                                for aqueous phase reactions
c        RSHET - real    - SO2 heterogeneous loss rate (fraction/hr)
c         TEMP - real    - temperature (deg. K)
c         RHUM - real    - relative humidity (percent)
c       RHOAIR - real    - surface air density (kg/m**3)
c        PIVOL - real    - Reciprocal of puff volume (1/m**3)
C         ZLEN - real    - Puff vertical length scale (m)
c         ZPUF - real    - Puff/Slug elevation (m MSL)
c       CLDAMR - real    - Average cloud water mixing ratio (g/kg)
c
c --- 6.42_x1.1
c        FZCLD - real    - Fraction of puff layer in cloud
c         CLDT - real    - Average cloud temperature (K)
c         CLDP - real    - Average cloud pressure (atm)
c      RH_ISRP - real    - Minimum relative humidity (%)
c                          for ISORROPIA
c     SO4_ISRP - real    - Minimum SO4 (g/m3) for ISORROPIA
c
c        CLOUD - real    - Cloud cover (tenths)
c        PRATE - real    - Precip. rate (mm/hr)
c        ZCOEF - real    - Cosine of solar zenith angle
c        NSPEC - real    - number of species
c         LDB1 - logical - Control variable for printing of debug
c                          information
c          IO6 - integer - Fortran unit number of printed output
c
c
c --- OUTPUT:
c  QIN(mxspec) - real    - Pollutant mass (g) in the puff
c                            QIN(1)  = SO2
c                            QIN(2)  = SO4
c                            QIN(3)  = NO
c                            QIN(4)  = NO2
c                            QIN(5)  = TNO3 (HNO3 + NO3)
c                            QIN(6)  = NH4NO3
c                            QIN(7)  = Primary OC (POC)
c                            QIN(8)  = TOL (Toluene)
c                            QIN(9)  = TOLAER1 (Condensable product)
c                            QIN(10) = TOLAER2 (Condensable product)
c                            QIN(11) = ATOLA1 (SOA 1 from TOL)
c                            QIN(12) = ATOLA2 (SOA 2 from TOL)
c                            QIN(13) = XYL (Xylene)
c                            QIN(14) = XYLAER1 (Condensable product)
c                            QIN(15) = XYLAER2 (Condensable product)
c                            QIN(16) = AXYLA1 (SOA 1 from XYL)
c                            QIN(17) = AXYLA2 (SOA 2 from XYL)
c                            QIN(18) = ALKH (Higher alkanes)
c                            QIN(19) = ALKHAER (Condensable product)
c                            QIN(20) = ALKHA (SOA 1 from ALKH)
c                            QIN(21) = PAH
c                            QIN(22) = PAHAER1 (Condensable product)
c                            QIN(23) = PAHAER2 (Condensable product)
c                            QIN(24) = APAHA1 (SOA 1 from PAH)
c                            QIN(25) = APAHA2 (SOA 2 from PAH)
c      RATE(8) - real    - Transformation rates (percent/hour)
c                            R(1) -- SO2 loss rate
c                            R(2) -- NOX loss rate
c                            R(3) -- TNO3 formation rate
c                            R(4) -- NO  loss rate
c                            R(5) -- TOL loss rate
c                            R(6) -- XYL loss rate
c                            R(7) -- ALKH loss rate
c                            R(8) -- PAH loss rate
c     SCAV(6) - real    -  Scavenging coefficients (1/s)
c                            SCAV(1) -- SO2
c                            SCAV(2) -- SO4
c                            SCAV(3) -- NO
c                            SCAV(4) -- NO2
c                            SCAV(5) -- HNO3
c                            SCAV(6) -- NO3
c
c --- CHEMRIV7 called by: CHEM
c --- CHEMRIV7 calls:     PHOT, CHMRIV7, ISODRIVER, SOADRIVER, AQRADM
c----------------------------------------------------------------------
c
      implicit none

! --- Arguments
      integer nspec, maqchem, io6
      real delt, coz, ctnh3, ch2o2, temp, rhum, rhoair, pivol, zlen
      real zpuf, cloud, prate, zcoef, rshet
      real qin(nspec), scav(nspec)
      real rate(8)
      real cldamr

c --- 6.42_x1.1
      real cldt,cldp,fzcld,fcloud2
      real rh_isrp,so4_isrp
      real cldta,cldpa
      real confcta,rhoaira
      real test

      logical ldb1

! --- Locals
      real ppb(25),ppbi(25),q(25)
      real rmwt(25)
      real lwc    ! Liquid water content, g/m3
      real cozm, ch2o2m, ctnh3m  ! Concentrations in mols/mols air units
      real tno3floor
c --- 6.42_x1.1
      real tso4min
c --- Local controls
      logical l_kgpcm, l_tno3floor, l_raincloud

c --- Concentration array (moles/mole of air) for AQRADM
      real con(6)

c --- Note: TNO3 is weighted as NO3
      data rmwt/64.,96.,30.,46.,62.,62.,180.,
     &          5*92.,5*106.,3*226.,5*156./

      real dt, vkgmol, confct, rk1, zcoefb, o3ppm
      real presur, f, ppbix,  ppbi4, ppbx
      real delno, tso4, tno3, tnh3, rhfrac, pno3
      real patm, taucld, rhoairm
      integer is, iss, j, ii, i
c
      real pcloud, fcloud

c ----------------------
c --- Set local controls
c ----------------------
c --- These enable 2 changes to the code to be reverted to their
c --- original condition.
c --- 1.  Selecting l_kgpcm=.TRUE. converts the LWC passed to AQRADM
c ---     from g/m3 to kg/m3, since AQRADM assumes it to be kg/m3.
c ---     Selecting l_kgpcm=.FALSE. retains the original code and
c ---     passes LWC to AQRADM in g/m3.
      data l_kgpcm/.TRUE./
c --- 2.  Selecting l_tno3floor=.TRUE. uses the cut-off TNO3
c ---     concentration as a floor, and computes NO3 corresponding
c ---     to this floor for all TNO3 less than this floor.  The
c ---     resulting ratio of NO3/TNO3(floor) is then multiplied by
c ---     the actual TNO3 to obtain the final NO3 concentration.
c ---     Selecting l_tno3floor=.FALSE. retains the original code
c ---     and NO3=0.0 for all TNO3<TNO3(floor).
      data l_tno3floor/.TRUE./
c --- 3.  Selecting l_raincloud=.TRUE. sets a minimum cloud cover
c ---     of 10% whenever liquid precipitation is non-zero.  This
c ---     only applies to the aqueous phase option and forces wet
c ---     removal whenever there is rain.  When cloud cover is zero,
c ---     aqueous conversion and wet removal are also zero.
c ---     (This also changes the effective scavenging coefficients
c ---     by limiting their action in any timestep to just the
c ---     cloud fraction affecting the puff mass.)--NA
      data l_raincloud/.TRUE./
c ----------------------

c --- Transfer mass data to local array
      iss = MIN(nspec,25)
      do is = 1, iss
         q(is) = qin(is)
      end do
      do is = (iss+1), 25
         q(is) = 0.0
      end do

c --- Compute initial concentrations as PPB
c --- kg-Molar volume (m^3) at ambient T,P computed from air density
      vkgmol = 28.97/rhoair
c --- Conversion factor to ppm in RIVAD
      confct = (1.0e-3)*vkgmol*pivol
      do is = 1, iss
         ppbi(is) = (1.0e09)*confct*q(is)/rmwt(is)
      end do

c --- Get NO2 photolysis rate
      call PHOT(zpuf,cloud,zcoef,rk1,zcoefb)

c --- Do conversions needed for RIVAD input arguments
c --- RIVAD weights TNO3 as HNO3: scale mass by 63/62=1.016129
      q(5) = q(5)*1.016129
c --- ozone in PPM rather than PPB
      o3ppm = 1.0e-03*coz
c --- Implied pressure (mb) -- use T0=273. P0=1013. as in RIVAD
      patm = (22.4/vkgmol)*(temp/273.)
      presur = 1013.*patm

      if(ldb1) then
         write(io6,*)'CHEMRIV7:'
         write(io6,*)'    rk1,rhoair,O3ppb = ',rk1,rhoair,coz
         write(io6,*)'     TNH3ppb,H2O2ppb = ',ctnh3, ch2o2
         write(io6,*)'    presur,temp,rhum = ',presur,temp,rhum
         write(io6,*)'  cloud,prate,cldamr = ',cloud,prate,cldamr
         write(io6,*)'     zlen,zmsl,zcoef = ',zlen,zpuf,zcoef
         write(io6,*)' vkgmol,confct,pivol = ',vkgmol,confct,pivol

c --- 6.42_x1.1
         write(io6,*)'     fzcld,cldt,cldp = ',fzcld,cldt,cldp
         write(io6,*)'    rh_isrp,so4_isrp = ',rh_isrp,so4_isrp

         write(io6,*)
c         write(io6,101)
101      format('ppb =  SO2',7x,'SO4',7x,'NO',7x,'NO2',7x,'TNO3',
     &              6x,'NO3')
         write(io6,*)'Starting Concs -----'
         write(io6,101)
         write(io6,'(3x,6e10.2)') (ppbi(j),j=1,6)
      endif

c --- Call RIVAD module
      call CHMRIV7(q,temp,presur,rhum,o3ppm,rk1,zcoef,delt,confct,
     &             rshet)
c --- ozone in PPB
      coz = 1.0e03*o3ppm

c --- Re-weight TNO3 as NO3
      q(5) = q(5)*0.984127

      if(ldb1) then
         write(io6,*)'After CHMRIV6, Puff Mass (g) -----'
         write(io6,'(3x,6f10.2)') (q(j),j=1,6)
      endif

      if (maqchem.EQ.1) then
c
c --- initialize scavenging coefficients
         do ii = 1, 6
            scav(ii) = 0.
         end do

c --- Use local cloud water mixing ratio if valid, or
c --- Assign liquid water content if there is cloud cover and the
c --- temperature is above freezing
         fcloud = cloud * 0.1    ! fractional cloud cover
         pcloud = 0.0
         if(l_raincloud .AND. temp.GT.273.15) then
c ---       Force at least 10% cloud cover when there is rain
            if(prate.GT.0.0) fcloud=MAX(fcloud,0.1)
         endif

         if(fcloud.GT.0.0) pcloud = prate/fcloud

c --- 6.42_x1.1
         rhoaira=rhoair
         confcta=confct
         cldta=temp
         cldpa=patm
c ---    Fraction of puff within cloud horizontally and vertically
         fcloud2=fcloud
         if(cldamr.GE.0.0) then
c ---       Valid cloud water provided (use it)
c ---       Use average cloud temperature and pressure
            cldta=cldt
            cldpa=cldp
            rhoaira=rhoair*(cldpa/patm)*(temp/cldta)
            confcta=(1.0e-3)*(28.97/rhoaira)*pivol
c ---       Convert: g/kg(air) * rhoair(kg/m3) ==> g/m3(air)
            lwc=cldamr*rhoaira
c ---       Set minimum cloud fraction to 10%
            fcloud=MAX(fcloud,0.1)
            pcloud=prate/fcloud
c ---       Fraction of puff within cloud horizontally and vertically
            fcloud2=fcloud*fzcld

         elseif (fcloud.gt.0. .and. temp.gt.273.15) then
            if (prate .gt. 0.) then
               lwc = 0.5
            else
               lwc = 0.1
            end if
         else
            lwc = 0.
         end if

         if(l_kgpcm) then
c ---       Units for lwc in AQRADM are kg/m3
            lwc=0.001*lwc
         endif

         if (lwc .gt. 0.) then
c
c --- Calculate concs in moles/mole air units
            do ii = 1, 6

c --- 6.42_x1.1
               con(ii) = MAX(confcta*q(ii)/rmwt(ii),0.)

            end do
c
c --- Get HNO3 conc from total nitrate and PM nitrate
            con(5) = MAX(con(5) - con(6),0.)
c
            cozm = coz * 1.E-9
            ch2o2m = ch2o2 * 1.E-9
            ctnh3m = ctnh3 * 1.E-9
c
c --- Call aqueous-phase chemistry module
            taucld = delt * 3600.   ! timestep in seconds

c --- 6.42_x1.1
            rhoairm = rhoaira * 1.E3/28.97   ! air density in moles/m3
            if(ldb1) then
               write(io6,*)'AQRADM:cldta,cldpa,pcloud,lwc,cozm,'//
     &                     'ch2o2m,ctnh3m,con6= '
               write(io6,*)'called:   ',cldta,cldpa,pcloud,lwc,cozm,
     &                      ch2o2m,ctnh3m,con
            endif
            call AQRADM(cldta,cldpa,taucld,pcloud,lwc,cozm,ch2o2m,
     &                  ctnh3m,con,rhoairm,zlen,scav)
            if(ldb1) then
               write(io6,*)'returned: ',cldta,cldpa,pcloud,lwc,cozm,
     &                      ch2o2m,ctnh3m,con
               write(io6,*)'    scav: ',scav
            endif

c --- Assign adjusted SO2 and SO4 concs; adjust for cloud cover
            q(1) = q(1)*(1. - fcloud2) + fcloud2*con(1)*rmwt(1)/confcta
            q(2) = q(2)*(1. - fcloud2) + fcloud2*con(2)*rmwt(2)/confcta
c
c --- Adjusted oxidant concs
            coz = coz*(1. - fcloud2) + fcloud2*cozm*1.E9
            ch2o2 = ch2o2*(1. - fcloud2) + fcloud2*ch2o2m*1.E9

c
c --- Adjusted scavenging rates
            if (prate .gt. 0.) then
c ---          Possible alternate method (NA)
c ---           if(l_raincloud .AND. fcloud.LT.0.99) then
c ---              do ii = 1, 6
c ---                 scav(ii)=-ALOG(1.0 - fcloud*
c --- &                        (1.0 - EXP(-scav(ii)*taucld) ))/taucld
c ---              end do
c ---           else
                  do ii = 1, 6
                     scav(ii) = scav(ii) * fcloud
                  end do
c ---           endif
            end if
         end if 
      end if 

c --- Condition mass results
      do is = 1,iss
         q(is) = MAX(q(is),0.0)
      end do

c --- Compute ending concentrations as PPB
      do is = 1,iss
         ppb(is) = (1.0e09)*confct*q(is)/rmwt(is)
      end do

c --- Compute conversion rates (%/hr) for QA review
      do i = 1,8
         rate(i) = 0.0
      end do

c --- Compute these logs only if debug output is ON
      if(ldb1) then
         dt = .01*delt
c ---    SOX:
         if(ppbi(1).GT.0.0 .AND. ppb(1).GT.0.0) rate(1) = -ALOG(ppb(1)/
     &                                                   ppbi(1))/dt
c ---    NOX:
         ppbix = ppbi(4) + ppbi(3)
         ppbx = ppb(4) + ppb(3)
         if(ppbix.GT.0.0 .AND. ppbx.GT.0.0) rate(2) = -ALOG(ppbx/
     &                                                   ppbix)/dt
c ---    TNO3:  Conversion of NO2 after NO:NO2 process
         ppbi4 = ppbi(4) + ppbi(3) - ppb(3)
         if(ppbi4.GT.0.0 .AND. ppb(4).GT.0.0) rate(3) = -ALOG(ppb(4)/
     &                                                   ppbi4)/dt
c ---    NO:
         delno = ppb(3) - ppbi(3)
         if(delno.LT.0.0) then
            if(ppbi(3).GT.0.0 .AND. ppb(3).GT.0.0) rate(4) = 
     &                          ALOG(ppb(3)/ppbi(3))/dt
         elseif(delno.GT.0.0) then
            if(ppbi4.GT.0.0 .AND. ppb(4).GT.0.0) rate(4) = -ALOG(ppbi4/
     &                                                    ppbi(4))/dt
c ---    TOL:
         if(ppbi(8).GT.0.0 .AND. ppb(8).GT.0.0) rate(5) = -ALOG(ppb(8)/
     &                                                    ppbi(8))/dt
c ---    XYL:
         if(ppbi(13).GT.0.0 .AND. ppb(13).GT.0.0) rate(6) =
     &                                  -ALOG(ppb(13)/ppbi(13))/dt
c ---    ALKH:
         if(ppbi(18).GT.0.0 .AND. ppb(18).GT.0.0) rate(7) =
     &                                  -ALOG(ppb(18)/ppbi(18))/dt
c ---    PAH:
         if(ppbi(21).GT.0.0 .AND. ppb(21).GT.0.0) rate(8) =
     &                                  -ALOG(ppb(21)/ppbi(21))/dt
         endif
      endif

c --- Inorganic aerosol equilibrium with ISORROPIA
      if(q(5).gt.0.0)then
c
c --- concs in mols/m3

c --- 6.42_x1.1
c ---   Apply so4(g/m3) concentration constraint from control file
        tso4=q(2)*pivol
        tso4=MAX(tso4,so4_isrp)/rmwt(2)

        tso4 = MAX(tso4,1.E-12)
        tno3 = q(5) * pivol / rmwt(6)
        tnh3 = ctnh3 * rhoair / 28.97E6

c --- 6.42_x1.1
c ---   Apply RH(%) constraint from control file to compute RH fraction
        rhfrac=0.01*MAX(rhum,rh_isrp)

        if(tno3.gt.1.E-10)then
          if(ldb1) then

c --- 6.42_x1.1
             write(io6,*)'ISODRIVER:tso4,tno3,tnh3,rhfrac,temp     = '
             write(io6,*)'called:   ',tso4,tno3,tnh3,rhfrac,temp

          endif
          call isodriver(tso4,tno3,tnh3,rhfrac,temp,pno3)
          if(ldb1) then
             write(io6,*)'returned: ',tso4,tno3,tnh3,rhfrac,temp,pno3
          endif

        elseif(l_tno3floor) then
          tno3floor=1.E-10
          if(ldb1) then
             write(io6,*)
     &             'ISODRIVER:tso4,tno3floor,tnh3,rhfrac,temp,pno3= '

c --- 6.42_x1.1
             write(io6,*)'called:   ',
     &                        tso4,tno3floor,tnh3,rhfrac,temp

          endif
          call isodriver(tso4,tno3floor,tnh3,rhfrac,temp,pno3)
          if(ldb1) then
             write(io6,*)'returned: ',
     &                        tso4,tno3floor,tnh3,rhfrac,temp,pno3
          endif
          pno3=pno3*(tno3/tno3floor)

        else
          pno3 = 0.
        endif
        q(6) = pno3 * rmwt(6) / pivol

c --- 6.42_x1.1
c ---   Condition NO3 (q(6)) to be no smaller than zero
        if(q(6).LT.0.0) then
           write(io6,*)'CHEMRIV6 Warning: reset NO3(g) from ',
     &                 q(6),' to ZERO'
           q(6)=0.0
        endif
c ---   Condition NO3 to be no larger than TNO3 (q(5))
c ---   (q(5)>0.0 in this block)
        test=q(6)/q(5)-1.0
        if(test.GT.1.0e-06) then
           write(io6,*)'CHEMRIV6 Warning: reset NO3(g) from ',
     &                 q(6),' to ',q(5)
        endif
        q(6)=MIN(q(5),q(6))

      endif
c
c --- SOA equilibrium
c --- 6.42_x1.1
      call soadriver(io6,q,temp,presur,pivol)
c
c --- Transfer revised mass data to original array
      do is = 1,iss
         qin(is) = q(is)
      end do

      return
      end

c----------------------------------------------------------------------
      subroutine chmriv6(PM,TEMPER,PRESUR,RH,O3PUFF,RK1,ZCOEF,
     &                   TSTP,CONFCT,RSHET)
c----------------------------------------------------------------------
c
c --- CALPUFF    Version: TNG-7.1.0    Level: 100917           CHMRIV6
c                P. Karamchandani, AER (Adapted from chemriv)
c
c --- ADOPTED FROM ARM3 (see banner below) 
c                Sulfate & nitrate conversion uses exponential fcn
c                Unused variables are deactivated
c
c --- UPDATE
c --- V5.8-V6.302  100917 (DGS): Restructure value**-n to value**(-n)
c                                to satisfy LF95 compiler
c                  100917 (DGS): Add heterogeneous reaction rate for
c                                SO4 and pass RSHET(fraction/hr)
c --- V5.8         071025  (PK): Original
c
c----------------------------------------------------------------------
      IMPLICIT NONE
      SAVE
C
CDECK.CHMRIV
C
C  DATE: NOVEMBER 1987
C  VERSION: ARM3-1.0
C
C     CALCULATE TRANSFORMATION OF SO2 TO SO4 AND NO2 TO HNO3-NO3
C     FOR ONE TIME STEP, TSTP
C     BASED ON RIVAD CHEMICAL MECHANISM
C     ADPATED FROM THE RIVAD MODEL
C
C
C  INPUT ARGUMENTS:
C                   PM     R  MASS OF SPECIES (I) IN PUFF (G)
C                             SO2, SO4, NO, NO2, HNO3-NO3, TSP
C                   TEMPER R  TEMPERATURE AT PLUME HEIGHT (K)
C                   PRESUR R  PRESSURE AT PLUME HEIGHT (MB)
C                   RH     R  RELATIVE HUMIDITY AT PLUME (%)
C                   O3PUFF R  PUFF OZONE CONCENTRATION (PPM)
C                   RK1    R  NO2 PHOTOLYSIS RATE CONSTANT (PPM/MIN)
C                   ZCOEF  R  COSINE OF SOLAR ZENITH ANGLE
C                   TSTP   R  TIME STEP (HOURS)
C                   CONFCT R  CONVERSION FACTOR FOR UG/M3 TO PPM-MWT
c                   RSHET  R  SO2 heterogeneous loss rate (fraction/hr)
C
C  OUTPUT ARGUMENTS:
C                   PM     R  NEW MASS OF PUFF DUE TO CHANGES BASED ON
C
C
C  SUBROUTINES CALLED:
C
C  CALLED BY:   CHEMRIV6
C

! --- Constants
      REAL COEF1                  ! Molec/cc to ppm conv factor coefficient
      PARAMETER ( COEF1 = 7.33981E+15 )

      REAL CONSTC                 ! Constant for falloff type reaction
      PARAMETER ( CONSTC = 0.6 )

      REAL TI300                  ! 1.0 / 300.
      PARAMETER ( TI300 = 1.0 / 300.0 )

! --- Arguments
      REAL PM(6)
      REAL TEMPER, PRESUR, RH, O3PUFF, RK1, ZCOEF, TSTP, CONFCT, RSHET

! --- Locals
      REAL TFACT, R26, ROHM, QJ, PHIKK, RCONST
      INTEGER L
      REAL CNO, CNO2, CNOX, TAMB, PAMB, RH1, H2O, SUM
      REAL XNO, XNOX, XO3, XOX, XNO2, RNO2X, XNO3, XN2O5
      REAL RSULF, RNITR, RKX, RNO3, RNITRN, RNITRD, SULFN
      REAL XOHMAX, XOH
      REAL A0, A, B, C

      REAL RK0               ! K0 in falloff rate expressions
      REAL RKINF             ! KINF in falloff rate expressions
      REAL XEND              ! Exponent in falloff rate expressions
C
      REAL KSO2OH            ! SO2 + OH rate constant
      REAL KNO2OH            ! NO2 + OH rate constant
C
      REAL*8 TINV, CFACT, RFACT
C
      REAL PPM(5),SMWT(5)
      LOGICAL LFIRST
      DATA LFIRST/.TRUE./
      DATA SMWT/64.0,96.0,30.0,46.0,63.0/
C
C     FIRST TIME THROUGH SET UP SOME GLOBAL RATE CONSTANTS
C
      IF (LFIRST) THEN
         TFACT = 1./273.
         R26 = 1./26.4
         ROHM = 4.87E-7/1.32E-3
         LFIRST = .FALSE.
      END IF
C
      DO L = 1,5
         PPM(L) = 1.E6*CONFCT*PM(L)/SMWT(L)
      END  DO
C
      CNO = PPM(3)
      CNO2 = PPM(4)
      CNOX = CNO + CNO2
C
      TAMB = TEMPER
      PAMB = PRESUR/1013.0
      TINV = 1. / TAMB
      CFACT = COEF1 * PAMB * TINV
C
      IF (RK1.LE.0.0) THEN
         QJ = 0.0
         PHIKK = 0.0
      ELSE
         PHIKK = RK1*R26
         QJ = (1.338E-3)*ZCOEF**2.74
      ENDIF
      IF (TAMB.GE.273.) THEN
         RCONST = 18.02*(597.3-.566*(TAMB-273.))/1.9869
      ELSE
         RCONST = 6133.17
      END IF
      RH1 = MIN(RH,95.)
      RH1 = MAX(RH1,0.)
      H2O = (6030.*.01*RH1/PAMB)*EXP(RCONST*(TFACT-TINV))
C
C  DO SIMPLE CHEMISTRY
C
      XNOX = CNOX
      XOX = O3PUFF + CNO2
      SUM = XOX + XNOX + PHIKK
      XNO2 = 0.5*(SUM-SQRT(ABS(SUM*SUM-4.*XNOX*XOX)))
      XNO2 = MIN(XNO2,XNOX)
      XNO2 = MAX(XNO2,0.)
c dgs      XNO=XNOX-XNO2
c dgs      IF (XNO.LT.0.0) XNO=0.0
      RNO2X = 0.
      IF (XNOX.GT.0.) RNO2X = XNO2/XNOX

c dgs Use NO2/NOX ratio to test for zero NO (single precision)
      xno = xnox - xno2
      if(rno2x.GE.0.999999) XNO=0.0

      XO3 = XOX - XNO2
      XO3 = MAX(XO3,0.)

      IF (ZCOEF.LT.0.06975) THEN
C
C  NIGHTTIME CHEMISTRY
C
         RSULF = 0.
C
C  IMPLEMENT NEW NIGHTTIME CHEMISTRY - 1/86
C
         RNITR = 0.0
         XO3 = MAX(XO3,0.)
         IF (XO3.GT.0.) THEN
            RKX = 1780./(1.9E-6*H2O + 3.12)
            RNO3 = 0.086
            A0 = 0.59 + 1780. -3.12*RKX
            A = 2.*RKX*RNO3-A0
            B = RNO3 + 0.0474*XO3 +XNO2*A0
            C = -0.0474*XNO2*XO3
C
            XNO3 = -2.*C/(B+SQRT(B*B-4.*A*C))
            XN2O5 = XNO3*XNO2*RKX
            RNITRN = 2.*1.9E-6*H2O*XN2O5*60.*TSTP
            RNITRN = MIN(XNO2,RNITRN)
            PPM(5) = PPM(5) + RNITRN
            XNO2 = XNO2 - RNITRN
         END IF
      ELSE
C
C  DO DAYTIME CHEMISTRY
C
         XOHMAX = ROHM*QJ
         XOH = XOHMAX
         IF (PPM(1).NE.0..OR.XNO2.NE.0.)
     1   XOH = 2.*QJ*3.4E5*H2O*XO3/((4.45E10+3.4E5*H2O)*
     1       (2000.*PPM(1) + 14000.*XNO2))
         XOH = MIN(XOH,XOHMAX)

C --- SO2 + OH rate constant (falloff expression)
         RK0 = 1.0E+06 * CFACT * 3.0E-31 * ( TAMB * TI300 )**(-3.3)
         RKINF = 1.5E-12
         XEND = 1.0 / ( 1.0 + ( LOG10( RK0 / RKINF ) )**2 )
         KSO2OH = ( RK0 / ( 1.0 + RK0 / RKINF ) ) * CONSTC**XEND

C --- NO2 + OH rate constant (falloff expression)
         RK0 = 1.0E+06 * CFACT * 2.6E-30 * ( TAMB * TI300 )**(-3.2)
         RKINF = 2.4E-11 * ( TAMB * TI300 )**(-1.3)
         XEND = 1.0 / ( 1.0 + ( LOG10( RK0 / RKINF ) )**2 )
         KNO2OH = ( RK0 / ( 1.0 + RK0 / RKINF ) ) * CONSTC**XEND
C
C --- Convert rate constants from molec-cc-1 s-1 to ppm-1 hr-1 units
         rfact = 2.64e19 * (pamb * tinv)
         kso2oh = kso2oh * rfact
         kno2oh = kno2oh * rfact
         rsulf = 1. - EXP(-kso2oh*xoh*tstp)
         rnitr = 1. - EXP(-kno2oh*xoh*tstp)
         RNITRD = RNITR*XNO2
         RNITRD = MIN(XNOX,RNITRD)
         PPM(5) = PPM(5)+RNITRD
         XNOX = XNOX-RNITRD
         XNOX =  MAX(XNOX,0.)
         XNO = XNOX*(1.-RNO2X)
         XNO2 = XNOX*RNO2X

      END IF
C
c --- Add heterogeneous rate wso4=rshet*ppm(1)*tstp
      SULFN = PPM(1)*RSULF + rshet*ppm(1)*tstp
      SULFN = MIN(PPM(1),SULFN)
      PPM(2) = PPM(2) + SULFN
      PPM(1) = PPM(1) - SULFN
C
C     UPDATE NEW STEADY-STATE NO AND NO2 VALUES
C
      PPM(3) = XNO
      PPM(4) = XNO2
C
C     UPDATE NEW O3 VALUE
      O3PUFF = XO3
C
C     CONVERT BACK TO GRAMS OF SPECIES IN PUFF
C
      DO L = 1,5
         PM(L) = 1.E-6*PPM(L)*SMWT(L)/CONFCT
      END DO
      RETURN
      END

c----------------------------------------------------------------------
      subroutine chmriv7(PM,TEMPER,PRESUR,RH,O3PUFF,RK1,ZCOEF,
     &                   TSTP,CONFCT,RSHET)
c----------------------------------------------------------------------
c
c --- CALPUFF    Version: TNG-7.1.0    Level: 100917           CHMRIV7
c                P. Karamchandani, AER (Adapted from chemriv)
c
c --- ADOPTED FROM ARM3 (see banner below) 
c                Sulfate & nitrate conversion uses exponential fcn
c                Unused variables are deactivated
c
c --- UPDATE
c --- V5.8-V6.302  100917 (DGS): Restructure value**-n to value**(-n)
c                                to satisfy LF95 compiler
c                         (DGS): Add heterogeneous reaction rate for
c                                SO4 and pass RSHET(fraction/hr)
c --- V5.8         071025  (PK): Original
c
c----------------------------------------------------------------------
      IMPLICIT NONE
      SAVE
C
CDECK.CHMRIV
C
C  DATE: NOVEMBER 1987
C  VERSION: ARM3-1.0
C
C     CALCULATE TRANSFORMATION OF SO2 TO SO4 AND NO2 TO HNO3-NO3
C     FOR ONE TIME STEP, TSTP
C     BASED ON RIVAD CHEMICAL MECHANISM
C     ADPATED FROM THE RIVAD MODEL
C
C
C  INPUT ARGUMENTS:
C                   PM     R  MASS OF SPECIES (I) IN PUFF (G)
C                             SO2, SO4, NO, NO2, HNO3-NO3, TSP
C                   TEMPER R  TEMPERATURE AT PLUME HEIGHT (K)
C                   PRESUR R  PRESSURE AT PLUME HEIGHT (MB)
C                   RH     R  RELATIVE HUMIDITY AT PLUME (%)
C                   O3PUFF R  PUFF OZONE CONCENTRATION (PPM)
C                   RK1    R  NO2 PHOTOLYSIS RATE CONSTANT (PPM/MIN)
C                   ZCOEF  R  COSINE OF SOLAR ZENITH ANGLE
C                   TSTP   R  TIME STEP (HOURS)
C                   CONFCT R  CONVERSION FACTOR FOR UG/M3 TO PPM-MWT
c                   RSHET  R  SO2 heterogeneous loss rate (fraction/hr)
C
C  OUTPUT ARGUMENTS:
C                   PM     R  NEW MASS OF PUFF DUE TO CHANGES BASED ON
C
C
C  SUBROUTINES CALLED:
C
C  CALLED BY:   CHEMRIV7
C

! --- Constants
      REAL COEF1                  ! Molec/cc to ppm conv factor coefficient
      PARAMETER ( COEF1 = 7.33981E+15 )

      REAL CONSTC                 ! Constant for falloff type reaction
      PARAMETER ( CONSTC = 0.6 )

      REAL TI300                  ! 1.0 / 300.
      PARAMETER ( TI300 = 1.0 / 300.0 )

! --- Arguments
      REAL PM(25)
      REAL TEMPER, PRESUR, RH, O3PUFF, RK1, ZCOEF, TSTP, CONFCT, RSHET

! --- Locals
      REAL TFACT, R26, ROHM, QJ, PHIKK, RCONST
      INTEGER L
      REAL CNO, CNO2, CNOX, TAMB, PAMB, RH1, H2O, SUM
      REAL XNO, XNOX, XO3, XOX, XNO2, RNO2X, XNO3, XN2O5
      REAL RSULF, RNITR, RKX, RNO3, RNITRN, RNITRD, SULFN
      REAL XOHMAX, XOH
      REAL A0, A, B, C

      REAL RK0               ! K0 in falloff rate expressions
      REAL RKINF             ! KINF in falloff rate expressions
      REAL XEND              ! Exponent in falloff rate expressions
C
      REAL KSO2OH            ! SO2 + OH rate constant
      REAL KNO2OH            ! NO2 + OH rate constant
      REAL KTOLOH            ! TOL + OH rate constant
      REAL KXYLOH            ! XYL + OH rate constant
      REAL KALKHOH           ! ALKH + OH rate constant
      REAL KPAHOH            ! PAH + OH rate constant
C
      REAL DTOL, DXYL, DALKH, DPAH
C
      REAL*8 TINV, CFACT, RFACT
C
      REAL PPM(25),SMWT(25)
      LOGICAL LFIRST
      DATA LFIRST/.TRUE./
      DATA SMWT/64.0,96.0,30.0,46.0,63.0,62.0,180.0,
     &          5*92.0,5*106.0,3*226.0,5*156.0/

C
C     FIRST TIME THROUGH SET UP SOME GLOBAL RATE CONSTANTS
C
      IF (LFIRST) THEN
         TFACT = 1./273.
         R26 = 1./26.4
         ROHM = 4.87E-7/1.32E-3
         LFIRST = .FALSE.
      END IF
C
      DO L = 1,5
         PPM(L) = 1.E6*CONFCT*PM(L)/SMWT(L)
      END  DO
C
      DO L = 8,25
         PPM(L) = 1.E6*CONFCT*PM(L)/SMWT(L)
      END  DO
C
      CNO = PPM(3)
      CNO2 = PPM(4)
      CNOX = CNO + CNO2
C
      TAMB = TEMPER
      PAMB = PRESUR/1013.0
      TINV = 1. / TAMB
      CFACT = COEF1 * PAMB * TINV
C
      IF (RK1.LE.0.0) THEN
         QJ = 0.0
         PHIKK = 0.0
      ELSE
         PHIKK = RK1*R26
         QJ = (1.338E-3)*ZCOEF**2.74
      ENDIF
      IF (TAMB.GE.273.) THEN
         RCONST = 18.02*(597.3-.566*(TAMB-273.))/1.9869
      ELSE
         RCONST = 6133.17
      END IF
      RH1 = MIN(RH,95.)
      RH1 = MAX(RH1,0.)
      H2O = (6030.*.01*RH1/PAMB)*EXP(RCONST*(TFACT-TINV))
C
C  DO SIMPLE CHEMISTRY
C
      XNOX = CNOX
      XOX = O3PUFF + CNO2
      SUM = XOX + XNOX + PHIKK
      XNO2 = 0.5*(SUM-SQRT(ABS(SUM*SUM-4.*XNOX*XOX)))
      XNO2 = MIN(XNO2,XNOX)
      XNO2 = MAX(XNO2,0.)
c dgs      XNO=XNOX-XNO2
c dgs      IF (XNO.LT.0.0) XNO=0.0
      RNO2X = 0.
      IF (XNOX.GT.0.) RNO2X = XNO2/XNOX

c dgs Use NO2/NOX ratio to test for zero NO (single precision)
      xno = xnox - xno2
      if(rno2x.GE.0.999999) XNO = 0.0

      XO3 = XOX - XNO2
      XO3 = MAX(XO3,0.)
      IF (ZCOEF.LT.0.06975) THEN
C
C  NIGHTTIME CHEMISTRY
C
         RSULF = 0.
C
C  IMPLEMENT NEW NIGHTTIME CHEMISTRY - 1/86
C
         RNITR = 0.0
         XO3 = MAX(XO3,0.)
         IF (XO3.GT.0.) THEN
            RKX = 1780./(1.9E-6*H2O + 3.12)
            RNO3 = 0.086
            A0 = 0.59 + 1780. -3.12*RKX
            A = 2.*RKX*RNO3-A0
            B = RNO3 + 0.0474*XO3 +XNO2*A0
            C = -0.0474*XNO2*XO3
C
            XNO3 = -2.*C/(B+SQRT(B*B-4.*A*C))
            XN2O5 = XNO3*XNO2*RKX
            RNITRN = 2.*1.9E-6*H2O*XN2O5*60.*TSTP
            RNITRN = MIN(XNO2,RNITRN)
            PPM(5) = PPM(5) + RNITRN
            XNO2 = XNO2 - RNITRN
         END IF
      ELSE
C
C  DO DAYTIME CHEMISTRY
C
         XOHMAX = ROHM*QJ
         XOH = XOHMAX
         IF (PPM(1).NE.0..OR.XNO2.NE.0.)
     1   XOH = 2.*QJ*3.4E5*H2O*XO3/((4.45E10+3.4E5*H2O)*
     1     (2000.*PPM(1) + 14000.*XNO2))
         XOH = MIN(XOH,XOHMAX)

C --- SO2 + OH rate constant (falloff expression)
         RK0 = 1.0E+06 * CFACT * 3.0E-31 * ( TAMB * TI300 )**(-3.3)
         RKINF = 1.5E-12
         XEND = 1.0 / ( 1.0 + ( LOG10( RK0 / RKINF ) )**2 )
         KSO2OH = ( RK0 / ( 1.0 + RK0 / RKINF ) ) * CONSTC**XEND
C
C --- NO2 + OH rate constant (falloff expression)
         RK0 = 1.0E+06 * CFACT * 2.6E-30 * ( TAMB * TI300 )**(-3.2)
         RKINF = 2.4E-11 * ( TAMB * TI300 )**(-1.3)
         XEND = 1.0 / ( 1.0 + ( LOG10( RK0 / RKINF ) )**2 )
         KNO2OH = ( RK0 / ( 1.0 + RK0 / RKINF ) ) * CONSTC**XEND
C
C --- TOL + OH rate constant
         ktoloh = 2.1e-12 * EXP(322.*tinv)
C
C --- XYL + OH rate constant
         kxyloh = 1.7e-11 * EXP(116.*tinv)
C
C --- ALKH + OH rate constant
         kalkhoh = 1.97e-11
C
C --- PAH + OH rate constant
         kpahoh = 7.7e-11
C
C --- Convert rate constants from molec-cc-1 s-1 to ppm-1 hr-1 units
         rfact = 2.64e19 * (pamb * tinv)
         kso2oh = kso2oh * rfact
         kno2oh = kno2oh * rfact
         rsulf = 1. - EXP(-kso2oh*xoh*tstp)
         rnitr = 1. - EXP(-kno2oh*xoh*tstp)
         RNITRD = RNITR*XNO2
         RNITRD = MIN(XNOX,RNITRD)
         PPM(5) = PPM(5) + RNITRD
         XNOX = XNOX - RNITRD
         XNOX = MAX(XNOX,0.)
         XNO = XNOX*(1.-RNO2X)
         XNO2 = XNOX*RNO2X

         ktoloh = ktoloh * rfact
         kxyloh = kxyloh * rfact
         kalkhoh = kalkhoh * rfact
         kpahoh = kpahoh * rfact
C
C --- Change in SOA precursor concentrations
         dtol = (1. - EXP(-ktoloh*xoh*tstp)) * ppm(8)
         dxyl = (1. - EXP(-kxyloh*xoh*tstp)) * ppm(13)
         dalkh = (1. - EXP(-kalkhoh*xoh*tstp)) * ppm(18)
         dpah = (1. - EXP(-kpahoh*xoh*tstp)) * ppm(21)

         ppm(8) = ppm(8) - dtol          ! TOL
         ppm(9) = ppm(9) + 0.071*dtol    ! TOLAER1
         ppm(10) = ppm(10) + 0.138*dtol  ! TOLAER2

         ppm(13) = ppm(13) - dxyl        ! XYL
         ppm(14) = ppm(14) + 0.038*dxyl  ! XYLAER1
         ppm(15) = ppm(15) + 0.167*dxyl  ! XYLAER2

         ppm(18) = ppm(18) - dalkh       ! ALKH
         ppm(19) = ppm(19) + 1.173*dalkh ! ALKHAER

         ppm(21) = ppm(21) - dpah        ! PAH
         ppm(22) = ppm(22) + 0.156*dpah  ! PAHAER1
         ppm(23) = ppm(23) + 0.777*dpah  ! PAHAER2
      END IF

c --- Add heterogeneous rate wso4=rshet*ppm(1)*tstp
      SULFN = PPM(1)*RSULF + rshet*ppm(1)*tstp
      SULFN = MIN(PPM(1),SULFN)
      PPM(2) = PPM(2) + SULFN
      PPM(1) = PPM(1) - SULFN
C
C     UPDATE NEW STEADY-STATE NO AND NO2 VALUES
C
      PPM(3) = XNO
      PPM(4) = XNO2
C
C     UPDATE NEW O3 VALUE
      O3PUFF = XO3
C
C     CONVERT BACK TO GRAMS OF SPECIES IN PUFF
C
      DO L = 1,5
         PM(L) = 1.E-6*PPM(L)*SMWT(L)/CONFCT
      END DO
C
      DO L = 8,25
         PM(L) = 1.E-6*PPM(L)*SMWT(L)/CONFCT
      END DO
C
      RETURN
      END

c---------------------------------------------------------------------
      subroutine setbckoc(ndathr)
c---------------------------------------------------------------------
c
c --- CALPUFF    Version: TNG-7.1.0    Level: 071025          SETBCKOC
c                P. Karamchandani, AER (Adapted from setsoa)
c
c --- PURPOSE:  Background OC concentration (ug/m3) for current hour
c
c --- INPUTS:
c            NDATHR - integer    - YYJJJHH date-time for hour
c
c     Common Block /CHEMDAT/ variables:
c        BCKPMF(12),OFRAC(12)
c
c --- OUTPUT:
c
c     Common Block /NEWSOA/ variables: bckoc
c
c --- SETBCKOC called by:  COMP
c --- SETBCKOC calls:      GRDAY
c----------------------------------------------------------------------
      include 'params.puf'
      include 'chemdat.puf'
      include 'newsoa.puf'

c --- Extract month from date-time
      iyr=ndathr/100000
      ijul=ndathr/100 - 1000*iyr
      call GRDAY(io6,iyr,ijul,imo,iday)

c --- Set background OC data for this month
      bckoc=bckpmf(imo)*ofrac(imo)

      return
      end

c----------------------------------------------------------------------
      subroutine isodriver(so4,no3,nh3,nrh,ntempk,pno3c)
c----------------------------------------------------------------------
c
c --- CALPUFF    Version: TNG-7.1.0    Level: 140521         ISODRIVER
c                P. Karamchandani, AER
c
c --- PURPOSE:  Driver routine to calculate gas/particle equilibrium
c               using ISORROPIA aerosol equilibrium module
c               This version uses double precision
c
c --- UPDATES:
c
c --- V6.4-V6.42_x1.1  140521  : Call ISOROPIA with METASTABLE ON
c
c --- V6.302-V6.4  101025 (DGS): Revise ISOROPIA arguments for V2.1
c
c --- INPUTS:
c            so4    - real - Total sulfate concentration (mole/m3)
c            no3    - real - Total nitrate concentration (mole/m3)
c            nh3    - real - Total ammonia concentration (mole/m3)
c            nrh    - real - Fractional relative humidity
c            ntempk - real - Temperature (K)
c
c --- OUTPUT:
c            pno3c  - real - Particle-phase equilibrium NO3
c                            concentration (mole/m3)
c
c --- ISODRIVER called by:  CHEMRIV6, CHEMRIV7
c --- ISODRIVER calls:      ISOROPIA
c----------------------------------------------------------------------

C***********************************************************************
C                                                                      * 
C  REVISION HISTORY:                                                   *
C     Version 1.0 developed November 2006 by PK, AER, Inc. for         *
C     CALPUFF for API Contract No. 2006-102376                         *
C                                                                      *
C***********************************************************************
C
C...........  INCLUDES

      INCLUDE 'isrpia.inc'

C...........  ARGUMENTS and their descriptions

      REAL SO4             ! Total sulfate (moles/m3)
      REAL NO3             ! Total nitrate (moles/m3)
      REAL NH3             ! Total ammonia (moles/m3)
      REAL NRH             ! Relative humidity as fraction
      REAL NTEMPK          ! Temperature in Kelvin

      REAL PNO3C           ! Particle-phase NO3 concentration (mole/m**3)

! --- Locals
      REAL GNO3C           ! Gas-phase concentration of HNO3 in mole/m**3 air

      INTEGER NCTRL, NOTHER
cV1.7 PARAMETER(NCTRL = 2,NOTHER = 6)
cV2.1 PARAMETER(NCTRL = 2,NOTHER = 9)
      PARAMETER(NCTRL = 2,NOTHER = 9)

! --- Gas-phase concentration array in moles/m**3 air
      REAL*8  GAS(NGASAQ)

! --- Aqueous-phase concentration array in moles/m**3 air
      REAL*8  AERLIQ(NIONS+NGASAQ+2)

! --- Solid-phase concentration array in moles/m**3 air
      REAL*8  AERSLD(NSLDS)

! --- Flag for different types of problems solved and
! --- different state of aerosols (deliquescent or metastable)
      REAL*8  CNTRL(NCTRL)

! Solution information array (see ISOCOM.f for details)
      REAL*8  OTHER(NOTHER)

! Total species concentrations in moles/m**3 air
      REAL*8  WI(NCOMP)

! Temperature and humidity
      REAL*8  TEMPI, RHI
C***********************************************************************
C  begin body of subroutine 
C
C *** Assign input total concentrations to WI

      WI(1) = 0.    ! Na concentration
      WI(2) = so4
      WI(3) = nh3
      WI(4) = no3
      WI(5) = 0.    ! Cl concentration
cV2.1 Additional 3 are not used here
      WI(6) = 0.    ! total calcium concentration
      WI(7) = 0.    ! total potassium concentration
      WI(8) = 0.    ! total magnesium concentration

      RHI   = NRH
      TEMPI = NTEMPK
C
C *** CALL ISORROPIA 
C
      CNTRL(1) = 0.     ! 0 = FORWARD PROBLEM,  1 = REVERSE PROBLEM

c --- 6.42_x1.1
      CNTRL(2) = 1.     ! 0 = SOLID + LIQUID AEROSOL,  1 = METASTABLE
C
      CALL ISOROPIA (WI,  RHI,  TEMPI,   CNTRL,  
     &               W,   GAS,  AERLIQ,  AERSLD,  SCASE,  OTHER)
C
C *** SAVE RESULTS

! Gas-phase HNO3 (moles/m3)
      GNO3C  = GAS(2)

! Particle-phase NO3 (moles/m3)
      PNO3C  = MAX(0.,NO3 - GNO3C)
C
      RETURN
      END


c----------------------------------------------------------------------
      subroutine soadriver(ilst,q,tempk,press,pivol)
c----------------------------------------------------------------------

C***********************************************************************
C  FUNCTION: Driver routine to calculate SOA gas-particle equilibrium  *
C                                                                      * 
c
c --- UPDATES:
c
c --- V6.4-V6.42_x1.1  140521  : Iterate on CALTECH_SOA call to update
c                                absorbing organic mass
c
C  INPUTS:                                                             *
C     ilst   - List-file unit                                          *
C     q      - Mass of species in puff                                 *
C     tempk  - Temperature (K)                                         *
C     press  - Pressure at plume height (mb)                           *
C     pivol  - Reciprocal of puff volume (1/m**3)                      *
C                                                                      * 
C  RETURN VALUES:                                                      *
C     q      - Adjusted mass of species in puff                        *
C                                                                      * 
C  REVISION HISTORY:                                                   *
C     Version 1.0 developed November 2006 by PK, AER, Inc. for         *
C     CALPUFF for API Contract No. 2006-102376                         *
C***********************************************************************
      IMPLICIT NONE
C
C...........  INCLUDES

      INCLUDE 'newsoa.puf'
      INCLUDE 'soadat.puf'

C...........  ARGUMENTS and their descriptions

c --- 6.42_x1.1
      integer ilst         ! List-file unit

      REAL q(25)           ! Puff masses (g)
      REAL tempk           ! Temperature in Kelvin
      REAL press           ! Pressure in mb
      REAL pivol           ! reciprocal of puff volume (1/m3)

! Local variables
      integer iorg ! loop index
! Secondary organic aerosol concentrations (ug/m3)
! --- Gas-phase compounds
      REAL gasorg(NORG)
! --- Particle-phase compounds
      REAL partorg(NORG)
! --- Total (gas-phase + particle-phase)
      REAL worg(NORG+1)  ! additional species at end for primary OC

! --- Conversion factor
      REAL cfact

c --- 6.42_x1.1
! --- OA mass iteration
      integer iter, niter
      real psum1, psum2, pdiff

C***********************************************************************
C  begin body of subroutine 
C
C *** Convert input puff masses (g) to puff concentrations (ug/m3)

      cfact = pivol * 1.E6

! Gases
      gasorg(1) = q(9) * cfact    ! TOLAER1
      gasorg(2) = q(10) * cfact   ! TOLAER2
      gasorg(3) = q(14) * cfact   ! XYLAER1
      gasorg(4) = q(15) * cfact   ! XYLAER2
      gasorg(5) = q(19) * cfact   ! ALKHAER
      gasorg(6) = q(22) * cfact   ! PAHAER1
      gasorg(7) = q(23) * cfact   ! PAHAER2

! Particles
      partorg(1) = q(11) * cfact   ! ATOLA1
      partorg(2) = q(12) * cfact   ! ATOLA2
      partorg(3) = q(16) * cfact   ! AXYLA1
      partorg(4) = q(17) * cfact   ! AXYLA2
      partorg(5) = q(20) * cfact   ! AALKHA
      partorg(6) = q(24) * cfact   ! APAHA1
      partorg(7) = q(25) * cfact   ! APAHA2

! Total
      do iorg = 1, NORG
        worg(iorg) = gasorg(iorg) + partorg(iorg)
      end do

! Organic absorbing mass
      worg( NORG + 1 ) = MAX(q(7)*cfact + bckoc, 0.)

! Calculate organic aerosol formation using Caltech SOA Module

c --- 6.42_x1.1
! --- Iterate on this call until difference in particulate OC total <1%
! --- PDIFF is 1% criterion for difference
! --- NITER is set large but finite to trap endless loop
! --- ITER is current loop counter
      pdiff=0.01
      niter=10000
      iter=0
10    psum1=0.0
      do iorg = 1, NORG
        psum1=psum1+partorg(iorg)
      end do

      call caltech_soa(worg, gasorg, partorg, tempk)

c --- 6.42_x1.1
      iter=iter+1
      psum2=-psum1
      do iorg = 1, NORG
        psum2=psum2+partorg(iorg)
      end do
      if(psum1.GT.0.0) then
         if(iter.LE.niter) then
            if(ABS(psum2/psum1) .GT. pdiff) goto 10
         else
! ---       Report too many iterations warning and stop iterating
            write(ilst,*)'SOADRIVER:  Warning ...'
            write(ilst,*)'  Iterations exceed NITER = ',niter,
     &                   ' when computing particulate SOA '
            write(ilst,*)'  Target fractional SOA change = ',pdiff
            write(ilst,*)'  Last fractional SOA change = ',psum2/psum1
         endif
      elseif(worg(norg+1).GT.0.0 .AND. iter.EQ.1) then
         goto 10
      endif

! --- Assign adjusted concentrations back to puff masses

      cfact = 1./cfact
! Gases
      q(9) = gasorg(1) * cfact    ! TOLAER1
      q(10) = gasorg(2) * cfact   ! TOLAER2
      q(14) = gasorg(3) * cfact   ! XYLAER1
      q(15) = gasorg(4) * cfact   ! XYLAER2
      q(19) = gasorg(5) * cfact   ! ALKHAER
      q(22) = gasorg(6) * cfact   ! PAHAER1
      q(23) = gasorg(7) * cfact   ! PAHAER2

! Particles
      q(11) = partorg(1) * cfact   ! ATOLA1
      q(12) = partorg(2) * cfact   ! ATOLA2
      q(16) = partorg(3) * cfact   ! AXYLA1
      q(17) = partorg(4) * cfact   ! AXYLA2
      q(20) = partorg(5) * cfact   ! AALKHA
      q(24) = partorg(6) * cfact   ! APAHA1
      q(25) = partorg(7) * cfact   ! APAHA2
C
      RETURN
      END

c----------------------------------------------------------------------
      SUBROUTINE CALTECH_SOA (WORG, GASORG, PARTORG, CURTEMP)
c----------------------------------------------------------------------

C***********************************************************************
C  FUNCTION: Program to simulate Secondary Organic Aerosols            *
C     Total no. of species = 4 + 34 = 38                               *
C     Partition equation                                               *
C     Kom, i = (Ai/Mo)/Gi                                              *
C     Ai = particle-phase concentration (ug/m**3 air)                  *
C     Gi = gas-phase concentration (ug/m**3 air)                       *
C     MSUM = sum Ai + primary organics                                 *
C     Kom, i has the units of m3/ug                                    *
C  PRECONDITION REQUIRED: called from subr. AEROEQ                     *
C  RETURN VALUES:                                                      *
C     PARTORG(I) - Particulate concentration of organic species i      *
C     GASORG(I)  - Gas-phase concentration of organic species i        *
C  REVISION HISTORY:                                                   *
C     Written by Betty K. Pun of AER, Inc. for EPRI's Aerosol          *
C         Module Implementation Project, May, 1999                     *
C     Revised by Yang Zhang of AER, Inc. for EPRI's Aerosol            *
C         Module Implementation Project based on Models3's             *
C         coding standard July, 1999                                   *
C     Revised by Betty K. Pun of AER, Inc. November, 99                *
C         for incorporation into 3-D model under EPRI.                 *
C         To reduce computational requirement and to                   *
C         facilitate numerical solution, simultaneous equations        *
C         are not solved in this version.  Instead, the partition of   *
C         each organic aerosol at each time depends on the amount of   *
C         material in the organic phase at the step before the partition*
C     Modified by Betty K. Pun, AER to include temperature dependence  *
C         based on generic Hvap, February/March 2002                   *
c     Revised by BKP March 2006, change reference temperature from     *
C             310 to 298K                                              *
c     Updated by PK December 2006, for implementation in CALPUFF       *
c             for API Contract No. 2006-102376                         *
C  REFERENCES:                                                         *
C    1. Odum et al.,  97.  EST 31:1890                                 *
C    2. Griffin et al.,  99.  JGR 104:3555                             *
C                                                                      *
C***********************************************************************

      IMPLICIT NONE

C...........  INCLUDES
      INCLUDE 'soadat.puf'

C...........  ARGUMENTS and their descriptions and some other variables

      REAL WORG(NORG+1)   ! Total organic species concentration
      REAL PARTORG(NORG)  ! Particulate concentration of organic species i
      REAL GASORG(NORG)   ! Gas-phase concentration of organic species i
      REAL CURTEMP        ! Puff temperature used in modifying parititon
                          ! constants

C..........  Local Variables

      REAL MP             ! Total organic species concentration
      REAL MSUM           ! Sum of particle-phase concentration (ug/m**3 air)  
      INTEGER I           ! Organic species index

C***********************************************************************
C  begin body of subroutine 

      MP = WORG(NORG + 1)              ! OC (primary+background)
      MSUM = MP

      DO I = 1,  NORG
         IF (WORG(I) . LT . 0.0) THEN
            WRITE (*,  95000) I
95000       FORMAT ('ERROR: Negative conc. read for species',  I2)
            WRITE (*,  *) 'Negative Conc. = ',WORG(I)
            call flush(6)
            STOP
         END IF
         MSUM = MSUM + PARTORG(I)      !add starting SOA to MSUM
      END DO
 
      IF (MSUM . GT . 0.0) THEN
         call SOASUB (MSUM, WORG, PARTORG, GASORG, CURTEMP)
      ENDIF

      DO I = 1,  NORG   
         IF (PARTORG(I) . LT . 0.0) THEN
            PARTORG(I) = 0.0
            GASORG(I) = WORG(I)
         END IF
      END DO

      RETURN
      END

c----------------------------------------------------------------------
      SUBROUTINE SOASUB (MSUM, WORG, PARTORG, GASORG, CURTEMP)   
c----------------------------------------------------------------------

C***********************************************************************
C  FUNCTION: Program to calculate the equilibria                       *
C  PRECONDITION REQUIRED: called from subr. CALTECHSOA                 *
C  RETURN VALUES:                                                      *
C     PARTORG(J) - Particule-phase concentrations (microgram/m**3)     *
C     GASORG(J) - Gas-phase concentrations (microgram/m**3)            *
C  REVISION HISTORY:                                                   *
C     Written by Betty K. Pun of AER, Inc. for EPRI's Aerosol          *
C         Module Implementation Project, May, 1999                     *
C     Revised by Yang Zhang of AER, Inc. for EPRI's Aerosol            *
C         Module Implementation Project based on Models3's             *
C         coding standard July, 1999                                   *
C     Modified by Betty K. Pun of AER, Inc, for Models-3 application   *
C         Analytical solution calculated November, 99                  *
C     Modified by Betty K. Pun, AER to include temperature dependence  *
C         based on generic Hvap, February/March 2002                   *
C***********************************************************************

      IMPLICIT NONE

C...........  INCLUDES

      INCLUDE 'soadat.puf'

C...........  ARGUMENTS and their descriptions

      REAL MSUM           ! Sum of particle-phase concentration (ug/m**3 air)
      REAL WORG(NORG+1)   ! Total organic species concentration
      REAL PARTORG(NORG)  ! Particulate concentration of organic species i
      REAL GASORG(NORG)   ! Gas-phase concentration of organic species i
      REAL CURTEMP        ! Grid cell temperature used in modifying parititon
                          ! constants
c...........  Local variables
      INTEGER J           ! Organic species index                          
      REAL KCORR(NORG)    ! Partition coefficients, corrected for temperature
                          ! [m**3/ug]
      REAL TEXPT          ! temperature where KOM (experimental values) 
                          ! are obtained
      PARAMETER (TEXPT = 298.0)

C ..........  Other Variables

C***********************************************************************
C  begin body of subroutine

      IF (MSUM . EQ . 0.0) THEN
         WRITE(*,  *)    'Error in SOA: MSUM = 0.0'
         STOP
      END IF
C
C *** Calculate the equilibrium concentration
C     GASORG(J) = WORG(J) - PARTORG(J)
C     G         = C      -  A        
C     (A/M)/G = K is equal to A/(C-A) = KM is equal to A = CKM - AKM
C     Therefore A = CKM / (1 + KM) 

C add code to correct for CURTEMPerature, assuming KOM is experimental
C values obtained at 310 K (changed to a ref temp of 298K)
      DO J = 1, NORG
         KCORR(J) = EXP(HVAP(J)*(1/CURTEMP - 1/TEXPT)/RKJMOLK)
     &                 * KOM(J) * CURTEMP/TEXPT
         PARTORG(J) = WORG(J) * KCORR(J)* MSUM / 
     &                    (1. + MSUM * KCORR(J)) 
         GASORG(J) = WORG (J) - PARTORG(J) 
      END DO

      RETURN
      END


c----------------------------------------------------------------------
      subroutine aqradm ( temp, pres_atm, taucld, prcrate, wcavg, coz,
     &                    ch2o2, ctnh3, conc, rhoair, len, scav )
c----------------------------------------------------------------------
C
C  DESCRIPTION:
C    Compute concentration changes in cloud due to aqueous chemistry
C    Adapted from RADM Cloud implementation in CMAQ/SCICHEM for CALPUFF
C    by PK, AER, January 2007 for API Contract No. 2006-102376
C
C  Reference: 
C     Walcek & Taylor, 1986, A theoretical Method for computing
C      vertical distributions of acidity and sulfate within cumulus
C      clouds, J. Atmos Sci.,  Vol. 43, no. 4 pp 339 - 355
C
C  Called by:  CHEMRIV6, CHEMRIV7
C
C  Calls the following functions:  HLCONST
C
C  ARGUMENTS     TYPE      I/O       DESCRIPTION
C  ---------     ----  ------------  --------------------------------
C   CONC(6)      real  input&output  Concentration for species i=1,11
C                                    (1) = SO2   conc (mol/mol of SO2)
C                                    (2) = SO4   conc (mol/mol of SO4)
C                                    (3) = NO    conc (mol/mol of NO)
C                                    (4) = NO2   conc (mol/mol of NO2)
C                                    (5) = HNO3  conc (mol/mol of HNO3)
C                                    (6) = NO3   conc (mol/mol of NO3)
C-----------------------------------------------------------------------

      IMPLICIT NONE

C...........PARAMETERS and their descriptions:
!      include 'params.puf'
! temporarily set io6=2 here since params.puf variables are not declared,
! resulting in compile errors.
      INTEGER IO6
      PARAMETER (IO6 = 2) ! from params.puf

! number of oxidizing reactions
      INTEGER NUMOX
      PARAMETER (NUMOX =  3)   ! H2O2, O3, and Fe-Mn catalyzed

! minimum and maximum pH
      REAL PHMIN, PHMAX
      PARAMETER (PHMIN = 0.0001, PHMAX = 10.0)

! minimum concentration
      REAL CONCMIN
      PARAMETER (CONCMIN = 1.0E-30)

! convert seconds to hours
      REAL SEC2HR
      PARAMETER (SEC2HR = 1.0 / 3600.0)
C
! Molar volume at STP [ L/mol ] Non MKS units
      REAL MOLVOL
      PARAMETER (MOLVOL = 22.41410)

! Standard Temperature [ K ]
      REAL STDTEMP
      PARAMETER (STDTEMP = 273.15)

! density of water at 20 C and 1 ATM (kg/m3)
      REAL H2ODENS
      PARAMETER (H2ODENS = 1000.0)

! Molecular weights of Fe and Mn
      REAL MW_FE, MW_MN
      PARAMETER (MW_FE = 55.8, MW_MN = 54.9)

! CO2 background concentration (moles/mole air)
      REAL CCO2
      PARAMETER (CCO2 = 3.4E-04)   ! 340 ppm

C...........ARGUMENTS and their descriptions

      REAL         TEMP            ! temperature (K)
      REAL         PRES_ATM        ! pressure (atm)
      REAL         TAUCLD          ! timestep for cloud (s)
      REAL         PRCRATE         ! precip rate (mm/hr)
      REAL         WCAVG           ! liquid water content (kg/m3)
      REAL         COZ             ! ozone concentration (mol/mol)
      REAL         CH2O2           ! H2O2 concentration (mol/mol)
      REAL         CTNH3           ! total ammonium  concentration (mol/mol)
      REAL         CONC   ( 6 )    ! species concentrations (mol/molV)
      real         rhoair          ! air density, moles/m3

! --- puff length scale (for scavenging coefficient calculations) (m)
      real len

! --- Scavenging coefficients (1/s)
      real scav( 6 )

C...........LOCAL VARIABLES (scalars) and their descriptions:

      REAL         RT              ! gas const * temperature (liter atm/mol)
      REAL         RECIPAP1        ! one over pressure (/atm)
      REAL         ONE_OVER_TEMP   ! 1.0 / TEMP

! --- Gas liquid equilibria

      REAL         PH2O20          ! total H2O2 partial pressure (atm)
      REAL         PH2O2F          ! gas only H2O2 partial pressure (atm)
      REAL         H2O2H           ! Henry's Law Constant for H2O2
      REAL         H2O2L           ! H2O2 conc in cloudwater (mol/liter)

      REAL         PHNO30          ! total HNO3 partial pressure (atm)
      REAL         PHNO3F          ! gas only HNO3 partial pressure (atm)
      REAL         HNO3H           ! Henry's Law Constant for HNO3
      REAL         HNO31           ! First dissociation constant for HNO3
      REAL         HNO31H          ! HNO31*HNO3H
      REAL         HNO3L           ! HNO3 conc in cloudwater (mol/liter)

      REAL         PNH30           ! total NH3 partial pressure (atm)
      REAL         PNH3F           ! gas only NH3 partial pressure (atm)
      REAL         NH3H            ! Henry's Law Constant for NH3
      REAL         NH31            ! First dissociation constant for NH3
      REAL         NH3DH2O         ! 
      REAL         NH31HDH         !       
      REAL         NH3L            ! NH3 conc in cloudwater (mol/liter)

      REAL         PO30            ! total O3 partial pressure (atm)
      REAL         PO3F            ! gas only O3 partial pressure (atm)
      REAL         O3H             ! Henry's Law Constant for O3
      REAL         O3L             ! O3 conc in cloudwater (mol/liter)

      REAL         PSO20           ! total SO2 partial pressure (atm)      
      REAL         PSO2F           ! gas only SO2 partial pressure (atm)
      REAL         SO2H            ! Henry's Law Constant for SO2
      REAL         SO21            ! First dissociation constant for SO2
      REAL         SO22            ! Second dissociation constant for SO2
      REAL         SO212           ! SO21*SO22
      REAL         SO212H          ! SO21*SO22*SO2H
      REAL         SO21H           ! SO21*SO2H
      REAL         SO2L            ! SO2 conc in cloudwater (mol/liter)
      REAL         SO3             ! SO3= conc in cloudwater (mol/liter)

      REAL         PNO2            ! total NO2 partial pressure (atm)
      REAL         NO2H            ! Henry's Law Constant for NO2 (M/atm)
      REAL         NO2L            ! NO2 conc in cloudwater (mol/liter)

      REAL         PNO             ! total NO partial pressure (atm)
      REAL         NOH             ! Henry's Law Constant for NO (M/atm)
      REAL         NOL             ! NO conc in cloudwater (mol/liter)

      REAL         PCO20           ! total CO2 partial pressure (atm)
      REAL         PCO2F           ! gas only CO2 partial pressure (atm)
      REAL         CO2H            ! Henry's Law constant for CO2
      REAL         CO21            ! First dissociation constant for CO2
      REAL         CO22            ! Second dissociation constant for CO2
      REAL         CO212           ! CO21*CO22
      REAL         CO212H          ! CO2H*CO21*CO22
      REAL         CO21H           ! CO2H*CO21
      REAL         CO2L            ! CO2 conc in cloudwater (mol/liter)

      REAL         XL              ! conversion factor (liter-atm/mol)
      REAL         ONE_OVER_XL     ! 1.0 / XL
      REAL         PRES_ATM_OVER_XL     ! PRES_ATM / XL
      REAL         XLCO2           !
      REAL         XLH2O2          !
      REAL         XLHNO3          !
      REAL         XLNH3           !
      REAL         XLO3            ! 
      REAL         XLSO2           ! 

      REAL         HCO3            ! HCO3 conc in cloudwater (mol/liter)
      REAL         HSO3            ! HSO3 conc in cloudwater (mol/liter)
      REAL         HSO4            ! HSO4 concn in cloudwater (mol/liter)

      REAL         MN              ! Mn++ conc in cloudwater (mol/liter)
      REAL         MNA             ! initial Mn in cloudwater (mol/liter)
      REAL         NH4             ! NH4+ conc in cloudwater (mol/liter)
      REAL         NO3M            ! NO3- conc in cloudwater (mol/liter)
      REAL         OHM             ! OH- conc in cloudwater (mol/liter)
      REAL         SO4             ! SO4= conc in cloudwater (mol/liter)
      REAL         CO3             ! CO3= conc in cloudwater (mol/liter)

      REAL         A               ! iron's anion concentration
      REAL         B               ! manganese's anion concentration
      REAL         FE              ! Fe+++ conc in cloudwater (mol/liter)
      REAL         FEA             ! initial Fe in cloudwater (mol/liter)

      INTEGER      I20C            ! loop counter for do loop 20
      INTEGER      I30C            ! loop counter for do loop 30
      INTEGER      ITERAT          ! # iterations of aqueous chemistry solver
      INTEGER      I7777C          ! aqueous chem iteration counter
      INTEGER      ICNTAQ          ! aqueous chem iteration counter
      INTEGER      IOX             ! index over oxidation reactions

      REAL         ACT1            ! activity correction factor!single ions
      REAL         ACT2            ! activity factor correction!double ions
      REAL         ACTB            ! 

      REAL         FTST            !
      REAL         GM1             !
      REAL         GM1LOG          !      
      REAL         GM2             ! activity correction factor
      REAL         GM2LOG          !

      REAL         FA              ! functional value ??
      REAL         FB              ! functional value ??
      REAL         AC              ! H+ concentration in cloudwater (mol/liter)
      REAL         AE              ! guess for H+ conc in cloudwater (mol/l)
      REAL         BB              ! lower limit guess of cloudwater pH
      REAL         HA              !
      REAL         HB              !
      REAL         STION           ! ionic strength  

      REAL         H2OW            !
      REAL         HTST            !
      REAL         RATE            !
      REAL         RECIPA1         !                   
      REAL         RECIPA2         ! 

      REAL         RH2O2           !
      REAL         RMHP            !
      REAL         RPAA            ! 
      REAL         SIV             ! dissolved so2 in cloudwater (mol/liter)
      REAL         SK6             !  
      REAL         SK6TS6          !
      REAL         DTS6            !                  
      REAL         TAC             ! 
      REAL         TEMP1           ! 
      REAL         TIMEW           ! cloud chemistry clock (sec)
      REAL         TOTOX           !      
      REAL         TS6             ! SO4 conc in cloudwater (mol/liter)
      REAL         TSIV            ! 
      REAL         TST             ! 

      REAL         DSIVDT( 0:NUMOX ) ! rate of so2 oxid incloud (mol/liter/sec)
      REAL         DS4   ( 0:NUMOX ) ! S(IV) oxidized over timestep DTW(0)
      real         ds40old           ! total s(iv) oxidized
      REAL         DTW   ( 0:NUMOX ) ! cloud chemistry timestep (sec)

      real         depfactor

C
C --- Define background concs of iron and manganese (ug/m3)
      real cfe, cmn
      data cfe/0.01/, cmn/0.005/

C ... Specify fraction of activation for particles.
      real fracma
      data fracma/1.0 /
                                         
C...........EXTERNAL FUNCTIONS and their descriptions:

      REAL HLCONST
      EXTERNAL HLCONST

C*********************************************************************
C     begin body of subroutine AQRADM

C...Check for bad temperature or pressure
      if ( temp .LE. 0.0 .or. pres_atm .LE. 0.0 ) then
         write(io6,*)'Invalid temp and/or pressure; T,P: ',temp,pres_atm
         stop 'Halted in AQRADM -- see list file.'
      end if

      one_over_temp = 1.0 / temp

C...Compute several conversion factors

      icntaq = 0
      iterat = 0
      rt = ( MOLVOL / STDTEMP ) * temp    ! r * t (liter atm / mol)
      xl   = wcavg * rt / H2ODENS         ! conversion factor (l-atm/mol)
      one_over_xl = 1.0 / xl
      pres_atm_over_xl = pres_atm / xl
      tst  = 0.999
      act1 = 1.0
      act2 = 1.0
      gm2  = 1.0
      timew = 0.0
      recipap1 = 1.0 / pres_atm

C...set equilibrium constants as a function of temperature
C...Henry's law constants

      so2h  = HLCONST( 'SO2', temp, .false., 0.0 )
      co2h  = HLCONST( 'CO2', temp, .false., 0.0 )
      nh3h  = HLCONST( 'NH3', temp, .false., 0.0 )
      h2o2h = HLCONST( 'H2O2', temp, .false., 0.0 )
      o3h   = HLCONST( 'O3', temp, .false., 0.0 )
      hno3h = HLCONST( 'HNO3', temp, .false., 0.0 )
      noh   = HLCONST( 'NO', temp, .false., 0.0 )
      no2h  = HLCONST( 'NO2', temp, .false., 0.0 )

C...Dissociation constants

      temp1 = one_over_temp - 1.0 / 298.0

      sk6   = 1.02e-02 * EXP(  2.72e+03 * temp1 )    ! Smith and Martell (1976)
      so21  = 1.30e-02 * EXP(  1.96e+03 * temp1 )    ! Smith and Martell (1976)
      so22  = 6.60e-08 * EXP(  1.50e+03 * temp1 )    ! Smith and Martell (1976)
      co21  = 4.30e-07 * EXP( -1.00e+03 * temp1 )    ! Smith and Martell (1976)
      co22  = 4.68e-11 * EXP( -1.76e+03 * temp1 )    ! Smith and Martell (1976)
      h2ow  = 1.00e-14 * EXP( -6.71e+03 * temp1 )    ! Smith and Martell (1976)
      nh31  = 1.70e-05 * EXP( -4.50e+02 * temp1 )    ! Smith and Martell (1976)
      hno31 = 1.54e+01 * EXP(  8.70e+03 * temp1 )    ! Schwartz (1984)

C...Kinetic oxidation rates
C...   From Chamedies (1982)

      rh2o2 = 8.0e+04 * EXP( -3650.0 * temp1 )

C...Make initializations

      do iox = 0, NUMOX
         dsivdt(iox) = 0.0
         dtw(iox)    = 0.0
         ds4(iox)    = 0.0
      end do

      fea = cfe * 1.E-6 * pres_atm_over_xl / (rhoair * MW_FE)
      mna = cmn * 1.E-6 * pres_atm_over_xl / (rhoair * MW_MN)

C...Set constant factors that will be used in later multiplications (moles/atm)

      xlh2o2  = h2o2h * xl
      xlo3    = o3h   * xl
      xlso2   = so2h  * xl
      xlnh3   = nh3h  * xl
      xlhno3  = hno3h * xl
      xlco2   = co2h  * xl

      so212   = so21  * so22
      so21h   = so21  * so2h
      so212h  = so212 * so2h
      co212   = co21  * co22
      co21h   = co21  * co2h
      co212h  = co22  * co21h
      nh3dh2o = nh31  / h2ow
      nh31hdh = nh3h  * nh3dh2o
      hno31h  = hno31 * hno3h

C...If kinetic calculations are made, return to this point

      i20c = 0
20    continue

      i20c = i20c + 1
      if ( i20c >= 1000 ) then 
         write(io6,*) 'Excessive looping at I20C'
         stop 'Halted in AQRADM -- see list file.'
      end if

C...Initial gas phase partial pressures (atm)
      pnh30  = ctnh3 * pres_atm
      phno30 = ( conc(5)  + conc(6)*fracma ) * pres_atm
      pco20  = cco2 * pres_atm

! --- Reactive species

! --- H2O2
      ph2o20 = ch2o2 * pres_atm + xl * ds4( 1 )
! --- check if too much h2o2 has reacted (possible in plume with
! --- high SO2 concs)
      if ( ph2o20 .LT. 0. ) then
         ds4( 0 ) = ds4( 0 ) - ds4( 1 )
         ds4( 1 ) = -ch2o2 * pres_atm * one_over_xl
         ds4( 0 ) = ds4( 0 ) + ds4( 1 )
         ph2o20 = 0.
      end if

! --- O3
      po30   = coz * pres_atm + xl * ds4( 2 )
! --- check if too much o3 has reacted
      if ( po30 .LT. 0. ) then
         ds4( 0 ) = ds4( 0 ) - ds4( 2 )
         ds4( 2 ) = -coz * pres_atm * one_over_xl
         ds4( 0 ) = ds4( 0 ) + ds4( 2 )
         po30 = 0.
      end if

! --- SO2
      pso20  = conc( 1  ) * pres_atm + ds4( 0 ) * xl
! --- check if too much SO2 has reacted
      if ( pso20 .LT. 0. ) then
         ds40old = ds4( 0 )
         ds4( 0 ) = -conc( 1 ) * pres_atm * one_over_xl
         do iox = 1, NUMOX
            ds4( iox ) = ds4( iox ) * ds4( 0 ) / ds40old
         end do
         pso20 = 0.
         ph2o20 = ch2o2 * pres_atm + xl * ds4( 1 )
         po30   = coz * pres_atm + xl * ds4( 2 )
      end if

C...Don't allow gas concentrations to go below zero

!     pso20  = MAX( pso20,  0.0 )
!     ph2o20 = MAX( ph2o20, 0.0 )
!     po30   = MAX( po30,   0.0 )
      pnh30  = MAX( pnh30,  0.0 )
      pco20  = MAX( pco20,  0.0 )
      phno30 = MAX( phno30, 0.0 )

C...Molar concentrations of soluble aerosols
      ts6     = conc( 2 ) * fracma * pres_atm_over_xl
     &        - ds4( 0 )   ! Sulfate 

      fe      = fea
      mn      = mna
      a       = 3.0 * fe
      b       = 2.0 * mn

C...Don't allow aerosol concentrations to go below zero

      ts6     = MAX( ts6,     0.0 )

      sk6ts6 = sk6 * ts6

C...Find solution of the equation using a method of reiterative 
C...bisections. Make initial guesses for pH between PHMIN  to  PHMAX. 

      ha = PHMIN
      hb = PHMAX

      i7777c = 0
7777  continue

      i7777c = i7777c + 1
      if ( i7777c .GT. 1000 ) then
         write(io6,*)'Excessive looping at I7777C'
         stop 'Halted in AQRADM -- see list file.'
      end if

!     ha = MAX( ha - 0.8, 0.1 )
!     hb = MIN( hb + 0.8, 9.9 )
      ha = MAX( ha - 0.8, PHMIN )
      hb = MIN( hb + 0.8, PHMAX )
      ae = 10.0**( -ha )

      recipa1 = 1.0 / ( ae * act1 )
      recipa2 = 1.0 / ( ae * ae * act2 )

C...Calculate final gas phase partial pressure of SO2, NH3, HNO3 and
C...CO2 (atm)

      pso2f = pso20 / ( 1.0 + xlso2 * ( 1.0 + so21 * recipa1
     &      + so212 * recipa2 ) )

      pnh3f = pnh30 / ( 1.0 + xlnh3 * ( 1.0 + nh3dh2o * ae ) )

      phno3f = phno30 / ( 1.0 + xlhno3 * ( 1.0 + hno31 * recipa1 ) )

      pco2f = pco20 / ( 1.0 + xlco2 * ( 1.0 + co21 * recipa1
     &      + co212 * recipa2 ) ) 

C...Calculate liquid phase concentrations (moles/liter) 

      so4  = sk6ts6 / ( ae * gm2 + sk6 )
      hso4 = ts6 - so4
      so3  = so212h  * pso2f  * recipa2
      hso3 = so21h   * pso2f  * recipa1
      co3  = co212h  * pco2f  * recipa2
      hco3 = co21h   * pco2f  * recipa1
      ohm  = h2ow    * recipa1
      nh4  = nh31hdh * pnh3f  * ae
      no3m = hno31h  * phno3f * recipa1

C...Compute functional value

      fa = ae + nh4 - 2.0 *  (co3 + so3 + so4 ) - ohm - hco3
     &   - hso3 - no3m - hso4

C...Start iteration and bisection ****************<<<<<<<

      i30c = 0
30    continue

      i30c = i30c + 1
      if ( i30c .GT. 1000 ) then
         write(io6,*)'Excessive looping at I30C'
         stop 'Halted in AQRADM -- see list file.'
      end if

      bb = 0.5 * ( ha + hb )
      ae = 10.0**( -bb )

! --- don't solve for H+ if fa < 0 at first try
      if ( i7777c .EQ. 1 .and. fa .LT. 0. ) then

         bb = ha
         hb = ha
         ae = 10.0**( -bb )

      end if

      recipa1 = 1.0 / ( ae * act1 )
      recipa2 = 1.0 / ( ae * ae * act2 )

C...Calculate final gas phase partial pressure of SO2, NH3, HNO3 and
C...CO2 (atm)

      pso2f = pso20 / ( 1.0 + xlso2
     &	    * ( 1.0 + so21 * recipa1 + so212 * recipa2 ) )

      pnh3f = pnh30 / ( 1.0 + xlnh3 * ( 1.0 + nh3dh2o * ae ) )

      phno3f = phno30 / ( 1.0 + xlhno3 * ( 1.0 + hno31 * recipa1 ) )

      pco2f = pco20 / ( 1.0 + xlco2 * ( 1.0 + co21 * recipa1
     &      + co212 * recipa2 ) ) 

C...Calculate liquid phase concentrations (moles/liter)

      so4  = sk6ts6 / ( ae * gm2 + sk6 )
      hso4 = ts6 - so4
      so3  = so212h  * pso2f  * recipa2
      hso3 = so21h   * pso2f  * recipa1
      co3  = co212h  * pco2f  * recipa2
      hco3 = co21h   * pco2f  * recipa1
      ohm  = h2ow    * recipa1
      nh4  = nh31hdh * pnh3f  * ae
      no3m = hno31h  * phno3f * recipa1

C...compute functional value

      fb = ae + nh4 - 2.0 * ( co3 + so3 + so4 ) - ohm - hco3
     &   - hso3 - no3m - hso4

C...Calculate and check the sign of the product of the two functional values

      ftst = fa * fb
      if ( ftst .LE. 0.0 ) then 
        hb = bb
      else
        ha = bb
        fa = fb
      end if

C...Check convergence of solutions 

      htst = ha / hb
      if ( htst .LE. tst ) go to 30

C...end of zero-finding routine ****************<<<<<<<<<<<< 

C...compute Ionic strength and activity coefficient by the Davies equation

      stion = 0.5 * (ae + nh4 + ohm + hco3 + hso3
     &      + 4.0 * (so4 + co3 + so3)
     &      + no3m + hso4 + 9.0 * fe + a + b)
      gm1log = -0.509 * ( SQRT( stion )
     &       / ( 1.0 + SQRT( stion ) ) - 0.2 * stion )
      gm2log = gm1log * 4.0
      gm1  = 10.0**gm1log
      gm2  = MAX( 10.0**gm2log, 1.0e-30 )
      actb = act1
      act1 = MAX( gm1 * gm1, 1.0e-30 )
      act2 = MAX( gm1 * gm1 * gm2, 1.0e-30 )

C...check for convergence and possibly go to 7777, to recompute 
C...  Gas and liquid phase concentrations

! --- don't solve for H+ if fa < 0 at first try
      if ( i7777c .EQ. 1 .and. fa .LT. 0. ) then
         actb = act1
      end if

      tac = ABS( actb - act1 ) / actb
      if ( tac >= 1.0e-2 ) then

         icntaq = icntaq + 1
         if ( icntaq .GT. 100 ) then
            write(io6,*)'Maximum iterations for pH calculation exceeded'
            write(io6,*)'Using last pH value'
         else
           go to 7777
         end if

      end if

C...return an error if the pH is not in range 

ccc      if ( ( ha .lt. 0.02 ) .or. ( ha .gt. 9.49 ) ) then 
      if ( ( ha .LT. PHMIN ) .or. ( ha .GT. PHMAX ) ) then
         write(io6,*)'pH value out of range: ',ha
         stop 'Halted in AQRADM -- see list file.'
      end if

C...Make those concentration calculations which can be made outside
C...  of the function.

      so2l = so2h * pso2f
      ac = 10.0**( -bb )
      siv = so3 + hso3 + so2l

C...Calculate final gas phase concentrations of oxidants (atm) 

      ph2o2f = ph2o20 / ( 1.0 + xlh2o2 )
      po3f   = po30   / ( 1.0 + xlo3   )

      ph2o2f = MAX( ph2o2f, 0.0 )
      po3f   = MAX( po3f,   0.0 )

C...Calculate liquid phase concentrations (moles/liter) 

      h2o2l = ph2o2f * h2o2h
      o3l   = po3f   * o3h
      nh3l  = pnh3f  * nh3h
      co2l  = pco2f  * co2h
      hno3l = phno3f * hno3h

C...if the maximum cloud lifetime has not been reached, then compute
C...the next timestep.

      if ( timew .LT. taucld ) then

C...make kinetics calculations
C...  note: DS4(i) and DSIV(I) are negative numbers!

        iterat = iterat + 1

C...Define the total S(iv) available for oxidation

        tsiv = pso20 * one_over_xl

C...Calculate sulfur iv oxidation rate due to H2O2

        dsivdt( 1 ) = -rh2o2 * h2o2l * so2l / ( 0.1 + ac )
        totox = ph2o20 * one_over_xl
        if ( ( dsivdt( 1 ) .EQ. 0.0 ) .or.
     &       ( tsiv  .LE. CONCMIN ) .or.
     &       ( totox .LE. CONCMIN ) ) then
          dtw(1) = taucld 
        else
          dtw( 1 ) = -0.05 * MIN( totox, tsiv ) / dsivdt( 1 )
        end if

C...Calculate sulfur iv oxidation rate due to O3

        if ( bb .GE. 2.7 ) then
          dsivdt( 2 ) = -4.19e5 * ( 1.0 + 2.39e-4 / ac ) * o3l * siv
        else
          dsivdt( 2 ) = -1.9e4 * siv * o3l / SQRT( ac )
        end if
        totox = po30 * one_over_xl
        if ( ( dsivdt( 2 ) .EQ. 0.0 ) .or.
     &       ( tsiv  .LE. CONCMIN ) .or.
     &       ( totox .LE. CONCMIN ) ) then
          dtw( 2 ) = taucld
        else
          dtw( 2 ) = -0.01 * MIN( totox, tsiv ) / dsivdt( 2 )
        end if

C...Calculate sulfur iv oxidation rate due to O2 catalyzed by Mn++ 
C...  and Fe+++  See Table IV Walcek & Taylor ( 1986) 

        if ( bb .GE. 4.0 )  then  ! 4.0  < ph
	   
          if ( siv .LE. 1.0e-5 ) then
            dsivdt( 3 ) = -5000.0 * mn * hso3       
          else
            dsivdt( 3 ) = -( 4.7 * mn * mn / ac
     &                  + 1.0e7 * fe * siv * siv )
          end if  ! end of first pass through siv conc.

        else          ! ph < 4.0

          if ( siv .LE. 1.0e-5 ) then
            dsivdt( 3 ) = -3.0 * ( 5000.0 * mn * hso3
     &                  + 0.82 * fe * siv / ac )
          else
            dsivdt( 3 ) = -( 4.7 * mn * mn / ac
     &                  + ( 0.82 * fe * siv / ac )
     &                  * ( 1.0 + 1.7e3 * mn**1.5 / ( 6.3e-6 + fe ) ) )
          end if ! end of second pass through siv conc.
        end if  ! end of pass through ph

        if ( ( dsivdt( 3 ) .EQ. 0.0 ) .or. ( tsiv .LE. CONCMIN ) ) then
          dtw( 3 ) = taucld
        else
          dtw( 3 ) = -0.1 * tsiv / dsivdt( 3 )
        end if

C...Calculate total sulfur iv oxidation rate

        dsivdt( 0 ) = 0.0
        do iox = 1, NUMOX
          dsivdt( 0 ) = dsivdt( 0 ) + dsivdt( iox )
        end do

C...Calculate a minimum time step required

        dtw( 0 ) = MIN( dtw( 1 ), dtw( 2 ), dtw( 3 ) )

C...check for large time step

        if ( dtw( 0 ) .GT. 8.0e+37 ) then
          write(io6,1001) dsivdt(0), ts6, dtw(0)
        else

C...calculate the change in sulfur iv for this time step

60        continue
          dts6 = ABS( dtw( 0 ) * ( -dsivdt( 0 ) ) )

C...If DSIV(0), sulfur iv oxidized during this time step would be 
C...less than 5% of sulfur oxidized since time 0, then double DT 

          if ( dtw( 0 ) .LE. taucld ) then
            if ( dts6 .LT. 0.05 * ts6 ) then 
              dtw( 0 ) = dtw( 0 ) * 2.0 
	      go to 60
            end if
          end if
        end if
        dtw( 0 ) = MIN( dtw( 0 ), taucld )

C...If the total time after this time increment will be greater than 
C...  TAUCLD sec., then set DTW(0) so that total time will be TAUCLD

        if ( timew + dtw( 0 ) .GT. taucld ) dtw( 0 ) = taucld - timew
        if ( ts6 .LT. 1.0e-11 ) dtw( 0 ) = taucld - timew
        if ( iterat .GT. 100 ) dtw( 0 ) = taucld - timew 

C...Set DSIV(I), I = 0,NUMOX, the amount of S(IV) oxidized by each 
C... individual oxidizing agent, as well as the total.

        do iox = 0, NUMOX
           ds4( iox ) = ds4( iox ) + dtw( 0 ) * dsivdt( iox )
        end do

        timew = timew + dtw( 0 )

C...Return to make additional calculations

        go to 20
      end if

C --- Calculate liquid-phase concentrations of other species
      pno    = conc(3) * pres_atm
      pno2   = conc(4) * pres_atm

      nol    = pno   * noh   / ( 1.0 + noh    * xl )
      no2l   = pno2  * no2h  / ( 1.0 + no2h   * xl )

C...Compute the output concentrations

C...gas concentrations (mol/molV) (only for reactive species)

      conc(1) = (pso2f  + xl * siv)   * recipap1
      ch2o2   = (ph2o2f + xl * h2o2l) * recipap1
      coz     = (po3f   + xl * o3l)   * recipap1

! --- calculate scavenging coefficients for gases
      depfactor = prcrate * SEC2HR / ( rhoair * len )
      if ( conc( 1 ) > CONCMIN ) then
         scav( 1 ) = siv * depfactor / conc( 1 )
      end if
      if ( conc( 3 ) > CONCMIN ) then
         scav( 3 ) = nol * depfactor / conc( 3 )
      end if
      if ( conc( 4 ) > CONCMIN ) then
         scav( 4 ) = no2l * depfactor / conc( 4 )
      end if
      if ( conc( 5 ) > CONCMIN ) then
         scav( 5 ) = hno3l * depfactor / conc( 5 )
      end if

C...aerosol concentrations (mol/molV) (only reactive species-so4)

      conc( 2 ) = conc( 2 ) * ( 1.0 - fracma ) + ts6 * xl * recipap1

! --- calculate scavenging coefficients for aerosols

      if ( conc( 2 ) > CONCMIN ) then
         scav( 2 ) = ts6 * depfactor / conc( 2 )
      end if
      if ( conc( 6 ) > CONCMIN ) then
         scav( 6 ) = no3m * depfactor / conc( 6 )
      end if

      return

C...formats

1001  format (1X,'DSIVDT(0) =', F10.5,  
     &       'TS6=', F10.5, 'DTW(0)=', F10.5)

      end

c----------------------------------------------------------------------
      REAL FUNCTION HLCONST ( NAME, TEMP, EFFECTIVE, HPLUS )
c----------------------------------------------------------------------

C
C  FUNCTION: return the Henry's law constant for the specified substance
C            at the given temperature
C  Adapted for CALPUFF from CMAQ version, PK, AER, Feb 2007

      IMPLICIT NONE

C...........PARAMETERS and their descriptions:
!      include 'params.puf'
! temporarily set io6=2 here since params.puf variables are not declared,
! resulting in compile errors.
      INTEGER IO6
      PARAMETER (IO6 = 2) ! from params.puf

! Number of species
      INTEGER MXSPCS
      PARAMETER (MXSPCS = 8)
! Number of dissociating species
      INTEGER MXDSPCS
      PARAMETER (MXDSPCS = 8)

C...........ARGUMENTS and their descriptions

      CHARACTER*(*) NAME                ! name of substance
      REAL          TEMP                ! temperature (K)
      LOGICAL       EFFECTIVE           ! true=compute the effective henry's law constant
      REAL          HPLUS               ! hydrogen ion concentration (mol/l)

C...........SCRATCH LOCAL VARIABLES and their descriptions:
      CHARACTER*16 SUBNAME(MXSPCS)
      SAVE SUBNAME

      INTEGER       SPC                 ! species index
      INTEGER       LSO2                ! SO2 pointer
      INTEGER       LHSO3               ! HSO3 pointer
      INTEGER       LHNO3               ! HNO3 pointer
      INTEGER       LCO2                ! CO2 pointer
      INTEGER       LHCO3               ! HCO3 pointer
      INTEGER       LH2O2               ! H2O2 pointer
      INTEGER       LHO2                ! HO2 pointer
      INTEGER       LNH4OH              ! NH4OH pointer
      INTEGER       LH2O                ! H2O pointer

      REAL          HPLUSI              ! 1 / HPLUS
      REAL          HPLUS2I             ! 1 / HPLUS**2
      REAL          TFAC                ! (298-T)/(T*298)
      REAL          AKEQ1               ! temp var for dissociation constant
      REAL          AKEQ2               ! temp var for dissociation constant
      REAL          OHION               ! OH ion concentration
      REAL          KH                  ! temp var for henry's law constant

! Henry's law constants at 298.15K (M/atm) (taken from Rolf Sanders'
! Compilation of Henry's Law Constants for Inorganic and Organic Species
! of Potential Importance in Environment Chemistry, 1999)
      REAL A(MXSPCS)
      SAVE A

! Enthalpy (like activation energy) (K) (taken from Rolf Sanders'
! Compilation of Henry's Law Constants for Inorganic and Organic Species
! of Potential Importance in Environment Chemistry, 1999)
      REAL E(MXSPCS)
      SAVE E

! Dissociation constants at 298.15K (M or M2) (taken from Table 6.A.1,
! Seinfeld and Pandis, Atmospheric Chemistry and Physics, 1997)
      REAL B(MXDSPCS)
      SAVE B

! -dH/R (K) (taken from Table 6.A.1,
! Seinfeld and Pandis, Atmospheric Chemistry and Physics, 1997)
      REAL D(MXDSPCS)
      SAVE D

      DATA SUBNAME(1), A(1), E(1) / 'O3', 1.2E-02, 2.7E+03 /  ! Chameides 1984
      DATA SUBNAME(2), A(2), E(2) / 'H2O2', 8.3E+04, 7.4E+03 /  ! O'Sullivan et al. 1996
      DATA SUBNAME(3), A(3), E(3) / 'NH3', 6.1E+01, 4.2E+03 /  ! Clegg and Brimblecombe 1989
      DATA SUBNAME(4), A(4), E(4) / 'NO', 1.9E-03, 1.4E+03 /  ! Lide and Frederikse 1995
      DATA SUBNAME(5), A(5), E(5) / 'NO2', 1.2E-02, 2.5E+03 /  ! Chameides 1984
      DATA SUBNAME(6), A(6), E(6) / 'HNO3', 2.1E+05, 8.7E+03 /  ! Leieveld and Crutzen 1991
      DATA SUBNAME(7), A(7), E(7) / 'SO2', 1.4E+00, 2.9E+03 /  ! Linde and Frederikse 1995
      DATA SUBNAME(8), A(8), E(8) / 'CO2', 3.6E-02, 2.2E+03 /  ! Zheng et al. 1997

      DATA LSO2,  B(1), D(1) /  1, 1.30E-02,  1.96E+03 /  ! SO2*H2O<=>HSO3+H     : Smith and Martell (1976)
      DATA LHSO3, B(2), D(2) /  2, 6.60E-08,  1.50E+03 /  ! HSO3<=>SO3+H         : Smith and Martell (1976)
      DATA LHNO3, B(3), D(3) /  3, 1.54E+01,  8.70E+03 /  ! HNO3(aq)<=>NO3+H     : Schwartz (1984)
      DATA LCO2,  B(4), D(4) /  4, 4.30E-07, -1.00E+03 /  ! CO2*H2O<=>HCO3+H     : Smith and Martell (1976)
      DATA LHCO3, B(5), D(5) /  5, 4.68E-11, -1.76E+03 /  ! HCO3<=>CO3+H         : Smith and Martell (1976)
      DATA LH2O2, B( 6), D(6) /  6, 2.20E-12, -3.73E+03 /  ! H2O2(aq)<=>HO2+H     : Smith and Martell (1976)
      DATA LNH4OH, B(7), D(7) /  7, 1.70E-05, -4.50E+02 /  ! NH4*OH<=>NH4+OH      : Smith and Martell (1976)
      DATA LH2O,   B(8), D(8) /  8, 1.00E-14, -6.71E+03 /  ! H2O<=>H+OH           : Smith and Martell (1976)

C...........EXTERNAL FUNCTIONS and their descriptions:

! Function to look up name in table
      INTEGER INDEX1
      EXTERNAL INDEX1

C-----------------------------------------------------------------------
C  begin body of subroutine HLCONST

      SPC = INDEX1( NAME, MXSPCS, SUBNAME )

C...error if species not found in table

      IF ( SPC <= 0 ) THEN
         write(io6,*)TRIM(NAME) // 
     &         ' not found in Henrys Law Constant table'
         stop 'Halted in HLCONST -- see list file.'
      END IF

C...compute the Henry's Law Constant
      TFAC = (298.0 - TEMP) / (298.0 * TEMP)
      KH = A(SPC) * EXP(E(SPC) * TFAC)
      HLCONST = KH

C...compute the effective Henry's law constants

      IF (EFFECTIVE) THEN

         IF ( HPLUS <= 0.0 ) THEN
            write(io6,*)'Negative or Zero [H+] concentration specified '
            stop 'Halted in HLCONST -- see list file.'
         END IF

         HPLUSI = 1.0 / HPLUS
         HPLUS2I = HPLUSI * HPLUSI

         IF (TRIM(NAME) .EQ. 'SO2') THEN

            AKEQ1 = B(LSO2) * EXP(D(LSO2) * TFAC)     !SO2H2O <=> HSO3- + H+
            AKEQ2 = B(LHSO3) * EXP(D(LHSO3) * TFAC)   !HSO3- <=> SO3= + H+
            HLCONST = KH * (1.0 + AKEQ1*HPLUSI + AKEQ1*AKEQ2*HPLUS2I)

         ELSE IF (TRIM(NAME) .EQ. 'HNO3') THEN

            AKEQ1 = B(LHNO3) * EXP(D(LHNO3) * TFAC)   !HNO3(aq) <=> NO3- + H+
            HLCONST = KH * (1.0 + AKEQ1*HPLUSI)

         ELSE IF (TRIM(NAME) .EQ. 'CO2') THEN

            AKEQ1 = B(LCO2) * EXP(D(LCO2)  * TFAC)    !CO2H2O <=> HCO3- + H+
            AKEQ2 = B(LHCO3) * EXP(D(LHCO3) * TFAC)   !HCO3- <=> CO3= + H+
            HLCONST = KH * (1.0 + AKEQ1*HPLUSI + AKEQ1*AKEQ2*HPLUS2I)

         ELSE IF (TRIM(NAME) .EQ. 'H2O2') THEN

            AKEQ1 = B(LH2O2) * EXP(D(LH2O2) * TFAC)   !H2O2(aq) <=> HO2- + H+
            HLCONST = KH * (1.0 + AKEQ1*HPLUSI)

         ELSE IF (TRIM(NAME) .EQ. 'NH3') THEN

            AKEQ1 = B(LNH4OH) * EXP(D(LNH4OH) * TFAC) !NH4OH <=> NH4+ + OH-
            AKEQ2 = B(LH2O) * EXP(D(LH2O) * TFAC)
            OHION = AKEQ2 * HPLUSI
            HLCONST = KH * (1.0 + AKEQ1/OHION)

         END IF

      END IF

      RETURN
      END

c----------------------------------------------------------------------
      INTEGER FUNCTION INDEX1 (NAME, N, NLIST)
c----------------------------------------------------------------------

C***********************************************************************
C  subroutine body starts at line 39
C
C  FUNCTION:
C
C    Searches for NAME in list NLIST and returns the subscript
C    (1...N) at which it is found, or returns 0 when NAME not
C    found in NLIST
C
C  PRECONDITIONS REQUIRED:  none
C
C  SUBROUTINES AND FUNCTIONS CALLED:  none
C
C  Based on index1 routine from Models-3 I/O Library
C
C***********************************************************************

      IMPLICIT NONE
 
C.......   Arguments and their descriptions:

      CHARACTER*(*) NAME        !  Character string being searched for
      INTEGER       N           !  Length of array to be searched
      CHARACTER*(*) NLIST(*)    !  array to be searched

C.......   Local variable:

      INTEGER       I   !  loop counter

C.....................................................................
C.......   begin body of INDEX1()

      DO 100 I = 1, N

          IF (TRIM(NAME) .EQ. TRIM(NLIST(I))) THEN    ! Found NAME in NLIST
              INDEX1 = I
              RETURN
          END IF

100   CONTINUE

      INDEX1 = 0        !  not found
      RETURN

      END
