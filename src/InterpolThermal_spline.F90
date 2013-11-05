! Name:
!   Interpol_Thermal_spline
!
! Purpose:
!   Interpolates LW transmittances and radiances to the cloud pressure level.
!
! Arguments:
!   Name        Type             In/Out   Description
!   Ctrl        struct       In       Control structure
!   SPixel      struct       In       Super-pixel structure
!   Pc          float             In       Cloud pressure (from state vector X)
!   SAD_Chan    float array  In       Channel characteristics.
!   RTM_Pc      struct       Out      Contains Tac, Tbc (interpolated
!                                          transmittances above and below cloud)
!                                          and gradients wrt cloud pressure.
!   status      int             Out      Standard status value not set here
!                        
! Algorithm:
!   
! Local variables:
!   Name            Type          Description
!   i               int           RTM pressure level index
!   j               int           Search loop counter
!   delta_p         float         Difference in pressure between consecutive RTM levels
!   delta_Tac       float array   Difference in Tac between consecutive RTM levels
!   delta_Tbc       float array   Difference in Tbc between consecutive RTM levels
!   delta_Rac_up    float array   Difference in Rac_up between consecutive RTM levels
!   delta_Rac_dwn   float array   Difference in Rac_dwn between consecutive RTM levels
!   delta_Rbc_up    float array   Difference in Rbc_up between consecutive RTM levels
!   delta_T         float array   Difference in temp between consecutive RTM levels
!   delta_H         float array   Difference in GBH between consecutive RTM levels

!   dT_dPc          float         Change in Temp w.r.t. Pc
!   dH_dPc          float         Change in GPH w.r.t. Pc
!   T               float array   T at Pc for each channel (all the same but
!                                 converted to array of Radiances by T2R, hence
!                                 array of input temps required).
!   H               float array   H at Pc for each channel
!   d2T_dP2         float array   2nd deriv. of T at RTM levels
!   d2H_dP2         float array   2nd deriv. of H at RTM levels
!   d2Tac_dP2       float array   2nd deriv. of Tac at RTM levels
!   d2Tbc_dP2       float array   2nd deriv. of Tbc at RTM levels
!   d2Rac_up_dP2    float array   2nd deriv. of Rac_up at RTM levels
!   d2Rac_dwn_dP2   float array   2nd deriv. of Rac_dwn at RTM levels
!   d2Rbc_up_dP2    float array   2nd deriv. of Rbc_up at RTM levels
!   dTac_dPc        float array   Change in Tac w.r.t. Pc
!   dTbc_dPc        float array   Change in Tbc w.r.t. Pc
!   delta_Pc        float         Difference in pressure between Pc and lower RTM level
!   delta_Tc        float array   Difference in trans. between Pc and lower RTM level
!   delta_Rc        float array   Difference in radiance between Pc and lower RTM level
!   dB_dT           float array   Gradient of Planck function w.r.t surface T
!   message         char          Warning or error message to pass to Write_Log
!
! History:
!   21st November, 2000, Kevin M. Smith : original version
!   14th February, 2001, KMS : Corrected the position of the end of the first 
!                              main if block.
!   16th Feb 2001, Andy Smith :
!      Using RTM_Pc struct to pass Tac, Tbc etc.
!   20th Feb 2001, Andy Smith :
!      Converting calculations from B to T, since B is not available from 
!      the SPixel RTM input. SAD_Chan is now required as an input argument.
!   26th Feb 2001, Andy Smith:
!      Changed test for P levels bounding Pc. Using 
!      (Pc >= p(j) and Pc < p(j+1)) instead of  
!      (Pc > p(j) and Pc <= p(j+1)) because P levels now go in order of
!      increasing pressure. Previously, if Pc was equal to P(1) it was not
!      matched (and if Pc < P(1) it's P index is set to 1!)
!    1st March 2001, Andy Smith:
!      Fixed dB_dT declaration. Was scalar, should have been array (Nthermal).
!   15th Mar 2001, Andy Smith:
!      Added ThF and ThL indices for RTM_Pc%LW arrays. Required because these
!      arrays are allocated to match the no. of thermal channels requested, but
!      in twilight not all of the requested thermal channels may be used.
!   11th May 2001, Andy Smith:
!      Added setting of RTM_Pc%Tc.
!   20th Jul 2001, Andy Smith:
!      Fixed setting of ThL. Last thermal channel should always be the one 
!      specified in Ctrl, not SPixel, since the RTM_Pc and RTM arrays are of
!      fixed size for the whole run, i.e are not re-allocated to match the no.
!      of thermal channels used in each SPixel, and it is the lower numbered
!      thermal channels that are not used in certain conditions.  
!      Added breakpoint outputs. 
!    7th Dec 2001, Andy Smith:
!      Added more information to interpolation failure warning message. 
!      Changed first test to locate Pc in the RTM pressure levels, now uses
!      >= rather than > in the test of Pc vs. SPixel...P(...Np). Previously,
!      Pc = max RTM P level was flagged as an interpolation failure.
!    12th December 2002 Caroline Poulsen
!      added in GPH follwed the example of temperature
!    23rd December 2002, Andy Smith:
!      Added height gradient dHc_dPC to RTM_Pc structure. Required later
!      for ascribing an error value to the height.
!    21st May 2003, Sam Dean:
!      Added temperature gradient dTc_dPC to RTM_Pc structure for same reason
!    5th Sep 2011, Chris Arnold:
!      Added d2X_dP2 variables s/t a cubic spline interpolation can be used.
!      The second derivatives of the tabulated RTM data are calculated using
!      the routine 'spline'. These are used to calculate the interpolates/
!      interpolated gradients using the cubic spline method described 
!      in 'Numerical Recipes for fortran 90' [Press, Flannery, Teukolsky, Vetterling]
!    5th Sep 2011, Chris Arnold:
!      Added surf_pressure and surf_index variables - RTM data is only interpolated
!      in range surf > TOA.
!    2nd November 2011 c. Poulsen added 'spline' into debug output
!    6th February 2012 C. Arnold Removed surf_pressure/surf_index - bug fix to 
!    deal with compiler issues
!    7th February 2012 C. Arnold added intent() to argument declarations
! 2013/01/17 Matthias Jerg: Adds code to extract RTM_Pc%dHc_dPc and RTM_Pc%dTc_dPc
!
! Bugs:
!   None known.
!
!------------------------------------------------------------------------------------
subroutine Interpol_Thermal_spline(Ctrl, SPixel, Pc, SAD_Chan, RTM_Pc, status)

    use CTRL_def
    use SPixel_def
    use SAD_Chan_def
    use RTM_Pc_def
    use Int_Routines_def

    implicit none

!   Define arguments

    type(CTRL_t), intent(in)            :: Ctrl
    type(SPixel_t), intent(in)          :: SPixel
    type(SAD_Chan_t), intent(in)        :: SAD_Chan(SPixel%Ind%Nthermal)
    type(RTM_Pc_t) , intent(inout)      :: RTM_Pc
    real, intent(in)                    :: Pc
    integer , intent(inout)             :: status

!   Define local variables

    integer  :: i,j,k                  ! Counters
    real     :: dP                     ! Fractional distance of Pc from bottom of interval
    real     :: P1                     ! Fractional distance of Pc from top of interval
    real     :: k0                     ! Spline coefficient
    real     :: k1                     ! Spline coefficient
    real     :: delta_p
    real     :: delta_Tac(SPixel%Ind%Nthermal)
    real     :: delta_Tbc(SPixel%Ind%Nthermal)
    real     :: delta_Rac_up(SPixel%Ind%Nthermal)
    real     :: delta_Rac_dwn(SPixel%Ind%Nthermal)
    real     :: delta_Rbc_up(SPixel%Ind%Nthermal)
    real     :: delta_T       ! Step in Temp between pressure levels 
    real     :: delta_H       ! Step in GPH between pressure levels 
    real     :: dT_dPc        ! Gradient of T wrt Pc
    real     :: dH_dPc        ! Gradient of GPH wrt Pc
    real     :: T(SPixel%Ind%Nthermal) ! Calculated Temp at Pc. T2R wants array
    real     :: H(SPixel%Ind%Nthermal) ! Calculated GPH at Pc.
    real     :: d2T_dP2(SPixel%RTM%LW%Np)
    real     :: d2H_dP2(SPixel%RTM%LW%Np)
    real     :: d2Tac_dP2(SPixel%Ind%Nthermal,SPixel%RTM%LW%Np)
    real     :: d2Tbc_dP2(SPixel%Ind%Nthermal,SPixel%RTM%LW%Np)
    real     :: d2Rac_up_dP2(SPixel%Ind%Nthermal,SPixel%RTM%LW%Np)
    real     :: d2Rac_dwn_dP2(SPixel%Ind%Nthermal,SPixel%RTM%LW%Np)
    real     :: d2Rbc_up_dP2(SPixel%Ind%Nthermal,SPixel%RTM%LW%Np)
!    real     :: dTac_dPc(SPixel%Ind%Nthermal) ! var not used
!    real     :: dTbc_dPc(SPixel%Ind%Nthermal) ! var not used
    real     :: delta_Pc
!    real     :: delta_Tc(SPixel%Ind%Nthermal) ! var not used
!    real     :: delta_Rc(SPixel%Ind%Nthermal) ! var not used
    real     :: dB_dT(SPixel%Ind%Nthermal)         ! Gradient of Planck function wrt Temp.
    integer  :: ThF, ThL     ! First, last thermal channel indices for RTM_Pc%LW arrays
    character(180) :: message
    integer  :: bkp_lun, ios ! Logical unit number and IO status for breakpoint file.

!    write(*,*) 'spline0'

!   Set initial value of error status equal to zero (i.e. no error)

    status = 0

!   Use ThF and ThL to access the first and last required thermal channels
!   from RTM_Pc and SPixel %LW arrays, since these are always allocated to size 
!   Ctrl%Ind%Nthermal, but not all thermal channels are used in all pixels
!   hence SPixel%Ind%ThermalFirst may not equal Ctrl%Ind%ThermalFirst.

    ThF = 1 + SPixel%Ind%ThermalFirst - Ctrl%Ind%ThermalFirst
    ThL = Ctrl%Ind%Nthermal

!   Search for Pc in the LW RTM pressure levels.  If Pc lies outwith the RTM pressure
!   levels avoid search and set index to 1 or the penultimate RTM level.

    if (Pc > SPixel%RTM%LW%P(SPixel%RTM%LW%Np)) then
!       When Pc above pressure at highest level in RTM
        i = SPixel%RTM%LW%Np - 1
        if (abs(Pc-SPixel%RTM%LW%p(SPixel%RTM%LW%Np)) > 50.0) then
!           When there is a difference of more than 50 hPa between Pc and RTM level
            write(unit=message, fmt=*) 'Interpol_Thermal_spline: Extrapolation warning'
            call Write_Log(Ctrl, trim(message), status) ! Write to log
        end if
    else if (Pc < SPixel%RTM%LW%p(1)) then
!       When Pc below lowest in RTM
        i = 1
        if (abs(Pc-SPixel%RTM%LW%p(1)) > 50.0) then
!           When there is a difference of more than 50 hPa between Pc and RTM level
            write(unit=message, fmt=*) 'Interpol_Therm: Extrapolation warning'
            call Write_Log(Ctrl, trim(message), status) ! Write to log
        end if
    else
        do j = 1, SPixel%RTM%LW%Np-1 ! Search through RTM levels sequentially
                                     ! to find those bounding Pc
            if (Pc >= SPixel%RTM%LW%p(j) .and. Pc < SPixel%RTM%LW%p(j+1)) then
                i = j ! Set index equal to the lower bounding RTM level
                status = 0
                exit ! Break out of do loop to save time
            end if
            status = 1 ! Bounding levels not found
        end do
    end if
!    write(*,*) 'spline1','status',status,'PC',pc
!    pause
    if (status /= 0) then
!      If none of the above conditions are met (e.g. Pc = NaN) then return
!      with a fatal error
       status = IntTransErr ! Set status to indicate failure of interpolation
       write(unit=message, fmt=*) 'Interpol_Thermal_spline: Interpolation failure, ',&
          'Pc = ',Pc,', SPixel starting at',SPixel%Loc%X0, SPixel%Loc%Y0, &
          ' RTM P(Np), P(1): ', SPixel%RTM%LW%P(SPixel%RTM%LW%Np), &
          SPixel%RTM%LW%p(1)
       call Write_Log(Ctrl, trim(message), status) ! Write to log
       write(*,*) 'FATAL ERROR'
       stop
    else

!      Start the interpolation or extrapolation calculations
!      Note: Implicit looping over instrument channels from here onwards

        do k = ThF,ThL
!           write(*,*) 'spline2 ',k
            call spline(SPixel%RTM%LW%p(1:SPixel%RTM%LW%Np),&
                 & SPixel%RTM%LW%Tac(k,1:SPixel%RTM%LW%Np),d2Tac_dP2(k,1:SPixel%RTM%LW%Np))
            call spline(SPixel%RTM%LW%p(1:SPixel%RTM%LW%Np),&
                 & SPixel%RTM%LW%Tbc(k,1:SPixel%RTM%LW%Np),d2Tbc_dP2(k,1:SPixel%RTM%LW%Np))
            call spline(SPixel%RTM%LW%p(1:SPixel%RTM%LW%Np),&
                 & SPixel%RTM%LW%Rac_up(k,1:SPixel%RTM%LW%Np),d2Rac_up_dP2(k,1:SPixel%RTM%LW%Np))
            call spline(SPixel%RTM%LW%p(1:SPixel%RTM%LW%Np),&
                 & SPixel%RTM%LW%Rac_dwn(k,1:SPixel%RTM%LW%Np),d2Rac_dwn_dP2(k,1:SPixel%RTM%LW%Np))
            call spline(SPixel%RTM%LW%p(1:SPixel%RTM%LW%Np),&
                 & SPixel%RTM%LW%Rbc_up(k,1:SPixel%RTM%LW%Np),d2Rbc_up_dP2(k,1:SPixel%RTM%LW%Np))
        enddo       

!        write(*,*) 'spline3'
        call spline(SPixel%RTM%LW%p(1:SPixel%RTM%LW%Np),&
             & SPixel%RTM%LW%T(1:SPixel%RTM%LW%Np),d2T_dP2(1:SPixel%RTM%LW%Np))
!        write(*,*) 'spline4'
        call spline(SPixel%RTM%LW%p(1:SPixel%RTM%LW%Np),&
             & SPixel%RTM%LW%H(1:SPixel%RTM%LW%Np),d2H_dP2(1:SPixel%RTM%LW%Np))

!       Change in pressure between RTM levels i and i+1
!       (delta_p is negative for decreasing pressure with increasing i)

!        write(*,*) 'spline5'
        delta_p = SPixel%RTM%LW%p(i+1) - SPixel%RTM%LW%p(i)
        dP      = (SPixel%RTM%LW%p(i+1)-Pc)/delta_p
        p1      = 1.0 - dP
        k0 = (((3.0*dP*dP)-1.0)/6.0) * delta_p
        k1 = (((3.0*p1*p1)-1.0)/6.0) * delta_p

!       Change in transmittances between RTM levels i and i+1
!       (delta_Tac/bc are positive for increasing trans. with increasing i)
!            write(*,*) 'spline6'
        delta_Tac = &
        SPixel%RTM%LW%Tac(ThF:ThL,i+1) - SPixel%RTM%LW%Tac(ThF:ThL,i)
        delta_Tbc = &
        SPixel%RTM%LW%Tbc(ThF:ThL,i+1) - SPixel%RTM%LW%Tbc(ThF:ThL,i)

!       Gradients of trans. w.r.t. pressure (around Pc)

!        write(*,*) 'spline7'
        do k = ThF,ThL
            RTM_Pc%LW%dTac_dPc(k) = (delta_Tac(k) / delta_p) - (k0 * d2Tac_dP2(k,i)) + (k1 * d2Tac_dP2(k,i+1)) 
            RTM_Pc%LW%dTbc_dPc(k) = (delta_Tbc(k) / delta_p) - (k0 * d2Tbc_dP2(k,i)) + (k1 * d2Tbc_dP2(k,i+1))
        enddo

!       Change in radiances between RTM levels i and i+1
!        write(*,*) 'spline8'
        delta_Rac_up  = &
        SPixel%RTM%LW%Rac_up(ThF:ThL,i+1) - SPixel%RTM%LW%Rac_up(ThF:ThL,i)
        delta_Rac_dwn = &
        SPixel%RTM%LW%Rac_dwn(ThF:ThL,i+1) - SPixel%RTM%LW%Rac_dwn(ThF:ThL,i)
        delta_Rbc_up  = &
        SPixel%RTM%LW%Rbc_up(ThF:ThL,i+1) - SPixel%RTM%LW%Rbc_up(ThF:ThL,i)

!       Gradients of radiances w.r.t. pressure (around Pc)
!        write(*,*) 'spline9'
        do k = ThF,ThL
!           write(*,*) 'spline10 ',k
            RTM_Pc%LW%dRac_up_dPc(k) =&
                 & (delta_Rac_up(k) / delta_p) -&
                 & (k0 * d2Rac_up_dP2(k,i)) + (k1 * d2Rac_up_dP2(k,i+1)) 
            RTM_Pc%LW%dRac_dwn_dPc(k) =&
                 & (delta_Rac_dwn(k) / delta_p) -&
                 & (k0 * d2Rac_dwn_dP2(k,i)) + (k1 * d2Rac_dwn_dP2(k,i+1))
            RTM_Pc%LW%dRbc_up_dPc(k) =&
                 (delta_Rbc_up(k) / delta_p) -&
                 (k0 * d2Rbc_up_dP2(k,i)) + (k1 * d2Rbc_up_dP2(k,i+1))
        enddo
!        write(*,*) 'spline11'
!       Change in temperature between RTM levels i and i+1
        delta_T = SPixel%RTM%LW%T(i+1) - SPixel%RTM%LW%T(i)
!        write(*,*) 'spline12'
!       Change in GPH between RTM levels i and i+1

        delta_H = SPixel%RTM%LW%H(i+1) - SPixel%RTM%LW%H(i)

!       Gradient of Planck functions w.r.t. pressure (around Pc)

        dT_dPc = (delta_T / delta_p) - (k0 * d2T_dP2(i)) + (k1 * d2T_dP2(i+1))
        RTM_Pc%dTc_dPc = dT_dPc 

!       Gradient of delta GPH w.r.t. pressure (around Pc)

        dH_dPc = (delta_H / delta_p) - (k0 * d2H_dP2(i)) + (k1 * d2H_dP2(i+1))
        RTM_Pc%dHc_dPc = dH_dPc

!        write(*,*) 'spline13'

!       Interpolated transmittances
!       (Sign conventions same as for delta_p. If Pc is outwith the RTM pressure
!       levels then extrapolation takes place using the same equations as for
!       interpolation. Note: The sign of delta_Pc will change for Pc greater than
!       the pressure of the lowest altitude RTM pressure level)

        k0 = (((dP*dP*dP)-dP) * (delta_p*delta_p))/6.0
        k1 = (((p1*p1*p1)-p1) * (delta_p*delta_p))/6.0

        delta_Pc = Pc - SPixel%RTM%LW%p(i) ! Diff. between Pc and lower RTM level

!        write(*,*) 'spline14'

!       Diff. in trans from gradient

        do k = ThF,ThL
!           write(*,*) 'spline15 ',k
            RTM_Pc%LW%Tac(k)  = (dP * SPixel%RTM%LW%Tac(k,i)) + &
                                (p1 * SPixel%RTM%LW%Tac(k,i+1)) + &
                                (k0 * d2Tac_dP2(k,i)) + (k1 * d2Tac_dP2(k,i+1))
        enddo



!       Diff. in trans from gradient
!        write(*,*) 'spline16'
        do k = ThF,ThL
!           write(*,*) 'spline17 ',k
            RTM_Pc%LW%Tbc(k)  = (dP * SPixel%RTM%LW%Tbc(k,i)) + &
                                (p1 * SPixel%RTM%LW%Tbc(k,i+1)) + &
                                (k0 * d2Tbc_dP2(k,i)) + (k1 * d2Tbc_dP2(k,i+1))
        enddo

!       Interpolated radiances
!        write(*,*) 'spline18'
        do k = ThF,ThL
!           write(*,*) 'spline19 ',k
            RTM_Pc%LW%Rac_up(k)  = (dP * SPixel%RTM%LW%Rac_up(k,i)) + &
                                   (p1 * SPixel%RTM%LW%Rac_up(k,i+1)) + &
                                   (k0 * d2Rac_up_dP2(k,i)) + (k1 * d2Rac_up_dP2(k,i+1))
        enddo

!        write(*,*) 'spline20'
        do k = ThF,ThL
!           write(*,*) 'spline21 ',k
            RTM_Pc%LW%Rac_dwn(k)  = (dP * SPixel%RTM%LW%Rac_dwn(k,i)) + &
                                    (p1 * SPixel%RTM%LW%Rac_dwn(k,i+1)) + &
                                    (k0 * d2Rac_dwn_dP2(k,i)) + (k1 * d2Rac_dwn_dP2(k,i+1))
        enddo
!        write(*,*) 'spline22'
        do k = ThF,ThL
!           write(*,*) 'spline23 ',k
            RTM_Pc%LW%Rbc_up(k)  = (dP * SPixel%RTM%LW%Rbc_up(k,i)) + &
                                   (p1 * SPixel%RTM%LW%Rbc_up(k,i+1)) + &
                                   (k0 * d2Rbc_up_dP2(k,i)) + (k1 * d2Rbc_up_dP2(k,i+1))
        enddo

!       Interpolated Planck functions: calculate T and convert to B using T2R
!       delta_T here is the step from the next lowest level to the current Pc,
!       unlike above where it is the step between RTM levels.

!        write(*,*) 'spline24'
        delta_T = delta_Pc * dT_dPc

!       Set up GPH
!        write(*,*) 'spline25'
        delta_H = delta_Pc * dH_dPc

!       Set current temperature RTM_Pc%Tc and calculate equivalent radiance.
!       (T2R requires T to be an array).
!        write(*,*) 'spline26'
        RTM_Pc%Tc = (dP * SPixel%RTM%LW%T(i)) + &
                    (p1 * SPixel%RTM%LW%T(i+1)) + &
                    (k0 * d2T_dP2(i)) + (k1 * d2T_dP2(i+1))
        T = RTM_Pc%Tc
!        write(*,*) 'spline27'
        call T2R(SPixel%Ind%Nthermal, SAD_Chan, T, RTM_Pc%LW%B(ThF:ThL), dB_dT, status)
        RTM_Pc%LW%dB_dPc(ThF:ThL) = dT_dPc * dB_dT

!       Set current GPH and save the rate of change w.r.t. Pc for use later
!        write(*,*) 'spline28'
        RTM_Pc%Hc = (dP * SPixel%RTM%LW%H(i)) + &
                    (p1 * SPixel%RTM%LW%H(i+1)) + &
                    (k0 * d2H_dP2(i)) + (k1 * d2H_dP2(i+1))
        H = RTM_Pc%Hc
        RTM_Pc%dHc_dPc = dH_dPc


    end if

!write(*,*) 'begin'
!write(*,*) RTM_Pc%LW%Tac(ThF:ThL), RTM_Pc%LW%Tbc(ThF:ThL), &
!          RTM_Pc%LW%Rac_up(ThF:ThL), RTM_Pc%LW%Rac_dwn(ThF:ThL), &
!          RTM_Pc%LW%Rbc_up(ThF:ThL), RTM_Pc%LW%dB_dPc(ThF:ThL), RTM_Pc%dHc_dPc
!write(*,*) 'end'

!  Open breakpoint file if required, and write our reflectances and gradients.

#ifdef BKP
   if (Ctrl%Bkpl >= BkpL_Interpol_Thermal) then
      call Find_Lun(bkp_lun)
      open(unit=bkp_lun,      & 
           file=Ctrl%FID%Bkp, &
           status='old',      &
           position='append', &
           iostat=ios)
      if (ios /= 0) then
         status = BkpFileOpenErr
         call Write_Log(Ctrl, 'Interpol_Thermal_spline: Error opening breakpoint file', status)
      else
         write(bkp_lun,'(/,a,/)')'Interpol_Thermal_spline:'
      end if

      do i=ThF,ThL
         write(bkp_lun,'(a,i2,5(a,f9.4))') 'Channel index: ', i, &
            ' Tac: ', RTM_Pc%LW%Tac(i), ' Tbc: ', RTM_Pc%LW%Tbc(i), &
            ' Rac up: ', RTM_Pc%LW%Rac_up(i), ' Rac dwn: ', RTM_Pc%LW%Rac_dwn(i),&
            ' Rbc up: ', RTM_Pc%LW%Rbc_up(i)
      end do

      write(bkp_lun, '(a,/)') 'Interpol_Thermal_spline: end'
      close(unit=bkp_lun)
   end if
#endif

end subroutine Interpol_Thermal_spline
