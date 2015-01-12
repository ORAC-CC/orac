!-------------------------------------------------------------------------------
! Name:
!    FM_Thermal
!
! Description:
!    Radiance forward model (thermal channels) for a defined pixel and pressure
!    level.
!
! Arguments:
!    Name     Type	 In/Out/Both  Description
!    Ctrl     struct	 In	      Control structure
!    SAD_LUT  struct	 In	      SAD look up table
!    SAD_Chan array of structs
!                        In           SAD channel structures: only the
!             			      thermal channel part of the array is
!             			      passed in.
!    SPixel   struct	 Both         Super-pixel structure
!    RTM_Pc   struct	 In	      Contains transmittances, radiances
!             			      and their derivatives
!    X        real array In	      State vector
!    CRP      real array Both         Cloud LW radiative properties (no. of
!                                     channels by no of cloud radiative
!                                     properties - note not all CRPs are
!                                     interpolated here: only Td, Rd and Em).
!                                     This array could be made smaller (3 in 2nd
!                                     dimension) but this should not affect
!                                     speed and SetCRPThermal would also have to
!                                     be changed. Note also only the thermal
!                                     channels should be passed into this
!                                     routine, although the full array used
!                                     higher up the call tree contains all
!                                     channels.
!    d_CRP    real array Both         LW CRP gradients in Tau, Re
!    BT       real array Out	      Part cloudy brightness temperatures
!    d_BT     real array Out	      Gradients in part cloudy brightness temps.
!    R        real array Out	      Part cloudy radiance calculated at Pc
!    d_R      real array Out	      Part cloudy radiance gradients calculated
!                                     at Pc.
!    status   int	 Out	      Error status
!
! Algorithm:
!   Update clear radiances at each RTM pressure level (Ts = Ts_0 + delta_Ts).
!   Set up long-wave cloud properties.
!   Update the below cloud upward radiance (this can be done at Pc only to save
!      time)
!   Calculate overcast and part-cloudy radiances.
!   Calculate part-cloudy radiance gradients w.r.t. each of the state variables.
!   Convert the radiances and radiance gradients to brightness temperatures.
!
! Local variables:
!    Name Type Description
!
! History:
!    16th November, 2000, Kevin M. Smith : original version
!    21st November, 2000, KMS : corrections
!    24th November, 2000, KMS : brought calculation of d_T up from R2T
!    19th Jan 2001, Andy Smith:
!       Use FM_Routines_def: contains interface definition for SetCRPThermal
!     9th Feb 2001, Andy Smith:
!       Updating to match recent changes in arguments and change in call
!       sequence. Using constants to select parts of BT and d_BT arrays
!       referring to different state variables.
!    20th Feb 2001, Andy Smith:
!       CRP, d_CRP are required as arguments. Passed to solar routine later to
!       populate the remaining channels.
!     1st Mar 2001, Andy Smith:
!       delta_Ts made local.
!     8th Mar 2001, Andy Smith:
!       dB_dTs argument removed. Now part of SPixel.
!    15th Mar 2001, Andy Smith:
!       Added ThF and ThL indices for RTM_Pc%LW arrays. Required because these
!       arrays are allocated to match the no. of thermal channels requested, but
!       in twilight not all of the requested thermal channels may be used.
!    22nd Mar 2001, Andy Smith:
!       Replaced pressure level index in RTM Tac value in clear radiance
!       calculation. Was Np, now 1.
!    20th Jul 2001, Andy Smith:
!       Fixed setting of ThL. Last thermal channel should always be the one
!       specified in Ctrl, not SPixel, since the RTM_Pc and RTM arrays are of
!       fixed size for the whole run, i.e are not re-allocated to match the no.
!       of thermal channels used in each SPixel, and it is the lower numbered
!       thermal channels that are not used in certain conditions.
!    20th Dec 2014, Greg McGarragh:
!       Cleaned up code.
!    24th Dec 2014, Greg McGarragh:
!       Some intent changes.
!     9th Jan 2015, Adam Povey:
!       Replacing ThF:ThL with SPixel index array. Eliminate RTM_Pc%Tac, Tbc.
!    12th Jan 2015, Adam Povey:
!       Remove CRP arguments.
!
! Bugs:
!   None known.
!
! $Id$
!
!-------------------------------------------------------------------------------

subroutine FM_Thermal(Ctrl, SAD_LUT, SPixel, SAD_Chan, RTM_Pc, X, GZero, &
                      BT, d_BT, R, d_R, status)

   use CTRL_def
   use ECP_Constants
   use GZero_def
   use RTM_Pc_def
   use SAD_Chan_def
   use SAD_LUT_def
   use SPixel_def

   implicit none

   ! Define arguments

   type(CTRL_t),     intent(in)    :: Ctrl
   type(SAD_LUT_t),  intent(in)    :: SAD_LUT
   type(SPixel_t),   intent(in)    :: SPixel
   type(SAD_Chan_t), intent(in)    :: SAD_Chan(SPixel%Ind%Nthermal)
   type(RTM_Pc_t),   intent(inout) :: RTM_Pc
   real,             intent(in)    :: X(MaxStateVar)
   type(GZero_t),    intent(in)    :: GZero
   real,             intent(out)   :: BT(SPixel%Ind%Nthermal)
   real,             intent(out)   :: d_BT(SPixel%Ind%Nthermal, MaxStateVar)
   real,             intent(out)   :: R(SPixel%Ind%Nthermal)
   real,             intent(out)   :: d_R(SPixel%Ind%Nthermal, MaxStateVar)
   integer,          intent(out)   :: status

   ! Define local variables

   real    :: CRP(SPixel%Ind%Nthermal, MaxCRProps)
   real    :: d_CRP(SPixel%Ind%Nthermal, MaxCRProps, 2)
   integer :: i
   integer :: Thermal(SPixel%Ind%NThermal)
   real    :: delta_Ts
   real    :: R_clear(SPixel%Ind%Nthermal)
   real    :: R_over(SPixel%Ind%Nthermal)
   real    :: Es_dB_dTs(SPixel%Ind%Nthermal)
   real    :: fTac(SPixel%Ind%Nthermal)
   real    :: dT_dR(SPixel%Ind%Nthermal)
#ifdef BKP
   integer :: j       ! For breakpoint output loops
   integer :: bkp_lun ! Unit number for breakpoint file
   integer :: ios     ! I/O status for breakpoint file
#endif
   status = 0

   ! Use ThF and ThL to access the first and last required thermal channels from
   ! Subscripts for thermal channels in RTM arrays
   Thermal = SPixel%spixel_y_thermal_to_ctrl_y_thermal_index(:SPixel%Ind%NThermal)

   ! Calculate delta_Ts
   delta_Ts = X(ITs) - SPixel%RTM%LW%T(SPixel%RTM%LW%Np)

   ! Update clear radiances at each RTM pressure level

   R_clear = SPixel%RTM%LW%R_clear(Thermal) + &
      (delta_Ts * SPixel%RTM%LW%dB_dTs(Thermal) * SPixel%RTM%LW%Ems(Thermal) * &
      SPixel%RTM%LW%Tac(Thermal,1))

   ! Set up the LW cloud radiative properties
   call Set_CRP_Thermal(Ctrl, SPixel%Ind, GZero, SAD_LUT, CRP, d_CRP, status)

   ! Calculate product dB_dTs * SPixel%RTM%LW%Ems (for efficiency)
   Es_dB_dTs = SPixel%RTM%LW%dB_dTs(Thermal) * SPixel%RTM%LW%Ems(Thermal)

   ! Update below cloud radiance after interpolation to Pc
   RTM_Pc%LW%Rbc_up(Thermal) = RTM_Pc%LW%Rbc_up(Thermal) + &
      (delta_Ts * Es_dB_dTs * RTM_Pc%LW%Tbc)

   ! Calculate overcast radiances at cloud pressure level
   R_over = (RTM_Pc%LW%Rbc_up(Thermal)  * CRP(:,ITd)  + &
             RTM_Pc%LW%B(Thermal)       * CRP(:,IEm)  + &
             RTM_Pc%LW%Rac_dwn(Thermal) * CRP(:,IRd)) * RTM_Pc%LW%Tac + &
            RTM_Pc%LW%Rac_up(Thermal)

   ! Calculate part cloudy radiances (a linear combination of R_clear and R_over)

   R = (X(IFr) * R_over) + ((1.0 - X(IFr)) * R_clear)

   ! Calculate product X%frac*Tac (for efficiency)
   fTac = X(IFr) * RTM_Pc%LW%Tac

   ! Calculate radiance gradients

   ! Gradient w.r.t. cloud optical depth, tau
   d_R(:,ITau) = fTac * (RTM_Pc%LW%Rbc_up(Thermal)  * d_CRP(:,ITd,Itau) + &
                         RTM_Pc%LW%B(Thermal)       * d_CRP(:,IEm,Itau) + &
                         RTM_Pc%LW%Rac_dwn(Thermal) * d_CRP(:,IRd,Itau))

   ! Gradient w.r.t. effective radius, re
   d_R(:,IRe) = fTac * (RTM_Pc%LW%Rbc_up(Thermal)  * d_CRP(:,ITd,Ire) + &
                        RTM_Pc%LW%B(Thermal)       * d_CRP(:,IEm,Ire) + &
                        RTM_Pc%LW%Rac_dwn(Thermal) * d_CRP(:,IRd,Ire))

   ! Gradient w.r.t. cloud pressure, Pc
   d_R(:,IPc) = X(IFr) * ( RTM_Pc%LW%dTac_dPc(Thermal) *                  &
                           ( RTM_Pc%LW%Rbc_up(Thermal)  * CRP(:,ITd) +   &
                             RTM_Pc%LW%B(Thermal)       * CRP(:,IEm) +   &
                             RTM_Pc%LW%Rac_dwn(Thermal) * CRP(:,IRd))  &
                         + RTM_Pc%LW%dRac_up_dPc(Thermal))               &
              + fTac * ( RTM_Pc%LW%dRbc_up_dPc(Thermal)  * CRP(:,ITd) + &
                         RTM_Pc%LW%dB_dPc(Thermal)       * CRP(:,IEm) + &
                         RTM_Pc%LW%dRac_dwn_dPc(Thermal) * CRP(:,IRd))

   ! Gradient w.r.t. cloud fraction, f
   d_R(:,IFr) = R_over - R_clear

   ! Gradient w.r.t. surface temperature, Ts
   d_R(:,ITs) = fTac * Es_dB_dTs * RTM_Pc%LW%Tbc * CRP(:,ITd) + &
      (1.0 - X(IFr)) * Es_dB_dTs * SPixel%RTM%LW%Tac(Thermal,1)

   ! Convert radiances to brightness temperatures
   call R2T(SPixel%Ind%Nthermal, SAD_Chan, R, BT, dT_dR, status)

   ! Calculate the change in brightness temperatures w.r.t. state parameters
   ! using dBT_dX = dT_dR * dR_dX (loop through each state parameter)
   do i = 1, MaxStateVar
      d_BT(:,i) = dT_dR * d_R(:,i)
   end do

   ! Open breakpoint file if required, and write our reflectances and gradients.
#ifdef BKP
   if (Ctrl%Bkpl >= BkpL_FM_Thermal) then
      call Find_Lun(bkp_lun)
      open(unit=bkp_lun,      &
           file=Ctrl%FID%Bkp, &
	   status='old',      &
	   position='append', &
	   iostat=ios)
      if (ios /= 0) then
         write(*,*) 'ERROR: FM_Thermal(): Error opening breakpoint file'
         stop BkpFileOpenErr
      else
         write(bkp_lun,'(/,a)')'FM_Thermal:'
      end if
      write(bkp_lun,'(/)')

      write(bkp_lun,'(a,f9.4)') 'delta_Ts: ',delta_Ts
      write(bkp_lun,'(a)') 'SPixel Lw RTM contributions to R_Clear:'
      do i=1,SPixel%Ind%NThermal
         write(bkp_lun,'(a,i2,3(a,f9.4))') 'Channel index: ', i, &
	    ' R_Clear: ', SPixel%RTM%LW%R_Clear(Thermal(i)), &
	    ' dB_dTs : ', SPixel%RTM%LW%dB_dTs(Thermal(i)), &
	    ' Ems: ', SPixel%RTM%LW%Ems(Thermal(i))
      end do

      do i=1,SPixel%Ind%NThermal
         write(bkp_lun,'(a,i2,3(a,f9.4))') 'Channel index: ', i, &
	    ' CRP Td: ', CRP(i,ITd), '  CRP Em: ', CRP(i,IEm), &
	    '  CRP Rd: ', CRP(i,IRd)
      end do

      do i=1,SPixel%Ind%NThermal
         write(bkp_lun,'(a,i2,3(a,f9.4))') 'Channel index: ', i, &
	    ' R_Clear: ', R_Clear(i), '  Rbc_up: ', RTM_Pc%LW%Rbc_up(Thermal(i)), &
	    '  R_Over: ',R_Over(i)
      end do
      write(bkp_lun,'(/)')

      do i=1,SPixel%Ind%NThermal
      	 write(bkp_lun,'(a,i2,a,2f9.4)') 'Channel index: ', i, &
	    ' BT, R: ', BT(i), R(i)
      end do
      write(bkp_lun,'(/)')

      do i=1,SPixel%Ind%NThermal
      	 write(bkp_lun,'(a,i2,a,5f10.3)') 'Channel index: ', i, &
	    ' dBT: ', (d_BT(i,j),j=1,MaxStateVar)
	 write(bkp_lun,'(a,5f10.3)') &
	    '                   dR: ',  (d_R(i,j),j=1,MaxStateVar)
      end do
      write(bkp_lun,'(/)')

      write(bkp_lun, '(a,/)') 'FM_Thermal: end'
      close(unit=bkp_lun)
   end if
#endif

end subroutine FM_Thermal
