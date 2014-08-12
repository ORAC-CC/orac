!-------------------------------------------------------------------------------
! Name:
!    Interpol_Solar_spline
!
! Purpose:
!    Interpolates SW RTM transmittances onto current cloud pressure level
!    NOTE: It is assumed that the pressure levels are ordered in decreasing
!    pressure (increasing altitude).
!
! Arguments:
!    Name   Type   In/Out/Both Description
!    Ctrl   struct In          Control structure
!    SPixel struct Both        Super-pixel structure
!    Pc     float  In          Current cloud pressure
!    RTM_Pc struct Out         Contains Tac, Tbc (interpolated transmittances
!                              above and below cloud) and gradients wrt cloud
!                              pressure.
!    status int    Out         Standard status value not set here
!
! Algorithm:
!    Check to see if Pc is outwith the RTM pressure level range
!    (Warn if extrapolation beyond 50 hPa past the first/last RTM level is
!     necessary)
!       If not, search sequentially through the RTM pressure levels to find the
!          pair that are bounding Pc
!       Return an error if none of these conditions are satisfied (e.g. Pc=NaN)
!    Determine the change in p and trans. between bounding RTM levels
!    Determine transmittance gradients (d_T_d_p) around Pc
!    Calculate change in trans. between RTM level i and Pc
!    Calculate absolute trans. at Pc
!
! Local variables:
!    Name Type Description
!
! History:
!    15th Nov 2000, Kevin M. Smith: Original version
!     1st Feb 2001, Andy Smith:
!       Pressure level indices reversed in SPixel RTM arrays, i.e. values now
!       increase with index.
!     9th Feb 2001, Andy Smith:
!       First fully completed and (informally) tested version.
!    15th Feb 2001, Andy Smith:
!       Channel ranges used for array size setting and indexing now picked up
!       from SPixel.
!    16th Feb 2001, Andy Smith:
!       Checked through Pressure level tests and updated comments. Pressure
!       levels were originally intended to be stored from highest pressure to
!       lowest (i.e. ground upwards), but are now in order of pressure.
!    20th Feb 2001, Andy Smith:
!       Removing excess underscores from RTM_Pc variable names.
!    26th Feb 2001, Andy Smith:
!       Changed test for P levels bounding Pc. Using
!       (Pc >= p(j) and Pc <  p(j+1)) instead of
!       (Pc >  p(j) and Pc <= p(j+1)) because P levels now go in order of
!       increasing pressure. Previously, if Pc was equal to P(1) it was not
!       matched (and if Pc < P(1) it's P index is set to 1!)
!     5th Sep 2011, Chris Arnold:
!       Added d2X_dP2 variables s/t a cubic spline interpolation can be used.
!       The second derivatives of the tabulated RTM data are calculated using the
!       routine 'spline'. These are used to calculate the interpolates/
!       interpolated gradients using the cubic spline method described
!       in 'Numerical Recipes for fortran 90' [Press, Flannery, Teukolsky,
!       Vetterling]
!     6th Feb 2012, Chris Arnold:
!       Explicit indexing when spline routine is called ie call
!          spline(SPixel%RTM%SW%P(1:SPixel%RTM%SW%Np), &
!                 SPixel%RTM%SW%Tac(k,1:SPixel%RTM%SW%Np), &
!                 d2Tac_dP2(k,:))
! 	replaced with -> call
!          spline(SPixel%RTM%SW%P(1:SPixel%RTM%SW%Np), &
!                 SPixel%RTM%SW%Tbc(k,1:SPixel%RTM%SW%Np), &
!                 d2Tbc_dP2(k,1:SPixel%RTM%SW%Np))
!     8th Jul 2014, Caroline Poulsen:
!       Small bug fix to BKP readout.
!     5th Aug 2014, Greg McGarragh:
!       Cleaned up the code.
!     5th Aug 2014, Greg McGarragh:
!       Put Interpol_* common code into subroutine find_Pc().
!
! Bugs:
!    None known.
!
! $Id$
!
!-------------------------------------------------------------------------------

subroutine Interpol_Solar_spline(Ctrl, SPixel, Pc, RTM_Pc, status)

   use CTRL_def
   use ECP_Constants
   use Int_Routines_def
   use RTM_Pc_def
   use SPixel_def

   implicit none

   ! Define arguments

   type(CTRL_t),   intent(in)    :: Ctrl
   type(SPixel_t), intent(in)    :: SPixel
   real,           intent(in)    :: Pc
   type(RTM_Pc_t), intent(inout) :: RTM_Pc
   integer,        intent(inout) :: status

   ! Define local variables

   integer :: i
   integer :: j
   real    :: dP ! Fractional distance of Pc from bottom of interval
   real    :: P1 ! Fractional distance of Pc from top of interval
   real    :: k0 ! Spline coefficient
   real    :: k1 ! Spline coefficient
   real    :: delta_p
   real    :: delta_Tac(SPixel%Ind%NSolar)
   real    :: delta_Tbc(SPixel%Ind%NSolar)
   real    :: d2Tac_dP2(SPixel%Ind%NSolar,SPixel%RTM%SW%Np)
   real    :: d2Tbc_dP2(SPixel%Ind%NSolar,SPixel%RTM%SW%Np)
   character(ECPLogReclen) :: message
#ifdef BKP
   integer :: bkp_lun ! Unit number for breakpoint file
   integer :: ios     ! I/O status for breakpoint file
#endif

   ! Set initial value of error status equal to zero (i.e. no error)
   status = 0

   ! Search for Pc in the SW RTM pressure levels. If Pc lies outwith the RTM
   ! pressure levels avoid search and set index to 1 or the penultimate RTM level.
   call find_Pc(Ctrl, SPixel%RTM%SW%Np, SPixel%RTM%SW%P, Pc, i, status)

   if (status /= 0) then
      ! If none of the above conditions are met (e.g. Pc = NaN) then return with
      ! a fatal error
      status = IntTransErr ! Set status to indicate failure of interpolation
      write(unit=message, fmt=*) 'ERROR: Interpol_Solar(), Interpolation failure, ', &
         'SPixel starting at: ',SPixel%Loc%X0, SPixel%Loc%Y0, ', P(1), P(Np), Pc: ', &
         SPixel%RTM%SW%P(1), SPixel%RTM%SW%P(SPixel%RTM%SW%Np), Pc
      call Write_Log(Ctrl, trim(message), status) ! Write to log
      stop
   else
      ! Start the interpolation or extrapolation calculations
      ! Note: Implicit looping over instrument channels from here onwards

      do j = 1, SPixel%Ind%NSolar
         call spline(SPixel%RTM%SW%P(1:SPixel%RTM%SW%Np),&
            SPixel%RTM%SW%Tac(j,1:SPixel%RTM%SW%Np),d2Tac_dP2(j,1:SPixel%RTM%SW%Np))
         call spline(SPixel%RTM%SW%P(1:SPixel%RTM%SW%Np),&
             SPixel%RTM%SW%Tbc(j,1:SPixel%RTM%SW%Np),d2Tbc_dP2(j,1:SPixel%RTM%SW%Np))
      end do

      ! Change in pressure between RTM levels i and i+1
      ! (delta_p is negative for decreasing pressure with increasing i)
      delta_p =  SPixel%RTM%SW%P(i+1) - SPixel%RTM%SW%P(i)

      dP = (SPixel%RTM%SW%P(i+1)-Pc)/delta_p
      p1 = 1.0 - dP

      k0 = (((3.0*dP*dP)-1.0)/6.0) * delta_p
      k1 = (((3.0*p1*p1)-1.0)/6.0) * delta_p

      ! Change in transmittances between RTM levels i and i+1
      ! (delta_Tac/bc are positive for increasing trans. with increasing i)
      delta_Tac = SPixel%RTM%SW%Tac(:,i+1) - SPixel%RTM%SW%Tac(:,i)
      delta_Tbc = SPixel%RTM%SW%Tbc(:,i+1) - SPixel%RTM%SW%Tbc(:,i)

      ! Gradients of transmittance w.r.t. pressure (around Pc)
      do j = 1, SPixel%Ind%NSolar
         RTM_Pc%SW%dTac_dPc(j) = (delta_Tac(j) / delta_p) - (k0 * d2Tac_dP2(j,i)) + &
                                 (k1 * d2Tac_dP2(j,i+1))
         RTM_Pc%SW%dTbc_dPc(j) = (delta_Tbc(j) / delta_p) - (k0 * d2Tbc_dP2(j,i)) + &
                                 (k1 * d2Tbc_dP2(j,i+1))
      end do

      ! Interpolated transmittances
      ! (Sign conventions same as for delta_p. If Pc is outwith the RTM pressure
      ! levels then extrapolation takes place using the same equations as for
      ! interpolation. Note: The sign of delta_Pc will change for Pc greater than
      ! the pressure of the lowest altitude RTM pressure level)

      k0 = (((dP*dP*dP)-dP) * (delta_p*delta_p))/6.0
      k1 = (((p1*p1*p1)-p1) * (delta_p*delta_p))/6.0

      do j = 1, SPixel%Ind%NSolar
         RTM_Pc%SW%Tac(j) = (dP * SPixel%RTM%SW%Tac(j,i)) + &
             (p1 * SPixel%RTM%SW%Tac(j,i+1)) + &
             (k0 * d2Tac_dP2(j,i)) + (k1 * d2Tac_dP2(j,i+1))
      end do

      do j = 1, SPixel%Ind%NSolar
         RTM_Pc%SW%Tbc(j) = (dP * SPixel%RTM%SW%Tbc(j,i)) + &
             (p1 * SPixel%RTM%SW%Tbc(j,i+1)) + &
             (k0 * d2Tbc_dP2(j,i)) + (k1 * d2Tbc_dP2(j,i+1))
      end do
   end if

   ! Open breakpoint file if required, and write our transmittances etc.

#ifdef BKP
   if (Ctrl%Bkpl >= BkpL_Interpol_Solar) then
      call Find_Lun(bkp_lun)
      open(unit=bkp_lun,      &
           file=Ctrl%FID%Bkp, &
           status='old',      &
           position='append', &
           iostat=ios)
      if (ios /= 0) then
         status = BkpFileOpenErr
         call Write_Log(Ctrl, 'Interpol_Solar_spline: Error opening breakpoint file', &
            status)
      else
         write(bkp_lun,*)'Interpol_Solar_spline:'
      end if

      write(bkp_lun,'(a)') 'Chan ind  Tac       Tbc       dTac_dPc  dTbc_dPc'
      do i=1, SPixel%Ind%NY - SPixel%Ind%NThermal
         write(bkp_lun,'(5x,i2,4(1x,f9.4))') i, &
            RTM_Pc%SW%Tac(i), RTM_Pc%SW%Tbc(i), RTM_Pc%SW%dTac_dPc(i), &
            RTM_Pc%SW%dTbc_dPc(i)
      end do

      write(bkp_lun, '(a,/)') 'Interpol_Solar_spline: end'
      close(unit=bkp_lun)
   end if
#endif

end subroutine Interpol_Solar_spline
