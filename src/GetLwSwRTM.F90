!-------------------------------------------------------------------------------
! Name:
!    Get_LwSwRTM
!
! Purpose:
!    Performs the bilinear interpolation of the long wave RTM data to the
!    current super pixel coordinates.
!    Also assigns the surface to TOA transmittances for the current super pixel.
!
! Arguments:
!    Name     Type         In/Out/Both Description
!    Ctrl     struct       In          Control structure
!    SAD_Chan struct array In          Channel description structures
!    RTM      alloc struct In          RTM structure
!    SPixel   struct       Both        Super-pixel structure
!    status   integer      Out         Error status
!
! Algorithm:
!    Method taken directly from Numerical Recipes p96, 1987.
!
! Local variables:
!    Name Type Description
!
! History:
!    12th Jan 2001, Kevin M. Smith: Original version.
!    25th Jan 2001, Kevin M. Smith:
!       Added checks to see if super pixel lat,lon are covered by the RTM
!       ranges. Simplified calculation of XN and YN
!    21st Feb 2001, Andy Smith:
!       Added Tbc to LW structure. Previously missing from model data.
!       Tsf removed from LW structure.
!     1st Mar 2001, Andy Smith:
!       Changed setting of R_Clear values. These are not present in the
!       RTM struct as they are not read from the RTM data file.
!     6th Mar 2001, Andy Smith:
!       Tsf required in RTM struct again.
!     8th Mar 2001, Andy Smith:
!       Now sets dB_dTs in LW sub-struct.
!       Added SAD_Chan argument (required by T2R for dB_dTs calculation).
!    30th Mar 2001, Andy Smith:
!       Using whole array operations in the interpolations across all pressure
!       levels and channels to improve performance.
!       Bs calculation removed. Bs is not used by subsequent code.
!    20th Jul 2001, Andy Smith:
!       dB_dTs calculation fixed: was using no. of thermal channels from SPixel,
!       should have used Ctrl values as the RTM Lw arrays are all allocated to
!       size Ctrl%Ind%NThermal and left at this size for the whole image.
!    **************** ECV work starts here *************************************
!    21st Feb 2011, Andy Smith:
!       Re-introducing changes made in late 2001/2002.
!    12th Dec 2003, Caroline Poulsen:
!       Added geopotential height.
!    22nd Sep 2011, Caroline Poulsen:
!       Added in swrtm variables.
!     7th Nov 2011, Caroline Poulsen:
!       Tidied up comments. no actual change.
!    15/09/2012, M. Stengel: Puts in coefs fudge to account for missing values.
!    03/11/2012, MJ: Changed Coeffs if block.
!    23/07/2014, AP: Grid no longer assumed to defined points rather than the
!       cells centres (as is actually the case).
!    23/07/2014, CP: Added in extrapolation into stratosphere.
!    30/07/2014, GM: Cleaned up the code.
!    30/07/2014, GM: Put the duplicate interpolation code into subroutines.
!
! Bugs:
!    None known.
!
! $Id$
!
!-------------------------------------------------------------------------------

module Get_LwSwRTM_m

   implicit none

   private

   public :: Get_LwSwRTM

contains

subroutine Get_LwSwRTM(Ctrl, SAD_Chan, RTM, SPixel, status)

   use Ctrl_def
   use ECP_Constants
   use RTM_def
   use SAD_Chan_def
   use SPixel_def

   implicit none

   ! Declare arguments

   type(Ctrl_t),     intent(in)    :: Ctrl
   type(SAD_Chan_t), intent(in)    :: SAD_Chan(Ctrl%Ind%Ny)
   type(RTM_t),      intent(in)    :: RTM
   type(SPixel_t),   intent(inout) :: SPixel
   integer,          intent(out)   :: status

   ! Declare local variables

   real           :: X
   integer        :: Nx
   integer        :: Nx1
   real           :: XN
   real           :: d_X
   real           :: t
   real           :: Y
   integer        :: Ny
   integer        :: Ny1
   real           :: YN
   real           :: d_Y
   real           :: u
   real           :: coef(4)
   real           :: R(Ctrl%Ind%NThermal)
   real           :: T_Array(Ctrl%Ind%NThermal)
   character(180) :: message

   ! Set status to zero
   status = 0

   ! Bilinear interpolation (method taken from Numerical Recipes p96, 1987)

   ! Latitude
   X   = SPixel%Loc%Lat - RTM%LW%Grid%Lat0              ! Latitude relative to grid origin
   Nx  = 1 + int( X * RTM%LW%Grid%inv_delta_Lat + 0.5 ) ! Integer number of grid points from origin to X
   XN  = RTM%LW%Lat(Nx,1)                               ! Latitude at Nx grid points
   d_X = SPixel%Loc%Lat - XN                            ! Latitude relative to XN
   t   = d_X * RTM%LW%Grid%inv_delta_Lat                ! Ratio of d_X and latitude grid spacing

   ! Longitude
   Y   = SPixel%Loc%Lon - RTM%LW%Grid%Lon0              ! Longitude relative to grid origin
   Ny  = 1 + int( Y * RTM%LW%Grid%inv_delta_Lon + 0.5 ) ! Integer number of grid points from origin to Y
   YN  = RTM%LW%Lon(Nx,Ny)                              ! Longitude at Ny grid points
   d_Y = SPixel%Loc%Lon - YN                            ! Longitude relative to YN
   u   = d_Y * RTM%LW%Grid%inv_delta_Lon                ! Ratio of d_Y and longitude grid spacing

   ! Calculate coordinates used in all interpolation
   Nx1 = Nx + 1
   Ny1 = Ny + 1


   ! Check that required coordinates are within range (skip super pixel if not)
   if (Nx1 > RTM%LW%Grid%NLat) then
      if (Nx == RTM%LW%Grid%NLat) then
         ! if in last grid cell, extrapolate
         Nx1 = Nx
         Nx = Nx-1
      else
         status = GetLwSwRTMLat
         write(unit=message, fmt=*) 'Get_LwSwRTM: Latitude outside RTM coverage in super pixel starting at:', &
              SPixel%Loc%X0, SPixel%Loc%Y0
         call Write_log(Ctrl, trim(message), status)
      end if
   end if

   if (Ny1 > RTM%LW%Grid%NLon) then
      if (Ny == RTM%LW%Grid%NLon) then
         ! if in last grid cell, extrapolate
         Ny1 = Ny
         Ny = Ny-1
      else
         status = GetLwSwRTMLon
         write(unit=message, fmt=*) 'Get_LwSwRTM: Longitude outside RTM coverage in super pixel starting at:', &
              SPixel%Loc%X0, SPixel%Loc%Y0
         call Write_log(Ctrl, trim(message), status)
      end if
   end if

   if (status == 0) then
      ! Calculate coeficients used in all interpolations
      coef(1) = (1-t) * (1-u)
      coef(2) =    t  * (1-u)
      coef(3) =    t  *    u
      coef(4) = (1-t) *    u

      ! Parameters that are dependent on channel only
      if (RTM%LW%Rac_up(Nx,  Ny,  1, 1) .lt. 0.) coef(1)=0.
      if (RTM%LW%Rac_up(Nx1, Ny,  1, 1) .lt. 0.) coef(2)=0.
      if (RTM%LW%Rac_up(Nx1, Ny1, 1, 1) .lt. 0.) coef(3)=0.
      if (RTM%LW%Rac_up(Nx,  Ny1, 1, 1) .lt. 0.) coef(4)=0.

      ! Rescale the coefs to 1.
      coef(1:4)=coef(1:4)*1./sum(coef(1:4))


      ! LW

      ! P
      call interp_field_1d(RTM%LW%P, SPixel%RTM%LW%P, Nx, Nx1, Ny, Ny1, coef)

      ! T
      call interp_field_1d(RTM%LW%T, SPixel%RTM%LW%T, Nx, Nx1, Ny, Ny1, coef)

      ! H
      call interp_field_1d(RTM%LW%H, SPixel%RTM%LW%H, Nx, Nx1, Ny, Ny1, coef)

      ! skint
      call interp_field_0d(RTM%LW%skint, SPixel%RTM%LW%skint, Nx, Nx1, Ny, Ny1, coef)

      ! sp
      call interp_field_0d(RTM%LW%sp, SPixel%RTM%LW%sp, Nx, Nx1, Ny, Ny1, coef)

      ! LW Ems
      call interp_field_1d(RTM%LW%Ems, SPixel%RTM%LW%Ems, Nx, Nx1, Ny, Ny1, coef)

      ! LW Tbc
      call interp_field_2d(RTM%LW%Tbc, SPixel%RTM%LW%Tbc, Nx, Nx1, Ny, Ny1, coef)

      ! LW Tac
      call interp_field_2d(RTM%LW%Tac, SPixel%RTM%LW%Tac, Nx, Nx1, Ny, Ny1, coef)

      ! Rac_up
      call interp_field_2d(RTM%LW%Rac_up, SPixel%RTM%LW%Rac_up, Nx, Nx1, Ny, Ny1, coef)

      ! Rac_dwn
      call interp_field_2d(RTM%LW%Rac_dwn, SPixel%RTM%LW%Rac_dwn, Nx, Nx1, Ny, Ny1, coef)

      ! Rbc_up
      call interp_field_2d(RTM%LW%Rbc_up, SPixel%RTM%LW%Rbc_up, Nx, Nx1, Ny, Ny1, coef)

      ! Set surface level to TOA transmittances
      SPixel%RTM%LW%Tsf = SPixel%RTM%LW%Tac(:,RTM%LW%Np)

      ! Set R_Clear using Rbc_up at the TOA
      SPixel%RTM%LW%R_clear = SPixel%RTM%LW%Rbc_up(:,1)


      ! SW

      ! P
      call interp_field_1d(RTM%LW%P, SPixel%RTM%SW%P, Nx, Nx1, Ny, Ny1, coef)

      ! SW Tbc
      call interp_field_2d(RTM%SW%Tbc, SPixel%RTM%SW%Tbc, Nx, Nx1, Ny, Ny1, coef)

      ! SW Tac
      call interp_field_2d(RTM%SW%Tac, SPixel%RTM%SW%Tac, Nx, Nx1, Ny, Ny1, coef)

      ! Set surface level to TOA transmittances
      SPixel%RTM%SW%Tsf = SPixel%RTM%SW%Tac(:,RTM%SW%Np)


      ! Modify profile in boundary layer inversion not implemented yet
!     call Blmodification(SPixel)

      ! Extrapolate temperature profile into stratosphere to deal with deep
      ! convective clouds primarily in tropics that push through the trop.
      call extrap_into_tropopause(SPixel)


      ! Set dB_dTs using the surface temperature. (T2R needs an array of T values,
      ! one per channel, to convert).

      T_Array = SPixel%RTM%LW%T(SPixel%RTM%LW%Np)
      call T2R (Ctrl%Ind%NThermal, &
         SAD_Chan(Ctrl%Ind%ThermalFirst:Ctrl%Ind%ThermalLast), &
         T_Array, R, SPixel%RTM%LW%dB_dTs, status)
   end if

end subroutine Get_LwSwRTM


subroutine interp_field_0d(in, out, Nx, Nx1, Ny, Ny1, coef)

   implicit none

   real,    intent(in)    :: in(:,:)
   real,    intent(inout) :: out
   integer, intent(in)    :: Nx
   integer, intent(in)    :: Nx1
   integer, intent(in)    :: Ny
   integer, intent(in)    :: Ny1
   real,    intent(in)    :: coef(:)

   out = (coef(1) * in(Nx,  Ny)) + &
         (coef(2) * in(Nx1, Ny)) + &
         (coef(3) * in(Nx1, Ny1)) + &
         (coef(4) * in(Nx,  Ny1))

end subroutine interp_field_0d


subroutine interp_field_1d(in, out, Nx, Nx1, Ny, Ny1, coef)

   implicit none

   real,    intent(in)    :: in(:,:,:)
   real,    intent(inout) :: out(:)
   integer, intent(in)    :: Nx
   integer, intent(in)    :: Nx1
   integer, intent(in)    :: Ny
   integer, intent(in)    :: Ny1
   real,    intent(in)    :: coef(:)

   out = (coef(1) * in(Nx,  Ny,  :)) + &
         (coef(2) * in(Nx1, Ny,  :)) + &
         (coef(3) * in(Nx1, Ny1, :)) + &
         (coef(4) * in(Nx,  Ny1, :))

end subroutine interp_field_1d


subroutine interp_field_2d(in, out, Nx, Nx1, Ny, Ny1, coef)

   implicit none

   real,    intent(in)    :: in(:,:,:,:)
   real,    intent(inout) :: out(:,:)
   integer, intent(in)    :: Nx
   integer, intent(in)    :: Nx1
   integer, intent(in)    :: Ny
   integer, intent(in)    :: Ny1
   real,    intent(in)    :: coef(:)

   out = (coef(1) * in(Nx,  Ny,  :, :)) + &
         (coef(2) * in(Nx1, Ny,  :, :)) + &
         (coef(3) * in(Nx1, Ny1, :, :)) + &
         (coef(4) * in(Nx,  Ny1, :, :))

end subroutine interp_field_2d

end module Get_LwSwRTM_m
