!-------------------------------------------------------------------------------
! Name: SADChan.F90
!
! Purpose:
! Module containing Static Application Data Channel structure, used to hold
! Static Application Data for measurement channels in the ECP.
!
! History:
! 2000/08/03, AS: Original version
! 2000/11/23, AS: Rs in Solar is now an array of 2 values rather than a single 
!    real.
! 2001/01/19, KS:Added SRs and CRs fields to the Solar type.
! 2001/01/29, KS: Removed SRs and CRs fields - now taken from Ctrl.
! 2001/07/11, AS: Added f1 to solar struct. Allows for calculation of f0 based 
!    on the day of year rather than just using an annual mean value.
! 2014/05/23, GM: Cleaned up code.
! 2014/08/01, GM: Added Find_MDAD_SW() and Find_MDAD_LW().
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module SAD_Chan_def

   use ECP_constants

   implicit none

   private

   public :: Solar_t, &
             Thermal_t, &
             SAD_Chan_t, &
             Find_MDAD_SW, &
             Find_MDAD_LW, &
             Read_SAD_Chan

   type Solar_t
      integer(1)  :: Flag        ! Value 1 indicates solar source is present
      real        :: F0          ! Solar constant (annual mean).
      real        :: F1          ! Amplitude of variation in solar constant.
      real        :: NeHomog(MaxCloudType) ! Homogeneity noise
      real        :: NeCoreg(MaxCloudType) ! Coregistration noise
      real        :: NedR        ! Noise equivalent delta radiance
      real        :: Rs(2)       ! 'Typical' land/sea reflectance
   end type Solar_t

   type Thermal_t
      integer(1)  :: Flag        ! Value 1 indicates thermal sources present
      real        :: B1          ! Planck function coefficient
      real        :: B2          ! Planck function coefficient
      real        :: T1          ! Planck function coefficient
      real        :: T2          ! Planck function coefficient
      real        :: NeHomog(MaxCloudType) ! Homogeneity noise
      real        :: NeCoreg(MaxCloudType) ! Coregistration noise
      real        :: NEBT        ! Noise equivalent brightness temperature
   end type Thermal_t

   type SAD_Chan_t
      character(10)   :: Desc    ! Descriptor (e.g. 0.6 um)
      character(6)    :: FileID  ! File tag (e.g. CH3)
      real            :: WvN     ! Central wavenumber
      type(Thermal_t) :: Thermal ! Thermal sub-structure
      type(Solar_t)   :: Solar   ! Solar sub-structure
   end type SAD_Chan_t

contains

!-------------------------------------------------------------------------------
! Name: Find_MDAD_SW and Find_MDAD_LW
!
! Purpose:
! Finds reference channels for the MDAD first-guess method.
!
! Description:
! Algorithm:
! 1) If SW, search each channel for that with wavenumber between 10000 and 20000
!    cm-1 that is closest to 14925.
! 2) If LW, search each channel for that with wavenumber greater than 2500 cm-1
!    that is closest to 909.
! 3) On fail, returns 0.
!
! Arguments:
! Name      Type    In/Out/Both    Description
! ------------------------------------------------------------------------------
! Ny        integer In             Number of channels
! SAD_Chan  array of structs (out) Channel description info.
! index     array of ints (in)     Optional. Indices denoting a subset of the
!                                  SAD_Chan array to be searched.
!
! History:
! 2014/08/01, GM: Original version
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

function Find_MDAD_SW(Ny, SAD_Chan, index) result(MDAD_SW)

   implicit none

   integer,          intent(in)           :: Ny
   type(SAD_Chan_t), intent(in)           :: SAD_Chan(:)
   integer,          intent(in), optional :: index(:)

   integer                                :: MDAD_SW

   integer :: i
   integer :: ii
   real    :: diff
   real    :: min_diff

   MDAD_SW = 0

   min_diff = huge(0.)

   ! Loop over all channels
   do i = 1, Ny
      if (.not. present(index)) then
         ii = i
      else
         ii = index(i)
      end if

      ! If the channel WN is greater than 10000 cm-1 (1.0 um) and less than
      ! 20000 cm-1 (0.5 um) then check how close it is to 14925 cm-1 (0.67 um).
      ! If it is the closest of the channels tried so far, update the minimum
      ! difference and set the channel index
      if (SAD_Chan(ii)%WvN > 10000.0 .and. SAD_Chan(ii)%WvN < 20000.0) then
         ! Difference between central WN and 14925 cm-1 (0.67 um)
         diff = abs(SAD_Chan(ii)%WvN - 14925.0)
         if (diff < min_diff) then
            min_diff = diff
            MDAD_SW = i
         end if
      end if
   end do

end function Find_MDAD_SW


function Find_MDAD_LW(Ny, SAD_Chan, index) result(MDAD_LW)

   implicit none

   integer,          intent(in)           :: Ny
   type(SAD_Chan_t), intent(in)           :: SAD_Chan(:)
   integer,          intent(in), optional :: index(:)

   integer                                :: MDAD_LW

   integer :: i
   integer :: ii
   real    :: diff
   real    :: min_diff

   MDAD_LW = 0

   min_diff = huge(0.)

   ! Loop over all channels
   do i = 1, Ny
      if (.not. present(index)) then
         ii = i
      else
         ii = index(i)
      end if

      ! If the channel WN is greater than 2500 cm-1 (4.0 um) then check how
      ! close it is to 909 cm-1 (11.0 um). If it is the closest of the channels
      ! tried so far, update the minimum difference and set the channel index
      if (SAD_Chan(ii)%WvN < 2500.0) then
         ! Difference between central WN and 909 cm-1 (11.0 um)
         diff = abs(SAD_Chan(ii)%WvN - 909.0)
         if (diff < min_diff) then
            min_diff = diff
            MDAD_LW = i
         end if
      end if
   end do

end function Find_MDAD_LW


include 'ReadSADChan.F90'

end module SAD_Chan_def
