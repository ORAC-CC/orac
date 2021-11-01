!-------------------------------------------------------------------------------
! Name: sad_chan.F90
!
! Purpose:
! Module containing Static Application Data Channel structure, used to hold
! Static Application Data for measurement channels in the ORAC.
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
! 2015/08/14, AP: Replace Find_MDAD() with generalised Find_Channel().
! 2015/08/21, AP: Move make_sad_filename() here from ReadSADLUT.
! 2017/06/21, OS: string name fix for METOP
! 2021/10/12, ATP: Add reference brightness temperature (T0) to
!    Thermal type and signal-to-noise ratio (SNR) to Solar type.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module SAD_Chan_m

   use ORAC_Constants_m

   implicit none

   private

   public :: Solar_t, &
             Thermal_t, &
             SAD_Chan_t, &
             Find_Channel, &
             Read_SAD_Chan, &
             create_sad_filename

   type Solar_t
      integer(1)  :: Flag        ! Value 1 indicates solar source is present
      real        :: F0          ! Solar constant (annual mean).
      real        :: F1          ! Amplitude of variation in solar constant.
      real        :: NeHomog(MaxCloudType) ! Homogeneity noise
      real        :: NeCoreg(MaxCloudType) ! Coregistration noise
      real        :: NedR        ! Noise equivalent delta radiance
      real        :: Rs(2)       ! 'Typical' land/sea reflectance
      real        :: SNR         ! Signal-to-noise ratio
   end type Solar_t

   type Thermal_t
      integer(1)  :: Flag        ! Value 1 indicates thermal sources present
      real        :: B1          ! Planck function coefficient
      real        :: B2          ! Planck function coefficient
      real        :: T1          ! Planck function coefficient
      real        :: T2          ! Planck function coefficient
      real        :: T0          ! Reference temperature for NEBT
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
! Name: Find_Channel
!
! Purpose:
! Locates the closest channel to a given wavenumber. With multiple views, will
! return the first channel in the ordering unless the mask argument is used.
!
! Description and algorithm details:
! 1) Loop over channels that have wavenumber within the specified range. If
!    searching for an index that isn't wrt Ctrl%Ind%Ny, specify the conversion
!    indices.
! 2) On fail, returns 0.
!
! Arguments:
! Name      Type    In/Out/Both    Description
! ------------------------------------------------------------------------------
! wvn       real    In             Wavenumber of the desired channel
! Ny        integer In             Number of channels
! SAD_Chan  array of structs (out) Channel description info.
! index     array of ints (in)     Optional. Indices denoting a subset of the
!                                  SAD_Chan array to be searched.
! min       real    In             Optional. Minimal permitted wavenumber
! max       real    In             Optional. Maximal permitted wavenumber
! mask      array of logicals (In) Optional. Indicates valid channels.
! channel   integer Out            (Return value) Index of desired channel.
!                                  0 is no suitable channel found.
!
! History:
! 2014/08/01, GM: Original version
! 2015/08/14, AP: Generalised Find_MDAD_SW and Find_MDAD_LW
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------
function Find_Channel(wvn, Ny, SAD_Chan, index, min, max, mask) result(channel)

   implicit none

   real,              intent(in) :: wvn
   integer,           intent(in) :: Ny
   type(SAD_Chan_t),  intent(in) :: SAD_Chan(:)
   integer, optional, intent(in) :: index(:)
   real,    optional, intent(in) :: min
   real,    optional, intent(in) :: max
   logical, optional, intent(in) :: mask(:)
   integer                       :: channel

   integer :: i,  ii
   real    :: diff,  min_diff
   real    :: min_wvn, max_wvn

   channel  = 0
   min_diff = huge(0.)

   if (present(min)) then
      min_wvn = min
   else
      min_wvn = 0.0
   end if
   if (present(max)) then
      max_wvn = max
   else
      max_wvn = huge(0.)
   end if

   do i = 1, Ny
      ! Skip masked channels
      if (present(mask)) then
         if (.not. mask(i)) cycle
      end if

      ! Convert desired index to Ctrl%Ind%Ny index for SAD_Chan
      if (present(index)) then
         ii = index(i)
      else
         ii = i
      end if

      if (SAD_Chan(ii)%WvN > min_wvn .and. SAD_Chan(ii)%WvN < max_wvn) then
         diff = abs(SAD_Chan(ii)%WvN - wvn)
         if (diff < min_diff) then
            min_diff = diff
            channel = i
         end if
      end if
   end do

end function Find_Channel


!-------------------------------------------------------------------------------
! Name: create_sad_filename
!
! Purpose:
! Create a SAD filename given the lut name and string channel number.
!
! Algorithm:
!
! Arguments:
! Name Type In/Out/Both Description
!
! History:
! 2014/10/10, GM: Original version
! 2015/08/21, AP: Made crp_name optional so this can generate SAD_Chan
!    filenames. Generalised treatment of NOAA7/9.
! 2016/05/03, AP: Convert to a function and make chan_num optional.
! 2016/07/27, GM: Changes for the multilayer retrieval.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------
function create_sad_filename(Ctrl, chan_num, i_layer, crp_name) result(filename)

   use Ctrl_m

   implicit none

   ! Argument declarations
   type(Ctrl_t),           intent(in)  :: Ctrl
   character(*), optional, intent(in)  :: chan_num
   integer,      optional, intent(in)  :: i_layer
   character(*), optional, intent(in)  :: crp_name
   character(FilenameLen)              :: filename

   integer :: i_layer2

   if (.not. present(i_layer)) then
      i_layer2 = 1
   else
      i_layer2 = i_layer
   end if

   if (i_layer2 .eq. 1) then
      filename = create_sad_filename2(Ctrl, chan_num, Ctrl%FID%SAD_Dir, &
                                      Ctrl%LUTClass, crp_name)
   else
      filename = create_sad_filename2(Ctrl, chan_num, Ctrl%FID%SAD_Dir2, &
                                      Ctrl%LUTClass2, crp_name)
   end if

end function create_sad_filename


function create_sad_filename2(Ctrl, chan_num, SAD_Dir, LUTClass, crp_name) &
   result(filename)

   use Ctrl_m

   implicit none

   ! Argument declarations
   type(Ctrl_t),           intent(in) :: Ctrl
   character(*), optional, intent(in) :: chan_num
   character(*),           intent(in) :: SAD_Dir
   character(*),           intent(in) :: LUTClass
   character(*), optional, intent(in) :: crp_name
   character(FilenameLen)             :: filename

   character(InstNameLen) :: InstName

   InstName = Ctrl%InstName
   ! NOAA files use (I0) formatting in their filename; LUT files use (I2).
   if (InstName(1:10) == 'AVHRR-NOAA') then
      if (len_trim(InstName(11:)) == 1) then
         InstName(12:12) = InstName(11:11)
         InstName(11:11) = '0'
      end if
   else if (InstName(1:11) == 'AVHRR-METOP') then
      ! For Metop, only platform name's first letter is capitalized
      InstName(8:11) = 'etop'
      ! Replace MetopA with Metop2 and MetopB with Metop1
      if (InstName(12:12) == 'A') then
         InstName(12:12) = '2'
      else if (InstName(12:12) == 'B') then
         InstName(12:12) = '1'
      else
         write(*,*) 'ERROR: SADChan(): METOP platform name not one of METOPA or METOPB'
         stop error_stop_code
      end if
   end if

   if (present(chan_num)) then
      if (present(crp_name)) then
         filename = trim(SAD_Dir) // '/' // trim(InstName) // '_' // &
                    trim(LUTClass) // '_' // trim(crp_name) // '_' // &
                    trim(chan_num) // '.sad'
      else
         filename = trim(SAD_Dir) // '/' // trim(InstName) // '_' // &
                    trim(chan_num) // '.sad'
      end if
   else
      if (present(crp_name)) then
         filename = trim(SAD_Dir) // '/' // trim(InstName) // '_' // &
                    trim(LUTClass) // '_' // trim(crp_name) // '.sad'
      else
         filename = trim(SAD_Dir) // '/' // trim(InstName) // '.sad'
      end if
   end if

end function create_sad_filename2


#include "read_sad_chan.F90"

end module SAD_Chan_m
