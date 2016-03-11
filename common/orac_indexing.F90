!-------------------------------------------------------------------------------
! Name: orac_common.F90
!
! Purpose:
! Module defining structures common to ORAC executables
!
! History:
! 2016/03/02, AP: Initial version, forked from orac_input and _output.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module orac_indexing

   use common_constants

   implicit none

   type common_file_flags
      ! Flags relevant to both files
      logical :: do_cloud               ! Output cloud retrieval terms
      logical :: do_aerosol             ! Output aerosol retrieval terms
      logical :: do_rho                 ! Output retrieved surface reflectances
      logical :: do_swansea             ! Output retrieved Swansea parameters
      logical :: do_indexing            ! Output channel indexing data

      ! Primary file flags
      logical :: do_phase_pavolonis     ! Output the Pavolonis cloud phase
      logical :: do_cldmask             ! Output neural net cloud mask
      logical :: do_cldmask_uncertainty ! Output the uncertainty on that
      logical :: do_phase               ! Output particle type

      ! Secondary file flags
      logical :: do_covariance          ! Output the final covariance matrix
   end type common_file_flags


   type common_indices
      ! Channel indexing
      integer          :: Ny             ! No. of instrument channels used
      integer, pointer :: Y_Id(:)        ! Instrument IDs for used chs
      integer          :: NSolar         ! No. of chs with solar source
      integer, pointer :: YSolar(:)      ! Array indices for solar chs
                                         ! out of those used (Y)
      integer          :: NThermal       ! No. of chs w/ thermal source
      integer, pointer :: YThermal(:)    ! Array indices for thermal ch
      integer          :: NViews         ! No. of instrument views available
      integer, pointer :: View_Id(:)     ! IDs of instrument views
      integer, pointer :: Ch_Is(:)       ! A bit flag of ch properties

      ! State vector
      integer          :: Nx             ! Dimension of covariance matrix
      logical, pointer :: rho_terms(:,:) ! Flags use of surface property

      ! Instrument swath
      integer          :: Xdim           ! Across track dimension
      integer          :: X0             ! First pixel across track
      integer          :: X1             ! Last pixel across track
      integer          :: Ydim           ! Along track dimension
      integer          :: Y0             ! First pixel along track
      integer          :: Y1             ! Last pixel along track

      type(common_file_flags) :: flags      ! Fields to in/output
   end type common_indices


   ! common_file_flags bitmask locations
   integer, parameter :: cloud_bit      = 0
   integer, parameter :: aerosol_bit    = 1
   integer, parameter :: rho_bit        = 2
   integer, parameter :: swansea_bit    = 3
   integer, parameter :: indexing_bit   = 4
   integer, parameter :: pavolonis_bit  = 5
   integer, parameter :: cldmask_bit    = 6
   integer, parameter :: cldmask_u_bit  = 7
   integer, parameter :: phase_bit      = 8
   integer, parameter :: covariance_bit = 9


contains

subroutine make_bitmask_from_common_file_flags(flags, bitmask)

   implicit none

   type(common_file_flags), intent(in)  :: flags
   integer,                 intent(out) :: bitmask

   bitmask = 0
   if (flags%do_cloud)               bitmask = ibset(bitmask, cloud_bit)
   if (flags%do_aerosol)             bitmask = ibset(bitmask, aerosol_bit)
   if (flags%do_rho)                 bitmask = ibset(bitmask, rho_bit)
   if (flags%do_swansea)             bitmask = ibset(bitmask, swansea_bit)
   if (flags%do_indexing)            bitmask = ibset(bitmask, indexing_bit)
   if (flags%do_phase_pavolonis)     bitmask = ibset(bitmask, pavolonis_bit)
   if (flags%do_cldmask)             bitmask = ibset(bitmask, cldmask_bit)
   if (flags%do_cldmask_uncertainty) bitmask = ibset(bitmask, cldmask_u_bit)
   if (flags%do_phase)               bitmask = ibset(bitmask, phase_bit)
   if (flags%do_covariance)          bitmask = ibset(bitmask, covariance_bit)

end subroutine make_bitmask_from_common_file_flags


subroutine set_common_file_flags_from_bitmask(bitmask, flags)

   implicit none

   integer,                 intent(in)  :: bitmask
   type(common_file_flags), intent(out) :: flags

   flags%do_cloud               = btest(bitmask, cloud_bit)
   flags%do_aerosol             = btest(bitmask, aerosol_bit)
   flags%do_rho                 = btest(bitmask, rho_bit)
   flags%do_swansea             = btest(bitmask, swansea_bit)
   flags%do_indexing            = btest(bitmask, indexing_bit)
   flags%do_phase_pavolonis     = btest(bitmask, pavolonis_bit)
   flags%do_cldmask             = btest(bitmask, cldmask_bit)
   flags%do_cldmask_uncertainty = btest(bitmask, cldmask_u_bit)
   flags%do_phase               = btest(bitmask, phase_bit)
   flags%do_covariance          = btest(bitmask, covariance_bit)

end subroutine set_common_file_flags_from_bitmask


subroutine make_bitmask_from_rho_terms(ind, bitmask)

   implicit none

   type(common_indices), intent(in)  :: ind
   integer(byte),        intent(out) :: bitmask(:) ! byte as an output variable

   integer :: i, j

   bitmask = 0
   do i=1,ind%NSolar
      do j=1,MaxRho_XX
         if (ind%rho_terms(i,j)) bitmask(i) = ibset(bitmask(i), j)
      end do
   end do

end subroutine make_bitmask_from_rho_terms


subroutine set_rho_terms_from_bitmask(bitmask, ind)

   implicit none

   integer,              intent(in)  :: bitmask(:)
   type(common_indices), intent(out) :: ind

   integer :: i, j

   do i=1,ind.NSolar
      do j=1,MaxRho_XX
         ind%rho_terms(i,j) = btest(bitmask(i), j)
      end do
   end do

end subroutine set_rho_terms_from_bitmask


subroutine create_rho_field_name(rho_index, mode, input_num, &
                                 field_name, description)

   implicit none

   integer,                    intent(in)  :: rho_index
   integer,                    intent(in)  :: mode
   character(len=*),           intent(in)  :: input_num
   character(len=*),           intent(out) :: field_name
   character(len=*), optional, intent(out) :: description

   character(len=512) :: descr1, descr2, field1, field2

   select case (rho_index)
   case(IRho_0V)
      descr1 = 'surface direct beam reflectance'
      field1 = '0V'
   case(IRho_0D)
      descr1 = 'surface direct-to-diffuse reflectance'
      field1 = '0D'
   case(IRho_DV)
      descr1 = 'surface diffuse-to-direct reflectance'
      field1 = 'DV'
   case(IRho_DD)
      descr1 = 'surface diffuse reflectance'
      field1 = 'DD'
   end select

   select case (mode)
   case(1)
      descr2 = ''
      field2 = ''
   case(2)
      descr2 = 'uncertainty in'
      field2 = '_uncertainty'
   case(3)
      descr2 = 'a priori'
      field2 = '_ap'
   case(4)
      descr2 = 'first guess'
      field2 = '_fg'
   end select

   field_name = 'rho_'//trim(field1)//trim(field2)// &
        '_in_channel_no_'//trim(adjustl(input_num))

   if (present(description)) then
      description = trim(descr2)//' '//trim(descr1)
      description = trim(adjustl(description))// &
           ' in channel no '//trim(adjustl(input_num))
   end if

end subroutine create_rho_field_name


subroutine dealloc_common_indices(ind)

   implicit none

   type(common_indices), intent(inout) :: ind

   deallocate(ind%Y_Id)
   deallocate(ind%YSolar)
   deallocate(ind%YThermal)
   deallocate(ind%View_Id)
   deallocate(ind%Ch_Is)
   if (associated(ind%rho_terms)) deallocate(ind%rho_terms)

end subroutine dealloc_common_indices


end module orac_indexing
