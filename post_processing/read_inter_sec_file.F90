
! Name: read_inter_file.f90
!
!
! Purpose:
! The file contains a collection of subroutines which define netcdf output for different attribute/variable type combinations.
! Subroutines names are selfdescriptive.
!
!
! Description and Algorithm details:
!
!
! Arguments:
! Name Type In/Out/Both Description
!
!
! Local variables:
! Name Type Description
!
!
! History:
!2012/02/20: Caroline Poulsen creates initial file.
!2012/06/20: C. Poulsen modified to work for all sensors
!2012/06/20: C. Poulsen modified to add albedo
!2012/06/20: C. Poulsen changed arguments of read_inter_sec_file added in reading of albedo added in reading of channel radiances
!2014/09/29: C. Poulsen changed number of arguments read in and added MODIS specific call
!2014/10/10: A. Povey Added case statement to secondary read to very badly
!2014/12/03: CP uncommented albedo because need for snow mask added common_constants
!   deal with differing instrument channels.
!
! $Id$
!
! Bugs:
! not a bug but this code assumes 5 channels of instrument data
!
!none known

!--------------------------------------------------
!--------------------------------------------------
SUBROUTINE read_refl_and_bt(iphase,fname,chan_id,l2_input_2d_refl_bt,xdim,ydim,wo,ierr)
   !--------------------------------------------------
   use netcdf

   use vartypes_pp
  use common_constants
   use structures_pp

   implicit none
   INTEGER,INTENT(IN) :: iphase,wo
   integer :: ivar,idim,ndim,nvar,nattr,dummyint
   integer :: ncid,ierr,ny=5,nx=5
   character(len=varlength) :: fname,name
   integer (kind=nint), allocatable :: dimids(:), varids(:), attrids(:), dimlength(:)
   character(len=varlength), allocatable :: dname(:)


   character (len=varlength), allocatable, dimension(:) ::  available_names(:)
   INTEGER (kind=nint),INTENT(OUT) ::  xdim,ydim

   CHARACTER(LEN=unitlength) ::  dummy_unit

   integer :: i
   character(len=varlength), intent(in) :: chan_id(1:5)

   type(l2_input_struct_2d_refl_bt) :: l2_input_2d_refl_bt

   real (kind=sreal), allocatable, dimension(:,:) :: l2var_dummy
   integer(kind=byte), allocatable, dimension(:,:) :: l2var_dummy_byte
   integer(kind=sint), allocatable, dimension(:,:) :: l2var_dummy_short


   call nc_open_pp(ncid,fname,ierr,wo)
   call nc_info_pp(ncid,ndim,nvar,nattr,wo)


   allocate(dimids(ndim))
   dimids=0
   allocate(dname(ndim))
   dname=''
   allocate(dimlength(ndim))
   dimlength=0
   allocate(varids(nvar))
   varids=0
   allocate(attrids(nattr))
   attrids=0


   allocate(available_names(nvar))
   available_names=''

   name='across_track'
   call nc_dim_id_pp(ncid,trim(adjustl(name)),dimids(1),wo)
   call nc_dim_length_pp(ncid,name,dimids(1),dummyint,wo)
   dimlength(1)=dummyint
   xdim=int(dimlength(1),kind=nint)

   name='along_track'
   call nc_dim_id_pp(ncid,trim(adjustl(name)),dimids(2),wo)
   call nc_dim_length_pp(ncid,name,dimids(2),dummyint,wo)
   dimlength(2)=dummyint
   ydim=int(dimlength(2),kind=nint)


   allocate(l2var_dummy(xdim,ydim))
   l2var_dummy=real_fill_value

   allocate(l2var_dummy_short(xdim,ydim))
   l2var_dummy_short=short_int_fill_value

   allocate(l2var_dummy_byte(xdim,ydim))
   l2var_dummy_byte=byte_fill_value


   call set_l2_refl_and_bt(l2_input_2d_refl_bt,xdim,ydim)




   deallocate(l2var_dummy)
   deallocate(l2var_dummy_short)
   deallocate(l2var_dummy_byte)

   !close  input file
   ierr=nf90_close(ncid)


END SUBROUTINE read_refl_and_bt


!--------------------------------------------------
!--------------------------------------------------
subroutine read_inter_sec_file(cinst, fname, l2_input_2d_secondary, xdim, ydim, &
     wo, ierr)

   use netcdf
   use vartypes_pp
  use common_constants
   use structures_pp

   implicit none

   character(len=attribute_length),    intent(in)    :: cinst
   character(len=cpathlength),         intent(in)    :: fname
   type(l2_input_struct_2d_secondary), intent(inout) :: l2_input_2d_secondary
   integer(kind=nint),                 intent(in)    :: xdim, ydim
   integer,                            intent(in)    :: wo
   integer,                            intent(out)   :: ierr
   
   integer                   :: ncid
   character(len=unitlength) :: dummy_unit


   write(*,*) 'Opening secondary input file ', trim(fname)
   call nc_open_pp(ncid,fname,ierr,wo)

   ! Read appropriate channels from file for the instrument
   select case (cinst)
   case('AVHRR')
      ! Read albedo values (commented out until needed)
    call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
        'albedo_in_channel_no_1', &
        l2_input_2d_secondary%albedo_in_channel_no_1,dummy_unit,wo)
    call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
        'albedo_in_channel_no_2', &
        l2_input_2d_secondary%albedo_in_channel_no_2,dummy_unit,wo)
    call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
        'albedo_in_channel_no_3', &
        l2_input_2d_secondary%albedo_in_channel_no_3,dummy_unit,wo)

      ! Read reflectances (third ch ommitted until needed)
      call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
           'reflectance_in_channel_no_1', &
           l2_input_2d_secondary%reflectance_in_channel_no_1,dummy_unit,wo)
      call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
           'reflectance_in_channel_no_2', &
           l2_input_2d_secondary%reflectance_in_channel_no_2,dummy_unit,wo)

      ! Read brightness temperatures
      call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
           'brightness_temperature_in_channel_no_4', &
           l2_input_2d_secondary%brightness_temperature_in_channel_no_4, &
           dummy_unit,wo)
      call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
           'brightness_temperature_in_channel_no_5', &
           l2_input_2d_secondary%brightness_temperature_in_channel_no_5, &
           dummy_unit,wo)
      call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
           'brightness_temperature_in_channel_no_6', &
           l2_input_2d_secondary%brightness_temperature_in_channel_no_6, &
           dummy_unit,wo)

   case('AATSR')
      ! Read albedo values (commented out until needed)
    call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
         'albedo_in_channel_no_2', &
         l2_input_2d_secondary%albedo_in_channel_no_2,dummy_unit,wo)
    call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
         'albedo_in_channel_no_3', &
         l2_input_2d_secondary%albedo_in_channel_no_3,dummy_unit,wo)
!    call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
!         'albedo_in_channel_no_4', &
!         l2_input_2d_secondary%albedo_in_channel_no_4,dummy_unit,wo)

      ! Read reflectances (third ch ommitted until needed)
      call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
           'reflectance_in_channel_no_2', &
           l2_input_2d_secondary%reflectance_in_channel_no_2,dummy_unit,wo)
      call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
           'reflectance_in_channel_no_3', &
           l2_input_2d_secondary%reflectance_in_channel_no_3,dummy_unit,wo)

      ! Read brightness temperatures
      call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
           'brightness_temperature_in_channel_no_5', &
           l2_input_2d_secondary%brightness_temperature_in_channel_no_5, &
           dummy_unit,wo)
      call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
           'brightness_temperature_in_channel_no_6', &
           l2_input_2d_secondary%brightness_temperature_in_channel_no_6, &
           dummy_unit,wo)
      call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
           'brightness_temperature_in_channel_no_7', &
           l2_input_2d_secondary%brightness_temperature_in_channel_no_7, &
           dummy_unit,wo)

   case('MODIS')
      ! Read albedo values (commented out until needed)
    call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
         'albedo_in_channel_no_1', &
         l2_input_2d_secondary%albedo_in_channel_no_1,dummy_unit,wo)
    call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
         'albedo_in_channel_no_2', &
         l2_input_2d_secondary%albedo_in_channel_no_2,dummy_unit,wo)
!    call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
!         'albedo_in_channel_no_6', &
!         l2_input_2d_secondary%albedo_in_channel_no_6,dummy_unit,wo)

      ! Read reflectances (third ch ommitted until needed)
      call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
           'reflectance_in_channel_no_1', &
           l2_input_2d_secondary%reflectance_in_channel_no_1,dummy_unit,wo)
      call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
           'reflectance_in_channel_no_2', &
           l2_input_2d_secondary%reflectance_in_channel_no_2,dummy_unit,wo)

      ! Read brightness temperatures
      call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
           'brightness_temperature_in_channel_no_20', &
           l2_input_2d_secondary%brightness_temperature_in_channel_no_20, &
           dummy_unit,wo)
      call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
           'brightness_temperature_in_channel_no_31', &
           l2_input_2d_secondary%brightness_temperature_in_channel_no_31, &
           dummy_unit,wo)
      call nc_read_array_2d_short_orac_pp(ncid,xdim,ydim, &
           'brightness_temperature_in_channel_no_32', &
           l2_input_2d_secondary%brightness_temperature_in_channel_no_32, &
           dummy_unit,wo)
   end select

   !close  input file
   ierr=nf90_close(ncid)
   write(*,*) 'Closed secondary input file.'
   

end subroutine read_inter_sec_file
