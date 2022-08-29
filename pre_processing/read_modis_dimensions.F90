!-------------------------------------------------------------------------------
! Name: read_modis_dimensions.F90
!
! Purpose:
! Open geo input file to determine the size of the data array for the purposes
! of dynamic array allocation.
!
! Description and Algorithm details:
! 1) Open geolocation file
! 2) Access the Latitude field and output its dimensions
! 3) Close file
!
! Arguments:
! Name             Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! path_to_geo_file string in  Full path to geolocation file
! n_across_track   lint   out Number of pixels available perpendicular to the
!                             direction of travel
! n_along_track    lint   out Number of pixels in the direction of travel
!
! History:
! 2011/12/12, MJ: produces draft code which opens and reads MODIS geo hdf files
! 2013/09/11, AP: tidying, removed path_to_l1b_file
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine read_modis_dimensions(path_to_geo_file, n_across_track, n_along_track)

   use preproc_constants_m
   use hdf_m, only: DFACC_READ

   implicit none

   character(len=*),   intent(in)  :: path_to_geo_file
   integer(kind=lint), intent(out) :: n_across_track, n_along_track

   integer                    :: geo_id, dims(2), err_code
   integer                    :: dummy_var_id, dummy_rank, dummy_type, &
                                 dummy_numattrs
   character(len=MAX_NC_NAME) :: dummy_name

   integer(kind=4), external  :: sfstart, sfselect, sfn2index, sfginfo
   integer(kind=4), external  :: sfendacc, sfend

   ! this only serves to get us the dimensions of the granule
   geo_id = sfstart(path_to_geo_file, DFACC_READ)
   dummy_var_id = sfselect(geo_id, sfn2index(geo_id, "Latitude"))
   err_code = sfginfo(dummy_var_id, dummy_name, dummy_rank, dims, dummy_type, &
        dummy_numattrs)

   n_across_track = dims(1)
   n_along_track = dims(2)

   err_code = sfendacc(dummy_var_id)
   err_code = sfend(geo_id)

end subroutine read_modis_dimensions
