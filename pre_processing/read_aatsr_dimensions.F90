!-------------------------------------------------------------------------------
! Name: read_aatsr_dimensions.F90
!
! Purpose:
! Determine the dimensions of AATSR data to read in, based on a day_night flag
! and lat-lon limit. This is mostly a wrapper for read_aatsr_beam.c
!
! Description and Algorithm details:
! 1) Prepare variables.
! 2) Call the C function get_aatsr_dimension_ctof90.
! 3) If processing night data, call the function again.
!
! Arguments:
! Name                Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! path_to_l1b_file    string in  Full path to L1B file
! n_across_track      lint   out Number of pixels available perpendicular to the
!                                direction of travel
! n_along_track       lint   out Number of pixels in the direction of travel
! along_track_offset  lint   out Pixel number at which reading should begin
!                                (generally where daylight begins)
! day_night           stint  in  1: daytime data; 2: night data; 3: everything
!                                4: 1st half of night; 5: 2nd half of night
! loc_limit           sreal  in  (/ minimum latitude, minimum
!                                longitude, maximum latitude, maximum
!                                longitude /)
! n_along_track2      lint   out When considering night data, there
!                                are two chunks of data (each end of the orbit).
!                                This gives the second chunk's length.
! along_track_offset2 lint   out The pixel number of the beginning of
!                                a secondchunk for night data.
!
! History:
! 2012/06/22, GT: First version
! 2012/07/29, CP: changed filename to lower case
! 2012/08/21, GT: Tidied up unused variables
! 2012/08/27, GT: Changed C call to use ISO C binding
! 2013/08/14, GT: Added half_orbit optional input parameter used for dealing
!    with night time data.
! 2013/09/06, AP: tidying
! 2013/10/08, AP: altered call to C routine
! 2013/01/24, MJ: removed "optionality" of some arguments
! 2015/09/15, CP: Put a stop in if reads too narrow swath this is a bug fix to
!    prevent zero files being created
! 2019/06/21, GT: Added day_night options 4 and 5, which allow either the first
!    or second parts of the night-side (ascending) orbit to be read.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine read_aatsr_dimensions(path_to_l1b_file,n_across_track, &
     n_along_track,along_track_offset,day_night,loc_limit,n_along_track2, &
     along_track_offset2,verbose)

   use iso_c_binding
   use preproc_constants_m

   implicit none

   interface
      subroutine get_aatsr_dimension(l1b_file, daynight, limit, half_orbit, &
           nx, ny, miny, stat, verb) bind(C,name='get_aatsr_dimension')
         use iso_c_binding

         implicit none

         character(kind=c_char), dimension(*) :: l1b_file
         integer(kind=c_short)                :: daynight, half_orbit, stat
         real(kind=c_float), dimension(4)     :: limit
         integer(kind=c_long)                 :: nx, ny, miny
         logical(kind=c_bool)                 :: verb
      end subroutine get_aatsr_dimension
   end interface

   character(len=*),               intent(in)  :: path_to_l1b_file
   integer(kind=lint),             intent(out) :: n_across_track, n_along_track
   integer(kind=lint),             intent(out) :: along_track_offset
   integer(kind=sint),             intent(in)  :: day_night
   real(kind=sreal), dimension(4), intent(in)  :: loc_limit
   integer(kind=lint),             intent(out) :: n_along_track2
   integer(kind=lint),             intent(out) :: along_track_offset2
   logical,                        intent(in)  :: verbose

   character(len=path_length,kind=c_char) :: l1b_file_C
   integer(c_short)                       :: half_orbit, c_dynght
   real(c_float), dimension(4)            :: tmp_limit
   integer(c_short)                       :: err_code
   logical(c_bool)                        :: verb
   integer(c_long)                        :: tmp_nx, tmp_ny, tmp_miny

   n_across_track = 2
   n_along_track = 3
   along_track_offset = 4

   tmp_nx = n_across_track
   tmp_ny = n_along_track
   tmp_miny = along_track_offset

   ! Check for the presence of the optional day_night and lat-lon limit
   ! input variables and set defaults if not present.
   tmp_limit = loc_limit

   ! If we are dealing with night time data, we need to call
   ! get_aatsr_dimension_ctof90 twice; once for the start of the orbit
   ! and once for the end. We also populate the optional output parameters
   ! "n_along_track2" and "along_track_offset2"
   ! If, however, the option 4 or 5 have been passed for day_night, then we
   ! only read either the start OR end of the orbit.
   if (day_night .eq. 2 .or. day_night .eq. 4) then
      half_orbit = 1
   else
      if (day_night .eq. 5) then
         half_orbit = 2
      else
         half_orbit = 0
      end if
   end if
   ! Redefine the dynht variable, so that it is either 0 (everything),
   ! 1 (daylight) or 2 (night)
   if (day_night .eq. 2 .or. day_night .eq. 4 .or. day_night .eq. 5) then
      c_dynght = 2
   else if (day_night .eq. 1) then
      c_dynght = 1
   else
      c_dynght = 0
   end if

   ! This is a wrapper function for C code using the EPR_API from
   ! Brokemann Consulting
   if (verbose) then
      verb = .true.
   else
      verb = .false.
   end if
   l1b_file_C = trim(path_to_l1b_file)//C_NULL_CHAR
   call get_aatsr_dimension(l1b_file_C, c_dynght, tmp_limit, &
        half_orbit, tmp_nx, tmp_ny, tmp_miny, err_code, verb)

   n_across_track = tmp_nx
   n_along_track  = tmp_ny
   along_track_offset = tmp_miny

   if (n_across_track < 10) then
      write(*,*) 'ERROR: read_aatsr_dimensions(): swath too narrow, ' // &
         'n_across_track = ', n_across_track
      stop error_stop_code
   end if

   ! make second call for second night time chunk
   if (day_night .eq. 2) then
      half_orbit = 2
      call get_aatsr_dimension(l1b_file_C, c_dynght, tmp_limit, &
           half_orbit, tmp_nx, tmp_ny, tmp_miny, err_code, verb)

      n_along_track2  = tmp_ny
      along_track_offset2 = tmp_miny
   end if

end subroutine read_aatsr_dimensions
