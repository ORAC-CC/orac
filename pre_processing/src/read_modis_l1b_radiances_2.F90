!-------------------------------------------------------------------------------
! Name: read_modis_l1b_radiances_2.F90
!
! Purpose:
! Read the radiance values from an already open MODIS M*D02!KM file
!
! Description and Algorithm details:
! 1) Select appropriate resolution band for requested channel.
! 2) Determine which channels are available in the file for that band.
! 3) Select the requested channel from among those.
! 4) Read data, valid range, scale factors, and offsets.
! 5) Apply scale factor and offset to data.
!
! Arguments:
! Name             Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! fid              HID_T  in   File ID from SFstart
! band             int    in   Instrument channel number to read
! cal_type_is_refl logic  in   1: channel measures reflectance; 0: brightness
!                              temperature
! ixstart          lint   in   First pixel to read across track
! ixstop           lint   in   Last pixel to read across track
! iystart          lint   in   First pixel to read along track
! iystop           lint   in   Last pixel to read along track
! level1b_buffer   sreal  both Initialised array into which data will be stored
! verbose          logic  in   F: minimise information printed to screen;
!                              T: don't
!
! History:
! 2011/12/??, MJ: First version
! 2013/03/22, GT: Added code to assign the band index number dynamically using
!   information contained in the M*D02 file itself.
! 2013/09/12, AP: Tidying, added where statement
! 2013/10/15, MJ: Changes reading of band names for MODIS.
! 2013/10/22, AP: When a field in the spectrally subsetted files contained only
!   one channel, the band_names field ended in NULL characters which Fortran
!   could not manage. Those have been removed.
! 2013/11/05, GM: Moved the verbose statement that prints band_names from just
!   before Adam's removing of NULL characters to just after as some text
!   operations can be affected by them.
! 2014/01/12, GM: Fixed it so that the right scales and offsets are used.
! 2014/01/12, GM: Cleaned up the code.
! 2014/07/23, AP: More efficient array writing
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine read_modis_l1b_radiances_2(fid, band, Cal_type_is_refl, &
     ixstart, ixstop, iystart, iystop, level1b_buffer, verbose)

   use imager_structures
   use preproc_constants

   implicit none

   include "hdf.f90"
   include "dffunc.f90"

   integer(kind=lint), intent(in)    :: fid
   integer,            intent(in)    :: band
   logical,            intent(in)    :: Cal_type_is_refl
   integer(kind=lint), intent(in)    :: ixstart, ixstop, iystart, iystop
   real(kind=sreal),   intent(inout) :: level1b_buffer(ixstart:ixstop, &
        iystart:iystop)
   logical,            intent(in)    :: verbose

   integer(kind=lint)         :: ix, jy
   character(len=MAX_NC_NAME) :: SDS_name, SDS_name_2
   character(len=128)         :: Dim_band_index
   integer(kind=lint)         :: file_id, attr_id, var_id, err_code
   character(len=MAX_NC_NAME) :: tmpname
   integer                    :: rank, type, num_attrs
   integer, dimension(3)      :: dimsizes
   integer                    :: number_of_bands
   character(len=MAX_NC_NAME) :: band_names
   logical                    :: flag
   integer                    :: iband,comma_i,comma_i_old
   integer                    :: band_name_length,current_band
   integer(kind=sint)         :: fv, vr(2)
   real(kind=sreal)           :: scale_factors(20), offsets(20)
   integer(kind=lint)         :: start(3), stride(3), edge(3)
   integer(kind=sint)         :: temp(ixstart:ixstop,iystart:iystop)

   if (verbose) write(*,*) '<<<<<<<<<<<<<<< Entering read_modis_l1b_radiances_2()'

   if (verbose) write(*,*) 'band: ',             band
   if (verbose) write(*,*) 'Cal_type_is_refl: ', Cal_type_is_refl
   if (verbose) write(*,*) 'ixstart: ',          ixstart
   if (verbose) write(*,*) 'ixstop: ',           ixstop
   if (verbose) write(*,*) 'iystart: ',          iystart
   if (verbose) write(*,*) 'iystop: ',           iystop

   if (band >= 1 .and. band <= 2) then
      SDS_name = "EV_250_Aggr1km_RefSB"
      SDS_name_2 = "EV_250_Aggr1km_RefSB_Uncert_Indexes"
      Dim_band_index = "Band_250M"
   end if
   if (band >= 3 .and. band <= 7) then
      SDS_name = "EV_500_Aggr1km_RefSB"
      SDS_name_2 = "EV_500_Aggr1km_RefSB_Uncert_Indexes"
      Dim_band_index = "Band_500M"
   end if
   if (band >= 8 .and. band<= 19 .or. band == 26) then
      SDS_name = "EV_1KM_RefSB"
      SDS_name_2 = "EV_1KM_RefSB_Uncert_Indexes"
      Dim_band_index = "Band_1KM_RefSB"
   end if
   if (band >= 20 .and. band <= 36 .and. band /= 26) then
      SDS_name = "EV_1KM_Emissive"
      SDS_name_2 = "EV_1KM_Emissive_Uncert_Indexes"
      Dim_band_index = "Band_1KM_Emissive"
   end if

   if (verbose) then
      write(*,*), 'SDS_name: ', trim(SDS_name)
      write(*,*), 'SDS_name_2: ', trim(SDS_name_2)
      write(*,*), 'Dim_band_index: ', trim(Dim_band_index)
   end if

   file_id = fid
   var_id = sfselect(file_id, sfn2index(file_id, SDS_name))
   err_code = sfginfo(var_id, tmpname, rank, dimsizes, type, num_attrs)

   number_of_bands = dimsizes(3)

   if (verbose) then
      write(*,*), 'sfginfo() output',trim(adjustl(SDS_name_2))
      write(*,*), '   name: ', trim(adjustl(tmpname))
      write(*,*), '   rank: ', rank
      write(*,*), '   dimsizes: ', dimsizes
      write(*,*), '   type: ', type
      write(*,*), '   nattrs: ', num_attrs
      write(*,*), 'Number of MODIS bands: ', number_of_bands
   end if

   band_names=''
   attr_id=sffattr(var_id, "band_names")
   err_code=sfrattr(var_id, attr_id, band_names)
   band_names=trim(adjustl(band_names))

   ! remove NULL characters
   comma_i=index(band_names, achar(0))
   do while (comma_i .gt. 0)
      band_names(comma_i:comma_i) = ' '
      comma_i=index(band_names, achar(0))
   end do

   if (verbose) write(*,*), 'Bands found: ', trim(band_names)

   flag = .true.
   comma_i_old=1
   band_name_length=len_trim(band_names)

   do iband=1,number_of_bands
      if (iband .eq. number_of_bands) then
         read(band_names(comma_i_old:band_name_length), '(i2)') current_band
      else
         comma_i=index(trim(adjustl(band_names(comma_i_old:band_name_length))),&
              ',')+comma_i_old-1
         read(band_names(comma_i_old:comma_i-1), '(i6)') current_band
         comma_i_old=comma_i+1
      end if

      if (current_band .eq. band) then
         flag = .false.
         if (verbose) write(*,*), 'Selected band is: ',current_band,iband
         exit
      end if
   end do

   if (flag) then
      write(*,*) 'ERROR: read_modis_l1b_radiances_2(): Band: ', band, &
                 ' not found in MODIS M*D02 file!'
      stop error_stop_code
   end if

   ! data stored with 0 offset
   start(2) = iystart-1
   start(1) = ixstart-1
   start(3) = iband-1

   stride = 1

   edge(1) = ixstop-ixstart+1
   edge(2) = iystop-iystart+1
   edge(3) = 1

   attr_id=sffattr(var_id, "_FillValue")
   err_code=sfrattr(var_id, attr_id, fv)

   attr_id=sffattr(var_id, "valid_range")
   err_code=sfrattr(var_id, attr_id, vr)

   if (Cal_type_is_refl) then
      attr_id = sffattr(var_id, "reflectance_scales")
      err_code = sfrattr(var_id, attr_id, scale_factors)
      attr_id = sffattr(var_id, "reflectance_offsets")
      err_code = sfrattr(var_id, attr_id, offsets)

   else
      attr_id = sffattr(var_id, "radiance_scales")
      err_code = sfrattr(var_id, attr_id, scale_factors)
      attr_id = sffattr(var_id, "radiance_offsets")
      err_code = sfrattr(var_id, attr_id, offsets)
   end if

   var_id = sfselect(file_id, sfn2index(file_id, SDS_name))
   err_code = sfrdata(var_id, start, stride, edge, temp)

   do jy=iystart,iystop
      do ix=ixstart,ixstop
         if (temp(ix,jy) .ge. vr(1) .and. temp(ix,jy) .le. vr(2)) then
            level1b_buffer(ix,jy) = (real(temp(ix,jy),kind=sreal) - &
                 offsets(iband)) * scale_factors(iband)
         else
            level1b_buffer(ix,jy) = sreal_fill_value
         end if
      end do
   end do

   err_code = sfendacc(var_id)

   if (verbose) write(*,*) '>>>>>>>>>>>>>>> Leaving read_modis_l1b_radiances_2()'

end subroutine read_modis_l1b_radiances_2
