! Name: read_L1B_radiances_reflectances.F90
!
!
! Purpose:
! Read the radiance values from an already open MODIS M*D02!KM file
! 
! Description and Algorithm details:
! 1) Select appropriate resolution band for requested channel.
! 2) Determine which channels are available in the file for that band.
! 3) Select the requested channel from among those (band_index).
! 4) Read data, valid range, scale factors, and offsets.
! 5) Apply scale factor and offset to data.
!
! Arguments:
! Name Type In/Out/Both Description
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
! Local variables:
! Name Type Description
!
!
! History:
! 2011/12/??: MJ First version
! 2013/03/22: GT Added code to assign the band index number dynamically 
!                using information contained in the M*D02 file itself.
! 2013/09/12: AP tidying, added where statement
! 2013/10/15: MJ changes reading of band names for MODIS.
! 2013/10/22: AP when a field in the spectrally subsetted files contained only
!                one channel, the band_names field ended in NULL characters
!                which Fortran could not manage. Those have been removed.
! 2013/11/05: GM Moved the verbose statement that prints band_names from just 
!                before Adam's removing of NULL characters to just after as some
!                text operations can be affected by them.
!
! $Id$
!
! Bugs:
! none known
!

!-------------------------------------------------------
!-------------------------------------------------------
subroutine read_L1B_modis_reflectances_radiances(fid, band, Cal_type_is_refl, &
     ixstart, ixstop, iystart, iystop, level1b_buffer, verbose)

   use preproc_constants
   use imager_structures

   implicit none

   include "hdf.f90"
   include "dffunc.f90"

   integer(kind=lint), intent(in)  :: fid
   logical, intent(in)             :: Cal_type_is_refl, verbose
   integer, intent(in)             :: band
   real(kind=sreal), intent(inout) :: level1b_buffer(ixstart:ixstop,iystart:iystop)

   integer(kind=lint)    :: file_id, var_id, err_code, start(3), stride(3), &
        edge(3), attr_id
   real(kind=sreal)      :: scale_factors(20), offsets(20)
   real(kind=sreal)      :: spec_unc(20), unc_scale(20)
   character(len=100)    :: SDS_name, SDS_unc_name, Dim_band_index,band_names
   integer               :: number_of_bands,iband,comma_i,comma_i_old,band_name_length,current_band
   character(len=100)    :: tmpname
   integer               :: tmprank, tmptype, tmpnattrs
!MJORG   integer, dimension(1) :: tmpdimsizes
   integer, dimension(3) :: tmpdimsizes
   real(kind=sreal), allocatable, dimension(:) :: band_numbers
   integer               :: band_index
   integer(kind=lint)    :: ixstart, ixstop, iystart, iystop, ix, jy
        
   integer(kind=stint)   :: temp(ixstart:ixstop,iystart:iystop), &
        fv, vr(2)

   if (band >= 1 .and. band <= 2) then 
      SDS_name = "EV_250_Aggr1km_RefSB"
      SDS_unc_name = "EV_250_Aggr1km_RefSB_Uncert_Indexes"
      Dim_band_index = "Band_250M"
   endif
   if (band >= 3 .and. band <= 7) then
      SDS_name = "EV_500_Aggr1km_RefSB" 
      SDS_unc_name = "EV_500_Aggr1km_RefSB_Uncert_Indexes"
      Dim_band_index = "Band_500M"
   endif
   if (band >= 8 .and. band<= 19 .or. band == 26) then
      SDS_name = "EV_1KM_RefSB" 
      SDS_unc_name = "EV_1KM_RefSB_Uncert_Indexes"
      Dim_band_index = "Band_1KM_RefSB"
   endif
   if (band >= 20 .and. band <= 36 .and. band /= 26) then 
      SDS_name = "EV_1KM_Emissive"
      SDS_unc_name = "EV_1KM_Emissive_Uncert_Indexes"
      Dim_band_index = "Band_1KM_Emissive"
   endif

   ! first we need to find where to start
   ! GT Calculate the band index using the Band number SDS's contained
   !    within in the file (rather than hardwiried index numbers)
   file_id = fid
   !MJ ORGvar_id = sfselect(file_id, sfn2index(file_id, Dim_band_index))
   var_id = sfselect(file_id,  sfn2index(file_id, SDS_name))
   ! Extract the number of bands in this group, the dimension of Dim_band_index
   err_code = sfginfo(var_id, tmpname, tmprank, tmpdimsizes, tmptype, tmpnattrs)

   number_of_bands = tmpdimsizes(3)
   if(verbose) then
      print*, 'sfginfo on ',trim(adjustl(Dim_band_index))
      print*, 'tmpname: ', trim(adjustl(tmpname))
      print*, 'tmprank: ', tmprank
      print*, 'tmpdimsizes: ', tmpdimsizes
      print*, 'tmptype: ', tmptype
      print*, 'tmpnattrs: ',tmpnattrs
      print*, 'Band wanted: ',band
      print*, 'Number of MODIS bands: ',number_of_bands
   endif

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

   if(verbose) print*, 'Bands found: ',trim(band_names)

   band_index=0
   comma_i_old=1
   band_name_length=len_trim(band_names)

   do iband=1,number_of_bands
      if(iband .eq. number_of_bands) then 
         read(band_names(comma_i_old:band_name_length), '(i2)') current_band

      else
         comma_i=index(trim(adjustl(band_names(comma_i_old:band_name_length))),&
              ',')+comma_i_old-1
         read(band_names(comma_i_old:comma_i-1), '(i6)') current_band
         comma_i_old=comma_i+1
      endif

      if(current_band .eq. band) then
         band_index=band_index+1
         if(verbose) print*, 'Selected band is: ',current_band,' ',iband    

         exit
      endif
   enddo

   if (band_index .eq.0) then
      write(*,*) 'Band ',band,' not found in MODIS M*D02 file!'
      stop
   endif
   

   start(2) = iystart-1
   start(1) = ixstart-1
   !MJ ORG start(3) = band_index-1 ! data stored with 0 offset
   start(3) = iband-1 ! data stored with 0 offset

   stride = 1

   edge(1) = ixstop-ixstart+1
   edge(2) = iystop-iystart+1
   edge(3) = 1

   var_id = sfselect(file_id, sfn2index(file_id, SDS_name))

   err_code = sfrdata(var_id, start, stride, edge, temp)

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

   endif

!   where(temp.ge.vr(1) .and. temp.le.vr(2))
!      level1b_buffer = (real(temp,kind=sreal) - offsets(band_index)) * &
!           scale_factors(band_index)
!   end where
   do ix=ixstart,ixstop
      do jy=iystart,iystop
         if(temp(ix,jy) .ge. vr(1) .and. temp(ix,jy) .le. vr(2)) then
            level1b_buffer(ix,jy) = (real(temp(ix,jy),kind=sreal) - &
                 offsets(band_index)) * scale_factors(band_index)
          else
             level1b_buffer(ix,jy) = real_fill_value
          endif
      enddo
   enddo

   err_code = sfendacc(var_id)

end subroutine read_L1B_modis_reflectances_radiances

