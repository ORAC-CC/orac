! Name: get_surface_emissivity.F90
!
! Purpose:
! Loads land surface emissivity data, from the CIMSS global land surface
! IR emissivity data (University of Wisconsin), and uses it to populate both the
! instrument grid (surface structure) and pre-processing grid (preproc_surf)
!
! Description and algorithm details:
!
! Arguments:
! Name            Type      In/Out/Both Description
! ------------------------------------------------------------------------------
! emissivity_path    char   in   Path to CIMSS emis_inf10_month data
! imager_flags       struct in   Imager structure containing land/sea flag
! imager_geolocation struct in   Imager structure containing lat/lon points
! preproc_dims       struct in   Preprocessing dimensions, including sw and lw
!                                channel counts
! surface            struct both Surface properties structure
! preproc_surf       struct both Preproc surface properties structure
!
! History:
! 30/05/2012 Gareth Thomas. Finished first version
! 31/05/2012 MJ fixes: sum(emis%emissivity(k,preloni(j):preloni(j+1),prelati(i):prelat(i+1))) / &
!                      real(size(emis%emissivity(k,preloni(j):preloni(j+1),prelati(i):prelat(i+1))))
!               with:  sum(emis%emissivity(k,preloni(j):preloni(j+1),prelati(i):prelati(i+1))) / &
!                      real(size(emis%emissivity(k,preloni(j):preloni(j+1),prelati(i):prelati(i+1))))
!               to make code compile with sun compiler.
! 26/06/2012 CP updated preproc_dims with channel_info struture variables
! 08/08/2012 CP changed format of emis array to be consistent with msi order
! 08/08/2012 CP fixed bug channel_info was not in arguments
! 08/08/2012 CP changed bilinear to nearestneighbout interpolation, also
!               swicthed lats and lons around in interpolatio subroutine
!               initialised allocate arrays
! 25/02/2013 GT preproc_geoloc arrays are now used directly in averaging the
!               emissivity onto the preproc (rtm) grid. As a consequence,
!               preproc_gelolc added to the argument list
! 06/03/2013 GT Small tidy up of code.
! 15/03/2013 GT Reinstated bilinear interpolation, as new version is nearly as
!               fast as nearest neighbour.
!               Fixed a bug with indexing of surface%emissivity
! 16/10/2013 GM get_surface_emissivity() was using preproc_dims%dellat for the
!               longitude center to edge transformation.  Changed it to use
!               preproc_dims%dellon and reordered the statements a bit.
! 16/10/2013 GM Fixed indexing bug in get_surface_emissivity() when finding
!               index numbers in the emissivity lat-lon grid that correspond to
!               each of the grid cells in the preproc grid.
! 05/11/2013 GM Removed my commented out original fixes for those of Matthias
!               and otherwise fixed a small memory leak and cleaned up the code.
! 10/02/2014 AP Variable renaming
!
! $Id$
!
! Bugs:
! none known
!

subroutine get_surface_emissivity(emissivity_path, imager_flags, &
     imager_geolocation, channel_info, &
     preproc_dims, preproc_geoloc, surface, &
     preproc_surf)

   use preproc_constants
   use preproc_structures
   use channel_structures
   use imager_structures
   use surface_structures
   use emis_def
   use interpol_bilinear_def
   use interpol_nearest_neighbour_def

   implicit none

   interface
      function read_cimss_emissivity(path_to_file, emis, bands, flag, wavenumber, loc) &
           result (stat)

         use preproc_constants
         use emis_def
         use netcdf

         implicit none

         ! Input variables
         character(len=pathlength)           :: path_to_file
         integer(kind=stint),  dimension(:)  :: bands
         integer(kind=stint),       optional :: flag, wavenumber
         character(len=pathlength), optional :: loc

         ! Output variables
         type(emis_s)                        :: emis
         integer(kind=stint)                 :: stat
      end function read_cimss_emissivity
   end interface

   ! Input/output variables
   character(len=pathlength),  intent(in)          :: emissivity_path
   type(imager_flags_s),       intent(in)          :: imager_flags
   type(imager_geolocation_s), intent(in)          :: imager_geolocation
   type(channel_info_s),       intent(in)          :: channel_info
   type(preproc_dims_s),       intent(in)          :: preproc_dims
   type(preproc_geoloc_s),     intent(in)          :: preproc_geoloc
   type(surface_s),            intent(inout)       :: surface
   type(preproc_surf_s),       intent(inout)       :: preproc_surf

   ! Local variables
   type(emis_s)                                       :: emis
   integer(kind=stint),              dimension(3)     :: embands
   integer(kind=stint), allocatable, dimension(:)     :: bands
   real(kind=sreal),    allocatable, dimension(:)     :: datlat, datlon, emlat, emlon
   real(kind=sreal)                                   :: latstep, lonstep
   real(kind=sreal),    allocatable, dimension(:,:,:) :: transemis
   real(kind=sreal),    allocatable, dimension(:,:)   :: datemis
   real(kind=sreal),    allocatable, dimension(:)     :: prelat, prelon
   integer(kind=lint),  allocatable, dimension(:)     :: prelati, preloni
   integer(kind=stint)                                :: stat
   integer(kind=lint)                                 :: i,j,k
   integer(kind=stint)                                :: nlwchannels
   integer(kind=lint)                                 :: nland, lndcount
   integer(kind=lint)                                 :: location1d(1)
   integer(kind=lint)                                 :: sum_counter,ii,jj
   real(kind=sreal)                                   :: summy

   ! Count the number of land and sea pixels, using the imager land/sea mask
   nland = count(imager_flags%lsflag .eq. 1)
   write(*,*) 'channel_info%channel_ids_abs: ',channel_info%channel_ids_abs
   nlwchannels = count(channel_info%channel_ids_abs .gt. 3)
   write(*,*)'nland, nlwchannels: ',nland, nlwchannels

   ! If there are no land pixels in the scene, we have nothing more to do
   if (nland .eq. 0) return

   ! Allocate local arrays for lat, lon
   allocate(datlat(nland))
   allocate(datlon(nland))
   allocate(datemis(nlwchannels,nland))
   datlat=real_fill_value
   datlon=real_fill_value
   datemis=real_fill_value

   ! Extract only the land pixels from the imager structures
   lndcount = 1
   do i=1,imager_geolocation%ny
      do j=imager_geolocation%startx,imager_geolocation%endx
         if (imager_flags%lsflag(j,i) .eq. 1) then
            datlat(lndcount) = imager_geolocation%latitude(j,i)
            datlon(lndcount) = imager_geolocation%longitude(j,i)
            lndcount = lndcount+1
         end if
      end do
   end do

   ! Read the emissivity data - file contains data for all of the MODIS thermal
   ! bands. We need to reference these in terms of the legacy channels: 3.7, 11,
   ! 12 microns
   embands = (/ 1_stint, 8_stint, 9_stint /)
   ! So, which of these have been selected in the preproc_dims%channels index?

   allocate(bands(nlwchannels))
   bands=0

   ! embands is modis numbering of IR channels where,
   ! channel_info%channel_ids_abs .gt. 3
   bands(:) = embands
   write(*,*)'channel_info%channel_ids_abs: ',channel_info%channel_ids_abs
   write(*,*)'embands: ',embands
   write(*,*)'bands: ',bands

   ! Read the data itself
   stat = read_cimss_emissivity(emissivity_path, emis, bands, flag=1_stint)

   ! Resort the emission data so that latitude runs from -90 to 90, rather than
   ! 90 to -90
   allocate(transemis(emis%nbands,emis%nlon,emis%nlat))
   do i=1,emis%nbands
      transemis(i,:,:) = transpose(emis%emissivity(i,:,:))
   end do

   ! Note that we have not read the location (or wavenumber) data. Generate the
   ! cimss lat-lon grid here:
   allocate(emlat(emis%nlat))
   allocate(emlon(emis%nlon))
   emlat=real_fill_value
   emlon=real_fill_value

   latstep = 180.0 / real(emis%nlat)
   ! interpol__nearest_neighbour expects the "y" dimension to be upside down...
   emlat = 90. - (latstep*real((/ (i,i=1,emis%nlat) /)) - latstep/2.0)
   lonstep = 360.0 / real(emis%nlon)
   emlon = lonstep*real((/ (i,i=1,emis%nlon) /)) - 180. - lonstep/2.0

   ! This emissivity data has very few missing values, but there are some. Set
   ! these to 0.999 (as close to 1 as the emissitivty data itself gets). This is
   ! also a vaguely reasonable value for the water as well....
   where(transemis .le. 0.0) transemis = 0.999

   ! Now interpolate this data onto both the data grid and the RTTOV "preproc"
   ! grid
   do i=1,nlwchannels
      ! Can use either bilinear or nearest neighbour...

      !    call interpol_nearest_neighbour(emlon, emlat, transemis(i,:,:), &
      !                                    datlon, datlat, datemis(i,:))

      call interpol_bilinear(emlon, emlat, transemis(i,:,:), &
           datlon, datlat, datemis(i,:))
   end do

   ! Now copy the values out of the 2D datemis array, into the 3D array contained
   ! in the surface structure
   lndcount=1
   do i=1,imager_geolocation%ny
      do j=imager_geolocation%startx,imager_geolocation%endx
         if (imager_flags%lsflag(j,i) .ne. 0) then
            surface%emissivity(j,i,:) = datemis(:,lndcount)
            lndcount = lndcount+1
         end if
      end do
   end do

   ! For the much lower resolution RTTOV grid we calculate the mean in each
   ! lat-lon grid box. For this we need to define the preproc lat/lon grid box
   ! edges
   allocate(prelat(preproc_dims%ydim+1))
   allocate(prelon(preproc_dims%xdim+1))
   allocate(prelati(preproc_dims%ydim+1))
   allocate(preloni(preproc_dims%xdim+1))
   prelat=real_fill_value
   prelon=real_fill_value
   prelati=long_int_fill_value
   preloni=long_int_fill_value

   ! The latitude/longitude arrays in preproc_geoloc store the bin centres, so we
   ! need to convert to the edges.
   ! Note that, strangely, dellat is defined as 1./(change in lat) in
   ! define_proproc_grid. Likewise dellon
   prelat(:preproc_dims%ydim) = preproc_geoloc%latitude - 0.5/preproc_dims%dellat
   prelat(preproc_dims%ydim+1) = preproc_geoloc%latitude(preproc_dims%ydim) &
        & + 0.5/preproc_dims%dellat

   prelon(:preproc_dims%xdim) = preproc_geoloc%longitude - 0.5/preproc_dims%dellon
   prelon(preproc_dims%xdim+1) = preproc_geoloc%longitude(preproc_dims%xdim) &
        & + 0.5/preproc_dims%dellon

   ! Define the index numbers in the emissivity lat-lon grid with correspond to
   ! each of the grid cells in the preproc grid
   location1d=minloc(emlat, mask=((emlat .ge. prelat(1)) .and. &
        & (emlat .lt. prelat(2))))
   prelati(1)=location1d(1)
   do i=1,preproc_dims%ydim
      location1d=maxloc(emlat, mask=((emlat .ge. prelat(i)) .and. &
           & (emlat .lt. prelat(i+1))))
      prelati(i+1) =location1d(1)
   end do

   location1d=minloc(emlon, mask=((emlon .ge. prelon(1)) .and. &
        & (emlon .lt. prelon(2))))
   preloni(1) = location1d(1)
   do i=1,preproc_dims%xdim
      location1d= maxloc(emlon, mask=((emlon .ge. prelon(i)) .and. &
           & (emlon .lt. prelon(i+1))))
      preloni(i+1) =location1d(1)
   end do


   ! Now calculate the mean emissivity in each preproc grid
   do i=1,preproc_dims%ydim
      do j=1,preproc_dims%xdim
         do k=1,nlwchannels
            summy=0.00_sreal
            sum_counter=0_lint
            do ii=i,i+1
               do jj=j,j+1
                  if(transemis(k,preloni(jj),prelati(ii)) .ge. 0.00) then
!!$                    write(*,*) size(preloni)
!!$                    write(*,*) size(prelati)
!!$                    write(*,*) i,j,k
                     summy=summy+transemis(k,preloni(jj),prelati(ii))
                     sum_counter=sum_counter+1_lint
                  endif
               enddo
            enddo

            if(sum_counter .gt. 0) then
               preproc_surf%emissivity(j,i,k)=summy/sum_counter
            else
               preproc_surf%emissivity(j,i,k)=real_fill_value
            endif
         end do
      end do
   end do

   deallocate(datlat)
   deallocate(datlon)
   deallocate(datemis)
   deallocate(bands)
   deallocate(transemis)
   deallocate(emlat)
   deallocate(emlon)
   deallocate(prelat)
   deallocate(prelon)
   deallocate(prelati)
   deallocate(preloni)

end subroutine get_surface_emissivity
