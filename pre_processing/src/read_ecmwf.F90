! Name: read_ecmwf.f90
!
!
! Purpose:
! Read ECMWF ERA Interim grib files.
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
!2012/01/10: Matthias Jerg produces code which reads required parameters from ERA INTERIM GRIB files
!2012/03/26: MJ implements 3D pressure array code.
!2012/05/04: MJ fixes a bug with the mapping of the grids.
!2012/06/29: MJ implements comutation of geopotential height.
!2012/08/06: CP modified so it deals with BADC style ecmwf files, improved readbility
!2013/03/06: CP modified to rearrange grib files 
!2013/10/29: CP changed array definition of phi_lay and phi_lev
!2013/02/04: MJ fixes typo and indents some lines properly.
!
! $Id$
!
! Bugs:
!
!none known

!---------------------------------------------------
!---------------------------------------------------
subroutine read_ecmwf_grib(ecmwf_path,ecmwf_dims,ecmwf_3d,ecmwf_2d,preproc_dims,preproc_geoloc,preproc_prtm)
   !---------------------------------------------------
   !---------------------------------------------------

   use grib_api
   use preproc_constants
   use preproc_structures
   use ecmwf_structures
   use rearrange

   implicit none

   character(len=pathlength) :: ecmwf_path

   character(len=errlength) :: err_msg

   type(ecmwf_dims_s) :: ecmwf_dims
   type(ecmwf_3d_s) :: ecmwf_3d
   type(ecmwf_2d_s) :: ecmwf_2d

   type(preproc_geoloc_s) :: preproc_geoloc
   type(preproc_dims_s) :: preproc_dims
   type(preproc_prtm_s) :: preproc_prtm

   real(kind=sreal), allocatable, dimension(:) :: dummyvector_lat,dummyvector_lon,dummyvector_value

   integer(kind=lint) :: gribid,igrib,iret,parameter,status,ilevel

   logical :: lgetindices,lnn

   real(kind=dreal), allocatable, dimension(:) :: shiftlat,shiftlon

   !near neighbor
   real(kind=dreal), allocatable, dimension(:,:) :: nnoutlat,nnoutlon,nnvalue,nndistance
   integer(kind=kindOfInt), allocatable, dimension(:,:) :: nnindex

   !interpolation
   real(kind=dreal), allocatable, dimension(:,:,:) :: intoutlats,intoutlons,intvalues,intdistances
   integer(kind=kindOfInt), allocatable, dimension(:,:,:) :: intindexes

   integer(kind=lint) :: idim,jdim,ik

   integer(kind=lint) :: PVPresent,nb_pv
   integer :: dim1,dim2

   real(kind=sreal), allocatable, dimension(:) :: pv_dummy,avector,bvector,spec_hum,temperature,phi_lev,phi_lay
   real(kind=sreal) :: geopot,sp


   !set switches
   lgetindices=.true.
   !lnn is set here locally, needs to be passed from the surrouding environment later!
   lnn=.true. !do nearest neighbor
   !lnn=.false. !do real 4-point averaging, is very very slow at the moment!

   !open the grib file for reading
   call grib_open_file(gribid,trim(ecmwf_path),'r',status)

   !everything OK?
   if(status .ne. 0 ) then
      call grib_get_error_string(status,err_msg)
      write(*,*) 'GRIB API ERROR OPENING FILE:', trim(ecmwf_path),'msg:', trim(err_msg)
   endif

   !load the grib message?
   call grib_new_from_file(gribid,igrib,iret)

   !everything OK?
   if(iret .ne. 0 ) then
      call grib_get_error_string(status,err_msg)
      write(*,*) 'GRIB API ERROR GETTING GRIB_ID:',  trim(err_msg)
   endif

   !allocate dummy placeholder fields for reading ecmwf data
   allocate(dummyvector_value(1:ecmwf_dims%xdim*ecmwf_dims%ydim))
   dummyvector_value=real_fill_value
   allocate(dummyvector_lat(1:ecmwf_dims%xdim*ecmwf_dims%ydim))
   dummyvector_lat=real_fill_value
   allocate(dummyvector_lon(1:ecmwf_dims%xdim*ecmwf_dims%ydim))
   dummyvector_lon=real_fill_value

   allocate(avector(1:ecmwf_dims%kdim+1))
   avector=real_fill_value
   allocate(bvector(1:ecmwf_dims%kdim+1))
   bvector=real_fill_value

   !this loops over the stored data. data is stored per level and variable, so this loops over levels and variables simultaneously
   !the variables loop faster than the levels
   !in horizontal direction data is stored on a per pixel basis, meaning it is not stored in an 2D array structure but rather as
   !datapoint after datapoint, therefore only 1D arrays with the total number of pixels can be decoded (aka "read").

   !resolution of preprocessing grid is either identical with ecmwf grid or has higher resolution 
   !=> interpolate ecmwf data to this higher resolution grid. 
   !Even if resolution remains unchanged, preprocessing grid is defined at cell centres,
   !thus interpolation of data from cell corners (original grib definition) to centres is necessary

   do while(iret .ne. grib_end_of_file)

      !get the current level
      call grib_get(igrib,'level',ilevel,status)
      !everything OK?
      if(status .ne. 0 ) then
         call grib_get_error_string(status,err_msg)
         write(*,*) 'GRIB API ERROR GETTING LEVEL:',  trim(err_msg)
      endif


      !read the parameter number of the variable which is just processed by the loop
      call grib_get(igrib,'parameter',parameter,status)
      write(*,*)'grib parameter',parameter
      !everything OK?
      if(status .ne. 0 ) then
         call grib_get_error_string(status,err_msg)
         write(*,*) 'GRIB API ERROR GETTING PARAMETER:',  trim(err_msg)
      endif




      !read the data
      call grib_get_data(igrib,dummyvector_lat,dummyvector_lon,dummyvector_value,status)
      !write(*,*)'dummyvector_value',dummyvector_value

      !write(*,*)'igrib',igrib


      !everything OK?
      if(status .ne. 0 ) then
         call grib_get_error_string(status,err_msg)
         write(*,*) 'GRIB API ERROR GETTING DATA:',  trim(err_msg)
      endif


      !get the indices for the interpolation/nearest neighbor
      if(lgetindices) then
         write(*,*) 'Get indices'
         !allocate temporary fields to hold the preproc grid in the succession and ordering of the ecmwf grid

         allocate(shiftlon(preproc_dims%xdim))
         shiftlon=double_fill_value
         allocate(shiftlat(preproc_dims%ydim))
         shiftlat=double_fill_value

         !shift preproc grid to comply with ecmwf grid ordering !now done differently, see below!
         !        shiftlon=dble(preproc_geoloc%longitude+preproc_dims%lon_offset)
         !        shiftlat=dble(-preproc_geoloc%latitude)

         !get only one nearest point (near neighbor) for each input point
         if(lnn) then

            allocate(nnoutlat(preproc_dims%xdim,preproc_dims%ydim))
            nnoutlat=double_fill_value
            allocate(nnoutlon(preproc_dims%xdim,preproc_dims%ydim))
            nnoutlon=double_fill_value
            allocate(nnvalue(preproc_dims%xdim,preproc_dims%ydim))
            nnvalue=double_fill_value
            allocate(nndistance(preproc_dims%xdim,preproc_dims%ydim))
            nndistance=double_fill_value
            allocate(nnindex(preproc_dims%xdim,preproc_dims%ydim))
            nnindex=int(long_int_fill_value,kind=kindOfInt)

            !           do idim=1,preproc_dims%xdim
            !              do jdim=1,preproc_dims%ydim
            do jdim=preproc_dims%min_lat,preproc_dims%max_lat
               do idim=preproc_dims%min_lon,preproc_dims%max_lon

                  !MST included the following IF to shorten this here a little
                  !to have this variable available here I switch the calls of read_ecmwf_grib()
                  !and build_preproc_fields() in preprocerssinf_for_orac

                  if(preproc_dims%filter_array_lw(idim,jdim) .eq. 1) then

                     if(preproc_geoloc%longitude(idim) .lt. 0.00) then
                        shiftlon(idim)=dble(preproc_geoloc%longitude(idim)+360.00)
                     else
                        shiftlon(idim)=dble(preproc_geoloc%longitude(idim))
                     endif
                     shiftlat(jdim)=dble(preproc_geoloc%latitude(jdim))


                     !OBS BUG                 shiftlon(idim)=dble(preproc_geoloc%longitude(idim)+preproc_dims%lon_offset)
                     !OBS BUG                 shiftlat(jdim)=dble(preproc_geoloc%latitude(jdim))

                     !write(*,*) 'ij',idim,jdim,shiftlon(idim),shiftlat(jdim)

                     call grib_find_nearest_single(igrib,.false.,shiftlat(jdim),shiftlon(idim),&
                          & nnoutlat(idim,jdim),nnoutlon(idim,jdim),&
                          & nnvalue(idim,jdim),nndistance(idim,jdim),nnindex(idim,jdim),&
                          & status)

                     if(status .ne. 0 ) then
                        call grib_get_error_string(status,err_msg)
                        write(*,*) 'GRIB API ERROR FINDING NEAREST SINGLE:',  trim(err_msg)
                     endif

!!$                 write(*,*) 'idim,jdim', idim,jdim,shiftlon(idim),shiftlat(jdim),nnindex(idim,jdim),nndistance(idim,jdim),&
!!$                      & dummyvector_lon(nnindex(idim,jdim)+1),dummyvector_lat(nnindex(idim,jdim)+1)

                  endif

               enddo
            enddo

            !get all four nearest points (in order to do a full bilinear interpolation/averaging)
         else

            allocate(intoutlats(preproc_dims%xdim,preproc_dims%ydim,1:4))
            intoutlats=double_fill_value
            allocate(intoutlons(preproc_dims%xdim,preproc_dims%ydim,1:4))
            intoutlons=double_fill_value
            allocate(intvalues(preproc_dims%xdim,preproc_dims%ydim,1:4))
            intvalues=double_fill_value
            allocate(intdistances(preproc_dims%xdim,preproc_dims%ydim,1:4))
            intdistances=double_fill_value
            allocate(intindexes(preproc_dims%xdim,preproc_dims%ydim,1:4))
            intindexes=int(long_int_fill_value,kind=kindOfInt)

            !           do idim=1,preproc_dims%xdim
            !              do jdim=1,preproc_dims%ydim
            do jdim=preproc_dims%min_lat,preproc_dims%max_lat
               do idim=preproc_dims%min_lon,preproc_dims%max_lon

                  if(preproc_geoloc%longitude(idim) .lt. 0.00) then
                     shiftlon(idim)=dble(preproc_geoloc%longitude(idim)+360.00)
                  else
                     shiftlon(idim)=dble(preproc_geoloc%longitude(idim))
                  endif
                  shiftlat(jdim)=dble(preproc_geoloc%latitude(jdim))

                  !                 write(*,*) 'ij',idim,jdim,shiftlon(idim),shiftlat(jdim)

                  call grib_find_nearest_four_single(igrib,.false.,shiftlat(jdim),shiftlon(idim),&
                       & intoutlats(idim,jdim,1:4),intoutlons(idim,jdim,1:4),&
                       & intvalues(idim,jdim,1:4),intdistances(idim,jdim,1:4),intindexes(idim,jdim,1:4),&
                       & status)

                  if(status .ne. 0 ) then
                     call grib_get_error_string(status,err_msg)
                     write(*,*) 'GRIB API ERROR FINDING NEAREST SINGLE:',  trim(err_msg)
                  endif

               enddo
            enddo


         end if



         !write(*,*) 'After finding nearest point(s)'

         !get the A,B for the vertical structure
         !query if the information is present in the file header
         call grib_get(igrib,'PVPresent',PVPresent)
         !get the number of entries:
         !REMARK: This should return nb_pv=122=61(= # model levels+1)*2
         !model levels are actually layer centres and there the archived fields are defined.
         !the pressure and hence A,B are defined at the interfaces.
         !The first 61 entries of pv_dummy are the As, the remianing 61 entries are the Bs.
         !They are contained in the header information of the grib file, thus constant in space,
         !thus there is no need to interpolate them to a different grid.
         !Now get the damned stuff:
         if(PVPresent .eq. 1) then
            !get the length of the pv_dummy array
            call grib_get_size(igrib,'pv',nb_pv)
            allocate(pv_dummy(nb_pv))
            pv_dummy=-999.0
            !get the pv array
            call grib_get(igrib,'pv',pv_dummy)

            !extract as and bs and store in seperate arrays
            avector=pv_dummy(1:ecmwf_dims%kdim+1)
            bvector=pv_dummy(ecmwf_dims%kdim+2:2*ecmwf_dims%kdim+2)
            deallocate(pv_dummy)

         endif

         !set logical to .false. cause it needs to be done only once as indices are stored!
         lgetindices=.false.

      endif



!!!!!!!!!!!!!!!!!!!
      !temperature profile
!!!!!!!!!!!!!!!!!!!!
      if(parameter .eq. 130) then

         ecmwf_3d%temperature(:,:,ilevel)=reshape(dummyvector_value, (/ecmwf_dims%xdim,ecmwf_dims%ydim/))

         !now do the actual filling of the preprocessing grid by:
         !nearest neighbor
         if(lnn) then

            call nn_ecmwf(preproc_dims,igrib,nnindex,nnvalue)
            preproc_prtm%temperature(:,:,ilevel)=real(nnvalue,kind=sreal)

            !four point average
         else

            call int_ecmwf(preproc_dims,igrib,intdistances,intindexes,intvalues)
            preproc_prtm%temperature(:,:,ilevel)=real(intvalues(:,:,1),kind=sreal)

         endif
!!!!!!!!!!!!!!!!
         !specific humidity
!!!!!!!!!!!!!!!!!
      elseif(parameter .eq. 133) then

         ecmwf_3d%spec_hum(:,:,ilevel)=reshape(dummyvector_value, (/ecmwf_dims%xdim,ecmwf_dims%ydim/))
         !write(*,*) 'spec_hum',ecmwf_3d%spec_hum(:,:,ilevel)
         !write(*,*) 'spec_hum'
         dim1=ecmwf_dims%xdim
         dim2=ecmwf_dims%ydim

         call rearrange_ecmwf(ecmwf_3d%spec_hum(:,:,ilevel),ecmwf_dims, dim1,dim2)

         !now do the actual filling of the preprocessing grid by:
         !nearest neighbor
         if(lnn) then

            call nn_ecmwf(preproc_dims,igrib,nnindex,nnvalue)
            preproc_prtm%spec_hum(:,:,ilevel)=real(nnvalue,kind=sreal)

            !four point average
         else

            call int_ecmwf(preproc_dims,igrib,intdistances,intindexes,intvalues)
            preproc_prtm%spec_hum(:,:,ilevel)=real(intvalues(:,:,1),kind=sreal)
         endif




!!!!!!!!!!!!!!!
         !ozone mass mixing ratio
!!!!!!!!!!!!!!!!
      elseif(parameter .eq. 203) then


         ecmwf_3d%ozone(:,:,ilevel)=reshape(dummyvector_value, (/ecmwf_dims%xdim,ecmwf_dims%ydim/))
         !write(*,*)'ozone',ecmwf_3d%ozone(:,:,ilevel)
         dim1=ecmwf_dims%xdim
         dim2=ecmwf_dims%ydim

         call rearrange_ecmwf(ecmwf_3d%ozone(:,:,ilevel),ecmwf_dims, dim1,dim2)


         !
         !now do the actual filling of the preprocessing grid by:
         !nearest neighbor
         !
         if(lnn) then

            call nn_ecmwf(preproc_dims,igrib,nnindex,nnvalue)
            preproc_prtm%ozone(:,:,ilevel)=real(nnvalue,kind=sreal)

            !four point average
         else

            call int_ecmwf(preproc_dims,igrib,intdistances,intindexes,intvalues)
            preproc_prtm%ozone(:,:,ilevel)=real(intvalues(:,:,1),kind=sreal)

         endif



!!!!!!!!!!!!!!!!
         !geopotential
!!!!!!!!!!!!!!!
      elseif(parameter .eq. 129) then

         ecmwf_2d%geopot(:,:)=reshape(dummyvector_value, (/ecmwf_dims%xdim,ecmwf_dims%ydim/))

         !now do the actual filling of the preprocessing grid by:
         !nearest neighbor
         if(lnn) then

            call nn_ecmwf(preproc_dims,igrib,nnindex,nnvalue)
            preproc_prtm%geopot(:,:)=real(nnvalue,kind=sreal)


            !four point average
         else

            call int_ecmwf(preproc_dims,igrib,intdistances,intindexes,intvalues)
            preproc_prtm%geopot(:,:)=real(intvalues(:,:,1),kind=sreal)

         endif

!!!!!!!!!!!!!!!!!!
         !logarithm of surface pressure
!!!!!!!!!!!!!!!!!
      elseif(parameter .eq. 152) then

         ecmwf_2d%lnsp(:,:)=reshape(dummyvector_value, (/ecmwf_dims%xdim,ecmwf_dims%ydim/))



         !now do the actual filling of the preprocessing grid by:
         !nearest neighbor
         if(lnn) then

            call nn_ecmwf(preproc_dims,igrib,nnindex,nnvalue)
            preproc_prtm%lnsp(:,:)=real(nnvalue,kind=sreal)

            !four point average
         else

            call int_ecmwf(preproc_dims,igrib,intdistances,intindexes,intvalues)
            preproc_prtm%lnsp(:,:)=real(intvalues(:,:,1),kind=sreal)

            write(*,*)' ec grib preproc_prtm%lnsp(:,:)', preproc_prtm%lnsp(:,:)
         endif
!!!!!!!!!!!!!!!!!!
         !sea ice cover 0-1
!!!!!!!!!!!!!!!!!
      elseif(parameter .eq. 31) then

         ecmwf_2d%sea_ice_cover(:,:)=reshape(dummyvector_value, (/ecmwf_dims%xdim,ecmwf_dims%ydim/))

         !now do the actual filling of the preprocessing grid by:
         !nearest neighbor
         if(lnn) then

            call nn_ecmwf(preproc_dims,igrib,nnindex,nnvalue)
            preproc_prtm%sea_ice_cover(:,:)=real(nnvalue,kind=sreal)

            !four point average
         else

            call int_ecmwf(preproc_dims,igrib,intdistances,intindexes,intvalues)
            preproc_prtm%sea_ice_cover(:,:)=real(intvalues(:,:,1),kind=sreal)

         endif
!!!!!!!!!!!!!!!!!!!
         !snow albedo 0-1
!!!!!!!!!!!!!!!!!!!!
      elseif(parameter .eq. 32) then

         ecmwf_2d%snow_albedo(:,:)=reshape(dummyvector_value, (/ecmwf_dims%xdim,ecmwf_dims%ydim/))

         !now do the actual filling of the preprocessing grid by:
         !nearest neighbor
         if(lnn) then

            call nn_ecmwf(preproc_dims,igrib,nnindex,nnvalue)
            preproc_prtm%snow_albedo(:,:)=real(nnvalue,kind=sreal)

            !four point average
         else

            call int_ecmwf(preproc_dims,igrib,intdistances,intindexes,intvalues)
            preproc_prtm%snow_albedo(:,:)=real(intvalues(:,:,1),kind=sreal)

         endif


         !        write(*,*) ecmwf_2d%snow_albedo
!!!!!!!!!!!!!!!!!!!!!!!!
         !sea surface temperature
!!!!!!!!!!!!!!!!!!!!!!!!
      elseif(parameter .eq. 34) then

         ecmwf_2d%sst(:,:)=reshape(dummyvector_value, (/ecmwf_dims%xdim,ecmwf_dims%ydim/))

         !now do the actual filling of the preprocessing grid by:
         !nearest neighbor
         if(lnn) then

            call nn_ecmwf(preproc_dims,igrib,nnindex,nnvalue)
            preproc_prtm%sst(:,:)=real(nnvalue,kind=sreal)

            !four point average
         else

            call int_ecmwf(preproc_dims,igrib,intdistances,intindexes,intvalues)
            preproc_prtm%sst(:,:)=real(intvalues(:,:,1),kind=sreal)

         endif

!!!!!!!!!!!!!!!!
         !total column water vapor
!!!!!!!!!!!!!!!!
      elseif(parameter .eq. 137) then

         ecmwf_2d%totcolwv(:,:)=reshape(dummyvector_value, (/ecmwf_dims%xdim,ecmwf_dims%ydim/))

         !now do the actual filling of the preprocessing grid by:
         !nearest neighbor
         if(lnn) then

            call nn_ecmwf(preproc_dims,igrib,nnindex,nnvalue)
            preproc_prtm%totcolwv(:,:)=real(nnvalue,kind=sreal)

            !four point average
         else

            call int_ecmwf(preproc_dims,igrib,intdistances,intindexes,intvalues)
            preproc_prtm%totcolwv(:,:)=real(intvalues(:,:,1),kind=sreal)

         endif
!!!!!!!!!!!!!!!!!!!!
         !snow depth
!!!!!!!!!!!!!!!!!!!!!!
      elseif(parameter .eq. 141) then

         ecmwf_2d%snow_depth(:,:)=reshape(dummyvector_value, (/ecmwf_dims%xdim,ecmwf_dims%ydim/))

         !now do the actual filling of the preprocessing grid by:
         !nearest neighbor
         if(lnn) then

            call nn_ecmwf(preproc_dims,igrib,nnindex,nnvalue)
            preproc_prtm%snow_depth(:,:)=real(nnvalue,kind=sreal)

            !four point average
         else

            call int_ecmwf(preproc_dims,igrib,intdistances,intindexes,intvalues)
            preproc_prtm%snow_depth(:,:)=real(intvalues(:,:,1),kind=sreal)

         endif


!!!!!!!!!!!!!!!!!!!!!!!
         !10m u wind component
!!!!!!!!!!!!!!!!!!!!!
      elseif(parameter .eq. 165) then

         ecmwf_2d%u10(:,:)=reshape(dummyvector_value, (/ecmwf_dims%xdim,ecmwf_dims%ydim/))

         !now do the actual filling of the preprocessing grid by:
         !nearest neighbor
         if(lnn) then

            call nn_ecmwf(preproc_dims,igrib,nnindex,nnvalue)
            preproc_prtm%u10(:,:)=real(nnvalue,kind=sreal)

            !four point average
         else

            call int_ecmwf(preproc_dims,igrib,intdistances,intindexes,intvalues)
            preproc_prtm%u10(:,:)=real(intvalues(:,:,1),kind=sreal)

         endif
!!!!!!!!!!!!!!!!!
         !10m v wind component
!!!!!!!!!!!!!!!!
      elseif(parameter .eq. 166) then

         ecmwf_2d%v10(:,:)=reshape(dummyvector_value, (/ecmwf_dims%xdim,ecmwf_dims%ydim/))


         !now do the actual filling of the preprocessing grid by:
         !nearest neighbor
         if(lnn) then

            call nn_ecmwf(preproc_dims,igrib,nnindex,nnvalue)
            preproc_prtm%v10(:,:)=real(nnvalue,kind=sreal)

            !four point average
         else

            call int_ecmwf(preproc_dims,igrib,intdistances,intindexes,intvalues)
            preproc_prtm%v10(:,:)=real(intvalues(:,:,1),kind=sreal)

         endif
!!!!!!!!!!!!!!!!!!!!
         !2m temperature
!!!!!!!!!!!!!!!!
      elseif(parameter .eq. 167) then

         ecmwf_2d%temp2(:,:)=reshape(dummyvector_value, (/ecmwf_dims%xdim,ecmwf_dims%ydim/))


         !now do the actual filling of the preprocessing grid by:
         !nearest neighbor
         if(lnn) then

            call nn_ecmwf(preproc_dims,igrib,nnindex,nnvalue)
            preproc_prtm%temp2(:,:)=real(nnvalue,kind=sreal)

            !four point average
         else

            call int_ecmwf(preproc_dims,igrib,intdistances,intindexes,intvalues)
            preproc_prtm%temp2(:,:)=real(intvalues(:,:,1),kind=sreal)

         endif
!!!!!!!!!!!!!!!!!!
         !land-sea mask 0-1
!!!!!!!!!!!!!!!!!!
      elseif(parameter .eq. 172) then

         ecmwf_2d%land_sea_mask(:,:)=reshape(dummyvector_value, (/ecmwf_dims%xdim,ecmwf_dims%ydim/))

         !now do the actual filling of the preprocessing grid by:
         !nearest neighbor
         if(lnn) then

            call nn_ecmwf(preproc_dims,igrib,nnindex,nnvalue)
            preproc_prtm%land_sea_mask(:,:)=real(nnvalue,kind=sreal)

            !four point average
         else

            call int_ecmwf(preproc_dims,igrib,intdistances,intindexes,intvalues)
            preproc_prtm%land_sea_mask(:,:)=real(intvalues(:,:,1),kind=sreal)

         endif

         !        write(*,*) ecmwf_2d%land_sea_mask
!!!!!!!!!!!!!!!!!!!!!!
         !skin temperature
!!!!!!!!!!!!!!!!!!!!!!!!
      elseif(parameter .eq. 235) then

         ecmwf_2d%skin_temp(:,:)=reshape(dummyvector_value, (/ecmwf_dims%xdim,ecmwf_dims%ydim/))


         !now do the actual filling of the preprocessing grid by:
         !nearest neighbor
         if(lnn) then

            call nn_ecmwf(preproc_dims,igrib,nnindex,nnvalue)
            preproc_prtm%skin_temp(:,:)=real(nnvalue,kind=sreal)

            !four point average
         else

            call int_ecmwf(preproc_dims,igrib,intdistances,intindexes,intvalues)
            preproc_prtm%skin_temp(:,:)=real(intvalues(:,:,1),kind=sreal)

         endif

      endif

      !release the "message" (however, igrib never changed?)
      call grib_release(igrib,status)
      !everything OK?
      if(status .ne. 0 ) then
         call grib_get_error_string(status,err_msg)
         write(*,*) 'GRIB API ERROR RELEASING GRIB_ID:',  trim(err_msg)
      endif

      !load the (next) message?
      call grib_new_from_file(gribid,igrib,iret)
      !everything OK?
      if(iret .ne. 0 ) then
         call grib_get_error_string(status,err_msg)
         write(*,*) 'GRIB API ERROR (RE)GETTING GRIB_ID:',  trim(err_msg)
      endif

      !     write(*,*) 'grib',gribid,igrib,trim(ecmwf_path)


   enddo

   !build now the pressure coordinate at cell centers from that:

   allocate(spec_hum(1:preproc_dims%kdim))
   spec_hum=0.00
   allocate(temperature(1:preproc_dims%kdim))
   temperature=0.00
   allocate(phi_lay(1:preproc_dims%kdim-1))
   phi_lay=0.00
   allocate(phi_lev(1:preproc_dims%kdim))
   phi_lev=0.00

   !write(*,*)'bvector',bvector
   !write(*,*)'convert ec lnsp',exp(preproc_prtm%lnsp(:,:))
   do jdim=preproc_dims%min_lat,preproc_dims%max_lat !1,preproc_dims%ydim
      do idim=preproc_dims%min_lon,preproc_dims%max_lon !1,preproc_dims%xdim
         !write(*,*)'convert ec lnsp',exp(preproc_prtm%lnsp(idim,jdim)),preproc_prtm%lnsp(idim,jdim)
         do ik=1,ecmwf_dims%kdim
            !equation to convert model levels to pressure levels.
            !http://www.ecmwf.int/research/ifsdocs/DYNAMICS/Chap2_Discretization4.html#961180
            preproc_prtm%pressure(idim,jdim,ik)=0.5*(avector(ik)+avector(ik+1)+&
                 & (bvector(ik)+bvector(ik+1))*exp(preproc_prtm%lnsp(idim,jdim)))



         enddo

         !write(*,*)'convert ec',preproc_prtm%pressure(idim,jdim,ik)


      enddo
   enddo

   !stop

   deallocate(spec_hum)
   deallocate(temperature)
   deallocate(phi_lev)
   deallocate(phi_lay)

   !  write(*,*) 'PRES',minval(preproc_prtm%pressure),maxval(preproc_prtm%pressure)

   deallocate(avector)
   deallocate(bvector)

   deallocate(dummyvector_value)
   deallocate(dummyvector_lat)
   deallocate(dummyvector_lon)

   if(lnn) then

      deallocate(nnoutlat)
      deallocate(nnoutlon)
      deallocate(nnvalue)
      deallocate(nndistance)
      deallocate(nnindex)

   else

      deallocate(intoutlats)
      deallocate(intoutlons)
      deallocate(intvalues)
      deallocate(intdistances)
      deallocate(intindexes)

   endif

   deallocate(shiftlon)
   deallocate(shiftlat)   

   !close the grib file
   call grib_close_file(gribid,status)
   !everything OK?
   if(status .ne. 0 ) then
      call grib_get_error_string(status,err_msg)
      write(*,*) 'GRIB API ERROR RELEASING FILE_ID:',  trim(err_msg)
   endif


end subroutine read_ecmwf_grib

