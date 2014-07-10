!-------------------------------------------------------------------------------
! Name: read_ecmwf_nc.f90
!
! Purpose:
! Reads NetCDF format ECMWF ERA interim data. It is interpolated onto the
! preprocessing grid using the EMOS package
! 
! Description and Algorithm details:
! 1) Prepare interpolation.
! 2) Open file.
! 3) Loop over variables:
!    a) Identify variable with desired data field.
!    b) Read data and interpolate.
!    c) Copy data into preprocessor structure.
! 4) Close file.
!
! Arguments:
! Name            Type In/Out/Both Description
! ------------------------------------------------------------------------------
! ecmwf_path     string  In   NetCDF ECMWF file to be opened.
! ecmwf          struct  both Structure summarising contents of ECMWF files.
! preproc_dims   struct  In   Dimensions of the preprocessing grid.
! preproc_prtm   struct  Both Pressure-level information for RTTOV.
! verbose        logic  in   T: Print min/max of each field; F: Don't.
!
! History:
! 2012/08/06,   : Initial version ecmwf code
! 2012/08/06, CP: modified to write data into preprocessing structures
! 2012/08/07, CP: added in reading of surface data pressure level data, added in
!   ozone profile and geopotential height,
! 2012/08/13, CP: totally rewrote program to cope with multiple netcdf file read
! 2012/11/13, CP: added in surface pressure and pressure
! 2012/11/29, CP: added ecmwf_2d definitions for u10 and v10
! 2013/01/29, CP: changed how geopotetntial was read out
! 2013/03/05, CP: small change to work in gfortran
! 2013/03/06, CP: tidy up and rearrange badc files
! 2013/03/07, CP: tidied up allocations and changed code to read in q and 03 form
!   a netcdf file because grib code did not work for badc style grb files also
!   added computation of geopot because was previously dome in grib read
! 2013/03/18, GT: Altered the allocation of temporary arrays to hold the various
!   ECMWF variable to avoid the compiler complaining of possible use of
!   unallocated arrays.
! 2013/03/19, GT: Fixed the reading of the gpam file (containing specific
!   humidity and O3 data). Moved the rearranging of the ECMWF arrays into the
!   same if statements as the reading commands, and changed the generation of
!   the pressure profile array so that it is created on the rearranged grid.
!   Removed quite a few debugging print statements
! 2013/03/20, GT: Fixed a bug introduced in yesterday's changes (10 m wind
!   components were not being writen to ECMWF structures)
! 2013/10/29,   : Changed array allocation of phi_lay and phi_lev
! 2014/02/10, AP: Extreme tidying. Made all allocatable arrays definite size.
!   Removed check of file dimensions. Made a,bvector global. Added nearest
!   neighbour functionality. Made geopotential calculation external. Removed
!   surfaceflag.
! 2014/05/08, AP: Complete rewrite, vastly tidying the original and updating to
!   the new ecmwf structure.
!
! $Id$
!
! Bugs:
! - you need to be careful with parameter naming as the variable names are not
!   consistent across files for example the variable name could be lnsp or LNSP
!-------------------------------------------------------------------------------

subroutine read_ecmwf_nc(ecmwf_path, ecmwf, preproc_dims, preproc_geoloc, &
     preproc_prtm, verbose)

   use netcdf
   use orac_ncdf
   use preproc_constants
   use preproc_structures
 
   implicit none

   character(len=pathlength), intent(in)    :: ecmwf_path
   type(ecmwf_s),             intent(in)    :: ecmwf
   type(preproc_dims_s),      intent(in)    :: preproc_dims
   type(preproc_geoloc_s),    intent(inout) :: preproc_geoloc
   type(preproc_prtm_s),      intent(inout) :: preproc_prtm
   logical,                   intent(in)    :: verbose

   integer(lint),     external              :: INTIN,INTOUT,INTF
   integer(lint),     parameter             :: BUFFER = 2000000

   integer(lint),     dimension(1)          :: intv,old_grib,new_grib
   real(dreal)                              :: grid(2),area(4)
   real(dreal),       dimension(BUFFER)     :: old_data,new_data
   character(len=20), dimension(1)          :: charv

   real(sreal),       pointer               :: array2d(:,:), array3d(:,:,:)
   integer(4)                               :: n,ni,nj,i,j,k,ivar
   integer(4)                               :: fid,ndim,nvar,natt
   integer(4)                               :: old_len,new_len
   character(len=20)                        :: name
   logical                                  :: three_d
   real(sreal) :: dummy2d(ecmwf%xdim,ecmwf%ydim,1,1)
   real(sreal) :: dummy3d(ecmwf%xdim,ecmwf%ydim,ecmwf%kdim,1)

   n=ecmwf%xdim*ecmwf%ydim

   ! input details of new grid (see note in read_ecmwf_grib)
   charv(1)='yes'
   grid(1)=real_fill_value
   if (INTIN('missingvalue',intv,grid,charv).ne.0) &
        STOP 'READ_ECMWF_NC: INTIN missingvalue failed.'
   charv(1)='unpacked'
   if (INTIN('form',intv,grid,charv).ne.0) &
        STOP 'READ_ECMWF_NC: INTIN form failed.'
   if (INTOUT('form',intv,grid,charv).ne.0) &
        STOP 'READ_ECMWF_NC: INTOUT form failed.'
   intv(1)=ecmwf%ydim/2
   if (INTIN('regular',intv,grid,charv).ne.0) &
        STOP 'READ_ECMWF_NC: INTIN reg failed.'
   grid(1) = 0.5 / preproc_dims%dellon
   grid(2) = 0.5 / preproc_dims%dellat
   if (INTOUT('grid',intv,grid,charv).ne.0) &
        STOP 'READ_ECMWF_NC: INTOUT grid failed.'
   area(1) = preproc_geoloc%latitude(preproc_dims%max_lat) + 0.01*grid(2)
   area(2) = preproc_geoloc%longitude(preproc_dims%min_lon) + 0.01*grid(1)
   area(3) = preproc_geoloc%latitude(preproc_dims%min_lat) + 0.01*grid(2)
   area(4) = preproc_geoloc%longitude(preproc_dims%max_lon) + 0.01*grid(1)
   if (INTOUT('area',intv,area,charv).ne.0) &
        STOP 'READ_ECMWF_NC: INTOUT area failed.'
   ni = ceiling((area(4)+180.)/grid(1)) - floor((area(2)+180.)/grid(1)) + 1
   nj = ceiling((area(1)+90.)/grid(2)) - floor((area(3)+90.)/grid(2)) + 1
   
   ! open file
   call nc_open(fid,ecmwf_path)
   if (nf90_inquire(fid,ndim,nvar,natt) .ne. 0) &
        STOP 'READ_ECMWF_NC: NF INQ failed.'

   ! loop over variables
   do ivar=1,nvar
      ! determine if field should be read
      if (nf90_inquire_variable(fid,ivar,name) .ne. 0) &
           STOP 'READ_ECMWF_NC: NF VAR INQUIRE failed.'
      select case (name)
      case('Z','z')
         three_d=.false.
         array2d => preproc_prtm%geopot
      case('Q','q')
         three_d=.true.
         array3d => preproc_prtm%spec_hum
      case('T')
         three_d=.true.
         array3d => preproc_prtm%temperature
      case('O3','o3')
         three_d=.true.
         array3d => preproc_prtm%ozone
      case('LNSP','lnsp')
         three_d=.false.
         array2d => preproc_prtm%lnsp
      case('CI','ci')
         three_d=.false.
         array2d => preproc_prtm%sea_ice_cover
      case('ASN','asn')
         three_d=.false.
         array2d => preproc_prtm%snow_albedo
      case('TCWV','tcwv')
         three_d=.false.
         array2d => preproc_prtm%totcolwv
      case('SD','sd')
         three_d=.false.
         array2d => preproc_prtm%snow_depth
      case('U10','u10')
         three_d=.false.
         array2d => preproc_prtm%u10
      case('V10','v10')
         three_d=.false.
         array2d => preproc_prtm%v10
      case('T2','t2')
         three_d=.false.
         array2d => preproc_prtm%temp2
      case('SKT','skt')
         three_d=.false.
         array2d => preproc_prtm%skin_temp
      case('SSTK','sstk')
         three_d=.false.
         array2d => preproc_prtm%sst
      case('AL','al')
         three_d=.false.
         array2d => preproc_prtm%land_sea_mask
      case default
         cycle
      end select
      if (nf90_inquire_variable(fid,ivar,name) .ne. 0) &
           STOP 'READ_ECMWF_NC: NF VAR INQUIRE failed.'

      if (three_d) then
         call nc_read_array(fid,name,dummy3d,0)
         do k=1,ecmwf%kdim
            old_len=n
            old_data(1:n)=reshape(real(dummy3d(:,:,k,1),kind=8),[n])
            
            new_len=BUFFER
            if (INTF(old_grib,old_len,old_data,new_grib,new_len,new_data).ne.0)&
                 STOP 'READ_ECMWF_NC: INTF failed.'
            if (new_len .ne. ni*nj) print*,'Interpolation grid wrong.'

            ! copy data into preprocessing grid
            do j=1,nj,2
               do i=1,ni,2
                  array3d(preproc_dims%min_lon+i/2, &
                       preproc_dims%min_lat+(nj-j)/2,k) = &
                       real(new_data(i+(j-1)*ni), kind=4)
               end do
            end do
         end do
         if (verbose) print*,trim(name),') Min: ',minval(array3d), &
              ', Max: ',maxval(array3d)
      else
         call nc_read_array(fid,name,dummy2d,0)
         old_len=n
         old_data(1:n)=reshape(real(dummy2d,kind=8),[n])

         new_len=BUFFER
         if (INTF(old_grib,old_len,old_data,new_grib,new_len,new_data) .ne. 0) &
              STOP 'READ_ECMWF_NC: INTF failed.'
         if (new_len .ne. ni*nj) print*,'Interpolation grid wrong.'
         
         ! copy data into preprocessing grid
         do j=1,nj,2
            do i=1,ni,2
               array2d(preproc_dims%min_lon+i/2, &
                    preproc_dims%min_lat+(nj-j)/2) = &
                    real(new_data(i+(j-1)*ni), kind=4)
            end do
         end do
         if (verbose) print*,trim(name),') Min: ',minval(array2d), &
              ', Max: ',maxval(array2d)
      end if
   end do

   if (nf90_close(fid) .ne. 0) STOP 'READ_ECMWF_NC: Failure to close file.'
   
end subroutine read_ecmwf_nc
