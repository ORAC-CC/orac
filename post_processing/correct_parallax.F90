!-------------------------------------------------------------------------------
! Name:
!    correct_parallax.F90
!
! Purpose:
!    For high satellite zenith angles the apparent location of a cloud will be
!    offset from its actual position by a significant margin that increases with
!    cloud top height: This is known as the parallax effect
!
!    This function will compute the lat/lon offset due to the parallax effect
!    and apply a correction to the input data field to correct for parallax.
!    Some pixels will not contain any data after correction, these will be set
!    to the fill value, no cosmetic filling will be done.
!
!    NOTE: Only the following variables are affected, others remain uncorrected.
!          Where a variable is given, uncertainty is also affected:
!
!    - cth, cer, cot, cwp, cldmask, cldtype
!
! Description:
!
! Algorithm:
!    On a per-pixel basis:
!       1) Extract cth, lat, lon and satellite geometry
!       2) Compute the offset due to parallax
!       3) Shift all pixels to compensate for this effect
!
! History:
! 2018/06/08, SP: Original version
!
! Bugs:
!    Satellite height is assumed based upon the instrument. Ideally it should be
!    read from metadata.
!    Hardcoded to view number 1. Other views ignored.
!-------------------------------------------------------------------------------

module correct_parallax_m

   use postproc_constants_m
   implicit none

contains

! Compute the satellite altitude above the surface based on the platform id
subroutine get_attr(global_atts, sath, eqrrad, polrad, sobs_lat, sobs_lon)

   use global_attributes_m
   type(global_attributes_t), intent(in)  :: global_atts
   real,                      intent(out) :: sath
   real,                      intent(out) :: eqrrad
   real,                      intent(out) :: polrad
   real,                      intent(out) :: sobs_lat
   real,                      intent(out) :: sobs_lon

   character(len=attribute_length)      :: platform
   character(len=attribute_length_long) :: tmpstr
   integer                              :: i, pos, lenner

   platform = global_atts%platform

   ! In the absence of anything else to do, set everything to fill
   sath     = sreal_fill_value
   eqrrad   = sreal_fill_value
   polrad   = sreal_fill_value
   sobs_lat = sreal_fill_value
   sobs_lon = sreal_fill_value

   ! But geostationary sats are a bit higher
   if (platform(1:3) == 'MSG' .or. platform(1:4) == 'GOES' .or. &
       platform(1:8) == 'Himawari') then
      tmpstr = global_atts%Satpos_Metadata
      do i = 1, 5
         lenner = len_trim(tmpstr)
         pos    = index(tmpstr,",")
         if (pos .eq. 0) pos = lenner
         if (i .eq. 1) read(tmpstr(1:pos-1), '(f10.7)')sobs_lat
         if (i .eq. 2) read(tmpstr(1:pos-1), '(f10.7)')sobs_lon
         if (i .eq. 3) read(tmpstr(1:pos-1), '(f10.0)')sath
         if (i .eq. 4) read(tmpstr(1:pos-1), '(f10.0)')eqrrad
         if (i .eq. 5) read(tmpstr(1:pos-1), '(f10.0)')polrad
         tmpstr=tmpstr(pos+1:lenner)
      end do
   end if

   sath   = sath / 1000.
   eqrrad = eqrrad / 1000.
   polrad = polrad / 1000.

   return

end subroutine get_attr


real(kind=sreal) function get_ave_real(inarr)

   real(kind=sreal), intent(in) :: inarr(:,:) ! (3,3)
   real(kind=sreal)             :: outval
   integer(kind=sint)           :: counter, i, j

   counter = 0
   outval  = 0

   do i = 1, 3
      do j = 1, 3
         if (inarr(i,j) .gt. 0) then
            counter = counter + 1
            outval  = outval + inarr(i,j)
         end if
      end do
   end do

   if (counter .gt. 2) then
      get_ave_real = outval / counter
   else
      get_ave_real = sreal_fill_value
   end if

end function get_ave_real


subroutine correct_parallax(primary, indexing, global_atts, verbose)

   use postproc_constants_m
   use global_attributes_m
   use orac_input_m
   use omp_lib

   implicit none

   type(input_data_primary_t), intent(inout) :: primary
   type(input_indices_t),      intent(inout) :: indexing
   type(global_attributes_t),  intent(in)    :: global_atts
   logical,                    intent(in)    :: verbose

   integer                         :: i, j, ni, nj
   real(kind=sreal),   allocatable :: dist(:,:)
   real(kind=sreal),   allocatable :: orig_cth(:,:)
   integer(kind=sint), allocatable :: outx(:,:), outy(:,:)


   real(kind=sreal),   allocatable :: ncth(:,:), ncthu(:,:)
   real(kind=sreal),   allocatable :: ncer(:,:), nceru(:,:)
   real(kind=sreal),   allocatable :: ncot(:,:), ncotu(:,:)
   real(kind=sreal),   allocatable :: nctt(:,:), ncttu(:,:)
   real(kind=sreal),   allocatable :: nctp(:,:), nctpu(:,:)
   real(kind=sreal),   allocatable :: ncost(:,:)
   integer(kind=byte), allocatable :: ncldmask(:,:), ncldtype(:,:), nphase(:,:)

   real(kind=sreal)                :: sat_h, eqr_rad, pol_rad, sat_lat, sat_lon
   real(kind=sreal)                :: obs_lat, obs_lat1, obs_lon
   real(kind=sreal)                :: sat_lat_m, sat_x, sat_y, sat_z
   real(kind=sreal)                :: r, omod_x, omod_y, omod_z, ob_mod
   real(kind=sreal)                :: mod_x, mod_y, mod_z, ax_mod
   real(kind=sreal)                :: cosbeta, aa, bb, cc, x
   real(kind=sreal)                :: axis_x, axis_y, axis_z, bpos(2)
   real(kind=sreal)                :: new_lat, new_lon, rad_rat
   integer(kind=sint)              :: x0, x1, y0, y1

   integer(kind=sint)             :: pixdelt = 12

   ! Figure out the satellite altitude *above the surface*
   call get_attr(global_atts, sat_h, eqr_rad, pol_rad, sat_lat, sat_lon)
   rad_rat = (eqr_rad / pol_rad) * (eqr_rad / pol_rad)

   if (sat_h .le. 0) then
      if (verbose) write(*,*) "Parallax correction is unavailable. Skipping."
      return
   end if

   if (verbose) write(*,*) "Parallax correction underway"

   ! Allocate variables for computing parallax
   allocate(orig_cth(indexing%X0:indexing%X1,indexing%Y0:indexing%Y1))
   allocate(outx(indexing%X0:indexing%X1,indexing%Y0:indexing%Y1))
   allocate(outy(indexing%X0:indexing%X1,indexing%Y0:indexing%Y1))

   outx(:,:) = sint_fill_value
   outy(:,:) = sint_fill_value

   orig_cth  = primary%cth

   ! Allocate temporary output variables
   allocate(ncth(indexing%X0:indexing%X1,indexing%Y0:indexing%Y1))
   allocate(ncthu(indexing%X0:indexing%X1,indexing%Y0:indexing%Y1))
   allocate(ncer(indexing%X0:indexing%X1,indexing%Y0:indexing%Y1))
   allocate(nceru(indexing%X0:indexing%X1,indexing%Y0:indexing%Y1))
   allocate(ncot(indexing%X0:indexing%X1,indexing%Y0:indexing%Y1))
   allocate(ncotu(indexing%X0:indexing%X1,indexing%Y0:indexing%Y1))
   allocate(nctt(indexing%X0:indexing%X1,indexing%Y0:indexing%Y1))
   allocate(ncttu(indexing%X0:indexing%X1,indexing%Y0:indexing%Y1))
   allocate(nctp(indexing%X0:indexing%X1,indexing%Y0:indexing%Y1))
   allocate(nctpu(indexing%X0:indexing%X1,indexing%Y0:indexing%Y1))
   allocate(ncost(indexing%X0:indexing%X1,indexing%Y0:indexing%Y1))
   allocate(nphase(indexing%X0:indexing%X1,indexing%Y0:indexing%Y1))
   allocate(ncldmask(indexing%X0:indexing%X1,indexing%Y0:indexing%Y1))
   allocate(ncldtype(indexing%X0:indexing%X1,indexing%Y0:indexing%Y1))

   ncth(:,:)     = sreal_fill_value
   ncthu(:,:)    = sreal_fill_value
   ncer(:,:)     = sreal_fill_value
   nceru(:,:)    = sreal_fill_value
   ncot(:,:)     = sreal_fill_value
   ncotu(:,:)    = sreal_fill_value
   nctt(:,:)     = sreal_fill_value
   ncttu(:,:)    = sreal_fill_value
   nctp(:,:)     = sreal_fill_value
   nctpu(:,:)    = sreal_fill_value
   ncost(:,:)    = sreal_fill_value
   nphase(:,:)   = byte_fill_value
   ncldmask(:,:) = byte_fill_value
   ncldtype(:,:) = byte_fill_value

   sat_lat_m = atan(tan(sat_lat) / rad_rat)
   sat_x     = sat_h * cos(sat_lat_m) * cos(sat_lon)
   sat_y     = sat_h * cos(sat_lat_m) * sin(sat_lon)
   sat_z     = sat_h * sin(sat_lat_m)

   !$OMP PARALLEL &
   !$OMP PRIVATE(i) &
   !$OMP PRIVATE(j) &
   !$OMP PRIVATE(obs_lat,obs_lon) &
   !$OMP PRIVATE(omod_x,omod_y,omod_z,ob_mod) &
   !$OMP PRIVATE(mod_x,mod_y,mod_z,ax_mod) &
   !$OMP PRIVATE(cosbeta,aa,bb,cc,x) &
   !$OMP PRIVATE(axis_x,axis_y,axis_z) &
   !$OMP PRIVATE(x0,x1,y0,y1) &
   !$OMP PRIVATE(new_lat, new_lon, dist, bpos)
   !$OMP DO SCHEDULE(GUIDED)

   do j = indexing%Y0, indexing%Y1
      do i = indexing%X0, indexing%X1

         if (primary%cth(i,j) .lt. 0.001) cycle
         if (primary%lat(i,j) .lt. -100.) cycle
         if (primary%lon(i,j) .lt. -200.) cycle

         obs_lat  = primary%lat(i,j)*d2r
         obs_lon  = primary%lon(i,j)*d2r

         obs_lat1 = atan(tan(obs_lat) / rad_rat)

         r        = sqrt(eqr_rad*eqr_rad*cos(obs_lat1)*cos(obs_lat1) + pol_rad*pol_rad*sin(obs_lat1)*sin(obs_lat1))

         omod_x   = r * cos(obs_lat1) * cos(obs_lon)
         omod_y   = r * cos(obs_lat1) * sin(obs_lon)
         omod_z   = r * sin(obs_lat1)

         ob_mod   = sqrt(omod_x * omod_x+omod_y * omod_y+omod_z * omod_z)

         mod_x    = sat_x - omod_x
         mod_y    = sat_y - omod_y
         mod_z    = sat_z - omod_z

         ax_mod   = sqrt(mod_x*mod_x + mod_y*mod_y + mod_z*mod_z)

         cosbeta  = -(omod_x*mod_x + omod_y*mod_y + omod_z*mod_z)/(ob_mod*ax_mod)
         aa       = 1.
         bb       = -2*ob_mod * cosbeta
         cc       = ob_mod*ob_mod - (ob_mod+primary%cth(i,j))*(ob_mod+primary%cth(i,j))
         x        = (-bb+sqrt(bb*bb-4*aa*cc))/(2*aa)

         axis_x   = omod_x + (x/ax_mod)*mod_x
         axis_y   = omod_y + (x/ax_mod)*mod_y
         axis_z   = omod_z + (x/ax_mod)*mod_z

         new_lat  = atan(axis_z/sqrt((axis_x*axis_x) + (axis_y*axis_y)))
         new_lat  = atan(tan(new_lat)*rad_rat)
         new_lat  = new_lat * (1./d2r)
         new_lon  = atan2(axis_y,axis_x)*(1./d2r)

         if (new_lat .lt. -90. .or. new_lat .gt. 90.) new_lat = 1e7
         if (new_lon .lt. -180. .or. new_lon .gt. 180.) new_lon = 1e7

         x0 = i-pixdelt
         if (x0 .lt. indexing%X0) x0 = indexing%X0
         x1 = i+pixdelt
         if (x1 .ge. indexing%X1) x1 = indexing%X1
         y0 = j-pixdelt
         if (y0 .lt. indexing%Y0) y0 = indexing%Y0
         y1 = j+pixdelt
         if (y1 .ge. indexing%Y1) y1 = indexing%Y1

         allocate(dist(x0:x1,y0:y1))
         dist(:,:) = 1e7

         dist(:,:) = (new_lat - primary%lat(x0:x1,y0:y1))*(new_lat - primary%lat(x0:x1,y0:y1))
         dist(:,:) = dist(:,:) + (new_lon - primary%lon(x0:x1,y0:y1))*(new_lon - primary%lon(x0:x1,y0:y1))

         bpos = minloc(dist)
         if (sqrt(minval(dist)) .gt. 0.02) then
            outx(i,j) = sint_fill_value
            outy(i,j) = sint_fill_value
         else
            outx(i,j) = bpos(1) + x0
            outy(i,j) = bpos(2) + y0
         end if

         deallocate(dist)
      end do
   end do
   !$OMP END DO
   !$OMP END PARALLEL

   !$OMP PARALLEL &
   !$OMP PRIVATE(i,j) &
   !$OMP PRIVATE(ni,nj)
   !$OMP DO SCHEDULE(GUIDED)
   do j = indexing%Y0, indexing%Y1
      do i = indexing%X0, indexing%X1
         ni = outx(i,j)
         nj = outy(i,j)
         if (ni .lt. 1 .or. nj .lt. 1) cycle
         if (ni .gt. indexing%X1 .or. nj .gt. indexing%Y1) cycle
         if (ncth(ni,nj) .gt. primary%cth(i,j)) cycle
         if (primary%cth(i,j) .lt. 0.001) cycle

         ncth(ni,nj)     = primary%cth(i,j)
         ncthu(ni,nj)    = primary%cth_uncertainty(i,j)
         ncer(ni,nj)     = primary%cer(i,j)
         nceru(ni,nj)    = primary%cer_uncertainty(i,j)
         ncot(ni,nj)     = primary%cot(i,j)
         ncotu(ni,nj)    = primary%cot_uncertainty(i,j)
         nctt(ni,nj)     = primary%ctt(i,j)
         ncttu(ni,nj)    = primary%ctt_uncertainty(i,j)
         nctp(ni,nj)     = primary%ctp(i,j)
         nctpu(ni,nj)    = primary%ctp_uncertainty(i,j)
         ncost(ni,nj)    = primary%costjm(i,j)
         nphase(ni,nj)   = primary%phase(i,j)
         ncldmask(ni,nj) = primary%cldmask(i,j,1)
         ncldtype(ni,nj) = primary%cldtype(i,j,1)
      end do
   end do
   !$OMP END DO
   !$OMP END PARALLEL

   primary%cth             = ncth
   primary%cth_uncertainty = ncthu

   primary%cer             = ncer
   primary%cer_uncertainty = nceru

   primary%cot             = ncot
   primary%cot_uncertainty = ncotu

   primary%ctt             = nctt
   primary%ctt_uncertainty = ncttu

   primary%ctp             = nctp
   primary%ctp_uncertainty = nctpu

   primary%costjm          = ncost
   primary%phase           = nphase
   primary%cldmask(:,:,1)  = ncldmask
   primary%cldtype(:,:,1)  = ncldtype

   !$OMP PARALLEL &
   !$OMP PRIVATE(i,j)
   !$OMP DO SCHEDULE(GUIDED)
   do j = indexing%Y0+1, indexing%Y1-1
      do i = indexing%X0+1, indexing%X1-1
         if (ncth(i,j) .lt. 0 .and. orig_cth(i,j) .gt. 0) then
            primary%cth(i,j)             = get_ave_real(ncth(i-1:i+1,j-1:j+1))
            primary%cth_uncertainty(i,j) = get_ave_real(ncthu(i-1:i+1,j-1:j+1))
            primary%cer(i,j)             = get_ave_real(ncer(i-1:i+1,j-1:j+1))
            primary%cer_uncertainty(i,j) = get_ave_real(nceru(i-1:i+1,j-1:j+1))
            primary%cot(i,j)             = get_ave_real(ncot(i-1:i+1,j-1:j+1))
            primary%cot_uncertainty(i,j) = get_ave_real(ncotu(i-1:i+1,j-1:j+1))
            primary%ctt(i,j)             = get_ave_real(nctt(i-1:i+1,j-1:j+1))
            primary%ctt_uncertainty(i,j) = get_ave_real(ncttu(i-1:i+1,j-1:j+1))
            primary%ctp(i,j)             = get_ave_real(nctp(i-1:i+1,j-1:j+1))
            primary%ctp_uncertainty(i,j) = get_ave_real(nctpu(i-1:i+1,j-1:j+1))
         end if
      end do
   end do
   !$OMP END DO
   !$OMP END PARALLEL

   ! Deallocate parallax variables
   deallocate(outx)
   deallocate(outy)

   ! Deallocate temporary output variables
   deallocate(ncth)
   deallocate(ncthu)
   deallocate(ncer)
   deallocate(nceru)
   deallocate(ncot)
   deallocate(ncotu)
   deallocate(nctt)
   deallocate(ncttu)
   deallocate(nctp)
   deallocate(nctpu)
   deallocate(ncost)
   deallocate(nphase)
   deallocate(ncldmask)
   deallocate(ncldtype)

   if (verbose) write(*,*) "Parallax correction complete"

end subroutine correct_parallax

end module correct_parallax_m
