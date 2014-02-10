! Name: define_preprop_grid.F90
!
!
! Purpose:
! Define the preprocessing grid in terms of known dimensions.
! 
! Description and Algorithm details:
! 1) If option 1, determine dellat/lon from the ECMWF lat/lon grid.
! 2) If option 2, do nothing.
! 3) If option 3, determine horizontal grid size from dellat/lon.
!
! Arguments:
! Name         Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! grid_flag    string In  1 - ECMWF grid; 2 - MODIS L3 grid; 3 - own definition
! ecmwf_2d     struct In  Structure containing 2-D ECMWF fields.
! ecmwf_dims   struct In  Structure summarising dimensions of ECMWF files.
! preproc_dims struct Out Structure summarising dimensions of preprocessing.
! verbose      logic  In  in F: minimise information printed to screen; T: don't
!
! History:
! 2012/01/19: MJ produces initial code version.
! 2012/05/02: GT implicit none statement moved to correct location
! 2012/05/02: GT implicit none statement moved to correct location
! 2012/10/25: CP added extra level so surface information can b stored in the
!                same profile
! 2013/10/23: AP Tidying. Removed ecmwf_prtm argument.
!
! $Id$
!
! Bugs:
! none known
! 

subroutine define_preprop_grid(grid_flag,ecmwf_2d,ecmwf_dims, &
     preproc_dims,verbose)

   use preproc_constants
   use imager_structures
   use ecmwf_structures
   use preproc_structures

   implicit none

   type(preproc_dims_s) :: preproc_dims

   type(ecmwf_2d_s)     :: ecmwf_2d
   type(ecmwf_dims_s)   :: ecmwf_dims

   integer(kind=sint)   :: grid_flag !1:ecmwf grid, 2:L3 grid, 3: own definition
   logical              :: verbose

   !ecmwf grid but define everything at cell centres
   if(grid_flag .eq. 1) then

      preproc_dims%dellon=1./abs(ecmwf_2d%longitude(2,1)-ecmwf_2d%longitude(1,1))
      preproc_dims%dellat=1./abs(ecmwf_2d%latitude(1,1)-ecmwf_2d%latitude(1,2))

      preproc_dims%xdim=ecmwf_dims%xdim
      preproc_dims%ydim=ecmwf_dims%ydim ! CP removed -1

      !Preprocessing has same vertical structure as ecmwf grid
      preproc_dims%kdim=ecmwf_dims%kdim+1

      !modis L3 grid
   elseif(grid_flag .eq. 2) then
      print*,'DEFINE_PREPROP_GRID: !!!Option 2 does not currently function!!!'

      !user defined definition
   elseif(grid_flag .eq. 3) then

      preproc_dims%xdim=nint(2.*preproc_dims%lon_offset* &
           preproc_dims%dellon,kind=lint)
      preproc_dims%ydim=nint(2.*preproc_dims%lat_offset* &
           preproc_dims%dellat,kind=lint)     

      !Preprocessing has same vertical structure as ecmwf grid. added an extra 
      !level so surface information can be stored in the same profile
      preproc_dims%kdim=ecmwf_dims%kdim+1

   endif

   if (verbose) then
      write(*,*) 'preproc_dims: ',preproc_dims%xdim, &
           preproc_dims%ydim,preproc_dims%kdim
      write(*,*) 'dellon, dellat: ',preproc_dims%dellon,preproc_dims%dellat
   end if


end subroutine define_preprop_grid
