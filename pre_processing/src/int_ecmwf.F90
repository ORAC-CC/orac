! Name: int_ecmwf.f90
!
!
! Purpose:
! Average/interpolate to preprocessing grid from the four neighboring ECMWF
! grid points.
! 
! Description and Algorithm details:
!
!
! Arguments:
! Name Type In/Out/Both Description
! ------------------------------------------------------------------------------
!
!
! History:
! 2012/02/24: MJ produces initial code version.
! 2013/05/23: MJ implements untested OMP paralleization, not yet thread save,
!                therefore tuned-off
! 2014/02/10: AP variable renaming
!
! $Id$
!
! Bugs: 
! - Is very slow. Cloud compute some sums etc. outside and store them.
!

subroutine int_ecmwf(preproc_dims,igrib,intdistances,intindexes,intvalues)

   use preproc_constants

   use preproc_structures

   use grib_api

   implicit none

   integer(kind=lint) :: igrib

   real(kind=dreal) :: dummysum1,dummysum2,normdist(1:4)

   integer(kind=lint) :: idim,jdim,ik

   type(preproc_dims_s) :: preproc_dims

   real(kind=dreal), dimension(preproc_dims%xdim,preproc_dims%ydim,1:4) :: intvalues,intdistances

   integer(kind=kindOfInt), dimension(preproc_dims%xdim,preproc_dims%ydim,1:4) :: intindexes

   ! integer :: nthreads,nompthreads
   ! integer :: OMP_GET_max_THREADS

   !how many threads are available?
!!$  nompthreads=OMP_get_max_threads()
!!$  if( nompthreads .ge. 4)  nompthreads=4
!!$  nompthreads=max(1,nompthreads)
!!$  call OMP_set_num_threads(nompthreads)
   !write(*,*) 'Loops running on: ', nompthreads, 'threads'

   !OLS
   !  do idim=1,preproc_dims%xdim
   !     do jdim=1,preproc_dims%ydim

   !OMP PARALLEL DO PRIVATE(idim,jdim,dummysum1,dummysum2,normdist) &
   !OMP SCHEDULE(DYNAMIC,50)
   do jdim=preproc_dims%min_lat,preproc_dims%max_lat        
      do idim=preproc_dims%min_lon,preproc_dims%max_lon


         !get the values of the four ecmwf pixels which are closest to the preprocessing pixel
         call grib_get_element(igrib,"values",intindexes(idim,jdim,1:4),intvalues(idim,jdim,1:4))

         dummysum1=0.00
         !sum the distances to those four neighboring points
         dummysum1=sum(intdistances(idim,jdim,1:4)) !SAVE

         !normalize the distances with the sum
         normdist(1:4)=intdistances(idim,jdim,1:4)/dummysum1 !SAVE

         !sum the values of the neighboring points and weigh them with their 1-distances
         !=>closer points get higher weight
         dummysum2=0.00
         do ik=1,4
            dummysum2=dummysum2+(1.0-normdist(ik))*intvalues(idim,jdim,ik) !SAVE "1-" part
         enddo

         !"abuse" first entry of third dimension of intvalues array to hold the result.
         !Divide my 3.0 to apply final normalization
         intvalues(idim,jdim,1)=dummysum2/3.0 !SAVE "/3.0" in weights already!
         !write(*,*) idim,jdim, intindexes(idim,jdim,1:4),intvalues(idim,jdim,1)
         !pause

      enddo
   enddo
   !OMP END PARALLEL DO

end subroutine int_ecmwf
