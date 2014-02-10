! Name: nn_ecmwf.F90
!
!
! Purpose:
! Pick the nearest neighbor and asign it.
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
! none known
!

subroutine nn_ecmwf(preproc_dims,igrib,nnindex,nnvalue)

   use preproc_constants

   use preproc_structures

   use grib_api

   implicit none

   integer(kind=lint) :: igrib

   integer(kind=lint) :: idim,jdim

   type(preproc_dims_s) :: preproc_dims

   integer(kind=kindOfInt) :: nnindex(preproc_dims%xdim,preproc_dims%ydim)

   real(kind=dreal) :: nnvalue(preproc_dims%xdim,preproc_dims%ydim)

   ! integer :: nthreads,nompthreads,OMP_GET_max_THREADS,OMP_GET_NUM_THREADS

   !how many threads are available?
!!$  nompthreads=OMP_get_max_threads()
!!$  if( nompthreads .ge. 4)  nompthreads=4
!!$  nompthreads=max(1,nompthreads)
!!$  call OMP_set_num_threads(nompthreads)
   !  nompthreads = OMP_GET_NUM_THREADS()
   !  write(*,*) 'Loops running on: ', nompthreads, 'threads'

   !OLS
   !  do idim=1,preproc_dims%xdim
   !     do jdim=1,preproc_dims%ydim

   !OMP PARALLEL DO PRIVATE(idim,jdim) SCHEDULE(DYNAMIC,50)
   do jdim=preproc_dims%min_lat,preproc_dims%max_lat        
      do idim=preproc_dims%min_lon,preproc_dims%max_lon

         !get the value of the ecmwf pixel which is closest to the preprocessing pixel
         call grib_get_element(igrib,"values",nnindex(idim,jdim),nnvalue(idim,jdim))

         !write(*,*) 'idim,jdim',idim,jdim,nnindex(idim,jdim),nnvalue(idim,jdim)


      enddo
   enddo
   !OMP END PARALLEL DO

   !  write(*,*) 'in nn'
   !pause

end subroutine nn_ecmwf
