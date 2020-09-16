!------------------------------------------------------------------------------
! Name: seviri_neural_net.F90
!
! Purpose:
! Module for SEVIRI neural network cloud detection and cloud phase 
! determination. Neural network prediction is done in Python using 
! the Keras library with Tensorflow or Theano backends. This module
! establishes an interface to a C layer from which the Python neural 
! network module is called. Neural net input (radiances and auxuiliary 
! data) are passed to the Python module through the C interface. The 
! predicted field is then passed back to this module.
!
! History:
! 2020/07/20, DP: Initial version
! 
! Bugs:
! None known.
!------------------------------------------------------------------------------


module seviri_neural_net_m
     use iso_c_binding
     
     implicit none
     
     ! interface to C
     interface
          type(c_ptr) function py_neural_net(vis006, vis008, nir016, ir039, & 
                                             ir062, ir073, ir087, ir108, &
                                             ir120, ir134, lsm, skt, &
                                             nx, ny) bind(C, name="py_neural_net")
               import :: c_ptr
               import :: c_int
               type(c_ptr), value :: vis006
               type(c_ptr), value :: vis008
               type(c_ptr), value :: nir016
               type(c_ptr), value :: ir039
               type(c_ptr), value :: ir062
               type(c_ptr), value :: ir073
               type(c_ptr), value :: ir087
               type(c_ptr), value :: ir108
               type(c_ptr), value :: ir120
               type(c_ptr), value :: ir134
               type(c_ptr), value :: lsm
               type(c_ptr), value :: skt
               integer(c_int) :: nx, ny
          end function
     end interface
contains


!------------------------------------------------------------------------------
! Name: seviri_ann
!
! Prupose:
! Subroutine accepting neural network input data which are passed to C
! and the Python neural network subsequently.
!------------------------------------------------------------------------------
subroutine seviri_ann_cph_cot(nx, ny, vis006, vis008, nir016, ir039, ir062, ir073, &
                      ir087, ir108, ir120, ir134, lsm, skt, regression_out_cot, &
                      binary_out_cot, uncertainty_out_cot, regression_out_cph, &
                      binary_out_cph, uncertainty_out_cph)
     use iso_c_binding
     
     ! data types and fill values
     integer, parameter :: SREAL = 4
     integer, parameter :: BYTE = 1
     integer, parameter :: SREAL_FILL_VALUE = -999.0
     integer, parameter :: BYTE_FILL_VALUE = -127

     ! cloud flags
     integer, parameter :: IS_CLOUD = 1
     integer, parameter :: IS_CLEAR = 0
     
     ! number of variables
     integer, parameter :: nvars = 6

     ! loop indices
     integer :: i, j, k
     ! indexing integer
     integer ::  cidx
     
     ! internal 1d pointer containing neural network output
	real(kind=SREAL), pointer :: output1d(:)
     ! output arrays 
     real(kind=SREAL), intent(out) :: regression_out_cot(:,:), uncertainty_out_cot(:,:)
     real(kind=SREAL), intent(out) :: regression_out_cph(:,:), uncertainty_out_cph(:,:)
     integer(kind=BYTE), intent(out) :: binary_out_cot(:,:), binary_out_cph(:,:)

     ! C-types
     integer(c_int) :: nx ,ny
     integer(c_int) :: nv = nvars
	real(c_float), dimension(nx,ny), target :: vis006, vis008, nir016, ir039, &
                                                ir062, ir073, ir087, ir108, &
                                                ir120, ir134, skt
     integer(c_char), dimension(nx,ny), target :: lsm

     type(c_ptr) :: cptr_vis006, cptr_vis008, cptr_nir016, cptr_ir039, & 
                    cptr_ir062, cptr_ir073, cptr_ir087, cptr_ir108, &
                    cptr_ir120, cptr_ir134, cptr_lsm, cptr_skt, cptr_out
     
     allocate(output1d(nvars*nx*ny))

     cptr_vis006 = c_loc(vis006(1,1))
     cptr_vis008 = c_loc(vis008(1,1))
     cptr_nir016 = c_loc(nir016(1,1))
     cptr_ir039 = c_loc(ir039(1,1))
     cptr_ir062 = c_loc(ir062(1,1))
     cptr_ir073 = c_loc(ir073(1,1))
     cptr_ir087 = c_loc(ir087(1,1))
     cptr_ir108 = c_loc(ir108(1,1))
     cptr_ir120 = c_loc(ir120(1,1))
     cptr_ir134 = c_loc(ir134(1,1))
     cptr_lsm = c_loc(lsm(1,1))
     cptr_skt = c_loc(skt(1,1))
     
     ! Call Python neural network via Python C-API
     cptr_out = py_neural_net(cptr_vis006, cptr_vis008, cptr_nir016, &
                              cptr_ir039, cptr_ir062, cptr_ir073, &
                              cptr_ir087, cptr_ir108, cptr_ir120,&
                              cptr_ir134, cptr_lsm, cptr_skt, &
                              nx, ny)
     
     ! convert C to Fortran pointer
     call c_f_pointer(cptr_out, output1d, shape=[nvars*nx*ny])

     ! assign results to the correct output arrays:
     ! k==1 -> COT regression
     ! k==2 -> COT binary
     ! k==3 -> COT uncertainty
     ! k==4 -> CPH regression
     ! k==5 -> CPH binary
     ! k==6 -> CPH uncertainty
     ! 
     ! the C function returns a 1d array which is a flattened 3d array:
  
     do k=1, nv            
         do j=1,ny         
             do i=1,nx     

                 !3d index in linear representation for column-major 
                 !ordering starting at index 1
                 cidx = i + (j-1)*nx + (k-1)*nx*ny

                 if (k == 1) then 
                     regression_out_cot(i,j) = output1d(cidx)
                 else if (k==2) then
                     binary_out_cot(i,j) = int(output1d(cidx), BYTE)
                 else if (k==3)  then
                     uncertainty_out_cot(i,j) = output1d(cidx)
                 else if (k==4) then
                     regression_out_cph(i,j) = output1d(cidx)
                 else if (k==5) then
                     binary_out_cph(i,j) = int(output1d(cidx), BYTE)
                 else if (k==6) then
                     uncertainty_out_cph(i,j) = output1d(cidx)
                 end if
                  
              end do
          end do
     end do

     deallocate(output1d)

end subroutine seviri_ann_cph_cot

end module seviri_neural_net_m
