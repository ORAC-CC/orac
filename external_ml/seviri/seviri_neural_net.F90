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
! 2020/09/30, DP: Moved assignment of NN results to Fortran arrays to C.
!                 Fixed memory leak. Major Simplifications + improvements 
!                 + cleanup. Revised subroutine header.
!
! Bugs:
! None known.
!------------------------------------------------------------------------------


module seviri_neural_net_m
    use iso_c_binding
     
    implicit none
     
    ! interface to C function py_neural_net
    interface
        subroutine py_neural_net(vis006, vis008, nir016, ir039, & 
                                 ir062, ir073, ir087, ir108, ir120, &
                                 ir134, lsm, skt, nx, ny, reg_cot, &
                                 bin_cot, unc_cot, reg_cph, bin_cph, &
                                 unc_cph) bind(C, name="py_neural_net")
            import :: c_ptr
            import :: c_int
            import :: c_float
            import :: c_char
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
            real(c_float), dimension(*), intent(out) :: reg_cot, unc_cot, &
                                                        & reg_cph, unc_cph
            integer(c_char), dimension(*), intent(out) :: bin_cot, bin_cph
            integer(c_int) :: nx, ny
        end subroutine py_neural_net
    end interface
contains


!------------------------------------------------------------------------------
! Name: seviri_ann_cph_cot
!
! Purpose:
! Subroutine accepting neural network input data from ORAC which are 
! passed to C and the Python neural network subsequently. Calls the C 
! interface function
!
! Arguments:
! Name                 Type  I/O Description
!------------------------------------------------------------------------------
! nx                   int   In  Dimension in x direction
! ny                   int   In  Dimension in y direction
! vis006               2darr In  SEVIRI VIS006 measurements
! vis008               2darr In  SEVIRI VIS008 measurements
! nir016               2darr In  SEVIRI NIR016 measurements
! ir039                2darr In  SEVIRI IR039 measurements
! ir062                2darr In  SEVIRI IR062 measurements
! ir073                2darr In  SEVIRI IR073 measurements
! ir087                2darr In  SEVIRI IR087 measurements
! ir108                2darr In  SEVIRI IR108 measurements
! ir120                2darr In  SEVIRI IR120 measurements
! ir134                2darr In  SEVIRI IR134 measurements
! lsm                  2darr In  Land-sea mask
! skt                  2darr In  Skin temperature
! regression_cot       2darr Out COT NN regression value
! binary_cot           2darr Out COT binary value after thresholding (CMA)
! uncertainty_cot      2darr Out COT uncertainty of CMA
! regression_cph       2darr Out CPH NN regression value
! binary_cph           2darr Out CPH binary value after thresholding
! uncertainty_cph      2darr Out CPH uncertainty of CPH
!------------------------------------------------------------------------------

subroutine seviri_ann_cph_cot(nx, ny, vis006, vis008, nir016, ir039, ir062, ir073, &
                        ir087, ir108, ir120, ir134, lsm, skt, regression_cot, &
                        binary_cot, uncertainty_cot, regression_cph, &
                        binary_cph, uncertainty_cph)
    use iso_c_binding
    
    ! output arrays 
    real(c_float), intent(out) :: regression_cot(:,:), uncertainty_cot(:,:) 
    real(c_float), intent(out) :: regression_cph(:,:), uncertainty_cph(:,:)
    integer(c_char), intent(out) :: binary_cot(:,:), binary_cph(:,:)

    ! C-types
    integer(c_int) :: nx ,ny
    real(c_float), dimension(nx,ny), target :: vis006, vis008, nir016, ir039, &
                                               & ir062, ir073, ir087, ir108, &
                                               & ir120, ir134, skt
    integer(c_char), dimension(nx,ny), target :: lsm
  
    ! Call Python neural network via Python C-API
    call py_neural_net(c_loc(vis006(1,1)), c_loc(vis008(1,1)), c_loc(nir016(1,1)), &
                       c_loc(ir039(1,1)), c_loc(ir062(1,1)), c_loc(ir073(1,1)), &
                       c_loc(ir087(1,1)), c_loc(ir108(1,1)), c_loc(ir120(1,1)), &
                       c_loc(ir134(1,1)), c_loc(lsm(1,1)), c_loc(skt(1,1)), &
                       nx, ny, regression_cot, binary_cot, uncertainty_cot, &
                       regression_cph, binary_cph, uncertainty_cph)
     
end subroutine seviri_ann_cph_cot

end module seviri_neural_net_m
