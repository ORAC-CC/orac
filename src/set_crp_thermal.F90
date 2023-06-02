!-------------------------------------------------------------------------------
! Name: set_crp_thermal.F90
!
! Purpose:
! Interpolates Cloud Radiative Properties for the ORAC thermal channels (and
! the channels with thermal plus solar contributions).
!
! Description and Algorithm details:
! Takes the SAD LUT array of look up table values and interpolates the arrays
! of Rbd, Tb etc from the LUT grid to the current point in the multi-spectral
! image data.
!
! For each LUT array in SAD_LUT (i.e. Tbd etc)
!    Pass GZero and SAD_LUT info to the appropriate interpolation routine
!      (depending on the array dimensions)
!    Note the SAD_LUT arrays are only interpolated for channels with a
!      thermal component in the case of Rd, Td.
!
! Arguments:
! Name    Type      In/Out/Both Description
! ------------------------------------------------------------------------------
! Ctrl    struct    In          Control structure
! Ind     struct    In          Sub-struct of SPixel, contains channel
!                               indices used in selecting Thermal parts of
!                               the SAD_LUT arrays.
! SAD_LUT struct    In          Static Application Data structure
!                               containing the arrays of Look-Up Tables to
!                               be interpolated.
! GZero   Struct    In          Holds "0'th point" information relating to
!                               the grid on which the SAD_LUT CRP arrays are
!                               based.
! CRPOut  real(8)   Out         Array of interpolated values
! dCRPOut real(8,2) Out         Array of interpolated gradients in Tau, Re
! status  int       Out         Standard status code set by ORAC routines
!
! History:
! 2001/01/16, AS: original version
! 2001/01/23, AS: "Zero'th point" calculation moved out of this routine into a
!    separate subroutine called before this one (info is common to both
!    SetCRPSolar and Thermal). New argument GZero passed in, Tau, Re, Geom
!    arguments no longer required as a result.
! 2001/02/20, AS: New argument Ind. Part of SPixel structure containing updated
!    channel indices. Used to index channels within the SAD_LUT arrays.
! 2001/02/27, AS: Argument First no longer required following the change above.
!    CRP, dCRP array indexing changed since only the required channels will be
!    passed by the calling routine.
! 2011/09/05, CA: Status now passed to interpolation routines IntLUT*.f90
! 2012/02/07, CA: Ctrl struct now passed to interpolation routines IntLUT*.f90
! 2013/12/03, MJ: Makes LUTs more flexible wrt channel and properties.
! 2014/01/16, GM: Made use of i_chan_to_ctrl_offset and i_chan_to_spixel_offset
!    arguments to Int_LUT_TauSatRe().
! 2014/01/20, GM: Cleaned up code.
! 2014/01/24, GM: Some intent changes.
! 2015/01/13, AP: Switch to array-based channel indexing rather than using
!    offsets.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine Set_CRP_Thermal(Ctrl, Ind, chan_to_ctrl_index, GZero, SAD_LUT, &
     CRPOut, dCRPOut, status)

   use Ctrl_m
   use GZero_m
   use Int_LUT_Routines_m
   use SAD_LUT_m
   use SPixel_m

   implicit none

   ! Argument declarations
   ! Note if these arguments are changed, the interface definition in
   ! FMRoutines.f90 must be updated to match.

   type(Ctrl_t),           intent(in)  :: Ctrl
   type(SPixel_Ind_t),     intent(in)  :: Ind
   integer,                intent(in)  :: chan_to_ctrl_index(:)
   type(GZero_t),          intent(in)  :: GZero
                                          ! Struct containing "zero'th" grid
                                          ! points
   type(SAD_LUT_t),        intent(in)  :: SAD_LUT
   real, dimension(:,:),   intent(out) :: CRPOut
                                          ! Interpolated values returned
                                          ! (CRPOut(1)=Rbd, (2)=Tb, ...)
   real, dimension(:,:,:), intent(out) :: dCRPOut
                                          ! Interpolated gradients of CRPOut in
                                          ! Tau and Re
   integer,                intent(out) :: status

   ! Status is not actually set at present. Error-checking would be very costly
   ! in terms of CPU here. Leave status argument in case of future updates. Set
   ! to 0 to avoid compiler warnings.
   status = 0

   ! Interpolation is done over all passed channels - handled by the Int f'ns
   ! N.B. Grid is the same for all SAD_LUT channels in a given cloud class -
   ! channel is a dimension of the LUT arrays inside SAD_LUT.

   ! Td, Rd and Em are interpolated in Tau, SatZen and Re over a range of
   ! channels from First to NY.
   call Int_LUT_TauSatRe(SAD_LUT%Rd, Ind%NThermal, SAD_LUT%Grid, GZero, Ctrl, &
           CRPOut(:,IRd), dCRPOut(:,IRd,:), chan_to_ctrl_index, Ind%YThermal, &
           status)

   call Int_LUT_TauSatRe(SAD_LUT%Td, Ind%NThermal, SAD_LUT%Grid, GZero, Ctrl, &
           CRPOut(:,ITd), dCRPOut(:,ITd,:), chan_to_ctrl_index, Ind%YThermal, &
           status)

   call Int_LUT_TauSatRe(SAD_LUT%Em, Ind%NThermal, SAD_LUT%Grid, GZero, Ctrl, &
           CRPOut(:,IEm), dCRPOut(:,IEm,:), chan_to_ctrl_index, Ind%YThermal, &
           status)

end subroutine Set_CRP_Thermal
