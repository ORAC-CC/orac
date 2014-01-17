! Name:
!    Set_CRP_Thermal
!
! Description:
!    Interpolates Cloud Radiative Properties for the ECP thermal channels
!    (and the channels with thermal plus solar contributions).
!
!    Takes the SAD LUT array of look up table values and interpolates the
!    arrays of RBd, TB etc from the LUT grid to the current point in the 
!    multi-spectral image data.
!
! Arguments:
!    Name       Type    In/Out/Both  Description
!
!    Ctrl       struct  In           Standard ECP control structure
!    Ind        struct  In           Sub-struct of SPixel, contains channel
!                                    indices used in selecting Thermal parts
!                                    of the SAD_LUT arrays.
!    SAD_LUT    struct  In           Static Application Data structure 
!                                    containing the arrays of Look-Up Tables
!                                    to be interpolated.
!    GZero      Struct  In           Holds "0'th point" information relating to
!                                    the grid on which the SAD_LUT CRP arrays
!                                    are based.
!    CRPOut     real(8)  Out         Array of interpolated values:
!                                    1=RBd, 2=Tb, 3=TFBd, 4=Td, 5=TFd, 6=Rd,
!                                    7=RFd, 8=Em
!    dCRPOut    real(8,2)  Out       Array of interpolated gradients in Tau, Re:
!                                    1=RBd, 2=Tb, 3=TFBd, 4=Td, 5=TFd, 6=Rd,
!                                    7=RFd, 8=Em
!                                    7=RFd, 8=Em
!    status     int     Out          Standard status code set by ECP routines
!
! Algorithm:
!    For each LUT array in SAD_LUT (i.e. TBd etc)
!    - Pass GZero and SAD_LUT info to the appropriate interpolation routine
!      (depending on the array dimensions)
!      Note the SAD_LUT arrays are only interpolated for channels with a 
!      thermal component in the case of Rd, Td. 
!
! Local variables:
!    Name       Type    Description
!
! History:
!    16th Jan 2001, Andy Smith : original version
!    23rd Jan 2001, Andy Smith :
!      "Zero'th point" calculation moved out of this routine into a separate 
!      subroutine called before this one (info is common to both SetCRPSolar 
!      and Thermal). New argument GZero passed in, Tau, Re, Geom arguments 
!      no longer required as a result.
!    20th Feb 2001, Andy Smith:
!       New argument Ind. Part of SPixel structure containing updated channel 
!       indices. Used to index channels within the SAD_LUT arrays.
!    27th Feb 2001, Andy Smith:
!       Argument First no longer required folowing the change above.
!       CRP, dCRP array indexing changed since only the required channels
!       will be passed by the calling routine.
!    5th Sep 2011, Chris Arnold:
!       Status now passed to interpolation routines IntLUT*.f90
!    7th Feb 2012, Chris Arnold:
!       Ctrl struct now passed to interpolation routines IntLUT*.f90
!20131203 MJ makes LUTs more flexible wrt channel and properties
!   16th Jan 2014, Greg McGarragh:
!       Made use of i_chan_to_ctrl_offset and i_chan_to_spixel_offset arguments
!       to Int_LUT_TauSatRe().
!
! Bugs:
!    None known.
!
!---------------------------------------------------------------------

Subroutine Set_CRP_Thermal (Ctrl, Ind, GZero, SAD_LUT, &
   CRPOut, dCRPOut, status)

   use Int_Routines_def
   use Ctrl_def
   use SPixel_def
   use SAD_LUT_def
   use GZero_def
   
   implicit none

!  Argument declarations 
!  Note if these arguments are changed, the interface definition in
!  FMRoutines.f90 must be updated to match.
   
   type(Ctrl_t), intent(in)              :: Ctrl
   type(Spixel_Ind_t), intent(in)        :: Ind
   type(GZero_t), intent(inout)          :: GZero  ! Struct containing 
                                                   ! "zero'th" grid points
   type(SAD_LUT_t), intent(inout)        :: SAD_LUT
   real, dimension(:,:), intent(inout)   :: CRPOut  
                        		   ! Interpolated values returned
                        		   ! (CRPOut(1)=RBD, (2)=TB, ...
   real, dimension(:,:,:), intent(inout) :: dCRPOut
                                	   ! Interpolated gradients of CRPOut in 
					   ! Tau and Re
   integer, intent(inout)                :: status

!  Status is not actually set at present. Error-checking would be very
!  costly in terms of CPU here. Leave status argument in case of future
!  updates. Set to 0 to avoid compiler warnings.

   status = 0

!  Interpolation is done over all passed channels - handled by the Int f'ns
!  N.B. Grid is the same for all SAD_LUT channels in a given cloud class
!  - channel is a dimension of the LUT arrays inside SAD_LUT.

!  Td, Rd and Em are interpolated in Tau, SatZen and Re over a range of 
!  channels from First to NY. 

   call Int_LUT_TauSatRe(SAD_LUT%Td(Ind%ThermalFirst:Ind%ThermalLast,:,:,:), &
        & SAD_LUT%Grid, GZero,Ctrl, CRPOut(:,ITd), dCRPOut(:,ITd,:), &
        & iTd, Ctrl%Ind%NSolar - Ctrl%Ind%NMixed, Ind%NSolar - Ind%NMixed, status)

   call Int_LUT_TauSatRe(SAD_LUT%Rd(Ind%ThermalFirst:Ind%ThermalLast,:,:,:), &
        & SAD_LUT%Grid, GZero, Ctrl, CRPOut(:,IRd), dCRPOut(:,IRd,:), &
        & iRd, Ctrl%Ind%NSolar - Ctrl%Ind%NMixed,Ind%NSolar - Ind%NMixed, status)

   call Int_LUT_TauSatRe(SAD_LUT%Em(Ind%ThermalFirst:Ind%ThermalLast,:,:,:), &
        & SAD_LUT%Grid, GZero,Ctrl, CRPOut(:,IEm), dCRPOut(:,IEm,:), &
        & iEm, Ctrl%Ind%NSolar - Ctrl%Ind%NMixed,Ind%NSolar - Ind%NMixed, status)

!MJ ORG
!!$   call Int_LUT_TauSatRe(SAD_LUT%Td(Ind%ThermalFirst:Ind%ThermalLast,:,:,:), &
!!$      SAD_LUT%Grid, GZero, Ctrl, CRPOut(:,ITd), dCRPOut(:,ITd,:),status)
!!$
!!$   call Int_LUT_TauSatRe(SAD_LUT%Rd(Ind%ThermalFirst:Ind%ThermalLast,:,:,:), &
!!$      SAD_LUT%Grid, GZero, Ctrl, CRPOut(:,IRd), dCRPOut(:,IRd,:),status)
!!$
!!$   call Int_LUT_TauSatRe(SAD_LUT%Em(Ind%ThermalFirst:Ind%ThermalLast,:,:,:), &
!!$      SAD_LUT%Grid, GZero, Ctrl, CRPOut(:,IEm), dCRPOut(:,IEm,:),status)


End Subroutine Set_CRP_Thermal
