!-------------------------------------------------------------------------------
! Name: SetCRPSolar.F90
!
! Purpose:
! Interpolates Cloud Radiative Properties for the ECP solar channels.
!
! Description and Algorithm details:
! Takes the SAD LUT array of look up table values and interpolates the arrays
! of RBd, TB etc from the LUT grid to the current point in the multi-spectral
! image data.
!
! For each LUT array in SAD_LUT (i.e. TBd etc)
!    Pass GZero and SAD_LUT info to the appropriate interpolation routine
!       (depending on the array dimensions) and interpolate the Solar channels.
!
! Arguments:
! Name               Type      In/Out/Both Description
! ------------------------------------------------------------------------------
! Ctrl               struct    In          Standard ECP control structure
! Ind                struct    In          Sub-struct of SPixel, contains channel
!                                          indices used in selecting Solar parts
!                                          of the SAD_LUT arrays.
! chan_to_ctrl_index int       In          Indices for each SAD channel, giving
!                                          the index in the Ctrl arrays
! SAD_LUT            struct    In          Static Application Data structure
!                                          containing the arrays of Look-Up
!                                          Tables to be interpolated.
! GZero              Struct    In          Holds "0'th point" information
!                                          relating to  the grid on which the
!                                          SAD_LUT CRP arrays are based.
! CRPOut             real(8)   Out         Array of interpolated values
! dCRPOut            real(8,2) Out         Array of interpolated gradients in
!                                          Tau, Re
! status             int       Out         Standard status code set by ECP
!                                          routines
!
! History:
! 2000/11/02, AS: original version
! 2000/11/16, AS: Extending to interpolate arrays in (Tau, Re, Solzen) (original
!    just handled (Tau, Re) interpolation.
! 2000/11/24, AS: Changed subroutine interface: SatZen, SunZe, RelAzi now passed
!    in Geom structure (defined in SPixel module).
! 2000/12/01, AS: Renamed routines containing sun or Solzen in their names.
!    Using Sol or SolZen instead. Variables using Sun or Su also renamed Sol/So
! 2001/01/11, AS: Ctrl%Ind%Y renamed Y_Id
! 2001/01/16, AS: Header comments brought up to date. Change of array indices
!    for LUT arrays that cover both solar and thermal channels.
! 2001/01/17, AS: Interpolation of Rd removed. Not required in FM_Solar.
! 2001/01/23, AS: "Zero'th point" calculation moved out of this routine into a
!    separate subroutine called before this one (info is common to both
!    SetCRPSolar and Thermal). New argument GZero passed in, Tau, Re, Geom
!    arguments no longer required as a result.
! 2001/02/16, AS: Only "purely" solar channels are handled by FMSolar and it's
!    subordinates. New argument SPixel: struct contains revised channel indices.
!    Use these to determine which channels to interpolate.
! 2001/02/19, AS: Error in previous update. Ranges of channels interpolated were
!    correct previously, although the index values should be picked up
!    from SPixel.
! 2001/02/20, AS: Use of SPixel argument changed: only the Ind part of SPixel is
!    used hence only this sub-struct is passed. Simplifies array indexing.
! 2011/05/06, AS: Extension to multiple instrument views. Re-worked debug output
!    to new viewing geometry arrays.
! 2011/09/05, CA: Status now passed to interpolation routines IntLUT*.f90
! 2012/02/07, CA: Ctrl struct now passed to interpolation routines IntLUT*.f90
! 2013/12/03, MJ: Makes LUTs more flexible wrt channel and properties.
! 2014/01/16, GM: Made use of i_chan_to_ctrl_offset and i_chan_to_spixel_offset
!    arguments to Int_LUT_TauSatRe().
! 2014/01/20, GM: Cleaned up code.
! 2014/01/24, GM: Some intent changes.
! 2014/05/28, GM: Do not assume that Set_CRP_Solar() took care of the mixed
!    channels.
! 2014/09/09, GM: Changes related to new BRDF support.
! 2014/12/01, CP: Added in interpolation for cloud albedo
! 2015/01/13, AP: Switch to array-based channel indexing rather than using
!    offsets.
! 2015/10/21, GM: Removed interpolation for cloud albedo as it is now done
!    elsewhere.
! 2016/06/06, GM: Obtain the operator T_dv (TD) from the T_0d (TFBD) LUT when
!    Ctrl%get_T_dv_from_T_0d=.true.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine Set_CRP_Solar(Ctrl, Ind, chan_to_ctrl_index, GZero, SAD_LUT, &
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
                                          ! (CRPOut(1)=RBD, (2)=TB, ...)
   real, dimension(:,:,:), intent(out) :: dCRPOut
                                          ! Interpolated gradients of CRPOut in
                                          ! Tau and Re
   integer,                intent(out) :: status

   ! Local variables
#ifdef DEBUG
   integer :: i, j
#endif

   ! Status is not actually set at present. Error-checking would be very costly
   ! in terms of CPU here. Leave status argument in case of future updates. Set
   ! to 0 to avoid compiler warnings.
   status = 0

   ! Interpolation is done over all passed channels - handled by the Int f'ns
   ! N.B. Grid is the same for all SAD_LUT channels in a given cloud class -
   ! channel is a dimension of the LUT arrays inside SAD_LUT.

   ! Call functions to interpolate the arrays

   call Int_LUT_TauSatSolAziRe(SAD_LUT%Rbd, Ind%NSolar, &
           SAD_LUT%Grid, GZero, Ctrl, CRPOut(:, IRBd), dCRPOut(:,IRBd,:), &
           IRBd, chan_to_ctrl_index, Ind%YSolar, status)

   call Int_LUT_TauRe(SAD_LUT%Rfd, Ind%NSolar, &
           SAD_LUT%Grid, GZero, Ctrl, CRPOut(:,IRFd), dCRPOut(:,IRFd,:), &
           IRFd,chan_to_ctrl_index, Ind%YSolar,  status)

   call Int_LUT_TauSolRe(SAD_LUT%Tb, Ind%NSolar, &
           SAD_LUT%Grid, GZero, Ctrl, CRPOut(:,ITB), dCRPOut(:,ITB,:), &
           ITB, chan_to_ctrl_index, Ind%YSolar, status)

   call Int_LUT_TauSatReOnSol(SAD_LUT%Tb, Ind%NSolar, &
           SAD_LUT%Grid, GZero, Ctrl, CRPOut(:,ITB_u), dCRPOut(:,ITB_u,:), &
           ITB, chan_to_ctrl_index, Ind%YSolar, status)

   call Int_LUT_TauSolRe(SAD_LUT%Tfbd, Ind%NSolar, &
           SAD_LUT%Grid, GZero, Ctrl, CRPOut(:,ITFbd), dCRPOut(:,ITFBd,:), &
           ITFBd, chan_to_ctrl_index, Ind%YSolar, status)

   ! See detailed description of Ctrl%get_T_dv_from_T_0d in ReadDriver.F90
   if (.not. Ctrl%get_T_dv_from_T_0d) then
      call Int_LUT_TauSatRe(SAD_LUT%Td, Ind%NSolar, &
              SAD_LUT%Grid, GZero, Ctrl, CRPOut(:,ITd), dCRPOut(:,ITd,:), &
              ITd, chan_to_ctrl_index, Ind%YSolar, status)
   else
      call Int_LUT_TauSatRe(SAD_LUT%Tfbd, Ind%NSolar, &
              SAD_LUT%Grid, GZero, Ctrl, CRPOut(:,ITd), dCRPOut(:,ITd,:), &
              ITd, chan_to_ctrl_index, Ind%YSolar, status)
   end if

   call Int_LUT_TauRe(SAD_LUT%Tfd, Ind%NSolar, &
           SAD_LUT%Grid, GZero, Ctrl, CRPOut(:,ITFd), dCRPOut(:,ITFd,:), &
           ITFd, chan_to_ctrl_index, Ind%YSolar, status)

#ifdef BKP
!   write(*,*) ' SetCRPSolar: Tb values (2 channels only)'
!   do j=Ind%SolarFirst, Ind%SolarFirst+1
!      write(*,'(a,4(f10.5,1x))')' Function values at top corners', &
!           SAD_LUT%Tb(j, GZero%iT0, GZero%iSoZ0(j), GZero%iR1),  &
!           SAD_LUT%Tb(j, GZero%iT1, GZero%iSoZ0(j), GZero%iR1),  &
!           SAD_LUT%Tb(j, GZero%iT0, GZero%iSoZ1(j), GZero%iR1),  &
!           SAD_LUT%Tb(j, GZero%iT1, GZero%iSoZ1(j), GZero%iR1)
!      write(*,'(a,4(f10.5,1x))')' Function values at bot corners', &
!           SAD_LUT%Tb(j, GZero%iT0, GZero%iSoZ0(j), GZero%iR0),  &
!           SAD_LUT%Tb(j, GZero%iT1, GZero%iSoZ0(j), GZero%iR0),  &
!           SAD_LUT%Tb(j, GZero%iT0, GZero%iSoZ1(j), GZero%iR0),  &
!           SAD_LUT%Tb(j, GZero%iT1, GZero%iSoZ1(j), GZero%iR0)
!
!      write(*,*)' Interpolated value ',CRPOut(j,ITB)
!      write(*,*)' Gradient values    ',(dCRPOut(j,ITB,i),i=1,2)
!      write(*,*)
!   end do

!  For debugging: check interpolated values
   write(*,*) ' SetCRPSolar: TFd values, solar 1st to solar last: ', &
      Ind%SolarFirst,Ind%SolarLast
   do j=Ind%SolarFirst, Ind%SolarLast
      write(*,*)' Function values at top corners', &
         SAD_LUT%TFd(j, GZero%iT0(j,ITFd), GZero%iR1(j,ITFd)), &
         SAD_LUT%TFd(j, GZero%iT1(j,ITFd), GZero%iR1(j,ITFd))
      write(*,*)' Function values at bot corners', &
         SAD_LUT%TFd(j, GZero%iT0(j,ITFd), GZero%iR0(j,ITFd)), &
         SAD_LUT%TFd(j, GZero%iT1(j,ITFd), GZero%iR0(j,ITFd))

      write(*,*)' Interpolated value ',CRPOut(j,ITFd)
      write(*,*)' Gradient values    ',(dCRPOut(j,ITFd,i),i=1,2)
      write(*,*)
   end do

   write(*,*) ' SetCRPSolar: RFd values'
   do j=Ind%SolarFirst, Ind%SolarLast
      write(*,*) 'channels',j,Ind%SolarFirst, Ind%SolarLast
      write(*,*)' Function values at top corners', &
         SAD_LUT%RFd(j, GZero%iT0(j,IRFd), GZero%iR1(j,IRFd)), &
         SAD_LUT%RFd(j, GZero%iT1(j,IRFd), GZero%iR1(j,IRFd))
      write(*,*)' Function values at bot corners', &
        SAD_LUT%RFd(j, GZero%iT0(j,IRFd), GZero%iR0(j,IRFd)), &
        SAD_LUT%RFd(j, GZero%iT1(j,IRFd), GZero%iR0(j,IRFd))

      write(*,*)' Interpolated value ',CRPOut(j,IRFd)
      write(*,*)' Gradient values    ',(dCRPOut(j,IRFd,i),i=1,2)
      write(*,*) 'Indices of corners', GZero%iT0(j,IRFd), GZero%iT1(j,IRFd), &
         GZero%iR0(j,IRFd), GZero%iR1(j,IRFd)
      write(*,*) 'ranges of indices',SAD_LUT%Grid%nTau(j,IRFd), &
         SAD_LUT%Grid%nre(j,IRFd)
      write(*,*)
   end do
#endif


end subroutine Set_CRP_Solar
