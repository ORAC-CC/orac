!-------------------------------------------------------------------------------
! Name:
!    Set_CRP_Solar
!
! Description:
!    Interpolates Cloud Radiative Properties for the ECP solar channels.
!
!    Takes the SAD LUT array of look up table values and interpolates the arrays
!    of RBd, TB etc from the LUT grid to the current point in the multi-spectral
!    image data.
!
! Arguments:
!    Name    Type      In/Out/Both Description
!
!    Ctrl    struct    In          Standard ECP control structure
!    Ind     struct    In          Sub-struct of SPixel, contains channel
!                                  indices used in selecting Solar parts of the
!                                  SAD_LUT arrays.
!    SAD_LUT struct    In          Static Application Data structure containing
!                                  the arrays of Look-Up Tables to be
!                                  interpolated.
!    GZero   Struct    In          Holds "0'th point" information relating to
!                                  the grid on which the SAD_LUT CRP arrays are
!                                  based.
!    CRPOut  real(8)   Out         Array of interpolated values
!    dCRPOut real(8,2) Out         Array of interpolated gradients in Tau, Re
!    status  int       Out         Standard status code set by ECP routines
!
! Algorithm:
!    For each LUT array in SAD_LUT (i.e. TBd etc)
!       Pass GZero and SAD_LUT info to the appropriate interpolation routine
!          (depending on the array dimensions) and interpolate the Solar
!          channels.
!
! Local variables:
!    Name Type Description
!
! History:
!     2nd Nov 2000, Andy Smith : original version
!    16th Nov 2000, Andy Smith :
!       Extending to interpolate arrays in (Tau, Re, Solzen) (original just
!       handled (Tau, Re) interpolation.
!    24th Nov 2000, Andy Smith :
!       Changed subroutine interface: SatZen, SunZe, RelAzi now passed in Geom
!       structure (defined in SPixel module).
!     1st Dec 2000, Andy Smith :
!       Renamed routines containing sun or Solzen in their names. Using Sol or
!       SolZen instead. Variables using Sun or Su also renamed Sol/So
!    11th Jan 2001, Andy Smith :
!       Ctrl%Ind%Y renamed Y_Id
!    16th Jan 2001, Andy Smith :
!       Header comments brought up to date.
!       Change of array indices for LUT arrays that cover both solar and thermal
!       channels.
!    17th Jan 2001, Andy Smith :
!       Interpolation of Rd removed. Not required in FM_Solar.
!    23rd Jan 2001, Andy Smith :
!       "Zero'th point" calculation moved out of this routine into a separate
!       subroutine called before this one (info is common to both SetCRPSolar
!       and Thermal). New argument GZero passed in, Tau, Re, Geom arguments no
!       longer required as a result.
!    16th Feb 2001, Andy Smith:
!       Only "purely" solar channels are handled by FMSolar and it's
!       subordinates. New argument SPixel: struct contains revised channel
!       indices. Use these to determine which channels to interpolate.
!    19th Feb 2001, Andy Smith:
!       Error in previous update. Ranges of channels interpolated were correct
!       previously, although the index values should be picked up from SPixel.
!    20th Feb 2001, Andy Smith:
!       Use of SPixel argument changed: only the Ind part of SPixel is used
!       hence only this sub-struct is passed. Simplifies array indexing.
!     6th May 2011, Andy Smith:
!       Extension to multiple instrument views. Re-worked debug output to new
!       viewing geometry arrays.
!     5th Sep 2011, Chris Arnold:
!       Status now passed to interpolation routines IntLUT*.f90
!     7th Feb 2012, Chris Arnold:
!       Ctrl struct now passed to interpolation routines IntLUT*.f90
!     3rd Dec 2013, MJ:
!       Makes LUTs more flexible wrt channel and properties.
!    16th Jan 2014, Greg McGarragh:
!       Made use of i_chan_to_ctrl_offset and i_chan_to_spixel_offset arguments
!       to Int_LUT_TauSatRe().
!    20th Dec 2014, Greg McGarragh:
!       Cleaned up code.
!    24th Dec 2014, Greg McGarragh:
!       Some intent changes.
!    28th May 2014, Greg McGarragh:
!       Do not assume that Set_CRP_Solar() took care of the mixed channels.
!
! Bugs:
!    None known.
!
! $Id$
!
!-------------------------------------------------------------------------------

subroutine Set_CRP_Solar(Ctrl, Ind, GZero, SAD_LUT, CRPOut, dCRPOut, status)

   use Ctrl_def
   use GZero_def
   use Int_LUT_Routines_def
   use SAD_LUT_def
   use SPixel_def

   implicit none

   ! Argument declarations
   ! Note if these arguments are changed, the interface definition in
   ! FMRoutines.f90 must be updated to match.

   type(Ctrl_t),           intent(in)  :: Ctrl
   type(SPixel_Ind_t),     intent(in)  :: Ind
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

   ! Call functions to interpolate the arrays: TFd and RFD are interpolated only
   ! in Tau and Re.
   call Int_LUT_TauRe(SAD_LUT%RFd(Ind%SolarFirst:Ind%SolarLast,:,:), &
           SAD_LUT%Grid, GZero,Ctrl, CRPOut(Ind%SolarFirst:Ind%SolarLast,IRFd), &
           dCRPOut(Ind%SolarFirst:Ind%SolarLast,IRFd,:),IRFd,status)

   call Int_LUT_TauRe(SAD_LUT%TFd(Ind%SolarFirst:Ind%SolarLast,:,:), &
           SAD_LUT%Grid, GZero,Ctrl, CRPOut(Ind%SolarFirst:Ind%SolarLast,ITFd), &
           dCRPOut(Ind%SolarFirst:Ind%SolarLast,ITFd,:),ITFd,status)

   ! Tb and TFBd are interpolated in Tau, Solzen and Re
   call Int_LUT_TauSolRe(SAD_LUT%Tb(Ind%SolarFirst:Ind%SolarLast,:,:,:), &
           SAD_LUT%Grid, GZero,Ctrl, CRPOut(Ind%SolarFirst:Ind%SolarLast,ITB), &
           dCRPOut(Ind%SolarFirst:Ind%SolarLast,ITB,:),ITB,status)

   call Int_LUT_TauSolRe(SAD_LUT%TFbd(Ind%SolarFirst:Ind%SolarLast,:,:,:), &
           SAD_LUT%Grid, GZero, Ctrl, CRPOut(Ind%SolarFirst:Ind%SolarLast,ITFbd), &
           dCRPOut(Ind%SolarFirst:Ind%SolarLast,ITFBd,:),ITFBd,status)

   ! Td is interpolated in Tau, SatZen and Re Only process the channels that are
   ! exclusively solar. Channels with a thermal component are interpolated by
   ! SetCRPThermal.
   !
   ! 2014/05/28, GM: This was causing problems that were hard to debug and
   ! gained little in performance.  Now the Solar and Thermal forward model
   ! calls are independent so that contents of CRP and d_CRP do not need to be
   ! passed from the thermal call to the solar call.

   call Int_LUT_TauSatRe(SAD_LUT%Td(Ind%SolarFirst:Ind%SolarLast,:,:,:), &
           SAD_LUT%Grid, GZero,Ctrl, CRPOut(Ind%SolarFirst:Ind%SolarLast,ITd), &
           dCRPOut(Ind%SolarFirst:Ind%SolarLast,ITd,:),ITd,0,0,status)

   ! RBd is interpolated in Tau, SatZen, SolZen, RelAzi and Re
   call Int_LUT_TauSatSolAziRe(SAD_LUT%RBd(Ind%SolarFirst:Ind%SolarLast,:,:,:,:,:), &
           SAD_LUT%Grid, GZero, Ctrl, CRPOut(Ind%SolarFirst:Ind%SolarLast,IRBd), &
           dCRPOut(Ind%SolarFirst:Ind%SolarLast,IRBd,:),iRBd,status)

#ifdef DEBUG
!   write(*,*) ' SetCRPSolar: Tb values (2 channels only)'
!   do j=Ind%SolarFirst, Ind%SolarFirst+1
!      write(*,'(a,4(f10.5,1x))')' Function values at top corners', &
!	 SAD_LUT%Tb(j, GZero%iT0, GZero%iSoZ0(j), GZero%iR1),  &
!	 SAD_LUT%Tb(j, GZero%iT1, GZero%iSoZ0(j), GZero%iR1),  &
!	 SAD_LUT%Tb(j, GZero%iT0, GZero%iSoZ1(j), GZero%iR1),  &
!	 SAD_LUT%Tb(j, GZero%iT1, GZero%iSoZ1(j), GZero%iR1)
!      write(*,'(a,4(f10.5,1x))')' Function values at bot corners', &
!	 SAD_LUT%Tb(j, GZero%iT0, GZero%iSoZ0(j), GZero%iR0),  &
!	 SAD_LUT%Tb(j, GZero%iT1, GZero%iSoZ0(j), GZero%iR0),  &
!	 SAD_LUT%Tb(j, GZero%iT0, GZero%iSoZ1(j), GZero%iR0),  &
!	 SAD_LUT%Tb(j, GZero%iT1, GZero%iSoZ1(j), GZero%iR0)
!
!      write(*,*)' Interpolated value ',CRPOut(j,ITB)
!      write(*,*)' Gradient values    ',(dCRPOut(j,ITB,i),i=1,2)
!      write(*,*)
!   end do

!  For debugging: check interpolated values
   write(*,*) ' SetCRPSolar: TFd values, solar 1st to solar last: ',Ind%SolarFirst,&
     Ind%SolarLast
   do j=Ind%SolarFirst, Ind%SolarLast
      write(*,*)' Function values at top corners', &
         SAD_LUT%TFd(j, GZero%iT0(j,iTFd), GZero%iR1(j,iTFd)),  &
         SAD_LUT%TFd(j, GZero%iT1(j,iTFd), GZero%iR1(j,iTFd))
      write(*,*)' Function values at bot corners', &
         SAD_LUT%TFd(j, GZero%iT0(j,iTFd), GZero%iR0(j,iTFd)),  &
         SAD_LUT%TFd(j, GZero%iT1(j,iTFd), GZero%iR0(j,iTFd))

      write(*,*)' Interpolated value ',CRPOut(j,ITFd)
      write(*,*)' Gradient values    ',(dCRPOut(j,ITFd,i),i=1,2)
      write(*,*)
   end do

   write(*,*) ' SetCRPSolar: RFd values'
   do j=Ind%SolarFirst, Ind%SolarLast
      write(*,*) 'channels',j,Ind%SolarFirst, Ind%SolarLast
      write(*,*)' Function values at top corners', &
         SAD_LUT%RFd(j, GZero%iT0(j,iRFd), GZero%iR1(j,iRFd)),  &
         SAD_LUT%RFd(j, GZero%iT1(j,iRFd), GZero%iR1(j,iRFd))
      write(*,*)' Function values at bot corners', &
        SAD_LUT%RFd(j, GZero%iT0(j,iRFd), GZero%iR0(j,iRFd)),  &
        SAD_LUT%RFd(j, GZero%iT1(j,iRFd), GZero%iR0(j,iRFd))

      write(*,*)' Interpolated value ',CRPOut(j,IRFd)
      write(*,*)' Gradient values    ',(dCRPOut(j,IRFd,i),i=1,2)
      write(*,*) 'Indices of corners', GZero%iT0(j,iRFd), GZero%iT1(j,iRFd),GZero%iR0(j,iRFd), GZero%iR1(j,iRFd)
      write(*,*) 'ranges of indices',SAD_LUT%Grid%nTau(j,iRFd),SAD_LUT%Grid%nre(j,iRFd)
      write(*,*)
   end do
#endif

end subroutine Set_CRP_Solar
