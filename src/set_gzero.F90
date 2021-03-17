!-------------------------------------------------------------------------------
! Name: set_gzero.F90
!
! Purpose:
! Sets up the "zero'th point" grid information used in interpolating
! the SAD_LUT CRP arrays.
!
! Description and Algorithm details:
! Use the grid info in SAD_LUT and the current Tau, Re and Geom values to
! find the "zero'th" point for interpolation (see IntLUTTauRe comments).
! Populate GZero struct with zero'th point data, i.e.
!  - index of closest (lower) point in LUT data grid, in Tau, Re, SatZen etc
!    (i.e. the zero'th point) - ensure that 0'th point index is between 1 and
!    npoints-1
!  - index of next-nearest points (set to nearest point if we're at the edge
!    of the LUT).
!  - Denominator for calculating finite difference gradients. Will either be
!    equal to LUT grid spacing (at edges if LUT), or twice this.
!  - fractional grid step from 0'th point to supplied Tau, Re, Sat zen etc
!  - 1 minus fractional step from above
!
! Arguments:
! Name    Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! Tau     real   In          Current value of optical depth
! Re      real   In          Current value of effective radius
! Ctrl    struct In          Control structure
! SAD_LUT struct In          Static Application Data structure  containing
!                            the arrays of Look-Up Tables to be interpolated
!                            and the grids on which they are stored.
! GZero   Struct Out         Holds "0'th point" information relating to the
!                            grid on which the SAD_LUT CRP arrays are based.
! status  int    Out         Standard status code set by ORAC routines
!
! History:
! 2001/01/23, AS: original version
! 2007/05/31, AY: Consolidation of code for dual-view BRDF retrieval. This
!    means added calculation of new geometry variables for second view (copied
!    existing code, adding "_2" suffix).
! 2008/03/21, GT: Changes for spline interpolation: Added mT0, mT1, mR0, mR1
!    calculations - gradients at iT0, iT1, iR0 and iR1 LUT points.
! 2001/09/05, CA: 'locate' routine is now used for domain searching
! 2011/09/05, CA: next nearest neighbour indices iTm1,iTp1,iRm1,iRp1 now
!    evaluated
! 2012/12/13, CP: added in allocation of isaz0 parameter
! 2012/09/15, CP: remove deallocation test from this routine and instead
!    deallocate at the end of FM.F90 this solved g95
! 2013/05/08, MJ: changes loop boundaries and parameter list contents memory
!    problem
! 2013/12/03, MJ: makes LUTs more flexible wrt channel and properties
! 2014/01/16, GM: Fixed indexing operations on uninitialized (garbage) values
!    introducing use of SPixel%spixel_y_to_ctrl_y_index and the
!    SAD_LUT%table_use* arrays.
! 2014/01/23, GM: Cleaned up code.
! 2014/09/09, GM: Changes related to new BRDF support.
! 2017/01/17, GM: Eliminate the unnecessary indexing of the GZero parameters wrt
!    LUT type and changes related to similar indexing simplifications of the LUT
!    grid.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine Set_GZero(Tau, Re, Ctrl, SPixel, SAD_LUT, GZero, status)

   use Ctrl_m
   use Int_Routines_m
   use SAD_LUT_m
   use SPixel_m

   implicit none

   ! Argument declarations

   real,            intent(in)    :: Tau
   real,            intent(in)    :: Re
   type(Ctrl_t),    intent(in)    :: Ctrl
   type(SPixel_t),  intent(in)    :: SPixel
   type(SAD_LUT_t), intent(in)    :: SAD_LUT
   type(GZero_t),   intent(inout) :: GZero
   integer,         intent(out)   :: status

   ! Local variables

   integer :: i

   ! Status is not actually set at present. Error-checking here could be very
   ! costly in terms of CPU. Leave status argument in case of future updates.
   ! Set to 0 to avoid compiler warnings.
   status = 0

   ! Set the "zero'th" array indices for the interpolations, i.e. find the array
   ! indices of the nearest neighbour grid points in each dimension. Use the
   ! 'locate' function
   do i = 1, SPixel%Ind%Ny
      GZero%iT0(i) = &
         max(min(locate(SAD_LUT%Grid%Tau(1:SAD_LUT%Grid%nTau),Tau), &
                 SAD_LUT%Grid%nTau-1),1)
      GZero%iR0(i) = &
         max(min(locate(SAD_LUT%Grid%Re (1:SAD_LUT%Grid%nRe ),Re), &
                 SAD_LUT%Grid%nRe-1),1)
      GZero%iSaZ0(i) = &
         max(min(locate(SAD_LUT%Grid%SatZen(1:SAD_LUT%Grid%nSatzen),&
                 SPixel%Geom%Satzen(SPixel%ViewIdx(i))),SAD_LUT%Grid%nSatzen-1),1)
      GZero%iSoZ0(i) = &
         max(min(locate(SAD_LUT%Grid%SolZen(1:SAD_LUT%Grid%nSolzen),&
                 SPixel%Geom%Solzen(SPixel%ViewIdx(i))),SAD_LUT%Grid%nSolzen-1),1)
      GZero%iSaZSoZ0(i) = &
         max(min(locate(SAD_LUT%Grid%SolZen(1:SAD_LUT%Grid%nSolzen),&
                 SPixel%Geom%Satzen(SPixel%ViewIdx(i))),SAD_LUT%Grid%nSolzen-1),1)
      GZero%iRA0(i) = &
         max(min(locate(SAD_LUT%Grid%Relazi(1:SAD_LUT%Grid%nRelazi),&
                 SPixel%Geom%Relazi(SPixel%ViewIdx(i))),SAD_LUT%Grid%nRelazi-1),1)
   end do

   ! This sets the upper bracketing index, the locate above set the lower index
   do i = 1, SPixel%Ind%Ny
      GZero%iT1(i)      = GZero%iT0(i)   + 1
      GZero%iR1(i)      = GZero%iR0(i)   + 1

      GZero%iSaZ1(i)    = GZero%iSaZ0(i) + 1
      GZero%iSoZ1(i)    = GZero%iSoZ0(i) + 1
      GZero%iRA1(i)     = GZero%iRA0(i)  + 1

      GZero%iSaZSoZ1(i) = GZero%iSaZSoZ0(i) + 1
   end do

   ! This sets the next pair of bracketing indices around the primary one
   do i = 1, SPixel%Ind%Ny
      if (GZero%iT0(i) == 1) then
         GZero%iTm1(i) = GZero%iT0(i)
         GZero%iTp1(i) = GZero%iT1(i)+1
      else if (GZero%iT1(i) == SAD_LUT%Grid%nTau) then
         GZero%iTm1(i) = GZero%iT0(i)-1
         GZero%iTp1(i) = GZero%iT1(i)
      else
         GZero%iTm1(i) = GZero%iT0(i)-1
         GZero%iTp1(i) = GZero%iT1(i)+1
      end if

      if (GZero%iR0(i) == 1) then
         GZero%iRm1(i) = GZero%iR0(i)
         GZero%iRp1(i) = GZero%iR1(i)+1
      else if (GZero%iR1(i) == SAD_LUT%Grid%nRe) then
         GZero%iRm1(i) = GZero%iR0(i)-1
         GZero%iRp1(i) = GZero%iR1(i)
      else
         GZero%iRm1(i) = GZero%iR0(i)-1
         GZero%iRp1(i) = GZero%iR1(i)+1
      end if
   end do

   ! Calculate dT, dR: these are the distances in T, R, etc from the grid point
   ! with indices (0, 0, ...) to (Tau, Re, ...) expressed as a fraction of the
   ! LUT grid steps.

   ! These variables are not used and could be removed? No, they are used!
   do i = 1, SPixel%Ind%Ny
      GZero%dT(i) = (Tau - SAD_LUT%Grid%Tau(GZero%iT0(i))) / &
         (SAD_LUT%Grid%Tau(GZero%iT1(i)) - SAD_LUT%Grid%Tau(GZero%iT0(i)))
      GZero%dR(i) = (Re - SAD_LUT%Grid%Re(GZero%iR0(i))) / &
         (SAD_LUT%Grid%Re(GZero%iR1(i)) - SAD_LUT%Grid%Re(GZero%iR0(i)))
      GZero%dSaZ(i) = &
         (SPixel%Geom%SatZen(SPixel%ViewIdx(i)) - SAD_LUT%Grid%SatZen(GZero%iSaZ0(i))) / &
         (SAD_LUT%Grid%SatZen(GZero%iSaZ1(i)) - SAD_LUT%Grid%SatZen(GZero%iSaZ0(i)))
      GZero%dSoZ(i) = &
         (SPixel%Geom%SolZen(SPixel%ViewIdx(i)) - SAD_LUT%Grid%SolZen(GZero%iSoZ0(i))) / &
         (SAD_LUT%Grid%SolZen(GZero%iSoZ1(i)) - SAD_LUT%Grid%SolZen(GZero%iSoZ0(i)))
      GZero%dSaZSoZ(i) = &
         (SPixel%Geom%SatZen(SPixel%ViewIdx(i)) - SAD_LUT%Grid%SolZen(GZero%iSaZSoZ0(i))) / &
         (SAD_LUT%Grid%SolZen(GZero%iSaZSoZ1(i)) - SAD_LUT%Grid%SolZen(GZero%iSaZSoZ0(i)))
      GZero%dRA(i) = &
         (SPixel%Geom%RelAzi(SPixel%ViewIdx(i)) - SAD_LUT%Grid%RelAzi(GZero%iRA0(i))) / &
         (SAD_LUT%Grid%RelAzi(GZero%iRA1(i)) - SAD_LUT%Grid%RelAzi(GZero%iRA0(i)))
   end do

   ! Calculate 1.0 minus each of the d values above - used several times by the
   ! interpolation routines.
   do i = 1, SPixel%Ind%Ny
      GZero%T1(i)    = 1.0 - GZero%dT(i)
      GZero%R1(i)    = 1.0 - GZero%dR(i)
      GZero%Sa1(i)   = 1.0 - GZero%dSaZ(i)
      GZero%So1(i)   = 1.0 - GZero%dSoZ(i)
      GZero%Ra1(i)   = 1.0 - GZero%dRA(i)
      GZero%SaSo1(i) = 1.0 - GZero%dSaZSoZ(i)
   end do

end subroutine Set_GZero
