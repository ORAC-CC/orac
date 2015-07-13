!-------------------------------------------------------------------------------
! Name: SetGZero.F90
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
! status  int    Out         Standard status code set by ECP routines
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
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine Set_GZero(Tau, Re, Ctrl, SPixel, SAD_LUT, GZero, status)

   use Ctrl_def
   use Int_Routines_def
   use SAD_LUT_def
   use SPixel_def

   implicit none

   ! Argument declarations

   real,            intent(in)  :: Tau
   real,            intent(in)  :: Re
   type(CTRL_t),    intent(in)  :: Ctrl
   type(SPixel_t),  intent(in)  :: SPixel
   type(SAD_LUT_t), intent(in)  :: SAD_LUT
   type(GZero_t),   intent(out) :: GZero
   integer,         intent(out) :: status

   ! Local variables

   integer :: i,ii,j

   ! Status is not actually set at present. Error-checking here could be very
   ! costly in terms of CPU. Leave status argument in case of future updates.
   ! Set to 0 to avoid compiler warnings.
   status = 0

   ! Set the "zero'th" array indices for the interpolations, i.e. find the array
   ! indices of the nearest neighbour grid points in each dimension. Use the
   ! 'locate' function
   do j=1,maxcrprops
      do i=1,SPixel%Ind%Ny
         ii = SPixel%spixel_y_to_ctrl_y_index(i)

         if (SAD_LUT%table_used_for_channel(ii, j)) then
            GZero%iT0(i,j) = max(min(locate(SAD_LUT%Grid%Tau(ii,1:SAD_LUT%Grid%nTau(ii,j),j),Tau), &
                                     SAD_LUT%Grid%nTau(ii,j)-1),1)
            GZero%iR0(i,j) = max(min(locate(SAD_LUT%Grid%Re (ii,1:SAD_LUT%Grid%nRe (ii,j),j),Re ), &
                                     SAD_LUT%Grid%nRe (ii,j)-1),1)

            if (SAD_LUT%table_uses_satzen(j)) then
               GZero%iSaZ0(i,j) = &
                  max(min(locate(SAD_LUT%Grid%SatZen(ii,1:SAD_LUT%Grid%nSatzen(ii,j),j),&
                          SPixel%Geom%Satzen(SPixel%ViewIdx(i))),SAD_LUT%Grid%nSatzen(ii,j)-1),1)
            end if
            if (SAD_LUT%table_uses_solzen(j)) then
               GZero%iSoZ0(i,j) = &
                  max(min(locate(SAD_LUT%Grid%SolZen(ii,1:SAD_LUT%Grid%nSolzen(ii,j),j),&
                          SPixel%Geom%Solzen(SPixel%ViewIdx(i))),SAD_LUT%Grid%nSolzen(ii,j)-1),1)

               GZero%iSaZSoZ0(i,j) = &
                  max(min(locate(SAD_LUT%Grid%SolZen(ii,1:SAD_LUT%Grid%nSolzen(ii,j),j),&
                          SPixel%Geom%Satzen(SPixel%ViewIdx(i))),SAD_LUT%Grid%nSolzen(ii,j)-1),1)
            end if
            if (SAD_LUT%table_uses_relazi(j)) then
               GZero%iRA0(i,j) = &
                  max(min(locate(SAD_LUT%Grid%Relazi(ii,1:SAD_LUT%Grid%nRelazi(ii,j),j),&
                          SPixel%Geom%Relazi(SPixel%ViewIdx(i))),SAD_LUT%Grid%nRelazi(ii,j)-1),1)
            end if
         end if
      end do

      ! This sets the upper bracketing index, the locate above set the lower
      ! index
      do i=1,SPixel%Ind%Ny
         GZero%iT1(i,j)      = GZero%iT0(i,j)   + 1
         GZero%iR1(i,j)      = GZero%iR0(i,j)   + 1

         GZero%iSaZ1(i,j)    = GZero%iSaZ0(i,j) + 1
         GZero%iSoZ1(i,j)    = GZero%iSoZ0(i,j) + 1
         GZero%iRA1(i,j)     = GZero%iRA0(i,j)  + 1

         GZero%iSaZSoZ1(i,j) = GZero%iSaZSoZ0(i,j) + 1
      end do

     ! This sets the next pair of bracketing indices around the primary one
      do i=1,SPixel%Ind%Ny
         ii = SPixel%spixel_y_to_ctrl_y_index(i)
         if (GZero%iT0(i,j) == 1) then
            GZero%iTm1(i,j) = GZero%iT0(i,j)
            GZero%iTp1(i,j) = GZero%iT1(i,j)+1
         else if (GZero%iT1(i,j) == SAD_LUT%Grid%nTau(ii,j)) then
            GZero%iTm1(i,j) = GZero%iT0(i,j)-1
            GZero%iTp1(i,j) = GZero%iT1(i,j)
         else
            GZero%iTm1(i,j) = GZero%iT0(i,j)-1
            GZero%iTp1(i,j) = GZero%iT1(i,j)+1
         end if

         if (GZero%iR0(i,j) == 1) then
            GZero%iRm1(i,j) = GZero%iR0(i,j)
            GZero%iRp1(i,j) = GZero%iR1(i,j)+1
         else if (GZero%iR1(i,j) == SAD_LUT%Grid%nRe(ii,j)) then
            GZero%iRm1(i,j) = GZero%iR0(i,j)-1
            GZero%iRp1(i,j) = GZero%iR1(i,j)
         else
            GZero%iRm1(i,j) = GZero%iR0(i,j)-1
            GZero%iRp1(i,j) = GZero%iR1(i,j)+1
         end if
      end do

      ! Calculate dT, dR: these are the distances in T, R, etc from the grid
      ! point with indices (0, 0, ...) to (Tau, Re, ...) expressed as a
      ! fraction of the LUT grid steps.

      ! These variables are not used and could be removed? No, they are used!
      do i=1,SPixel%Ind%Ny
         ii = SPixel%spixel_y_to_ctrl_y_index(i)

         if (SAD_LUT%table_used_for_channel(ii, j)) then
            GZero%dT(i,j) = (Tau - SAD_LUT%Grid%Tau(ii,GZero%iT0(i,j),j)) / &
               (SAD_LUT%Grid%Tau(ii,GZero%iT1(i,j),j) - SAD_LUT%Grid%Tau(ii,GZero%iT0(i,j),j))

            GZero%dR(i,j) = (Re - SAD_LUT%Grid%Re(ii,GZero%iR0(i,j),j)) / &
               (SAD_LUT%Grid%Re(ii,GZero%iR1(i,j),j) - SAD_LUT%Grid%Re(ii,GZero%iR0(i,j),j))

            if (SAD_LUT%table_uses_satzen(j)) then
               GZero%dSaZ(i,j) = &
                  (SPixel%Geom%SatZen(SPixel%ViewIdx(i)) - SAD_LUT%Grid%SatZen(ii,GZero%iSaZ0(i,j),j)) / &
                  (SAD_LUT%Grid%SatZen(ii,GZero%iSaZ1(i,j),j) - SAD_LUT%Grid%SatZen(ii,GZero%iSaZ0(i,j),j))
            end if

            ! The other two angles only exist when we have solar channels
            ! (including mixed)
            if (SAD_LUT%table_uses_solzen(j)) then
               GZero%dSoZ(i,j) = &
                  (SPixel%Geom%SolZen(SPixel%ViewIdx(i)) - SAD_LUT%Grid%SolZen(ii,GZero%iSoZ0(i,j),j)) / &
                  (SAD_LUT%Grid%SolZen(ii,GZero%iSoZ1(i,j),j) - SAD_LUT%Grid%SolZen(ii,GZero%iSoZ0(i,j),j))

               GZero%dSaZSoZ(i,j) = &
                  (SPixel%Geom%SatZen(SPixel%ViewIdx(i)) - SAD_LUT%Grid%SolZen(ii,GZero%iSaZSoZ0(i,j),j)) / &
                  (SAD_LUT%Grid%SolZen(ii,GZero%iSaZSoZ1(i,j),j) - SAD_LUT%Grid%SolZen(ii,GZero%iSaZSoZ0(i,j),j))
            end if
            if (SAD_LUT%table_uses_relazi(j)) then
               GZero%dRA(i,j) = &
                  (SPixel%Geom%RelAzi(SPixel%ViewIdx(i)) - SAD_LUT%Grid%RelAzi(ii,GZero%iRA0(i,j),j)) / &
                  (SAD_LUT%Grid%RelAzi(ii,GZero%iRA1(i,j),j) - SAD_LUT%Grid%RelAzi(ii,GZero%iRA0(i,j),j))
            end if
         end if
      end do

      ! Calculate 1.0 minus each of the d values above - used several times by
      ! the interpolation routines.

      do i=1,SPixel%Ind%Ny
         GZero%T1(i,j)    = 1.0 - GZero%dT(i,j)
         GZero%R1(i,j)    = 1.0 - GZero%dR(i,j)
         GZero%Sa1(i,j)   = 1.0 - GZero%dSaZ(i,j)
         GZero%So1(i,j)   = 1.0 - GZero%dSoZ(i,j)
         GZero%Ra1(i,j)   = 1.0 - GZero%dRA(i,j)
         GZero%SaSo1(i,j) = 1.0 - GZero%dSaZSoZ(i,j)
      end do

#ifdef DEBUG
      do i=1,SPixel%Ind%Ny
         ii = SPixel%spixel_y_to_ctrl_y_index(i)
         if (SAD_LUT%table_used_for_channel(ii, j)) then
            write(*,*) 'channel = ',i, ', i_table_type = ', j
            write(*,*)'SetGZero: CurT, TGrid(1), delT:', Tau, SAD_LUT%Grid%Tau(ii,GZero%iT0(i,j),j), &
               SAD_LUT%Grid%Tau(ii,GZero%iT1(i,j),j)
            write(*,*)'SetGZero: CurR, RGrid(1), delR:', Re, SAD_LUT%Grid%Re(ii,GZero%iR0(i,j),j), &
               SAD_LUT%Grid%Re(ii,GZero%iR1(i,j),j)
            if (SAD_LUT%table_uses_satzen(j)) then
               write(*,*)'SetGZero: CurSa(1), SaGrid(1), delSa:', SPixel%Geom%SatZen(SPixel%ViewIdx(1)), &
                  SAD_LUT%Grid%Satzen(ii,GZero%iSaZ0(i,j),j),SAD_LUT%Grid%Satzen(ii,GZero%iSaZ1(i,j),j)
            end if
            if (SAD_LUT%table_uses_solzen(j)) then
               write(*,*)'SetGZero: CurSo(1), SoGrid(1), delSo:', SPixel%Geom%SolZen(SPixel%ViewIdx(1)), &
                  SAD_LUT%Grid%Solzen(ii,GZero%iSoZ0(i,j),j),SAD_LUT%Grid%Solzen(ii,GZero%iSoZ1(i,j),j)
            end if
            if (SAD_LUT%table_uses_relazi(j)) then
               write(*,*)'SetGZero: CurRa(1), RaGrid(1), delRa:', SPixel%Geom%RelAzi(SPixel%ViewIdx(1)), &
                  SAD_LUT%Grid%Relazi(ii,GZero%iRA0(i,j),j),      SAD_LUT%Grid%Relazi(ii,GZero%iRA1(i,j),j)
            end if
            write(*,'(a, 5i3.1)')' indices of T0, R0, Saz0(1), SoZ0(1), Ra0(1)', &
               GZero%iT0(i,j), GZero%iR0(i,j), GZero%iSaZ0(i,j), GZero%iSoZ0(i,j), GZero%iRA0(i,j)
            write(*,'(a, 5i3.1)')' indices of T1, R1, Saz1(1), SoZ1(1), Ra1(1): ', &
               GZero%iT1(i,j), GZero%iR1(i,j), GZero%iSaZ1(i,j), GZero%iSoZ1(i,j), GZero%iRA1(i,j)
         end if
      end do
#endif
   end do

end subroutine Set_GZero
