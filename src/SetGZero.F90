! Name:
!    Set_GZero
!
! Purpose:
!    Sets up the "zero'th point" grid information used in interpolating
!    the SAD_LUT CRP arrays.
!
! Arguments:
!    Name       Type    In/Out/Both    Description
!    Tau        real    In           Current value of optical depth
!    Re         real    In           Current value of effective radius
!    Geom       struct  In           Sub-structure of SPixel, contains the
!                                    sat zen, sun zen and rel azi values.
!    SAD_LUT    struct  In           Static Application Data structure 
!                                    containing the arrays of Look-Up Tables
!                                    to be interpolated and the grids on which
!                                    they are stored.
!    GZero      Struct  Out          Holds "0'th point" information relating to
!                                    the grid on which the SAD_LUT CRP arrays
!                                    are based.
!    status     int     Out          Standard status code set by ECP routines
!
!
! Algorithm:
!    Use the grid info in SAD_LUT and the current Tau, Re and Geom values
!    to find the "zero'th" point for interpolation (see IntLUTTauRe comments).
!    Populate GZero struct with zero'th point data, i.e.
!     - index of closest (lower) point in LUT data grid, in Tau, Re, SatZen etc
!       (i.e. the zero'th point) - ensure that 0'th point index is between 1
!       and npoints-1 
!     - index of next-nearest points (set to nearest point if we're at the 
!       edge of the LUT).
!     - Denominator for calculating finite difference gradients. Will either
!       be equal to LUT grid spacing (at edges if LUT), or twice this.
!     - fractional grid step from 0'th point to supplied Tau, Re, Sat zen etc
!     - 1 minus fractional step from above
!
!
! Local variables:
!    Name       Type    Description
!
!
! History:
!    23rd Jan 2001, Andy Smith : original version     
!    31st May 2007, Andy Sayer:
!     Consolidation of code for dual-view BRDF retrieval. This means added
!     calculation of new geometry variables for second view (copied existing
!     code, adding "_2" suffix).
!    20th March 2008, Gareth Thomas:
!     Changes for spline interpolation: Added mT0, mT1, mR0, mR1
!     calculations - gradients at iT0, iT1, iR0 and iR1 LUT points.
!    5th Sep 2011, Chris Arnold:
!     'locate' routine is now used for domain searching
!    5th Sep 2011, Chris Arnold:
!     next nearest neighbour indices iTm1,iTp1,iRm1,iRp1 
!     now evaluated
!    13th December Caroline Poulsen added in allocation of isaz0 parameter
!    15th December Caroline Poulsen remove deallocation test from this
!       routine and instead deallocate at the end of FM.F90 this solved g95
!2013 MJ changes loop boundaries and parameter list contents
!       memory problem
!20131203 MJ makes LUTs more flexible wrt channel and properties
! Bugs:
!    None known.
!
!---------------------------------------------------------------------

Subroutine Set_GZero (Tau, Re, Ctrl,Spixel, SAD_LUT, GZero, status)
!MJ ORGSubroutine Set_GZero (Tau, Re, Geom, NChans, ViewIdx, SAD_LUT, GZero, status)
  use Ctrl_def
   use SPixel_def
   use SAD_LUT_def
   use GZero_def
   use Int_Routines_def

   implicit none

!  argument declarations 
   real, intent(in)                      :: Tau, Re
   type(CTRL_t), intent(in)        :: Ctrl
   type(SPixel_t)   :: SPixel
   !MJORGtype(SPixel_Geom_t), intent(in)       :: Geom
   !integer, intent(in)                   :: NChans
   !integer, intent(in)                   :: ViewIdx(NChans)
   type(SAD_LUT_t), intent(inout)        :: SAD_LUT
   type(GZero_t), intent(out)            :: GZero  ! Struct containing 
                                                   ! "zero'th" grid points
   integer, intent(out)                  :: status

   integer :: i,j ! Loop counters
   integer :: k

!  Status is not actually set at present. Error-checking here could be very
!  costly in terms of CPU. Leave status argument in case of future
!  updates. Set to 0 to avoid compiler warnings.

   status = 0

!  Allocate the arrays dependent on viewing geometry

   !write(*,*) ViewIdx,'ViewIdx',shape(SPixel%Geom%Satzen),NChans
   !pause

!!$   allocate(GZero%iSaZ0(NChans))        
!!$   allocate(GZero%iSoZ0(NChans))
!!$   allocate(GZero%iRA0(NChans))
!!$   allocate(GZero%iSaZ1(NChans))
!!$   allocate(GZero%iSoZ1(NChans))
!!$   allocate(GZero%iRA1(NChans))
!!$   allocate(GZero%dSaZ(NChans))
!!$   allocate(GZero%dSoZ(NChans))
!!$   allocate(GZero%dRA(NChans))
!!$   allocate(GZero%Sa1(NChans))
!!$   allocate(GZero%So1(NChans))
!!$   allocate(GZero%Ra1(NChans))

!  Set the "zero'th" array indices for the interpolations, i.e. find the 
!  array indices of the nearest neighbour grid points in each dimension. 
!  Use the 'locate' function
!MJ ORG:
!!$   GZero%iT0   = max(min(locate(SAD_LUT%Grid%Tau(1:SAD_LUT%Grid%nTau),Tau),SAD_LUT%Grid%nTau-1),1)
!!$   GZero%iR0   =   max(min(locate(SAD_LUT%Grid%Re(1:SAD_LUT%Grid%nRe),Re),SAD_LUT%Grid%nRe-1),1)

   do j=1,maxcrprops
      do i=1,Spixel%Ind%Ny
         GZero%iT0(i,j)   = max(min(locate(SAD_LUT%Grid%Tau(i,1:SAD_LUT%Grid%nTau(i,j),j),Tau),SAD_LUT%Grid%nTau(i,j)-1),1)
         write(*,*) 'test gz',i,j,GZero%iT0(i,j),tau,SAD_LUT%Grid%nTau(i,j),SAD_LUT%Grid%Tau(i,GZero%iT0(i,j),j)
         write(*,*) 'lut tau',SAD_LUT%Grid%Tau(i,:,j)
         GZero%iR0(i,j)   =   max(min(locate(SAD_LUT%Grid%Re(i,1:SAD_LUT%Grid%nRe(i,j),j),Re),SAD_LUT%Grid%nRe(i,j)-1),1)
      enddo



   write(*,*) 'hewre ViewIdx',Spixel%ViewIdx
!   write(*,*) 'tests',
!   stop

   do i=1,Spixel%Ind%Ny

      !write(*,*) 'subs:',i,Spixel%Ind%Ny,SAD_LUT%Grid%nSatzen,SPixel%ViewIdx(i),SPixel%Geom%Satzen(SPixel%ViewIdx(i)),&
	!& SAD_LUT%Grid%nSatzen-1,SAD_LUT%Grid%nSolzen-1,SAD_LUT%Grid%nSolzen-1,SAD_LUT%Grid%nRelazi-1,&
	!& SPixel%Geom%Solzen(Spixel%ViewIdx(i)),SPixel%Geom%Relazi(Spixel%ViewIdx(i))

      !MJ ORG
!!$      GZero%iSaZ0(i) = &
!!$           & max(min(locate(SAD_LUT%Grid%SatZen(1:SAD_LUT%Grid%nSatzen),&
!!$           & SPixel%Geom%Satzen(Spixel%ViewIdx(i))),SAD_LUT%Grid%nSatzen-1),1)
!!$      GZero%iSoZ0(i) = &
!!$           & max(min(locate(SAD_LUT%Grid%SolZen(1:SAD_LUT%Grid%nSolzen),&
!!$           & SPixel%Geom%Solzen(Spixel%ViewIdx(i))),SAD_LUT%Grid%nSolzen-1),1)
!!$      GZero%iRA0(i)  = &
!!$           & max(min(locate(SAD_LUT%Grid%Relazi(1:SAD_LUT%Grid%nRelazi),&
!!$           & SPixel%Geom%Relazi(Spixel%ViewIdx(i))),SAD_LUT%Grid%nRelazi-1),1)

      write(*,*) 'ichan zero',i
      GZero%iSaZ0(i,j) = &
           & max(min(locate(SAD_LUT%Grid%SatZen(i,1:SAD_LUT%Grid%nSatzen(i,j),j),&
           & SPixel%Geom%Satzen(Spixel%ViewIdx(i))),SAD_LUT%Grid%nSatzen(i,j)-1),1)
      GZero%iSoZ0(i,j) = &
           & max(min(locate(SAD_LUT%Grid%SolZen(i,1:SAD_LUT%Grid%nSolzen(i,j),j),&
           & SPixel%Geom%Solzen(Spixel%ViewIdx(i))),SAD_LUT%Grid%nSolzen(i,j)-1),1)
      GZero%iRA0(i,j)  = &
           & max(min(locate(SAD_LUT%Grid%Relazi(i,1:SAD_LUT%Grid%nRelazi(i,j),j),&
           & SPixel%Geom%Relazi(Spixel%ViewIdx(i))),SAD_LUT%Grid%nRelazi(i,j)-1),1)

   end do

! The checks below should now be obsolote...?

   !this sets the upper bracketing index, the locate above set the lower index
!MJ ORG
!!$   GZero%iT1   = GZero%iT0 + 1
!!$   GZero%iR1   = GZero%iR0 + 1
   do i=1,Spixel%Ind%Ny
      GZero%iSaZ1(i,j) = GZero%iSaZ0(i,j) + 1
      GZero%iSoZ1(i,j) = GZero%iSoZ0(i,j) + 1
      GZero%iRA1(i,j)  = GZero%iRA0(i,j) + 1

      GZero%iT1(i,j)   = GZero%iT0(i,j) + 1
      GZero%iR1(i,j)   = GZero%iR0(i,j) + 1
   
   end do

   !this sets the next pair of bracketing indices around the primary one
   do i=1,Spixel%Ind%Ny
      if (GZero%iT0(i,j) == 1) then
         GZero%iTm1(i,j) = GZero%iT0(i,j)
         GZero%iTp1(i,j) = GZero%iT1(i,j)+1
      else if (GZero%iT1(i,j) == SAD_LUT%Grid%nTau(i,j)) then
         GZero%iTm1(i,j) = GZero%iT0(i,j)-1
         GZero%iTp1(i,j) = GZero%iT1(i,j)
      else
         GZero%iTm1(i,j) = GZero%iT0(i,j)-1
         GZero%iTp1(i,j) = GZero%iT1(i,j)+1
      endif

      if (GZero%iR0(i,j) == 1) then
         GZero%iRm1(i,j) = GZero%iR0(i,j)
         GZero%iRp1(i,j) = GZero%iR1(i,j)+1
      else if (GZero%iR1(i,j) == SAD_LUT%Grid%nRe(i,j)) then
         GZero%iRm1(i,j) = GZero%iR0(i,j)-1
         GZero%iRp1(i,j) = GZero%iR1(i,j)
      else
         GZero%iRm1(i,j) = GZero%iR0(i,j)-1
         GZero%iRp1(i,j) = GZero%iR1(i,j)+1
      endif

   enddo

!  Calcuate dT, dR: these are the distances in T, R, etc from the grid 
!  point with indices (0, 0, ...) to (Tau, Re, ...) expressed as a fraction 
!  of the LUT grid steps.

   ! These variables are not used and could be removed? No, they are used!
   do i=1,Spixel%Ind%Ny
      write(*,*) 'compute dt',i,j,tau, SAD_LUT%Grid%Tau(i,GZero%iT0(i,j),j),&
           & SAD_LUT%Grid%Tau(i,GZero%iT1(i,j),j), SAD_LUT%Grid%tau(i,GZero%iT0(i,j),j)
      if(abs(SAD_LUT%Grid%Tau(i,GZero%iT1(i,j),j)-SAD_LUT%Grid%Tau(i,GZero%iT0(i,j),j)) .le. &
           & ditherm6) then
         GZero%dT(i,j)   = 0.0
      else
         GZero%dT(i,j)   = (Tau - SAD_LUT%Grid%Tau(i,GZero%iT0(i,j),j)) / &
              & (SAD_LUT%Grid%Tau(i,GZero%iT1(i,j),j) - SAD_LUT%Grid%Tau(i,GZero%iT0(i,j),j))
      endif
      if(abs(SAD_LUT%Grid%Re(i,GZero%iR1(i,j),j) - SAD_LUT%Grid%Re(i,GZero%iR0(i,j),j)) .le.&
           & ditherm6) then
         GZero%dT(i,j)   = 0.0
      else 
         GZero%dR(i,j)   = (Re - SAD_LUT%Grid%Re(i,GZero%iR0(i,j),j)) / &
              & (SAD_LUT%Grid%Re(i,GZero%iR1(i,j),j) - SAD_LUT%Grid%Re(i,GZero%iR0(i,j),j))
      endif
      write(*,*) 'compute dr',Re, SAD_LUT%Grid%Re(i,GZero%iR0(i,j),j),&
           & SAD_LUT%Grid%Re(i,GZero%iR1(i,j),j), SAD_LUT%Grid%Re(i,GZero%iR0(i,j),j)
   enddo
   

   do i=1,Spixel%Ind%Ny
      !write(*,*) 'angle loop',i,SAD_LUT%Grid%SatZen(i,GZero%iSaZ1(i,j),j), SAD_LUT%Grid%SatZen(i,GZero%iSaZ0(i,j),j)
      if(abs(SAD_LUT%Grid%SatZen(i,GZero%iSaZ1(i,j),j) - SAD_LUT%Grid%SatZen(i,GZero%iSaZ0(i,j),j)) .le. &
           & ditherm6) then
         GZero%dSaZ(i,j) = 0.0
      else
         GZero%dSaZ(i,j) = &
              & (SPixel%Geom%SatZen(Spixel%ViewIdx(i)) - SAD_LUT%Grid%SatZen(i,GZero%iSaZ0(i,j),j)) / &
              & (SAD_LUT%Grid%SatZen(i,GZero%iSaZ1(i,j),j) - SAD_LUT%Grid%SatZen(i,GZero%iSaZ0(i,j),j)) 
      endif
      !the other two angles only exist when we have solar channels (including mixed)
      !if( i .le. Ctrl%Ind%SolarLast) then
         if(abs(SAD_LUT%Grid%SolZen(i,GZero%iSoZ1(i,j),j) - SAD_LUT%Grid%SolZen(i,GZero%iSoZ0(i,j),j)) .le. &
              & ditherm6 ) then
            GZero%dSoZ(i,j)=0.0
         else
            GZero%dSoZ(i,j) = &
                 & (SPixel%Geom%SolZen(Spixel%ViewIdx(i)) - SAD_LUT%Grid%SolZen(i,GZero%iSoZ0(i,j),j)) / &
                 & (SAD_LUT%Grid%SolZen(i,GZero%iSoZ1(i,j),j) - SAD_LUT%Grid%SolZen(i,GZero%iSoZ0(i,j),j)) 
         endif
         if(abs(SAD_LUT%Grid%RelAzi(i,GZero%iRA1(i,j),j)  - SAD_LUT%Grid%RelAzi(i,GZero%iRA0(i,j),j)) .le. &
              & ditherm6) then
            GZero%dRA(i,j)=0.0
         else            
            GZero%dRA(i,j)  = &
                 & (SPixel%Geom%RelAzi(Spixel%ViewIdx(i)) - SAD_LUT%Grid%RelAzi(i,GZero%iRA0(i,j),j))  / &
                 & (SAD_LUT%Grid%RelAzi(i,GZero%iRA1(i,j),j)  - SAD_LUT%Grid%RelAzi(i,GZero%iRA0(i,j),j)) 
         endif
      
   end do

!  Caclulate 1.0 minus each of the d values above - used several times by
!  the interpolation routines.

   do i=1,Spixel%Ind%Ny
      GZero%T1(i,j)  = 1.0 - GZero%dT(i,j)
      GZero%R1(i,j)  = 1.0 - GZero%dR(i,j)
      GZero%Sa1(i,j) = 1.0 - GZero%dSaZ(i,j)
      GZero%So1(i,j) = 1.0 - GZero%dSoZ(i,j)
      GZero%Ra1(i,j) = 1.0 - GZero%dRA(i,j)
   enddo

#ifdef DEBUG
   do i=1,Spixel%Ind%Ny
      write(*,*) 'channel',i,j
      write(*,*)'SetGZero: CurT, TGrid(1), delT:', Tau, SAD_LUT%Grid%Tau(i,GZero%iT0(i,j),j), &
           & SAD_LUT%Grid%Tau(i,GZero%iT1(i,j),j)
      write(*,*)'SetGZero: CurR, RGrid(1), delR:', Re, SAD_LUT%Grid%Re(i,GZero%iR0(i,j),j), &
           & SAD_LUT%Grid%Re(i,GZero%iR1(i,j),j)
      write(*,*)'SetGZero: CurSa(1), SaGrid(1), delSa:', SPixel%Geom%SatZen(Spixel%ViewIdx(1)), &
           & SAD_LUT%Grid%Satzen(i,GZero%iSaZ0(i,j),j),SAD_LUT%Grid%Satzen(i,GZero%iSaZ1(i,j),j)
      write(*,*)'SetGZero: CurSo(1), SoGrid(1), delSo:', SPixel%Geom%SolZen(Spixel%ViewIdx(1)), &
           & SAD_LUT%Grid%Solzen(i,GZero%iSoZ0(i,j),j),SAD_LUT%Grid%Solzen(i,GZero%iSoZ1(i,j),j)
      write(*,*)'SetGZero: CurRa(1), RaGrid(1), delRa:', SPixel%Geom%RelAzi(Spixel%ViewIdx(1)), &
           & SAD_LUT%Grid%Relazi(i,GZero%iRA0(i,j),j),      SAD_LUT%Grid%Relazi(i,GZero%iRA1(i,j),j)
      
      write(*,'(a, 5i3.1)')' indices of T0, R0, SoZ0(1), Saz0(1), Ra0(1)', &
           & GZero%iT0(i,j), GZero%iR0(i,j), GZero%iSoZ0(i,j), GZero%iSaZ0(i,j), GZero%iRA0(i,j)
      write(*,'(a, 5i3.1)')' indices of T1, R1, SoZ1(1), Saz1(1), Ra1(1): ', &
           & GZero%iT1(i,j), GZero%iR1(i,j), GZero%iSoZ1(i,j), GZero%iSaZ1(i,j), GZero%iRA1(i,j)
   enddo

!MJ OLD:
!!$   write(*,*)'SetGZero: CurT, TGrid(1), delT:', Tau, SAD_LUT%Grid%Tau(1), &
!!$      SAD_LUT%Grid%dTau
!!$   write(*,*)'SetGZero: CurR, RGrid(1), delR:', Re, SAD_LUT%Grid%Re(1), &
!!$      SAD_LUT%Grid%dRe
!!$   write(*,*)'SetGZero: CurSa(1), SaGrid(1), delSa:', SPixel%Geom%SatZen(Spixel%ViewIdx(1)), &
!!$      SAD_LUT%Grid%Satzen(1), SAD_LUT%Grid%dSatzen
!!$   write(*,*)'SetGZero: CurSo(1), SoGrid(1), delSo:', SPixel%Geom%SolZen(Spixel%ViewIdx(1)), &
!!$      SAD_LUT%Grid%Solzen(1), SAD_LUT%Grid%dSolzen
!!$   write(*,*)'SetGZero: CurRa(1), RaGrid(1), delRa:', SPixel%Geom%RelAzi(Spixel%ViewIdx(1)), &
!!$      SAD_LUT%Grid%Relazi(1), SAD_LUT%Grid%dRelAzi
!!$
!!$   write(*,'(a, 5i3.1)')' indices of T0, R0, SoZ0(1), Saz0(1), Ra0(1)', &
!!$      GZero%iT0, GZero%iR0, GZero%iSoZ0(1), GZero%iSaZ0(1), GZero%iRA0(1)
!!$   write(*,'(a, 5i3.1)')' indices of T1, R1, SoZ1(1), Saz1(1), Ra1(1): ', &
!!$      GZero%iT1, GZero%iR1, GZero%iSoZ1(1), GZero%iSaZ1(1), GZero%iRA1(1)
#endif

enddo

 End Subroutine Set_GZero
