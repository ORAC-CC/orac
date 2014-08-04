!-------------------------------------------------------------------------------
! Name:
!    Write_Diag
!
! Description:
!    Writes out the requested parts of the diagnostic structure from a
!    particular ECP inversion.
!
! Arguments:
!    Name     Type   In/Out/Both Description
!    Ctrl     struct In          ECP control structure.
!    SPixel   struct In          Super-pixel structure. Required for nos. of
!                                active measurement channels and state variables.
!    Diag     struct In          The diagnostic structure.
!    diag_lun int    In          Logical unit number for the diagnostic output
!                                file. This file is left open for the duration
!                                of image processing.
!    status   int    Out         Status/error value.
!
! Algorithm:
!    File pointed to by diag_lun should already be open for writing as
!    unformatted.
!    Write out SPixel values for number of active channels and state variables
!    Check each of the diagnostic flags in array Ctrl%Diagl in turn and output
!    the relevant data if the flag is set, as follows:
!    if (Qc requested) write QC flag
!    if ((Iterations requested) write Iteration count
!    if (phase changes requested) write Phase change count
!    if (costs requested) write measurement and a priori costs
!    if (St 1 requested) write square root of the diagonals of error covariance
!        matrix St (errors from 'null space' or a priori sources
!    if (Ss 1 requested) write square root of the diagonals of error covariance
!        matrix Ss from 'model parameter' noise
!    if (St 2 requested) write full error covariance matrix St
!    if (Ss 2 requested) write full error covariance matrix Ss
!    if (Measurement fit requested) write measurement fit array
!    if (A priori fit requested) write a priori fit array
!    if (A priori value requested) write SPixel a priori array
!    if (First guess value requested) write SPixel first guess array
!    if (A priori error requested) write SPixel a priori error array
!    if (Measurement error requested) write SPixel first guess error array
!
! Local variables:
!    Name Type Description
!
! History:
!     9th July 2001, Andy Smith: Original version
!    17th July 2001, Andy Smith:
!       Bug fix: St and Ss arrays now o/p as square root when 1st St/Ss option
!       selected. These arrays also required de-scaling.
!     8th June 2011, Caroline Poulsen:
!       Remove variables from diag output.
!    18th June 2012, Caroline Poulsen:
!       Changed illum definition.
!
! Bugs:
!    None known.
!
! $Id$
!
!-------------------------------------------------------------------------------

subroutine Write_Diag(Ctrl, SPixel, Diag, diag_lun, status)

   use Ctrl_def
   use Diag_def
   use ECP_Constants
   use SPixel_def

   implicit none

   ! Argument declarations

   type(Ctrl_t),   intent(in)  :: Ctrl
   type(SPixel_t), intent(in)  :: SPixel
   type(Diag_t),   intent(in)  :: Diag
   integer,        intent(in)  :: diag_lun
   integer,        intent(out) :: status

   ! Local variables

   integer :: m   ! Loop counter
   integer :: ios ! I/O status value from file operations


   ! Write super-pixel specific values that determine arrays sizes with Diag
   ! (no. of active state variables and channels). Also write out the active
   ! state variable indices and channel IDs.

   write(unit=diag_lun, iostat=ios, err=999) SPixel%NX
   write(unit=diag_lun, iostat=ios, err=999) SPixel%Ind%Ny
   write(unit=diag_lun, iostat=ios, err=999) SPixel%X
   select case (SPixel%Illum(1))
   case (IDay)
      write(unit=diag_lun, iostat=ios, err=999) &
         Ctrl%Ind%Y_ID(SPixel%Ind%SolarFirst:SPixel%Ind%ThermalLast)
   case (ITwi, INight)
      write(unit=diag_lun, iostat=ios, err=999) &
         Ctrl%Ind%Y_ID(SPixel%Ind%ThermalFirst:SPixel%Ind%ThermalLast)
   end select

   ! Write super-pixel quality control flag

   write(unit=diag_lun, iostat=ios, err=999) SPixel%QC

   ! Check diagnostic flags and write out required values.

!   if (Ctrl%Diagl(DiFlagQC) > 0) &
!      write(unit=diag_lun, iostat=ios, err=999) Diag%QCFlag

!   if (Ctrl%Diagl(DiFlagIter) > 0) &
!      write(unit=diag_lun, iostat=ios, err=999) Diag%Iterations

!   if (Ctrl%Diagl(DiFlagPhCh) > 0) &
!      write(unit=diag_lun, iostat=ios, err=999) Diag%PhaseChanges

!   if (Ctrl%Diagl(DiFlagCost) > 0) &
!      write(unit=diag_lun, iostat=ios, err=999) Diag%Jm, Diag%Ja

   ! State expected errors from null space: level 1, square roots of diagonals
   ! Values set only for active state variables.

   if (Ctrl%Diagl(DiFlagSt1) > 0) &
      write(unit=diag_lun, iostat=ios, err=999) &
      (sqrt(Diag%St(m,m)) / Ctrl%Invpar%XScale(SPixel%X(m)), m = 1,SPixel%NX)

   ! State expected errors from model parameters: level 1, square roots of
   ! diagonals. Values set only for active state variables.

   if (Ctrl%Diagl(DiFlagSs1) > 0) &
      write(unit=diag_lun, iostat=ios, err=999) &
      (sqrt(Diag%Ss(m,m)) / Ctrl%Invpar%XScale(SPixel%X(m)), m = 1,SPixel%NX)

   ! State expected errors from null space: level 2, full matrix.
   ! Values set only for active state variables.

   if (Ctrl%Diagl(DiFlagSt2) > 0) then
      write(unit=diag_lun, iostat=ios, err=999) &
         (Diag%St(m,1:SPixel%NX) / &
	 (Ctrl%Invpar%XScale(SPixel%X(m)) * Ctrl%Invpar%XScale(SPixel%X)), &
	 m = 1,SPixel%NX)
   end if

   ! State expected errors from model parameters: level 2, full matrix.
   ! Values set only for active state variables.

   if (Ctrl%Diagl(DiFlagSs2) > 0) then
      write(unit=diag_lun, iostat=ios, err=999) &
         (Diag%Ss(m,1:SPixel%NX) / &
	 (Ctrl%Invpar%XScale(SPixel%X(m)) * Ctrl%Invpar%XScale(SPixel%X)), &
	 m = 1,SPixel%NX)
   end if

   ! Measurement and a priori fit

   if (Ctrl%Diagl(DiFlagYFit) > 0) &
      write(unit=diag_lun, iostat=ios, err=999) Diag%YmFit(1:SPixel%Ind%Ny)

   if (Ctrl%Diagl(DiFlagXFit) > 0) &
      write(unit=diag_lun, iostat=ios, err=999) Diag%APFit(1:SPixel%Nx)

   ! A priori and first guess values.

!   if (Ctrl%Diagl(DiFlagAP) > 0) &
!      write(unit=diag_lun, iostat=ios, err=999) SPixel%Xb

!   if (Ctrl%Diagl(DiFlagFG) > 0) &
!      write(unit=diag_lun, iostat=ios, err=999) SPixel%X0

   ! A priori and measurement error covariances (square roots of diagonals)
   ! YError is already set to the square root values.

   if (Ctrl%Diagl(DiFlagSx) > 0) &
      write(unit=diag_lun, iostat=ios, err=999) &
         (sqrt(SPixel%Sx(m,m)), m=1,MaxStateVar)

   if (Ctrl%Diagl(DiFlagSy) > 0) &
      write(unit=diag_lun, iostat=ios, err=999) Diag%YError(1:SPixel%Ind%Ny)

   !  Error handling

999 if (ios /= 0) then
      status = DiagFileWriteErr
      call Write_Log(Ctrl, 'Error writing to diagnostic file',status)
   end if

end subroutine Write_Diag
