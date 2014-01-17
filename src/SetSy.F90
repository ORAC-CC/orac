! Name:
!    Set_Sy
!
! Description:
!    Sets up the state dependent part of the error covariance matrix Sy 
!    (errors in the measurements Y). SPixel%Sy is already populated with 
!    instrument dependent data and pixel dependent data.
!    This routine adds the state-dependent terms. Currently only Rs is
!    supported. Spixel%Sy is not overwritten as the current state vector 
!    might not be kept (if the step takes the inversion away from convergence).
!
! Arguments:
!    Name       Type    In/Out/Both  Description
!    SPixel     struct  In           Info on the current super-pixel
!    Kbj        array   In           Gradients in measurements calculated by the
!                                    forward model w.r.t. model parameters (only
!                                    Rs at present).
!    Sy         array   Out          The local (to Invert_Marquardt) error 
!                                    covariance in the measurements. Set to the
!                                    sum of the scene Sy (from Spixel) and the
!                                    requested model parameter values (Rs).
!    status     int     Out          Standard ECP error code (not set at
!                                    present, no error conditions identified).
!
! Algorithm:
!    if Ctrl flag indicates that Rs errors should be included in Sy:
!     - calculate Syb = Kbj * Sb * transpose(Kbj)
!     - Sy = scene Sy + Syb 
!
! Local variables:
!    Name    Type        Description
!    Syb     real array  Sb mapped into measurement errors
!    i, j    int         Counters
!
! History:
!    16th May 2001, Andy Smith: original version
!     6th June 2001, Andy Smith: 
!       Implicit none statement was wrongly placed. Ctrl argument added.
!     5th July 2001, Andy Smith: 
!       Added test for NSolar > 0 before adding Kbj terms.
!
! Bugs:
!    None known.
!
!---------------------------------------------------------------------

Subroutine Set_Sy(Ctrl, SPixel, Kbj, Sy, status)

   use ECP_Constants
   use Ctrl_def
   use SPixel_def
   
   Implicit none
   
!  Declare arguments

   type(Ctrl_t), intent(in)    :: Ctrl
   type(SPixel_t), intent(in)  :: SPixel
   real, intent(in)            :: Kbj(SPixel%Ind%Ny, SPixel%Ind%NSolar)
   real, intent(out)           :: Sy(SPixel%Ind%Ny, SPixel%Ind%Ny)   
   integer, intent(out)        :: status

!  Declare local variables 

!   real       :: Sb(SPixel%Ind%Ny, SPixel%Ind%Ny)  ! Model par. error covariance
   real       :: Syb(SPixel%Ind%Ny, SPixel%Ind%Ny) ! Sb mapped into measurement 
                                                   ! errors.
!  integer    :: i             ! Counters

   status = 0

   Sy = SPixel%Sy

!  Ctrl flag Ctrl%EqMPN%Rs indicates whether EqMPN from Rs errors should be 
!  used. Kbj only contains values if there are solar channels in use.

   if (Ctrl%EqMPN%Rs /= 0 .and. SPixel%Ind%NSolar > 0) then

!     Set the Sb array using values from Ctrl
!     Removed for now: already set in GetRs. However GetRs is slow. This
!     simpler code may be useful in future! 
!
!      Sb = 0
!      do i=1, SPixel%Ind%Ny
!	 Sb(i,i) = Ctrl%Rs%Sb !  Add Ctrl%Rs%Cb, the correlation term as well?
!      end do


!     Calculate Syb = Kb * SRs * KbT.
!     Kbj is size (Ny * NSolar), SPixel%Rs is NSolar square. Hence the result 
!     is Ny square and conformable with Sy.

      Syb = matmul(Kbj, matmul(SPixel%SRs, transpose(Kbj)))
      
!     Add Syb to Sy  

      Sy = Sy + Syb
   end if 

End Subroutine Set_Sy
