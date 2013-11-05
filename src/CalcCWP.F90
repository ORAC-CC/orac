! Name:
!   Calc CWP
!
! Description:
!    This routine calculates the Cloud water path and the associated error on the Cloud waterPath
!
! Arguments:
!    Name       Type    In/Out/Both    Description
!    N/A
!
! Algorithm:
!    
!
! Local variables:
!    Name       Type    Description
!    Ctrl       struct  ECP control structure read from driver file
!    SAD_CloudClass     Array of structures each containing info on a cloud

!               logical
!
! History:

! 8th Nov 2011 Caroline Poulsen original version adapted from idl version
! 28th Nov2011 Caroline Poulsen remove log write statement
!
! Bugs:
!   None known
!
! $Id:$
!
!---------------------------------------------------------------------

Subroutine Calc_CWP (Ctrl,SPixel, status)

!  Modules used by this program. 

   use ECP_Constants
   use ECP_Routines_def    ! Defines subroutine interfaces for ReadSAD etc
   use SPixel_def
   use CTRL_def
!  Local variable declarations

   implicit none
   type(SPixel_t), intent(inout)  :: SPixel
   type(CTRL_t), intent(in)    :: Ctrl
   character(180)   :: message  ! Error message string returned by Read_Driver
   integer ,   intent(inout)   :: status  ! Status value returned from subroutines
   integer          :: ios  =0.    ! I/O status value from file operations
                                   ! ran successfully, arrays are allocated.
   real             :: rho ! liquid or water denisty
   real             :: fac !CWP factor
   real             :: s_cot_cre  !co-variance cot, effective radius (from log10cot, effective radius)
   real             :: s_cot  !variance in cot from variance in log10cot
   real             :: tenpcot  !10^cot convert from log value to linear value
   real              ::    al10e    =.434294  ! i.e  log10(exp(1.))  
   real              ::   al10e2         
! -------------------------------------------------------------------
! ------------------ Data Preparation functions
! -------------------------------------------------------------------

   if (trim(Ctrl%CloudClass%Name) == 'WAT') then
      rho=rhowat
      fac=(4./3.)*rho/qextwat
   else
      rho=rhoice
      fac=(4./3.)*rho/qextice  
   end if


   al10e2=al10e*al10e 

   tenpcot=10.**(Spixel%Xn(iTau))
  
   Spixel%cwp=fac* tenpcot *Spixel%Xn(iRe)


! covariance 
   s_cot_cre=(Spixel%Sn(iTau,iRe)*tenpcot)/al10e


   !error on optical depth
   s_cot=(Spixel%Sn(iTau,iTau)*tenpcot*tenpcot)/al10e2
   
   Spixel%cwp_error=Spixel%Xn(iRe)*Spixel%Xn(iRe)*s_cot+ &
        tenpcot* tenpcot*Spixel%Sn(iRe,iRe)+ &
        2.* tenpcot*Spixel%Xn(iRe)*s_cot_cre

!   write(*,*)'lwp error',Spixel%cwp_error
 

  !based on
   !Spixel%cwp_error=fac*sqrt(cre*cre*s_cot+cot*cot*s_cre+2.*cre*cot*s_cot_cre)
   

999 if (ios /= 0) then
      status = CWP_Calcerror
      call Write_Log(Ctrl,'Error calculating CWP',status)
   end if

 
   
 End Subroutine Calc_CWP
 
