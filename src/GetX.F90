! Name:
!    Get_X
!
! Purpose:
!    Gets the first guess and a priori state vector values (X0 and Xb) for 
!    the current super pixel according to the methods specified by SPixel%FG 
!    and SPixel%AP.
!
! Arguments:
!    Name             Type           In/Out   Description
!    Ctrl             struct         In       Control structure
!    SAD_CloudClass   struct         In       SAD cloud class structure
!    SPixel           struct         Both     Super-pixel structure (contains
!                                             the phase and AP, FG arrays to be
!                                             set).
!    status           integer        Out      Error status
!    
! Algorithm:
!    Set "first guess" phase to the value for the retrieval (phase is no longer 
!       retrieved). 
!    Set cloud class depending on the FG phase
!    For each state variable
!    - Set a priori state vector and error covariance by the selected method
!    - Set first guess state vector by the selected method
!      (if the FG method matches the AP, just copy the AP value to the FG)
!    - Check both state vectors for internal consistency (i.e. check vs. the
!      limits for the cloud class)
!    (Note that not all methods are supported for all variables).
!
!    Note that when error values are associated with the a priori the values 
!    must be squared for use in the ECP. It is assumed that the values 
!    obtained by each method are the rms errors.
!
! Local variables:
!    Name        Type      Description
!    i           int       Loop counter
!    SetErr      logical   Switch to tell X_MDAD whether to supply
!                          an error value.
!    X           real      State variable set by X_MDAD
!    Err         real      Error in state variable set by X_MDAD
!    message     char      Log file message
!
! History:
!  30th January, 2001, Kevin M. Smith : original version
!  24th April 2001, Andy Smith:
!     Using named constants for allowed selection method values.
!     Moved test for valid selection method to Read_Driver. No need to test
!     for every super-pixel.
!     Using SPixel%FGPhase instead of SPixel%X0(6) to store phase.
!     Added setting of SPixel%FGCloudClass.
!   8th June 2001, Andy Smith:
!     Changed structure of first guess phase setting to take account of 
!     cases when XMDAD setting is requested but unsuccessful.   
!     Added scaling of Sx.  
!  11th June 2001, Andy Smith:
!     Xscale must be squared where used with Sx since Sx contains the square
!     of the errors in X.
!   6th July 2001, Andy Smith:
!     Update to comments.
!     Added status check on SDAD a priori setting.
!  17th July 2001, Andy Smith:
!     Using named constants for Ts errors when set by AUX method. Errors 
!     now squared.
!  ***************************** ECV work starts here *********************
!  21st Mar 2011, Andy Smith:
!     Removal of functionality to change phase during retrieval. 
!     Phase will be initialised to the Ctrl value in the main function. 
!     Since phase is now fixed for the retrieval only 1 cloud class is used, 
!     hence the first guess cloud class is the 1 available class. 
!  22nd Mar 2011, Andy Smith:
!     Removal of phase change, phase 2. Only 1 cloud class rather than an 
!     array of N cloud classes. Ctrl struct reorganised so that X0, Xb, limits
!     etc only refer to one fixed phase (N.B. state variable not updated, so 
!     phase is still "retrieved"). 
!   5th Apr 2011, Andy Smith:
!     Removed selection methods SAD and SDAD. Renamed SelmMDAD to SelmMeas.
!     Mopping up from removal of phase change. Removed setting of Spixel%FGPhase
!     as it is redundant. 
!  5th Aug 2011, Caroline Polsen remove rf to sadcloudclass to simplify code
!     these values can be read from the driver file instead.
!  20th June 2012 C. Poulsen added in sacura option
!  2nd October 2012 C. Poulsen set in active statearibaes error to a very low
!     number i.e assume they are well known and let information csignal go to
!     active state variables
! 3rd October 2012 C. Poulsen modified how ctrl%sx is set added skint to be
!     surface temperature first guess
! 26th/2/2013 CP added in option to process pixel if only one IR channel is present
! Bugs:
!   None known.
!
! $Id: GetX.f90 76 2011-08-16 16:12:01Z capoulse $
!
!------------------------------------------------------------------------------------

Subroutine Get_X(Ctrl, SAD_Chan, SPixel, status)

   use ECP_Constants
   use CTRL_def
   use SAD_Chan_def
   use SPixel_def
   
   implicit none
   
!  Declare arguments

   type(Ctrl_t), intent(inout)           :: Ctrl
   type(SAD_Chan_t), intent(in)       :: SAD_Chan(Ctrl%Ind%Ny)
   type(SPixel_t), intent(inout)      :: SPixel
   integer, intent(out)               :: status   

!  Local variables

   logical    :: SetErr  ! Tells X_MDAD/X_SDAD that an error value should be set
   integer    :: i ,m      ! Loop counter
   real       :: X       ! State variable value returned by X_MDAD
   real       :: Err     ! Error value returned by X_MDAD
     

   SetErr = .false.
   
!  Set a priori and associated error covariance, then set first guess.
!  Having set both, check for internal consistency vs the cloud class limits. 
!  Loop over all variables, checking the method for each. One loop is used 
!  for both AP and FG setting. This is intended to minimise the number of
!  operations in incrementing the counter, but the setting of SetErr may offset
!  this saving.

   SPixel%Sx = 0 
   do i = 1, MaxStateVar
      if (status /= 0) exit
      SetErr = .true.   ! Error values are required for a priori
      select case (SPixel%AP(i))
      case (SelmMeas)   !  Measurement dependent. Not supported for all variables.
	 call X_MDAD(Ctrl, SAD_Chan, SPixel, i, SetErr, X, Err, status)
	 if (status /= XMDADMeth) then
	    SPixel%Xb(i)   = X
!write(*,*)'GetX:  SPixel%Xb(i) ', SPixel%Xb(i) 
	    SPixel%Sx(i,i) = (Err * Ctrl%Invpar%XScale(i)) ** 2
         end if

      case (SelmAUX)    !  AUX method not supported for most vars.
         if (i == ITs) then
!           Error setting could be much more sophisticated. The current scheme
!           takes no account of relative proportions of land/sea in the 
!           current SPixel.

	    SPixel%Xb(i)   = SPixel%RTM%LW%skint
	    if (SPixel%Surface%Sea == 1)  &
	      &  SPixel%Sx(i,i) = (AUXErrTsSea * Ctrl%Invpar%XScale(i)) ** 2
	    if (SPixel%Surface%Land == 1) &
	      &  SPixel%Sx(i,i) = (AUXErrTsLand * Ctrl%Invpar%XScale(i)) ** 2	    
         end if

      end select
        
!     Ctrl method, used if method is Ctrl or other methods failed.
!     Ctrl%Sx is squared after reading in.

      if (SPixel%AP(i) == SelmCtrl .or. &   
            (SPixel%AP(i) == SelmMeas .and. status == XMDADMeth) ) then  
         SPixel%Xb(i)   = Ctrl%Xb(i)
 


        if (SPixel%Illum(1) == IDay .or. SPixel%Illum(1) == IDaynore) then
            SPixel%Sx(i,i) = (Ctrl%Sx(i) * Ctrl%Invpar%XScale(i)) ** 2
            Ctrl%SX(:)=Ctrl%defaultSX(:)

!
!check if all IR channels are present if only one is present then remove surface temperature state variable from state vector by setting uncertainty to a very small number
! SPixel%QC = ibset(SPixel%QC, SPixTemp)
            if (btest(Spixel%QC,SPixTemp )) then
! check at least one thermal channel is present

!               if ( SPixel%Ym(Ctrl%Ind%ThermalFirst) .or. SPixel%Ym(Ctrl%Ind%ThermalLast) .lt. 2.0) then                  
!                  Ctrl%SX(5)=1.0e-5
!write(*,*)'ym',SPixel%Ym

!pause
!               end if
            end if

         else
! assume that the inactive state variables are well known do not try to retrieve

            do m=1,SPixel%NxI
               Ctrl%SX(SPixel%XI(m))=1.0e-5
            enddo
            SPixel%Sx(i,i) = (Ctrl%Sx(i) * Ctrl%Invpar%XScale(i)) ** 2
         endif

	 status = 0
    end if                                ! End of AP setting.


!     Having set a priori for the variable, set first guess. If the FG method is
!     the same as the AP, just copy the AP value. The first "if" is slightly 
!     dangerous: the assumption is that the first guess method is legal and
!     supported. This holds provided that the same methods are supported for
!     FG and AP.

!write(*,*)'aa',SPixel%FG(i), Spixel%AP(i)
      if (SPixel%FG(i) == Spixel%AP(i)) then
         SPixel%X0(i) = SPixel%Xb(i)

      else 
         if (status /= 0) exit
         SetErr = .false.  ! Error values are not required for first guess
	  
         select case (Spixel%FG(i))
         case (SelmMeas)   !  MDAD method. Not supported for all variables.
     
           call X_MDAD(Ctrl, SAD_Chan, SPixel, i, SetErr, X, Err, status)
 
	    SPixel%X0(i) = X
!write(*,*)'GetX:  SPixel%X0(i) ',i, SPixel%X0(i) 
	 case (Sacura)   !first guess is set using sacura
!	    SPixel%X0(i) =sacuravalue
	 case (SelmAUX)    !  AUX method not supported for most vars.
            if (i == ITs) &
	       SPixel%X0(i) = SPixel%RTM%LW%T(SPixel%RTM%LW%Np)

	 end select

!        Ctrl method, used if method is Ctrl or other methods failed.

	 if (SPixel%FG(i) == SelmCtrl .or. &             
               (SPixel%FG(i) == SelmMeas .and. status == XMDADMeth)) then   
            SPixel%X0(i) = Ctrl%X0(i)	 
	    status = 0
	 end if 

      end if   ! End of first guess setting    

!     Check for internal consistency with the cloud class limits. Set the 
!     a priori or first guess values equal to any limit they exceed.

!write(*,*)'SPixel%Xb',SPixel%Xb
!write(*,*)'ll',Ctrl%Invpar%xLlim
!write(*,*)'il',Ctrl%Invpar%xUlim
      if (SPixel%Xb(i) > Ctrl%Invpar%xUlim(i)) then
         SPixel%Xb(i) = Ctrl%Invpar%xUlim(i)
      else if (SPixel%Xb(i) < Ctrl%Invpar%xLlim(i)) then
         SPixel%Xb(i) = Ctrl%Invpar%xLlim(i)
      end if
         
      if (SPixel%X0(i) > Ctrl%Invpar%xUlim(i)) then
         SPixel%X0(i) = Ctrl%Invpar%xUlim(i)
      else if (SPixel%X0(i) < Ctrl%Invpar%xLlim(i)) then
         SPixel%X0(i) = Ctrl%Invpar%xLlim(i)
      end if 
!write(*,*)'SPixel%Xb i',SPixel%Xb
!pause        


   end do ! End of state variable loop.

End Subroutine Get_X
