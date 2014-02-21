! Name:
!    X_MDAD
!
! Purpose:
!    Sets up parts of the state vector, X, depending on measurements.
!
! Description: 
!    Sets a value for Tau, Pc or the cloud fraction F based on the 
!    measurement values Y in the current super-pixel. Only one variable
!    is set in a given call (which one is determined by the index value 
!    supplied). Result is returned in X.
!
!    Alternatively sets a phase value in SPixel%FGPhase.
!
! Arguments:
!    Name       Type       In/Out    Description
!    Ctrl       struct     In        Control structure
!    SPixel     struct     Both      Super pixel structure
!    index      int        In        The required state parameter index
!    SetErr     logical    In        State parameter error flag 
!    X          real       Out       The state parameter
!    Err        real       Out       (A priori) error in state parameter
!    status     int        Out       Indicates success/failure of subroutine.
!
! Algorithm:
!    Use index to select which variable is set:
!    Tau:
!       if (daytime and SPixel%Ind%MDAD_SW is available)
!          calculate an overcast reflectance for the channel nearest 0.67 microns
!          determine which of a pre-determined set of bands the value falls into
!          set X=the Tau value for the band
!       else
!          can't use this method for setting Tau, set status
!    Pc: 
!       if (SPixel%Ind%MDAD_LW is available)
!          Convert the measurement value nearest 11 microns to radiance 
!          Calculate overcast radiance from observed radiance and cloud fraction
!          Convert radiance back to brightness temperature
!          Find at which pressure level the calculated BT best matches the 
!          temperature in the RTM data 
!       else
!          can't use this method for setting Pc, set status
!   ( *** Phase section redundant, as of Mar 2011
!    Phase:
!       if (SPixel%Ind%MDAD_LW is available)
!          Convert the measurement value nearest 11 microns to radiance 
!          Calculate overcast radiance from observed radiance and cloud fraction
!          Convert radiance back to brightness temperature
!          check calculated BT vs Ctrl%PhaseT values for water/ice
!       else
!          can't use this method for setting Phase, set status
!   *** )
!    Cloud fraction: 
!       Use the SPixel cloud fraction value     
!
! Local variables:
!    Name       Type    Description
!    FGOP       real    First guess optical depths
!    Ref_o      real    Overcast reflectance around 0.67 microns
!    Rs         real    Surface reflectance around 0.67 microns
!    iFGOP      int     Index of first guess optical depth
!    BT_o       real    Overcast brightness temperature around 11 microns
!    Rad_o      real    Overcast radiance around 11 microns
!    dR_dT      real    Change in radiance with brightness temperature
!    dT_dR      real    Change in brightness temperature with radiance
!    i          int     Loop counter
!    message    char    Message to write to log
!
! History:
!    6 Feb 2000, Kevin Smith : Original version
!    1st Jun 2001, Andy Smith: 
!      2nd original version! Changes due to updates in overall design of 
!      first guess/a priori setting and coding changes elsewhere.
!    7th Jun 2001, Andy Smith:
!      Checks MDAD_Lw/MDAD_Sw are available where they need to be used.
!      The user's choice of channels as well as the illumination conditions
!      for the SPixel can determine whether or not a given variable can be
!      set by this method.
!    5th July 2001, Andy Smith:
!      Bug fix in indexing of R_clear with MDAD_LW.
!   16th July 2001, Andy Smith:
!      Added more error checking. Checks values of Rad_o used in Phase and Pc
!      calculations don't go negative.  
!      Using named constants for error values. 
!   26th Oct 2001, Andy Smith:
!      Fix to Tau calculation. Reflectances are now expressed as fractions
!      rather than percentages. Calculation of iFGOP updated to match.
!   ************************** ECV work starts here ********************
!   11th Mar 2011, Andy Smith:
!      Re-applying changes made in late 2001/2. 
!   (!   20th August changed the fractional error flagg C poulsen )
!      Plus correction to value in FGOP array: 1.15 instead of 1.5.
!      iPc loop to set BT_o was reverse, i.e. Np to 1 rather than 1 to Np.
!      iPhase: < replaced by <= in BT_o setting.
!   21st Mar 2011, Andy Smith:
!      Removing functionality to change phase during the retrieval. 
!      Commented out handling of iPhase. Phase will be fixed at the Ctrl
!      value.
!   30th Mar 2011, Andy Smith:
!      Removed commented-out code for handling phase, following removal 
!      of phase change functionality. 
!      Removal of super-pixel averaging. Removed use of Ctrl%Resoln%Space 
!      in cloud fraction setting. 
!  26th Apr 2011, Andy Smith:
!     Extension to handle multiple instrument views. The viewing geometry 
!     becomes a set of arrays, e.g. 1 value of sat. zen angle per view. 
!     Sec_o used in setting first guess optical path is now one value from the 
!     array. 
! 1st May 2012 Caroline Poulsen modified first guess cloud top temperature
!   to be an interpolation between layers rather than the lower layer
!  15th June 2012 C. Poulsen modief illum to be an array
!  8th Jul 2012 C. Poulsen fixed invalid memory access error
! 20120817 MJ fixed bug with divide by zero 
! 20121002 CP changed selection of first guess height
! 20131122 MJ rewrites selection of ctp FG/AP based on BT interpolation.
! Bugs:
!    None known.
!
! $Id$
!
!---------------------------------------------------------------------
subroutine X_MDAD(Ctrl, SAD_Chan, SPixel, index, SetErr, X, Err, status)

   use ECP_Constants
   use Ctrl_def
   use SAD_Chan_def
   use SPixel_def

   implicit none
  
!  Declare arguments

   type(Ctrl_t), intent(in)      :: Ctrl
   type(SAD_Chan_t), intent(in)  :: SAD_Chan(Ctrl%Ind%Ny)
   type(SPixel_t), intent(inout) :: SPixel   
   integer, intent(in)           :: index
   logical, intent(in)           :: SetErr
   real, intent(out)             :: X
   real, intent(out)             :: Err
   integer, intent(out)          :: status

!  Declare local variables

   real            :: FGOP(11)
   real            :: Ref_o
   integer         :: iFGOP
   real            :: BT_o
   real            :: Rad
   real            :: Rad_o
   real            :: dR_dT
   real            :: dT_dR
!  real            :: tdiff,tfrac,pnew,pdiff,tdiff1,tdiffstore
!  real            :: pdiffstore, pdiffa,pstop
!  integer         :: i,startp,endp,stepp,indexstop
!  character(180)  :: message
   
   status=0

!  pdiffstore=2000000.
!  tdiffstore=2000.
   !  Set up first guess optical depth vector

   data FGOP / 0.1, 0.3, 0.65, 0.8, 1.0, 1.15, 1.3, 1.5, 1.7, 2.0, 2.4 /
   !write(*,*) 'inside xmdad',SPixel%Ind%MDAD_LW

   !  Parameters supported are Tau, Pc and f.

   select case (index)

   case (iTau)   !     Cloud optical depth, Tau.
      
      if (SPixel%Illum(1) == IDay .and. SPixel%Ind%MDAD_SW > 0) then
         !           Uses channel nearest 0.67 microns, index Ctrl%Ind%MDAD_SW.   
         !           Calculate overcast reflectance.      
      
         Ref_o = ( SPixel%Ym(SPixel%Ind%MDAD_SW) -   &
              & ( SPixel%Rs(SPixel%Ind%MDAD_SW) * &
              & (1.0-SPixel%Cloud%Fraction)    &
              & ) &
              & ) / SPixel%Cloud%Fraction

         !           Convert albedo (range 0 - 1) into index (range 1 to 10)
         !           Use the first sec_o value, assuming that all values are quite 
         !           close and we only need an approximation for first guess setting.
         
         iFGOP = int( ( Ref_o * SPixel%Geom%SEC_o(1) * 10.0 ) + 1.5 )

         if (iFGOP > 11) then
            iFGOP = 11
         else if (iFGOP < 1) then
            iFGOP = 1
         end if

         X = FGOP(iFGOP)	 
         if (SetErr) Err = MDADErrTau	       
         
      else    !    Can't calculate Tau unless it's daylight
         status = XMDADMeth
      end if
	 
   case (iPc)    !     Cloud pressure, Pc.

      if (SPixel%Ind%MDAD_LW > 0) then
         !           Uses channel nearest 11 microns, index Ctrl%Ind%MDAD_LW in SAD_Chan,
         !           but SPixel%Ind%MDAD_LW in the measurement array.
         !           Convert observed brightness temperature to radiance
         !write(*,*) 'size of ym',size(SPixel%Ym)
         
         call T2R(1, SAD_Chan(Ctrl%Ind%MDAD_LW), &
              SPixel%Ym(SPixel%Ind%MDAD_LW), Rad, dR_dT, status)

         !           Calculate overcast radiance from observed radiance and cloud 
         !           fraction. Note MDAD_LW must be offset for use with R_Clear since
         !           R_Clear stores thermal channels only. 
         !write(*,*) 'rclear',SPixel%RTM%LW%R_clear(Ctrl%Ind%MDAD_LW-Ctrl%Ind%ThermalFirst+1)
         Rad_o = ( Rad - &
              SPixel%RTM%LW%R_clear(Ctrl%Ind%MDAD_LW-Ctrl%Ind%ThermalFirst+1) * &
              (1.0 - SPixel%Cloud%Fraction) )  &
              / SPixel%Cloud%Fraction
         !  Exclude negative Rad_o (can arise due to approximation in the RTM)
         !           Exclude negative Rad_o (can arise due to approximation in the RTM)

         if (Rad_o >= 0.0) then
            !write(*,*) 'rad>0'
            !              Convert overcast radiance back to brightness temperature
            call R2T(1, SAD_Chan(Ctrl%Ind%MDAD_LW), Rad_o, BT_o, dT_dR,status) 
             
            !this interpolates for the BT to the rad. profile to get ctp FG/AP
            call interpolate2ctp(SPixel,Ctrl,BT_o,X,Err)

!This is obsolete:            
!!$            do i = SPixel%RTM%LW%Np-1,1,-1
!!$               ! stop at closest to 80Hpa
!!$               pstop=60 !HPa
!!$        
!!$               pdiffa=abs(SPixel%RTM%LW%P(i+1)-pstop)
!!$        
!!$               if (pdiffa .lt. pdiffstore) then
!!$                  pdiffstore=pdiffa
!!$                  indexstop=i
!!$               endif
!!$            enddo
!!$
!!$            if (Ctrl%CloudClass%Name == 'ICE') then
!!$               startp= SPixel%RTM%LW%Np-2
!!$               endp=indexstop
!!$               stepp= -1
!!$            else
!!$               startp= SPixel%RTM%LW%Np-2
!!$               endp=indexstop
!!$               stepp= -1
!!$            endif
!!$
!!$            do i=startp,endp,stepp
!!$           
!!$               if (BT_o .gt. SPixel%RTM%LW%T(i-1)) then	    
!!$              
!!$                  !Xnew = SPixel%RTM%LW%P(i) !better to start one lower
!!$                  !write(*,*)'xmdad start',i, X  
!!$                  tdiff=SPixel%RTM%LW%T(i+1)-SPixel%RTM%LW%T(i)
!!$                  tdiff1=abs(BT_o -SPixel%RTM%LW%T(i))
!!$              
!!$                  if(tdiff .lt. ditherm3) tdiff=ditherm3
!!$                  if (tdiff1 .lt. tdiffstore) then
!!$                     tdiffstore=tdiff1
!!$                     tfrac=tdiff1/tdiff
!!$                     if (tfrac .gt. 1.0) then
!!$                        tfrac=0.0
!!$                     end if
!!$                     
!!$                     pdiff=SPixel%RTM%LW%P(i+1)-SPixel%RTM%LW%P(i)
!!$                     X=SPixel%RTM%LW%P(i)+tfrac*pdiff
!!$                     
!!$                     ! stop firstguess from going underaground
!!$                     if (X .gt. SPixel%RTM%LW%sp+5) then 
!!$                        X = SPixel%RTM%LW%sp+5
!!$                     end if
!!$                     if (SetErr) Err = MDADErrPc
!!$                     
!!$                 !                       		     exit	  
!!$                  end if
!!$               end if
!!$            end do

         else

            !              Negative Rad_O value: method won't work.
            !write(*,*) 'rad<0'
            !if no interpolation possible set BP_o and DBP_o to hardcoded values to recover:
            X=Ctrl%X0(3)
            !FG does not need Error but AP does
            Err=MDADErrPc
            status = XMDADMeth
         end if

         
      else    !    Can't calculate Pc if required LW channels not selected
         status = XMDADMeth
      end if
      
   case (iFr)   !     Cloud fraction, f, 

      !        Value is taken straight from the SPixel structure.      

      X = SPixel%Cloud%Fraction
      if (SetErr) Err = MDADErrF1

   end select

   !write(*,*) 'Error status', err,status

end subroutine X_MDAD
