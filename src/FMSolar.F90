! Name:
!   FM_Solar
!
! Purpose:
!   Reflectance forward model (solar channels) for a defined pixel and
!   pressure level.
!
! Arguments:
!   Name     Type          In/Out Description
!   Ctrl     struct        In     Control structure
!   SAD_LUT  struct        In     SAD look up table
!   SPixel   struct        In     Super-pixel structure
!   RTM_Pc   struct        In     Contains transmittances, radiances and their 
!                                 derivatives
!   X        real array    In     State vector
!   GZero    struct        In     "Zero'th point" grid info for SAD_LUT CRP
!                                 array interpolation.
!   CRP      float array   In     Interpolated cloud radiative properties
!                                 (calculated by SetCRPSolar, but an input
!                                 argument because CRP for Td for the mixed
!                                 channels is set by SetCRPThermal. Only the 
!                                 solar channels should be passed by the calling
!                                 routine).
!   d_CRP    float array   In     Grads. of interpolated cloud rad. properties
!                                 (see comment for CRP).
!   REF      float array   Out    TOA reflectances for part cloudy conditions
!   d_REF    float array   Out    Derivatives d[ref]/d[tau,Re,pc,f,Ts,Rs]t
!   status   int           Out    Status from Set_CRP_Solar/breakpoint file open
!
! Algorithm:
!   Note: FMThermal must have been called prior to this routine, to populate 
!      CRP(ThermalFirst:SolarLast, ITd), i.e. for the part-thermal channels.
!   Get radiance functions
!   Calculate transmittances for current geometry
!   Calculate top of the atmosphere reflectances for clear, overcast, and
!      part cloudy conditions
!   Calculate derivatives of reflectance w.r.t. all other variables
!
! Local variables:
!   Name       Type	     Description
!   Tac_o      float array   Above cloud trans. at solar zenith angle
!   Tac_v      float array   Above cloud trans. at viewing angle
!   Tbc2       float array   The square of Tbc
!   REF_over   float array   TOA ref. for overcast conditions
!   T          float array   Above cloud trans. for reflected radiation
!   T_all      float array   Sum of direct beam and diffuse trans.
!   S          float array   Below cloud reflectance
!   Sp         float array   "S prime". Used where S must be divided by TBTD
!                            or Rs. Both terms can go to 0, hence Sp avoids
!                            divide by 0 errors.         
!   S_dnom     float array   Denominator of S
!   TBTD       float array   Product of T_B and T_D as in ATBD
!   bkp_lun    int	     Unit number for breakpoint file
!   ios        int	     I/O status for breakpoint file
!
! History:
!   7th November, 2000, Kevin M. Smith : original version
!   21st November, 2000: added X structure (state vector)
!   17th Jan 2001, Andy Smith: 
!      Changed indexing of CRP arrays to use constants to reference the different
!      LUT values (IRBd etc)
!      Using FM_Routines_def: contains interface definition for SetCRPSolar.
!   23rd Jan 2001, Andy Smith: 
!      Added GZero argument, interface to SetCRPSolar changed.
!      Updated CRP, d_CRP and Ref, d_Ref array indexing to use constants to 
!      pick out the values depending on Tau, Re etc.
!    2nd Feb 2001, Andy Smith: 
!      Transmittance values Tac etc made arguments, Interpol_Solar is now 
!      called before this function rather than from this function.
!    7th Feb 2001, Andy Smith: 
!      Picks up Tac values etc from RTM_Pc structure.
!    9th Feb 2001, Andy Smith:
!      First fully completed and (informally) tested version.
!   15th Feb 2001, Andy Smith:
!      Array sizes changed: Ctrl%Ind%NSolar to SPixel%Ind%NSolar
!      Indices changed where whole dimension is used, replaced 
!      array(1: SPixel%Ind%NSolar) with array(:).
!   16th Feb 2001, Andy Smith:
!      Array sizes changed again: only the purely solar channels are 
!      required. Use Ny-NThermal instead of NSolar. Calculate a local
!      NSolar variable (with the same value) for indexing SPixel%Rs.
!      SetCRPSolar now requires SPixel as an argument.
!   19th Feb 2001, Andy Smith:
!      Error in previous revision. Arrays must hold all solar channels, 
!      not just purely solar.
!      CRP, d_CRP required as arguments (partially populated by FMThermal).
!      SetCRPSolar now takes SPixel%Ind as an argument instead of SPixel.
!    2nd Mar 2001, Andy Smith:
!      Updates to cope with transmittances etc as fractions instead of 
!      percentages.
!    6th Mar 2001, Andy Smith:
!      Calculation of Ref_clear and dRef_clear_dRs moved to Get_SPixel.
!      (Values are now part of SPixel%RTM).
!    7th Mar 2001, Andy Smith:
!      Tac, Tbc etc now picked up from the overall RTM_Pc struct rather than
!      the SW sub-structure, so that all solar channels are selected, and not
!      just the purely solar.
!   13th Mar 2001, Andy Smith:
!      Changed some of the "compound" variables: S, TBTD etc can go to 0
!      and are used in division operations. New variable Sp (S prime, replaces
!      S / TBTD).
!   22nd Mar 2001, Andy Smith:
!      Corrected equations for d_Ref wrt Tau and Re
!   23rd Mar 2001, Andy Smith:
!      Updated calculation of dRef wrt Rs to include divide by SPixel%Geom%SEC_o
!      since SPixel Rs values now include this factor.
!   15th Oct 2001, Andy Smith:
!      Gradient w.r.t Rs fixed. Previously only the second term was divided by
!      sec_o, whereas the whole expression should have been divided. Also the 
!      bracketing was incorrect: instead of a "f" term and a "1-f" term, 
!      everything was multiplied by f. (For "f" read "X(IFr)" in the code).
!    5th May 2011, Andy Smith:
!      Extension to multiple viewing angles. Some whole-array assignments replaced 
!      by loops over channels, where appropriate viewing geometry must be selected. 
!      Added some breakpoint outputs. 
! ! 20 Jan 2012 C Poulsen fix bug with rtm_pc%tbc array allocation

! Bugs:
!   None known.
!
!------------------------------------------------------------------------------------

Subroutine FM_Solar(Ctrl, SAD_LUT, SPixel, RTM_Pc, X, GZero, CRP, d_CRP, &
   REF, d_REF, status)

   use ECP_Constants
   use CTRL_def
   use SAD_LUT_def
   use SPixel_def
   use RTM_Pc_def
   use GZero_def
   use FM_Routines_def

   implicit none

!  Define arguments

   type(CTRL_t), intent(in)       :: Ctrl
   type(SAD_LUT_t), intent(inout) :: SAD_LUT
   type(SPixel_t), intent(in)     :: SPixel
   type(RTM_Pc_t), intent(in)     :: RTM_Pc
   real, intent(in)               :: X(MaxStateVar)
   type(GZero_t), intent(inout)   :: GZero
   real, intent(inout)            :: CRP(SPixel%Ind%NSolar,MaxCRProps) 
                                     ! CRP is size Ny in the calling routine,
				     ! only Solar channels handled here.
   real, intent(inout)            :: d_CRP(SPixel%Ind%NSolar,MaxCRProps,2) 
   real, intent(inout)            :: REF(SPixel%Ind%NSolar)
   real, intent(inout)            :: d_REF(SPixel%Ind%NSolar, MaxStateVar+1) 
                                    ! Gradients in all state vars + Rs
   integer, intent(inout)         :: status

!  Define local variables

   real    :: Tac_o(SPixel%Ind%NSolar)            
   real    :: Tac_v(SPixel%Ind%NSolar)                   
   real    :: Tbc2(SPixel%Ind%NSolar)           
   real    :: REF_over(SPixel%Ind%NSolar)                   
   real    :: T(SPixel%Ind%NSolar)                 
   real    :: T_all(SPixel%Ind%NSolar)             
   real    :: S(SPixel%Ind%NSolar)                 
   real    :: Sp(SPixel%Ind%NSolar)                 
   real    :: S_dnom(SPixel%Ind%NSolar)
   real    :: TBTD(SPixel%Ind%NSolar)
   integer :: bkp_lun                 ! Unit number for breakpoint file
   integer :: ios                     ! I/O status for breakpoint file
   integer :: i,j                     ! For breakpoint output loops

!  Interpolate cloud radiative property LUT data to the current Tau, Re values
!  Note that Set_CRP_Solar interpolates values for all solar channels, except
!  in the case of Td. This is interpolated by SetCRPThermal, which is called
!  by FMThermal prior to this routine.

   call Set_CRP_Solar(Ctrl, SPixel%Ind, GZero, SAD_LUT, CRP, d_CRP, status)
!!$   write(*,*) 'after set'
!!$   do i=1,SPixel%Ind%NSolar
!!$      do j=1,MaxCRProps
!!$         write(*,*) i,j,crp(i,j)
!!$      enddo
!!$   enddo
!!$   stop
!  Calculate transmittances
   !striclty this loop should have solarfirst and last as boundaries?
   do i=1,SPixel%Ind%NSolar
      ! Above cloud at solar zenith angle:
      Tac_o(i) = RTM_Pc%Tac(i) ** SPixel%Geom%SEC_o(SPixel%ViewIdx(i))
      ! Above cloud at viewing angle:
      Tac_v(i) = RTM_Pc%Tac(i) ** SPixel%Geom%SEC_v(SPixel%ViewIdx(i))
   end do

   ! Calculate square of T_BC
   Tbc2 = RTM_Pc%Tbc(1:SPixel%Ind%Nsolar) * RTM_Pc%Tbc(1:SPixel%Ind%Nsolar)

   !  Calculate top of atmosphere 2-path transmittance for overcast (full cloud cover)
   !  conditions
   ! Above cloud transmittance for reflected radiation:
   T = Tac_o(1:SPixel%Ind%Nsolar) * Tac_v(1:SPixel%Ind%Nsolar)
   
   !  Sum of direct and diffuse beam transmissions
   !  Referencing of different properties stored in CRP and d_CRP: the last
   !  index of the CRP array refers to the property. Hence ITB is the index
   !  of TB, etc.
   T_all = (CRP(:,ITB) + CRP(:,ITFBd)) 

   S_dnom = 1.0 - (SPixel%Rs * CRP(:,IRFd) * Tbc2)
   !  CRP(1:SPixel%Ind%Nsolar,IRFd), diffuse reflectance

   S = ( T_all * SPixel%Rs * CRP(:,ITd) * Tbc2 ) / S_dnom

   Sp = Tbc2 / S_dnom

   !  Below cloud transmittance for reflected radiation 
   REF_over = T * (CRP(:,IRBd) + S) ! Total 

   !  Calculate top of atmosphere reflectance for fractional cloud cover
   !!MJ this is actualy obsolete as X(iFR)==1
   REF = (X(IFr)*REF_over)   + ((1.0-X(IFr)) * SPixel%RTM%REF_clear) 

   !  Calculate derivatives of reflectance w.r.t. all other variables
   !  for part cloudy conditions
   TBTD = T_all * CRP(:,ITd) ! See ATBD, T_B * T_D

!   write(*,*) 'd_REF(:,ITau)',d_REF(:,ITau)
        
!  Derivative w.r.t. cloud optical depth, Tau
   d_REF(:,ITau) = X(IFr) * T * &
        & ( d_CRP(:,IRBd,ITau) + &
        & Sp * SPixel%Rs * &
        & ( T_all * d_CRP(:,ITd,ITau) + &
        & CRP(:,ITd) * (d_CRP(:,ITB,ITau) + d_CRP(:,ITFBd,ITau)) &
        & ) + &
        & S * SPixel%Rs * Tbc2 * d_CRP(:,IRFd,ITau) / S_dnom)

   


!  Derivative w.r.t. cloud drop size, r_e
   d_REF(:,IRe) = X(IFr) * T * &
        & ( d_CRP(:,IRBd,IRe) + &
        & Sp * SPixel%Rs * &
        & ( T_all * d_CRP(:,ITd,IRe) +  &
        & CRP(:,ITd) * (d_CRP(:,ITB,IRe) + d_CRP(:,ITFBd,IRe))) + &
        & S * SPixel%Rs * Tbc2 * d_CRP(:,IRFd,Ire) / S_dnom)
!   write(*,*) 's_dnom',s_dnom
!   write(*,*) 'd_REF(:,IRe)',d_REF(:,IRe)

!!$   d_REF(:,IRe) = X(IFr) * T * &
!!$        & ( d_CRP(:,IRBd,IRe) + &
!!$        & Sp * SPixel%Rs * &
!!$        & ( T_all * d_CRP(:,ITd,IRe) +  &
!!$        & CRP(:,ITd) * (d_CRP(:,ITB,IRe) + d_CRP(:,ITFBd,IRe))) + &
!!$        & S * SPixel%Rs * Tbc2 * d_CRP(:,IRFd,Ire) / S_dnom)


!   write(*,*) 'after that',d_REF(:,IRe)
!   stop

!!$   d_REF(:,IRe) = X(IFr) * T * &
!!$        & ( d_CRP(:,IRBd,IRe) + &
!!$        & Sp * SPixel%Rs * &
!!$        & ( T_all * d_CRP(:,ITd,IRe) +  &
!!$        & CRP(:,ITd) * (d_CRP(:,ITB,IRe) + d_CRP(:,ITFBd,IRe)) &
!!$        & ) + &
!!$        & S * SPixel%Rs * Tbc2 * d_CRP(:,IRFd,Ire) / S_dnom)

!  Derivative w.r.t. cloud pressure, p_c
   do i=1,SPixel%Ind%NSolar
      d_REF(i,IPc) = X(IFr) * &
           & ( 1.0 * RTM_Pc%dTac_dPc(i) * & 
           & (SPixel%Geom%SEC_o(SPixel%ViewIdx(i)) + SPixel%Geom%SEC_v(SPixel%ViewIdx(i))) &
           & * REF_over(i)/RTM_Pc%Tac(i))+ &
           & ( 2.0 * RTM_Pc%dTbc_dPc(i) * T(i) * S(i) * RTM_Pc%Tbc(i) *  &
           & ( (1.0/Tbc2(i)) + (CRP(i,IRFd) * SPixel%Rs(i) / &
           & S_dnom(i)) ))
   end do

!  Derivative w.r.t. cloud fraction, f
   d_REF(:,IFr) = ( REF_over - SPixel%RTM%REF_clear )

!  Derivative w.r.t. surface temperature, T_s
   d_REF(:,ITs) = 0.0 ! Constant and zero

!  Derivative w.r.t. surface reflectance, R_s
   do i=1,SPixel%Ind%NSolar
      d_REF(i,IRs) =       &
           & (  X(IFr) * T(i) * &
           & ( Sp(i) * TBTD(i) + (S(i) * CRP(i,IRFd) * Tbc2(i)/S_dnom(i)) ) + &
           & (SPixel%RTM%dREF_clear_dRs(i) * (1.0-X(IFr))))/ &
           & SPixel%Geom%SEC_o(SPixel%ViewIdx(i))
   end do


   
!  Open breakpoint file if required, and write out reflectances and gradients. 

#ifdef BKP
   if (Ctrl%Bkpl >= BkpL_FM_Solar) then
      call Find_Lun(bkp_lun)
      open(unit=bkp_lun,      & 
           file=Ctrl%FID%Bkp, &
	   status='old',      &
	   position='append', &
	   iostat=ios)
      if (ios /= 0) then
         status = BkpFileOpenErr
	 call Write_Log(Ctrl, 'FM_Solar: Error opening breakpoint file', status)
      else
         write(bkp_lun,*)'FM_Solar:'
      end if

      do i=SPixel%Ind%SolarFirst, SPixel%Ind%SolarLast
      	 write(bkp_lun,'(a,i2,a,f9.4)') 'Channel index: ', i, &
	    ' Ref:         ', Ref(i)
      	 write(bkp_lun,'(a,i2,a,f9.4)') 'Channel index: ', i, &
	    ' Ref_over:    ', Ref_over(i)
      	 write(bkp_lun,'(a,i2,a,f9.4)') 'Channel index: ', i, &
	    ' T:           ', T(i)
      	 write(bkp_lun,'(a,i2,a,f9.4)') 'Channel index: ', i, &
	    ' T_all:       ', T_all(i)
      	 write(bkp_lun,'(a,i2,a,f9.4)') 'Channel index: ', i, &
	    ' S:           ', S(i)
      	 write(bkp_lun,'(a,i2,a,f9.4)') 'Channel index: ', i, &
	    ' CRP(:,IRBd): ',CRP(i,IRBd) 
      	 write(bkp_lun,'(a,i2,a,f9.4)') 'Channel index: ', i, &
	    ' CRP(:,ITd):  ',CRP(i,ITd) 
      	 write(bkp_lun,'(a,i2,a,f9.4)') 'Channel index: ', i, &
	    ' CRP(:,ITB):  ',CRP(i,ITB) 
      	 write(bkp_lun,'(a,i2,a,f9.4)') 'Channel index: ', i, &
	    ' CRP(:,ITFBd):',CRP(i,ITFBd) 
      	 write(bkp_lun,'(a,i2,a,f9.4)') 'Channel index: ', i, &
	    ' CRP(:,IRFd): ',CRP(i,IRFd) 
      end do

      do i=SPixel%Ind%SolarFirst, SPixel%Ind%SolarLast
      	 write(bkp_lun,'(a,i2,a,6f9.4)') 'Channel index: ', i, &
	    ' dRef: ', (d_Ref(i,j),j=1,MaxStateVar+1)
      end do

      write(bkp_lun, '(a,/)') 'FM_Solar: end'
      close(unit=bkp_lun)
   end if   
#endif


! End of subroutine FM_Solar

End Subroutine FM_Solar
