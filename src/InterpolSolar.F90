! Name:
!   Interpol_Solar
!
! Purpose:
!   Interpolates SW RTM transmittances onto current cloud pressure level
!   NOTE: It is assumed that the pressure levels are ordered in decreasing pressure
!   (increasing altitude). 
!
! Arguments:
!   Name            Type       In/Out	Description
!   Ctrl            struct     In	Control structure
!   SPixel          struct     In/Out	Super-pixel structure
!   Pc              float      In	Current cloud pressure
!   RTM_Pc          struct     Out	Contains Tac, Tbc (interpolated
!                               	transmittances above and below cloud)
!                               	and gradients wrt cloud pressure.
!   status          int        Out      Standard status value not set here
!
! Algorithm:
!   Check to see if Pc is outwith the RTM pressure level range
!   (Warn if extrapolation beyond 50 hPa past the first/last RTM level is necessary)
!       If not, search sequentially through the RTM pressure levels to find the
!           pair that are bounding Pc
!       Return an error if none of these conditions are satisfied (e.g. Pc=NaN)
!   Determine the change in p and trans. between bounding RTM levels
!   Determine transmittance gradients (d_T_d_p) around Pc
!   Calculate change in trans. between RTM level i and Pc
!   Calculate absolute trans. at Pc
!
! Local variables:
!   Name        Type          Description
!   i           int           RTM pressure level index
!   j           int           Search loop counter
!   delta_p     float         Difference in pressure between consecutive RTM levels
!   delta_Tac   float array   Difference in Tac between consecutive RTM levels
!   delta_Tbc   float array   Difference in Tbc between consecutive RTM levels
!   delta_Pc    float         Difference in pressure between Pc and lower RTM level
!   delta_Tc    float         Difference in trans. between Pc and lower RTM level
!   message     char          Warning or error message to pass to Write_Log
!   bkp_lun     int           Unit number for breakpoint file
!   ios         int           I/O status for breakpoint file
!
! History:
!   15th November, 2000, Kevin M. Smith : original version
!    1st Feb 2001, Andy Smith:
!       Pressure level indices reversed in SPixel RTM arrays, i.e. 
!       values now increase with index.
!    9th Feb 2001, Andy Smith:
!       First fully completed and (informally) tested version.
!   15th Feb 2001, Andy Smith:
!       Channel ranges used for array size setting and indexing now picked up
!       from SPixel. 
!   16th Feb 2001, Andy Smith:
!       Checked through Pressure level tests and updated comments. Pressure
!       levels were originally intended to be stored from highest pressure
!       to lowest (i.e. ground upwards), but are now in order of pressure.
!   20th Feb 2001, Andy Smith:
!       Removing excess underscores from RTM_Pc variable names.
!   26th Feb 2001, Andy Smith:
!       Changed test for P levels bounding Pc. Using 
!       (Pc >= p(j) and Pc < p(j+1)) instead of  
!       (Pc > p(j) and Pc <= p(j+1)) because P levels now go in order of
!       increasing pressure. Previously, if Pc was equal to P(1) it was not
!       matched (and if Pc < P(1) it's P index is set to 1!)
!  3rd November 2011 MJ changed definition of delta_tac and delta_tbc arrays
! Bugs:
!   None known.
!
!------------------------------------------------------------------------------------
subroutine Interpol_Solar(Ctrl, SPixel, Pc, RTM_Pc, status)

    use ECP_Constants
    use CTRL_def
    use SPixel_def 
    use RTM_Pc_def

    implicit none

!   Define arguments

    type(CTRL_t), intent(in)      :: Ctrl
    type(SPixel_t), intent(in)    :: SPixel
!   SPixel%RTM
!    SW%Np  = number of pressure levels
!    SW%p   = pressure levels
!    SW%Tbc = Surface-to-p transmittances from SW RTM
!    SW%Tac = p-to-TOA transmittances from SW RTM
    real, intent(in)              :: Pc
    type(RTM_Pc_t), intent(inout) :: RTM_Pc
    integer, intent(inout)        :: status

!   Define local variables

    integer :: i
    integer :: j
    real    :: delta_p
!    real    :: delta_Tac(SPixel%Ind%Ny-SPixel%Ind%NThermal)
!    real    :: delta_Tbc(SPixel%Ind%Ny-SPixel%Ind%NThermal)
    real    :: delta_Tac(SPixel%Ind%NSolar)
    real    :: delta_Tbc(SPixel%Ind%NSolar)
    real    :: delta_Pc
!    real    :: delta_Tc(SPixel%Ind%Ny-SPixel%Ind%NThermal)
    real    :: delta_Tc(SPixel%Ind%NSolar)
    character(180) :: message
    integer :: bkp_lun                 !    Unit number for breakpoint file
    integer :: ios                     ! I/O status for breakpoint file
    integer :: k ! for testing

!   Set initial value of error status equal to zero (i.e. no error)

    status = 0

!   Search for Pc in the SW RTM pressure levels.  If Pc lies outwith the RTM pressure
!   levels avoid search and set index to 1 or the penultimate RTM level.

    if (Pc > SPixel%RTM%SW%p(SPixel%RTM%SW%Np)) then

!       When Pc above highest pressure in RTM 
        i = SPixel%RTM%SW%Np-1
        if (abs(Pc - SPixel%RTM%SW%p(SPixel%RTM%SW%Np)) > 50.0) then
!           When there is a difference of more than 50 hPa between Pc and RTM level
            write(unit=message, fmt=*) 'Interpol_Solar high: Extrapolation warning', Pc , SPixel%RTM%SW%p(SPixel%RTM%SW%Np)
            call Write_Log(Ctrl, trim(message), status) ! Write to log
        end if

    else if (Pc < SPixel%RTM%SW%p(1)) then

!       When Pc below lowest in level RTM 
        i = 1
        if (abs(Pc - SPixel%RTM%SW%p(1)) > 50.0) then
!           When there is a difference of more than 50 hPa between Pc and RTM level
            write(unit=message, fmt=*) 'Interpol_Solar low: Extrapolation warning', Pc , SPixel%RTM%SW%p(1)
            call Write_Log(Ctrl, trim(message), status) ! Write to log
        end if

    else

        do j = 1, SPixel%RTM%SW%Np-1 ! Search through RTM levels sequentially 
                                     ! to find those bounding Pc

            if (Pc >= SPixel%RTM%SW%p(j) .and. Pc < SPixel%RTM%SW%p(j+1)) then
                i = j ! Set index equal to the lower bounding RTM level (higher p)
                status = 0
		exit ! Break out of do loop to save time
            end if
    
            status = 1 ! Set status to unity if bounding levels not found
        end do
    end if
    
    If (status /= 0) then
!       If none of the above conditions are met (e.g. Pc = NaN) then return
!       with a fatal error

        status = IntTransErr ! Set status to indicate failure of interpolation
        write(unit=message, fmt=*) 'Interpol_Solar: Interpolation failure' 
        call Write_Log(Ctrl, trim(message), status) ! Write to log
	    
    else
!       Start the interpolation or extrapolation calculations
!       Note: Implicit looping over instrument channels from here onwards 
        
!       Change in pressure between RTM levels i and i+1

        delta_p = SPixel%RTM%SW%p(i+1) - SPixel%RTM%SW%p(i)

!       Change in transmittances between RTM levels i and i+1
!       (delta_Tac/bc are positive for increasing trans. with increasing i)

        delta_Tac = SPixel%RTM%SW%Tac(:,i+1) - SPixel%RTM%SW%Tac(:,i)
	delta_Tbc = SPixel%RTM%SW%Tbc(:,i+1) - SPixel%RTM%SW%Tbc(:,i)

!       Gradients of trans. with pressure (around Pc)
!       Change in trans / change in p

        RTM_Pc%SW%dTac_dPc = delta_Tac / delta_p 
        RTM_Pc%SW%dTbc_dPc = delta_Tbc / delta_p

!       Interpolated transmittances
!       (Sign conventions same as for delta_p. If Pc is outwith the RTM pressure
!       levels then extrapolation takes place using the same equations as for
!       interpolation. Note: The sign of delta_Pc will change for Pc greater than
!       the pressure of the lowest altitude RTM pressure level)

        delta_Pc = Pc - SPixel%RTM%SW%p(i) ! Diff. between Pc and lower RTM level

        delta_Tc = delta_Pc * RTM_Pc%SW%dTac_dPc ! Diff. in trans from gradient
        RTM_Pc%SW%Tac  = SPixel%RTM%SW%Tac(:,i) + delta_Tc ! Abs. above cloud trans.
	
        delta_Tc = delta_Pc * RTM_Pc%SW%dTbc_dPc ! Diff. in trans from gradient
        RTM_Pc%SW%Tbc  = SPixel%RTM%SW%Tbc(:,i) + delta_Tc ! Abs. below cloud trans.

    end if 

!  Open breakpoint file if required, and write our transmittances etc. 

#ifdef BKP
   if (Ctrl%Bkpl >= BkpL_Interpol_Solar) then
      call Find_Lun(bkp_lun)
      open(unit=bkp_lun,      & 
           file=Ctrl%FID%Bkp, &
	   status='old',      &
	   position='append', &
	   iostat=ios)
      if (ios /= 0) then
         status = BkpFileOpenErr
	 call Write_Log(Ctrl, 'Interpol_Solar: Error opening breakpoint file', &
	    status)
      else
         write(bkp_lun,*)'Interpol_Solar:'
      end if

      write(bkp_lun,'(a)') 'Chan ind  Tac       Tbc       dTac_dPc  dTbc_dPc'
      do i=1, SPixel%Ind%NY - SPixel%Ind%NThermal
      	 write(bkp_lun,'(5x,i2,4(1x,f9.4))') i, &
	    RTM_Pc%SW%Tac(i), RTM_Pc%SW%Tbc(i), RTM_Pc%SW%dTac_dPc(i),    &
	    RTM_Pc%SW%dTbc_dPc(i)
      end do
      
      write(bkp_lun, '(a,/)') 'Interpol_Solar: end'
      close(unit=bkp_lun)
   end if   
#endif


! End of subroutine Interpol_Solar

End Subroutine Interpol_Solar
