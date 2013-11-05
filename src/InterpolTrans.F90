! Name:
!   Interpol_Trans
!
! Purpose:
!   Interpolates SW RTM transmittances onto current cloud pressure level
!   NOTE: It is assumed that the pressure levels are ordered in decreasing pressure
!   (increasing altitude). 
!
! Arguments:
!   Name            Type          In/Out   Description
!   Ctrl            struct        In       Control structure
!   Spixel          struct        In/Out   Super-pixel structure
!   Pc              float         In       Current cloud pressure
!   Tac             float array   Out      Interpolated above cloud transmittances
!   Tbc             float array   Out      Interpolated below cloud transmittances
!   d_Tac_d_Pc      float array   Out      Changes in Tac w.r.t. cloud pressure level
!   d_Tbc_d_Pc      float array   Out      Changes in Tbc w.r.t. cloud pressure level
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
!
! History:
!   15th November, 2000, Kevin M. Smith : original version
!
! Bugs:
!   None known.
!
!------------------------------------------------------------------------------------
subroutine Interpol_Trans(Ctrl, Spixel, Pc, Tac, Tbc, &
                          d_Tac_d_Pc, d_Tbc_d_Pc, status)

    use ECP_Constants
    use CTRL_def
    use Spixel_def 

    implicit none

!   Define arguments

    type(CTRL_t)   :: Ctrl
    type(Spixel_t) :: Spixel

!   Spixel%RTM
!    SW%np  = number of pressure levels
!    SW%p   = pressure levels
!    SW%Tbc = Surface-to-p transmittances from SW RTM
!    SW%Tac = p-to-TOA transmittances from SW RTM

    real*4 :: Pc
    real*4 :: Tac(Ctrl%Ind%Nsolar)
    real*4 :: Tbc(Ctrl%Ind%Nsolar)
    real*4 :: d_Tac_d_Pc(Ctrl%Ind%Nsolar)
    real*4 :: d_Tbc_d_Pc(Ctrl%Ind%Nsolar)
    integer :: status

!   Define local variables

    integer :: i
    integer :: j
    real*4  :: delta_p
    real*4  :: delta_Tac(Ctrl%Ind%Nsolar)
    real*4  :: delta_Tbc(Ctrl%Ind%Nsolar)
    real*4  :: delta_Pc
    real*4  :: delta_Tc(Ctrl%Ind%Nsolar)
    character(180) :: message

!   Set initial value of error status equal to zero (i.e. no error)

    status = 0

!   Search for Pc in the SW RTM pressure levels.  If Pc lies outwith the RTM pressure
!   levels avoid search and set index to 1 or the penultimate RTM level.

    if (Pc.gt.Spixel%RTM%SW%p(1)) then

!       When Pc below lowest level in RTM (i.e. Pc is larger than 1st RTM level)
        i = 1
        if (abs(Pc-Spixel%RTM%SW%p(1)).gt.50.0) then
!           When there is a difference of more than 50 hPa between Pc and RTM level
            write(unit=message, fmt=*) 'Interpol_Trans: Extrapolation warning' 
            call Write_Log(Ctrl, trim(message), status) ! Write to log
        end if

    else if (Pc.lt.Spixel%RTM%SW%p(Spixel%RTM%SW%Np)) then

!       When Pc above highest in level RTM (i.e. Pc is smaller than last RTM level)
        i = Spixel%RTM%SW%Np-1
        if (abs(Pc-Spixel%RTM%SW%p(Spixel%RTM%SW%Np)).gt.50.0) then
!           When there is a difference of more than 50 hPa between Pc and RTM level
            write(unit=message, fmt=*) 'Interpol_Trans: Extrapolation warning' 
            call Write_Log(Ctrl, trim(message), status) ! Write to log
        end if

    else

        do j = 1, Spixel%RTM%SW%Np-1 ! Search through RTM levels sequentially 
                                     ! to find those bounding Pc

            if (Pc.lt.Spixel%RTM%SW%p(j).and.Pc.ge.Spixel%RTM%SW%p(j+1)) then
                i = j ! Set index equal to the lower bounding RTM level (higher p)
                status = 0
		exit ! Break out of do loop to save time
            end if
    
            status = 1 ! Set status to unity if bounding levels not found
        end do
    end if
    
    If (status.ne.0) then

!           If none of the above conditions are met (e.g. Pc = NaN) then return
!           with a fatal error
            status = IntTransErr ! Set status to indicate failure of interpolation
            write(unit=message, fmt=*) 'Interpol_Trans: Interpolation failure' 
            call Write_Log(Ctrl, trim(message), status) ! Write to log
	    
    else

!           Start the interpolation or extrapolation calculations
!           Note: Implicit looping over instrument channels from here onwards 
        
!           Change in pressure between RTM levels i and i+1
!           (delta_p is negative for decreasing pressure with increasing i)

            delta_p = Spixel%RTM%SW%p(i+1) - Spixel%RTM%SW%p(i)

!           Change in transmittances between RTM levels i and i+1
!           (delta_Tac/bc are positive for increasing trans. with increasing i)

            delta_Tac = Spixel%RTM%SW%Tac(1:Ctrl%Ind%Nsolar,i+1) - Spixel%RTM%SW%Tac(1:Ctrl%Ind%Nsolar,i)
            delta_Tbc = Spixel%RTM%SW%Tbc(1:Ctrl%Ind%Nsolar,i+1) - Spixel%RTM%SW%Tbc(1:Ctrl%Ind%Nsolar,i)

!           Gradients of trans. with pressure (around Pc)

            d_Tac_d_Pc = delta_Tac / delta_p ! Change in trans / change in p
            d_Tbc_d_Pc = delta_Tbc / delta_p

!           Interpolated transmittances
!           (Sign conventions same as for delta_p. If Pc is outwith the RTM pressure
!           levels then extrapolation takes place using the same equations as for
!           interpolation. Note: The sign of delta_Pc will change for Pc greater than
!           the pressure of the lowest altitude RTM pressure level)

            delta_Pc = Pc - Spixel%RTM%SW%p(i) ! Diff. between Pc and lower RTM level
            
            delta_Tc = delta_Pc * d_Tac_d_Pc ! Diff. in trans from gradient
            Tac = Spixel%RTM%SW%Tac(1:Ctrl%Ind%Nsolar,i) + delta_Tc ! Abs. above cloud trans.
          
            delta_Tc = delta_Pc * d_Tbc_d_Pc ! Diff. in trans from gradient
            Tbc = Spixel%RTM%SW%Tbc(1:Ctrl%Ind%Nsolar,i) + delta_Tc ! Abs. below cloud trans.

    end if 

! End of subroutine Interpol_Trans

end
