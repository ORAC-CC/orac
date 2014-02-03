! Name:
!   Interpol_Therm
!
! Purpose:
!   Interpolates LW transmittances and radiances to the cloud pressure level.
!
! Arguments:
!   Name            Type          In/Out   Description
!   Ctrl            struct        In       Control structure
!   Spixel          struct        In       Super-pixel structure
!   X%Pc            float         In       Cloud pressure (from state vector X)
!   Tac             float array   Out      Above cloud trans. interpolated to Pc
!   Tbc             float array   Out      Below cloud trans. interpolated to Pc
!   B               float array   Out      Planck functions interpolated to Pc
!   Rac_up          float array   Out      Above cloud upward rad. interpolated to Pc
!   Rac_dwn         float array   Out      Above cloud downward rad. interpolated to Pc
!   Rbc_up          float array   Out      Below cloud upward rad. interpolated to Pc
!   d_B_d_Pc        float array   Out      Change in Planck functions w.r.t Pc
!   d_Rac_up_d_Pc   float array   Out      Change in Rac_up w.r.t Pc
!   d_Rac_dwn_d_Pc  float array   Out      Change in Rac_down w.r.t Pc
!   d_Rbc_up_d_Pc   float array   Out      Change in Rbc_up w.r.t Pc
!                        
! Algorithm:
!   
! Local variables:
!   Name            Type          Description
!   i               int           RTM pressure level index
!   j               int           Search loop counter
!   delta_p         float         Difference in pressure between consecutive RTM levels
!   delta_Tac       float array   Difference in Tac between consecutive RTM levels
!   delta_Tbc       float array   Difference in Tbc between consecutive RTM levels
!   delta_Rac_up    float array   Difference in Rac_up between consecutive RTM levels
!   delta_Rac_dwn   float array   Difference in Rac_dwn between consecutive RTM levels
!   delta_Rbc_up    float array   Difference in Rbc_up between consecutive RTM levels
!   delta_B         float array   Difference in B between consecutive RTM levels
!   d_Tac_d_Pc      float array   Change in Tac w.r.t. Pc
!   d_Tbc_d_Pc      float array   Change in Tbc w.r.t. Pc
!   delta_Pc        float         Difference in pressure between Pc and lower RTM level
!   delta_Tc        float array   Difference in trans. between Pc and lower RTM level
!   delta_Rc        float array   Difference in radiance between Pc and lower RTM level
!   delta_Bc        float array   Difference in Planck functions beween Pc and lower RTM level
!   message         char          Warning or error message to pass to Write_Log
!
! History:
!   21st November, 2000, Kevin M. Smith : original version
!
! Bugs:
!   None known.
!
! $Id$
!
!------------------------------------------------------------------------------------
subroutine Interpol_Therm(Ctrl, Spixel, Pc, Tac, Tbc,         &
                          B, Rac_up, Rac_dwn, Rbc_up, d_B_d_Pc,        &
                          d_Rac_up_d_Pc, d_Rac_dwn_d_Pc, d_Rbc_up_d_Pc, status)

    use CTRL_def
    use Spixel_def

    implicit none

!   Define arguments

    type(CTRL_t)   :: Ctrl
    type(Spixel_t) :: Spixel

    real*4  :: Pc
    real*4  :: Tac(Ctrl%Ind%Nthermal)
    real*4  :: Tbc(Ctrl%Ind%Nthermal)
    real*4  :: B(Ctrl%Ind%Nthermal)
    real*4  :: Rac_up(Ctrl%Ind%Nthermal)
    real*4  :: Rac_dwn(Ctrl%Ind%Nthermal)
    real*4  :: Rbc_up(Ctrl%Ind%Nthermal)
    real*4  :: d_B_d_Pc(Ctrl%Ind%Nthermal)
    real*4  :: d_Rac_up_d_Pc(Ctrl%Ind%Nthermal)
    real*4  :: d_Rac_dwn_d_Pc(Ctrl%Ind%Nthermal)
    real*4  :: d_Rbc_up_d_Pc(Ctrl%Ind%Nthermal)
    integer :: status

!   Define local variables

    integer :: i
    integer :: j
    real*4  :: delta_p
    real*4  :: delta_Tac(Ctrl%Ind%Nthermal)
    real*4  :: delta_Tbc(Ctrl%Ind%Nthermal)
    real*4  :: delta_Rac_up(Ctrl%Ind%Nthermal)
    real*4  :: delta_Rac_dwn(Ctrl%Ind%Nthermal)
    real*4  :: delta_Rbc_up(Ctrl%Ind%Nthermal)
    real*4  :: delta_B(Ctrl%Ind%Nthermal)
    real*4  :: d_Tac_d_Pc(Ctrl%Ind%Nthermal)
    real*4  :: d_Tbc_d_Pc(Ctrl%Ind%Nthermal)
    real*4  :: delta_Pc
    real*4  :: delta_Tc(Ctrl%Ind%Nthermal)
    real*4  :: delta_Rc(Ctrl%Ind%Nthermal)
    real*4  :: delta_Bc(Ctrl%Ind%Nthermal)
    character(180) :: message

!   Set initial value of error status equal to zero (i.e. no error)

    status = 0

!   Search for Pc in the LW RTM pressure levels.  If Pc lies outwith the RTM pressure
!   levels avoid search and set index to 1 or the penultimate RTM level.

    if (Pc.gt.Spixel%RTM%LW%p(1)) then

!       When Pc below lowest level in RTM (i.e. Pc is larger than 1st RTM level)
        i = 1
        if (abs(Pc-Spixel%RTM%LW%p(1)).gt.50.0) then
!           When there is a difference of more than 50 hPa between Pc and RTM level
            write(unit=message, fmt=*) 'Interpol_Therm: Extrapolation  warning low', Pc,Spixel%RTM%LW%p(1)
            call Write_Log(Ctrl, trim(message), status) ! Write to log
        end if

    else if (Pc.lt.Spixel%RTM%LW%p(Spixel%RTM%LW%Np)) then

!       When Pc above highest in level RTM (i.e. Pc is smaller than last RTM level)
        i = Spixel%RTM%LW%Np-1
        if (abs(Pc-Spixel%RTM%LW%p(Spixel%RTM%LW%Np)).gt.50.0) then
!           When there is a difference of more than 50 hPa between Pc and RTM level
            write(unit=message, fmt=*) 'Interpol_Therm: Extrapolation warning high', Pc,Spixel%RTM%LW%p(Spixel%RTM%LW%Np)
            call Write_Log(Ctrl, trim(message), status) ! Write to log
        end if

    else

        do j = 1, Spixel%RTM%LW%Np-1 ! Search through RTM levels sequentially 
                                     ! to find those bounding Pc

            if (Pc.lt.Spixel%RTM%LW%p(j).and.Pc.ge.Spixel%RTM%LW%p(j+1)) then
                i = j ! Set index equal to the lower bounding RTM level (higher p)
                status = 0
                exit ! Break out of do loop to save time
            end if
            
            status = 1 ! Bounding levels not found

        end do

        if (status.ne.0) then

!           If none of the above conditions are met (e.g. Pc = NaN) then return
!           with a fatal error
            status = IntTransErr ! Set status to indicate failure of interpolation
            write(unit=message, fmt=*) 'Interpol_Therm: Interpolation failure' 
            call Write_Log(Ctrl, trim(message), status) ! Write to log

        else

!           Start the interpolation or extrapolation calculations
!           Note: Implicit looping over instrument channels from here onwards 
        
!           Change in pressure between RTM levels i and i+1
!           (delta_p is negative for decreasing pressure with increasing i)

            delta_p = Spixel%RTM%LW%p(i+1) - Spixel%RTM%LW%p(i)

!           Change in transmittances between RTM levels i and i+1
!           (delta_Tac/bc are positive for increasing trans. with increasing i)

            delta_Tac = Spixel%RTM%LW%Tac(1:Ctrl%Ind%Nthermal,i+1) - &
                        Spixel%RTM%LW%Tac(1:Ctrl%Ind%Nthermal,i)
            delta_Tbc = Spixel%RTM%LW%Tbc(1:Ctrl%Ind%Nthermal,i+1) - &
                        Spixel%RTM%LW%Tbc(1:Ctrl%Ind%Nthermal,i)

!           Gradients of trans. w.r.t. pressure (around Pc)

            d_Tac_d_Pc = delta_Tac / delta_p ! Change in trans / change in p
            d_Tbc_d_Pc = delta_Tbc / delta_p

!           Change in radiances between RTM levels i and i+1

            delta_Rac_up = Spixel%RTM%LW%Rac_up(1:Ctrl%Ind%Nthermal,i+1) - &
                           Spixel%RTM%LW%Rac_up(1:Ctrl%Ind%Nthermal,i)
            delta_Rac_dwn = Spixel%RTM%LW%Rac_dwn(1:Ctrl%Ind%Nthermal,i+1) - &
                           Spixel%RTM%LW%Rac_dwn(1:Ctrl%Ind%Nthermal,i)
            delta_Rbc_up = Spixel%RTM%LW%Rbc_up(1:Ctrl%Ind%Nthermal,i+1) - &
                           Spixel%RTM%LW%Rbc_up(1:Ctrl%Ind%Nthermal,i)

!           Gradients of radiances w.r.t. pressure (around Pc)

            d_Rac_up_d_Pc = delta_Rac_up / delta_p
            d_Rac_dwn_d_Pc = delta_Rac_dwn / delta_p
            d_Rbc_up_d_Pc = delta_Rbc_up / delta_p

!           Change in Planck functions between RTM levels i and i+1

            delta_B = Spixel%RTM%LW%B(1:Ctrl%Ind%Nthermal,i+1) - &
                      Spixel%RTM%LW%B(1:Ctrl%Ind%Nthermal,i)

!           Gradient of Planck functions w.r.t. pressure (around Pc)

            d_B_d_Pc = delta_B / delta_p
 
!           Interpolated transmittances
!           (Sign conventions same as for delta_p. If Pc is outwith the RTM pressure
!           levels then extrapolation takes place using the same equations as for
!           interpolation. Note: The sign of delta_Pc will change for Pc greater than
!           the pressure of the lowest altitude RTM pressure level)

            delta_Pc = Pc - Spixel%RTM%LW%p(i) ! Diff. between Pc and lower RTM level
            
            delta_Tc = delta_Pc * d_Tac_d_Pc ! Diff. in trans from gradient
            Tac = Spixel%RTM%LW%Tac(1:Ctrl%Ind%Nthermal,i) + delta_Tc
          
            delta_Tc = delta_Pc * d_Tbc_d_Pc ! Diff. in trans from gradient
            Tbc = Spixel%RTM%LW%Tbc(1:Ctrl%Ind%Nthermal,i) + delta_Tc

!           Interpolated radiances

            delta_Rc = delta_Pc * d_Rac_up_d_Pc
            Rac_up = Spixel%RTM%LW%Rac_up(1:Ctrl%Ind%Nthermal,i) + delta_Rc

            delta_Rc = delta_Pc * d_Rac_dwn_d_Pc
            Rac_dwn = Spixel%RTM%LW%Rac_dwn(1:Ctrl%Ind%Nthermal,i) + delta_Rc

            delta_Rc = delta_Pc * d_Rbc_up_d_Pc
            Rbc_up = Spixel%RTM%LW%Rbc_up(1:Ctrl%Ind%Nthermal,i) + delta_Rc

!           Interpolated Planck functions

            delta_Bc = delta_Pc * d_B_d_Pc
            B = Spixel%RTM%LW%B(1:Ctrl%Ind%Nthermal,i) + delta_Bc

        endif

    end if

! End of subroutine Interpol_Therm

end
