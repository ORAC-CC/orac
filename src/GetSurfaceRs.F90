! Name:
!    Get_Surface_Rs
!
! Purpose:
!    Obtains estimates of the surface reflectivity for the channels with
!    solar radiance components for a single pixel.
!
! Arguments:
!    Name     Type         In/Out   Description
!    Ctrl     struct       In       Control structure
!    Flag     byte         In       Land/sea pixel flag
!    Rs       real array   Out      Land & sea surface reflectances for all solar channels
!    SRs      real array   Out      Correlation matrix for Rs
!    status   integer      Out      Error status
!    
! Algorithm:
!
! Local variables:
!    Name   Type        Description
!    i,j    int         Loop counters
!    k      int         Land/sea index in Rs and SRs
!
! History:
!   4th December, 2000, Kevin M. Smith : original version
!
! Bugs:
!   None known.
!
! $Id$
!
!------------------------------------------------------------------------------------
subroutine Get_Surface_Rs(Ctrl, Flag, Rs, SRs, status)
   
    use CTRL_def

    implicit none

!   Define arguments

    type(CTRL_t)   :: Ctrl
    
    byte    :: Flag
    real(4) :: Rs(Ctrl%Ind%NSolar)
    real(4) :: SRs(Ctrl%Ind%NSolar, Ctrl%Ind%NSolar)
    integer :: status

!   Define local variables

    integer :: i, j, k

!   Set status to zero

    status = 0

!   Determine whether land or sea
!   In Ctrl%Rs%B the order is 1 = land, 2 = sea
!   In Data_LSFlags (and Flag here) 0 = sea, 1 = land

    k = 2 ! sea (default)
    if (Flag == 1) k = 1 ! land

!   Set Rs according to the method defined in control structure

!   CTRL method

    if (Ctrl%Rs%Flag = 1) then

!       Set Rs

        Rs  = Ctrl%Rs%B(1:Ctrl%Ind%NSolar,k)

!       Calculate the covariance matrix SRs

        do i = 1, Ctrl%Ind%NSolar

            do j = 1, Ctrl%Ind%NSolar

!               On-diagonal elements (% error taken from Ctrl%Rs%Sb)

                if (i == j) then

                    SRs(i,j) = ( ( Ctrl%Rs%Sb/100.0 ) * Ctrl%Rs%B(i,k) ) * &
                               ( ( Ctrl%Rs%Sb/100.0 ) * Ctrl%Rs%B(i,k) ) / &
                                   Ctrl%Ind%NSolar

!               Off-diagonal elements (correlation between channels from Ctrl%Rs%Cb)

                else

                    SRs(i,j) = ( ( Ctrl%Rs%Sb/100.0 ) * Ctrl%Rs%B(i,k) ) * &
                               ( ( Ctrl%Rs%Sb/100.0 ) * Ctrl%Rs%B(j,k) ) * &
                                   Ctrl%Rs%Cb / Ctrl%Ind%NSolar

                end if

            end do

        end do

!   SAD method

    else if (Ctrl%Rs%Flag = 2) then

!       Set Rs
        
        Rs  = SAD_Chan(Ctrl%Ind%SolarFirst:Ctrl%Ind%SolarLast)%Rs(k)

!       Calculate the covariance matrix SRs

        do i = Ctrl%Ind%SolarFirst, Ctrl%Ind%SolarLast

            do j = Ctrl%Ind%SolarFirst, Ctrl%Ind%SolarLast

!               On-diagonal elements (fixed 10% error in each channel)

                if (i == j) then

                    SRs(i,j) = ( 0.1 * SAD_Chan(i)%Rs(k) ) * &
                               ( 0.1 * SAD_Chan(i)%Rs(k) ) / &
                                       Ctrl%Ind%NSolar

!               Off-diagonal elements (fixed 80% correlation between channels)

                else

                    SRs(i,j) = ( 0.1 * SAD_Chan(i)%Rs(k) ) * &
                               ( 0.1 * SAD_Chan(j)%Rs(k) ) * &
                                 0.8 / Ctrl%Ind%NSolar

                end if

            end do

        end do

!   MDAD method (not supported in ECP delivery)

    else if (Ctrl%Rs%Flag = 3) then

        status = 1

!   SDAD method (not supported in ECP delivery)

    else if (Ctrl%Rs%Flag = 4) then

        status = 1

!   AUX method (not supported in ECP delivery)

    else if (Ctrl%Rs%Flag = 5) then

        status = 1

!   No method defined - return non-zero error flag

    else

        status = 1

    end if

end subroutine Get_Surface_Rs
