! Name:
!    Get_LwSwRTM
!
! Purpose:
!    Performs the bilinear interpolation of the long wave RTM data to the
!    current super pixel coordinates.
!    Also assigns the surface to TOA transmittances for the current super pixel.
!
! Arguments:
!    Name     Type           In/Out   Description
!    Ctrl     struct         In       Control structure
!    SAD_Chan struct array   In       Channel description structures
!    RTM      alloc struct   In       RTM structure
!    SPixel   struct         Both     Super-pixel structure
!    status   integer        Out      Error status
!    
! Algorithm:
!    Method taken directly from Numerical Recipes p96, 1987.
!
! Local variables:
!    Name      Type       Description
!    X         real       Super pixel latitude relative to grid origin
!    Nx        int        RTM data index (latitude) of the preceeding data point
!    Nx1       int        RTM data index (latitude) of the following data point
!    XN        real       The latitude at data point index Nx
!    d_X       real       The difference between X and NX
!    t         real       The ratio of d_X and the RTM data latitude grid spacing
!    Y         real       Super pixel longitude relative to grid origin
!    Ny        int        RTM data index (longitude) of the preceeding data point
!    Ny1       int        RTM data index (longitude) of the following data point
!    YN        real       The longitude at data point index Ny
!    d_Y       real       The difference between Y and NY
!    u         real       The ratio of d_Y and the RTM data longitude grid spacing
!    coeff     real arr   Interpolation coefficients
!    T_Array   real arr   Array (nThermal channels) of temperatures for input to
!                         T2R for dB_dT2 calculation.   
!    R         real arr   Array of radiances returned by T2R (values not kept)
!    i         int        Loop counter (over pressure levels)
!    message   char       Message to write to log file
!
! History:
!   12th  January, 2001, Kevin M. Smith : original version.
!   25th  January, 2001, KMS : Added checks to see if super pixel lat,lon are
!                              covered by the RTM ranges.
!                              Simplified calculation of XN and YN
!   21st Feb 2001, Andy Smith: 
!       Added Tbc to LW structure. Previously missing from model data.
!       Tsf removed from LW structure.
!    1st Mar 2001, Andy Smith: 
!       Changed setting of R_Clear values. These are not present in the 
!       RTM struct as they are not read from the RTM data file.
!    6th Mar 2001, Andy Smith: 
!       Tsf required in RTM struct again.
!    8th Mar 2001, Andy Smith: 
!       Now sets dB_dTs in LW sub-struct.
!       Added SAD_Chan argument (required by T2R for dB_dTs calculation).
!   30th Mar 2001, Andy Smith: 
!       Using whole array operations in the interpolations across all pressure
!       levels and channels to improve performance.
!       Bs calculation removed. Bs is not used by subsequent code.
!   20th July 2001, Andy Smith:
!       dB_dTs calculation fixed: was using no. of thermal channels from Spixel,
!       should have used Ctrl values as the RTM Lw arrays are all allocated to 
!       size Ctrl%Ind%NThermal and left at this size for the whole image. 
!    ******** ECV work starts here **********************
!   21st Feb 2011, Andy Smith:
!       Re-introducing changes made in late 2001/2002.
!   12th December 2003, Caroline Poulsen: added geopotential height    
!   22nd Sept 2011, Caroline Poulsen: added in swrtm variables     
!   7th November 2011, C. Poulsen: tidied up comments no actual change
!   15/09/2012, M. Stengel: puts in coeffs fudge to account for missing values
!   03/11/2012, MJ: changed coeffs if block
!   23/07/2014, AP: Grid no longer assumed to defined points rather than the
!       cells centres (as is actually the case).
!   23/07/2014 CP: added in extrapolation into stratosphere
!
! Bugs:
!   None known.
!
! $Id$
!
!-------------------------------------------------------------------------------
subroutine Get_LwSwRTM(Ctrl, SAD_Chan, RTM, SPixel, status)

    use Ctrl_def
    use SAD_Chan_def
    use SPixel_def
    use RTM_def
    use ECP_Constants

    implicit none

!   Declare arguments

    type(Ctrl_t), intent(in)         :: Ctrl
    type(SAD_Chan_t), intent(in)     :: SAD_Chan(Ctrl%Ind%Ny)
    type(RTM_t), intent(in)          :: RTM
    type(SPixel_t), intent(inout)    :: SPixel
    integer, intent(out)             :: status

!   Declare local variables

    real           :: X
    integer        :: Nx
    integer        :: Nx1
    real           :: XN
    real           :: d_X
    real           :: t
    real           :: Y
    integer        :: Ny
    integer        :: Ny1
    real           :: YN
    real           :: d_Y
    real           :: u
    real           :: coeff(4)
    real           :: R(Ctrl%Ind%NThermal)
    real           :: T_Array(Ctrl%Ind%NThermal)
    character(180) :: message

!   Set status to zero

    status = 0
    
!   Bilinear interpolation (method taken from Numerical Recipes p96, 1987)

!   Latitude


    X   = SPixel%Loc%Lat - RTM%LW%Grid%Lat0        ! Latitude relative to grid origin
    Nx  = 1 + int( X * RTM%LW%Grid%inv_delta_Lat + 0.5 ) ! Integer number of grid points from origin to X
    XN  = RTM%LW%Lat(Nx,1)                         ! Latitude at Nx grid points
    d_X = SPixel%Loc%Lat - XN                      ! Latitude relative to XN
    t   = d_X * RTM%LW%Grid%inv_delta_Lat          ! Ratio of d_X and latitude grid spacing 

!   Longitude
     Y   = SPixel%Loc%Lon - RTM%LW%Grid%Lon0        ! Longitude relative to grid origin
    Ny  = 1 + int( Y * RTM%LW%Grid%inv_delta_Lon + 0.5 ) ! Integer number of grid points from origin to Y
    YN  = RTM%LW%Lon(Nx,Ny)                        ! Longitude at Ny grid points
    d_Y = SPixel%Loc%Lon - YN                      ! Longitude relative to YN
    u   = d_Y * RTM%LW%Grid%inv_delta_Lon          ! Ratio of d_Y and longitude grid spacing

!   Calculate coordinates used in all interpolation

    Nx1 = Nx + 1
    Ny1 = Ny + 1

    
!   Check that required coordinates are within range (skip super pixel if not)   

    if (Nx1 > RTM%LW%Grid%NLat) then
       if (Nx == RTM%LW%Grid%NLat) then
          ! if in last grid cell, extrapolate
          Nx1 = Nx
          Nx = Nx-1
       else
          status = GetLwSwRTMLat
          write(unit=message, fmt=*) 'Get_LwSwRTM: Latitude outside RTM coverage in super pixel starting at:', &
               SPixel%Loc%X0, SPixel%Loc%Y0
          call Write_log(Ctrl, trim(message), status)
       end if
    end if

    if (Ny1 > RTM%LW%Grid%NLon) then
       if (Ny == RTM%LW%Grid%NLon) then
          ! if in last grid cell, extrapolate
          Ny1 = Ny
          Ny = Ny-1
       else
          status = GetLwSwRTMLon
          write(unit=message, fmt=*) 'Get_LwSwRTM: Longitude outside RTM coverage in super pixel starting at:', &
               SPixel%Loc%X0, SPixel%Loc%Y0
          call Write_log(Ctrl, trim(message), status)
       end if
    end if    
    
    if (status == 0) then
     
!      Calculate coefficients used in all interpolations

       coeff(1) = (1-t) * (1-u)
       coeff(2) =    t  * (1-u)
       coeff(3) =    t  *    u
       coeff(4) = (1-t) *    u
    
!      Parameters that are dependent on channel only


!MST Setting those coeffs zero which are associated with empty boxes
       !MJ if (RTM%LW%Rac_up(Nx,  Ny,  1, 1) .lt. 0. .or. RTM%LW%Rac_up(Nx,  Ny,  1, 1) .lt. 0.   .or. RTM%LW%Rac_up(Nx1, Ny1, 1, 1) .lt. 0. .or. RTM%LW%Rac_up(Nx, Ny1, 1, 1) .lt. 0.) then
          if(RTM%LW%Rac_up(Nx,  Ny,  1, 1) .lt. 0.) coeff(1)=0. 
          if(RTM%LW%Rac_up(Nx1, Ny,  1, 1) .lt. 0.) coeff(2)=0.
          if(RTM%LW%Rac_up(Nx1, Ny1, 1, 1) .lt. 0.) coeff(3)=0.
          if(RTM%LW%Rac_up(Nx,  Ny1, 1, 1) .lt. 0.) coeff(4)=0.
          ! and now rescale the coeffs to 1.
!          write(*,*)'missing value before', coeff
          coeff(1:4)=coeff(1:4)*1./sum(coeff(1:4))
!          write(*,*)'missing value after', coeff
          !MJendif
       
!      ems
       
       SPixel%RTM%LW%ems = ( coeff(1) * RTM%LW%ems(Nx,  Ny,  :) ) + &
                           ( coeff(2) * RTM%LW%ems(Nx1, Ny,  :) ) + &
                           ( coeff(3) * RTM%LW%ems(Nx1, Ny1, :) ) + &
			   ( coeff(4) * RTM%LW%ems(Nx,  Ny1, :) )
			   
			      
!      Parameters that are dependent on pressure level			      
!			      

!      Note that "array    = ..." is faster than
!                "array(:) = ..."
!      Tbc       
       
       SPixel%RTM%LW%Tbc = ( coeff(1) * RTM%LW%Tbc(Nx,  Ny,  :, :) ) + &
                           ( coeff(2) * RTM%LW%Tbc(Nx1, Ny,  :, :) ) + &
                           ( coeff(3) * RTM%LW%Tbc(Nx1, Ny1, :, :) ) + &
			   ( coeff(4) * RTM%LW%Tbc(Nx,  Ny1, :, :) )
			      
!      Tac       

       SPixel%RTM%LW%Tac = ( coeff(1) * RTM%LW%Tac(Nx,  Ny,  :, :) ) + &
                           ( coeff(2) * RTM%LW%Tac(Nx1, Ny,  :, :) ) + &
                           ( coeff(3) * RTM%LW%Tac(Nx1, Ny1, :, :) ) + &
			   ( coeff(4) * RTM%LW%Tac(Nx,  Ny1, :, :) )


!      SW Tbc       
       
       SPixel%RTM%SW%Tbc = ( coeff(1) * RTM%SW%Tbc(Nx,  Ny,  :, :) ) + &
                           ( coeff(2) * RTM%SW%Tbc(Nx1, Ny,  :, :) ) + &
                           ( coeff(3) * RTM%SW%Tbc(Nx1, Ny1, :, :) ) + &
			   ( coeff(4) * RTM%SW%Tbc(Nx,  Ny1, :, :) )


!      SW Tac       
       
       SPixel%RTM%SW%Tac = ( coeff(1) * RTM%SW%Tac(Nx,  Ny,  :, :) ) + &
                           ( coeff(2) * RTM%SW%Tac(Nx1, Ny,  :, :) ) + &
                           ( coeff(3) * RTM%SW%Tac(Nx1, Ny1, :, :) ) + &
			   ( coeff(4) * RTM%SW%Tac(Nx,  Ny1, :, :) )

!   Calculate the surface level to TOA transmittances

    SPixel%RTM%SW%Tsf = SPixel%RTM%SW%Tac(:,RTM%SW%Np)    

!      P


       SPixel%RTM%SW%P = ( coeff(1) * RTM%LW%P(Nx,  Ny,  :) ) + &
                         ( coeff(2) * RTM%LW%P(Nx1, Ny,  :) ) + &
                         ( coeff(3) * RTM%LW%P(Nx1, Ny1, :) ) + &
			 ( coeff(4) * RTM%LW%P(Nx,  Ny1, :) )


!      Rac_up

       SPixel%RTM%LW%Rac_up = ( coeff(1) * RTM%LW%Rac_up(Nx,  Ny,  :, :) ) + &
                              ( coeff(2) * RTM%LW%Rac_up(Nx1, Ny,  :, :) ) + &
                              ( coeff(3) * RTM%LW%Rac_up(Nx1, Ny1, :, :) ) + &
			      ( coeff(4) * RTM%LW%Rac_up(Nx,  Ny1, :, :) )
				 
!      Rac_dwn

       SPixel%RTM%LW%Rac_dwn = ( coeff(1) * RTM%LW%Rac_dwn(Nx,  Ny,  :, :) ) + &
                               ( coeff(2) * RTM%LW%Rac_dwn(Nx1, Ny,  :, :) ) + &
                               ( coeff(3) * RTM%LW%Rac_dwn(Nx1, Ny1, :, :) ) + &
			       ( coeff(4) * RTM%LW%Rac_dwn(Nx,  Ny1, :, :) )
				       
!      Rbc_up

       SPixel%RTM%LW%Rbc_up = ( coeff(1) * RTM%LW%Rbc_up(Nx,  Ny,  :, :) ) + &
                              ( coeff(2) * RTM%LW%Rbc_up(Nx1, Ny,  :, :) ) + &
                              ( coeff(3) * RTM%LW%Rbc_up(Nx1, Ny1, :, :) ) + &
			      ( coeff(4) * RTM%LW%Rbc_up(Nx,  Ny1, :, :) )				       				  

!      P

       SPixel%RTM%LW%P = ( coeff(1) * RTM%LW%P(Nx,  Ny,  :) ) + &
                         ( coeff(2) * RTM%LW%P(Nx1, Ny,  :) ) + &
                         ( coeff(3) * RTM%LW%P(Nx1, Ny1, :) ) + &
			 ( coeff(4) * RTM%LW%P(Nx,  Ny1, :) )



!      T

       SPixel%RTM%LW%T = ( coeff(1) * RTM%LW%T(Nx,  Ny,  :) ) + &
                         ( coeff(2) * RTM%LW%T(Nx1, Ny,  :) ) + &
                         ( coeff(3) * RTM%LW%T(Nx1, Ny1, :) ) + &
			 ( coeff(4) * RTM%LW%T(Nx,  Ny1, :) )	

!
!modify profile in boundary layer inversion not implemented yet
!	
!	call Blmodification(SPixel)

!
!extrapolate temperature profile into stratosphere to deal with deep convective clouds primarily in tropics that push through the trop.
!
        call extrap_into_tropopause(Spixel)

!      skint

       SPixel%RTM%LW%skint = ( coeff(1) * RTM%LW%skint(Nx,  Ny) ) + &
                         ( coeff(2) * RTM%LW%skint(Nx1, Ny) ) + &
                         ( coeff(3) * RTM%LW%skint(Nx1, Ny1) ) + &
			 ( coeff(4) * RTM%LW%skint(Nx,  Ny1) )	

!      surface pressure

       SPixel%RTM%LW%sp = ( coeff(1) * RTM%LW%sp(Nx,  Ny) ) + &
                         ( coeff(2) * RTM%LW%sp(Nx1, Ny) ) + &
                         ( coeff(3) * RTM%LW%sp(Nx1, Ny1) ) + &
			 ( coeff(4) * RTM%LW%sp(Nx,  Ny1) )	

!      H


       SPixel%RTM%LW%H = ( coeff(1) * RTM%LW%H(Nx,  Ny,  :) ) + &
                         ( coeff(2) * RTM%LW%H(Nx1, Ny,  :) ) + &
                         ( coeff(3) * RTM%LW%H(Nx1, Ny1, :) ) + &
			 ( coeff(4) * RTM%LW%H(Nx,  Ny1, :) )


!
!       end do
!       
    end if
    
!  Calculate the surface level to TOA transmittances

   SPixel%RTM%LW%Tsf = SPixel%RTM%LW%Tac(:,RTM%LW%Np)
			      
!  Set R_Clear using Rbc_up at the TOA

   SPixel%RTM%LW%R_clear = SPixel%RTM%LW%Rbc_up(:,1)

!  Set dB_dTs using the surface temperature. (T2R needs an array of T values,
!  one per channel, to convert).

   T_Array = SPixel%RTM%LW%T(SPixel%RTM%LW%Np)
   call T2R (Ctrl%Ind%NThermal, &
      SAD_Chan(Ctrl%Ind%ThermalFirst:Ctrl%Ind%ThermalLast), & 
      T_Array, R, SPixel%RTM%LW%dB_dTs, status)

end subroutine Get_LwSwRTM
