! Name:
!    Get_SwRTM
!
! Purpose:
!    Performs the linear interpolation/extrapolation of the short wave RTM data to the current
!    super pixel coordinates.
!    Also assigns the surface to TOA transmittances for the current super pixel.
!
! Arguments:
!    Name     Type      In/Out   Description
!    Ctrl     struct    In	 Control structure
!    RTM      struct    In       RTM structure
!    SPixel   struct    Both	 Super-pixel structure
!    status   integer   Out	 Error status
!    
! Algorithm:
!    Decide if the super pixel latitude lies above, below, or between latitude band centres
!    and determine d_X and indices accordingly.
!    Use the ratio of d_X and the latitude band centre spacing to interpolate
!    (use the same interpolation calculation for all three of above cases).
!     Assign the surface to TOA transmittances.
!
! Local variables:
!    Name      Type      Description
!    X         real      Super pixel latitude relative to grid origin
!    Nx        int       RTM data index (latitude) of the preceeding data point
!    Nx1       int       RTM data index (latitude) of the following data point
!    XN        real      The latitude at data point index Nx
!    d_X       real      The difference between X and NX
!    t         real      The ratio of d_X and the RTM data latitude grid spacing
!    i         int       Loop counter (over pressure levels)
!    indicies  int arr   Indices of latitude bands used in interpolation/extrapolation 
!
! History:
!   12th  January, 2001, Kevin M. Smith : original version
!    1st February, 2001, KMS: Added variable t, ratio of d_X and lat band spacing to
!                             avoid repeating calculation. 
!    8th March 2001, Andy Smith:
!      Order of arguments changed (inputs first) for consistency with LW 
!      routine. Added intent.
!    1st April 2001, Andy Smith:
!      Using whole array operations in the interpolations across all pressure
!      levels and channels to improve performance.
!
!
! Bugs:
!   None known.
!
!------------------------------------------------------------------------------------
subroutine Get_SwRTM(Ctrl, RTM, SPixel, status)

    use Ctrl_def
    use SPixel_def
    use RTM_def

    implicit none

!   Declare arguments

    type(Ctrl_t), intent(in)       :: Ctrl
    type(RTM_t), intent(in)        :: RTM
    type(SPixel_t), intent(inout)  :: SPixel
    integer, intent(out)           :: status

!   Declare local variables

    real    :: X
    integer :: Nx
    integer :: Nx1
    real    :: XN
    real    :: d_X
    real    :: t
    integer :: i
    integer :: indices(2)

!   Set status to zero

    status = 0
    
!   Linear interpolation/extrapolation - latitude only

!   Case 1 - super pixel lies below first latitude band centre (extrapolation)

    if (abs(SPixel%Loc%Lat) < RTM%SW%Grid%Lat0) then
    
       d_X = ( abs(SPixel%Loc%Lat) - RTM%SW%Grid%Lat0 )

       indices(1) = 1
       indices(2) = 2

!   Case 2 - super pixel lies above last band centre (extrapolation)   
    
    else if (abs(SPixel%Loc%Lat) > RTM%SW%Grid%LatN) then
    
       d_X = ( abs(SPixel%Loc%Lat) - RTM%SW%Grid%LatN ) + RTM%SW%Grid%delta_Lat

       indices(1) = RTM%SW%Grid%NLat-1
       indices(2) = RTM%SW%Grid%NLat
    
!   Case 3 - super pixel lies between band centres (interpolation)

    else
    
       X  = abs(SPixel%Loc%Lat) - RTM%SW%Grid%Lat0
       Nx = 1+ int( X * RTM%SW%Grid%inv_delta_Lat )
       XN = (Nx-1) * RTM%SW%Grid%delta_Lat + RTM%SW%Grid%Lat0
       d_X = abs(SPixel%Loc%Lat) - XN
       
       indices(1) = Nx
       indices(2) = Nx+1
    
    end if

! Ratio of d_X and latitude band centre spacing

    t = d_X / RTM%SW%Grid%delta_Lat

! Loop over pressure levels (and implicitly over all channels) 
! (removed for efficiency)       
!    do i = 1, RTM%SW%NP
!       
!   Tac       
       
    SPixel%RTM%SW%Tac = RTM%SW%Tac(indices(1),:,:)  + &
                     ( (RTM%SW%Tac(indices(2),:,:)  - &
			RTM%SW%Tac(indices(1),:,:)) * t )      
!   Tbc

    SPixel%RTM%SW%Tbc = RTM%SW%Tac(indices(1),:,:)  + &
                     ( (RTM%SW%Tbc(indices(2),:,:)  - &
			RTM%SW%Tac(indices(1),:,:)) * t )		      		         
!
!    end do
!    
!   Calculate the surface level to TOA transmittances

    SPixel%RTM%SW%Tsf = SPixel%RTM%SW%Tac(:,RTM%SW%Np)    

end subroutine Get_SwRTM
