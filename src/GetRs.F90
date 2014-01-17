! Name:
!    Get_Rs
!
! Purpose:
!    Calculates the super pixel mean surface reflectivities (and covariances)
!    for the solar channels according to the specified averaging method Ctrl%Resoln%Ameth
!
! Arguments:
!    Name        Type           In/Out   Description
!    Ctrl        struct         In       Control structure
!    SPixel      alloc struct   Both     Super pixel structure
!    SPixel_b    real array     In       Super pixel array of b (using super pixel surface flags)
!    SPixel_Sb   real array     In       Super pixel array of Sb (using super pixel surface flags)   
!    status      integer        Out      Error status
!    
! Algorithm:
!    Average method:
!       'All' - average super pixel surface reflectance array values (and covariances)
!               over all good pixels,            
!       'Cloudy' - average over good cloudy pixels,
!       'Central' - take values from the central pixel, if it is good.
!
! Local variables:
!    Name      Type   Description
!    i, j      int    Loop counters
!    message   char   Message to write to log file
!
! History:
!   4th December, 2000, Kevin M. Smith : original version
!  16th January,  2001,  KMS : Re-drafted
!  18th January,  2001,  KMS : Modified to take array of reflectances etc from SPixel%Surface
!                            : Changed name from Get_Surface_Rs to Get_Rs
!   16th Mar 2001, Andy Smith:
!      Removed checking for invalid averaging method. 
!      Using named constants for averaging method.
!   15th Jun 2001, Andy Smith:
!      Changed error message string assignments/writes. 
!      Long message strings were wrapped over two lines with only one set of 
!      quotes around the whole string (including the line continuation marker). 
!      Original code works on DEC but not Linux.
!   *************************** ECV work starts here *********************
!   23rd Feb 2011, Andy Smith:
!      Cloud flags converted to real to match current ORAC data. 
!   30th Mar 2011, Andy Smith:
!      Removal of super-pixel averaging. Process single pixels at X0, Y0, 
!      removed other SPixel indices Xc, Yc, Xn, Yn etc. 
!      Removed selection of averaging method. 
!      SPixel_B and SPixel_Sb re-dimensioned to remove SPixel size. 
!20th Jan 2012 general tidyup remove data varaible 
! Bugs:
!   None known.
!
!------------------------------------------------------------------------------
subroutine Get_Rs(Ctrl, SPixel, SPixel_b, SPixel_Sb, status)
   
    use CTRL_def
    use Data_def
    use SPixel_def

    implicit none

!   Define arguments

    type(CTRL_t)   :: Ctrl
     type(SPixel_t) :: SPixel

    real           :: SPixel_b( Ctrl%Ind%NSolar)
    real           :: SPixel_Sb(Ctrl%Ind%NSolar, Ctrl%Ind%NSolar)
    integer :: status
    
!   Define local variables

    character(180) :: message

!   Set status to zero

   status = 0

    
!   Check mask to see if pixel is 'good'    

   if (SPixel%Mask == 1) then
   
      SPixel%Rs(:)    = SPixel_b( :)
      SPixel%SRs(:,:) = SPixel_Sb(:, :)	  
      
      if (SPixel%Surface%Flags == 1) then 
         SPixel%Surface%Land = 1
         SPixel%Surface%Sea  = 0
      else
         SPixel%Surface%Land = 0
         SPixel%Surface%Sea  = 1
      end if	  
               
   else
      status = GetRsCentPix
      write(unit=message, fmt=*) 'Get_Rs: pixel contains bad data'
      call Write_log(Ctrl, trim(message), status) 
   end if

end subroutine Get_Rs
