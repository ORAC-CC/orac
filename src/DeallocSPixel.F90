! Name:
!    Dealloc_SPixel
!
! Purpose:
!    Deallocates the SPixel arrays to ensure that memory is freed once it
!    is no longer required.
!
! Arguments:
!    Name        Type           In/Out   Description
!    Ctrl        struct         In       Control structure
!    SPixel      alloc struct   Both     SPixel structure
!    status      int            Out      Error status
!    
! Algorithm:
!    Deallocates each of the arrays that is allocated by Alloc_SPixel.
!    At present, no check is made on deallocation status. What action 
!    should be taken if dealloc stat is non-zero? Checking and reporting an 
!    error on ever deallocation would make a very long routine. 
!
! Local variables:
!    Name   Type   Description
!    None
!
! History:
!   22nd Oct 2001, Andy Smith: Original version
!      ************** ECV work starts here ********************
!   21st Feb 2011, Andy Smith:
!     Re-applying changes from late 2001/2002.
!   12th December Caroline Poulsen added geopotential height
!   30th Mar 2011, Andy Smith:
!      Removal of super-pixelling, i.e. no averaging of flags etc needed. 
!      Any super-pixelling required will now be done in pre-processing. 
!      Resolution for the retrieval will be fixed at 1 pixel. 
!      No need to deallocate cloud or surface flags and mask.
!    22nd Sept 2011 Caroline Poulsen remove sw%p as now the same aslw%p
!    13th December 2011 Caroline Poulsen deallocated SPixel%Geom
!    arrays SPixel%SWRTM%P SPixel%ViewIdx
!   16th Jan 2014, Greg McGarrah:
!     Added deallocation of SPixel%spixel_y_to_ctrl_y_index.
!
! Bugs:
!   None known.
!
! $Id$
!
!------------------------------------------------------------------------------------
subroutine Dealloc_SPixel(Ctrl, SPixel, status)

   use Ctrl_def
   use SPixel_def

   implicit none
   
!  Declare arguments

   type(Ctrl_t), intent(in)      :: Ctrl
   type(SPixel_t), intent(inout) :: SPixel   
   integer, intent(inout)        :: status

!  Declare local variables (currently none used)
      
!  Quality control mask

!   deallocate(SPixel%Mask)

!  Get_CloudFlags arrays

!   deallocate(SPixel%Cloud%Flags)


  deallocate(SPixel%ViewIdx)

!  Get_RTM arrays

!  (Long wave)

   deallocate(SPixel%RTM%LW%Tac)
   deallocate(SPixel%RTM%LW%Tbc)
   deallocate(SPixel%RTM%LW%Tsf)   
   deallocate(SPixel%RTM%LW%Rac_up)
   deallocate(SPixel%RTM%LW%Rac_dwn)
   deallocate(SPixel%RTM%LW%Rbc_up)
   deallocate(SPixel%RTM%LW%R_clear)
   deallocate(SPixel%RTM%LW%Bs)     
   deallocate(SPixel%RTM%LW%dB_dTs)
   deallocate(SPixel%RTM%LW%Ems)    
   deallocate(SPixel%RTM%LW%T)      
   deallocate(SPixel%RTM%LW%P)      
   deallocate(SPixel%RTM%LW%H)      
               
! (Short wave)

   deallocate(SPixel%RTM%SW%Tac)                                 
   deallocate(SPixel%RTM%SW%Tbc)                                
   deallocate(SPixel%RTM%SW%Tsf) 
   deallocate(SPixel%RTM%SW%P) 
 
      
! deallocate overall RTM transmittances and reflectances

   deallocate(SPixel%RTM%Tsf_o)
   deallocate(SPixel%RTM%Tsf_v)
   deallocate(SPixel%RTM%Ref_clear)                                                                                 
   deallocate(SPixel%RTM%dRef_clear_dRs)                                                                                 

!  Get_Surface arrays

!   deallocate(SPixel%Surface%Flags)
   deallocate(SPixel%Rs)          
   deallocate(SPixel%SRs)               

!  Solar constant

   deallocate(SPixel%f0)

!  Super-pixel active and inactive state vectors, measurements and errors. 

   deallocate(Spixel%X)
   deallocate(Spixel%XI)
   deallocate(Spixel%Ym)
   deallocate(SPixel%Sy)


   deallocate(SPixel%illum)	

   deallocate(SPixel%Geom%SolZen)	
   deallocate(SPixel%Geom%SatZen)
   deallocate(SPixel%Geom%RelAzi)
   deallocate(SPixel%Geom%SEC_o)
   deallocate(SPixel%Geom%SEC_v)

   deallocate(SPixel%spixel_y_to_ctrl_y_index)

end subroutine Dealloc_SPixel
