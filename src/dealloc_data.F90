!-------------------------------------------------------------------------------
! Name: dealloc_data.F90
!
! Purpose:
! Deallocate the MSI_Data arrays at end of ORAC execution.
!
! Description and Algorithm details:
! Deallocates all arrays in the MSI_Data structure
!
! Arguments:
! Name      Type         In/Out/Both Description
! ------------------------------------------------------------------------------
! Ctrl      struct       In          Control structure
! MSI_Data  alloc struct In          MSI Data structure
!
! History:
! 2001/10/24, AS: original version
!    **************** ECV work starts here *************************************
! 2011/02/22, AS: Re-introducing changes made in late 2001/2002.
! 2011/11/01, AS: Added test of allocation status before each array is
!    deallocated. If an error occurs on reading one or more of the MSI Data
!    arrays it's possible that not all arrays are associated (see Read_Sat_Data,
!    Read_ATSR_MSI etc).
! 2012/06/05, CP: Deallocate ALB data
! 2011/07/08, CP: Deallocate scan data
! 2013/12/16, GM: Add deallocation of MSI_Data%illum and a bit of cleanup.
! 2014/05/27, GM: Some cleanup.
! 2014/09/09, GM: Changes related to new BRDF support.
! 2014/10/24, OS: added deallocation of CldType, CloudMask, CCCOT_pre, LUSFlags,
!    DEM, nisemask
! 2015/01/30, AP: Remove uscan and vscan as unnecessary.
! 2015/04/28, AP: Added fields for surface uncertainty and correlation.
! 2017/06/21, OS: added ann phase variables
! 2018/06/08, SP: Add satellite azimuth angle to output.
! 2022/01/27, GT: Added CTP prior data.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine Dealloc_Data(Ctrl, MSI_Data)

   use Ctrl_m

   implicit none

   ! Declare arguments
   type(Ctrl_t), intent(in)    :: Ctrl
   type(Data_t), intent(inout) :: MSI_Data

   if (associated(MSI_Data%ALB))          deallocate(MSI_Data%ALB)

   if (Ctrl%RS%read_full_brdf) then
      if (associated(MSI_Data%rho_0v))    deallocate(MSI_Data%rho_0v)
      if (associated(MSI_Data%rho_0d))    deallocate(MSI_Data%rho_0d)
      if (associated(MSI_Data%rho_dv))    deallocate(MSI_Data%rho_dv)
      if (associated(MSI_Data%rho_dd))    deallocate(MSI_Data%rho_dd)
   end if

   if (associated(MSI_Data%Type))         deallocate(MSI_Data%Type)
   if (associated(MSI_Data%cldtype))      deallocate(MSI_Data%cldtype)
   if (associated(MSI_Data%cldmask))      deallocate(MSI_Data%cldmask)
   if (associated(MSI_Data%cldmask_uncertainty)) &
                                          deallocate(MSI_Data%cldmask_uncertainty)
   if (associated(MSI_Data%cccot_pre))    deallocate(MSI_Data%cccot_pre)
   if (associated(MSI_Data%ann_phase))    deallocate(MSI_Data%ann_phase)
   if (associated(MSI_Data%ann_phase_uncertainty)) &
                                          deallocate(MSI_Data%ann_phase_uncertainty)
   if (associated(MSI_Data%cphcot))       deallocate(MSI_Data%cphcot)

   if (associated(MSI_Data%Geometry%Sol)) deallocate(MSI_Data%Geometry%Sol)
   if (associated(MSI_Data%Geometry%Sat)) deallocate(MSI_Data%Geometry%Sat)
   if (associated(MSI_Data%Geometry%Azi)) deallocate(MSI_Data%Geometry%Azi)
   if (associated(MSI_Data%Geometry%Saz)) deallocate(MSI_Data%Geometry%Saz)

   if (associated(MSI_Data%Location%Lat)) deallocate(MSI_Data%Location%Lat)
   if (associated(MSI_Data%Location%Lon)) deallocate(MSI_Data%Location%Lon)

   if (associated(MSI_Data%LSFlags))      deallocate(MSI_Data%LSFlags)

   if (associated(MSI_Data%lusFlags))     deallocate(MSI_Data%lusFlags)
   if (associated(MSI_Data%dem))          deallocate(MSI_Data%dem)
   if (associated(MSI_Data%nisemask))     deallocate(MSI_Data%nisemask)

   if (associated(MSI_Data%time))         deallocate(MSI_Data%time)

   if (associated(MSI_Data%MSI))          deallocate(MSI_Data%MSI)
   if (associated(MSI_Data%SD))           deallocate(MSI_Data%SD)
   if (associated(MSI_Data%cal_gain))     deallocate(MSI_Data%cal_gain)
   if (associated(MSI_Data%rho_dd_cor))   deallocate(MSI_Data%rho_dd_cor)
   if (associated(MSI_Data%rho_dd_unc))   deallocate(MSI_Data%rho_dd_unc)
   if (associated(MSI_Data%svd_unc))      deallocate(MSI_Data%svd_unc)
   if (associated(MSI_Data%veg_unc))      deallocate(MSI_Data%veg_unc)
   if (associated(MSI_Data%bare_unc))     deallocate(MSI_Data%bare_unc)
   if (associated(MSI_Data%snow_unc))     deallocate(MSI_Data%snow_unc)

   if (associated(MSI_Data%illum))        deallocate(MSI_Data%illum)

   if (associated(MSI_Data%State%CTP))    deallocate(MSI_Data%State%CTP)
   if (associated(MSI_Data%State%CTP_var)) &
                                          deallocate(MSI_Data%State%CTP_var)

end subroutine Dealloc_Data
