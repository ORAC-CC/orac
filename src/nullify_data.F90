!-------------------------------------------------------------------------------
! Name: nullify_data.F90
!
! Purpose:
! Nullify all pointers in Ctrl and MSI_Data structures.
!
! Description and Algorithm details:
! Many calls to nullify.
!
! Arguments:
! Name     Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! Ctrl     struct In          Control structure
! MSI_Data struct Both        Imager data structure
!
! History:
! 2014/09/10, GM: Original version
! 2015/12/19, GM: Added nullifications for recently added variables.
! 2015/01/30, AP: Remove uscan and vscan as unnecessary.
! 2015/07/03, OS: Added cldmask_uncertainty
! 2015/08/10, AP: Additional surface uncs.
! 2017/06/21, OS: Added ANN phase variables.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine Nullify_Data(Ctrl, MSI_Data)

   use Ctrl_m

   implicit none

   ! Declare arguments
   type(Ctrl_t), intent(in)    :: Ctrl
   type(Data_t), intent(inout) :: MSI_Data

   nullify(MSI_Data%ALB)

   if (Ctrl%RS%read_full_brdf) then
      nullify(MSI_Data%rho_0v)
      nullify(MSI_Data%rho_0d)
      nullify(MSI_Data%rho_dv)
      nullify(MSI_Data%rho_dd)
   end if

   nullify(MSI_Data%Type)
   nullify(MSI_Data%cldtype)
   nullify(MSI_Data%cldmask)
   nullify(MSI_Data%cldmask_uncertainty)
   nullify(MSI_Data%cccot_pre)
   nullify(MSI_Data%ann_phase)
   nullify(MSI_Data%ann_phase_uncertainty)
   nullify(MSI_Data%cphcot)

   nullify(MSI_Data%Geometry%Sol)
   nullify(MSI_Data%Geometry%Sat)
   nullify(MSI_Data%Geometry%Azi)
   nullify(MSI_Data%Geometry%Saz)

   nullify(MSI_Data%Location%Lat)
   nullify(MSI_Data%Location%Lon)

   nullify(MSI_Data%LSFlags)

   nullify(MSI_Data%lusflags)
   nullify(MSI_Data%dem)
   nullify(MSI_Data%nisemask)

   nullify(MSI_Data%time)

   nullify(MSI_Data%MSI)
   nullify(MSI_Data%SD)
   nullify(MSI_Data%cal_gain)
   nullify(MSI_Data%rho_dd_cor)
   nullify(MSI_Data%rho_dd_unc)
   nullify(MSI_Data%svd_unc)
   nullify(MSI_Data%veg_unc)
   nullify(MSI_Data%bare_unc)
   nullify(MSI_Data%snow_unc)

   nullify(MSI_Data%illum)
   nullify(MSI_Data%State%CTP)
   nullify(MSI_Data%State%CTP_var)

end subroutine Nullify_Data
