!-------------------------------------------------------------------------------
! Name: sabotage_inputs.F90
!
! Purpose:
! Purposefully remove data from inputs to sample all possible input cases.
!
! Description and Algorithm details:
! Start from the beginning of the scene and deliberately alter pixels so that
! all possible combinations of day, twilight, night, and missing channels has
! been achieved [2^(3 + Ctrl%Ind%Ny) pixels)].  Pixels with any missing
! channels are skipped.
!
! Arguments:
! Name     Type         In/Out/Both Description
! ------------------------------------------------------------------------------
! Ctrl     struct       Both        Control structure (date is read in here).
! MSI_Data struct       Both        Data structure: the MSI data part of this
!                                   struct is populated by this routine, and
!                                   is overwritten on successive calls.
!
! History:
! 2015/02/04, GM: Original version.
! 2015/09/07, AP: Allow verbose to be controlled from the driver file.
! 2015/01/01, GM: Base illumination settings on Ctrl%MaxSolZen and Ctrl%Sunset.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine sabotage_inputs(Ctrl, MSI_Data)

   use CTRL_def

   implicit none

   ! Define arguments

   type(CTRL_t), intent(in)    :: Ctrl
   type(Data_t), intent(inout) :: MSI_Data

   ! Local variables

   integer            :: i,j,jj,kk
   integer            :: count
   integer            :: missing_mask
   integer            :: i_illum
   integer, parameter :: n_illums = 3
   integer, parameter :: illum_list(n_illums) = (/IDay, ITwi, INight/)

   count = Ctrl%Ind%Ymax * Ctrl%Ind%Xmax

   j = 1
   do i_illum = 1, 3
      missing_mask = 0
      do while (j .le. count)
         jj = (j - 1) / Ctrl%Ind%Xmax + 1
         kk = mod(j - 1, Ctrl%Ind%Xmax) + 1

         if (.not. any(MSI_Data%MSI(kk,jj,:) .eq. sreal_fill_value)) then
            select case (illum_list(i_illum))
               case (IDay)
                  MSI_Data%Geometry%Sol(kk,jj,:) = Ctrl%MaxSolZen - 1.
               case (ITwi)
                  MSI_Data%Geometry%Sol(kk,jj,:) = (Ctrl%MaxSolZen + &
                                                    Ctrl%Sunset) / 2.
               case (INight)
                  MSI_Data%Geometry%Sol(kk,jj,:) = Ctrl%Sunset + 1.
            end select

            do i = 1, Ctrl%Ind%Ny
               if (btest(missing_mask, i - 1)) then
                  MSI_Data%MSI(kk,jj,i) = sreal_fill_value
               end if
            end do

            missing_mask = missing_mask + 1

            if (missing_mask .eq. 2**Ctrl%Ind%Ny) then
               goto 666
            end if
         end if

         j = j + 1
      end do

      666 continue
   end do

   if (missing_mask .ne. 2**Ctrl%Ind%Ny) then
      write(*,*) 'ERROR: sabotage_inputs(): Not enough pixels to sabotage'
      stop error_stop_code
   end if

end subroutine sabotage_inputs
