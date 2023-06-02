!-------------------------------------------------------------------------------
! Name: read_ctp.F90
!
! Purpose:
! Controls the reading a priori cloud-top pressure values from an auxiliary data
! file into the MSI_Data%State structure.
!
! Description and Algorithm details:
! if (MSI files are not yet open)
!    Open CTP file
!    If open error
!       Write error message to screen and log file
!    else
!      allocate MSI image segment array in Data_MSI struct.
!
! If (no error opening files)
!     Read header (not used further)
!     If read error
!        Write error message to screen and log file
!     Else
!        Read byte array of size defined by Ctrl structure
!        If read error
!           Write error message to log file
! Close CTP file
!
! Arguments:
! Name     Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! Ctrl     struct Both        Control structure
! MSI_Data struct Both        Data structure: contains the cloud flag array
!                             to be populated with data from the file. This
!                             is overwritten as successive segments of data
!                             are read in.
!
! History:
! 2022/01/22, GT: New, based off of existing Read routines.
! 
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine Read_CTP(Ctrl, MSI_Data)

   use Ctrl_m
   use orac_ncdf_m

   implicit none

   ! Argument declarations

   type(Ctrl_t), intent(in)    :: Ctrl
   type(Data_t), intent(inout) :: MSI_Data

   integer :: ncid

   ! Open CTP file
   if (Ctrl%verbose) write(*,*) 'CTP file: ', trim(Ctrl%FID%CTP)
   call ncdf_open(ncid, Ctrl%FID%CTP, 'Read_CTP_nc()')

   allocate(MSI_Data%State%CTP(Ctrl%Ind%Xmax, Ctrl%Ind%Ymax))
   allocate(MSI_Data%State%CTP_var(Ctrl%Ind%Xmax, Ctrl%Ind%Ymax))

   call ncdf_read_array(ncid, "ctp", MSI_Data%State%CTP)
   call ncdf_read_array(ncid, "ctp_var", MSI_Data%State%CTP_var)

   ! Close CTP file
   call ncdf_close(ncid, 'read_location_nc()')

end subroutine Read_CTP
