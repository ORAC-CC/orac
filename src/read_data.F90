!-------------------------------------------------------------------------------
! Name: read_data.F90
!
! Purpose:
! Controls the reading of Multi Spectral Image (ALB) values from ATSR-type
! files into the DATA_ALB array.
!
! Description and Algorithm details:
! Open everything in order, using dedicated routines for each.
!
! Arguments:
! Name     Type         In/Out/Both Description
! ------------------------------------------------------------------------------
! Ctrl     struct       Both        Control structure (date is read in here).
! MSI_Data struct       Both        Data structure: the ALB data part of this
!                                   struct is populated by this routine, and
!                                   is overwritten on successive calls.
! SAD_Chan struct array Both        Instrument channel parameters. Updated by
!                                   this routine: solar constant is
!                                   modified from annual average to value for
!                                   the day of the ALB data.
!
! History:
! 2012/08/16, MJ: Original version.
! 2013/02/26, CP: added in header and remove hardwiring of MSI_Data%ALB=3
! 2014/04/20, GM: Cleaned up the code.
! 2014/08/18, AP: Commented out reading of scanline data as it is not actually
!    used.
! 2014/04/20, GM: Added call to Nullify_Data() as some pointers may not be
!    associated.
! 2015/02/04, GM: Changes related to the new missing channel, illumination, and
!    channel selection code.
! 2015/09/07, AP: Allow verbose to be controlled from the driver file.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine Read_Data(Ctrl, MSI_Data, SAD_Chan)

   use Ctrl_m
   use SAD_Chan_m

   implicit none

   ! Define arguments

   type(Ctrl_t),     intent(inout) :: Ctrl
   type(Data_t),     intent(inout) :: MSI_Data
   type(SAD_Chan_t), intent(inout) :: SAD_Chan(:)

   ! Nullify pointers in case some are not associated.
   call Nullify_Data(Ctrl, MSI_Data)

   ! Define local variables

   ! Call appropriate satellite data reading routines
   ! (Sections to be added as reading routines become available

   if (Ctrl%RS%RsSelm == SelmAux .and. Ctrl%Ind%NSolar > 0) then
      if (Ctrl%verbose) write(*,*) 'Reading Albedo data'
      call Read_ALB(Ctrl, MSI_Data)
   end if

   if (Ctrl%verbose) write(*,*) 'Reading Cloud Flag data'
   call Read_CloudFlags(Ctrl, MSI_Data)

   if (Ctrl%verbose) write(*,*) 'Reading Geometry data'
   call Read_Geometry(Ctrl, MSI_Data)

   if (Ctrl%verbose) write(*,*) 'Reading Location data'
   call Read_Location(Ctrl, MSI_Data)

   if (Ctrl%verbose) write(*,*) 'Reading LS Flag data'
   call Read_LSFlags(Ctrl, MSI_Data)

   if (Ctrl%verbose) write(*,*) 'Reading MSI data'
   call Read_MSI(Ctrl, MSI_Data, SAD_Chan)

   if (Ctrl%sabotage_inputs) then
      if (Ctrl%verbose) write(*,*) 'Sabatoging input data'
      call sabotage_inputs(Ctrl, MSI_Data)
   end if

   if (Ctrl%verbose) write(*,*) 'Determining Illumination data'
   call Determine_Illum(Ctrl, MSI_Data)

end subroutine Read_Data
