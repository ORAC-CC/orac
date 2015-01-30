!-------------------------------------------------------------------------------
! Name:
!    Read_Data_nc
!
! Purpose:
!    Controls the reading of Multi Spectral Image (ALB) values from ATSR-type
!    files into the DATA_ALB array.
!
! Arguments:
!    Name     Type         In/Out/Both Description
!    Ctrl     struct       Both        Control structure (date is read in here).
!    NSegs    int          In          Number of image segments read in by
!                                      previous calls to this routine.
!    SegSize  int          In          Number of rows of pixels in an image
!                                      forth for subsequent reads.
!    MSI_Data struct       Both        Data structure: the ALB data part of this
!                                      struct is populated by this routine, and
!                                      is overwritten on successive calls.
!    SAD_Chan struct array Both        Instrument channel parameters. Updated by
!                                      this routine: solar constant is
!                                      modified from annual average to value for
!                                      the day of the ALB data.
!
! Algorithm:
!
! Local variables:
!    Name Type Description
!
! History:
!    ????/??/??, Matthias Jerg: Original version.
!    2013/02/26, CP: added in header and remove hardwiring of MSI_Data%ALB=3
!    2014/04/20, GM: Cleaned up the code.
!    2014/08/18, AP: Commented out reading of scanline data as it is not
!       actually used.
!    2014/04/20, GM: Added call to Nullify_Data() as some pointers may not be
!       associated.
!
! Bugs:
!   None known.
!
! $Id$
!
!-------------------------------------------------------------------------------

subroutine Read_Data_nc(Ctrl, NSegs, SegSize, MSI_Data, SAD_Chan, verbose)

   use netcdf

   use CTRL_def
   use SAD_Chan_def

   implicit none

   ! Define arguments

   type(CTRL_t),     intent(inout) :: Ctrl
   integer,          intent(in)    :: NSegs
   integer,          intent(in)    :: SegSize
   type(Data_t),     intent(inout) :: MSI_Data
   type(SAD_Chan_t), intent(inout) :: SAD_Chan(Ctrl%Ind%Ny)
   logical,          intent(in)    :: verbose

   ! Nullify pointers in case some are not associated
   call Nullify_Data(Ctrl, MSI_Data)

   ! Define local variables

   ! Call appropriate satellite data reading routines
   ! (Sections to be added as reading routines become available

   if (Ctrl%RS%Flag == SelmAux) then
      if (verbose) write(*,*) 'Reading Albedo data'
      call Read_ALB_nc(Ctrl, NSegs, SegSize, MSI_Data, verbose)
   end if

   if (verbose) write(*,*) 'Reading Cloud Flag data'
   call Read_CloudFlags_nc(Ctrl, NSegs, SegSize, MSI_Data, verbose)

   if (verbose) write(*,*) 'Reading Geometry data'
   call Read_Geometry_nc(Ctrl, NSegs, SegSize, MSI_Data, verbose)

   if (verbose) write(*,*) 'Reading Location data'
   call Read_Location_nc(Ctrl, NSegs, SegSize,MSI_Data, verbose)

   if (verbose)  write(*,*) 'Reading LS Flag data'
   call Read_LSFlags_nc(Ctrl, NSegs, SegSize, MSI_Data, verbose)

   if (verbose)  write(*,*) 'Reading MSI data'
   call Read_MSI_nc(Ctrl, NSegs, SegSize, MSI_Data, SAD_Chan, verbose)

   if (verbose)  write(*,*) 'Reading Illumination data'
   call Read_Illum_nc(Ctrl, NSegs, SegSize, MSI_Data, verbose)

end subroutine Read_Data_nc
