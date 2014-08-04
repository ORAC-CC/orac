!-------------------------------------------------------------------------------
! Name:
!    Read_sat_data_nc
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
!    status   int          Out         Error status
!
! Algorithm:
!
! Local variables:
!    Name Type Description
!
! History:
!    ????/??/??, Matthias Jerg: Orginial version.
!    2013/02/26, CP: added in header and remove hadr wiring of MSI_Data%ALB=3
!    2014/04/20, GM: Cleaned up the code.
!
! Bugs:
!   None known.
!
! $Id$
!
!-------------------------------------------------------------------------------

subroutine Read_SatData_nc(Ctrl, NSegs, SegSize, MSI_Data, SAD_Chan, status)

   use netcdf

   use CTRL_def
   use Data_def
   use SAD_Chan_def

   implicit none

   ! Define arguments

   type(CTRL_t), intent(in)        :: Ctrl
   integer, intent(in)             :: NSegs
   integer, intent(in)             :: SegSize
   type(Data_t), intent(inout)     :: MSI_Data
   type(SAD_Chan_t), intent(inout) :: SAD_Chan(Ctrl%Ind%Ny)
   integer, intent(out)            :: status

   ! Define local variables

   ! Call appropriate satellite data reading routines
   ! (Sections to be added as reading routines become available

   if (Ctrl%RS%Flag == SelmAux) then
      write(*,*) 'Reading Albedo data, status: ',status
      if (status == 0) call Read_ALB_nc(Ctrl, NSegs, SegSize, MSI_Data, status)
   endif

   write(*,*) 'Reading Cloud Flag data, status: ',status
   if (status == 0) call Read_CloudFlags_nc(Ctrl, NSegs, SegSize, MSI_Data, status)

   write(*,*) 'Reading Geometry data, status: ',status
   if (status == 0) call Read_Geometry_nc(Ctrl, NSegs, SegSize, MSI_Data, status)

   write(*,*) 'Reading Location data, status: ',status
   if (status == 0) call Read_Location_nc(Ctrl, NSegs, SegSize,MSI_Data, status)

   write(*,*) 'Reading LS Flag data, status: ',status
   if (status == 0) call Read_LSFlags_nc(Ctrl, NSegs, SegSize, MSI_Data, status)

   write(*,*) 'Reading MSI data, status: ',status
   call Read_MSI_nc(Ctrl, NSegs, SegSize, MSI_Data, SAD_Chan, status)

   write(*,*) 'Reading Scanline data, status: ',status
   if (status == 0) call Read_Scanlines_nc(Ctrl, NSegs, SegSize, MSI_Data, status)

   write(*,*) 'Reading Illumination data, status: ',status
   if (status == 0) call Read_Illum_nc(Ctrl, NSegs, SegSize, MSI_Data, status)

end subroutine Read_SatData_nc
