! Name: read_modis_lsflag.F90
!
!
! Purpose:
! Open and read L1b and geo input files
! 
!
! Description and Algorithm details:
! 1) Set start, end, and stride of data read.
! 2) Read data with SFRDATA.
! 3) Set all fill values to -1.
!
! Arguments:
! Name Type In/Out/Both Description
! ------------------------------------------------------------------------------
! fid      int    in   A file ID returned by SFSTART
! SDS_name string in   Name of the data field to read
! ixstart  lint   in   First pixel to read across track
! ixstop   lint   in   Last pixel to read across track
! iystart  lint   in   First pixel to read along track
! iystop   lint   in   Last pixel to read along track
! btemp    sint   both Initialised array into which data is stored
!
! Local variables:
! Name Type Description
!
!
! History:
! 2011/12/15: MJ produces draft code which opens and reads MODIS ls flag
! 2013/09/06: AP tidying, fixed bug where uninitialised array temp was used,
!                added where statement (if desired)
!
! $Id$
!
! Bugs:
! none known
!

!-------------------------------------------------------
!-------------------------------------------------------
subroutine read_modis_lsflag(fid,SDS_name,ixstart,ixstop,iystart,iystop,btemp)

   use preproc_constants

   implicit none

   include "hdf.f90"
   include "dffunc.f90"

   integer             :: ixstart, ixstop, iystart, iystop
!  integer             :: ix, jy
   
   integer, intent(in) :: fid
   
   integer             :: file_id, var_id, err_code, start(2), stride(2)
   integer             :: edge(2), attr_id
   character(len=*)    :: SDS_name

   integer(kind=sint)  :: btemp(ixstart:ixstop,iystart:iystop)

   integer(kind=sint) :: fv

   start(1) = ixstart-1
   start(2) = iystart-1
   stride = 1
   edge(1) = ixstop-ixstart+1
   edge(2) = iystop-iystart+1

   file_id = fid
   var_id = sfselect(file_id, sfn2index(file_id, SDS_name))

   ! read data into array
   btemp = -1
   err_code = sfrdata(var_id, start, stride, edge, btemp)

   attr_id=sffattr(var_id, "_FillValue")
   err_code=sfrattr(var_id, attr_id, fv)

   !write(*,*) 'fv ls flag',fv
   !write(*,*) 'btemp',btemp
   !stop
   ! overwrite fill value with ORAC value
   ! which of these is most efficient is compiler-dependant
   where(btemp .eq. fv) btemp=-1
!   do ix=ixstart,ixstop
!      do jy=iystart,iystop
!         if(btemp(ix,jy) .eq. fv ) btemp(ix,jy)=-1
!      enddo
!   enddo

   err_code=sfendacc(var_id)

end subroutine read_modis_lsflag

