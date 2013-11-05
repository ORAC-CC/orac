! Name: get_coefs_info.f90
!
!
! Purpose:
! Set up coefs specs.
! 
!
! Description and Algorithm details:
!
!
! Arguments:
! Name Type In/Out/Both Description
!
!
! Local variables:
! Name Type Description
!
!
! History:
!2012/03/27: Matthias Jerg provides initial implementation based on the example program example_fw
!                    of  Annex X of the RTTOV V10.2 user guide V1.3.
!2012/06/13 C. Poulsen small bug fixes
! $Id$
!
! Bugs:
!
!none known


subroutine get_coefs_info(platformnumber,instrument,nrttovid,icoeffs,coeffile)

 use preproc_constants

implicit none 

character(len=pathlength) :: coef_path
character(len=filelength) :: file_coef,coeffile
  character(len=filelength) :: form_coef
Integer, Allocatable  :: instrument(:,:) 
integer :: icoeffs,nrttovid
character(len=platformlength) :: platformnumber

    !set up coefficients
     !pick instrument
     !AVHRR
     if(instrument(3,nrttovid) .eq. 5) then
        !NOAA:
        if(instrument(1,nrttovid) .eq. 1) then
           !pick satellite directly through instrument(2,nrttovid)
           !first LW
           if(icoeffs .eq. 1) then
              coeffile='rtcoef_noaa_'//trim(adjustl(platformnumber))//'_avhrr.v10.dat'
              !now SW
           elseif(icoeffs .eq. 2) then
              coeffile='rtcoef_noaa_'//trim(adjustl(platformnumber))//'_avhrr_visnir_v1.v10.dat'
           endif
           !METOP
        elseif(instrument(1,nrttovid) .eq. 10) then
           if(icoeffs .eq. 1) then
              coeffile='rtcoef_metop_'//trim(adjustl(platformnumber))//'_avhrr.v10.dat'
              !now SW
           elseif(icoeffs .eq. 2) then
              coeffile='rtcoef_metop_'//trim(adjustl(platformnumber))//'_avhrr_visnir_v1.v10.dat'
           endif
        endif
          
        !MODIS
     elseif(instrument(3,nrttovid) .eq. 13) then
        !TERRA
        if(instrument(2,nrttovid) .eq. 1) then
           !first LW
           if(icoeffs .eq. 1) then           
              coeffile='rtcoef_eos_1_modis.v10.dat'
              !now SW
           elseif(icoeffs .eq. 2) then
              coeffile='rtcoef_eos_1_modis_visnir_v1.v10.dat'
           endif
           !AQUA
        elseif(instrument(2,nrttovid) .eq. 2) then
           if(icoeffs .eq. 1) then           
              coeffile='rtcoef_eos_2_modis.v10.dat'
              !now SW
           elseif(icoeffs .eq. 2) then
              coeffile='rtcoef_eos_2_modis_visnir_v1.v10.dat'
           endif
        endif

        !AATSR
     elseif(instrument(3,nrttovid) .eq. 14) then
        
     endif

end subroutine get_coefs_info