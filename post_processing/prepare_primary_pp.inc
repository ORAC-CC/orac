  ! Name: prepare_primary.inc.f90
  !
  !
  ! Purpose:
  ! Map internal representation of variables to output representation by applying scale and offset where necessary.
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
  !2011/12/19: Matthias Jerg creates initial output for main ooutput variables.
  !2012/01/06: Matthias Jerg added in cwp
  !2012/01/16: Caroline Poulsen bug fix: changed how offset applied
  ! 2012/07/06 MJ extensively overhauls and restructures the code
  ! 2012/07/17 MJ fixes bug in CWP write.
  ! 2012/07/31 MJ fixes bug in CTY write.
  ! 2013/01/17 Matthias Jerg: Adds code to accommodate uncertainties of ctt and cth
  ! 2014/01/22 MJ fixes FP overflow with COT.
  ! 2014/07/08 CP added more illumination options
  ! 2014/10/24 OS: added variables cccot_pre, lusflag, cldtype, cloudmask, DEM
  !  (currently deactivated), and nisemask
  ! 2014/11/20 OS: added Pavolonis cloud phase variable, which is here classified
  !  through Pavolonis cloud types
  !  2014/11/20 CP: added cloud albedo
  !  2015/02/07 CP: changed to common constants
  !  2015/07/04 CP: added corrected cth
  ! $Id$
  !
  ! Bugs:
  !
  !none known
  !-------------------------------------
  !-------------------------------------
  !TIME: this needs to be implemented once preprocessing is in fortran
  !-------------------------------------
  !-------------------------------------
  
  spixel_scan_out%time(i,j)=l2_input_2dice_primary%Time(i,j)


  !-------------------------------------
  !-------------------------------------
  !GEOLOCATION
  !-------------------------------------
  !-------------------------------------
  spixel_scan_out%lon(i,j)=l2_input_2dice_primary%Lon(i,j)/spixel_scan_out%lon_scale
  spixel_scan_out%lat(i,j)=l2_input_2dice_primary%Lat(i,j)/spixel_scan_out%lat_scale
  !write(*,*)'pre lon',j,i,spixel_scan_out%lon(i,j),l2_input_2dice_primary%Lon(i,j)

  !-------------------------------------
  !-------------------------------------
  !ANGLES
  !-------------------------------------
  !-------------------------------------
  !if more than view, this needs to be modified
  do ia=1,nviews
     spixel_scan_out%sat_zen(i,j)=l2_input_2dice_primary%satellite_zenith_view_no1(i,j)
     spixel_scan_out%sol_zen(i,j)=l2_input_2dice_primary%solar_zenith_view_no1(i,j)
     spixel_scan_out%rel_azi(i,j)=l2_input_2dice_primary%rel_azimuth_view_no1(i,j)
  enddo

  !-------------------------------------
  !-------------------------------------
  !STATE VARIABLES and COVARIANCE DIAGONAL
  !-------------------------------------
  !-------------------------------------

  !if (l2_input_2dice_primary%illum(i,j) .ne. 1_byte) then 
  !write(*,*)'l2_input_2dice_primary%illum(i,j)',l2_input_2dice_primary%illum(i,j)
  !endif

  !write only microphysical values if pixel  "day"
  if(l2_input_2dice_primary%illum(i,j) .eq. 1_byte .or.&
       & l2_input_2dice_primary%illum(i,j) .eq. 4_byte .or. &
       & l2_input_2dice_primary%illum(i,j) .eq. 5_byte .or.&
       & l2_input_2dice_primary%illum(i,j) .eq. 6_byte .or. &
       & l2_input_2dice_primary%illum(i,j) .eq. 7_byte .or.&
       & l2_input_2dice_primary%illum(i,j) .eq. 8_byte .or. &
       & l2_input_2dice_primary%illum(i,j) .eq. 9_byte ) then

     !-------------------------------------
     !COT
     !-------------------------------------
     if( (l2_input_2dice_primary%cot(i,j) .ge. 0.00) .and. &
          & (l2_input_2dice_primary%cot(i,j) .lt. huge_value*spixel_scan_out&
          &%cot_scale)) then
        !write(*,*) l2_input_2dice_primary%cot_uncertainty(i,j)
        dummyreal=(l2_input_2dice_primary%cot(i,j)-spixel_scan_out&
             &%cot_offset)/spixel_scan_out%cot_scale
     else
        dummyreal=sreal_fill_value
     endif

     if( dummyreal .ge. real(spixel_scan_out%cot_vmin) .and. &
          & dummyreal .le. real(spixel_scan_out%cot_vmax)) then
        spixel_scan_out%cot(i,j)=int(dummyreal, kind=sint)
     elseif(dummyreal .gt. real(spixel_scan_out%cot_vmax)) then
        spixel_scan_out%cot(i,j)=spixel_scan_out%cot_vmax
     elseif(dummyreal .lt. real(spixel_scan_out%cot_vmin)) then
        !spixel_scan_out%cot(i,j)=spixel_scan_out%cot_vmin
        spixel_scan_out%cot(i,j)=spixel_scan_out%int_fill_value
     endif


     if( (l2_input_2dice_primary%cot_uncertainty(i,j) .ge. 0.00) .and. &
          & (l2_input_2dice_primary%cot_uncertainty(i,j) .lt. huge_value*spixel_scan_out%cot_error_scale)) then
        !write(*,*) l2_input_2dice_primary%cot_uncertainty(i,j)
        dummyreal=((l2_input_2dice_primary%cot_uncertainty(i,j))-spixel_scan_out%cot_error_offset)/spixel_scan_out%cot_error_scale
     else
        dummyreal=sreal_fill_value
     endif


     if( dummyreal .ge. real(spixel_scan_out%cot_error_vmin) .and. &
          & dummyreal .le. real(spixel_scan_out%cot_error_vmax)) then
        spixel_scan_out%cot_error(i,j)=int(dummyreal, kind=sint)
     elseif(dummyreal .gt. real(spixel_scan_out%cot_error_vmax)) then
        spixel_scan_out%cot_error(i,j)=spixel_scan_out%cot_error_vmax
     elseif(dummyreal .lt. real(spixel_scan_out%cot_error_vmin)) then
        !   spixel_scan_out%cot_error(i,j)=spixel_scan_out%cot_error_vmin
        spixel_scan_out%cot_error(i,j)=spixel_scan_out%int_fill_value
     endif


     !-------------------------------------
     !REFF
     !-------------------------------------
     dummyreal=(l2_input_2dice_primary%ref(i,j)-spixel_scan_out%ref_offset)/spixel_scan_out%ref_scale
     !write(*,*) i,j,l2_input_2dice_primary%ref(i,j)
     if( dummyreal .ge. real(spixel_scan_out%ref_vmin,kind=sreal) .and. &
          & dummyreal .le. real(spixel_scan_out%ref_vmax,kind=sreal)) then
        spixel_scan_out%ref(i,j)=int(dummyreal, kind=sint)
        !write(*,*) i,j,spixel_scan_out%ref(i,j)
     elseif(dummyreal .gt. real(spixel_scan_out%ref_vmax,kind=sreal)) then
        spixel_scan_out%ref(i,j)=spixel_scan_out%ref_vmax
     elseif(dummyreal .lt. real(spixel_scan_out%ref_vmin,kind=sreal)) then
        spixel_scan_out%ref(i,j)=spixel_scan_out%int_fill_value
     endif

     if(l2_input_2dice_primary%ref_uncertainty(i,j) .ge. 0.00) then
        dummyreal=((l2_input_2dice_primary%ref_uncertainty(i,j))-spixel_scan_out%ref_error_offset)/spixel_scan_out%ref_error_scale
     else
        dummyreal=sreal_fill_value
     endif
     if( dummyreal .ge. real(spixel_scan_out%ref_error_vmin) .and. &
          & dummyreal .le. real(spixel_scan_out%ref_error_vmax)) then
        spixel_scan_out%ref_error(i,j)=int(dummyreal, kind=sint)
     elseif(dummyreal .gt. real(spixel_scan_out%ref_error_vmax)) then
        spixel_scan_out%ref_error(i,j)=spixel_scan_out%ref_error_vmax
     elseif(dummyreal .lt. real(spixel_scan_out%ref_error_vmin)) then
        spixel_scan_out%ref_error(i,j)=spixel_scan_out%int_fill_value
     endif


     !-------------------------------------
     !CWP
     !-------------------------------------
     dummyreal=(l2_input_2dice_primary%cwp(i,j)-spixel_scan_out%cwp_offset)/spixel_scan_out%cwp_scale

     if( dummyreal .ge. real(spixel_scan_out%cwp_vmin) .and. &
          & dummyreal .le. real(spixel_scan_out%cwp_vmax)) then
        spixel_scan_out%cwp(i,j)=int(dummyreal, kind=sint)
     elseif(dummyreal .gt. real(spixel_scan_out%cwp_vmax)) then
        spixel_scan_out%cwp(i,j)=spixel_scan_out%cwp_vmax
     elseif(dummyreal .lt. real(spixel_scan_out%cwp_vmin)) then
        spixel_scan_out%cwp(i,j)=spixel_scan_out%int_fill_value
     endif

     if(l2_input_2dice_primary%cwp_uncertainty(i,j) .ge. 0.00) then
        dummyreal=((l2_input_2dice_primary%CWP_uncertainty(i,j))-spixel_scan_out%cwp_error_offset)/spixel_scan_out%cwp_error_scale
     else
        dummyreal=sreal_fill_value
     endif

     if( dummyreal .ge. real(spixel_scan_out%cwp_error_vmin) .and. &
          & dummyreal .le. real(spixel_scan_out%cwp_error_vmax)) then
        spixel_scan_out%cwp_error(i,j)=int(dummyreal, kind=sint)
     elseif(dummyreal .gt. real(spixel_scan_out%cwp_error_vmax)) then
        spixel_scan_out%cwp_error(i,j)=spixel_scan_out%cwp_error_vmax
     elseif(dummyreal .lt. real(spixel_scan_out%cwp_error_vmin)) then
        spixel_scan_out%cwp_error(i,j)=spixel_scan_out%int_fill_value
     endif

  endif






     !-------------------------------------
     !CLOUD_ALBEDO
     !-------------------------------------

nchan_solar=2

      do iinput=1,nchan_solar
if(l2_input_2dice_primary%cloud_albedo(i,j,iinput) .ge. 0.00) then
     dummyreal=(l2_input_2dice_primary%cloud_albedo(i,j,iinput)-spixel_scan_out%cloud_albedo_offset)/spixel_scan_out%cloud_albedo_scale

     if( dummyreal .ge. real(spixel_scan_out%cloud_albedo_vmin) .and. &
          & dummyreal .le. real(spixel_scan_out%cloud_albedo_vmax)) then
        spixel_scan_out%cloud_albedo(i,j,iinput)=int(dummyreal, kind=sint)
     elseif(dummyreal .gt. real(spixel_scan_out%cloud_albedo_vmax)) then
        spixel_scan_out%cloud_albedo(i,j,iinput)=spixel_scan_out%cloud_albedo_vmax
     elseif(dummyreal .lt. real(spixel_scan_out%cloud_albedo_vmin)) then
        spixel_scan_out%cloud_albedo(i,j,iinput)=spixel_scan_out%int_fill_value
     endif


  endif

enddo






  !               write(*,*) 'write ctp'

  !-------------------------------------
  !CTP
  !-------------------------------------
  dummyreal=(l2_input_2dice_primary%ctp(i,j)-spixel_scan_out%ctp_offset)/spixel_scan_out%ctp_scale
  if( dummyreal .ge. real(spixel_scan_out%ctp_vmin) .and. &
       & dummyreal .le. real(spixel_scan_out%ctp_vmax)) then
     spixel_scan_out%ctp(i,j)=int(dummyreal, kind=sint)
  elseif(dummyreal .gt. real(spixel_scan_out%ctp_vmax)) then
     spixel_scan_out%ctp(i,j)=spixel_scan_out%ctp_vmax
  elseif(dummyreal .lt. real(spixel_scan_out%ctp_vmin)) then
     spixel_scan_out%ctp(i,j)=spixel_scan_out%int_fill_value
  endif

  if(l2_input_2dice_primary%ctp_uncertainty(i,j) .ge. 0.00) then
     dummyreal=((l2_input_2dice_primary%ctp_uncertainty(i,j))-spixel_scan_out%ctp_error_offset)/spixel_scan_out%ctp_error_scale
  else
     dummyreal=sreal_fill_value
  endif
  if( dummyreal .ge. real(spixel_scan_out%ctp_error_vmin) .and. &
       & dummyreal .le. real(spixel_scan_out%ctp_error_vmax)) then
     spixel_scan_out%ctp_error(i,j)=int(dummyreal, kind=sint)
  elseif(dummyreal .gt. real(spixel_scan_out%ctp_error_vmax)) then
     spixel_scan_out%ctp_error(i,j)=spixel_scan_out%ctp_error_vmax
  elseif(dummyreal .lt. real(spixel_scan_out%ctp_error_vmin)) then
     spixel_scan_out%ctp_error(i,j)=spixel_scan_out%int_fill_value
  endif

  !               write(*,*) 'write cct'


  !-------------------------------------
  !CCT
  !-------------------------------------
  dummyreal=(l2_input_2dice_primary%cc_total(i,j)-spixel_scan_out%cct_offset)/spixel_scan_out%cct_scale
  if( dummyreal .ge. real(spixel_scan_out%cct_vmin) .and. &
       & dummyreal .le. real(spixel_scan_out%cct_vmax)) then
     spixel_scan_out%cct(i,j)=int(dummyreal, kind=sint)
  else
     spixel_scan_out%cct(i,j)=spixel_scan_out%int_fill_value
  endif

  if(l2_input_2dice_primary%cc_total_uncertainty(i,j) .ge. 0.00) then
     dummyreal=((l2_input_2dice_primary%cc_total_uncertainty(i,j))- &
          spixel_scan_out%cct_error_offset)/spixel_scan_out%cct_error_scale
  else
     dummyreal=sreal_fill_value
  endif
  if( dummyreal .ge. real(spixel_scan_out%cct_error_vmin) .and. &
       & dummyreal .le. real(spixel_scan_out%cct_error_vmax)) then
     spixel_scan_out%cct_error(i,j)=int(dummyreal, kind=sint)
  else
     spixel_scan_out%cct_error(i,j)=spixel_scan_out%int_fill_value
  endif

  !===============================
  !NN OUTPUT
  !===============================

  !-------------------------------------
  !CCCOT
  !-------------------------------------
  dummyreal=(l2_input_2dice_primary%cccot(i,j)-spixel_scan_out%cccot_offset)/spixel_scan_out%cccot_scale
  if( dummyreal .ge. real(spixel_scan_out%cccot_vmin) .and. &
       & dummyreal .le. real(spixel_scan_out%cccot_vmax)) then
     spixel_scan_out%cccot(i,j)=int(dummyreal, kind=sint)
  elseif(dummyreal .gt. real(spixel_scan_out%cccot_vmax)) then
     spixel_scan_out%cccot(i,j)=spixel_scan_out%cccot_vmax
  elseif(dummyreal .lt. real(spixel_scan_out%cccot_vmin)) then
     spixel_scan_out%cccot(i,j)=spixel_scan_out%int_fill_value
  endif

  !-------------------------------------
  !CCCOT_pre
  !-------------------------------------
  dummyreal=(l2_input_2dice_primary%cccot_pre(i,j)- &
       & spixel_scan_out%cccot_pre_offset)/spixel_scan_out%cccot_pre_scale
  if( dummyreal .ge. real(spixel_scan_out%cccot_pre_vmin) .and. &
       & dummyreal .le. real(spixel_scan_out%cccot_pre_vmax)) then
     spixel_scan_out%cccot_pre(i,j)=int(dummyreal, kind=sint)
  elseif(dummyreal .gt. real(spixel_scan_out%cccot_pre_vmax)) then
     spixel_scan_out%cccot_pre(i,j)=spixel_scan_out%cccot_pre_vmax
  elseif(dummyreal .lt. real(spixel_scan_out%cccot_pre_vmin)) then
     spixel_scan_out%cccot_pre(i,j)=spixel_scan_out%int_fill_value
  endif

  !               write(*,*) 'write stemp'

  !-------------------------------------
  !STEMP
  !-------------------------------------

  dummyreal=(l2_input_2dice_primary%stemp(i,j)-spixel_scan_out%stemp_offset)/spixel_scan_out%stemp_scale
  if( dummyreal .ge. real(spixel_scan_out%stemp_vmin) .and. &
       & dummyreal .le. real(spixel_scan_out%stemp_vmax)) then
     spixel_scan_out%stemp(i,j)=int(dummyreal, kind=sint)
  elseif(dummyreal .gt. real(spixel_scan_out%stemp_vmax)) then
     spixel_scan_out%stemp(i,j)=spixel_scan_out%stemp_vmax
  elseif(dummyreal .lt. real(spixel_scan_out%stemp_vmin)) then
     spixel_scan_out%stemp(i,j)=spixel_scan_out%int_fill_value
  endif


  if(l2_input_2dice_primary%stemp_uncertainty(i,j) .ge. 0.00) then
     !write(*,*) l2_input_2dice_primary%stemp_uncertainty(i,j),spixel_scan_out%stemp_error_offset,spixel_scan_out%stemp_error_scale 
     dummyreal=(l2_input_2dice_primary%stemp_uncertainty(i,j)- &
          & spixel_scan_out%stemp_error_offset)/spixel_scan_out%stemp_error_scale
  else
     dummyreal=sreal_fill_value
  endif
  if( dummyreal .ge. real(spixel_scan_out%stemp_error_vmin,kind=sreal) .and. &
       & dummyreal .le. real(spixel_scan_out%stemp_error_vmax,kind=sreal)) then
     spixel_scan_out%stemp_error(i,j)=int(dummyreal, kind=sint)
  elseif(dummyreal .gt. real(spixel_scan_out%stemp_error_vmax,kind=sreal)) then
     spixel_scan_out%stemp_error(i,j)=spixel_scan_out%stemp_error_vmax
  elseif(dummyreal .lt. real(spixel_scan_out%stemp_error_vmin,kind=sreal)) then
     spixel_scan_out%stemp_error(i,j)=spixel_scan_out%int_fill_value
  endif

  !		    write(*,*) 'write cth',spixel_scan_out%cth_scale
  !-------------------------------------
  !NO REAL STATE VARIABLES=>NO COVARIANCE FROM HERE
  !-------------------------------------

  !-------------------------------------
  !CTH
  !-------------------------------------
  !		    write(*,*) 'cth',i,j,l2_input_2dice_primary%cth(i,j)
  dummyreal=(l2_input_2dice_primary%cth(i,j)-spixel_scan_out%cth_offset)/spixel_scan_out%cth_scale
  !		    write(*,*) 'dummyreal',dummyreal
  !		    write(*,*) 'bfore if'
  if( dummyreal .ge. real(spixel_scan_out%cth_vmin) .and. &
       & dummyreal .le. real(spixel_scan_out%cth_vmax)) then
     spixel_scan_out%cth(i,j)=int(dummyreal, kind=sint)
  elseif(dummyreal .gt. real(spixel_scan_out%cth_vmax)) then
     spixel_scan_out%cth(i,j)=spixel_scan_out%cth_vmax
  elseif(dummyreal .lt. real(spixel_scan_out%cth_vmin)) then
     spixel_scan_out%cth(i,j)=spixel_scan_out%int_fill_value
  endif

  !               write(*,*) 'write cth'
  dummyreal=(l2_input_2dice_primary%cth_uncertainty(i,j)-spixel_scan_out%cth_error_offset)/spixel_scan_out%cth_error_scale
  !		    write(*,*) 'dummyreal',dummyreal
  !		    write(*,*) 'bfore if'
  if( dummyreal .ge. real(spixel_scan_out%cth_error_vmin) .and. &
       & dummyreal .le. real(spixel_scan_out%cth_error_vmax)) then
     spixel_scan_out%cth_error(i,j)=int(dummyreal, kind=sint)
  elseif(dummyreal .gt. real(spixel_scan_out%cth_error_vmax)) then
     spixel_scan_out%cth_error(i,j)=spixel_scan_out%cth_error_vmax
  elseif(dummyreal .lt. real(spixel_scan_out%cth_error_vmin)) then
     spixel_scan_out%cth_error(i,j)=spixel_scan_out%int_fill_value
  endif




  !-------------------------------------
  !CTH_CORRECTED
  !-------------------------------------
  !		    write(*,*) 'cth_corrected',i,j,l2_input_2dice_primary%cth_corrected(i,j)
  dummyreal=(l2_input_2dice_primary%cth_corrected(i,j)-spixel_scan_out%cth_offset)/spixel_scan_out%cth_scale
  !		    write(*,*) 'dummyreal',dummyreal
  !		    write(*,*) 'bfore if'
  if( dummyreal .ge. real(spixel_scan_out%cth_vmin) .and. &
       & dummyreal .le. real(spixel_scan_out%cth_vmax)) then
     spixel_scan_out%cth_corrected(i,j)=int(dummyreal, kind=sint)
  elseif(dummyreal .gt. real(spixel_scan_out%cth_vmax)) then
     spixel_scan_out%cth_corrected(i,j)=spixel_scan_out%cth_vmax
  elseif(dummyreal .lt. real(spixel_scan_out%cth_vmin)) then
     spixel_scan_out%cth_corrected(i,j)=spixel_scan_out%int_fill_value
  endif

  !               write(*,*) 'write cth_corrected'
  dummyreal=(l2_input_2dice_primary%cth_corrected_uncertainty(i,j)-spixel_scan_out%cth_error_offset)/spixel_scan_out%cth_error_scale
  !		    write(*,*) 'dummyreal',dummyreal
  !		    write(*,*) 'bfore if'
  if( dummyreal .ge. real(spixel_scan_out%cth_error_vmin) .and. &
       & dummyreal .le. real(spixel_scan_out%cth_error_vmax)) then
     spixel_scan_out%cth_corrected_error(i,j)=int(dummyreal, kind=sint)
  elseif(dummyreal .gt. real(spixel_scan_out%cth_error_vmax)) then
     spixel_scan_out%cth_corrected_error(i,j)=spixel_scan_out%cth_error_vmax
  elseif(dummyreal .lt. real(spixel_scan_out%cth_error_vmin)) then
     spixel_scan_out%cth_corrected_error(i,j)=spixel_scan_out%int_fill_value
  endif







  !-------------------------------------
  !CTT
  !-------------------------------------
  dummyreal=(l2_input_2dice_primary%ctt(i,j)-spixel_scan_out%ctt_offset)/spixel_scan_out%ctt_scale
  if( dummyreal .ge. real(spixel_scan_out%ctt_vmin) .and. &
       & dummyreal .le. real(spixel_scan_out%ctt_vmax)) then
     spixel_scan_out%ctt(i,j)=int(dummyreal, kind=sint)
  elseif(dummyreal .gt. real(spixel_scan_out%ctt_vmax)) then
     spixel_scan_out%ctt(i,j)=spixel_scan_out%ctt_vmax
  elseif(dummyreal .lt. real(spixel_scan_out%ctt_vmin)) then
     spixel_scan_out%ctt(i,j)=spixel_scan_out%int_fill_value
  endif

  dummyreal=(l2_input_2dice_primary%ctt_uncertainty(i,j)-spixel_scan_out%ctt_error_offset)/spixel_scan_out%ctt_error_scale
  if( dummyreal .ge. real(spixel_scan_out%ctt_error_vmin) .and. &
       & dummyreal .le. real(spixel_scan_out%ctt_error_vmax)) then
     spixel_scan_out%ctt_error(i,j)=int(dummyreal, kind=sint)
  elseif(dummyreal .gt. real(spixel_scan_out%ctt_error_vmax)) then
     spixel_scan_out%ctt_error(i,j)=spixel_scan_out%ctt_error_vmax
  elseif(dummyreal .lt. real(spixel_scan_out%ctt_error_vmin)) then
     spixel_scan_out%ctt_error(i,j)=spixel_scan_out%int_fill_value
  endif



  !-------------------------------------
  !CONVERGENCE, NO OF ITERATIONS,  PHASE type
  !-------------------------------------
  spixel_scan_out%convergence(i,j)=int(l2_input_2dice_primary%convergence(i,j),kind=byte)

  if(l2_input_2dice_primary%convergence(i,j) .eq. 0 ) spixel_scan_out%niter(i,j)=int(l2_input_2dice_primary%niter(i,j),kind=byte)
  if(l2_input_2dice_primary%convergence(i,j) .eq. 1 ) spixel_scan_out%niter(i,j)=int(spixel_scan_out%byte_fill_value,kind=byte)

  if(int(l2_input_2dice_primary%phase(i,j),kind=byte) .ge. spixel_scan_out%phase_vmin &
       .and. int(l2_input_2dice_primary%phase(i,j),kind=byte) .le. & 
       spixel_scan_out%phase_vmax ) then
     spixel_scan_out%phase(i,j)=int(l2_input_2dice_primary%phase(i,j),kind=byte)
  else
     spixel_scan_out%phase(i,j)=spixel_scan_out%byte_fill_value
  endif

  if (     l2_input_2dice_primary%cldtype(i,j) .lt. 5 &
     .and. l2_input_2dice_primary%cldtype(i,j) .gt. 1 ) then
     spixel_scan_out%phase_pavolonis(i,j) = 1 ! phase = water
  elseif ( l2_input_2dice_primary%cldtype(i,j) .gt. 5 ) then
     spixel_scan_out%phase_pavolonis(i,j) = 2 ! phase = ice
  elseif ( l2_input_2dice_primary%cldtype(i,j) .eq. 0 ) then
     spixel_scan_out%phase_pavolonis(i,j) = 0 ! phase = clear     
  elseif ( l2_input_2dice_primary%cldtype(i,j) .eq. 7 ) then
     spixel_scan_out%phase_pavolonis(i,j) = 3 ! phase = mli     
  elseif ( l2_input_2dice_primary%cldtype(i,j) .eq. 8 ) then
     spixel_scan_out%phase_pavolonis(i,j) = 3 ! phase = mli     
  else
     spixel_scan_out%phase_pavolonis(i,j) = spixel_scan_out%byte_fill_value ! for all
     ! other values (should not occur)
  endif


  !dummy statement to set everything to liquid:
  !	       spixel_scan_out%pchange(i,j)=int(1,kind=byte)  

  !               write(*,*) 'write costjm'

  !-------------------------------------
  !COST DUE TO MEASUREMENTS
  !-------------------------------------
  dummyreal=real((l2_input_2dice_primary%costJm(i,j)-spixel_scan_out%costjm_offset)/spixel_scan_out%costjm_scale ,kind=sreal)
  if( dummyreal .ge. real(spixel_scan_out%costjm_vmin) .and. &
       & dummyreal .le. real(spixel_scan_out%costjm_vmax)) then
     spixel_scan_out%costjm(i,j)=real(dummyreal,kind=sreal)!int(dummyreal, kind=sint)
  else
     spixel_scan_out%costjm(i,j)=sreal_fill_value!spixel_scan_out%int_fill_value
  endif


  !               write(*,*) 'write costja'


  !-------------------------------------
  !COST DUE TO A PRIORI
  !-------------------------------------
  dummyreal=real((l2_input_2dice_primary%costJa(i,j)-spixel_scan_out%costja_offset)/spixel_scan_out%costja_scale,kind=sreal)
  if( dummyreal .ge. real(spixel_scan_out%costja_vmin) .and. &
       & dummyreal .le. real(spixel_scan_out%costja_vmax)) then
     spixel_scan_out%costja(i,j)=real(dummyreal,kind=sreal)!int(dummyreal, kind=sint)
  else
     spixel_scan_out%costja(i,j)=sreal_fill_value!spixel_scan_out%int_fill_value
  endif

  !               write(*,*) 'write lsflag'

  !-------------------------------------
  !LAND-SEA FLAG
  !-------------------------------------
  spixel_scan_out%lsflag(i,j)=int(l2_input_2dice_primary%LSFlag(i,j), kind=byte)

  !-------------------------------------
  !LAND USE FLAG
  !-------------------------------------
  spixel_scan_out%lusflag(i,j)=int(l2_input_2dice_primary%LUSFlag(i,j), kind=byte)

  !-------------------------------------
  !CLOUD TYPE
  !-------------------------------------
  spixel_scan_out%cldtype(i,j)=int(l2_input_2dice_primary%cldtype(i,j),&
       & kind=byte)

  !-------------------------------------
  !CLOUD MASK
  !-------------------------------------
  spixel_scan_out%cldmask(i,j)=int(l2_input_2dice_primary%cldmask(i,j), kind=byte)

  !-------------------------------------
  !DEM
  !-------------------------------------
  !spixel_scan_out%dem(i,j)=int(l2_input_2dice_primary%dem(i,j), kind=sint)

  !-------------------------------------
  !NISEMASK
  !-------------------------------------
  spixel_scan_out%nisemask(i,j)=int(l2_input_2dice_primary%nisemask(i,j), kind=byte)



  !               write(*,*) 'write qcflag'
  !-------------------------------------
  !QUALITY FLAG
  !-------------------------------------
  spixel_scan_out%qcflag(i,j)=int(l2_input_2dice_primary%QCFlag(i,j),kind=sint)

  !               write(*,*) 'write illum'
  !-------------------------------------
  !ILLUMNATION FLAG
  !-------------------------------------
  spixel_scan_out%illum(i,j)=int(l2_input_2dice_primary%Illum(i,j), kind=byte)
