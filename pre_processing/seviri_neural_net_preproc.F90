!-------------------------------------------------------------------------------
! Module calling the library for the external SEVIRI neural network
! cloud
! detection and cloud phase determination. This module is the bridge
! between ORAC and the external library in external_ml/seviri.
!
! 2020/09/16, DP: Initial version.
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module seviri_neural_net_preproc_m

     implicit None

     contains


subroutine cma_cph_seviri(cview, imager_flags, imager_angles, &
                          imager_geolocation, imager_measurements, &
                          imager_pavolonis, skt, channel_info, platform, &
                          do_nasa)

#ifdef INCLUDE_SEVIRI_NEURALNET
     use SEVIRI_NEURAL_NET_M
#endif
     use preproc_constants_m
     use imager_structures_m
     use channel_structures_m

     ! arguments
     integer,                     intent(in)     :: cview
     type(imager_flags_t),        intent(in)     :: imager_flags
     type(imager_angles_t),       intent(in)     :: imager_angles
     type(imager_geolocation_t),  intent(in)     :: imager_geolocation
     type(imager_measurements_t), intent(in)     :: imager_measurements
     type(imager_pavolonis_t),    intent(inout)  :: imager_pavolonis
     type(channel_info_t),        intent(in)     :: channel_info
     real(kind=sreal),            intent(in)     :: &
          skt(imager_geolocation%startx:imager_geolocation%endx, &
              1:imager_geolocation%ny)
     character(len=*),             intent(in)     :: platform
     logical,                     intent(in)     :: do_nasa

     integer(kind=1) :: msg_index
     logical(kind=1) :: undo_true_reflectances = .false.
     integer :: ch1, ch2, ch3, ch4, ch5, ch6, ch7, ch9, ch10, ch11

     ! channel indices
     ch1 = 1
     ch2 = 2
     ch3 = 3
     ch4 = 4
     ch5 = 5
     ch6 = 6
     ch7 = 7
     ch9 = 9
     ch10 = 10
     ch11 = 11

     select case (trim(platform))
     case ("MSG1")
         msg_index = 1
     case ("MSG2")
         msg_index = 2
     case ("MSG3")
         msg_index = 3
     case ("MSG4")
         msg_index = 4
     case default
         write(*,*) "Platform ", platform, " not supported with seviri_ml."
         stop
     end select

     ! if VIS calibration is not NASA (IMPF) do not apply NASA-to-IMPF
     ! conversion for ANNs
     if (.not. do_nasa) then
         msg_index = 0
     end if

#ifdef INCLUDE_SEVIRI_NEURALNET
     write(*,*) "PREDICTING COT/CPH"
     ! run external ANN
     call seviri_ann_cma(imager_geolocation%nx, &  ! xdim for reshaping
                imager_geolocation%ny, &               ! ydim for reshaping
                imager_measurements%data(:,:,ch1), &   ! VIS006
                imager_measurements%data(:,:,ch2), &   ! VIS008
                imager_measurements%data(:,:,ch3), &   ! NIR016
                imager_measurements%data(:,:,ch4), &   ! IR039
                imager_measurements%data(:,:,ch5), &   ! IR062
                imager_measurements%data(:,:,ch6), &   ! IR073
                imager_measurements%data(:,:,ch7), &   ! IR087
                imager_measurements%data(:,:,ch9), &   ! IR108
                imager_measurements%data(:,:,ch10), &  ! IR120
                imager_measurements%data(:,:,ch11), &  ! IR134
                imager_flags%lsflag, &                 ! Land-sea mask
                skt, &                                 ! ECMWF skin temp
                imager_angles%solzen(:,:,cview), &    ! Solar Zenith Angle
                imager_angles%satzen(:,:,cview), &    ! Satellite Zenith Angle
                imager_pavolonis%cccot_pre(:,:,cview), &
                imager_pavolonis%cldmask(:,:,cview), &
                imager_pavolonis%cldmask_uncertainty(:,:,cview), &
                msg_index, &
                undo_true_reflectances)

     call seviri_ann_cph(imager_geolocation%nx, &  ! xdim for reshaping
                imager_geolocation%ny, &               ! ydim for reshaping
                imager_measurements%data(:,:,ch1), &   ! VIS006
                imager_measurements%data(:,:,ch2), &   ! VIS008
                imager_measurements%data(:,:,ch3), &   ! NIR016
                imager_measurements%data(:,:,ch4), &   ! IR039
                imager_measurements%data(:,:,ch5), &   ! IR062
                imager_measurements%data(:,:,ch6), &   ! IR073
                imager_measurements%data(:,:,ch7), &   ! IR087
                imager_measurements%data(:,:,ch9), &   ! IR108
                imager_measurements%data(:,:,ch10), &  ! IR120
                imager_measurements%data(:,:,ch11), &  ! IR134
                imager_flags%lsflag, &                 ! Land-sea mask
                skt, &                                 ! ECMWF skin temp
                imager_angles%solzen(:,:,cview), &    ! Solar Zenith Angle
                imager_angles%satzen(:,:,cview), &    ! Satellite Zenith Angle
                imager_pavolonis%cphcot(:,:,cview), &
                imager_pavolonis%ann_phase(:,:,cview), &
                imager_pavolonis%ann_phase_uncertainty(:,:,cview), &
                imager_pavolonis%cldmask(:,:,cview), &
                msg_index, &
                undo_true_reflectances)
#else
     write(*,*) 'ERROR: ORAC has been compiled without SEVIRI neural &
                &network support. (1) Compile the SEVIRI neural &
                &network in ../seviri_neuralnet. (2) Link the modules &
                &to the ORAC pre_processor compilation. (3) Recompile &
                &ORAC with -DINCLUDE_SEVIRI_NEURALNET.'
     stop error_stop_code
#endif
end subroutine cma_cph_seviri


subroutine ctp_fg_seviri(cview, imager_flags, imager_angles, &
                         imager_geolocation, imager_measurements, &
                         imager_pavolonis, skt, channel_info, platform, &
                         do_nasa)

#ifdef INCLUDE_SEVIRI_NEURALNET
     use SEVIRI_NEURAL_NET_M
#endif
     use preproc_constants_m
     use imager_structures_m
     use channel_structures_m

     ! arguments
     integer,                     intent(in)     :: cview
     type(imager_flags_t),        intent(in)     :: imager_flags
     type(imager_angles_t),       intent(in)     :: imager_angles
     type(imager_geolocation_t),  intent(in)     :: imager_geolocation
     type(imager_measurements_t), intent(in)     :: imager_measurements
     type(imager_pavolonis_t),    intent(inout)  :: imager_pavolonis
     type(channel_info_t),        intent(in)     :: channel_info
     real(kind=sreal),            intent(in)     :: &
          skt(imager_geolocation%startx:imager_geolocation%endx, &
              1:imager_geolocation%ny)
     character(len=*),             intent(in)     :: platform
     logical,                     intent(in)     :: do_nasa

     integer(kind=1) :: msg_index
     logical(kind=1) :: undo_true_reflectances = .false.
     integer :: ch1, ch2, ch3, ch4, ch5, ch6, ch7, ch9, ch10, ch11


     ! channel indices
     ch1 = 1
     ch2 = 2
     ch3 = 3
     ch4 = 4
     ch5 = 5
     ch6 = 6
     ch7 = 7
     ch9 = 9
     ch10 = 10
     ch11 = 11

     select case (trim(platform))
     case ("MSG1")
         msg_index = 1
     case ("MSG2")
         msg_index = 2
     case ("MSG3")
         msg_index = 3
     case ("MSG4")
         msg_index = 4
     case default
         write(*,*) "Platform ", platform, " not supported with seviri_ml."
         stop
     end select

     ! if VIS calibration is not NASA (IMPF) do not apply NASA-to-IMPF
     ! conversion for ANNs
     if (.not. do_nasa) then
         msg_index = 0
     end if

#ifdef INCLUDE_SEVIRI_NEURALNET
     write(*,*) "PREDICTING CTP for first guess"
     ! run external ANN
     call seviri_ann_ctp(imager_geolocation%nx, &      ! xdim for reshaping
                imager_geolocation%ny, &               ! ydim for reshaping
                imager_measurements%data(:,:,ch1), &   ! VIS006
                imager_measurements%data(:,:,ch2), &   ! VIS008
                imager_measurements%data(:,:,ch3), &   ! NIR016
                imager_measurements%data(:,:,ch4), &   ! IR039
                imager_measurements%data(:,:,ch5), &   ! IR062
                imager_measurements%data(:,:,ch6), &   ! IR073
                imager_measurements%data(:,:,ch7), &   ! IR087
                imager_measurements%data(:,:,ch9), &   ! IR108
                imager_measurements%data(:,:,ch10), &  ! IR120
                imager_measurements%data(:,:,ch11), &  ! IR134
                imager_flags%lsflag, &                 ! Land-sea mask
                skt, &                                 ! ECMWF skin temp
                imager_angles%solzen(:,:,cview), &     ! Solar Zenith Angle
                imager_angles%satzen(:,:,cview), &     ! Satellite Zenith Angle
                imager_pavolonis%ctp_fg(:,:,cview), &   ! CTP first guess
                imager_pavolonis%ctp_fg_unc(:,:,cview), &  ! CTP first guess unc
                imager_pavolonis%cldmask(:,:,cview), &
                msg_index, &
                undo_true_reflectances)

     ! convert standard deviation to variance
     where(imager_pavolonis%ctp_fg_unc(:,:,cview) .ne. sreal_fill_value)
         imager_pavolonis%ctp_fg_unc(:,:,cview) = & 
         imager_pavolonis%ctp_fg_unc(:,:,cview) * imager_pavolonis%ctp_fg_unc(:,:,cview)
     end where
#else
     write(*,*) 'ERROR: ORAC has been compiled without SEVIRI neural &
                &network support. (1) Compile the SEVIRI neural &
                &network in ../seviri_neuralnet. (2) Link the modules &
                &to the ORAC pre_processor compilation. (3) Recompile &
                &ORAC with -DINCLUDE_SEVIRI_NEURALNET.'
     stop error_stop_code
#endif

end subroutine ctp_fg_seviri


subroutine mlay_seviri(cview, imager_flags, imager_angles, &
                       imager_geolocation, imager_measurements, &
                       imager_pavolonis, skt, channel_info, platform, &
                       do_nasa)

#ifdef INCLUDE_SEVIRI_NEURALNET
     use SEVIRI_NEURAL_NET_M
#endif
     use preproc_constants_m
     use imager_structures_m
     use channel_structures_m

     ! arguments
     integer,                     intent(in)     :: cview
     type(imager_flags_t),        intent(in)     :: imager_flags
     type(imager_angles_t),       intent(in)     :: imager_angles
     type(imager_geolocation_t),  intent(in)     :: imager_geolocation
     type(imager_measurements_t), intent(in)     :: imager_measurements
     type(imager_pavolonis_t),    intent(inout)  :: imager_pavolonis
     type(channel_info_t),        intent(in)     :: channel_info
     real(kind=sreal),            intent(in)     :: &
          skt(imager_geolocation%startx:imager_geolocation%endx, &
              1:imager_geolocation%ny)
     character(len=*),             intent(in)     :: platform
     logical,                     intent(in)     :: do_nasa

     integer(kind=1) :: msg_index
     logical(kind=1) :: undo_true_reflectances = .false.
     integer :: ch1, ch2, ch3, ch4, ch5, ch6, ch7, ch9, ch10, ch11

     ! channel indices
     ch1 = 1
     ch2 = 2
     ch3 = 3
     ch4 = 4
     ch5 = 5
     ch6 = 6
     ch7 = 7
     ch9 = 9
     ch10 = 10
     ch11 = 11

     select case (trim(platform))
     case ("MSG1")
         msg_index = 1
     case ("MSG2")
         msg_index = 2
     case ("MSG3")
         msg_index = 3
     case ("MSG4")
         msg_index = 4
     case default
         write(*,*) "Platform ", platform, " not supported with seviri_ml."
         stop
     end select

     ! if VIS calibration is not NASA (IMPF) do not apply NASA-to-IMPF
     ! conversion for ANNs
     if (.not. do_nasa) then
         msg_index = 0
     end if

#ifdef INCLUDE_SEVIRI_NEURALNET
     write(*,*) "PREDICTING Multilayer flag"
     ! run external ANN
     call seviri_ann_mlay(imager_geolocation%nx, &      ! xdim for reshaping
                imager_geolocation%ny, &               ! ydim for reshaping
                imager_measurements%data(:,:,ch1), &   ! VIS006
                imager_measurements%data(:,:,ch2), &   ! VIS008
                imager_measurements%data(:,:,ch3), &   ! NIR016
                imager_measurements%data(:,:,ch4), &   ! IR039
                imager_measurements%data(:,:,ch5), &   ! IR062
                imager_measurements%data(:,:,ch6), &   ! IR073
                imager_measurements%data(:,:,ch7), &   ! IR087
                imager_measurements%data(:,:,ch9), &   ! IR108
                imager_measurements%data(:,:,ch10), &  ! IR120
                imager_measurements%data(:,:,ch11), &  ! IR134
                imager_flags%lsflag, &                 ! Land-sea mask
                skt, &                                 ! ECMWF skin temp
                imager_angles%solzen(:,:,cview), &     ! Solar Zenith Angle
                imager_angles%satzen(:,:,cview), &     ! Satellite Zenith Angle
                imager_pavolonis%mlay_prob(:,:,cview), &   ! Multilayer probability
                imager_pavolonis%mlay_flag(:,:,cview), &   ! Multilayer flag
                imager_pavolonis%mlay_unc(:,:,cview), &    ! Multilayer uncertainty
                imager_pavolonis%cldmask(:,:,cview), &
                msg_index, &
                undo_true_reflectances)

#else
     write(*,*) 'ERROR: ORAC has been compiled without SEVIRI neural &
                &network support. (1) Compile the SEVIRI neural &
                &network in ../seviri_neuralnet. (2) Link the modules &
                &to the ORAC pre_processor compilation. (3) Recompile &
                &ORAC with -DINCLUDE_SEVIRI_NEURALNET.'
     stop error_stop_code
#endif

end subroutine mlay_seviri

end module seviri_neural_net_preproc_m
