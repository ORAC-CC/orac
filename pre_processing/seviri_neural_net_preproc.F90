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

subroutine get_msg_idx(msg_index, platform, do_nasa)
    character(len=*), intent(in)    :: platform
    integer(kind=1),  intent(inout) :: msg_index
    logical,          intent(in)    :: do_nasa

    select case (trim(platform))
        case ("MSG1","MSG-1")
            msg_index = 1
        case ("MSG2","MSG-2")
            msg_index = 2
        case ("MSG3","MSG-3")
            msg_index = 3
        case ("MSG4","MSG-4")
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
end subroutine get_msg_idx


subroutine cma_cph_seviri(cview, imager_flags, imager_angles, &
                          imager_geolocation, imager_measurements, ml_channels, &
                          imager_pavolonis, skt, channel_info, platform, &
                          do_nasa, verbose)

#ifdef INCLUDE_SEVIRI_NEURALNET
     use seviri_neural_net_m
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
     integer,                     intent(in)     :: ml_channels(10)
     type(imager_pavolonis_t),    intent(inout)  :: imager_pavolonis
     type(channel_info_t),        intent(in)     :: channel_info
     real(kind=sreal),            intent(in)     :: &
          skt(imager_geolocation%startx:imager_geolocation%endx, &
              1:imager_geolocation%ny)
     character(len=*),             intent(in)     :: platform
     logical,                      intent(in)     :: do_nasa
     logical,                      intent(in)     :: verbose

     integer(kind=1) :: msg_index
     logical(kind=1) :: undo_true_reflectances = .false.

     ! get Meteosat number from platform string
     call get_msg_idx(msg_index, platform, do_nasa)
#ifdef INCLUDE_SEVIRI_NEURALNET
     if (verbose) write(*,*) "PREDICTING COT/CPH"
     ! run external ANN
     call seviri_ann_cma(imager_geolocation%nx, &      ! xdim for reshaping
                imager_geolocation%ny, &               ! ydim for reshaping
                imager_measurements%data(:,:,ml_channels(1)), &   ! VIS006
                imager_measurements%data(:,:,ml_channels(2)), &   ! VIS008
                imager_measurements%data(:,:,ml_channels(3)), &   ! NIR016
                imager_measurements%data(:,:,ml_channels(4)), &   ! IR039
                imager_measurements%data(:,:,ml_channels(5)), &   ! IR062
                imager_measurements%data(:,:,ml_channels(6)), &   ! IR073
                imager_measurements%data(:,:,ml_channels(7)), &   ! IR087
                imager_measurements%data(:,:,ml_channels(8)), &   ! IR108
                imager_measurements%data(:,:,ml_channels(9)), &   ! IR120
                imager_measurements%data(:,:,ml_channels(10)), &  ! IR134
                imager_flags%lsflag, &                 ! Land-sea mask
                skt, &                                 ! ECMWF skin temp
                imager_angles%solzen(:,:,cview), &     ! Solar Zenith Angle
                imager_angles%satzen(:,:,cview), &     ! Satellite Zenith Angle
                imager_pavolonis%cccot_pre(:,:,cview), &
                imager_pavolonis%cldmask(:,:,cview), &
                imager_pavolonis%cldmask_uncertainty(:,:,cview), &
                msg_index, &
                undo_true_reflectances)

     call seviri_ann_cph(imager_geolocation%nx, &      ! xdim for reshaping
                imager_geolocation%ny, &               ! ydim for reshaping
                imager_measurements%data(:,:,ml_channels(1)), &   ! VIS006
                imager_measurements%data(:,:,ml_channels(2)), &   ! VIS008
                imager_measurements%data(:,:,ml_channels(3)), &   ! NIR016
                imager_measurements%data(:,:,ml_channels(4)), &   ! IR039
                imager_measurements%data(:,:,ml_channels(5)), &   ! IR062
                imager_measurements%data(:,:,ml_channels(6)), &   ! IR073
                imager_measurements%data(:,:,ml_channels(7)), &   ! IR087
                imager_measurements%data(:,:,ml_channels(8)), &   ! IR108
                imager_measurements%data(:,:,ml_channels(9)), &   ! IR120
                imager_measurements%data(:,:,ml_channels(10)), &  ! IR134
                imager_flags%lsflag, &                 ! Land-sea mask
                skt, &                                 ! ECMWF skin temp
                imager_angles%solzen(:,:,cview), &     ! Solar Zenith Angle
                imager_angles%satzen(:,:,cview), &     ! Satellite Zenith Angle
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
                         imager_geolocation, imager_measurements, ml_channels, &
                         imager_pavolonis, skt, channel_info, platform, &
                         do_nasa, verbose)

#ifdef INCLUDE_SEVIRI_NEURALNET
     use seviri_neural_net_m
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
     integer,                     intent(in)     :: ml_channels(10)
     type(imager_pavolonis_t),    intent(inout)  :: imager_pavolonis
     type(channel_info_t),        intent(in)     :: channel_info
     real(kind=sreal),            intent(in)     :: &
          skt(imager_geolocation%startx:imager_geolocation%endx, &
              1:imager_geolocation%ny)
     character(len=*),             intent(in)     :: platform
     logical,                      intent(in)     :: do_nasa
     logical,                      intent(in)     :: verbose

     integer(kind=1) :: msg_index
     logical(kind=1) :: undo_true_reflectances = .false.

     ! get Meteosat number from platform string
     call get_msg_idx(msg_index, platform, do_nasa)

#ifdef INCLUDE_SEVIRI_NEURALNET
     if (verbose) write(*,*) "PREDICTING CTP for first guess"
     ! run external ANN
     call seviri_ann_ctp(imager_geolocation%nx, &         ! xdim for reshaping
                imager_geolocation%ny, &                  ! ydim for reshaping
                imager_measurements%data(:,:,ml_channels(1)), &   ! VIS006
                imager_measurements%data(:,:,ml_channels(2)), &   ! VIS008
                imager_measurements%data(:,:,ml_channels(3)), &   ! NIR016
                imager_measurements%data(:,:,ml_channels(4)), &   ! IR039
                imager_measurements%data(:,:,ml_channels(5)), &   ! IR062
                imager_measurements%data(:,:,ml_channels(6)), &   ! IR073
                imager_measurements%data(:,:,ml_channels(7)), &   ! IR087
                imager_measurements%data(:,:,ml_channels(8)), &   ! IR108
                imager_measurements%data(:,:,ml_channels(9)), &   ! IR120
                imager_measurements%data(:,:,ml_channels(10)), &  ! IR134
                imager_flags%lsflag, &                    ! Land-sea mask
                skt, &                                    ! ECMWF skin temp
                imager_angles%solzen(:,:,cview), &        ! Solar Zenith Angle
                imager_angles%satzen(:,:,cview), &        ! Satellite Zenith Angle
                imager_pavolonis%ctp_fg(:,:,cview), &     ! CTP first guess
                imager_pavolonis%ctp_fg_unc(:,:,cview), & ! CTP first guess unc
                imager_pavolonis%cldmask(:,:,cview), &    ! Cloud mask
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
                       imager_geolocation, imager_measurements, ml_channels, &
                       imager_pavolonis, skt, channel_info, platform, &
                       do_nasa, verbose)

#ifdef INCLUDE_SEVIRI_NEURALNET
     use seviri_neural_net_m
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
     integer,                     intent(in)     :: ml_channels(10)
     type(imager_pavolonis_t),    intent(inout)  :: imager_pavolonis
     type(channel_info_t),        intent(in)     :: channel_info
     real(kind=sreal),            intent(in)     :: &
          skt(imager_geolocation%startx:imager_geolocation%endx, &
              1:imager_geolocation%ny)
     character(len=*),             intent(in)     :: platform
     logical,                      intent(in)     :: do_nasa
     logical,                      intent(in)     :: verbose

     integer(kind=1) :: msg_index
     logical(kind=1) :: undo_true_reflectances = .false.

     ! get Meteosat number from platform string
     call get_msg_idx(msg_index, platform, do_nasa)

#ifdef INCLUDE_SEVIRI_NEURALNET
     if (verbose) write(*,*) "PREDICTING MLAY for first guess"
     ! run external ANN
     call seviri_ann_mlay(imager_geolocation%nx, &       ! xdim for reshaping
                imager_geolocation%ny, &                 ! ydim for reshaping
                imager_measurements%data(:,:,ml_channels(1)), &   ! VIS006
                imager_measurements%data(:,:,ml_channels(2)), &   ! VIS008
                imager_measurements%data(:,:,ml_channels(3)), &   ! NIR016
                imager_measurements%data(:,:,ml_channels(4)), &   ! IR039
                imager_measurements%data(:,:,ml_channels(5)), &   ! IR062
                imager_measurements%data(:,:,ml_channels(6)), &   ! IR073
                imager_measurements%data(:,:,ml_channels(7)), &   ! IR087
                imager_measurements%data(:,:,ml_channels(8)), &   ! IR108
                imager_measurements%data(:,:,ml_channels(9)), &   ! IR120
                imager_measurements%data(:,:,ml_channels(10)), &  ! IR134
                imager_flags%lsflag, &                   ! Land-sea mask
                skt, &                                   ! ECMWF skin temp
                imager_angles%solzen(:,:,cview), &       ! Solar Zenith Angle
                imager_angles%satzen(:,:,cview), &       ! Satellite Zenith Angle
                imager_pavolonis%mlay_prob(:,:,cview), & ! Multilayer probability
                imager_pavolonis%mlay_flag(:,:,cview), & ! Multilayer flag
                imager_pavolonis%mlay_unc(:,:,cview), &  ! Multilayer uncertainty
                imager_pavolonis%cldmask(:,:,cview), &   ! Cloud mask
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
