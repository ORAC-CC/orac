module seviri_neural_net_preproc_m
     
     implicit None

     contains


subroutine cma_cph_seviri(cview, imager_flags, imager_angles, imager_geolocation, &
                          imager_measurements, imager_pavolonis, skt, channel_info)

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

     ! channels
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

#ifdef INCLUDE_SEVIRI_NEURALNET
     write(*,*) "PREDICTING COT/CPH"     
     ! run ANN for COT (CMA)
     call seviri_ann_cph_cot(imager_geolocation%nx, &  ! xdim for reshaping
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
                imager_pavolonis%cccot_pre(:,:,cview), &
                imager_pavolonis%cldmask(:,:,cview), &
                imager_pavolonis%cldmask_uncertainty(:,:,cview), &
                imager_pavolonis%cphcot(:,:,cview), &
                imager_pavolonis%ann_phase(:,:,cview), &
                imager_pavolonis%ann_phase_uncertainty(:,:,cview))
     
#else
     write(*,*) 'ERROR: ORAC has been compiled without SEVIRI neural ' \\ & 
                'network support. (1) Compile the SEVIRI neural ' \\ &
                'network in ../seviri_neuralnet. (2) Link the modules' \\ &
                'to the ORAC pre_processor compilation. (3) Recompile ' \\ &
                'ORAC with -DINCLUDE_SEVIRI_NEURALNET.'
     stop error_stop_code     
#endif

end subroutine cma_cph_seviri
end module seviri_neural_net_preproc_m
