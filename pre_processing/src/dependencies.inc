$(OBJS)/aatsr_corrections.o: $(OBJS)/calender.o $(OBJS)/date_type_structure.o \
        $(OBJS)/preproc_constants.o
$(OBJS)/attribute_structures.o: $(OBJS)/preproc_constants.o
$(OBJS)/bright_m.o: $(OBJS)/preproc_constants.o fundamental_constants.inc
$(OBJS)/build_preproc_fields.o: $(OBJS)/imager_structures.o \
        $(OBJS)/preproc_constants.o $(OBJS)/preproc_structures.o
$(OBJS)/calender.o: $(OBJS)/date_type_structure.o $(OBJS)/preproc_constants.o
$(OBJS)/channel_structures.o: $(OBJS)/preproc_constants.o \
        allocate_channel_info.F90 deallocate_channel_info.F90
$(OBJS)/correct_for_ice_snow.o: $(OBJS)/channel_structures.o \
        $(OBJS)/imager_structures.o $(OBJS)/interpol.o $(OBJS)/nise_def.o \
        $(OBJS)/preproc_constants.o $(OBJS)/preproc_structures.o \
        $(OBJS)/surface_structures.o
$(OBJS)/cox_munk.o: $(OBJS)/preproc_constants.o
$(OBJS)/date_type_structure.o: $(OBJS)/preproc_constants.o
$(OBJS)/define_preprop_grid.o: $(OBJS)/imager_structures.o \
        $(OBJS)/preproc_constants.o $(OBJS)/preproc_structures.o
$(OBJS)/ecmwf.o: $(OBJS)/orac_ncdf.o $(OBJS)/preproc_constants.o \
        $(OBJS)/preproc_structures.o compute_geopot_coordinate.F90 \
        deallocate_ecmwf_structures.F90 read_ecmwf_grib.F90 read_ecmwf_nc.F90 \
        read_ecmwf_wind_badc.F90 read_ecmwf_wind_grib.F90 read_ecmwf_wind_nc.F90 \
        rearrange_ecmwf.F90
$(OBJS)/extrap_into_tropopause.o: $(OBJS)/preproc_structures.o
$(OBJS)/fill_grid.o: $(OBJS)/preproc_constants.o
$(OBJS)/imager_structures.o: $(OBJS)/channel_structures.o \
        $(OBJS)/preproc_constants.o allocate_imager_structures.F90 \
        deallocate_imager_structures.F90
$(OBJS)/interpol.o: $(OBJS)/preproc_constants.o interpol_bilinear.F90 \
        interpol_nearest_neighbour.F90
$(OBJS)/map_time_to_pixel.o: $(OBJS)/imager_structures.o \
        $(OBJS)/preproc_constants.o
$(OBJS)/mcd43c.o: $(OBJS)/preproc_constants.o read_mcd43c3.F90
$(OBJS)/netcdf_output.o: $(OBJS)/attribute_structures.o \
        $(OBJS)/channel_structures.o $(OBJS)/imager_structures.o \
        $(OBJS)/netcdf_structures.o $(OBJS)/preproc_constants.o \
        $(OBJS)/preproc_structures.o $(OBJS)/surface_structures.o \
        close_netcdf_output.F90 nc_create_file.F90 open_netcdf_output.F90 \
        write_swath_to_netcdf.F90
$(OBJS)/netcdf_structures.o: $(OBJS)/preproc_constants.o
$(OBJS)/nise_def.o: read_nsidc_nise.F90
$(OBJS)/orac_ncdf.o: $(OBJS)/preproc_constants.o ncdf_read.inc
$(OBJS)/preparation.o: $(OBJS)/attribute_structures.o \
        $(OBJS)/imager_structures.o $(OBJS)/preproc_constants.o
$(OBJS)/preproc_structures.o: $(OBJS)/channel_structures.o \
        $(OBJS)/imager_structures.o $(OBJS)/preproc_constants.o \
        allocate_preproc_structures.F90 deallocate_preproc_structures.F90
$(OBJS)/preprocessing_for_orac.o: $(OBJS)/attribute_structures.o \
        $(OBJS)/channel_structures.o $(OBJS)/correct_for_ice_snow.o \
        $(OBJS)/ecmwf.o $(OBJS)/imager_structures.o $(OBJS)/netcdf_output.o \
        $(OBJS)/netcdf_structures.o $(OBJS)/preparation.o \
        $(OBJS)/preproc_constants.o $(OBJS)/preproc_structures.o \
        $(OBJS)/read_aatsr.o $(OBJS)/read_avhrr.o $(OBJS)/read_imager.o \
        $(OBJS)/read_modis.o $(OBJS)/rttov_driver.o $(OBJS)/setup.o \
        $(OBJS)/surface_emissivity.o $(OBJS)/surface_reflectance.o \
        $(OBJS)/surface_structures.o
$(OBJS)/read_aatsr.o: $(OBJS)/aatsr_corrections.o $(OBJS)/channel_structures.o \
        $(OBJS)/imager_structures.o $(OBJS)/preproc_constants.o \
        read_aatsr_dimensions.F90 read_aatsr_l1b.F90
$(OBJS)/read_avhrr.o: $(OBJS)/calender.o $(OBJS)/channel_structures.o \
        $(OBJS)/imager_structures.o $(OBJS)/preproc_constants.o \
        create_time_for_pixel.F90 read_L1B_avhrr_reflectances_radiances.F90 \
        read_avhrr_angles.F90 read_avhrr_dimensions.F90 read_avhrr_geo.F90 \
        read_avhrr_l1b.F90 read_avhrr_landseamask.F90 read_avhrr_lat_lon.F90 \
        read_avhrr_lsmask.F90 read_avhrr_time.F90
$(OBJS)/read_cimss_emissivity.o: $(OBJS)/orac_ncdf.o $(OBJS)/preproc_constants.o
$(OBJS)/read_imager.o: $(OBJS)/channel_structures.o $(OBJS)/imager_structures.o \
        $(OBJS)/preproc_constants.o $(OBJS)/read_aatsr.o $(OBJS)/read_avhrr.o \
        $(OBJS)/read_modis.o
$(OBJS)/read_modis.o: $(OBJS)/bright_m.o $(OBJS)/calender.o \
        $(OBJS)/channel_structures.o $(OBJS)/imager_structures.o \
        $(OBJS)/preproc_constants.o get_modis_time.F90 map_time_to_pixel.F90 \
        modis_bright.F90 read_L1b_radiances_reflectances.F90 \
        read_modis_angles.F90 read_modis_dimensions.F90 read_modis_geo.F90 \
        read_modis_l1b.F90 read_modis_lat_lon.F90 read_modis_lsflag.F90 \
        read_modis_time.F90
$(OBJS)/rttov_driver.o: $(OBJS)/channel_structures.o $(OBJS)/imager_structures.o \
        $(OBJS)/netcdf_structures.o $(OBJS)/preproc_constants.o \
        $(OBJS)/preproc_structures.o call_rtm_ir_rttov.F90 \
        call_rtm_solar_rttov.F90 effective_2way_za.F90
$(OBJS)/set_ecmwf.o: $(OBJS)/preproc_constants.o
$(OBJS)/setup.o: $(OBJS)/calender.o $(OBJS)/channel_structures.o \
        $(OBJS)/date_type_structure.o $(OBJS)/preproc_constants.o \
        $(OBJS)/preproc_structures.o
$(OBJS)/surface_emissivity.o: $(OBJS)/channel_structures.o \
        $(OBJS)/imager_structures.o $(OBJS)/interpol.o \
        $(OBJS)/preproc_constants.o $(OBJS)/preproc_structures.o \
        $(OBJS)/read_cimss_emissivity.o $(OBJS)/surface_structures.o \
        get_surface_emissivity.F90 select_modis_emiss_file.F90
$(OBJS)/surface_reflectance.o: $(OBJS)/channel_structures.o $(OBJS)/cox_munk.o \
        $(OBJS)/ecmwf.o $(OBJS)/fill_grid.o $(OBJS)/imager_structures.o \
        $(OBJS)/interpol.o $(OBJS)/mcd43c.o $(OBJS)/preproc_constants.o \
        $(OBJS)/preproc_structures.o $(OBJS)/surface_structures.o \
        get_surface_reflectance_lam.F90 select_modis_albedo_file.F90
$(OBJS)/surface_structures.o: $(OBJS)/channel_structures.o \
        $(OBJS)/imager_structures.o $(OBJS)/preproc_constants.o \
        $(OBJS)/preproc_structures.o allocate_surface_structures.F90 \
        deallocate_surface_structures.F90
$(OBJS)/test_rttov.o: $(OBJS)/preproc_constants.o $(OBJS)/preproc_structures.o
