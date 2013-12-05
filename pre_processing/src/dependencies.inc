$(OBJS)/aatsr_corrections.o: $(OBJS)/aatsr_drift_correction_def.o \
        $(OBJS)/date_type_structure.o $(OBJS)/preproc_constants.o
$(OBJS)/aatsr_drift_correction_def.o: $(OBJS)/preproc_constants.o
$(OBJS)/allocate_channel_info.o: $(OBJS)/channel_structures.o \
        $(OBJS)/imager_structures.o $(OBJS)/preproc_constants.o
$(OBJS)/allocate_ecmwf_structures.o: $(OBJS)/ecmwf_structures.o \
        $(OBJS)/preproc_constants.o
$(OBJS)/allocate_ecmwf_structures_nc.o: $(OBJS)/ecmwf_structures.o \
        $(OBJS)/preproc_constants.o
$(OBJS)/allocate_imager_structures.o: $(OBJS)/channel_structures.o \
        $(OBJS)/imager_structures.o $(OBJS)/preproc_constants.o
$(OBJS)/allocate_preproc_structures.o: $(OBJS)/channel_structures.o \
        $(OBJS)/ecmwf_structures.o $(OBJS)/imager_structures.o \
        $(OBJS)/preproc_constants.o $(OBJS)/preproc_structures.o
$(OBJS)/allocate_surface_structures.o: $(OBJS)/channel_structures.o \
        $(OBJS)/imager_structures.o $(OBJS)/preproc_constants.o \
        $(OBJS)/preproc_structures.o $(OBJS)/surface_structures.o
$(OBJS)/attribute_structures.o: $(OBJS)/preproc_constants.o
$(OBJS)/avhrr2oraclsflag.o: $(OBJS)/preproc_constants.o
$(OBJS)/bright_m.o: $(OBJS)/preproc_constants.o fundamental_constants.inc
$(OBJS)/build_preproc_fields.o: $(OBJS)/imager_structures.o \
        $(OBJS)/preproc_constants.o $(OBJS)/preproc_structures.o
$(OBJS)/calculate_rt.o: $(OBJS)/channel_structures.o $(OBJS)/ecmwf_structures.o \
        $(OBJS)/imager_structures.o $(OBJS)/netcdf_structures.o \
        $(OBJS)/preproc_constants.o $(OBJS)/preproc_structures.o
$(OBJS)/calender.o: $(OBJS)/date_type_structure.o $(OBJS)/preproc_constants.o
$(OBJS)/call_rtm_ir_rttov.o: $(OBJS)/channel_structures.o \
        $(OBJS)/imager_structures.o $(OBJS)/netcdf_structures.o \
        $(OBJS)/preproc_constants.o $(OBJS)/preproc_structures.o
$(OBJS)/call_rtm_solar_rttov.o: $(OBJS)/channel_structures.o \
        $(OBJS)/imager_structures.o $(OBJS)/netcdf_structures.o \
        $(OBJS)/preproc_constants.o $(OBJS)/preproc_structures.o
$(OBJS)/channel_structures.o: $(OBJS)/preproc_constants.o
$(OBJS)/close_netcdf_output.o: $(OBJS)/netcdf_structures.o \
        $(OBJS)/preproc_constants.o
$(OBJS)/compute_geopot_coordinate.o: $(OBJS)/ecmwf_structures.o \
        $(OBJS)/preproc_constants.o
$(OBJS)/correct_for_ice_snow.o: $(OBJS)/channel_structures.o \
        $(OBJS)/imager_structures.o $(OBJS)/interpol_bilinear.o \
        $(OBJS)/nise_def.o $(OBJS)/preproc_constants.o \
        $(OBJS)/preproc_structures.o $(OBJS)/surface_structures.o
$(OBJS)/cox_munk.o: $(OBJS)/preproc_constants.o
$(OBJS)/create_time_for_pixel.o: $(OBJS)/imager_structures.o \
        $(OBJS)/preproc_constants.o
$(OBJS)/date_type_structure.o: $(OBJS)/preproc_constants.o
$(OBJS)/deallocate_channel_info.o: $(OBJS)/channel_structures.o
$(OBJS)/deallocate_ecmwf_structures.o: $(OBJS)/ecmwf_structures.o \
        $(OBJS)/preproc_constants.o
$(OBJS)/deallocate_imager_structures.o: $(OBJS)/imager_structures.o \
        $(OBJS)/preproc_constants.o
$(OBJS)/deallocate_preproc_structures.o: $(OBJS)/channel_structures.o \
        $(OBJS)/preproc_constants.o $(OBJS)/preproc_structures.o
$(OBJS)/deallocate_surface_structures.o: $(OBJS)/preproc_constants.o \
        $(OBJS)/surface_structures.o
$(OBJS)/define_preprop_grid.o: $(OBJS)/ecmwf_structures.o \
        $(OBJS)/imager_structures.o $(OBJS)/preproc_constants.o \
        $(OBJS)/preproc_structures.o
$(OBJS)/ecmwf_structures.o: $(OBJS)/preproc_constants.o
$(OBJS)/fill_grid.o: $(OBJS)/preproc_constants.o
$(OBJS)/find_min_max_preproc.o: $(OBJS)/imager_structures.o \
        $(OBJS)/preproc_constants.o $(OBJS)/preproc_structures.o
$(OBJS)/get_coefs_info.o: $(OBJS)/preproc_constants.o
$(OBJS)/get_modis_time.o: $(OBJS)/imager_structures.o \
        $(OBJS)/preproc_constants.o
$(OBJS)/get_surface_emissivity.o: $(OBJS)/channel_structures.o \
        $(OBJS)/emis_def.o $(OBJS)/imager_structures.o \
        $(OBJS)/interpol_bilinear.o $(OBJS)/interpol_nearest_neighbour.o \
        $(OBJS)/preproc_constants.o $(OBJS)/preproc_structures.o \
        $(OBJS)/surface_structures.o
$(OBJS)/get_surface_reflectance_lam.o: $(OBJS)/channel_structures.o \
        $(OBJS)/ecmwf_structures.o $(OBJS)/fill_grid.o \
        $(OBJS)/imager_structures.o $(OBJS)/interpol_bilinear.o \
        $(OBJS)/interpol_nearest_neighbour.o $(OBJS)/mcd43c.o \
        $(OBJS)/preproc_constants.o $(OBJS)/preproc_structures.o \
        $(OBJS)/surface_structures.o
$(OBJS)/imager_structures.o: $(OBJS)/preproc_constants.o
$(OBJS)/int_ecmwf.o: $(OBJS)/preproc_constants.o $(OBJS)/preproc_structures.o
$(OBJS)/interpol_bilinear.o: $(OBJS)/preproc_constants.o
$(OBJS)/interpol_nearest_neighbour.o: $(OBJS)/preproc_constants.o
$(OBJS)/interpolate_to3h2o.o: $(OBJS)/preproc_constants.o
$(OBJS)/ls.o: $(OBJS)/preproc_constants.o
$(OBJS)/make_preprop_grid.o: $(OBJS)/preproc_constants.o \
        $(OBJS)/preproc_structures.o
$(OBJS)/map_time_to_pixel.o: $(OBJS)/imager_structures.o \
        $(OBJS)/preproc_constants.o
$(OBJS)/modis2oraclsflag.o: $(OBJS)/preproc_constants.o

$(OBJS)/nc_create_file.o: $(OBJS)/attribute_structures.o \
        $(OBJS)/channel_structures.o $(OBJS)/imager_structures.o \
        $(OBJS)/netcdf_structures.o $(OBJS)/preproc_constants.o \
        $(OBJS)/preproc_structures.o
$(OBJS)/nc_dim_info.o: $(OBJS)/preproc_constants.o

$(OBJS)/nc_read_file.o: $(OBJS)/preproc_constants.o
$(OBJS)/netcdf_structures.o: $(OBJS)/preproc_constants.o

$(OBJS)/nn_ecmwf.o: $(OBJS)/preproc_constants.o $(OBJS)/preproc_structures.o
$(OBJS)/open_netcdf_output.o: $(OBJS)/attribute_structures.o \
        $(OBJS)/channel_structures.o $(OBJS)/imager_structures.o \
        $(OBJS)/netcdf_structures.o $(OBJS)/preproc_constants.o \
        $(OBJS)/preproc_structures.o
$(OBJS)/preparation.o: $(OBJS)/attribute_structures.o \
        $(OBJS)/imager_structures.o $(OBJS)/preproc_constants.o
$(OBJS)/preproc_structures.o: $(OBJS)/preproc_constants.o
$(OBJS)/preprocessing_for_orac.o: $(OBJS)/attribute_structures.o \
        $(OBJS)/channel_structures.o $(OBJS)/ecmwf_structures.o \
        $(OBJS)/imager_structures.o $(OBJS)/netcdf_structures.o \
        $(OBJS)/preproc_constants.o $(OBJS)/preproc_structures.o \
        $(OBJS)/surface_structures.o
$(OBJS)/read_aatsr_dimensions.o: $(OBJS)/preproc_constants.o
$(OBJS)/read_aatsr_l1b.o: $(OBJS)/aatsr_drift_correction_def.o \
        $(OBJS)/channel_structures.o $(OBJS)/imager_structures.o \
        $(OBJS)/preproc_constants.o
$(OBJS)/read_avhrr_angles.o: $(OBJS)/preproc_constants.o
$(OBJS)/read_avhrr_dimensions.o: $(OBJS)/preproc_constants.o
$(OBJS)/read_avhrr_geo.o: $(OBJS)/imager_structures.o \
        $(OBJS)/preproc_constants.o
$(OBJS)/read_avhrr_l1b.o: $(OBJS)/channel_structures.o \
        $(OBJS)/imager_structures.o $(OBJS)/preproc_constants.o
$(OBJS)/read_avhrr_landseamask.o: $(OBJS)/preproc_constants.o
$(OBJS)/read_avhrr_lat_lon.o: $(OBJS)/preproc_constants.o
$(OBJS)/read_avhrr_lsmask.o: $(OBJS)/imager_structures.o \
        $(OBJS)/preproc_constants.o
$(OBJS)/read_avhrr_time.o: $(OBJS)/preproc_constants.o
$(OBJS)/read_cimss_emissivity.o: $(OBJS)/emis_def.o $(OBJS)/preproc_constants.o
$(OBJS)/read_ecmwf.o: $(OBJS)/ecmwf_structures.o $(OBJS)/preproc_constants.o \
        $(OBJS)/preproc_structures.o
$(OBJS)/read_ecmwf_dimensions.o: $(OBJS)/ecmwf_structures.o \
        $(OBJS)/preproc_constants.o
$(OBJS)/read_ecmwf_dimensions_nc.o: $(OBJS)/ecmwf_structures.o \
        $(OBJS)/preproc_constants.o
$(OBJS)/read_ecmwf_lat_lon.o: $(OBJS)/ecmwf_structures.o \
        $(OBJS)/preproc_constants.o
$(OBJS)/read_ecmwf_lat_lon_nc.o: $(OBJS)/ecmwf_structures.o \
        $(OBJS)/preproc_constants.o $(OBJS)/preproc_structures.o
$(OBJS)/read_ecmwf_nc.o: $(OBJS)/ecmwf_structures.o $(OBJS)/preproc_constants.o \
        $(OBJS)/preproc_structures.o
$(OBJS)/read_imager.o: $(OBJS)/channel_structures.o $(OBJS)/imager_structures.o \
        $(OBJS)/preproc_constants.o
$(OBJS)/read_L1B_avhrr_reflectances_radiances.o: $(OBJS)/preproc_constants.o
$(OBJS)/read_L1b_radiances_reflectances.o: $(OBJS)/imager_structures.o \
        $(OBJS)/preproc_constants.o
$(OBJS)/read_mcd43c3.o: $(OBJS)/mcd43c.o $(OBJS)/preproc_constants.o
$(OBJS)/read_modis_angles.o: $(OBJS)/preproc_constants.o
$(OBJS)/read_modis_dimensions.o: $(OBJS)/preproc_constants.o
$(OBJS)/read_modis_geo.o: $(OBJS)/imager_structures.o \
        $(OBJS)/preproc_constants.o
$(OBJS)/read_modis_l1b.o: $(OBJS)/channel_structures.o \
        $(OBJS)/imager_structures.o $(OBJS)/preproc_constants.o
$(OBJS)/read_modis_lat_lon.o: $(OBJS)/preproc_constants.o
$(OBJS)/read_modis_lsflag.o: $(OBJS)/preproc_constants.o
$(OBJS)/read_modis_time.o: $(OBJS)/preproc_constants.o
$(OBJS)/read_nsidc_nise.o: $(OBJS)/nise_def.o
$(OBJS)/rearrange_ecmwf.o: $(OBJS)/ecmwf_structures.o \
        $(OBJS)/preproc_structures.o
$(OBJS)/rearrange_ecmwf3.o: $(OBJS)/ecmwf_structures.o \
        $(OBJS)/preproc_structures.o
$(OBJS)/rttov_driver.o: $(OBJS)/channel_structures.o $(OBJS)/ecmwf_structures.o \
        $(OBJS)/imager_structures.o $(OBJS)/netcdf_structures.o \
        $(OBJS)/preproc_constants.o $(OBJS)/preproc_structures.o
$(OBJS)/select_modis_albedo_file.o: $(OBJS)/preproc_structures.o
$(OBJS)/select_modis_emiss_file.o: $(OBJS)/preproc_structures.o
$(OBJS)/set_ecmwf.o: $(OBJS)/preproc_constants.o
$(OBJS)/setup.o: $(OBJS)/channel_structures.o $(OBJS)/date_type_structure.o \
        $(OBJS)/preproc_constants.o $(OBJS)/preproc_structures.o
$(OBJS)/surface_structures.o: $(OBJS)/preproc_constants.o
$(OBJS)/test_rttov.o: $(OBJS)/preproc_constants.o $(OBJS)/preproc_structures.o
$(OBJS)/write_swath_to_netcdf.o: $(OBJS)/attribute_structures.o \
        $(OBJS)/channel_structures.o $(OBJS)/imager_structures.o \
        $(OBJS)/netcdf_structures.o $(OBJS)/preproc_constants.o \
        $(OBJS)/surface_structures.o
