# Make file for orac preprocessing
#
# History:
# 2011/12/09, Matthias Jerg: Created initial makefile to start code development
#    for the orac preprocessing, used orac makefile as template.
# 2012/01/23, MJ: Includes grib_api,netcdf,hdf4,hdf5 recently.
# 2012/05/01, Gareth Thomas: Added surface reflectance routines and hdf-eos lib
# 2012/05/31, Gareth Thomas: Added surface emissivity routines.
# 2012/08/24, MJ: Creates adds "includes" for better configurability
# 2013/08/01, Adam Povey: Save .o and .mod files to a seperate folder.
# 2013/12/03, Greg McGarragh: A couple of small fixes.
# 2013/12/03, Greg McGarragh: Renewed object list and dependencies and some small
#    clean ups.
#
# Notes:
# It appears that -lopenjpeg needs to be included in the LIBS list since on some
# systems RTTOV seems to require it.
#

# Set files from which compiler, flags, and libraries are set. archfile also
# defines the "$(OBJS)" variable, which places object and module files in a
# subdirectory
#archfile=arch.ifort.ox
#libfile=lib.ifort.ox

# ral/ox files are above, this is for dwd/ecmwf:
archfile=arch.ecmwf.c2a.xlf
libfile=lib.ecmwf.c2a.xlf

# Can even be set through env. variable
#archfile=$(orac_arch)
#libfile=$(orac_lib)

include $(archfile)
include $(libfile)


# Define object files to be linked. N.B. files containing module definitions
# should be listed before main program units and subroutines that use those
# modules so that the modules are known to the compiler.
PREPROC_OBJ=$(OBJS)/aatsr_corrections.o $(OBJS)/aatsr_drift_correction_def.o \
	    $(OBJS)/allocate_channel_info.o $(OBJS)/allocate_ecmwf_structures.o \
	    $(OBJS)/allocate_ecmwf_structures_nc.o \
	    $(OBJS)/allocate_imager_structures.o \
	    $(OBJS)/allocate_preproc_structures.o \
	    $(OBJS)/allocate_surface_structures.o $(OBJS)/attribute_structures.o \
	    $(OBJS)/avhrr2oraclsflag.o $(OBJS)/bright_m.o \
	    $(OBJS)/build_preproc_fields.o $(OBJS)/calculate_rt.o \
	    $(OBJS)/calender.o $(OBJS)/call_rtm_ir_rttov.o \
	    $(OBJS)/call_rtm_solar_rttov.o $(OBJS)/channel_structures.o \
	    $(OBJS)/close_netcdf_output.o $(OBJS)/compute_geopot_coordinate.o \
	    $(OBJS)/correct_for_ice_snow.o $(OBJS)/cox_munk.o \
	    $(OBJS)/create_time_for_pixel.o $(OBJS)/date_type_structure.o \
	    $(OBJS)/deallocate_channel_info.o \
	    $(OBJS)/deallocate_ecmwf_structures.o \
	    $(OBJS)/deallocate_imager_structures.o \
	    $(OBJS)/deallocate_preproc_structures.o \
	    $(OBJS)/deallocate_surface_structures.o $(OBJS)/define_preprop_grid.o \
	    $(OBJS)/ecmwf_structures.o $(OBJS)/effective_2way_za.o \
	    $(OBJS)/emis_def.o $(OBJS)/fill_grid.o $(OBJS)/find_min_max_preproc.o \
	    $(OBJS)/get_coefs_info.o $(OBJS)/get_modis_time.o \
	    $(OBJS)/get_surface_emissivity.o \
	    $(OBJS)/get_surface_reflectance_lam.o $(OBJS)/imager_structures.o \
	    $(OBJS)/int_ecmwf.o $(OBJS)/interpol_bilinear.o \
	    $(OBJS)/interpol_nearest_neighbour.o $(OBJS)/interpolate_to3h2o.o \
	    $(OBJS)/ls.o $(OBJS)/make_preprop_grid.o $(OBJS)/map_time_to_pixel.o \
	    $(OBJS)/mcd43c.o $(OBJS)/modis2oraclsflag.o $(OBJS)/modis_bright.o \
	    $(OBJS)/nc_close.o $(OBJS)/nc_create_file.o $(OBJS)/nc_dim_info.o \
	    $(OBJS)/nc_open.o $(OBJS)/nc_read_file.o $(OBJS)/netcdf_structures.o \
	    $(OBJS)/nise_def.o $(OBJS)/nn_ecmwf.o $(OBJS)/open_netcdf_output.o \
	    $(OBJS)/preparation.o $(OBJS)/preproc_constants.o \
	    $(OBJS)/preproc_structures.o $(OBJS)/preprocessing_for_orac.o \
	    $(OBJS)/read_aatsr_dimensions.o $(OBJS)/read_aatsr_l1b.o \
	    $(OBJS)/read_avhrr_angles.o $(OBJS)/read_avhrr_dimensions.o \
	    $(OBJS)/read_avhrr_geo.o $(OBJS)/read_avhrr_l1b.o \
	    $(OBJS)/read_avhrr_landseamask.o $(OBJS)/read_avhrr_lat_lon.o \
	    $(OBJS)/read_avhrr_lsmask.o $(OBJS)/read_avhrr_time.o \
	    $(OBJS)/read_cimss_emissivity.o $(OBJS)/read_ecmwf.o \
	    $(OBJS)/read_ecmwf_dimensions.o $(OBJS)/read_ecmwf_dimensions_nc.o \
	    $(OBJS)/read_ecmwf_lat_lon.o $(OBJS)/read_ecmwf_lat_lon_nc.o \
	    $(OBJS)/read_ecmwf_nc.o $(OBJS)/read_imager.o \
	    $(OBJS)/read_L1B_avhrr_reflectances_radiances.o \
	    $(OBJS)/read_L1b_radiances_reflectances.o $(OBJS)/read_mcd43c3.o \
	    $(OBJS)/read_modis_angles.o $(OBJS)/read_modis_dimensions.o \
	    $(OBJS)/read_modis_geo.o $(OBJS)/read_modis_l1b.o \
	    $(OBJS)/read_modis_lat_lon.o $(OBJS)/read_modis_lsflag.o \
	    $(OBJS)/read_modis_time.o $(OBJS)/read_nsidc_nise.o \
	    $(OBJS)/rearrange_ecmwf.o $(OBJS)/rearrange_ecmwf3.o \
	    $(OBJS)/rttov_driver.o $(OBJS)/select_modis_albedo_file.o \
	    $(OBJS)/select_modis_emiss_file.o $(OBJS)/set_ecmwf.o $(OBJS)/setup.o \
	    $(OBJS)/surface_structures.o $(OBJS)/test_rttov.o \
	    $(OBJS)/write_swath_to_netcdf.o $(OBJS)/read_aatsr_orbit.o


# Compilation rules
$(OBJS)/%.o: %.f90
	$(F90) -o $@ -c $(FFLAGS) $(INC) $(AUXFLAGS) $<

$(OBJS)/%.o: %.F90
	$(F90) -o $@ -c $(FFLAGS) $(INC) $(AUXFLAGS) $<

$(OBJS)/%.o: %.c
	$(CC) -o $@ -c $(CFLAGS) $(CINC) $<

# Rule to prevent make from identifying Fortran .mod files as Modula2 source
# files
%.o : %.mod


# Main program build
orac_preproc.x: $(PREPROC_OBJ) Makefile
	$(F90) -o orac_preproc.x $(LFLAGS) $(PREPROC_OBJ) $(LIBS) -I./$(OBJS)

tidy clean:
	rm -f $(OBJS)/*.o $(OBJS)/*.mod


# Utilities to change source code extensions when moving between DEC Unix and
# Linux systems.
DEC2Linux:
	rename.csh d2l
	rename_svn.csh d2l

Linux2DEC:
	rename.csh l2d
	rename_svn.csh l2d


# Object/source dependencies
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
$(OBJS)/call_rtm_ir_rttov.o: $(OBJS)/ $(OBJS)/channel_structures.o \
	$(OBJS)/imager_structures.o $(OBJS)/netcdf_structures.o \
	$(OBJS)/preproc_constants.o $(OBJS)/preproc_structures.o
$(OBJS)/call_rtm_solar_rttov.o: $(OBJS)/ $(OBJS)/channel_structures.o \
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
$(OBJS)/define_preprop_grid.o: $(OBJS)/ecmwf_structures.o $(OBJS)/imager_structures.o \
	$(OBJS)/preproc_constants.o $(OBJS)/preproc_structures.o
$(OBJS)/ecmwf_structures.o: $(OBJS)/preproc_constants.o
$(OBJS)/fill_grid.o: $(OBJS)/preproc_constants.o
$(OBJS)/find_min_max_preproc.o: $(OBJS)/imager_structures.o \
	$(OBJS)/preproc_constants.o $(OBJS)/preproc_structures.o
$(OBJS)/get_coefs_info.o: $(OBJS)/preproc_constants.o
$(OBJS)/get_modis_time.o: $(OBJS)/imager_structures.o $(OBJS)/preproc_constants.o
$(OBJS)/get_surface_emissivity.o: $(OBJS)/ $(OBJS)/channel_structures.o \
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
$(OBJS)/int_ecmwf.o: $(OBJS)/ $(OBJS)/preproc_constants.o \
	$(OBJS)/preproc_structures.o
$(OBJS)/interpol_bilinear.o: $(OBJS)/preproc_constants.o
$(OBJS)/interpol_nearest_neighbour.o: $(OBJS)/preproc_constants.o
$(OBJS)/interpolate_to3h2o.o: $(OBJS)/preproc_constants.o
$(OBJS)/ls.o: $(OBJS)/preproc_constants.o
$(OBJS)/make_preprop_grid.o: $(OBJS)/preproc_constants.o $(OBJS)/preproc_structures.o
$(OBJS)/map_time_to_pixel.o: $(OBJS)/imager_structures.o $(OBJS)/preproc_constants.o
$(OBJS)/modis2oraclsflag.o: $(OBJS)/preproc_constants.o
$(OBJS)/nc_close.o: $(OBJS)/
$(OBJS)/nc_create_file.o: $(OBJS)/ $(OBJS)/attribute_structures.o \
	$(OBJS)/channel_structures.o $(OBJS)/imager_structures.o \
	$(OBJS)/netcdf_structures.o $(OBJS)/preproc_constants.o \
	$(OBJS)/preproc_structures.o
$(OBJS)/nc_dim_info.o: $(OBJS)/ $(OBJS)/preproc_constants.o
$(OBJS)/nc_open.o: $(OBJS)/
$(OBJS)/nc_read_file.o: $(OBJS)/ $(OBJS)/preproc_constants.o
$(OBJS)/netcdf_structures.o: $(OBJS)/preproc_constants.o
$(OBJS)/nise_def.o: $(OBJS)/nise_def.o
$(OBJS)/nn_ecmwf.o: $(OBJS)/ $(OBJS)/preproc_constants.o $(OBJS)/preproc_structures.o
$(OBJS)/open_netcdf_output.o: $(OBJS)/attribute_structures.o \
	$(OBJS)/channel_structures.o $(OBJS)/imager_structures.o \
	$(OBJS)/netcdf_structures.o $(OBJS)/preproc_constants.o \
	$(OBJS)/preproc_structures.o
$(OBJS)/preparation.o: $(OBJS)/attribute_structures.o $(OBJS)/imager_structures.o \
	$(OBJS)/preproc_constants.o
$(OBJS)/preproc_structures.o: $(OBJS)/preproc_constants.o
$(OBJS)/preprocessing_for_orac.o: $(OBJS)/ $(OBJS)/attribute_structures.o \
	$(OBJS)/channel_structures.o $(OBJS)/ecmwf_structures.o \
	$(OBJS)/imager_structures.o $(OBJS)/netcdf_structures.o \
	$(OBJS)/preproc_constants.o $(OBJS)/preproc_structures.o \
	$(OBJS)/surface_structures.o
$(OBJS)/read_aatsr_dimensions.o: $(OBJS)/ $(OBJS)/preproc_constants.o
$(OBJS)/read_aatsr_l1b.o: $(OBJS)/ $(OBJS)/aatsr_drift_correction_def.o \
	$(OBJS)/channel_structures.o $(OBJS)/imager_structures.o \
	$(OBJS)/preproc_constants.o
$(OBJS)/read_avhrr_angles.o: $(OBJS)/ $(OBJS)/preproc_constants.o
$(OBJS)/read_avhrr_dimensions.o: $(OBJS)/ $(OBJS)/preproc_constants.o
$(OBJS)/read_avhrr_geo.o: $(OBJS)/ $(OBJS)/imager_structures.o \
	$(OBJS)/preproc_constants.o
$(OBJS)/read_avhrr_l1b.o: $(OBJS)/ $(OBJS)/channel_structures.o \
	$(OBJS)/imager_structures.o $(OBJS)/preproc_constants.o
$(OBJS)/read_avhrr_landseamask.o: $(OBJS)/ $(OBJS)/preproc_constants.o
$(OBJS)/read_avhrr_lat_lon.o: $(OBJS)/ $(OBJS)/preproc_constants.o
$(OBJS)/read_avhrr_lsmask.o: $(OBJS)/ $(OBJS)/imager_structures.o \
	$(OBJS)/preproc_constants.o
$(OBJS)/read_avhrr_time.o: $(OBJS)/ $(OBJS)/preproc_constants.o
$(OBJS)/read_cimss_emissivity.o: $(OBJS)/ $(OBJS)/emis_def.o \
	$(OBJS)/preproc_constants.o
$(OBJS)/read_ecmwf.o: $(OBJS)/ $(OBJS)/ecmwf_structures.o $(OBJS)/preproc_constants.o \
	$(OBJS)/preproc_structures.o
$(OBJS)/read_ecmwf_dimensions.o: $(OBJS)/ $(OBJS)/ecmwf_structures.o \
	$(OBJS)/preproc_constants.o
$(OBJS)/read_ecmwf_dimensions_nc.o: $(OBJS)/ $(OBJS)/ecmwf_structures.o \
	$(OBJS)/preproc_constants.o
$(OBJS)/read_ecmwf_lat_lon.o: $(OBJS)/ $(OBJS)/ecmwf_structures.o \
	$(OBJS)/preproc_constants.o
$(OBJS)/read_ecmwf_lat_lon_nc.o: $(OBJS)/ $(OBJS)/ecmwf_structures.o \
	$(OBJS)/preproc_constants.o $(OBJS)/preproc_structures.o
$(OBJS)/read_ecmwf_nc.o: $(OBJS)/ $(OBJS)/ecmwf_structures.o \
	$(OBJS)/preproc_constants.o $(OBJS)/preproc_structures.o
$(OBJS)/read_imager.o: $(OBJS)/channel_structures.o $(OBJS)/imager_structures.o \
	$(OBJS)/preproc_constants.o
$(OBJS)/read_L1B_avhrr_reflectances_radiances.o: $(OBJS)/ $(OBJS)/preproc_constants.o
$(OBJS)/read_L1b_radiances_reflectances.o: $(OBJS)/imager_structures.o \
	$(OBJS)/preproc_constants.o
$(OBJS)/read_mcd43c3.o: $(OBJS)/mcd43c.o $(OBJS)/preproc_constants.o
$(OBJS)/read_modis_angles.o: $(OBJS)/preproc_constants.o
$(OBJS)/read_modis_dimensions.o: $(OBJS)/preproc_constants.o
$(OBJS)/read_modis_geo.o: $(OBJS)/imager_structures.o $(OBJS)/preproc_constants.o
$(OBJS)/read_modis_l1b.o: $(OBJS)/channel_structures.o $(OBJS)/imager_structures.o \
	$(OBJS)/preproc_constants.o
$(OBJS)/read_modis_lat_lon.o: $(OBJS)/preproc_constants.o
$(OBJS)/read_modis_lsflag.o: $(OBJS)/preproc_constants.o
$(OBJS)/read_modis_time.o: $(OBJS)/preproc_constants.o
$(OBJS)/read_nsidc_nise.o: $(OBJS)/nise_def.o
$(OBJS)/rearrange_ecmwf.o: $(OBJS)/ecmwf_structures.o $(OBJS)/preproc_structures.o
$(OBJS)/rearrange_ecmwf3.o: $(OBJS)/ecmwf_structures.o $(OBJS)/preproc_structures.o
$(OBJS)/rttov_driver.o: $(OBJS)/ $(OBJS)/channel_structures.o \
	$(OBJS)/ecmwf_structures.o $(OBJS)/imager_structures.o \
	$(OBJS)/netcdf_structures.o $(OBJS)/preproc_constants.o \
	$(OBJS)/preproc_structures.o
$(OBJS)/select_modis_albedo_file.o: $(OBJS)/preproc_structures.o
$(OBJS)/select_modis_emiss_file.o: $(OBJS)/preproc_structures.o
$(OBJS)/set_ecmwf.o: $(OBJS)/preproc_constants.o
$(OBJS)/setup.o: $(OBJS)/channel_structures.o $(OBJS)/date_type_structure.o \
	$(OBJS)/preproc_constants.o $(OBJS)/preproc_structures.o
$(OBJS)/surface_structures.o: $(OBJS)/preproc_constants.o
$(OBJS)/test_rttov.o: $(OBJS)/ $(OBJS)/preproc_constants.o \
	$(OBJS)/preproc_structures.o
$(OBJS)/write_swath_to_netcdf.o: $(OBJS)/ $(OBJS)/attribute_structures.o \
	$(OBJS)/channel_structures.o $(OBJS)/imager_structures.o \
	$(OBJS)/netcdf_structures.o $(OBJS)/preproc_constants.o \
	$(OBJS)/surface_structures.o
$(OBJS)/read_aatsr_orbit.o: read_aatsr_orbit.h
