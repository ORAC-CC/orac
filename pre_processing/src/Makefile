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
# 2013/12/03, Greg McGarragh: Renewed object list and dependencies and some
#    small clean ups.
# 2013/12/04, Greg McGarragh: Added 'make depend' to automatically find all
#    Fortran file dependencies.
#
# Notes:
# It appears that -lopenjpeg needs to be included in the LIBS list since on some
# systems RTTOV seems to require it.
#

# Set files from which compiler, flags, and libraries are set. archfile also
# defines the "$(OBJS)" variable, which places object and module files in a
# subdirectory
include $(ORAC_ARCH)
include $(ORAC_LIB)


# Define object files to be linked. N.B. files containing module definitions
# should be listed before main program units and subroutines that use those
# modules so that the modules are known to the compiler.
PREPROC_OBJ_F90=$(OBJS)/aatsr_corrections.o \
                $(OBJS)/aatsr_drift_correction_def.o \
	        $(OBJS)/allocate_channel_info.o \
                $(OBJS)/allocate_ecmwf_structures.o \
	        $(OBJS)/allocate_imager_structures.o \
	        $(OBJS)/allocate_preproc_structures.o \
	        $(OBJS)/allocate_surface_structures.o \
                $(OBJS)/attribute_structures.o \
	        $(OBJS)/avhrr2oraclsflag.o \
                $(OBJS)/bright_m.o \
	        $(OBJS)/build_preproc_fields.o \
                $(OBJS)/calculate_rt.o \
	        $(OBJS)/calender.o \
                $(OBJS)/call_rtm_ir_rttov.o \
	        $(OBJS)/call_rtm_solar_rttov.o \
                $(OBJS)/channel_structures.o \
	        $(OBJS)/close_netcdf_output.o \
	        $(OBJS)/correct_for_ice_snow.o \
                $(OBJS)/cox_munk.o \
	        $(OBJS)/create_time_for_pixel.o \
                $(OBJS)/date_type_structure.o \
	        $(OBJS)/deallocate_channel_info.o \
	        $(OBJS)/deallocate_ecmwf_structures.o \
	        $(OBJS)/deallocate_imager_structures.o \
	        $(OBJS)/deallocate_preproc_structures.o \
	        $(OBJS)/deallocate_surface_structures.o \
                $(OBJS)/define_preprop_grid.o \
	        $(OBJS)/ecmwf_structures.o \
                $(OBJS)/effective_2way_za.o \
	        $(OBJS)/emis_def.o $(OBJS)/fill_grid.o \
                $(OBJS)/find_min_max_preproc.o \
	        $(OBJS)/get_coefs_info.o \
                $(OBJS)/get_modis_time.o \
	        $(OBJS)/get_surface_emissivity.o \
	        $(OBJS)/get_surface_reflectance_lam.o \
                $(OBJS)/imager_structures.o \
	        $(OBJS)/int_ecmwf.o \
                $(OBJS)/interpol_bilinear.o \
	        $(OBJS)/interpol_nearest_neighbour.o \
                $(OBJS)/interpolate_to3h2o.o \
	        $(OBJS)/ls.o \
                $(OBJS)/make_preprop_grid.o \
                $(OBJS)/map_time_to_pixel.o \
	        $(OBJS)/mcd43c.o \
                $(OBJS)/modis2oraclsflag.o \
                $(OBJS)/modis_bright.o \
                $(OBJS)/nc_create_file.o \
                $(OBJS)/netcdf_structures.o \
	        $(OBJS)/nise_def.o \
                $(OBJS)/nn_ecmwf.o \
                $(OBJS)/open_netcdf_output.o \
                $(OBJS)/orac_ecmwf.o \
                $(OBJS)/orac_ncdf.o \
	        $(OBJS)/preparation.o \
                $(OBJS)/preproc_constants.o \
	        $(OBJS)/preproc_structures.o \
                $(OBJS)/preprocessing_for_orac.o \
	        $(OBJS)/read_aatsr_dimensions.o \
                $(OBJS)/read_aatsr_l1b.o \
	        $(OBJS)/read_avhrr_angles.o \
                $(OBJS)/read_avhrr_dimensions.o \
	        $(OBJS)/read_avhrr_geo.o \
                $(OBJS)/read_avhrr_l1b.o \
	        $(OBJS)/read_avhrr_landseamask.o \
                $(OBJS)/read_avhrr_lat_lon.o \
	        $(OBJS)/read_avhrr_lsmask.o \
                $(OBJS)/read_avhrr_time.o \
	        $(OBJS)/read_cimss_emissivity.o \
                $(OBJS)/read_ecmwf.o \
	        $(OBJS)/read_ecmwf_dimensions.o \
	        $(OBJS)/read_ecmwf_lat_lon.o \
                $(OBJS)/read_imager.o \
	        $(OBJS)/read_L1B_avhrr_reflectances_radiances.o \
	        $(OBJS)/read_L1b_radiances_reflectances.o \
                $(OBJS)/read_mcd43c3.o \
	        $(OBJS)/read_modis_angles.o \
                $(OBJS)/read_modis_dimensions.o \
	        $(OBJS)/read_modis_geo.o \
                $(OBJS)/read_modis_l1b.o \
	        $(OBJS)/read_modis_lat_lon.o \
                $(OBJS)/read_modis_lsflag.o \
	        $(OBJS)/read_modis_time.o \
                $(OBJS)/read_nsidc_nise.o \
                $(OBJS)/rearrange_ecmwf.o \
	        $(OBJS)/rttov_driver.o \
                $(OBJS)/select_modis_albedo_file.o \
	        $(OBJS)/select_modis_emiss_file.o \
                $(OBJS)/set_ecmwf.o \
                $(OBJS)/setup.o \
	        $(OBJS)/surface_structures.o \
                $(OBJS)/test_rttov.o \
	        $(OBJS)/write_swath_to_netcdf.o

PREPROC_OBJ_C=  $(OBJS)/read_aatsr_orbit.o

PREPROC_OBJ  =  $(PREPROC_OBJ_F90) $(PREPROC_OBJ_C)

PREPROC_INC  =  compute_geopot_coordinate.F90 \
                fundamental_constants.inc \
                ncdf_read.inc \
                read_ecmwf_dimensions_nc.F90 \
                read_ecmwf_lat_lon_nc.F90 \
                read_ecmwf_nc.F90


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

# Rebuild dependencies housed in dependencies.inc
depend:
	@../../tools/make_depend.pl $(PREPROC_OBJ_F90) $(PREPROC_INC) > dependencies.inc

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


# Fortran 90 dependencies
include dependencies.inc

# Fortran 90 on C dependecies
read_aatsr_l1b.o: read_aatsr_orbit.o
