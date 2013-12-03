# Make file for orac preprocessing
#
# History:
# 2011/12/09    Matthias Jerg: created initial makefile to start code development for the orac 
#	 	  preprocessing, used orac makefile as template.
# 2012/01/23	MJ: includes grib_api,netcdf,hdf4,hdf5 recently.
# 2012/05/01    Gareth Thomas: Added surface reflectance routines and hdf-eos lib
# 2012/05/31    Gareth Thomas: Added surface emissivity routines.
# 2012/08/24    MJ: creates adds "includes" for better configurability
# 2013/08/01    Adam Povey: Save .o and .mod files to a seperate folder.
# 2013/12/03    G McGarragh: A couple of small fixes.

# Notes:
# - It appears that -lopenjpeg needs to be included in the LIBS list on some systems
#   RTTOV seems to require it.
#   
#

# load files listing compiler, flags, and libraries
#archfile=arch.ifort.ox
#libfile=lib.ifort.ox

#ral/ox files are above, this is for dwd/ecmwf:
archfile=arch.ecmwf.c2a.xlf
libfile=lib.ecmwf.c2a.xlf

include $(archfile)
include $(libfile)


# Define object files to be linked. N.B. files containing module definitions
# should be listed before main program units and subroutines that use those
# modules so that the modules are known to the compiler. 
PREPROC_OBJ=	$(OBJS)/preproc_constants.o \
		$(OBJS)/modis_bright.o $(OBJS)/attribute_structures.o \
		$(OBJS)/interpol_bilinear.o $(OBJS)/surface_structures.o \
	        $(OBJS)/fill_grid.o $(OBJS)/emis_def.o \
		$(OBJS)/netcdf_structures.o $(OBJS)/channel_structures.o \
		$(OBJS)/interpol_nearest_neighbour.o \
		$(OBJS)/preproc_structures.o \
		$(OBJS)/mcd43c.o $(OBJS)/imager_structures.o \
		$(OBJS)/aatsr_drift_correction_def.o \
		$(OBJS)/date_type_structure.o \
		$(OBJS)/nise_def.o $(OBJS)/ecmwf_structures.o \
		$(OBJS)/preprocessing_for_orac.o \
		$(OBJS)/select_modis_emiss_file.o \
		$(OBJS)/calender.o $(OBJS)/write_swath_to_netcdf.o \
		$(OBJS)/setup.o \
		$(OBJS)/open_netcdf_output.o $(OBJS)/preparation.o \
		$(OBJS)/nc_create_file.o \
		$(OBJS)/close_netcdf_output.o $(OBJS)/nc_close.o \
		$(OBJS)/read_imager.o \
		$(OBJS)/read_modis_l1b.o $(OBJS)/read_modis_geo.o \
		$(OBJS)/read_modis_dimensions.o \
		$(OBJS)/allocate_imager_structures.o \
		$(OBJS)/deallocate_imager_structures.o \
		$(OBJS)/read_modis_lat_lon.o \
		$(OBJS)/read_modis_angles.o $(OBJS)/read_modis_lsflag.o \
		$(OBJS)/modis2oraclsflag.o $(OBJS)/get_modis_time.o \
		$(OBJS)/map_time_to_pixel.o $(OBJS)/read_modis_time.o \
		$(OBJS)/read_L1b_radiances_reflectances.o $(OBJS)/bright_m.o \
		$(OBJS)/read_ecmwf_dimensions.o \
		$(OBJS)/read_ecmwf_dimensions_nc.o \
		$(OBJS)/read_ecmwf_lat_lon.o $(OBJS)/read_ecmwf_lat_lon_nc.o \
		$(OBJS)/read_ecmwf.o $(OBJS)/read_ecmwf_nc.o \
		$(OBJS)/compute_geopot_coordinate.o $(OBJS)/int_ecmwf.o \
		$(OBJS)/nn_ecmwf.o \
		$(OBJS)/nc_open.o $(OBJS)/nc_dim_info.o $(OBJS)/nc_read_file.o \
		$(OBJS)/allocate_ecmwf_structures.o \
		$(OBJS)/deallocate_ecmwf_structures.o \
		$(OBJS)/allocate_ecmwf_structures_nc.o \
		$(OBJS)/rearrange_ecmwf3.o \
		$(OBJS)/rearrange_ecmwf.o $(OBJS)/allocate_channel_info.o \
		$(OBJS)/deallocate_channel_info.o $(OBJS)/set_ecmwf.o \
		$(OBJS)/define_preprop_grid.o \
		$(OBJS)/allocate_preproc_structures.o \
		$(OBJS)/make_preprop_grid.o \
		$(OBJS)/deallocate_preproc_structures.o \
		$(OBJS)/read_avhrr_dimensions.o \
		$(OBJS)/read_avhrr_time.o $(OBJS)/create_time_for_pixel.o \
		$(OBJS)/read_avhrr_geo.o $(OBJS)/read_avhrr_lat_lon.o \
		$(OBJS)/read_avhrr_angles.o $(OBJS)/avhrr2oraclsflag.o \
		$(OBJS)/read_avhrr_l1b.o \
		$(OBJS)/read_L1B_avhrr_reflectances_radiances.o \
		$(OBJS)/read_avhrr_lsmask.o $(OBJS)/read_avhrr_landseamask.o \
		$(OBJS)/build_preproc_fields.o $(OBJS)/find_min_max_preproc.o \
		$(OBJS)/calculate_rt.o $(OBJS)/rttov_driver.o \
		$(OBJS)/allocate_surface_structures.o \
		$(OBJS)/deallocate_surface_structures.o \
		$(OBJS)/cox_munk.o $(OBJS)/read_mcd43c3.o \
		$(OBJS)/read_cimss_emissivity.o \
		$(OBJS)/get_surface_emissivity.o \
		$(OBJS)/get_surface_reflectance_lam.o \
		$(OBJS)/read_nsidc_nise.o $(OBJS)/correct_for_ice_snow.o \
		$(OBJS)/call_rtm_ir_rttov.o $(OBJS)/call_rtm_solar_rttov.o \
		$(OBJS)/effective_2way_za.o $(OBJS)/get_coefs_info.o \
		$(OBJS)/test_rttov.o \
		$(OBJS)/select_modis_albedo_file.o\
		$(OBJS)/read_aatsr_dimensions.o \
		$(OBJS)/aatsr_corrections.o $(OBJS)/read_aatsr_l1b.o  \
		$(OBJS)/read_aatsr_orbit.o


# Compilation rules
$(OBJS)/%.o: %.f90
	$(F90) -o $@ -c $(FFLAGS) $(INC) $(AUXFLAGS) $<


$(OBJS)/%.o: %.F90
	$(F90) -o $@ -c $(FFLAGS) $(INC) $(AUXFLAGS) $<


$(OBJS)/%.o: %.c
	$(CC) -o $@ -c $(CFLAGS) $(CINC) $< 

# Rule to prevent make from identifying Fortran .mod files as Modula2
# source files
%.o : %.mod


# Main program build
orac_preproc.x : $(PREPROC_OBJ) Makefile
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
preproc_constants.o:               preproc_constants.F90
date_type_structure.o:		   date_type_structure.F90
channel_structures.o: 		   channel_structures.F90
calender.o:			   calender.F90 date_type_structure.F90
setup.o :			   setup.F90 date_type_structure.F90 channel_structures.F90
preparation.o:			   preparation.F90
netcdf_structures.o:		   netcdf_structures.F90
surface_structures.o:		   surface_structures.F90 preproc_constants.o
select_modis_emiss_file.o:         select_modis_emiss_file.F90
preprocessing_for_orac.o :	   preprocessing_for_orac.F90 setup.F90 preproc_constants.F90 \
				   open_netcdf_output.F90 read_imager.F90 ecmwf_structures.F90 \
				   preproc_structures.F90 attribute_structures.F90 \
				   surface_structures.F90 channel_structures.F90 \
				   read_ecmwf_dimensions_nc.F90 read_ecmwf_nc.F90 \
				   select_modis_emiss_file.F90
open_netcdf_output.o:		   open_netcdf_output.F90  nc_create_file.F90 \
				   attribute_structures.F90 channel_structures.F90
close_netcdf_output.o:		   close_netcdf_output.F90  nc_create_file.F90
nc_create_file.o:		   nc_create_file.F90 attribute_structures.F90 \
				   preproc_structures.F90 imager_structures.F90 \
				   netcdf_structures.F90 channel_structures.o
nc_close.o:			   nc_close.F90
close_netcdf_output.o:		   close_netcdf_output.F90
read_modis_l1b.o:		   read_modis_l1b.F90 imager_structures.F90 \
				   channel_structures.F90
read_modis_dimensions.o:	   read_modis_dimensions.F90
read_modis_geo.o:		   read_modis_geo.F90 imager_structures.o
imager_structures.o:		   imager_structures.F90
read_imager.o:			   read_imager.F90 read_modis_l1b.F90 \
				   read_modis_dimensions.F90 imager_structures.F90 \
				   channel_structures.o
allocate_imager_structures.o:	   allocate_imager_structures.F90 channel_structures.F90
deallocate_imager_structures.o:	   deallocate_imager_structures.F90
allocate_channel_info.o:	   allocate_channel_info.F90 channel_structures.F90
deallocate_channel_info.o:	   deallocate_channel_info.F90 channel_structures.F90
read_modis_lat_lon.o:		   read_modis_lat_lon.F90
read_modis_angles.o: 		   read_modis_angles.F90
read_modis_lsflag.o:		   read_modis_lsflag.F90
modis2oraclsflag.o:		   modis2oraclsflag.F90
get_modis_time.o:		   get_modis_time.F90
map_time_to_pixel.o: 		   map_time_to_pixel.F90
read_modis_time.o:		   read_modis_time.F90
bright_m.o:			   bright_m.F90
modis_bright.o:			   modis_bright.F90
read_L1b_radiances_reflectances.o: read_L1b_radiances_reflectances.F90 bright_m.F90
compute_geopot_coordinate.o: 	   compute_geopot_coordinate.F90
read_ecmwf.o:			   read_ecmwf.F90 preproc_constants.F90 
read_ecmwf_nc.o:		   read_ecmwf_nc.F90 preproc_constants.F90 
int_ecmwf.o:			   int_ecmwf.F90 preproc_constants.F90
nn_ecmwf.o:			   nn_ecmwf.F90 preproc_constants.F90
nc_open.o:			   nc_open.F90 preproc_constants.F90 
nc_dim_info.o:			   nc_dim_info.F90 preproc_constants.F90
nc_read_file.o:			   nc_read_file.F90 preproc_constants.F90 nc_open.F90 
ecmwf_structures.o:		   ecmwf_structures.F90
read_ecmwf_dimensions.o:           read_ecmwf_dimensions.F90 ecmwf_structures.F90 \
				   preproc_constants.F90
read_ecmwf_dimensions_nc.o:        read_ecmwf_dimensions_nc.F90 ecmwf_structures.F90 \
				   preproc_constants.F90
read_ecmwf_lat_lon.o:		   read_ecmwf_lat_lon.F90 ecmwf_structures.F90 \
				   preproc_constants.F90
allocate_ecmwf_structures.o:	   allocate_ecmwf_structures.F90
allocate_ecmwf_structures_nc.o:    allocate_ecmwf_structures_nc.F90
deallocate_ecmwf_structures.o:	   deallocate_ecmwf_structures.F90
set_ecmwf.o:			   set_ecmwf.F90
preproc_structures.o:		   preproc_structures.F90
define_preprop_grid.o:		   define_preprop_grid.F90
allocate_preproc_structures.o:	   allocate_preproc_structures.F90 channel_structures.F90
deallocate_preproc_structures.o:   deallocate_preproc_structures.F90 channel_structures.F90
aatsr_mods.o:  			   aatsr_mods.F90 preproc_structures.F90 \
				   aatsr_drift_correction_def.F90 read_aatsr_beam_def.F90
read_aatsr_dimensions.o:	   read_aatsr_dimensions.F90 read_aatsr_l1b.F90 \
				   read_aatsr_beam_def.F90
read_imager.o:			   read_imager.F90 read_aatsr_l1b.F90
read_aatsr_beam_def.o:		   read_aatsr_beam_def.F90
read_aatsr_l1b.o:		   read_aatsr_l1b.F90 read_aatsr_beam_def.F90 \
				   aatsr_apply_corrections.F90 aatsr_drift_correction_def.F90
aatsr_apply_corrections.o:	   aatsr_apply_corrections.F90 aatsr_drift_correction_def.F90 \
				   read_aatsr_l1b.F90
aatsr_drift_correction_def.o:	   aatsr_drift_correction_def.F90
read_avhrr_dimensions.o:	   read_avhrr_dimensions.F90
read_avhrr_geo.o:		   read_avhrr_geo.F90
read_avhrr_lat_lon.o:		   read_avhrr_lat_lon.F90
read_avhrr_angles.o:		   read_avhrr_angles.F90
read_avhrr_l1b.o: 		   read_avhrr_l1b.F90
read_avhrr_lsmask.o:		   read_avhrr_lsmask.F90
read_avhrr_landseamask.o:	   read_avhrr_landseamask.F90
avhrr2oraclsflag.o:		   avhrr2oraclsflag.F90
read_L1B_avhrr_reflectances_radiances.o: read_L1B_avhrr_reflectances_radiances.F90
attribute_structures.o:		   attribute_structures.F90
make_preprop_grid.o:		   make_preprop_grid.F90
build_preproc_fields.o:		   build_preproc_fields.F90
find_min_max_preproc.o:		   find_min_max_preproc.F90
calculate_rt.o:			   calculate_rt.F90
call_rtm_ir_rttov.o:               call_rtm_ir_rttov.F90 channel_structures.F90
call_rtm_solar_rttov.o:            call_rtm_solar_rttov.F90 channel_structures.F90
effective_2way_za.o:  	           effective_2way_za.F90   
rttov_driver.o:			   rttov_driver.F90  effective_2way_za.F90 \
				   call_rtm_ir_rttov.F90 call_rtm_solar_rttov.F90 \
				   channel_structures.F90 test_rttov.F90
get_coefs_info.o:   		   get_coefs_info.F90     
get_instrument_info.o:  	   get_instrument_info.F90       
allocate_surface_structures.o:	   allocate_surface_structures.F90 surface_structures.F90 \
				   preproc_constants.F90 preproc_structures.F90 \
				   imager_structures.F90 channel_structures.F90
deallocate_surface_structures.o:   deallocate_surface_structures.F90 surface_structures.F90 \
				   preproc_constants.o
cox_munk.o:			   cox_munk.F90 preproc_constants.F90
mcd43c.o:			   mcd43c.F90
read_mcd43c3.o:			   read_mcd43c3.F90 mcd43c.F90
interpol_bilinear.o:		   interpol_bilinear.F90 preproc_constants.F90
fill_grid.o:			   fill_grid.F90 preproc_constants.F90
interpol_nearest_neighbour.o:      interpol_nearest_neighbour.F90
read_cimss_emissivity.o:           read_cimss_emissivity.F90 preproc_constants.F90 emis_def.F90
get_surface_emissivity.o:	   get_surface_emissivity.F90 read_cimss_emissivity.F90 \
				   preproc_constants.F90 emis_def.F90 preproc_structures.F90 \
				   imager_structures.F90 emis_def.F90 interpol_bilinear.F90 \
				   interpol_nearest_neighbour.F90 emis_def.F90 \
				   channel_structures.F90
select_modis_albedo_file.o:        select_modis_albedo_file.F90
get_surface_reflectance_lam.o:	   get_surface_reflectance_lam.F90 preproc_constants.F90 \
				   preproc_structures.F90 imager_structures.F90 mcd43c.F90 \
				   fill_grid.F90 interpol_bilinear.F90 cox_munk.F90 \
				   surface_structures.F90 interpol_nearest_neighbour.F90 \
				   read_mcd43c3.F90 mcd43c.F90 channel_structures.F90 \
				   select_modis_albedo_file.F90
nise_def.o:			   nise_def.F90  
read_nsidc_nise.o:		   read_nsidc_nise.F90 nise_def.F90
correct_for_ice_snow.o:		   correct_for_ice_snow.F90 preproc_constants.F90 \
				   preproc_structures.F90 imager_structures.F90 \
				   surface_structures.F90 nise_def.F90 channel_structures.F90
write_swath_to_netcdf.o:           write_swath_to_netcdf.F90 channel_structures.F90
read_avhrr_time.o:		   read_avhrr_time.F90
get_modis_time.o:		   get_modis_time.F90
map_time_to_pixel.o:		   map_time_to_pixel.F90
test_read_aatsr_l1b.o:		   read_aatsr_l1b.F90 read_aatsr_beam_def.F90 \
				   aatsr_apply_corrections.F90 aatsr_drift_correction_def.F90 \
				   setup.F90 imager_structures.F90 channel_structures.F90 \
				   preproc_structures.F90
test_rttov.o: 			   test_rttov.F90
read_aatsr_beam.o:                 read_aatsr_beam.c
