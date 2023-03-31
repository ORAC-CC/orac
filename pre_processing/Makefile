# Makefile for ORAC preprocessing
#
# History:
# 2011/12/09, MJ: Created initial makefile to start code development for the
#    orac preprocessing, used orac makefile as template.
# 2012/01/23, MJ: Includes grib_api,netcdf,hdf4,hdf5 recently.
# 2012/05/01, GT: Added surface reflectance routines and hdf-eos lib
# 2012/05/31, GT: Added surface emissivity routines.
# 2012/08/24, MJ: Creates adds "includes" for better configurability
# 2013/08/01, AP: Save .o and .mod files to a seperate folder.
# 2013/12/03, GM: A couple of small fixes.
# 2013/12/03, GM: Renewed object list and dependencies and some small clean ups.
# 2013/12/04, GM: Added 'make depend' to automatically find all Fortran file
#    dependencies.
# 2014/07/01, AP: Updated after I changed ECMWF routines.
# 2014/08/10, GM: Changes related to new BRDF support.
# 2014/08/22, GM: Use the library file instead of the object list for the
#    executable and make the executable dependent on the library file. Now
#    'make' will build both the executable and library in one command.
# 2014/10/23, OS: Changes related to USGS, Pavolonis, and NN cloud mask.
# 2015/02/19, GM: Added the read_seviri module.
# 2015/02/24, GM: Added the utils_for_main module.
# 2016/04/08, SP: Added the read_himawari module.
# 2016/05/16, SP: Added the read_viirs module.
# 2016/06/14, SP: Added the read_slstr module.
# 2017/02/07, SP: Added support for NOAA GFS atmosphere data (ExtWork)
# 2017/03/29, SP: Add ability to calculate tropospheric cloud emissivity (ExtWork)
# 2018/02/12, SP: Added the read_goes module.
# 2019/8/14, SP: Add Fengyun-4A support.
#
# Notes:
# It appears that -lopenjpeg needs to be included in the LIBS list since on some
# systems RTTOV seems to require it.
#

# Set files from which compiler, flags, and libraries are set. archfile also
# defines the "$(OBJS)" variable, which places object and module files in a
# subdirectory
include $(ORAC_LIB)
include $(ORAC_ARCH)

ORAC_COMMON = ../common


# Define object files to be linked.
PREPROC_OBJ_F90 = $(OBJS)/aatsr_corrections.o \
                  $(OBJS)/bright_m.o \
                  $(OBJS)/build_preproc_fields.o \
                  $(OBJS)/calender.o \
                  $(OBJS)/channel_structures.o \
                  $(OBJS)/cloud_emis.o \
                  $(OBJS)/cloud_typing_pavolonis.o \
                  $(OBJS)/correct_for_ice_snow.o \
                  $(OBJS)/cox_munk.o \
                  $(OBJS)/cox_munk_constants.o \
                  $(OBJS)/define_preprop_grid.o \
                  $(OBJS)/ecmwf.o \
                  $(OBJS)/fill_grid.o \
                  $(OBJS)/get_USGS_data.o \
                  $(OBJS)/imager_structures.o \
                  $(OBJS)/map_time_to_pixel.o \
                  $(OBJS)/mcd43c.o \
                  $(OBJS)/netcdf_output.o \
                  $(OBJS)/neural_net_constants.o \
                  $(OBJS)/neural_net_preproc.o \
                  $(OBJS)/nsidc_nise.o \
                  $(OBJS)/ocean_colour.o \
                  $(OBJS)/preparation.o \
                  $(OBJS)/preproc_constants.o \
                  $(OBJS)/preproc_structures.o \
                  $(OBJS)/gauss_leg_quad.o \
                  $(OBJS)/read_aatsr.o \
                  $(OBJS)/read_abi.o \
                  $(OBJS)/read_agri.o \
                  $(OBJS)/read_avhrr.o \
                  $(OBJS)/read_camel_emissivity.o \
                  $(OBJS)/read_cimss_emissivity.o \
                  $(OBJS)/read_himawari.o \
                  $(OBJS)/read_imager.o \
                  $(OBJS)/read_modis.o \
                  $(OBJS)/read_seviri.o \
                  $(OBJS)/read_slstr.o \
                  $(OBJS)/read_viirs_Iband.o \
                  $(OBJS)/read_viirs_Mband.o \
                  $(OBJS)/read_USGS_file.o \
                  $(OBJS)/remove_rayleigh.o \
                  $(OBJS)/ross_thick_li_sparse_r.o \
                  $(OBJS)/rttov_driver.o \
                  $(OBJS)/rttov_driver_gfs.o \
                  $(OBJS)/setup.o \
                  $(OBJS)/solar_position.o \
                  $(OBJS)/surface_emissivity.o \
                  $(OBJS)/surface_reflectance.o \
                  $(OBJS)/surface_structures.o \
                  $(OBJS)/utils_for_main.o

PREPROC_OBJ_C    = $(OBJS)/read_aatsr_orbit.o

PREPROC_OBJ      = $(PREPROC_OBJ_F90) $(PREPROC_OBJ_C)

PREPROC_OBJ_MAIN = $(OBJS)/orac_preproc.o


# Compilation rules
$(OBJS)/%.o: %.f90 $(ORAC_COMMON)/liborac_common.a
	$(F90) -o $@ -c $(FFLAGS) $(INC) -I$(ORAC_COMMON)/obj $(AUXFLAGS) $<

$(OBJS)/%.o: %.F90 $(ORAC_COMMON)/liborac_common.a
	$(F90) -o $@ -c $(FFLAGS) $(INC) -I$(ORAC_COMMON)/obj $(AUXFLAGS) $<

$(OBJS)/%.o: %.c
	$(CC) -o $@ -c $(CFLAGS) $(CINC) $<

# Rule to prevent make from identifying Fortran .mod files as Modula2 source
# files
%.o : %.mod


# Main target list
all: orac_preproc

# Main program build
orac_preproc: $(PREPROC_OBJ_MAIN) libpreproc.a $(ORAC_COMMON)/liborac_common.a Makefile
	$(F90) -o orac_preproc $(LFLAGS) $(PREPROC_OBJ_MAIN) libpreproc.a $(ORAC_COMMON)/liborac_common.a -I./$(OBJS) $(LIBS)

# Main library build
libpreproc.a: $(PREPROC_OBJ) Makefile
	$(AR) -rs libpreproc.a $(PREPROC_OBJ)

# Rebuild dependencies housed in dependencies.inc
depend:
	@../tools/make_depend.pl $(PREPROC_OBJ_F90) $(PREPROC_OBJ_MAIN) > dependencies.inc

clean tidy:
	rm -f $(OBJS)/*.o $(OBJS)/*.mod *.a orac_preproc


# Fortran 90 dependencies
include dependencies.inc

# C dependecies
$(OBJS)/read_aatsr_orbit.o: read_aatsr_orbit.h
