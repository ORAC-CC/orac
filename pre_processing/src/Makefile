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
# 2014/07/01, Adam Povey: Updated after I changed ECMWF routines.
# 2014/08/10, Greg McGarragh: Changes related to new BRDF support.
# 2014/08/22, Greg McGarragh: Use the library file instead of the object list
#    for the executable and make the executable dependent on the library file.
#    Now 'make' will build both the executable and library in one command.
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

ORAC_COMMON   = ../../common


# Define object files to be linked. N.B. files containing module definitions
# should be listed before main program units and subroutines that use those
# modules so that the modules are known to the compiler.
PREPROC_OBJ_F90 = $(OBJS)/aatsr_corrections.o \
                  $(OBJS)/attribute_structures.o \
                  $(OBJS)/bright_m.o \
                  $(OBJS)/build_preproc_fields.o \
                  $(OBJS)/calender.o \
                  $(OBJS)/channel_structures.o \
                  $(OBJS)/correct_for_ice_snow.o \
                  $(OBJS)/cox_munk.o \
                  $(OBJS)/date_type_structure.o \
                  $(OBJS)/define_preprop_grid.o \
                  $(OBJS)/ecmwf.o \
                  $(OBJS)/fill_grid.o \
                  $(OBJS)/imager_structures.o \
                  $(OBJS)/map_time_to_pixel.o \
                  $(OBJS)/mcd43c.o \
                  $(OBJS)/netcdf_output.o \
                  $(OBJS)/netcdf_structures.o \
                  $(OBJS)/nise_def.o \
                  $(OBJS)/nr.o \
                  $(OBJS)/preparation.o \
                  $(OBJS)/preproc_constants.o \
                  $(OBJS)/preproc_structures.o \
                  $(OBJS)/read_aatsr.o \
                  $(OBJS)/read_avhrr.o \
                  $(OBJS)/read_cimss_emissivity.o \
                  $(OBJS)/read_imager.o \
                  $(OBJS)/read_modis.o \
                  $(OBJS)/ross_thick_li_sparse_r.o \
                  $(OBJS)/rttov_driver.o \
                  $(OBJS)/set_ecmwf.o \
                  $(OBJS)/setup.o \
                  $(OBJS)/surface_emissivity.o \
                  $(OBJS)/surface_reflectance.o \
                  $(OBJS)/surface_structures.o \
                  $(OBJS)/test_rttov.o

PREPROC_OBJ_C    = $(OBJS)/read_aatsr_orbit.o

PREPROC_OBJ      = $(PREPROC_OBJ_F90) $(PREPROC_OBJ_C)

PREPROC_OBJ_MAIN = $(OBJS)/preprocessing_for_orac.o


# Compilation rules
$(OBJS)/%.o: %.f90
	$(F90) -o $@ -c $(FFLAGS) $(INC) -I$(ORAC_COMMON)/obj  $(AUXFLAGS) $<

$(OBJS)/%.o: %.F90
	$(F90) -o $@ -c $(FFLAGS) $(INC) -I$(ORAC_COMMON)/obj $(AUXFLAGS) $<

$(OBJS)/%.o: %.c
	$(CC) -o $@ -c $(CFLAGS) $(CINC) $<

# Rule to prevent make from identifying Fortran .mod files as Modula2 source
# files
%.o : %.mod


# Main program build
orac_preproc.x: $(PREPROC_OBJ_MAIN) libpreproc.a $(ORAC_COMMON)/liborac_common.a Makefile
	$(F90) -o orac_preproc.x $(LFLAGS) $(PREPROC_OBJ_MAIN) libpreproc.a $(ORAC_COMMON)/liborac_common.a -I./$(OBJS) $(LIBS)

libpreproc.a: $(PREPROC_OBJ) Makefile
	ar -rs libpreproc.a $(PREPROC_OBJ)

# Rebuild dependencies housed in dependencies.inc
depend:
	@../../tools/make_depend.pl $(PREPROC_OBJ_F90) $(PREPROC_OBJ_MAIN) > dependencies.inc

clean tidy:
	rm -f $(OBJS)/*.o $(OBJS)/*.mod *.a orac_preproc.x


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
