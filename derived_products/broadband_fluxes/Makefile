# Makefile for ORAC derived products / broadband fluxes
#

# Set files from which compiler, flags, and libraries are set. archfile also
# defines the "$(OBJS)" variable, which places object and module files in a
# subdirectory
include $(ORAC_LIB)
include $(ORAC_ARCH)

ORAC_COMMON = ../../common


# Define object files to be linked.
ORAC_OBJ = $(OBJS)/kinds.o \
           $(OBJS)/bugs_physconst.o \
           $(OBJS)/continuum.o \
           $(OBJS)/bandsolve.o \
           $(OBJS)/bugsrad_planck.o \
           $(OBJS)/gases_ckd.o \
           $(OBJS)/rayle.o \
           $(OBJS)/bugs_lwr.o \
           $(OBJS)/bugs_swr.o \
           $(OBJS)/cloud_correlate.o \
           $(OBJS)/cloudg.o \
           $(OBJS)/two_rt_lw.o \
           $(OBJS)/two_rt_lw_iter.o \
           $(OBJS)/two_rt_lw_ocastrndm.o \
           $(OBJS)/two_rt_lw_sel.o \
           $(OBJS)/two_rt_sw_bs.o \
           $(OBJS)/two_rt_sw.o \
           $(OBJS)/two_rt_sw_ocastrndm.o \
           $(OBJS)/comscp1.o \
           $(OBJS)/comscp2.o \
           $(OBJS)/driver_for_bugsrad.o \
           $(OBJS)/newexp.o \
           $(OBJS)/bugs_rad.o \
           $(OBJS)/comscp2.o \
           $(OBJS)/midlatsum1.o \
           $(OBJS)/preprocess_input.o \
           $(OBJS)/interpolate_meteorology.o \
           $(OBJS)/greg2jul.o \
           $(OBJS)/collocate_prtm_profile.o \
           $(OBJS)/collocate_aerosol2cloud.o \
           $(OBJS)/adiabatic_lwc.o \
           $(OBJS)/compute_lts.o \
           $(OBJS)/compute_fth.o \
           $(OBJS)/compute_column_o3.o \
           $(OBJS)/preprocess_bugsrad_sfc_albedo.o \
           $(OBJS)/preprocess_bugsrad_sfc_emissivity.o \
           $(OBJS)/preprocess_fuliou_sfc_albedo.o \
           $(OBJS)/read_hdf_sd_dims.o \
           $(OBJS)/read_hdf_sd_data.o \
           $(OBJS)/get_modis_aerosol.o \
           $(OBJS)/get_modis_cloud.o \
           $(OBJS)/driver_for_fuliou.o \
           $(OBJS)/driver_for_lut.o

ORAC_OBJ_MAIN = $(OBJS)/process_broadband_fluxes.o


# Compilation rules
$(OBJS)/%.o: %.f90 $(ORAC_COMMON)/liborac_common.a
	$(F90) -o $@ -c $(FFLAGS) $(INC) -I$(ORAC_COMMON)/obj $(AUXFLAGS) $<

$(OBJS)/%.o: %.F90 $(ORAC_COMMON)/liborac_common.a
	$(F90) -o $@ -c $(FFLAGS) $(INC) -I$(ORAC_COMMON)/obj $(AUXFLAGS) $<

$(OBJS)/%.o: bugsrad/%.f $(ORAC_COMMON)/liborac_common.a
	$(F77) -o $@ -c $(F77FLAGS) $(INC) -I$(ORAC_COMMON)/obj $(AUXFLAGS) $<

$(OBJS)/%.o: bugsrad/%.f90 $(ORAC_COMMON)/liborac_common.a
	$(F90) -o $@ -c $(FFLAGS) $(INC) -I$(ORAC_COMMON)/obj $(AUXFLAGS) $<


# Main target list
all: process_broadband_fluxes

# Main program build
process_broadband_fluxes: $(ORAC_OBJ_MAIN) libprocess_broadband_fluxes.a $(ORAC_COMMON)/liborac_common.a Makefile
	$(F90) -o process_broadband_fluxes $(LFLAGS) $(ORAC_OBJ_MAIN) libprocess_broadband_fluxes.a $(ORAC_COMMON)/liborac_common.a -I./$(OBJS) $(LIBS)

libprocess_broadband_fluxes.a: $(ORAC_OBJ) Makefile
	$(AR) -rs libprocess_broadband_fluxes.a $(ORAC_OBJ)

# Rebuild dependencies housed in dependencies.inc
depend:
	@../../tools/make_depend.pl --subdir bugsrad $(ORAC_OBJ) $(ORAC_OBJ_MAIN) > dependencies.inc

clean tidy:
	rm -f $(OBJS)/*.o $(OBJS)/*.mod *.a process_broadband_fluxes


# Object/source dependencies
include dependencies.inc
