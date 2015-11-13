# Make file for ORAC derived products
#
# History:
#   2nd Aug 2000, Andy Smith: original version
#
include $(ORAC_LIB)
include $(ORAC_ARCH)

ORAC_COMMON   = ../../common

# Define object files to be linked. N.B. files containing module definitions
# should be listed before main program units and subroutines that use those
# modules so that the modules are known to the compiler.
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
           $(OBJS)/preprocess_bugsrad.o \
           $(OBJS)/interpolate_meteorology.o \
           $(OBJS)/greg2jul.o \
           $(OBJS)/collocate_prtm_profile.o

ORAC_OBJ_MAIN = $(OBJS)/process_broadband_fluxes.o


# .F90 was originally the extension on DecUnix, but seems to be allowed by the
# ifc/ifort compiler.
$(OBJS)/%.o: %.for $(ORAC_COMMON)/liborac_common.a
	$(F90) -o $@ -c $(FFLAGS) $(INC) -I$(ORAC_COMMON)/obj $(AUXFLAGS) $<

$(OBJS)/%.o: %.f $(ORAC_COMMON)/liborac_common.a
	$(F90) -o $@ -c $(FFLAGS) $(INC) -I$(ORAC_COMMON)/obj $(AUXFLAGS) $<

$(OBJS)/%.o: %.f90 $(ORAC_COMMON)/liborac_common.a
	$(F90) -o $@ -c $(FFLAGS) $(INC) -I$(ORAC_COMMON)/obj $(AUXFLAGS) $<

$(OBJS)/%.o: %.F90 $(ORAC_COMMON)/liborac_common.a
	$(F90) -o $@ -c $(FFLAGS) $(INC) -I$(ORAC_COMMON)/obj $(AUXFLAGS) $<


# Main target list
all: process_broadband_fluxes

# Main program build
process_broadband_fluxes: $(ORAC_OBJ_MAIN) libprocess_broadband_fluxes.a $(ORAC_COMMON)/liborac_common.a Makefile
	$(F90) -o process_broadband_fluxes $(LFLAGS) $(ORAC_OBJ_MAIN) libprocess_broadband_fluxes.a $(ORAC_COMMON)/liborac_common.a -I./$(OBJS) $(LIBS)

libprocess_broadband_fluxes.a: $(ORAC_OBJ) Makefile
	ar -rs libprocess_broadband_fluxes.a $(ORAC_OBJ)

# Rebuild dependencies housed in dependencies.inc
depend:
	@../../tools/make_depend.pl $(ORAC_OBJ) $(ORAC_OBJ_MAIN) > dependencies.inc

# Tidy up .cpp files produced by C pre-compiler when compilation errors occur
# (screws up later attempts to compile - forces C++ compiler to be run)
clean tidy:
	rm -f $(OBJS)/*.o $(OBJS)/*.mod *.a process_broadband_fluxes


# Utilities to change source code extensions when moving between DEC Unix and
# Linux systems.
DEC2Linux:
	rename.csh d2l

Linux2DEC:
	rename.csh l2d


# Object/source dependencies
include dependencies.inc
