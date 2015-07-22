# Makefile for postprocessing
#
# Pre 2012/08/24, CP and MJ: Provide initial version
# 2012/08/24, MJ: creates adds "includes" for better configurability
# 2014/10/09, AP: homogenise with other Makefile layouts
# 2014/02/12, CP: added link to common libraries still need alot of tidying
#    though.

# Set files from which compiler, flags, and libraries are set. archfile also
# defines the "$(OBJS)" variable, which places object and module files in a
# subdirectory
include $(ORAC_LIB)
include $(ORAC_ARCH)

ORAC_COMMON = ../common

# Define object files to be linked.
POSTPROC_OBJ_F90 =  $(OBJS)/input_routines.o \
                    $(OBJS)/neural_net.o \
                    $(OBJS)/neural_net_constants_pp.o \
                    $(OBJS)/output_routines_pp.o  \
                    $(OBJS)/postproc_constants.o

POSTPROC_OBJ_MAIN = $(OBJS)/post_process_level2.o


# Compilation rules
$(OBJS)/%.o: %.F90 $(ORAC_COMMON)/liborac_common.a
	$(F90) -o $@ -c $(FFLAGS) $(INC)  -I$(ORAC_COMMON)/obj $(AUXFLAGS) $<

$(OBJS)/%.o: %.f90 $(ORAC_COMMON)/liborac_common.a
	$(F90) -o $@ -c $(FFLAGS) $(INC)  -I$(ORAC_COMMON)/obj $(AUXFLAGS) $<

$(OBJS)/%.o: %.f $(ORAC_COMMON)/liborac_common.a
	$(F90) -o $@ -c $(FFLAGS) $(INC) -I$(ORAC_COMMON)/obj $(AUXFLAGS)  $<


# Main target list
all: post_process_level2

# Main program build
post_process_level2: $(POSTPROC_OBJ_MAIN) libpostproc.a  $(ORAC_COMMON)/liborac_common.a Makefile
	$(F90) -o post_process_level2 $(LFLAGS) $(POSTPROC_OBJ_MAIN) libpostproc.a $(ORAC_COMMON)/liborac_common.a -I./$(OBJS) $(LIBS)

libpostproc.a: $(POSTPROC_OBJ_F90) Makefile
	ar -rs libpostproc.a $(POSTPROC_OBJ_F90)


# Rebuild dependencies housed in dependencies.inc
depend:
	@../tools/make_depend.pl $(POSTPROC_OBJ_F90) $(POSTPROC_OBJ_MAIN) > dependencies.inc

clean tidy:
	rm -f $(OBJS)/*.o $(OBJS)/*.mod *.a post_process_level2

# Fortran 90 dependencies
include dependencies.inc
