# Makefile for ORAC post processing
#
# Pre 2012/08/24, CP and MJ: Provide initial version
# 2012/08/24, MJ: creates adds "includes" for better configurability
# 2014/10/09, AP: homogenise with other Makefile layouts
# 2014/02/12, CP: added link to common libraries still need alot of tidying
#    though.
# 2015/07/16, GM: Many files removed from the object file list due to a major
#    cleanup of the code.
#
# Notes:
#

# Set files from which compiler, flags, and libraries are set. archfile also
# defines the "$(OBJS)" variable, which places object and module files in a
# subdirectory
include $(ORAC_LIB)
include $(ORAC_ARCH)

ORAC_COMMON = ../common


# Define object files to be linked.
POSTPROC_OBJ = $(OBJS)/orac_input.o \
               $(OBJS)/postproc_constants.o \
               $(OBJS)/postproc_utils.o \
               $(OBJS)/prepare_output_pp.o  \
               $(OBJS)/correct_parallax.o  \

POSTPROC_OBJ_MAIN = $(OBJS)/orac_postproc.o


# Compilation rules
$(OBJS)/%.o: %.f90 $(ORAC_COMMON)/liborac_common.a
	$(F90) -o $@ -c $(FFLAGS) $(INC)  -I$(ORAC_COMMON)/obj $(AUXFLAGS) $<

$(OBJS)/%.o: %.F90 $(ORAC_COMMON)/liborac_common.a
	$(F90) -o $@ -c $(FFLAGS) $(INC)  -I$(ORAC_COMMON)/obj $(AUXFLAGS) $<

# Rule to prevent make from identifying Fortran .mod files as Modula2 source
# files
%.o : %.mod


# Main target list
all: orac_postproc

# Main program build
orac_postproc: $(POSTPROC_OBJ_MAIN) libpostproc.a $(ORAC_COMMON)/liborac_common.a Makefile
	$(F90) -o orac_postproc $(LFLAGS) $(POSTPROC_OBJ_MAIN) libpostproc.a $(ORAC_COMMON)/liborac_common.a -I./$(OBJS) $(LIBS)

# Main library build
libpostproc.a: $(POSTPROC_OBJ) Makefile
	$(AR) -rs libpostproc.a $(POSTPROC_OBJ)

# Rebuild dependencies housed in dependencies.inc
depend:
	@../tools/make_depend.pl $(POSTPROC_OBJ) $(POSTPROC_OBJ_MAIN) > dependencies.inc

clean tidy:
	rm -f $(OBJS)/*.o $(OBJS)/*.mod *.a orac_postproc


# Object/source dependencies
include dependencies.inc
