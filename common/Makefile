# Make file for orac preprocessing
#
# History:
# 2014/08/22, Greg McGarragh: Original version
#
# Notes:
#

# Set files from which compiler, flags, and libraries are set. archfile also
# defines the "$(OBJS)" variable, which places object and module files in a
# subdirectory
include $(ORAC_ARCH)
include $(ORAC_LIB)


# Define object files to be linked. N.B. files containing module definitions
# should be listed before main program units and subroutines that use those
# modules so that the modules are known to the compiler.
PREPROC_OBJ_F90=$(OBJS)/common_constants.o \
                $(OBJS)/interpol.o \
                $(OBJS)/orac_ncdf.o \
                $(OBJS)/nc_utils.o \


PREPROC_OBJ_C=

PREPROC_OBJ  =  $(PREPROC_OBJ_F90) $(PREPROC_OBJ_C)


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
liborac_common.a: $(PREPROC_OBJ) Makefile
	ar -rs liborac_common.a $(OBJS)/*.o

# Rebuild dependencies housed in dependencies.inc
depend:
	@../tools/make_depend.pl $(PREPROC_OBJ_F90) > dependencies.inc

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
