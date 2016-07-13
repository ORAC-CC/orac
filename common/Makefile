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


# Define object files to be linked.
COMMON_OBJ_F90 =$(OBJS)/common_constants.o \
                $(OBJS)/chunk_utils.o \
                $(OBJS)/constants_cloud_typing_pavolonis.o \
                $(OBJS)/global_attributes.o \
                $(OBJS)/interpol.o \
                $(OBJS)/orac_indexing.o \
                $(OBJS)/orac_ncdf.o \
                $(OBJS)/orac_output.o \
                $(OBJS)/parsing.o \
                $(OBJS)/source_attributes.o \
                $(OBJS)/system_utils.o

COMMON_OBJ_C = $(OBJS)/struct_parser_utils.o \
               $(OBJS)/system_utils_c.o

COMMON_OBJ   = $(COMMON_OBJ_F90) $(COMMON_OBJ_C)


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


# Main target list
all: liborac_common.a parser

parser: generate_parser struct_parser.tab.cc

# Main library build
liborac_common.a: $(COMMON_OBJ) Makefile
	ar -rs liborac_common.a $(OBJS)/*.o

# Rebuild dependencies housed in dependencies.inc
depend:
	@../tools/make_depend.pl $(COMMON_OBJ_F90) > dependencies.inc

clean tidy:
	rm -f $(OBJS)/*.o $(OBJS)/*.mod *.a generate_parser

clean_parser:
	rm -f generate_parser.tab.c generate_parser.tab.h generate_parser.c \
              struct_parser.tab.cc struct_parser.tab.hh position.hh location.hh \
              stack.hh


# Build generate_parser executable
generate_parser.tab.c generate_parser.tab.h: generate_parser.y
	bison $?

generate_parser.c: generate_parser.tab.h generate_parser.lex
	flex generate_parser.lex

generate_parser: generate_parser.c generate_parser.tab.c generate_parser.h
	$(CC) -o $@ $(CFLAGS) generate_parser.c generate_parser.tab.c

# A generic driver file grammar
struct_parser.tab.cc struct_parser.tab.hh: struct_parser.y
	bison $?


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

# C dependencies
$(OBJS)/struct_parser_utils.o: struct_parser_utils.c print_data_template.h
