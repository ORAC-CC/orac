#Makefile for postprocessing
#Pre 2012/08/24  CP and MJ provide initial version
#2012/08/24  MJ creates adds "includes" for better configurability
#2014/10/09  AP homogenise with other Makefile layouts
#2/12/2014  CP added link to common libraries still need alot of tidying though.

# Set files from which compiler, flags, and libraries are set. archfile also
# defines the "$(OBJS)" variable, which places object and module files in a
# subdirectory
include $(ORAC_LIB)
include $(ORAC_ARCH)
ORAC_COMMON   = ../common

POSTPROC_OBJ_F90 =  $(OBJS)/vartypes_pp.o \
		    $(OBJS)/structures_pp.o \
                    $(OBJS)/set_struct_pp.o \
                    $(OBJS)/nc_write_L2.o \
                    $(OBJS)/nc_open.o \
                    $(OBJS)/nc_dim_info.o \
                    $(OBJS)/nc_read_file.o \
                    $(OBJS)/nc_close.o \
                    $(OBJS)/scanline_structure.o  \
                    $(OBJS)/alloc_spixel_scan_out_pp.o \
                    $(OBJS)/dealloc_spixel_scan_out_pp.o  \
                    $(OBJS)/nc_create_global_pp.o \
                    $(OBJS)/nc_defdata.o \
                    $(OBJS)/read_inter_file.o \
                    $(OBJS)/read_inter_sec_file.o \
                    $(OBJS)/alloc_spixel_scan_out_rt_pp.o \
                    $(OBJS)/read_input_dimensions.o \
                    $(OBJS)/neural_net_constants_postproc.o \
                    $(OBJS)/neural_net.o \
                    $(OBJS)/snow_ice_mask.o

POSTPROC_OBJ_MAIN = $(OBJS)/post_process_level2.o


# Compilation rules
$(OBJS)/%.o: %.F90 $(ORAC_COMMON)/liborac_common.a
	$(F90) -o $@ -c $(FFLAGS) $(INC)  -I$(ORAC_COMMON)/obj $(AUXFLAGS) $<

$(OBJS)/%.o: %.f90 $(ORAC_COMMON)/liborac_common.a
	$(F90) -o $@ -c $(FFLAGS) $(INC)  -I$(ORAC_COMMON)/obj $(AUXFLAGS) $<

$(OBJS)/%.o: %.f $(ORAC_COMMON)/liborac_common.a
	$(F90) -o $@ -c $(FFLAGS) $(INC) -I$(ORAC_COMMON)/obj $(AUXFLAGS)  $<


# Main program build
all: post_process_level2

post_process_level2: $(POSTPROC_OBJ_MAIN) libpostproc.a  $(ORAC_COMMON)/liborac_common.a Makefile
	$(F90) -o post_process_level2 $(LFLAGS) $(POSTPROC_OBJ_MAIN) libpostproc.a $(ORAC_COMMON)/liborac_common.a -I./$(OBJS) $(LIBS)

libpostproc.a: $(POSTPROC_OBJ_F90) Makefile
	ar -rs libpostproc.a $(POSTPROC_OBJ_F90)


# Rebuild dependencies housed in dependencies.inc
depend:
	@../tools/make_depend.pl $(POSTPROC_OBJ_F90) $(POSTPROC_OBJ_MAIN) > dependencies.inc

clean tidy: 
	rm -f $(OBJS)/*.o $(OBJS)/*.mod *.a post_process_level

# Fortran 90 dependencies
include dependencies.inc