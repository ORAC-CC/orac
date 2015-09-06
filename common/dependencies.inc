$(OBJS)/constants_cloud_typing_pavolonis.o: $(OBJS)/common_constants.o
$(OBJS)/global_attributes.o: $(OBJS)/common_constants.o
$(OBJS)/interpol.o: $(OBJS)/common_constants.o bound_grid.F90 interp_field.inc
$(OBJS)/orac_output.o: $(OBJS)/common_constants.o $(OBJS)/orac_ncdf.o \
        alloc_output_data.F90 dealloc_output_data.F90 def_output_primary.F90 \
        def_output_secondary.F90 write_output_primary.F90 \
        write_output_secondary.F90
$(OBJS)/orac_ncdf.o: $(OBJS)/common_constants.o $(OBJS)/global_attributes.o \
        $(OBJS)/source_attributes.o nc_def_var_template.inc nc_open_field.inc \
        nc_read_field.inc nc_read_packed_field.inc nc_read_template.inc \
        nc_write_template.inc orac_nc_utils.F90
$(OBJS)/parsing.o: $(OBJS)/common_constants.o parse_string.inc parse_string2.inc
$(OBJS)/source_attributes.o: $(OBJS)/common_constants.o
