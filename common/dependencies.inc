$(OBJS)/global_attributes.o: $(OBJS)/common_constants.o
$(OBJS)/interpol.o: $(OBJS)/common_constants.o bound_grid.F90 interp_field.inc
$(OBJS)/orac_ncdf.o: $(OBJS)/common_constants.o $(OBJS)/global_attributes.o \
        $(OBJS)/source_attributes.o nc_def_var_template.inc nc_open_field.inc \
        nc_read_field.inc nc_write_template.inc
$(OBJS)/parsing.o: $(OBJS)/common_constants.o parse_string.inc
$(OBJS)/source_attributes.o: $(OBJS)/common_constants.o
