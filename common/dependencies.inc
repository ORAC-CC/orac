$(OBJS)/global_attributes.o: $(OBJS)/common_constants.o
$(OBJS)/interpol.o: $(OBJS)/common_constants.o bound_grid.F90 interp_field.inc
$(OBJS)/orac_ncdf.o: $(OBJS)/common_constants.o nc_open_field.inc \
        nc_read_field.inc nc_write_template.inc
$(OBJS)/nc_utils.o: $(OBJS)/common_constants.o $(OBJS)/global_attributes.o \
        nc_attributes.F90 nc_create.F90 nc_def_var.F90 nc_def_var.inc
