$(OBJS)/constants_cloud_typing_pavolonis.o: $(OBJS)/common_constants.o \
        coefficients_spectral_response_correction.inc
$(OBJS)/global_attributes.o: $(OBJS)/common_constants.o
$(OBJS)/interpol.o: $(OBJS)/common_constants.o bound_grid.F90 interp_field.inc
$(OBJS)/orac_indexing.o: $(OBJS)/common_constants.o
$(OBJS)/orac_ncdf.o: $(OBJS)/common_constants.o $(OBJS)/global_attributes.o \
        $(OBJS)/source_attributes.o $(OBJS)/system_utils.o \
        ncdf_def_var_template.inc ncdf_open_field.inc ncdf_read_field.inc \
        ncdf_read_packed_field.inc ncdf_read_template.inc \
        ncdf_write_template.inc orac_ncdf_utils.F90
$(OBJS)/orac_output.o: $(OBJS)/orac_indexing.o $(OBJS)/orac_ncdf.o \
        alloc_output_data.F90 dealloc_output_data.F90 def_output_primary.F90 \
        def_output_secondary.F90 write_output_primary.F90 \
        write_output_secondary.F90
$(OBJS)/parsing.o: $(OBJS)/common_constants.o parse_string.inc parse_string2.inc
$(OBJS)/source_attributes.o: $(OBJS)/common_constants.o
