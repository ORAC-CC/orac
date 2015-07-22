$(OBJS)/alloc_input_data.o: $(OBJS)/input_routines.o \
        $(OBJS)/postproc_constants.o
$(OBJS)/dealloc_input_data.o: $(OBJS)/input_routines.o \
        $(OBJS)/postproc_constants.o
$(OBJS)/alloc_output_data_pp.o: $(OBJS)/input_routines.o \
        $(OBJS)/output_routines_pp.o $(OBJS)/postproc_constants.o
$(OBJS)/dealloc_output_data_pp.o: $(OBJS)/output_routines_pp.o \
        $(OBJS)/postproc_constants.o
$(OBJS)/def_vars_primary_pp.o: $(OBJS)/input_routines.o \
        $(OBJS)/output_routines_pp.o $(OBJS)/postproc_constants.o
$(OBJS)/def_vars_secondary_pp.o: $(OBJS)/input_routines.o \
        $(OBJS)/output_routines_pp.o $(OBJS)/postproc_constants.o
$(OBJS)/neural_net.o: $(OBJS)/postproc_constants.o
$(OBJS)/write_primary_pp.o: $(OBJS)/input_routines.o \
        $(OBJS)/output_routines_pp.o $(OBJS)/postproc_constants.o
$(OBJS)/prepare_primary_pp.o: $(OBJS)/input_routines.o \
        $(OBJS)/output_routines_pp.o
$(OBJS)/prepare_secondary_pp.o: $(OBJS)/input_routines.o \
        $(OBJS)/output_routines_pp.o
$(OBJS)/read_input_dimensions_pp.o: $(OBJS)/postproc_constants.o
$(OBJS)/read_primary_file.o: $(OBJS)/input_routines.o \
        $(OBJS)/postproc_constants.o
$(OBJS)/read_secondary_file.o: $(OBJS)/input_routines.o \
        $(OBJS)/postproc_constants.o
$(OBJS)/write_secondary_pp.o: $(OBJS)/input_routines.o \
        $(OBJS)/output_routines_pp.o $(OBJS)/postproc_constants.o
$(OBJS)/post_process_level2.o: $(OBJS)/input_routines.o \
        $(OBJS)/neural_net_constants_pp.o $(OBJS)/output_routines_pp.o \
        $(OBJS)/postproc_constants.o
