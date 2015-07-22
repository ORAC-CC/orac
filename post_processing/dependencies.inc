$(OBJS)/input_routines.o: $(OBJS)/postproc_constants.o alloc_input_data.F90 \
        dealloc_input_data.F90 read_input_dimensions_pp.F90 \
        read_primary_file.F90 read_secondary_file.F90
$(OBJS)/neural_net.o: $(OBJS)/postproc_constants.o
$(OBJS)/output_routines_pp.o: $(OBJS)/input_routines.o \
        $(OBJS)/postproc_constants.o alloc_output_data_pp.F90 \
        dealloc_output_data_pp.F90 def_vars_primary_pp.F90 \
        def_vars_secondary_pp.F90 prepare_primary_pp.F90 \
        prepare_secondary_pp.F90 write_primary_pp.F90 write_secondary_pp.F90
$(OBJS)/post_process_level2.o: $(OBJS)/input_routines.o \
        $(OBJS)/output_routines_pp.o $(OBJS)/postproc_constants.o
