$(OBJS)/input_routines.o: $(OBJS)/postproc_constants.o alloc_input_data.F90 \
        dealloc_input_data.F90 read_input_dimensions_pp.F90 \
        read_primary_file.F90 read_secondary_file.F90
$(OBJS)/prepare_output.o: $(OBJS)/input_routines.o prepare_primary_pp.F90 \
        prepare_secondary_pp.F90
$(OBJS)/post_process_level2.o: $(OBJS)/input_routines.o \
        $(OBJS)/postproc_constants.o $(OBJS)/prepare_output.o
