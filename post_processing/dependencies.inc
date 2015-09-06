$(OBJS)/orac_input.o: $(OBJS)/postproc_constants.o alloc_input_data.F90 \
        dealloc_input_data.F90 read_input_dimensions.F90 read_input_primary.F90 \
        read_input_secondary.F90
$(OBJS)/prepare_output.o: $(OBJS)/orac_input.o prepare_output_primary.F90 \
        prepare_output_secondary.F90
$(OBJS)/post_process_level2.o: $(OBJS)/orac_input.o $(OBJS)/postproc_constants.o \
        $(OBJS)/prepare_output.o
