$(OBJS)/orac_input.o: alloc_input_data.F90 dealloc_input_data.F90 \
        read_input_dimensions.F90 read_input_primary.F90 \
        read_input_secondary.F90
$(OBJS)/postproc_utils.o: $(OBJS)/orac_input.o $(OBJS)/postproc_constants.o
$(OBJS)/prepare_output_pp.o: $(OBJS)/orac_input.o $(OBJS)/postproc_constants.o \
        prepare_output_primary_pp.F90 prepare_output_secondary_pp.F90
$(OBJS)/correct_parallax.o: $(OBJS)/orac_input.o $(OBJS)/postproc_constants.o
$(OBJS)/orac_postproc.o: $(OBJS)/correct_parallax.o $(OBJS)/orac_input.o \
        $(OBJS)/postproc_constants.o $(OBJS)/postproc_utils.o \
        $(OBJS)/prepare_output_pp.o
