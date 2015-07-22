$(OBJS)/alloc_spixel_scan_out_pp.o: $(OBJS)/scanline_structure.o \
        $(OBJS)/structures_pp.o $(OBJS)/vartypes_pp.o
$(OBJS)/dealloc_spixel_scan_out_pp.o: $(OBJS)/scanline_structure.o \
        $(OBJS)/vartypes_pp.o
$(OBJS)/def_vars_primary_pp.o: $(OBJS)/scanline_structure.o \
        $(OBJS)/structures_pp.o $(OBJS)/vartypes_pp.o
$(OBJS)/def_vars_secondary_pp.o: $(OBJS)/scanline_structure.o \
        $(OBJS)/structures_pp.o $(OBJS)/vartypes_pp.o
$(OBJS)/neural_net.o: $(OBJS)/vartypes_pp.o
$(OBJS)/prepare_primary_pp.o: $(OBJS)/scanline_structure.o \
        $(OBJS)/structures_pp.o
$(OBJS)/prepare_secondary_pp.o: $(OBJS)/scanline_structure.o \
        $(OBJS)/structures_pp.o
$(OBJS)/read_input_dimensions.o: $(OBJS)/structures_pp.o $(OBJS)/vartypes_pp.o
$(OBJS)/read_inter_file.o: $(OBJS)/structures_pp.o $(OBJS)/vartypes_pp.o
$(OBJS)/read_inter_sec_file.o: $(OBJS)/structures_pp.o $(OBJS)/vartypes_pp.o
$(OBJS)/set_struct_pp.o: $(OBJS)/structures_pp.o $(OBJS)/vartypes_pp.o
$(OBJS)/snow_ice_mask.o: $(OBJS)/scanline_structure.o $(OBJS)/structures_pp.o \
        $(OBJS)/vartypes_pp.o
$(OBJS)/write_primary_pp.o: $(OBJS)/scanline_structure.o $(OBJS)/structures_pp.o \
        $(OBJS)/vartypes_pp.o
$(OBJS)/write_secondary_pp.o: $(OBJS)/scanline_structure.o \
        $(OBJS)/structures_pp.o $(OBJS)/vartypes_pp.o
$(OBJS)/post_process_level2.o: $(OBJS)/neural_net_constants_postproc.o \
        $(OBJS)/scanline_structure.o $(OBJS)/structures_pp.o \
        $(OBJS)/vartypes_pp.o
