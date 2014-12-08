$(OBJS)/structures_pp.o: $(OBJS)/vartypes_pp.o
$(OBJS)/set_struct_pp.o: $(OBJS)/structures_pp.o $(OBJS)/vartypes_pp.o
$(OBJS)/nc_write_L2.o: $(OBJS)/vartypes_pp.o
$(OBJS)/nc_dim_info.o: $(OBJS)/vartypes_pp.o
$(OBJS)/nc_read_file.o: $(OBJS)/vartypes_pp.o
$(OBJS)/scanline_structure.o: $(OBJS)/vartypes_pp.o
$(OBJS)/alloc_spixel_scan_out_pp.o: $(OBJS)/scanline_structure.o \
        $(OBJS)/vartypes_pp.o
$(OBJS)/dealloc_spixel_scan_out_pp.o: $(OBJS)/scanline_structure.o \
        $(OBJS)/vartypes_pp.o
$(OBJS)/nc_create_global_pp.o: $(OBJS)/vartypes_pp.o
$(OBJS)/nc_defdata.o: $(OBJS)/vartypes_pp.o
$(OBJS)/read_inter_file.o: $(OBJS)/structures_pp.o $(OBJS)/vartypes_pp.o
$(OBJS)/read_inter_sec_file.o: $(OBJS)/structures_pp.o $(OBJS)/vartypes_pp.o
$(OBJS)/alloc_spixel_scan_out_rt_pp.o: $(OBJS)/scanline_structure.o \
        $(OBJS)/structures_pp.o $(OBJS)/vartypes_pp.o
$(OBJS)/read_input_dimensions.o: $(OBJS)/structures_pp.o $(OBJS)/vartypes_pp.o
$(OBJS)/neural_net_constants_postproc.o: $(OBJS)/vartypes_pp.o
$(OBJS)/neural_net.o: $(OBJS)/vartypes_pp.o
$(OBJS)/snow_ice_mask.o: $(OBJS)/scanline_structure.o $(OBJS)/structures_pp.o \
        $(OBJS)/vartypes_pp.o
$(OBJS)/post_process_level2.o: $(OBJS)/neural_net_constants_postproc.o \
        $(OBJS)/scanline_structure.o $(OBJS)/structures_pp.o \
        $(OBJS)/vartypes_pp.o def_vars_primary_pp.inc prepare_primary_pp.inc \
        write_primary_pp.inc
