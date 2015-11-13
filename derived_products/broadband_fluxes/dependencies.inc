$(OBJS)/bugs_physconst.o90: $(OBJS)/kinds.o
$(OBJS)/continuum.o90: $(OBJS)/bugs_physconst.o $(OBJS)/kinds.o
$(OBJS)/bandsolve.o: $(OBJS)/kinds.o
$(OBJS)/bugsrad_planck.o90: $(OBJS)/kinds.o
$(OBJS)/gases_ckd.o90: $(OBJS)/bugs_physconst.o $(OBJS)/kinds.o
$(OBJS)/rayle.o90: $(OBJS)/bugs_physconst.o $(OBJS)/kinds.o
$(OBJS)/bugs_lwr.o: $(OBJS)/bugsrad_planck.o $(OBJS)/continuum.o \
        $(OBJS)/gases_ckd.o $(OBJS)/kinds.o
$(OBJS)/bugs_swr.o: $(OBJS)/gases_ckd.o $(OBJS)/kinds.o $(OBJS)/rayle.o
$(OBJS)/cloud_correlate.o90: $(OBJS)/kinds.o
$(OBJS)/cloudg.o: $(OBJS)/kinds.o
$(OBJS)/two_rt_lw.o: $(OBJS)/kinds.o
$(OBJS)/two_rt_lw_iter.o: $(OBJS)/kinds.o
$(OBJS)/two_rt_lw_ocastrndm.o90: $(OBJS)/kinds.o
$(OBJS)/two_rt_lw_sel.o: $(OBJS)/kinds.o
$(OBJS)/two_rt_sw_bs.o: $(OBJS)/kinds.o
$(OBJS)/two_rt_sw.o: $(OBJS)/kinds.o
$(OBJS)/two_rt_sw_ocastrndm.o90: $(OBJS)/kinds.o
$(OBJS)/comscp1.o: $(OBJS)/kinds.o
$(OBJS)/comscp2.o: $(OBJS)/kinds.o
$(OBJS)/driver_for_bugsrad.o90: $(OBJS)/bugs_physconst.o $(OBJS)/kinds.o
$(OBJS)/newexp.o90: $(OBJS)/kinds.o
$(OBJS)/bugs_rad.o: $(OBJS)/kinds.o
$(OBJS)/comscp2.o: $(OBJS)/kinds.o
