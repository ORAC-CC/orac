$(OBJS)/bugs_physconst.o: $(OBJS)/kinds.o
$(OBJS)/continuum.o: $(OBJS)/bugs_physconst.o $(OBJS)/kinds.o
$(OBJS)/bandsolve.o: $(OBJS)/kinds.o
$(OBJS)/bugsrad_planck.o: $(OBJS)/kinds.o
$(OBJS)/gases_ckd.o: $(OBJS)/bugs_physconst.o $(OBJS)/kinds.o
$(OBJS)/rayle.o: $(OBJS)/bugs_physconst.o $(OBJS)/kinds.o
$(OBJS)/bugs_lwr.o: $(OBJS)/bugsrad_planck.o $(OBJS)/continuum.o \
        $(OBJS)/gases_ckd.o $(OBJS)/kinds.o
$(OBJS)/bugs_swr.o: $(OBJS)/gases_ckd.o $(OBJS)/kinds.o $(OBJS)/rayle.o
$(OBJS)/cloud_correlate.o: $(OBJS)/kinds.o
$(OBJS)/cloudg.o: $(OBJS)/kinds.o
$(OBJS)/two_rt_lw.o: $(OBJS)/kinds.o
$(OBJS)/two_rt_lw_iter.o: $(OBJS)/kinds.o
$(OBJS)/two_rt_lw_ocastrndm.o: $(OBJS)/kinds.o
$(OBJS)/two_rt_lw_sel.o: $(OBJS)/kinds.o
$(OBJS)/two_rt_sw_bs.o: $(OBJS)/kinds.o
$(OBJS)/two_rt_sw.o: $(OBJS)/kinds.o
$(OBJS)/two_rt_sw_ocastrndm.o: $(OBJS)/kinds.o
$(OBJS)/comscp1.o: $(OBJS)/kinds.o
$(OBJS)/comscp2.o: $(OBJS)/kinds.o
$(OBJS)/driver_for_bugsrad.o: $(OBJS)/bugs_physconst.o $(OBJS)/kinds.o
$(OBJS)/newexp.o: $(OBJS)/kinds.o
$(OBJS)/bugs_rad.o: $(OBJS)/kinds.o
$(OBJS)/comscp2.o: $(OBJS)/kinds.o
$(OBJS)/fuinput.o: $(OBJS)/extras.o $(OBJS)/taucorr.o
$(OBJS)/fuoutput.o: $(OBJS)/fuinput.o
$(OBJS)/fuprint.o: $(OBJS)/extras.o $(OBJS)/fuinput.o $(OBJS)/fuoutput.o
$(OBJS)/entropy_lw.o: $(OBJS)/fuinput.o
$(OBJS)/icedirsfc.o: $(OBJS)/fuinput.o $(OBJS)/fuoutput.o
$(OBJS)/ma_tip.o: $(OBJS)/fuinput.o
$(OBJS)/vla.o: $(OBJS)/fuinput.o
$(OBJS)/uvcor_all.o: $(OBJS)/fuinput.o $(OBJS)/fuoutput.o
$(OBJS)/rad_multi.o: $(OBJS)/fuinput.o $(OBJS)/fuoutput.o $(OBJS)/icedirsfc.o \
        $(OBJS)/uvcor_all.o $(OBJS)/vla.o
$(OBJS)/seiji_k2d.o: $(OBJS)/fuinput.o
$(OBJS)/seiji_solver.o: $(OBJS)/entropy_lw.o $(OBJS)/fuinput.o \
        $(OBJS)/fuoutput.o
$(OBJS)/gflq.o: $(OBJS)/extras.o $(OBJS)/fuinput.o
$(OBJS)/calipso_output.o: $(OBJS)/fuinput.o $(OBJS)/fuoutput.o
$(OBJS)/aqua_wnflt_0404.o: $(OBJS)/fuoutput.o
$(OBJS)/misc.o: $(OBJS)/fuinput.o
$(OBJS)/cloud_optics.o: $(OBJS)/ar_asy.o $(OBJS)/fuinput.o $(OBJS)/icedirsfc.o
$(OBJS)/seiji_twostreamsolv_sw_v20.o: $(OBJS)/fuinput.o
$(OBJS)/aerosols.o: $(OBJS)/extras.o $(OBJS)/fuinput.o
$(OBJS)/driver_for_fuliou.o: $(OBJS)/calipso_output.o $(OBJS)/extras.o \
        $(OBJS)/gflq.o $(OBJS)/icedirsfc.o $(OBJS)/rad_multi.o
