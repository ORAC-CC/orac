$(OBJS)/cholesky.o: invert_cholesky.F90 solve_cholesky.F90
$(OBJS)/ctrl.o: $(OBJS)/orac_constants.o dealloc_ctrl.F90
$(OBJS)/data.o: $(OBJS)/ctrl.o $(OBJS)/int_routines.o $(OBJS)/orac_constants.o \
        $(OBJS)/sad_chan.o dealloc_data.F90 determine_illum.F90 nullify_data.F90 \
        read_alb.F90 read_cloudflags.F90 read_data.F90 read_geometry.F90 \
        read_location.F90 read_lsflags.F90 read_msi.F90 sabotage_inputs.F90
$(OBJS)/diag.o: $(OBJS)/ctrl.o $(OBJS)/data.o $(OBJS)/orac_constants.o \
        $(OBJS)/spixel.o set_diag.F90 zero_diag.F90
$(OBJS)/fm_routines.o: $(OBJS)/ctrl.o $(OBJS)/gzero.o $(OBJS)/int_lut_routines.o \
        $(OBJS)/interpol_routines.o $(OBJS)/orac_constants.o $(OBJS)/planck.o \
        $(OBJS)/rtm_pc.o $(OBJS)/sad_chan.o $(OBJS)/sad_lut.o $(OBJS)/spixel.o \
        fm.F90 fm_solar.F90 fm_thermal.F90 set_crp_solar.F90 set_crp_thermal.F90
$(OBJS)/gzero.o: $(OBJS)/ctrl.o $(OBJS)/int_routines.o $(OBJS)/orac_constants.o \
        $(OBJS)/sad_lut.o $(OBJS)/spixel.o set_gzero.F90
$(OBJS)/int_routines.o: $(OBJS)/orac_constants.o find_in_array.inc linint.F90 \
        locate.F90
$(OBJS)/interpol_routines.o: $(OBJS)/ctrl.o $(OBJS)/int_routines.o \
        $(OBJS)/orac_constants.o $(OBJS)/planck.o $(OBJS)/rtm_pc.o \
        $(OBJS)/sad_chan.o $(OBJS)/spixel.o interpol_solar.F90 \
        interpol_solar_spline.F90 interpol_thermal.F90 \
        interpol_thermal_spline.F90
$(OBJS)/int_lut_routines.o: $(OBJS)/ctrl.o $(OBJS)/gzero.o \
        $(OBJS)/int_routines.o $(OBJS)/sad_lut.o int_lut_re.F90 \
        int_lut_taure.F90 int_lut_tausatre.F90 int_lut_tausatreonsol.F90 \
        int_lut_tausatsolazire.F90 int_lut_tausolre.F90 interp3dlut.F90
$(OBJS)/inversion.o: $(OBJS)/cholesky.o $(OBJS)/ctrl.o $(OBJS)/diag.o \
        $(OBJS)/fm_routines.o $(OBJS)/gzero.o $(OBJS)/int_lut_routines.o \
        $(OBJS)/int_routines.o $(OBJS)/orac_constants.o $(OBJS)/planck.o \
        $(OBJS)/rtm_pc.o $(OBJS)/sad_chan.o $(OBJS)/sad_lut.o $(OBJS)/spixel.o \
        calc_corrected_ctx.F90 calc_cwp.F90 check_limits.F90 \
        invert_marquardt.F90 set_kx.F90 set_limits.F90 set_sy.F90
$(OBJS)/planck.o: $(OBJS)/orac_constants.o $(OBJS)/sad_chan.o r2t.F90 t2r.F90
$(OBJS)/prepare_output.o: $(OBJS)/ctrl.o $(OBJS)/data.o $(OBJS)/diag.o \
        $(OBJS)/orac_constants.o $(OBJS)/rtm_pc.o $(OBJS)/spixel.o \
        prepare_output_primary.F90 prepare_output_secondary.F90
$(OBJS)/read_utils.o: $(OBJS)/ctrl.o $(OBJS)/orac_constants.o \
        read_config_file.F90 read_input_dimensions.F90
$(OBJS)/read_driver.o: $(OBJS)/ctrl.o $(OBJS)/orac_constants.o \
        $(OBJS)/read_utils.o switch.inc
$(OBJS)/read_sad.o: $(OBJS)/ctrl.o $(OBJS)/orac_constants.o $(OBJS)/sad_chan.o \
        $(OBJS)/sad_lut.o
$(OBJS)/rtm.o: $(OBJS)/ctrl.o $(OBJS)/orac_constants.o dealloc_rtm.F90 \
        read_lwrtm.F90 read_prtm.F90 read_swrtm.F90
$(OBJS)/rtm_pc.o: $(OBJS)/ctrl.o alloc_rtm_pc.F90 dealloc_rtm_pc.F90
$(OBJS)/sad_chan.o: $(OBJS)/ctrl.o $(OBJS)/orac_constants.o $(OBJS)/sad_util.o \
        read_sad_chan.F90
$(OBJS)/sad_lut.o: $(OBJS)/ctrl.o $(OBJS)/orac_constants.o $(OBJS)/sad_chan.o \
        $(OBJS)/sad_util.o alloc_sad_lut.F90 dealloc_sad_lut.F90 \
        read_sad_lut.F90
$(OBJS)/sad_util.o: $(OBJS)/ctrl.o
$(OBJS)/spixel.o: $(OBJS)/ctrl.o $(OBJS)/orac_constants.o $(OBJS)/rtm.o \
        alloc_spixel.F90 dealloc_spixel.F90
$(OBJS)/spixel_routines.o: $(OBJS)/ctrl.o $(OBJS)/data.o $(OBJS)/fm_routines.o \
        $(OBJS)/gzero.o $(OBJS)/int_lut_routines.o $(OBJS)/int_routines.o \
        $(OBJS)/orac_constants.o $(OBJS)/planck.o $(OBJS)/rtm.o \
        $(OBJS)/sad_chan.o $(OBJS)/sad_lut.o $(OBJS)/spixel.o get_geometry.F90 \
        get_indexing.F90 get_location.F90 get_lsf.F90 get_lwswrtm.F90 \
        get_measurements.F90 get_rtm.F90 get_spixel.F90 get_surface.F90 \
        int_ctp.F90 x_mdad.F90 x_sdad.F90
$(OBJS)/orac.o: $(OBJS)/ctrl.o $(OBJS)/data.o $(OBJS)/diag.o $(OBJS)/inversion.o \
        $(OBJS)/orac_constants.o $(OBJS)/prepare_output.o $(OBJS)/read_driver.o \
        $(OBJS)/read_sad.o $(OBJS)/read_utils.o $(OBJS)/rtm.o $(OBJS)/rtm_pc.o \
        $(OBJS)/sad_chan.o $(OBJS)/sad_lut.o $(OBJS)/spixel.o \
        $(OBJS)/spixel_routines.o
