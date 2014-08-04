$(OBJS)/CalcCWP.o: $(OBJS)/Ctrl.o $(OBJS)/ECPConstants.o $(OBJS)/SPixel.o
$(OBJS)/CheckLimits.o: $(OBJS)/Ctrl.o $(OBJS)/ECPConstants.o $(OBJS)/RTM_Pc.o \
        $(OBJS)/SPixel.o
$(OBJS)/check_value.o: $(OBJS)/ECPConstants.o $(OBJS)/SPixel.o check_value.inc
$(OBJS)/config.o: $(OBJS)/ECPConstants.o
$(OBJS)/Ctrl.o: $(OBJS)/ECPConstants.o DeallocCtrl.F90
$(OBJS)/Data.o: $(OBJS)/Ctrl.o $(OBJS)/ECPConstants.o DeallocData.F90
$(OBJS)/DecomposeCholesky.o: $(OBJS)/ECPConstants.o
$(OBJS)/Diag.o: $(OBJS)/ECPConstants.o
$(OBJS)/extrap_into_tropopause.o: $(OBJS)/SPixel.o
$(OBJS)/FMRoutines.o: $(OBJS)/Ctrl.o $(OBJS)/ECPConstants.o $(OBJS)/GZero.o \
        $(OBJS)/IntLUTRoutines.o $(OBJS)/RTM_Pc.o $(OBJS)/SADChan.o \
        $(OBJS)/SADLUT.o $(OBJS)/SPixel.o FM.F90 FMSolar.F90 FMThermal.F90 \
        SetCRPSolar.F90 SetCRPThermal.F90
$(OBJS)/GetGeometry.o: $(OBJS)/Ctrl.o $(OBJS)/Data.o $(OBJS)/ECPConstants.o \
        $(OBJS)/SPixel.o
$(OBJS)/GetIllum.o: $(OBJS)/Ctrl.o $(OBJS)/Data.o $(OBJS)/ECPConstants.o \
        $(OBJS)/SADChan.o $(OBJS)/SPixel.o
$(OBJS)/GetLSF.o: $(OBJS)/Ctrl.o $(OBJS)/Data.o $(OBJS)/ECPConstants.o \
        $(OBJS)/SPixel.o
$(OBJS)/GetLwSwRTM.o: $(OBJS)/Ctrl.o $(OBJS)/ECPConstants.o $(OBJS)/RTM.o \
        $(OBJS)/SADChan.o $(OBJS)/SPixel.o
$(OBJS)/GetMeasurements.o: $(OBJS)/Ctrl.o $(OBJS)/Data.o $(OBJS)/ECPConstants.o \
        $(OBJS)/SADChan.o $(OBJS)/SPixel.o
$(OBJS)/GetRs.o: $(OBJS)/Ctrl.o $(OBJS)/Data.o $(OBJS)/SPixel.o
$(OBJS)/GetRTM.o: $(OBJS)/Ctrl.o $(OBJS)/ECPConstants.o $(OBJS)/GetLwSwRTM.o \
        $(OBJS)/RTM.o $(OBJS)/SADChan.o $(OBJS)/SPixel.o
$(OBJS)/GetSPixel.o: $(OBJS)/Ctrl.o $(OBJS)/Data.o $(OBJS)/ECPConstants.o \
        $(OBJS)/GetIllum.o $(OBJS)/RTM.o $(OBJS)/SADChan.o $(OBJS)/SPixel.o \
        $(OBJS)/check_value.o $(OBJS)/config.o
$(OBJS)/GetSurface.o: $(OBJS)/Ctrl.o $(OBJS)/Data.o $(OBJS)/ECPConstants.o \
        $(OBJS)/SPixel.o
$(OBJS)/GetX.o: $(OBJS)/Ctrl.o $(OBJS)/ECPConstants.o $(OBJS)/SADChan.o \
        $(OBJS)/SPixel.o
$(OBJS)/GZero.o: $(OBJS)/ECPConstants.o $(OBJS)/SPixel.o
$(OBJS)/interpolate_to_ctp.o: $(OBJS)/Ctrl.o $(OBJS)/ECPConstants.o \
        $(OBJS)/SPixel.o
$(OBJS)/InterpolSolar.o: $(OBJS)/Ctrl.o $(OBJS)/RTM_Pc.o $(OBJS)/SPixel.o
$(OBJS)/InterpolSolar_spline.o: $(OBJS)/Ctrl.o $(OBJS)/ECPConstants.o \
        $(OBJS)/IntRoutines.o $(OBJS)/RTM_Pc.o $(OBJS)/SPixel.o
$(OBJS)/InterpolThermal.o: $(OBJS)/Ctrl.o $(OBJS)/RTM_Pc.o $(OBJS)/SADChan.o \
        $(OBJS)/SPixel.o
$(OBJS)/InterpolThermal_spline.o: $(OBJS)/Ctrl.o $(OBJS)/IntRoutines.o \
        $(OBJS)/RTM_Pc.o $(OBJS)/SADChan.o $(OBJS)/SPixel.o
$(OBJS)/IntRoutines.o: $(OBJS)/ECPConstants.o Bcucof.F90 Bcuint.F90 Linint.F90 \
        Locate.F90 Spline.F90 Tridag.F90
$(OBJS)/IntLUTRoutines.o: $(OBJS)/Ctrl.o $(OBJS)/GZero.o $(OBJS)/IntRoutines.o \
        $(OBJS)/SADLUT.o IntLUTTauRe.F90 IntLUTTauSatRe.F90 \
        IntLUTTauSatSolAziRe.F90 IntLUTTauSolRe.F90 Interp3dLUT.F90
$(OBJS)/InvertMarquardt.o: $(OBJS)/Ctrl.o $(OBJS)/Diag.o $(OBJS)/ECPConstants.o \
        $(OBJS)/FMRoutines.o $(OBJS)/RTM_Pc.o $(OBJS)/SADChan.o $(OBJS)/SADLUT.o \
        $(OBJS)/SPixel.o
$(OBJS)/locate.o: $(OBJS)/ECPConstants.o
$(OBJS)/output_routines.o: $(OBJS)/Ctrl.o $(OBJS)/Data.o $(OBJS)/Diag.o \
        $(OBJS)/RTM_Pc.o $(OBJS)/SPixel.o $(OBJS)/config.o def_vars_primary.F90 \
        def_vars_secondary.F90 prepare_primary.F90 prepare_secondary.F90 \
        write_primary.F90 write_secondary.F90
$(OBJS)/nc_create_global.o: $(OBJS)/Ctrl.o $(OBJS)/ECPConstants.o
$(OBJS)/nc_defdata.o: $(OBJS)/ECPConstants.o
$(OBJS)/nc_dim_info.o: $(OBJS)/ECPConstants.o
$(OBJS)/nc_read_file.o: $(OBJS)/ECPConstants.o
$(OBJS)/nc_write_L2.o: $(OBJS)/ECPConstants.o
$(OBJS)/polint.o: $(OBJS)/ECPConstants.o
$(OBJS)/R2T.o: $(OBJS)/SADChan.o
$(OBJS)/read_config_file.o: $(OBJS)/Ctrl.o $(OBJS)/ECPConstants.o \
        $(OBJS)/config.o
$(OBJS)/read_input_dimensions.o: $(OBJS)/Ctrl.o $(OBJS)/ECPConstants.o
$(OBJS)/read_sat_data_nc.o: $(OBJS)/Ctrl.o $(OBJS)/Data.o $(OBJS)/SADChan.o
$(OBJS)/ReadALB_nc.o: $(OBJS)/Ctrl.o $(OBJS)/Data.o $(OBJS)/ECPConstants.o \
        $(OBJS)/SADChan.o
$(OBJS)/ReadCloudFlags_nc.o: $(OBJS)/Ctrl.o $(OBJS)/Data.o \
        $(OBJS)/ECPConstants.o
$(OBJS)/ReadDriver.o: $(OBJS)/Ctrl.o $(OBJS)/ECPConstants.o $(OBJS)/config.o
$(OBJS)/ReadGeometry_nc.o: $(OBJS)/Ctrl.o $(OBJS)/Data.o $(OBJS)/ECPConstants.o
$(OBJS)/ReadIllum_nc.o: $(OBJS)/Ctrl.o $(OBJS)/Data.o $(OBJS)/ECPConstants.o
$(OBJS)/ReadLocation_nc.o: $(OBJS)/Ctrl.o $(OBJS)/Data.o $(OBJS)/ECPConstants.o
$(OBJS)/ReadLSFlags_nc.o: $(OBJS)/Ctrl.o $(OBJS)/Data.o $(OBJS)/ECPConstants.o
$(OBJS)/ReadLwRTM_nc.o: $(OBJS)/Ctrl.o $(OBJS)/ECPConstants.o $(OBJS)/RTM.o
$(OBJS)/ReadMSI_nc.o: $(OBJS)/Ctrl.o $(OBJS)/Data.o $(OBJS)/ECPConstants.o \
        $(OBJS)/SADChan.o
$(OBJS)/ReadRTMData_nc.o: $(OBJS)/Ctrl.o $(OBJS)/ECPConstants.o $(OBJS)/RTM.o
$(OBJS)/ReadSAD.o: $(OBJS)/Ctrl.o $(OBJS)/ECPConstants.o $(OBJS)/SADChan.o \
        $(OBJS)/SADLUT.o
$(OBJS)/ReadScanLines_nc.o: $(OBJS)/Ctrl.o $(OBJS)/Data.o $(OBJS)/ECPConstants.o
$(OBJS)/ReadSwRTM_nc.o: $(OBJS)/Ctrl.o $(OBJS)/ECPConstants.o $(OBJS)/RTM.o
$(OBJS)/RTM.o: $(OBJS)/Ctrl.o DeallocRTM.F90
$(OBJS)/RTM_Pc.o: $(OBJS)/Ctrl.o AllocRTMPc.F90 DeallocRTMPc.F90
$(OBJS)/SADChan.o: $(OBJS)/Ctrl.o $(OBJS)/ECPConstants.o ReadChan.F90
$(OBJS)/SADCloudClass.o: $(OBJS)/ECPConstants.o
$(OBJS)/SADLUT.o: $(OBJS)/Ctrl.o $(OBJS)/ECPConstants.o $(OBJS)/SADChan.o \
        DeallocSADLUT.F90 ReadLUT.F90
$(OBJS)/SetDiag.o: $(OBJS)/Ctrl.o $(OBJS)/Diag.o $(OBJS)/ECPConstants.o \
        $(OBJS)/SPixel.o
$(OBJS)/SetGZero.o: $(OBJS)/Ctrl.o $(OBJS)/GZero.o $(OBJS)/IntRoutines.o \
        $(OBJS)/SADLUT.o $(OBJS)/SPixel.o
$(OBJS)/SetKx.o: $(OBJS)/Ctrl.o $(OBJS)/ECPConstants.o $(OBJS)/SPixel.o
$(OBJS)/SetLimits.o: $(OBJS)/Ctrl.o $(OBJS)/ECPConstants.o $(OBJS)/SPixel.o
$(OBJS)/SetSy.o: $(OBJS)/Ctrl.o $(OBJS)/ECPConstants.o $(OBJS)/SPixel.o
$(OBJS)/SetUnit.o: $(OBJS)/SPixel.o
$(OBJS)/SPixel.o: $(OBJS)/Ctrl.o $(OBJS)/ECPConstants.o $(OBJS)/RTM.o \
        AllocSPixel.F90 DeallocSPixel.F90 alloc_spixel_scan_out.F90 \
        dealloc_spixel_scan_out.F90
$(OBJS)/t_extrap_strat.o: $(OBJS)/SPixel.o
$(OBJS)/T2R.o: $(OBJS)/SADChan.o
$(OBJS)/WriteDiag.o: $(OBJS)/Ctrl.o $(OBJS)/Diag.o $(OBJS)/ECPConstants.o \
        $(OBJS)/SPixel.o
$(OBJS)/WriteLog.o: $(OBJS)/Ctrl.o
$(OBJS)/XMDAD.o: $(OBJS)/Ctrl.o $(OBJS)/ECPConstants.o $(OBJS)/SADChan.o \
        $(OBJS)/SPixel.o
$(OBJS)/XSDAD.o: $(OBJS)/Ctrl.o $(OBJS)/ECPConstants.o $(OBJS)/SPixel.o
$(OBJS)/ZeroDiag.o: $(OBJS)/Ctrl.o $(OBJS)/Diag.o $(OBJS)/ECPConstants.o
$(OBJS)/ECP.o: $(OBJS)/Ctrl.o $(OBJS)/Data.o $(OBJS)/Diag.o \
        $(OBJS)/ECPConstants.o $(OBJS)/RTM.o $(OBJS)/RTM_Pc.o $(OBJS)/ReadSAD.o \
        $(OBJS)/SADChan.o $(OBJS)/SADLUT.o $(OBJS)/SPixel.o $(OBJS)/config.o \
        $(OBJS)/output_routines.o
