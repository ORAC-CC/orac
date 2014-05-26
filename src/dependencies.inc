$(OBJS)/alloc_spixel_scan_out.o: $(OBJS)/ECPConstants.o $(OBJS)/SPixel.o
$(OBJS)/AllocRTMPc.o: $(OBJS)/Ctrl.o $(OBJS)/RTM_Pc.o
$(OBJS)/AllocSPixel.o: $(OBJS)/Ctrl.o $(OBJS)/RTM.o $(OBJS)/SPixel.o
$(OBJS)/CalcCWP.o: $(OBJS)/Ctrl.o $(OBJS)/ECPConstants.o $(OBJS)/SPixel.o
$(OBJS)/CheckByteArray.o: $(OBJS)/ECPConstants.o
$(OBJS)/CheckLimits.o: $(OBJS)/Ctrl.o $(OBJS)/ECPConstants.o $(OBJS)/RTM_Pc.o \
        $(OBJS)/SPixel.o
$(OBJS)/config_structure.o: $(OBJS)/ECPConstants.o
$(OBJS)/Ctrl.o: $(OBJS)/ECPConstants.o
$(OBJS)/Data.o: $(OBJS)/ECPConstants.o
$(OBJS)/dealloc_spixel_scan_out.o: $(OBJS)/ECPConstants.o $(OBJS)/SPixel.o
$(OBJS)/DeallocCtrl.o: $(OBJS)/Ctrl.o
$(OBJS)/DeallocData.o: $(OBJS)/Ctrl.o $(OBJS)/Data.o
$(OBJS)/DeallocRTM.o: $(OBJS)/Ctrl.o $(OBJS)/RTM.o
$(OBJS)/DeallocRTMPc.o: $(OBJS)/Ctrl.o $(OBJS)/RTM_Pc.o
$(OBJS)/DeallocSADLUT.o: $(OBJS)/Ctrl.o $(OBJS)/SADLUT.o
$(OBJS)/DeallocSPixel.o: $(OBJS)/Ctrl.o $(OBJS)/SPixel.o
$(OBJS)/DecomposeCholesky.o: $(OBJS)/ECPConstants.o
$(OBJS)/Diag.o: $(OBJS)/ECPConstants.o
$(OBJS)/FMRoutines.o: $(OBJS)/Ctrl.o $(OBJS)/ECPConstants.o $(OBJS)/GZero.o \
        $(OBJS)/IntLUTRoutines.o $(OBJS)/RTM_Pc.o $(OBJS)/SADChan.o \
        $(OBJS)/SADLUT.o $(OBJS)/SPixel.o FM.F90 FMSolar.F90 FMThermal.F90 \
        SetCRPSolar.F90 SetCRPThermal.F90
$(OBJS)/GetGeometry.o: $(OBJS)/Ctrl.o $(OBJS)/Data.o $(OBJS)/ECPConstants.o \
        $(OBJS)/SPixel.o
$(OBJS)/GetIllum.o: $(OBJS)/Ctrl.o $(OBJS)/Data.o $(OBJS)/ECPConstants.o \
        $(OBJS)/SPixel.o
$(OBJS)/GetLSF.o: $(OBJS)/Ctrl.o $(OBJS)/Data.o $(OBJS)/ECPConstants.o \
        $(OBJS)/SPixel.o
$(OBJS)/GetLwSwRTM.o: $(OBJS)/Ctrl.o $(OBJS)/ECPConstants.o $(OBJS)/RTM.o \
        $(OBJS)/SADChan.o $(OBJS)/SPixel.o
$(OBJS)/GetMeasurements.o: $(OBJS)/Ctrl.o $(OBJS)/Data.o $(OBJS)/ECPConstants.o \
        $(OBJS)/SADChan.o $(OBJS)/SPixel.o
$(OBJS)/GetRs.o: $(OBJS)/Ctrl.o $(OBJS)/Data.o $(OBJS)/SPixel.o
$(OBJS)/GetRTM.o: $(OBJS)/Ctrl.o $(OBJS)/ECPConstants.o $(OBJS)/RTM.o \
        $(OBJS)/SADChan.o $(OBJS)/SPixel.o
$(OBJS)/GetSPixel.o: $(OBJS)/Ctrl.o $(OBJS)/Data.o $(OBJS)/ECPConstants.o \
        $(OBJS)/RTM.o $(OBJS)/SADChan.o $(OBJS)/SPixel.o \
        $(OBJS)/config_structure.o
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
$(OBJS)/InterpolTrans.o: $(OBJS)/Ctrl.o $(OBJS)/ECPConstants.o $(OBJS)/SPixel.o
$(OBJS)/IntRoutines.o: $(OBJS)/ECPConstants.o Bcucof.F90 Bcuint.F90 Linint.F90 \
        Locate.F90 Spline.F90 Tridag.F90
$(OBJS)/IntLUTRoutines.o: $(OBJS)/Ctrl.o $(OBJS)/GZero.o $(OBJS)/IntRoutines.o \
        $(OBJS)/SADLUT.o IntLUTTauRe.F90 IntLUTTauSatRe.F90 \
        IntLUTTauSatSolAziRe.F90 IntLUTTauSolRe.F90 Interp3dLUT.F90
$(OBJS)/InvertMarquardt.o: $(OBJS)/Ctrl.o $(OBJS)/Diag.o $(OBJS)/ECPConstants.o \
        $(OBJS)/FMRoutines.o $(OBJS)/RTM_Pc.o $(OBJS)/SADChan.o $(OBJS)/SADLUT.o \
        $(OBJS)/SPixel.o
$(OBJS)/locate.o: $(OBJS)/ECPConstants.o
$(OBJS)/nc_create_global.o: $(OBJS)/Ctrl.o $(OBJS)/ECPConstants.o
$(OBJS)/nc_defdata.o: $(OBJS)/ECPConstants.o
$(OBJS)/nc_dim_info.o: $(OBJS)/ECPConstants.o
$(OBJS)/nc_read_file.o: $(OBJS)/ECPConstants.o
$(OBJS)/nc_write_L2.o: $(OBJS)/ECPConstants.o
$(OBJS)/polint.o: $(OBJS)/ECPConstants.o
$(OBJS)/R2T.o: $(OBJS)/ECPConstants.o $(OBJS)/SADChan.o
$(OBJS)/read_config_file.o: $(OBJS)/Ctrl.o $(OBJS)/ECPConstants.o \
        $(OBJS)/config_structure.o
$(OBJS)/read_input_dimensions.o: $(OBJS)/Ctrl.o $(OBJS)/ECPConstants.o
$(OBJS)/read_sat_data_nc.o: $(OBJS)/Ctrl.o $(OBJS)/Data.o $(OBJS)/SADChan.o
$(OBJS)/ReadAlb.o: $(OBJS)/Ctrl.o $(OBJS)/Data.o $(OBJS)/ECPConstants.o \
        $(OBJS)/SADChan.o
$(OBJS)/ReadAlb_nc.o: $(OBJS)/Ctrl.o $(OBJS)/Data.o $(OBJS)/ECPConstants.o \
        $(OBJS)/SADChan.o
$(OBJS)/ReadCloudFlags_nc.o: $(OBJS)/Ctrl.o $(OBJS)/Data.o \
        $(OBJS)/ECPConstants.o
$(OBJS)/ReadDataRTM.o: $(OBJS)/ECPConstants.o
$(OBJS)/ReadDriver.o: $(OBJS)/Ctrl.o $(OBJS)/ECPConstants.o \
        $(OBJS)/config_structure.o
$(OBJS)/ReadGeometry.o: $(OBJS)/Ctrl.o $(OBJS)/Data.o $(OBJS)/ECPConstants.o
$(OBJS)/ReadGeometry_nc.o: $(OBJS)/Ctrl.o $(OBJS)/Data.o $(OBJS)/ECPConstants.o
$(OBJS)/Readillum.o: $(OBJS)/Ctrl.o $(OBJS)/Data.o $(OBJS)/ECPConstants.o
$(OBJS)/Readillum_nc.o: $(OBJS)/Ctrl.o $(OBJS)/Data.o $(OBJS)/ECPConstants.o
$(OBJS)/ReadLocation.o: $(OBJS)/Ctrl.o $(OBJS)/Data.o $(OBJS)/ECPConstants.o
$(OBJS)/ReadLocation_nc.o: $(OBJS)/Ctrl.o $(OBJS)/Data.o $(OBJS)/ECPConstants.o
$(OBJS)/ReadLSFlags.o: $(OBJS)/Ctrl.o $(OBJS)/Data.o $(OBJS)/ECPConstants.o
$(OBJS)/ReadLSFlags_nc.o: $(OBJS)/Ctrl.o $(OBJS)/Data.o $(OBJS)/ECPConstants.o
$(OBJS)/ReadLwRTM.o: $(OBJS)/Ctrl.o $(OBJS)/ECPConstants.o $(OBJS)/RTM.o
$(OBJS)/ReadLwRTM_nc.o: $(OBJS)/Ctrl.o $(OBJS)/ECPConstants.o $(OBJS)/RTM.o
$(OBJS)/ReadMSI.o: $(OBJS)/Ctrl.o $(OBJS)/Data.o $(OBJS)/ECPConstants.o \
        $(OBJS)/SADChan.o
$(OBJS)/ReadMSI_nc.o: $(OBJS)/Ctrl.o $(OBJS)/Data.o $(OBJS)/ECPConstants.o \
        $(OBJS)/SADChan.o
$(OBJS)/ReadRTMData_nc.o: $(OBJS)/Ctrl.o $(OBJS)/ECPConstants.o $(OBJS)/RTM.o
$(OBJS)/ReadSAD.o: $(OBJS)/Ctrl.o $(OBJS)/ECPConstants.o $(OBJS)/SADChan.o \
        $(OBJS)/SADLUT.o
$(OBJS)/ReadScanLines.o: $(OBJS)/Ctrl.o $(OBJS)/Data.o $(OBJS)/ECPConstants.o
$(OBJS)/ReadScanLines_nc.o: $(OBJS)/Ctrl.o $(OBJS)/Data.o $(OBJS)/ECPConstants.o
$(OBJS)/ReadSwRTM_nc.o: $(OBJS)/Ctrl.o $(OBJS)/ECPConstants.o $(OBJS)/RTM.o
$(OBJS)/SADChan.o: $(OBJS)/Ctrl.o $(OBJS)/ECPConstants.o ReadChan.F90
$(OBJS)/SADCloudClass.o: $(OBJS)/ECPConstants.o
$(OBJS)/SADLUT.o: $(OBJS)/Ctrl.o $(OBJS)/ECPConstants.o $(OBJS)/SADChan.o \
        ReadLUT.F90
$(OBJS)/SetDiag.o: $(OBJS)/Ctrl.o $(OBJS)/Diag.o $(OBJS)/ECPConstants.o \
        $(OBJS)/SPixel.o
$(OBJS)/SetGZero.o: $(OBJS)/Ctrl.o $(OBJS)/GZero.o $(OBJS)/IntRoutines.o \
        $(OBJS)/SADLUT.o $(OBJS)/SPixel.o
$(OBJS)/SetKx.o: $(OBJS)/Ctrl.o $(OBJS)/ECPConstants.o $(OBJS)/SPixel.o
$(OBJS)/SetLimits.o: $(OBJS)/Ctrl.o $(OBJS)/ECPConstants.o $(OBJS)/SPixel.o
$(OBJS)/SetSy.o: $(OBJS)/Ctrl.o $(OBJS)/ECPConstants.o $(OBJS)/SPixel.o
$(OBJS)/SetUnit.o: $(OBJS)/SPixel.o
$(OBJS)/SPixel.o: $(OBJS)/ECPConstants.o
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
        $(OBJS)/SADChan.o $(OBJS)/SADLUT.o $(OBJS)/SPixel.o \
        $(OBJS)/config_structure.o $(OBJS)/input_structures.o \
        def_vars_primary.inc def_vars_secondary.inc prepare_primary.inc \
        prepare_secondary.inc write_primary.inc write_secondary.inc
