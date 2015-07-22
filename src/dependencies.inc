$(OBJS)/CalcCWP.o: $(OBJS)/Ctrl.o $(OBJS)/ECPConstants.o $(OBJS)/SPixel.o
$(OBJS)/Cholesky.o: $(OBJS)/ECPConstants.o DecomposeCholesky.F90 \
        InvertCholesky.F90 InvertCholeskyDC.F90 SolveCholesky.F90 \
        SolveCholeskyDC.F90 SquareCholeskyDC.F90
$(OBJS)/Ctrl.o: $(OBJS)/ECPConstants.o DeallocCtrl.F90
$(OBJS)/Data.o: $(OBJS)/Ctrl.o $(OBJS)/ECPConstants.o $(OBJS)/IntRoutines.o \
        $(OBJS)/SADChan.o DeallocData.F90 DetermineIllum.F90 NullifyData.F90 \
        ReadALB_nc.F90 ReadCloudFlags_nc.F90 ReadData_nc.F90 ReadGeometry_nc.F90 \
        ReadLSFlags_nc.F90 ReadLocation_nc.F90 ReadMSI_nc.F90 \
        sabotage_inputs.F90
$(OBJS)/Diag.o: $(OBJS)/Ctrl.o $(OBJS)/ECPConstants.o $(OBJS)/SPixel.o \
        SetDiag.F90 ZeroDiag.F90
$(OBJS)/extrap_into_tropopause.o: $(OBJS)/SPixel.o
$(OBJS)/blmodification.o: $(OBJS)/SPixel.o
$(OBJS)/FMRoutines.o: $(OBJS)/Ctrl.o $(OBJS)/ECPConstants.o $(OBJS)/GZero.o \
        $(OBJS)/IntLUTRoutines.o $(OBJS)/InterpolRoutines.o $(OBJS)/RTM_Pc.o \
        $(OBJS)/SADChan.o $(OBJS)/SADLUT.o $(OBJS)/SPixel.o $(OBJS)/planck.o \
        FM.F90 FMSolar.F90 FMThermal.F90 SetCRPSolar.F90 SetCRPThermal.F90
$(OBJS)/GZero.o: $(OBJS)/Ctrl.o $(OBJS)/ECPConstants.o $(OBJS)/IntRoutines.o \
        $(OBJS)/SADLUT.o $(OBJS)/SPixel.o SetGZero.F90
$(OBJS)/interpolate_to_ctp.o: $(OBJS)/Ctrl.o $(OBJS)/ECPConstants.o \
        $(OBJS)/IntRoutines.o $(OBJS)/SPixel.o
$(OBJS)/IntRoutines.o: $(OBJS)/ECPConstants.o Bcucof.F90 Bcuint.F90 Linint.F90 \
        Locate.F90 Spline.F90 Tridag.F90 polint.F90
$(OBJS)/InterpolRoutines.o: $(OBJS)/Ctrl.o $(OBJS)/ECPConstants.o \
        $(OBJS)/IntRoutines.o $(OBJS)/RTM_Pc.o $(OBJS)/SADChan.o \
        $(OBJS)/SPixel.o $(OBJS)/planck.o InterpolSolar.F90 \
        InterpolSolar_spline.F90 InterpolThermal.F90 InterpolThermal_spline.F90
$(OBJS)/IntLUTRoutines.o: $(OBJS)/Ctrl.o $(OBJS)/GZero.o $(OBJS)/IntRoutines.o \
        $(OBJS)/SADLUT.o IntLUTTauRe.F90 IntLUTTauSatRe.F90 \
        IntLUTTauSatReOnSol.F90 IntLUTTauSatSolAziRe.F90 IntLUTTauSolRe.F90 \
        Interp3dLUT.F90
$(OBJS)/Inversion.o: $(OBJS)/Cholesky.o $(OBJS)/Ctrl.o $(OBJS)/Diag.o \
        $(OBJS)/ECPConstants.o $(OBJS)/FMRoutines.o $(OBJS)/RTM_Pc.o \
        $(OBJS)/SADChan.o $(OBJS)/SADLUT.o $(OBJS)/SPixel.o CheckLimits.F90 \
        InvertMarquardt.F90 SetKx.F90 SetLimits.F90 SetSy.F90 SetUnit.F90
$(OBJS)/output_routines.o: $(OBJS)/Ctrl.o $(OBJS)/Data.o $(OBJS)/Diag.o \
        $(OBJS)/ECPConstants.o $(OBJS)/RTM_Pc.o $(OBJS)/SPixel.o \
        alloc_output_data.F90 dealloc_output_data.F90 def_vars_primary.F90 \
        def_vars_secondary.F90 prepare_primary.F90 prepare_secondary.F90 \
        write_primary.F90 write_secondary.F90
$(OBJS)/parse_user.o: $(OBJS)/ECPConstants.o parse_user_text.inc
$(OBJS)/planck.o: $(OBJS)/ECPConstants.o $(OBJS)/SADChan.o R2T.F90 T2R.F90
$(OBJS)/read_utils.o: $(OBJS)/Ctrl.o $(OBJS)/ECPConstants.o read_config_file.F90 \
        read_input_dimensions.F90
$(OBJS)/ReadDriver.o: $(OBJS)/Ctrl.o $(OBJS)/ECPConstants.o $(OBJS)/parse_user.o \
        $(OBJS)/read_utils.o
$(OBJS)/ReadSAD.o: $(OBJS)/Ctrl.o $(OBJS)/ECPConstants.o $(OBJS)/SADChan.o \
        $(OBJS)/SADLUT.o
$(OBJS)/RTM.o: $(OBJS)/Ctrl.o $(OBJS)/ECPConstants.o DeallocRTM.F90 \
        ReadLwRTM_nc.F90 ReadSwRTM_nc.F90
$(OBJS)/RTM_Pc.o: $(OBJS)/Ctrl.o AllocRTMPc.F90 DeallocRTMPc.F90
$(OBJS)/SADChan.o: $(OBJS)/Ctrl.o $(OBJS)/ECPConstants.o $(OBJS)/sad_util.o \
        ReadSADChan.F90
$(OBJS)/SADLUT.o: $(OBJS)/Ctrl.o $(OBJS)/ECPConstants.o $(OBJS)/SADChan.o \
        $(OBJS)/sad_util.o AllocSADLUT.F90 DeallocSADLUT.F90 ReadSADLUT.F90
$(OBJS)/sad_util.o: $(OBJS)/Ctrl.o
$(OBJS)/SPixel.o: $(OBJS)/Ctrl.o $(OBJS)/Data.o $(OBJS)/ECPConstants.o \
        $(OBJS)/IntRoutines.o $(OBJS)/RTM.o $(OBJS)/SADChan.o $(OBJS)/planck.o \
        AllocSPixel.F90 DeallocSPixel.F90 GetGeometry.F90 GetIllum.F90 \
        GetIndexing.F90 GetLSF.F90 GetLwSwRTM.F90 GetMeasurements.F90 GetRTM.F90 \
        GetRs.F90 GetSPixel.F90 GetSurface.F90 GetX.F90 IntCTP.F90 XMDAD.F90 \
        XSDAD.F90 check_value.inc
$(OBJS)/ECP.o: $(OBJS)/Ctrl.o $(OBJS)/Data.o $(OBJS)/Diag.o \
        $(OBJS)/ECPConstants.o $(OBJS)/Inversion.o $(OBJS)/RTM.o \
        $(OBJS)/RTM_Pc.o $(OBJS)/ReadDriver.o $(OBJS)/ReadSAD.o \
        $(OBJS)/SADChan.o $(OBJS)/SADLUT.o $(OBJS)/SPixel.o \
        $(OBJS)/output_routines.o $(OBJS)/read_utils.o
