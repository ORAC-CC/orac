# Make file for ORAC 
#
# History:
#   2nd August 2000, Andy Smith : original version
#   16th Nov 2000, Andy Smith :
#      Updated to link with new IntLUTTauReSun routine.
#      Added tidy_cpp target
#   17th Nov 2000, Andy Smith :
#      Int LUT routines renamed to follow order of Tau, SatZen, SunZen etc
#      in the LUT array dimensions.
#   22nd Nov 2000, Andy Smith :
#      Adding ReadATSR routines to ECP.
#   23rd Nov 2000, Andy Smith :
#      Added a few dependencies on Ctrl.f90 where previously omitted.
#      Added note 2. 
#   24th Nov 2000, Andy Smith :
#      Added SPixel structure.
#      Added R2T and T2R test harness
#   30th Nov 2000, Andy Smith :
#      .f90 extensions replaced by .f90 following move from ATSRLX to ATSRUS
#    1st Dec 2000, Andy Smith :
#      Int*Sun* routines renamed to use Sol instead of Sun. 
#      SetCRPSolar stripped out of ECP main program since separate test 
#      harness exists.
#      Updated HarnessSetCRPsolar dependencies.
#   14th Dec 2000, Kevin Smith :
#      Added test harness HarnessReadRTMData to test all read RTM subroutines
#   15th Dec 2000, Phil Watts
#      Added test harness HarnessReadLUT to test LUT ingest routine
#   19th Dec 2000, Kevin Smith
#      Added test harness HarnessReadSat to test ReadSatData ingest routines
#   20th Dec 2000, Kevin Smith
#      Added test harness HarnessGetSPixel to test GetSPixel and subroutines
#   15th Jan 2001, Kevin Smith
#      Added test harness HarnessGetRTM to test GetRTM and subroutines
#   17th Jan 2001, Andy Smith
#      Added test harness HarnessSetCRPThermal.
#   18th Jan 2001, Kevin Smith
#      Added test harness HarnessGetSurface to test GetSurface and GetRs
#   19th Jan 2001, Andy Smith
#      New interface definition module FMRoutines.
#   23rd Jan 2001, Andy Smith
#      New routine SetGZero.
#      New test harness HarnessFMSolar
#   24th Jan 2001, Kevin Smith
#      New test harness HarnessReadnGet
#      Integrated Get_Spixel and subroutines in ECP
#   18th May 2001, Tim Nightingale
#      Added Cholesky routines and test harness HarnessCholesky
#    6th June 2001, Andy Smith
#      Added GetX to FM test harnesses.
#      Added harness for Invert_Marquardt
#   18th June 2001, Andy Smith
#      Added targets to help with porting between DEC and Linux systems.
#      DEC2Linux and Linux2DEC to change source code file extensions.
#      LIBS macro for additional libraries required by ABSOFT compiler.
#   15th Aug 2001, Andy Smith:
#      Added updated ecp target to build integrated ecp executable.
#   23rd Aug 2001, Andy Smith:
#      Missing dependency added (SetLimits : Ctrl.f90)
#   18th Sept 2001, Andy Smith:
#      Noted that performance is improved significantly with debugging flags
#      removed from compile command.
#      Removed -pg option in .F90.o suffix rule. No such option supported by the
#      Absoft compiler.
#      Added $(LIBS) to ecp link command.
#      Removed some multiple occurences of RTM.o in object lists.
#   22nd Oct 2001, Andy Smith:
#      Added routines to deallocate SPixel, SADLUT, Data, RTM and RTMPc to 
#      object lists for ECP, HarnessInvMarq and the FM harnesses. 
#      New test harness for Write_Diag.
#
#     ************************************************************************
#     Essential Climate Variable work starts here
#
#   21st Jan 2011, Andy Smith:
#      Changed fortran compiler to ifc (ifort) for running on apps1.rsg.rl.ac.uk
#      Change compiler options, no longer using DECUnix. 
#      Current set of working source code has .f90 extension, set suffix 
#      rules to suit. 
#   28th Jan 2011, Andy Smith:
#      Created HarReadDriver. 
#      Changed Fotran compiler name to ifort. ifc produces a warning that the 
#      compiler has been renamed, ifort doesn't (but it's the same compiler). 
#   17th Feb 2011, Andy Smith:
#      Reviving HarReadSat. removed Invert.o from object list - not needed? 
#      Add ReadSAD.
#   9th Mar 2011, Andy Smith:
#     Re-applying changes from late 2001/2. Added ReadATSRAlb, ReadIntArray. 
#     Added deallocData to ReadSatData harness. 
#   22nd Mar 2011, Andy Smith:
#     Added DumpECPoutfile program. 
#   30th Mar 2011, Andy Smith:
#     GetCLoudFlags and GetLocation no longer called from Get_SPixel following removal 
#     of super-pixel averaging. Updated object file lists for ECP and all currently 
#     supported test harnesses. 
#     Extended "all" target to build all currently-supported test harnesses (swapped 
#     order of targets so that "all" is built by default). 
#   6th Apr 2011, Andy Smith:
#     Removal of redundant selection methods SAD, SDAD for a priori, first guess etc. 
#     XSDAD.f90 is now redundant.
#   14th Apr 2011, Andy Smith: 
#     HarnessReadDriver needs dependency on ECP_Routines.
#     New program CreateDummyGeo to write a dummy multiple-view geometry file. 
#   21st Apr 2011, Andy Smith:
#     Add HarnessGetSPixel to supported test harnesses again. 
#   18th May 2011, Andy Smith:
#     New program CreateDummyMSI to write a dummy multi-view MSI file. 
#   21st May 2011, Andy Smith:
#     Tidy-up for initial community access. Rename "ecp" executable to "orac".
#     Updated Notes below.
#     Removed object lists for unsupported test harnesses (saved in separate file).
#   28th June 2011 Caroline Poulsen remove refernce to ATSR routines
#   28th July 2011 Caroline Poulsen added in scanline routines
#   28th July 2011 remved ref to cloud class routines
#   22nd Sept 2011 Caroline Poulsen change GetLwRTM to getLwSwRTM remove getswrtm
#   5th October 2011, Chris Arnold: included new routines..
#     InterpolThermal_spline InterpolSolar_spline Bcubic_def.f90 Bcucof.f90 Bcuint.f90
#     Linint.f90 Locate.f90 Spline.f90 Tridag.f90 and updated dependencies 
#   8th October 2011 updated Harness info and include CWP_calc found lots of instances
#     where bcubic_def not  defined 
#   2nd November Caroline Poulsen included interpolthermal in FM dependacies as they had 
#     accidently been removed.
#   8th December 2011 Matthias Jerg added netcdf related routines nc_create_global.F90  
#     nc_close.F90 nc_defdata.F90 nc_write_L2.F90 alloc_spixel_scan_out.F90 and paths to 
#     netcdf libraries. Suffix was changed to *.F90 in order to ensure the application of 
#     the c preprocessor for compilers where this can not be explicitly requested by a flag.
#   13th December added in byte to integer routine and dealloccrl routine
#   24th August 2012 MJ creates adds "includes" for better configurability

# Notes:
# 1) The C preprocessor is used to allow conditional compilation (files with
# extension .f90 are automatically passed to cpp). If a unit compilation
# produces an error this can result in creation of a .cpp file for that 
# unit. On the next attempt to compile, make finds the .cpp file first 
# and runs g++ to try to make the .o file from the .cpp. To stop this 
# happening, use make -r. (Or use the "tidy" target - N.B. you can't do this
# every time as a precaution because if no .cpp files are found the command
# fails and the make stops).
#
# 2) The default compilation rule includes the -g flags for 
# development and debugging.
# 
# 3) To compile using breakpoints, do make <program> FFLAGS=-DBKP. This defines
# the BKP macro used for conditional compilation, such that breakpoint code
# is included. (Also available in some routines is DEBUG code). Breakpoint
# levels are defined for each routine in ECPConstants.f90.
#
# 4) To compile using log output, do make <program> FFLAGS=-DDEBUG orac
#
# 5) Object lists for some early test harnesses are out of date since not all
# test harnesses were re-compiled and re-run as development proceeded. In 
# general, HarnessFM, Harness FMSolar and Thermal were kept up to date.
#
# 6) Portability between DEC Unix/Linux: 
#   6.1) Source file names: extensions are .f90 on DEC Unix, .F90 for some Linux
#        compilers (ifort accepts .f90 on Linux). 
#        Rename all source code and globally edit dependencies in this file
#        TAKING CARE not to change the SUFFIX rules!
#        To rename source files automatically, use "make DEC2Linux" or
#        make "Linux2DEC"
#   6.2) Read_Driver uses a getenv call. On the DEC Unix system getenv is 
#        declared external in ReadDriver.f90. 
#        For the Absoft compiler the external declaration must be removed and 
#        -L$ABSOFT/lib -lU77 added to the link command in order to link in 
#        the libU77.a library.


# set files from which compiler, flags, and libraries are set
# archfile also defines the "$(OBJS)" variable, which places object and module
# files in a subdirectory
#archfile=arch.ifort.ox
#libfile=lib.ifort.ox

archfile=arch.ecmwf.xlf 
libfile=lib.ecmwf.xlf 

# can even be set through env. variable
#archfile=$(orac_arch)
#libfile=$(orac_lib)

include $(archfile)
include $(libfile)

# Define object files to be linked. N.B. files containing module definitions
# should be listed before main program units and subroutines that use those
# modules so that the modules are known to the compiler. 
ORAC_OBJ=	$(OBJS)/ECPConstants.o $(OBJS)/input_structures.o $(OBJS)/Ctrl.o \
		$(OBJS)/SADChan.o $(OBJS)/SADLUT.o $(OBJS)/GZero.o \
		$(OBJS)/SPixel.o $(OBJS)/Data.o $(OBJS)/RTM.o $(OBJS)/RTM_Pc.o $(OBJS)/Diag.o \
		$(OBJS)/Bcubic_def.o $(OBJS)/IntRoutines.o $(OBJS)/SADRoutines.o \
		$(OBJS)/FMRoutines.o $(OBJS)/FindLUN.o $(OBJS)/WriteLog.o \
		$(OBJS)/ReadDriver.o $(OBJS)/ReadChan.o  $(OBJS)/ReadLUT.o \
		$(OBJS)/ECPRoutines.o $(OBJS)/ReadSAD.o $(OBJS)/CheckFloatArray.o \
		$(OBJS)/CheckByteArray.o $(OBJS)/ReadByteArray.o $(OBJS)/ReadFPArray.o \
		$(OBJS)/ReadIntArray.o $(OBJS)/ReadLocation.o $(OBJS)/ReadLSFlags.o \
		$(OBJS)/ReadGeometry.o $(OBJS)/ReadAlb.o  $(OBJS)/ReadScanLines.o  \
		$(OBJS)/ReadRTMData_nc.o $(OBJS)/GetGeometry.o $(OBJS)/GetMeasurements.o \
		$(OBJS)/GetLwSwRTM.o $(OBJS)/GetRTM.o $(OBJS)/GetRs.o $(OBJS)/GetSurface.o \
		$(OBJS)/XMDAD.o $(OBJS)/GetX.o $(OBJS)/CalcCWP.o $(OBJS)/AllocSPixel.o \
		$(OBJS)/GetSPixel.o $(OBJS)/AllocRTMPc.o $(OBJS)/Tridag.o $(OBJS)/Spline.o \
		$(OBJS)/Bcucof.o $(OBJS)/Bcuint.o $(OBJS)/Linint.o $(OBJS)/Locate.o \
		$(OBJS)/IntLUTTauRe.o $(OBJS)/IntLUTTauSolRe.o $(OBJS)/IntLUTTauSatRe.o \
		$(OBJS)/IntLUTTauSatSolAziRe.o $(OBJS)/SetCRPThermal.o $(OBJS)/SetGZero.o \
		$(OBJS)/R2T.o $(OBJS)/T2R.o $(OBJS)/InterpolThermal.o \
		$(OBJS)/InterpolThermal_spline.o $(OBJS)/FMThermal.o $(OBJS)/SetCRPSolar.o \
		$(OBJS)/InterpolSolar.o $(OBJS)/InterpolSolar_spline.o $(OBJS)/FMSolar.o \
		$(OBJS)/FM.o $(OBJS)/DecomposeCholesky.o $(OBJS)/SolveCholeskyDC.o \
		$(OBJS)/SquareCholeskyDC.o $(OBJS)/InvertCholeskyDC.o $(OBJS)/SolveCholesky.o \
		$(OBJS)/InvertCholesky.o $(OBJS)/SetLimits.o $(OBJS)/CheckLimits.o \
		$(OBJS)/SetUnit.o $(OBJS)/SetKx.o $(OBJS)/SetSy.o $(OBJS)/SetDiag.o \
		$(OBJS)/WriteDiag.o $(OBJS)/ZeroDiag.o $(OBJS)/InvertMarquardt.o \
		$(OBJS)/DeallocSPixel.o $(OBJS)/DeallocSADLUT.o $(OBJS)/DeallocRTM.o \
		$(OBJS)/DeallocRTMPc.o $(OBJS)/DeallocData.o $(OBJS)/DeallocCtrl.o \
		$(OBJS)/nc_create_global.o  $(OBJS)/nc_close.o \
		$(OBJS)/nc_defdata.o $(OBJS)/nc_write_L2.o $(OBJS)/alloc_spixel_scan_out.o \
		$(OBJS)/dealloc_spixel_scan_out.o $(OBJS)/b1_to_si1.o $(OBJS)/b1_to_ui1.o \
		$(OBJS)/GetLSF.o $(OBJS)/GetIllum.o $(OBJS)/Readillum.o \
		$(OBJS)/read_sat_data_nc.o $(OBJS)/ReadAlb_nc.o $(OBJS)/Readillum_nc.o \
		$(OBJS)/ReadLwRTM_nc.o $(OBJS)/ReadSwRTM_nc.o $(OBJS)/ReadCloudFlags_nc.o \
		$(OBJS)/ReadLocation_nc.o $(OBJS)/ReadMSI_nc.o $(OBJS)/ReadScanLines_nc.o \
		$(OBJS)/ReadGeometry_nc.o $(OBJS)/ReadLSFlags_nc.o  \
		$(OBJS)/read_input_dimensions.o $(OBJS)/nc_open.o $(OBJS)/nc_dim_info.o \
		$(OBJS)/nc_read_file.o


# .F90 was originally the extension on DecUnix, but seems to be allowed 
# by the ifc/ifort compiler.
$(OBJS)/%.o: %.f90
	$(F90) -o $@ -c $(FFLAGS) $(INC) $(AUXFLAGS) $(OBJS) $<

$(OBJS)/%.o: %.F90
	$(F90) -o $@ -c $(FFLAGS) $(INC) $(AUXFLAGS) $(OBJS) $<


# Main target list
all : orac 

# Main program build
orac : $(ORAC_OBJ) $(OBJS)/ECPparallel_ecmwf.o Makefile
	$(F90) -o orac $(LFLAGS) $(ORAC_OBJ) $(OBJS)/ECPparallel_ecmwf.o $(LIBS) -I./$(OBJS)
orac_old : $(ORAC_OBJ) $(OBJS)/ECP.o Makefile
	$(F90) -o orac_old $(LFLAGS) $(ORAC_OBJ) $(OBJS)/ECP.o $(LIBS) -I./$(OBJS)

# Tidy up .cpp files produced by C pre-compiler when compilation errors occur
# (screws up later attempts to compile - forces C++ compiler to be run)
tidy_cpp :
	rm -f *.cpp

tidy : 
	rm -f $(OBJS)/*.o $(OBJS)/*.mod


clean_cpp:
	rm -f *.cpp

clean: 
	rm -f $(OBJS)/*.o $(OBJS)/*.mod



# Utilities to change source code extensions when moving between DEC Unix and
# Linux systems. 
DEC2Linux:
	rename.csh d2l

Linux2DEC:
	rename.csh l2d


# Object/source dependencies
ECPConstants.o           : ECPConstants.F90
CPL.o                    : CPL.F90 ECPConstants.F90
CRP.o                    : CRP.F90 ECPConstants.F90
Ctrl.o                   : Ctrl.F90 ECPConstants.F90
Diag.o                   : Diag.F90 ECPConstants.F90
RTM.o                    : RTM.F90 ECPConstants.F90
RTM_Pc.o                 : RTM_Pc.F90  ECPConstants.F90
Data.o                   : Data.F90 ECPConstants.F90
Invert.o                 : Invert.F90 ECPConstants.F90
SADChan.o                : SADChan.F90 ECPConstants.F90
SADLUT.o                 : SADLUT.F90 ECPConstants.F90
IntRoutines.o            : IntRoutines.F90 ECPConstants.F90 GZero.F90 SADLUT.F90 \
			   Bcubic_def.F90         
SADRoutines.o            : SADRoutines.F90 Ctrl.F90 SADChan.F90 ECPConstants.F90 \
                           SADLUT.F90 
ECPRoutines.o            : ECPRoutines.F90 Ctrl.F90 SADChan.F90 ECPConstants.F90 \
                           SADLUT.F90 GZero.F90
FMRoutines.o             : FMRoutines.F90 Ctrl.F90 SPixel.F90 SADLUT.F90 \
                           GZero.F90 IntRoutines.F90 
FindLUN.o                : FindLUN.F90
WriteLog.o               : WriteLog.F90 Ctrl.F90
ReadChan.o               : ReadChan.F90 Ctrl.F90 SADChan.F90 ECPConstants.F90
ReadDriver.o             : ReadDriver.F90 Ctrl.F90 ECPConstants.F90
ReadLUT.o                : ReadLUT.F90 ECPConstants.F90 \
                           SADLUT.F90 Ctrl.F90 local.F90
ReadSAD.o                : ReadSAD.F90 ECPConstants.F90 SADChan.F90 \
                           SADLUT.F90 ReadChan.F90 Ctrl.F90 \
		  	   SADRoutines.F90
ReadByteArray.o          : ReadByteArray.F90
ReadFPArray.o            : ReadFPArray.F90
ReadIntArray.o           : ReadIntArray.F90
ReadLocation.o           : ReadLocation.F90 Ctrl.F90 Data.F90 ECPConstants.F90 
ReadScanLines.o          : ReadLocation.F90 Ctrl.F90 Data.F90 ECPConstants.F90 
ReadLSFlags.o            : ReadLSFlags.F90 Ctrl.F90 Data.F90 ECPConstants.F90
ReadGeometry.o           : ReadGeometry.F90 Ctrl.F90 Data.F90 ECPConstants.F90
ReadAlb.o                : ReadAlb.F90 Ctrl.F90 Data.F90 ECPConstants.F90
ReadRTMData_nc.o         : ReadRTMData_nc.F90 Ctrl.F90 RTM.F90 ReadLwRTM_nc.F90 \
                           ReadSwRTM_nc.F90 ECPConstants.F90
AllocRTMPc.o             : AllocRTMPc.F90 Ctrl.F90 RTM_Pc.F90
AllocSPixel.o            : AllocSPixel.F90 Ctrl.F90 RTM.F90 SPixel.F90
DeallocRTMPc.o           : DeallocRTMPc.F90 Ctrl.F90 RTM_Pc.F90
DeallocSPixel.o          : DeallocSPixel.F90 Ctrl.F90 RTM.F90 SPixel.F90
DeallocRTM.o             : DeallocRTM.F90 Ctrl.F90 RTM.F90 
DeallocSADLUT.o          : DeallocSADLUT.F90 Ctrl.F90 SADLUT.F90 
DeallocData.o            : DeallocData.F90 Ctrl.F90 Data.F90
CheckByteArray.o         : CheckByteArray.F90
CheckFloatArray.o        : CheckFloatArray.F90
GetSPixel.o              : GetSPixel.F90 SPixel.F90 Ctrl.F90 Data.F90 SADChan.F90\
                           CheckFloatArray.F90 CheckByteArray.F90 RTM.F90 \
			   ECPConstants.F90
GetCloudFlags.o          : GetCloudFlags.F90 ECPConstants.F90 Ctrl.F90 Data.F90 \
                           SPixel.F90
GetGeometry.o            : GetGeometry.F90 ECPConstants.F90 Ctrl.F90 Data.F90 \
                           SPixel.F90
GetLocation.o            : GetLocation.F90 ECPConstants.F90 Ctrl.F90 Data.F90 \
                           SPixel.F90
GetMeasurements.o        : GetMeasurements.F90 ECPConstants.F90 Ctrl.F90 Data.F90 \
                           SPixel.F90
GetRTM.o                 : GetRTM.F90 Ctrl.F90 SADChan.F90 SPixel.F90 \
                           GetLwSwRTM.F90 ECPConstants.F90 
GetLwSwRTM.o             : GetLwSwRTM.F90 ECPConstants.F90 RTM.F90 Ctrl.F90 \
                           SADChan.F90 SPixel.F90
b1_to_si1.o		 : b1_to_si1.F90
b1_to_ui1.o 		 : b1_to_ui1.F90
GetSurface.o             : GetSurface.F90 GetRs.F90 SPixel.F90 Ctrl.F90 Data.F90 b1_to_ui1.o
GetRs.o                  : GetRs.F90 GetSurface.F90 SPixel.F90 Ctrl.F90 Data.F90
GetX.o                   : GetX.F90 ECPConstants.F90 Ctrl.F90 SPixel.F90 
XMDAD.o                  : XMDAD.F90 ECPConstants.F90 Ctrl.F90 SPixel.F90  
XSDAD.o                  : XSDAD.F90 ECPConstants.F90 SPixel.F90 
R2T.o                    : R2T.F90 Ctrl.F90 SADChan.F90
T2R.o                    : T2R.F90 Ctrl.F90
GZero.o                  : GZero.F90
Bcucof.o                 : Bcucof.F90
Bcuint.o                 : Bcuint.F90 Bcucof.F90
Linint.o                 : Linint.F90
Locate.o                 : Locate.F90
Bcubic_def.o             : Bcubic_def.F90
IntLUTTauRe.o            : IntLUTTauRe.F90 Ctrl.F90 SADLUT.F90 GZero.F90 Bcubic_def.F90
IntLUTTauSolRe.o         : IntLUTTauSolRe.F90 Ctrl.F90 SADLUT.F90 GZero.F90 Bcubic_def.F90
IntLUTTauSatRe.o         : IntLUTTauSatRe.F90 Ctrl.F90 SADLUT.F90 GZero.F90 Bcubic_def.F90
IntLUTTauSatSolAziRe.o   : IntLUTTauSatSolAziRe.F90 Ctrl.F90 SADLUT.F90 \
			   GZero.F90 Bcubic_def.F90
SetCRPSolar.o            : SetCRPSolar.F90 SADLUT.F90 GZero.F90 IntRoutines.F90 \
                           Ctrl.F90 SPixel.F90
SetCRPThermal.o          : SetCRPThermal.F90 SADLUT.F90 GZero.F90 IntRoutines.F90 \
                           Ctrl.F90 SPixel.F90
SetGZero.o               : SetGZero.F90 SPixel.F90 SADLUT.F90 GZero.F90 IntRoutines.F90
FMSolar.o                : FMSolar.F90 RTM_Pc.F90 ECPConstants.F90 Ctrl.F90 \
                           SPixel.F90 SADLUT.F90 GZero.F90 FMRoutines.F90
FMThermal.o              : FMThermal.F90 RTM_Pc.F90 ECPConstants.F90 Ctrl.F90 \
                           SPixel.F90 SADChan.F90 SADLUT.F90 GZero.F90 \
			   FMRoutines.F90
InterpolSolar.o          : InterpolSolar.F90 RTM_Pc.F90 ECPConstants.F90 Ctrl.F90 \
                           SPixel.F90
InterpolSolar_spline.o   : InterpolSolar_spline.F90 InterpolSolar.F90 RTM_Pc.F90 \
                           ECPConstants.F90 Ctrl.F90 \
                           SPixel.F90 Spline.F90 Tridag.F90 IntRoutines.F90 
InterpolThermal.o        : InterpolThermal.F90 RTM_Pc.F90 ECPConstants.F90 \
                           Ctrl.F90 SPixel.F90 SADChan.F90
InterpolThermal_spline.o : InterpolThermal_spline.F90 InterpolThermal.F90 RTM_Pc.F90 \
			   ECPConstants.F90 Ctrl.F90 \
			   SPixel.F90 SADChan.F90  Spline.F90 Tridag.F90 IntRoutines.F90
Spline.o                 : Spline.F90 Tridag.F90 IntRoutines.F90
Tridag.o                 : Tridag.F90 
FM.o                     : FM.F90 Ctrl.F90 SPixel.F90 SADChan.F90 SADLUT.F90 \
                           RTM_Pc.F90 ECPConstants.F90 GZero.F90 \
                           InterpolThermal.F90 InterpolSolar.F90 \
                           InterpolThermal_spline.F90 Tridag.F90 InterpolSolar_spline.F90
ECP.o                    : ECP.F90 Ctrl.F90 ECPConstants.F90 SADChan.F90 \
                           ECPRoutines.F90 SPixel.F90 CalcCWP.F90\
			   ReadRTMData.F90 SADLUT.F90 Data.F90 RTM.F90 \
                           prepare_primary.inc prepare_secondary.inc \
                           write_primary.inc write_secondary.inc \
                           def_vars_input.inc  def_vars_primary.inc  def_vars_secondary.inc
ECPparallel_ecmwf.o      : ECPparallel_ecmwf.F90 Ctrl.F90 ECPConstants.F90 SADChan.F90 \
                           ECPRoutines.F90 SPixel.F90 CalcCWP.F90\
			   ReadRTMData.F90 SADLUT.F90 Data.F90 RTM.F90 \
                           prepare_primary.inc prepare_secondary.inc \
                           write_primary.inc write_secondary.inc \
                           def_vars_input.inc  def_vars_primary.inc  def_vars_secondary.inc
CalcCWP.o                : CalcCWP.F90 ECPConstants.F90 Ctrl.F90 
DecomposeCholesky.o      : DecomposeCholesky.F90 ECPConstants.F90
SolveCholeskyDC.o        : SolveCholeskyDC.F90
InvertCholeskyDC.o       : InvertCholeskyDC.F90
SquareCholeskyDC.o       : SquareCholeskyDC.F90
SolveCholesky.o          : SolveCholesky.F90 DecomposeCholesky.F90 \
                           SolveCholeskyDC.F90
InvertCholesky.o         : InvertCholesky.F90 DecomposeCholesky.F90 \
                           InvertCholeskyDC.F90 SquareCholeskyDC.F90
SetLimits.o              : SetLimits.F90 ECPConstants.F90 SPixel.F90 RTM_Pc.F90 \
                           Ctrl.F90
CheckLimits.o            : CheckLimits.F90 ECPConstants.F90 Ctrl.F90 SPixel.F90 
SetUnit.o                : SetUnit.F90 ECPConstants.F90 SPixel.F90
SetKx.o                  : SetKx.F90 ECPConstants.F90 Ctrl.F90 SPixel.F90
SetSy.o                  : SetSy.F90 ECPConstants.F90 SPixel.F90
SetDiag.o                : SetDiag.F90 ECPConstants.F90 Ctrl.F90 SPixel.F90 \
                           Diag.F90
ZeroDiag.o               : ZeroDiag.F90 ECPConstants.F90 Ctrl.F90 Diag.F90
WriteDiag.o              : WriteDiag.F90 ECPConstants.F90 Ctrl.F90 SPixel.F90 \
                           Diag.F90
InvertMarquardt.o        : InvertMarquardt.F90 ECPConstants.F90 Ctrl.F90 \
                           SPixel.F90 SADChan.F90 SADLUT.F90 \
			   RTM_Pc.F90
nc_create_global.o       : nc_create_global.F90 
nc_close.o               : nc_close.F90
nc_def_data.o            : nc_defdata.F90
nc_write_L2.o            : nc_write_L2.F90
alloc_spixel_scan_out.o  : alloc_spixel_scan_out.F90
dealloc_spixel_scan_out.o: dealloc_spixel_scan_out.F90
input_structures.o       : input_structures.F90
DeallocCtrl.o	 	 : DeallocCtrl.F90
GetLSF.o                 : GetLSF.F90
GetIllum.o               : GetIllum.F90
Readillum.o              : Readillum.F90
read_sat_data_nc.o       : read_sat_data_nc.F90
ReadAlb_nc.o             : ReadAlb_nc.F90
Readillum_nc.o           : Readillum_nc.F90
ReadLwRTM_nc.o           : ReadLwRTM_nc.F90
ReadCloudFlags_nc.o      : ReadCloudFlags_nc.F90
ReadLocation_nc.o        : ReadLocation_nc.F90
ReadMSI_nc.o             : ReadMSI_nc.F90
ReadScanLines_nc.o       : ReadScanLines_nc.F90
ReadGeometry_nc.o        : ReadGeometry_nc.F90
ReadLSFlags_nc.o         : ReadLSFlags_nc.F90
ReadSwRTM_nc.o           : ReadSwRTM_nc.F90
read_input_dimensions.o  : read_input_dimensions.F90
nc_open.o                : nc_open.F90
nc_dim_info.o            : nc_dim_info.F90
nc_read_file.o           : nc_read_file.F90
