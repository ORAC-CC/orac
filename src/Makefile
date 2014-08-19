# Make file for ORAC
#
# History:
#   2nd Aug 2000, Andy Smith: original version
#   16th Nov 2000, Andy Smith:
#      Updated to link with new IntLUTTauReSun routine.
#      Added tidy_cpp target
#   17th Nov 2000, Andy Smith:
#      Int LUT routines renamed to follow order of Tau, SatZen, SunZen etc in
#      the LUT array dimensions.
#   22nd Nov 2000, Andy Smith:
#      Adding ReadATSR routines to ECP.
#   23rd Nov 2000, Andy Smith:
#      Added a few dependencies on Ctrl.f90 where previously omitted.
#      Added note 2.
#   24th Nov 2000, Andy Smith:
#      Added SPixel structure.
#      Added R2T and T2R test harness
#   30th Nov 2000, Andy Smith:
#      .f90 extensions replaced by .f90 following move from ATSRLX to ATSRUS
#    1st Dec 2000, Andy Smith:
#      Int*Sun* routines renamed to use Sol instead of Sun.
#      SetCRPSolar stripped out of ECP main program since separate test harness
#      exists.
#      Updated HarnessSetCRPsolar dependencies.
#   14th Dec 2000, Kevin Smith:
#      Added test harness HarnessReadRTMData to test all read RTM subroutines
#   15th Dec 2000, Phil Watts:
#      Added test harness HarnessReadLUT to test LUT ingest routine
#   19th Dec 2000, Kevin Smith:
#      Added test harness HarnessReadSat to test ReadSatData ingest routines
#   20th Dec 2000, Kevin Smith:
#      Added test harness HarnessGetSPixel to test GetSPixel and subroutines
#   15th Jan 2001, Kevin Smith:
#      Added test harness HarnessGetRTM to test GetRTM and subroutines
#   17th Jan 2001, Andy Smith:
#      Added test harness HarnessSetCRPThermal.
#   18th Jan 2001, Kevin Smith:
#      Added test harness HarnessGetSurface to test GetSurface and GetRs
#   19th Jan 2001, Andy Smith:
#      New interface definition module FMRoutines.
#   23rd Jan 2001, Andy Smith:
#      New routine SetGZero.
#      New test harness HarnessFMSolar
#   24th Jan 2001, Kevin Smith
#      New test harness HarnessReadnGet
#      Integrated Get_Spixel and subroutines in ECP
#   18th May 2001, Tim Nightingale
#      Added Cholesky routines and test harness HarnessCholesky
#   6th June 2001, Andy Smith
#      Added GetX to FM test harnesses.
#      Added harness for Invert_Marquardt
#   18th Jun 2001, Andy Smith
#      Added targets to help with porting between DEC and Linux systems.
#      DEC2Linux and Linux2DEC to change source code file extensions.
#      LIBS macro for additional libraries required by ABSOFT compiler.
#   15th Aug 2001, Andy Smith:
#      Added updated ecp target to build integrated ecp executable.
#   23rd Aug 2001, Andy Smith:
#      Missing dependency added (SetLimits : Ctrl.f90)
#   18th Sep 2001, Andy Smith:
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
#      *************************************************************************
#      Essential Climate Variable work starts here
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
#      Re-applying changes from late 2001/2. Added ReadATSRAlb, ReadIntArray.
#      Added deallocData to ReadSatData harness.
#   22nd Mar 2011, Andy Smith:
#      Added DumpECPoutfile program.
#   30th Mar 2011, Andy Smith:
#      GetCLoudFlags and GetLocation no longer called from Get_SPixel following
#      removal of super-pixel averaging. Updated object file lists for ECP and
#      all currently supported test harnesses.
#      Extended "all" target to build all currently-supported test harnesses
#      (swapped order of targets so that "all" is built by default).
#   6th Apr 2011, Andy Smith:
#      Removal of redundant selection methods SAD, SDAD for a priori, first
#      guess etc.  XSDAD.f90 is now redundant.
#   14th Apr 2011, Andy Smith:
#      HarnessReadDriver needs dependency on ECP_Routines.
#      New program CreateDummyGeo to write a dummy multiple-view geometry file.
#   21st Apr 2011, Andy Smith:
#      Add HarnessGetSPixel to supported test harnesses again.
#   18th May 2011, Andy Smith:
#      New program CreateDummyMSI to write a dummy multi-view MSI file.
#   21st May 2011, Andy Smith:
#      Tidy-up for initial community access. Rename "ecp" executable to "orac".
#      Updated Notes below.
#      Removed object lists for unsupported test harnesses (saved in separate
#      file).
#   28th Jun 2011, Caroline Poulsen:
#      remove refernce to ATSR routines
#   28th Jul 2011, Caroline Poulsen:
#      added in scanline routines
#   28th Jul 2011,
#      removed ref to cloud class routines
#   22nd Sep 2011, Caroline Poulsen:
#      change GetLwRTM to getLwSwRTM remove getswrtm
#   5th October 2011, Chris Arnold:
#      included new routines ... InterpolThermal_spline InterpolSolar_spline
#      Bcubic_def.f90 Bcucof.f90 Bcuint.f90 Linint.f90 Locate.f90 Spline.f90
#      Tridag.f90 and updated dependencies
#   8th Oct 2011,
#      updated Harness info and include CWP_calc found lots of instances
#      where bcubic_def not  defined
#   2nd Nov 2011, Caroline Poulsen:
#      included interpolthermal in FM dependacies as they had
#      accidently been removed.
#   8th Dec 2011, Matthias Jerg:
#      added netcdf related routines nc_create_global.F90 nc_close.F90
#      nc_defdata.F90 nc_write_L2.F90 alloc_spixel_scan_out.F90 and paths to
#      netcdf libraries. Suffix was changed to *.F90 in order to ensure the
#      application of the c preprocessor for compilers where this can not be
#      explicitly requested by a flag.
#   13th Dec 2001,
#      added in byte to integer routine and dealloccrl routine
#   24th Aug 2012, MJ:
#      creates adds "includes" for better configurability
#   3rd Dec 2013, Greg McGarragh:
#      Added object files and dependencies for recent chnages to ORAC.
#   3rd Dec 2013, Greg McGarragh:
#      Renewed object list and dependencies and some small clean ups.
#   4th Dec 2013, Greg McGarragh:
#      Added 'make depend' to automatically find all Fortran file dependencies.
#   29th Jan 2014, Greg McGarragh:
#      Changes related to renaming ECPparallel_ecmwf.F90 to ECP.F90.
#   18th Apr 2014, Greg McGarragh:
#      include $(ORAC_ARCH) and include $(ORAC_LIB) instead of site specific
#      hardwires.
#
# Notes:
# 1) The C preprocessor is used to allow conditional compilation (files with
# extension .f90 are automatically passed to cpp). If a unit compilation
# produces an error this can result in creation of a .cpp file for that unit. On
# the next attempt to compile, make finds the .cpp file first and runs g++ to
# try to make the .o file from the .cpp. To stop this happening, use make -r.
# (Or use the "tidy" target - N.B. you can't do this every time as a precaution
# because if no .cpp files are found the command fails and the make stops).
#
# 2) The default compilation rule includes the -g flags for development and
# debugging.
#
# 3) To compile using breakpoints, do make <program> FFLAGS=-DBKP. This defines
# the BKP macro used for conditional compilation, such that breakpoint code is
# included. (Also available in some routines is DEBUG code). Breakpoint levels
# are defined for each routine in ECPConstants.f90.
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


# Set files from which compiler, flags, and libraries are set. archfile also
# defines the "$(OBJS)" variable, which places object and module files in a
# subdirectory
include $(ORAC_LIB)
include $(ORAC_ARCH)


# Define object files to be linked. N.B. files containing module definitions
# should be listed before main program units and subroutines that use those
# modules so that the modules are known to the compiler.
ORAC_OBJ =        $(OBJS)/CalcCWP.o \
                  $(OBJS)/CheckLimits.o \
                  $(OBJS)/check_value.o \
                  $(OBJS)/config.o \
                  $(OBJS)/CPL.o \
                  $(OBJS)/CRP.o \
                  $(OBJS)/Ctrl.o \
                  $(OBJS)/Data.o \
                  $(OBJS)/DecomposeCholesky.o \
                  $(OBJS)/Diag.o \
                  $(OBJS)/ECPConstants.o \
                  $(OBJS)/extrap_into_tropopause.o \
                  $(OBJS)/FindLUN.o \
                  $(OBJS)/FMRoutines.o \
                  $(OBJS)/GetGeometry.o \
                  $(OBJS)/GetIllum.o \
                  $(OBJS)/GetLSF.o \
                  $(OBJS)/GetLwSwRTM.o \
                  $(OBJS)/GetMeasurements.o \
                  $(OBJS)/GetRs.o \
                  $(OBJS)/GetRTM.o \
                  $(OBJS)/GetSPixel.o \
                  $(OBJS)/GetSurface.o \
                  $(OBJS)/GetX.o \
                  $(OBJS)/GZero.o \
                  $(OBJS)/hpsort.o \
                  $(OBJS)/interpolate_to_ctp.o \
                  $(OBJS)/IntRoutines.o \
                  $(OBJS)/InterpolRoutines.o \
                  $(OBJS)/IntLUTRoutines.o \
                  $(OBJS)/InvertCholesky.o \
                  $(OBJS)/InvertCholeskyDC.o \
                  $(OBJS)/InvertMarquardt.o \
                  $(OBJS)/locate.o \
                  $(OBJS)/output_routines.o \
                  $(OBJS)/nc_create_global.o \
                  $(OBJS)/nc_defdata.o \
                  $(OBJS)/nc_write_L2.o \
                  $(OBJS)/polint.o \
                  $(OBJS)/R2T.o \
                  $(OBJS)/read_config_file.o \
                  $(OBJS)/read_input_dimensions.o \
                  $(OBJS)/read_sat_data_nc.o \
                  $(OBJS)/ReadALB_nc.o \
                  $(OBJS)/ReadCloudFlags_nc.o \
                  $(OBJS)/ReadDriver.o \
                  $(OBJS)/ReadGeometry_nc.o \
                  $(OBJS)/ReadIllum_nc.o \
                  $(OBJS)/ReadLocation_nc.o \
                  $(OBJS)/ReadLSFlags_nc.o \
                  $(OBJS)/ReadLwRTM_nc.o \
                  $(OBJS)/ReadMSI_nc.o \
                  $(OBJS)/ReadSAD.o \
                  $(OBJS)/ReadScanLines_nc.o \
                  $(OBJS)/ReadSwRTM_nc.o \
                  $(OBJS)/RTM.o \
                  $(OBJS)/RTM_Pc.o \
                  $(OBJS)/SADChan.o \
                  $(OBJS)/SADCloudClass.o \
                  $(OBJS)/SADLUT.o \
                  $(OBJS)/SetDiag.o \
                  $(OBJS)/SetGZero.o \
                  $(OBJS)/SetKx.o \
                  $(OBJS)/SetLimits.o \
                  $(OBJS)/SetSy.o \
                  $(OBJS)/SetUnit.o \
                  $(OBJS)/SolveCholesky.o \
                  $(OBJS)/SolveCholeskyDC.o \
                  $(OBJS)/SPixel.o \
                  $(OBJS)/SquareCholeskyDC.o \
                  $(OBJS)/t_extrap_strat.o \
                  $(OBJS)/T2R.o \
                  $(OBJS)/WriteDiag.o \
                  $(OBJS)/WriteLog.o \
                  $(OBJS)/XMDAD.o \
                  $(OBJS)/XSDAD.o \
                  $(OBJS)/ZeroDiag.o

ORAC_OBJ_MAIN =   $(OBJS)/ECP.o \


# .F90 was originally the extension on DecUnix, but seems to be allowed by the
# ifc/ifort compiler.
$(OBJS)/%.o: %.f90
	$(F90) -o $@ -c $(FFLAGS) $(INC) $(AUXFLAGS) $<

$(OBJS)/%.o: %.F90
	$(F90) -o $@ -c $(FFLAGS) $(INC) $(AUXFLAGS) $<


# Main target list
all: orac

# Main program build
orac: $(ORAC_OBJ) $(ORAC_OBJ_MAIN) Makefile
	$(F90) -o orac $(LFLAGS) $(ORAC_OBJ) $(OBJS)/ECP.o $(LIBS) -I./$(OBJS)

orac.lib: $(ORAC_OBJ) $(ORAC_OBJ_MAIN) Makefile
	ar -vr liborac.a $(OBJS)/*.o

# Rebuild dependencies housed in dependencies.inc
depend:
	@../tools/make_depend.pl $(ORAC_OBJ) $(ORAC_OBJ_MAIN) $(ORAC_OBJ_MAIN_OLD) > dependencies.inc

# Tidy up .cpp files produced by C pre-compiler when compilation errors occur
# (screws up later attempts to compile - forces C++ compiler to be run)
tidy_cpp clean_cpp:
	rm -f *.cpp

tidy clean:
	rm -f $(OBJS)/*.o $(OBJS)/*.mod orac


# Utilities to change source code extensions when moving between DEC Unix and
# Linux systems.
DEC2Linux:
	rename.csh d2l

Linux2DEC:
	rename.csh l2d


# Object/source dependencies
include dependencies.inc
