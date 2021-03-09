# Makefile for ORAC
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
#   22nd Aug 2014, Greg McGarragh: Use the library file instead of the object
#    list for the executable and make the executable dependent on the library
#    file. Now 'make' will build both the executable and library in one command.
#   13th June 2016, Adam Povey:
#      Add driver file parser. Remove inaccurate notes.
#
# Notes:
#

# Set files from which compiler, flags, and libraries are set. archfile also
# defines the "$(OBJS)" variable, which places object and module files in a
# subdirectory
include $(ORAC_LIB)
include $(ORAC_ARCH)

ORAC_COMMON = ../common


# Define object files to be linked.
ORAC_OBJ = $(OBJS)/cholesky.o \
           $(OBJS)/ctrl.o \
           $(OBJS)/data.o \
           $(OBJS)/diag.o \
           $(OBJS)/find_lun.o \
           $(OBJS)/fm_routines.o \
           $(OBJS)/gzero.o \
           $(OBJS)/int_routines.o \
           $(OBJS)/interpol_routines.o \
           $(OBJS)/int_lut_routines.o \
           $(OBJS)/inversion.o \
           $(OBJS)/orac_constants.o \
           $(OBJS)/planck.o \
           $(OBJS)/prepare_output.o \
           $(OBJS)/read_utils.o \
           $(OBJS)/read_driver.o \
           $(OBJS)/read_sad.o \
           $(OBJS)/rtm.o \
           $(OBJS)/rtm_pc.o \
           $(OBJS)/sad_chan.o \
           $(OBJS)/sad_lut.o \
           $(OBJS)/sad_util.o \
           $(OBJS)/spixel.o \
           $(OBJS)/spixel_routines.o

ORAC_OBJ_MAIN = $(OBJS)/orac.o


# Compilation rules
$(OBJS)/%.o: %.f90 $(ORAC_COMMON)/liborac_common.a
	$(F90) -o $@ -c $(FFLAGS) $(INC) -I$(ORAC_COMMON)/obj $(AUXFLAGS) $<

$(OBJS)/%.o: %.F90 $(ORAC_COMMON)/liborac_common.a
	$(F90) -o $@ -c $(FFLAGS) $(INC) -I$(ORAC_COMMON)/obj $(AUXFLAGS) $<

# Rule to prevent make from identifying Fortran .mod files as Modula2 source
# files
%.o : %.mod


# Main target list
all: orac

# Build parser for driver files from Ctrl, common_constants, and ORACConstants
STRUCT_NAME     = Ctrl
STRUCT_TYPE     = Ctrl_t
PARSER_NAME     = read_ctrl
PRINTER_NAME    = print_ctrl
PARSER_SOURCE   = $(ORAC_COMMON)/common_constants.F90 \
                  $(ORAC_COMMON)/constants_cloud_typing_pavolonis.F90 \
                  $(ORAC_COMMON)/orac_indexing.F90 orac_constants.F90 ctrl.F90
USE_STATEMENTS  = "use Ctrl_m; use ORAC_constants_m"
PLACE           = $(OBJS)/$(PARSER_NAME)
PARSER_INC      = -I$(ORAC_COMMON) -I../src/$(OBJ)
include $(ORAC_COMMON)/Makefile_struct_parser
$(OBJS)/ReadDriver.o: $(PLACE).f.o
$(PLACE).f.o: $(OBJS)/orac_constants.o $(OBJS)/ctrl.o

# Main program build
orac: $(ORAC_OBJ_MAIN) liborac.a $(ORAC_COMMON)/liborac_common.a Makefile
	$(F90) -o orac $(LFLAGS) $(ORAC_OBJ_MAIN) liborac.a $(ORAC_COMMON)/liborac_common.a -I./$(OBJS) $(LIBS)

# Main library build
liborac.a: $(ORAC_OBJ) $(PARSER_OBJ) Makefile
	ar -rs liborac.a $(ORAC_OBJ) $(PARSER_OBJ)

# Rebuild dependencies housed in dependencies.inc
depend:
	@../tools/make_depend.pl $(ORAC_OBJ) $(ORAC_OBJ_MAIN) > dependencies.inc

clean tidy:
	rm -f $(OBJS)/*.o $(OBJS)/*.mod *.a orac $(PARSER_CLEAN)


# Object/source dependencies
include dependencies.inc
