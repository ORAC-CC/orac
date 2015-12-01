#!/bin/bash
# Name:
#    header.sh
#
# Purpose:
#    A common header for the ORAC regression tests.
#    - The first section defines paths to the ORAC code and data repositories.
#      These should be altered to represent your local file system.
#    - The second section specifies the headers for the output NCDF files.
#      These should be altered to describe your institution.
#    - The third section deduces various required folders from the values given
#      in the first section. If you do not use the Trac repository folder
#      structure or wish to use different files for another reason, use the
#      appropriate command line arguments to override these choices.
#    - The fourth section process the command line arguments, described below.
#    - The fifth section uses the contents of the ORAC library file to update
#      LD_LIBRARY_PATH such that the executable can locate the shared libraries.
#    - The sixth section defines the parameters of the various regression tests.
#
#    This script should not be called indendendently. It will do nothing.
#
# Arguments:
# COMMON ARGUMENTS)
# -do             Select which of the test cases to process. This argument may
#                 be used multiple times to select multiple options. They are:
#    DAYAATSR     The daytime section of an AATSR orbit.
#    NITAATSR     The nighttime section of an AATSR orbit.
#    AATSR        DAYAATS and NITAATSR.
#    DAYAVHRR     The daytime section of an AVHRR orbit.
#    NITAVHRR     The nighttime section of an AVHRR orbit. Only exists w/ -short.
#    AVHRR        DAYAVHRR and NITAVHRR.
#    DAYMYD       The daytime section of an MODIS-AQUA orbit.
#    NITMYD       The nighttime section of an MODIS-AQUA orbit.
#    MYD|MODIS    DAYMYD and NITMYD.
#    DAY          DAYAATSR, DAYAVHRR, DAYMYD.
#    NIT|NIGHT    NITAATSR, NITAVHRR, NITMYD.
#    ALL          DAYAATS, NITAATSR, AVHRR, DAYMYD, and NITMYD; default option.
# -first          By default, the script determines the revision number from svn
#                 and then increments it by 10000 as you are comparing altered
#                 code to the previous commited revision. Set this argument to
#                 not increment the counter (presumably because you are
#                 processing unaltered code). Implies -no_compare.
# -idl_folder     The path of your IDL executable.
# -ld_set         Do not set the LD_LIBRARY_PATH variable.
# -in_folder      The path containing the input data files.
# -n_procs        The maximum number of simultaneous processes allowed. The
#                 default is 5.
# -no_compare     Do not compare the output files to a previous version after
#                 processing.
# -only_compare   Do not run processor. Only run the regression test.
# -out_folder     The path where the outputs should be stored.
# -orac_folder    The path containing the ORAC executable.
# -orac_lib       Name of the library file for the ORAC preprocessor, from which
#                 LD_LIBRARY_PATH will be generated.
# -preproc_folder The path containing the ORAC preprocessor executable.
# -postproc_folder The path containing the ORAC postprocessor executable.
# -revision       Force the code to use the given revision number. Implies
#                 -first but not -no_compare.
# -short          Process only a small segment of each of the test files (5
#                 lines). This is significantly faster than the default
#                 processing of the full files but may not catch all possible
#                 circumstances.
# -thresh         The threshold to be used in the rounding error regression test.
# -tool_folder    The path containing the ORAC tools (i.e. these scripts).
#
# PREPROCESSOR ONLY ARGUMENTS)
# -albedo_folder  The path containing albedo files (in folders by year).
# -brdf_off       Do not use the BRDF surface treatment.
# -calib_folder   The path containing ATSR drift coefficients.
# -coeffs_folder  The path containing channel information.
# -ecmwf|-badc    Value of ecmwf_flag to use. 0: Use a single GRIB file output
#                 by MARS. 1: Use three NCDF files regridded from the BADC.
#                 2: Use the one NCDF and two GRIB files native to BADC.
# -ecmwf_folder   The path containing ECMWF files if using GRIB files.
# -emiss_atlas_folder The path containing the emissivity atlas.
# -emiss_folder   The path containing MODIS emissivity files.
# -ggam_folder    The path containing NCDF ECMWF files named ggam.
# -ggas_folder    The path containing NCDF ECMWF files named ggas.
# -gpam_folder    The path containing NCDF ECMWF files named gpam.
# -ice_folder     The path containing NISE ice/snow files.
# -usgs_file      The file name of a USGS land-use data file.
# -verbose        Print all status statements during processing.
#
# MAIN PROCESSOR ONLY ARGUMENTS)
# -drop           Don't use the second and fifth channels. Mostly intended for
#                 bug identification rather than useful processing.
# -chan           A more general form of -drop, this directly sets the channel
#                 processing flag (a string on space-delimited 0/1's for each
#                 channel).
# -ICE            Use the ice-cloud look-up tables rather than the default WAT.
# -v21            Use version 2.1 AATSR data. Default is version 2.0.
#
# Known issues:
#    For the NCDF header variables, none may contain spaces.
#
# History :
# 2014/07/28, AP: Original version (from test_orac.sh and test_preproc.sh).
# 2014/11/21, GM: Added $brdf_folder.
# 2015/01/09, AP: Ammending how -drop and -v21 are managed. Adding -chan.
#
#------------------------------------------------------------------------------
# DEFINE LOCAL FOLDERS
#------------------------------------------------------------------------------
# path to ORAC repository
orac_repos=/network/home/aopp/povey/orac/trunk

# path to ORAC data repository
data_repos=/local/home/povey/povey/data

# path to SAD repository
sad_repos=/local/home/povey/povey/sad_dir

# path to RTTOV installation
rttov_dir=/local/home/povey/povey/libs/code/rttov112

# path to IDL executable
idl_folder=/usr/local/PACK/idl83/idl83/bin

#------------------------------------------------------------------------------
# DEFINE NCDF FILE HEADERS
#------------------------------------------------------------------------------
ncdf_version='4.3.1'
cf_convention='CF-1.4'
processing_inst='UoOx'
l2processor='ORAC'
contact_email='povey@atm.ox.ac.uk'
contact_website='www2.physics.ox.ac.uk/contacts/people/povey'
file_version='V1.0'
reference='ATBD'
hist='xxx'
summary='A_file_for_code_testing_purposes_only.'
keywords='Debugging,Testing'
comment='xxx'
#project='ESA_Cloud_cci' --- Being misused for file naming
license='http://proj.badc.rl.ac.uk/orac/wiki/License'

#------------------------------------------------------------------------------
# DETERMINE NECESSARY FOLDERS
#------------------------------------------------------------------------------
# assume the hierachy of the ORAC repository
orac_folder=$orac_repos/src
preproc_folder=$orac_repos/pre_processing
postproc_folder=$orac_repos/post_processing
tool_folder=$orac_repos/tools
in_folder=$data_repos/testinput
out_folder=$data_repos/testoutput

# path to albedo files (in folders by year)
albedo_folder=$data_repos/albedo

# path to brdf files (in folders by year)
brdf_folder=$data_repos/albedo

# path to channel information
coeffs_folder=$rttov_dir/rtcoef_rttov11
#coeffs_folder=$data_repos/coeffs/org

# path to emissivity atlas
emiss_atlas_folder=$rttov_dir/emis_data
#emiss_atlas_folder=$data_repos/emissivity

# path to ATSR drift coefficients
calib_folder=$data_repos/AATSR_VIS_DRIFT_V03-00.DAT

# path where ECMWF files are located (one argument per extension)
ggam_folder=$data_repos/ecmwf
ggas_folder=$data_repos/ecmwf
gpam_folder=$data_repos/ecmwf

# path where ice/snow files are located
ice_folder=$data_repos/ice_snow

# path where modis emissivity files are located
emiss_folder=$data_repos/emissivity

# path for USGS land use file
path_to_usgs=$data_repos/Aux_file_CM_SAF_AVHRR_GAC_ori_0.05deg.nc

#------------------------------------------------------------------------------
# MANAGE SETTINGS
#------------------------------------------------------------------------------
# default settings
phase="WAT"
short=0
n_procs=5
do_all=1
do_DAYAATSR=0
do_NITAATSR=0
do_DAYAVHRR=0
do_NITAVHRR=0
do_DAYMYD=0
do_NITMYD=0
do_CLOUD=0
do_compare=1
skip_proc=0
new=1
revision=0
ver21=0
ld_set=1
drop=0
badc_flag=1
verbose=true
brdf_flag=true
channels='1 1 1 1 1 1'
sabotage=0

# perl command used at end of script. specifies which lines of log file
# are printed
perl='print if ($_=~"No. of retrievals converged" or $_=~"Avge cost per conv")'

# deal with command arguments
while [[ $# > 0 ]]; do
    case "$1" in
        -albedo_folder)
            shift
            albedo_folder="$1"
            ;;
        -badc)
            shift
            badc_flag="$1"
            ;;
        -brdf_off)
            brdf_flag=false
            ;;
        -calib_folder)
            shift
            calib_folder="$1"
            ;;
        -chan)
            shift
            channels="$1"
            drop=1
            ;;
        -coeffs_folder)
            shift
            coeffs_folder="$1"
            ;;
        -drop)
            channels='1 0 1 1 0 1'
            drop=1
            ;;
        -ecmwf)
            shift
            badc_flag="$1"
            ;;
        -ecmwf_folder)
            shift
            ggam_folder="$1"
            badc_flag=0
            ;;
        -emiss_atlas_folder)
            shift
            emiss_atlas_folder="$1"
            ;;
        -emiss_folder)
            shift
            emiss_folder="$1"
            ;;
        -first)
            do_compare=0
            new=0
            ;;
        -ggam_folder)
            shift
            ggam_folder="$1"
            badc_flag=1
            ;;
        -ggas_folder)
            shift
            ggas_folder="$1"
            badc_flag=1
            ;;
        -gpam_folder)
            shift
            gpam_folder="$1"
            badc_flag=1
            ;;
        -ICE)
            phase="ICE"
            ;;
        -ice_folder)
            shift
            ice_folder="$1"
            ;;
        -idl_folder)
            shift
            idl_folder="$1"
            ;;
        -in_folder)
            shift
            in_folder="$1"
            ;;
        -ld_set)
            ld_set=0
            ;;
        -no_compare)
            do_compare=0
            ;;
        -n_procs)
            shift
            n_procs="$1"
            ;;
        -only_compare)
            skip_proc=1
            ;;
        -orac_lib)
            shift
            ORAC_LIB="$1"
            ;;
        -orac_folder)
            shift
            orac_folder="$1"
            ;;
        -out_folder)
            shift
            out_folder="$1"
            ;;
        -preproc_folder)
            shift
            preproc_folder="$1"
            ;;
        -revision)
            shift
            revision="$1"
            ;;
        -sabotage)
            sabotage=1
            ;;
        -short)
            short=1
            ;;
        -thresh)
            shift
            thresh="$1"
            ;;
        -tool_folder)
            shift
            tool_folder="$1"
            ;;
        -usgs_file)
            shift
            path_to_usgs="$1"
            ;;
        -verbose)
            verbose=true
            ;;
        -v21)
            ver21=1
            ;;
        -do)
            shift
            case "$1" in
                DAYAATSR)
                    do_DAYAATSR=1
                    do_all=0
                    ;;
                NITAATSR)
                    do_NITAATSR=1
                    do_all=0
                    ;;
                ATSR|AATSR)
                    do_DAYAATSR=1
                    do_NITAATSR=1
                    do_all=0
                    ;;
                DAYAVHRR)
                    do_DAYAVHRR=1
                    do_all=0
                    ;;
                NITAVHRR)
                    do_NITAVHRR=1
                    do_all=0
                    ;;
                AVHRR)
                    do_DAYAVHRR=1
                    do_NITAVHRR=1
                    do_all=0
                    ;;
                CLOUD)
                    do_CLOUD=1
                    do_all=0
                    ;;
                DAYMYD)
                    do_DAYMYD=1
                    do_all=0
                    ;;
                NITMYD)
                    do_NITMYD=1
                    do_all=0
                    ;;
                MYD|MODIS)
                    do_DAYMYD=1
                    do_NITMYD=1
                    do_all=0
                    ;;
                DAY)
                    do_DAYAATSR=1
                    do_DAYAVHRR=1
                    d0_DAYMYD=1
                    do_all=0
                    ;;
                NIT|NIGHT)
                    do_NITAATSR=1
                    do_NITAVHRR=1
                    do_NITMYD=1
                    do_all=0
                    ;;
                ALL)
                    do_all=1
                    ;;
            esac
            ;;
        *)
            echo "Error - Invalid option: $1"
            exit -1
            ;;
    esac
    shift
done

# determine new revision number by asking SVN for current number and adding 1
if [[ $revision -eq 0 ]]; then
    revstr=`svn info | grep 'Revision'`
    revind=$((`expr index "$revstr" '[0123456789]'`-1))
    if [[ $revind -lt 0 ]]; then
        echo 'Unable to determine version number from SVN.'
        revision=10000
    else
        revision=${revstr:$revind}
    fi
    if (( $new )); then
        let revision+=10000
    fi
fi

#------------------------------------------------------------------------------
# UPDATE LIBRARY PATH
#------------------------------------------------------------------------------
if (( $ld_set )); then
    # load library paths from Makefile lib file
    # 1) Read contents of lib file, whose location is given by $ORAC_LIB
    lib_contents=$(<${ORAC_LIB})
    # 2) Replace round braces with curly braces
    lib_commands=`echo "$lib_contents" | sed -e 's/(/\{/g' -e 's/)/\}/g'`
    # 3) Make a string that does everything before defining the variable $LIBS
    str='LIBS='
    not_libs="${lib_commands%%$str*}"
    # 4) Evaluate those lines
    eval "${lib_commands:0:${#not_libs}}"
    # 5) Now all of those variables are defined here so we can build the path
    LD_LIBRARY_PATH=${NCDFLIB}:${LD_LIBRARY_PATH}
    LD_LIBRARY_PATH=${NCDF_FORTRAN_LIB}:${LD_LIBRARY_PATH}
    LD_LIBRARY_PATH=${RTTOVLIB}:${LD_LIBRARY_PATH}
    LD_LIBRARY_PATH=${HDFLIB}:${LD_LIBRARY_PATH}
    LD_LIBRARY_PATH=${HDF5LIB}:${LD_LIBRARY_PATH}
    LD_LIBRARY_PATH=${GRIBLIB}:${LD_LIBRARY_PATH}
    LD_LIBRARY_PATH=${EPR_APILIB}:${LD_LIBRARY_PATH}
    LD_LIBRARY_PATH=${SZLIB}:${LD_LIBRARY_PATH}
    LD_LIBRARY_PATH=${EOSLIB}:${LD_LIBRARY_PATH}
    export LD_LIBRARY_PATH

    RTTOV_version=11.2
fi

#------------------------------------------------------------------------------
# DEFINE INPUT DATA INFORMATION
#------------------------------------------------------------------------------
i=0

#---- AQUA MODIS from LAADS (day) ----
if (( $do_all || $do_DAYMYD )); then
    sensor[$i]=MODIS
    platform[$i]=-AQUA
    label[$i]=DAYMYD
    path_to_l1b[$i]=$in_folder/MYD021KM.A2008172.0405.005.2009317014309.hdf
    # if you don't want to check against LAADSweb full files, use this instead
    #path_to_l1b[$i]=$in_folder/MYD021KM.A2008172.0405.005.2009317014309.bscs_000500531943.hdf
    path_to_geo[$i]=$in_folder/MYD03.A2008172.0405.005.2009316101940.hdf
    if (( $short )); then startx[$i]=700; else startx[$i]=0 ; fi
    endx[$i]=1299
    starty[$i]=1200
    endy[$i]=1204
    daynight[$i]=0

    frame[$i]=1
    let i+=1
fi

#---- AQUA MODIS from DWD (night) ----
if (( $do_all || $do_NITMYD )); then
    sensor[$i]=MODIS
    platform[$i]=-AQUA
    label[$i]=NITMYD
    path_to_l1b[$i]=$in_folder/MYD021KM.A2008172.1630.005.2009317021545.bscs_000500531943.hdf
    path_to_geo[$i]=$in_folder/MYD03.A2008172.1630.005.2009316104244.hdf
    if (( $short )); then startx[$i]=500; else startx[$i]=0 ; fi
    endx[$i]=1099
    starty[$i]=900
    endy[$i]=904
    daynight[$i]=0

    frame[$i]=1
    let i+=1
fi

#---- AATSR (day) ----
if (( $do_all || $do_DAYAATSR )); then
    sensor[$i]=AATSR
    platform[$i]=
    if (( $ver21 )); then
        label[$i]=DAYAATSRV21
        path_to_l1b[$i]=$in_folder/ATS_TOA_1PUUPA20080620_002337_000065272069_00345_32964_6203.N1
    else
        label[$i]=DAYAATSR
        path_to_l1b[$i]=$in_folder/ATS_TOA_1PRUPA20080620_002337_000065272069_00345_32964_0666.N1
    fi
    path_to_geo[$i]=${path_to_l1b[$i]}
    if (( $short )); then startx[$i]=1; else startx[$i]=0 ; fi
    endx[$i]=512
    starty[$i]=21366
    endy[$i]=21370
    daynight[$i]=1

    frame[$i]=4
    let i+=1
fi

#---- AATSR (night) ----
if (( $do_all || $do_NITAATSR )); then
    sensor[$i]=AATSR
    platform[$i]=
    if (( $ver21 )); then
        label[$i]=NITAATSRV21
        path_to_l1b[$i]=$in_folder/ATS_TOA_1PUUPA20080620_002337_000065272069_00345_32964_6203.N1
    else
        label[$i]=NITAATSR
        path_to_l1b[$i]=$in_folder/ATS_TOA_1PRUPA20080620_002337_000065272069_00345_32964_0666.N1
    fi
    path_to_geo[$i]=${path_to_l1b[$i]}
    if (( $short )); then startx[$i]=1; else startx[$i]=0 ; fi
    endx[$i]=512
    starty[$i]=37450
    endy[$i]=37454
    daynight[$i]=2

    frame[$i]=1
    let i+=1
fi

#---- AVHRR ----
if (( $do_all || $do_DAYAVHRR || ( (! $short) && $do_NITAVHRR) )); then
    sensor[$i]=AVHRR
    platform[$i]=-NOAA18
    path_to_l1b[$i]=$in_folder/noaa18_20080620_0050_99999_satproj_00000_13111_avhrr.h5
    path_to_geo[$i]=$in_folder/noaa18_20080620_0050_99999_satproj_00000_13111_sunsatangles.h5
    if (( $short )); then
        label[$i]=DAYAVHRR
        startx[$i]=1
    else
        label[$i]=AVHRR
        startx[$i]=0
    fi
    endx[$i]=409
    starty[$i]=5190
    endy[$i]=5194
    daynight[$i]=0

    frame[$i]=4
    let i+=1
fi

#---- AVHRR (short night segment) ----
if (( $short && ($do_all || $do_NITAVHRR) )); then
    sensor[$i]=AVHRR
    platform[$i]=-NOAA18
    label[$i]=NITAVHRR
    path_to_l1b[$i]=$in_folder/noaa18_20080620_0050_99999_satproj_00000_13111_avhrr.h5
    path_to_geo[$i]=$in_folder/noaa18_20080620_0050_99999_satproj_00000_13111_sunsatangles.h5
    startx[$i]=1
    endx[$i]=409
    starty[$i]=10150
    endy[$i]=10154
    daynight[$i]=0

    frame[$i]=1
    let i+=1
fi

#---- 2008/06/20 AATSR section with BL inversion cloud ----#
if (( $do_CLOUD )); then
    sensor[$i]=AATSR
    platform[$i]=
    label[$i]=CLOUD
    path_to_l1b[$i]=/home/minuit/imager/data/aatsr/2008/06/20/ATS_TOA_1PUUPA20080620_052525_000065272069_00348_32967_6206.N1
    path_to_geo[$i]=${path_to_l1b[$i]}
    startx[$i]=1
    endx[$i]=512
    starty[$i]=24000
    endy[$i]=26000
    daynight[$i]=1

    frame[$i]=1
    let i+=1
fi

# add distinction for short mode to labels
if (( $short )); then for i in ${!label[*]}; do
   label[i]=${label[i]}S
   frame[$i]=1
done
fi

# for only_compare, kill $sensor whilst leaving $label alone
if (( $skip_proc )); then
    unset sensor
fi

desc=''
if (( $drop)); then
    desc=D`echo $channels | sed -e 's/[ \t]//g'`
fi

if (( $sabotage)); then
    desc=T
fi

# Denote AATSR V2.1 files with at the end of the filename.
# THIS WILL MARK NON-AATSR FILES AS WELL. Haven't had better idea yet.
if (( $v21 )); then
    file_version='V2.1'
fi

ECMWF_version=1.90
SVN_version=`svn --version | grep 'svn, version'`
