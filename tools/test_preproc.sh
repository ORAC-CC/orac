#!/bin/bash
# test_preproc.sh
#
# A shell script to run the ORAC preprocessor on the test files and compare
# the results to the most recent previous version of the output. 
# The directories in the first section should be set to represent your local 
# file system.
#
# Calling sequence -
#    ./test_preproc.sh [-first] [-short|-long] [-no_compare] [-revision NUMBER]
#       [-do DAYAATSR|NITAATSR|AATSR|DAYAVHRR|NITAVHRR|AVHRR|DAYMYD|NITMYD|MYD|
#            MODIS|DAY|NIT|NIGHT|ALL|NONE] [-v21] [-badc 0|1|2]
#       [-verbose|-quiet] [-n_procs NUM] [-ld_set] [-orac_lib NAME]
#       [-idl_folder PATH] [-preproc_folder PATH] [-emiss_folder PATH]
#       [-tool_folder PATH] [-in_folder PATH] [-out_folder PATH] 
#       [-albedo_folder PATH] [-coeffs_folder PATH] [-emiss_atlas_folder PATH] 
#       [-calib_folder PATH]  [-ice_folder PATH] 
#       [-ggam_folder PATH -ggas_folder PATH -gpam_folder PATH|-e_folder PATH]
#
# Command line arguments -
# -do          Select which of the test cases to process. This argument may
#              be used multiple times to select multiple options. They are:
#    DAYAATSR  The daytime section of an AATSR orbit.
#    NITAATSR  The nighttime section of an AATSR orbit.
#    AATSR     DAYAATS and NITAATSR.
#    DAYAVHRR  The daytime section of an AVHRR orbit.
#    NITAVHRR  The nighttime section of an AVHRR orbit.
#    AVHRR     DAYAVHRR and NITAVHRR.
#    DAYMYD    The daytime section of an MODIS-AQUA orbit. For long processing,
#              only this option is valid.
#    NITMYD    The nighttime section of an MODIS-AQUA orbit. This only applies
#              for -short processing.
#    MYD|MODIS DAYMYD and NITMYD.
#    DAY       DAYAATSR, DAYAVHRR, DAYMYD.
#    NIT|NIGHT NITAATSR, NITAVHRR, NITMYD.
#    ALL       All of the above; the default option.
#    NONE      No processing is performed, only a comparison.
# -v21         Use version 2.1 AATSR data. Default is version 2.0.
# -short       Process only a small segment of each of the test files (5 lines).
#              This is significantly faster than the default processing of the
#              full files but may not catch all possible circumstances.
# -long        The opposite of -short and default behaviour.
# -no_compare  Do not compare the output files to a previous version after
#              processing.
# -first       By default, the script determines the revision number from svn
#              and then increments it by 1 as you are comparing altered code
#              to the previous commited revision. Set this argument to not
#              increment the counter (presumably because you are processing
#              unaltered code). Implies -no_compare.
# -revision    Force the code to use the given revision number. Implies
#              -first but not -no_compare.
# -verbose     Print maximal information to the log file and default behaviour.
# -quiet       The opposite of -verbose.
# -n_procs     The maximum number of simultaneous processes allowed. The default
#              is 5.
# -ecmwf       Value to be used for the ECMWF flag. 0 = A single GRIB file output
#              by the MARS server; 1 = Three NCDF files produced from BADC files;
#              2 = One NCDF file and two GRIB files from the BADC.
# -ld_set      Do not set the LD_LIBRARY_PATH variable.
# -orac_lib    Name of the library file for the ORAC preprocessor, from which
#              LD_LIBRARY_PATH will be generated.
# -idl_folder     The path of your IDL executable.
# -preproc_folder The path containing the ORAC preprocessor executable.
# -tool_folder    The path containing the ORAC tools (i.e. these scripts).
# -in_folder      The path containing the input data.
# -out_folder     The path where the outputs should be stored.
# -albedo_folder  The path containing albedo files (in folders by year).
# -coeffs_folder  The path containing channel information.
# -emiss_atlas_folder The path containing the emissivity atlas.
# -calib_folder   The path containing ATSR drift coefficients.
# -ggam_folder    The path containing NCDF ECMWF files named ggam.
# -ggas_folder    The path containing NCDF ECMWF files named ggas.
# -gpam_folder    The path containing NCDF ECMWF files named gpam.
# -ecmwf_folder   The path containing ECMWF files if using GRIB files.
# -ice_folder     The path containing ice/snow files.
# -emiss_folder   The path containing MODIS emissivity files.
#
# Known issues:
#   Currently, none of the arguments may contain spaces. This is only important
#   for the NCDF header variables, which should be text.
#
#   The "project" field from the file name has been requisitioned to 
#   distinguish the different test cases.
#
# History -
# 2013/11/01: AP Original version
# 2014/01/27: AP Altered formation of LD_LIBRARY_PATH to use library file.
#                Fixed bug in passing header arguments.
# 2014/02/04: AP Improved command line arguments.
# 2014/04/30: AP New folder structure. Minor bug fixes. Correctly assigned
#                NITMYD files. Updated AVHRR. Added -v21.
# 2014/07/01: AP Changed processing order as MODIS is faster.
#
set -e

#------------------------------------------------------------------------------
# DEFINE LOCAL FOLDERS
#------------------------------------------------------------------------------
# path to ORAC repository
orac_repos=/home/jupiter/eodg2/povey/orac/trunk

# path to ORAC data repository
data_repos=/local/home/povey/povey/data

# path to IDL executable
idl_folder=/usr/local/PACK/idl83/idl83/bin

#------------------------------------------------------------------------------
# DEFINE FILE HEADER
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

# this is the inverse of the actual lat/lon increment
dellon=1.38888889
dellat=1.38888889
#dellon=1.422222
#dellat=1.436783

# set flag to break ATSR files into smaller chunks
chunkproc=0

# set flag to use chunking when writing NCDF files
ncdf_chunk=0

# set flag to print maximal output
verbose=true

# set ECMWF EMOS library to print debugging output
#export JDCNDBG=1

#------------------------------------------------------------------------------
# DETERMINE NECESSARY FOLDERS
#------------------------------------------------------------------------------
# assume the hierachy of the ORAC repository
preproc_folder=$orac_repos/pre_processing/src
tool_folder=$orac_repos/tools
in_folder=$data_repos/testinput
out_folder=$data_repos/testoutput
# path to albedo files (in folders by year)
albedo_folder=$data_repos/albedo
# path to channel information
coeffs_folder=$data_repos/coeffs
# path to emissivity atlas
emiss_atlas_folder=$data_repos/emissivity
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
full_path=0

#------------------------------------------------------------------------------
# MANAGE SETTINGS
#------------------------------------------------------------------------------
# default settings
short=0
n_procs=5
do_all=1
do_DAYAATSR=0
do_NITAATSR=0
do_DAYAVHRR=0
do_NITAVHRR=0
do_DAYMYD=0
do_NITMYD=0
do_compare=1
new=1
revision=0
ver21=0
ld_set=1
badc_flag=1

# deal with command arguments
while [[ $# > 0 ]]; do
    case "$1" in
        -orac_lib)
            shift
            ORAC_LIB="$1"
            ;;
        -idl_folder)
            shift
            idl_folder="$1"
            ;;
        -preproc_folder)
            shift
            preproc_folder="$1"
            ;;
        -orac_folder)
            shift
            ;;
        -tool_folder)
            shift
            tool_folder="$1"
            ;;
        -in_folder)
            shift
            in_folder="$1"
            ;;
        -out_folder)
            shift
            out_folder="$1"
            ;;
        -albedo_folder)
            shift
            albedo_folder="$1"
            ;;
        -coeffs_folder)
            shift
            coeffs_folder="$1"
            ;;
        -emiss_atlas_folder)
            shift
            emiss_atlas_folder="$1"
            ;;
        -calib_folder)
            shift
            calib_folder="$1"
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
        -ecmwf_folder)
            shift
            ggam_folder="$1"
            badc_flag=0
            ;;
        -ice_folder)
            shift
            ice_folder="$1"
            ;;
        -emiss_folder)
            shift
            emiss_folder="$1"
            ;;
        -verbose)
            verbose=true
            ;;
        -quiet)
            verbose=false
            ;;
        -revision)
            shift
            revision="$1"
            ;;
        -first)
            do_compare=0
            new=0
            ;;
        -short)
            short=1
            ;;
        -long)
            short=0
            ;;
        -no_compare)
            do_compare=0
            ;;
        -n_procs)
            shift
            n_procs="$1"
            ;;
        -ld_set)
            ld_set=0
            ;;
        -v21)
            ver21=1
            ;;
        -ecmwf)
            shift
            badc_flag="$1"
            ;;
        -badc)
            shift
            badc_flag="$1"
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
                    ;;
                NONE)
                    do_all=0
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

if (( $ld_set )); then
    # load library paths from Makefile lib file
    # 1) Read contents of lib file, whose location is given by $ORAC_LIB
    lib_contents=$(<$preproc_folder/$ORAC_LIB)
    # 2) Replace round braces with curly braces
    lib_commands=`echo "$lib_contents" | sed -e 's/(/\{/g' -e 's/)/\}/g'`
    # 3) Make a string that does everything before defining the variable $LIBS
    str='LIBS='
    not_libs="${lib_commands%%$str*}"
    # 4) Evaluate those lines
    eval "${lib_commands:0:${#not_libs}}"
    # 5) Now all of those variables are defined here so we can build the path
    LD_LIBRARY_PATH=$NCDFLIB:$LD_LIBRARY_PATH
    LD_LIBRARY_PATH=$NCDF_FORTRAN_LIB:$LD_LIBRARY_PATH
    LD_LIBRARY_PATH=$RTTOVLIB:$LD_LIBRARY_PATH
    LD_LIBRARY_PATH=$HDFLIB:$LD_LIBRARY_PATH
    LD_LIBRARY_PATH=$HDF5LIB:$LD_LIBRARY_PATH
    LD_LIBRARY_PATH=$GRIBLIB:$LD_LIBRARY_PATH
    LD_LIBRARY_PATH=$EPR_APILIB:$LD_LIBRARY_PATH
    LD_LIBRARY_PATH=$SZLIB:$LD_LIBRARY_PATH
    LD_LIBRARY_PATH=$EOSLIB:$LD_LIBRARY_PATH
    export LD_LIBRARY_PATH
fi

#------------------------------------------------------------------------------
# DEFINE INPUT DATA INFORMATION
#------------------------------------------------------------------------------
i=0

#---- AQUA MODIS from LAADS (day) ----
if (( $do_all || $do_DAYMYD )); then
    sensor[$i]=MODIS-AQUA
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
    let i+=1
fi

#---- AQUA MODIS from DWD (night) ----
if (( $do_all || $do_NITMYD )); then
    sensor[$i]=MODIS-AQUA
    label[$i]=NITMYD
    path_to_l1b[$i]=$in_folder/MYD021KM.A2008172.1630.005.2009317021545.bscs_000500531943.hdf
    path_to_geo[$i]=$in_folder/MYD03.A2008172.1630.005.2009316104244.hdf
    if (( $short )); then startx[$i]=500; else startx[$i]=0 ; fi
    endx[$i]=1099
    starty[$i]=900
    endy[$i]=904
    daynight[$i]=0
    let i+=1
fi

#---- AATSR (day) ----
if (( $do_all || $do_DAYAATSR )); then
    sensor[$i]=AATSR
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
    let i+=1
fi

#---- AATSR (night) ----
if (( $do_all || $do_NITAATSR )); then
    sensor[$i]=AATSR
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
    let i+=1
fi

#---- AVHRR ----
if (( $do_all || $do_DAYAVHRR || ( (! $short) && $do_NITAVHRR) )); then
    sensor[$i]=AVHRR-NOAA18
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
    let i+=1
fi

#---- AVHRR (short night segment) ----
if (( $short && ($do_all || $do_NITAVHRR) )); then
    sensor[$i]=AVHRR-NOAA18
    label[$i]=NITAVHRR
    path_to_l1b[$i]=$in_folder/noaa18_20080620_0050_99999_satproj_00000_13111_avhrr.h5
    path_to_geo[$i]=$in_folder/noaa18_20080620_0050_99999_satproj_00000_13111_sunsatangles.h5
    startx[$i]=1
    endx[$i]=409
    starty[$i]=10150
    endy[$i]=10154
    daynight[$i]=0
    let i+=1
fi

# add distinction for short mode to labels
if (( $short )); then for j in ${!label[*]}; do
   label[j]=${label[j]}S
done
fi

#------------------------------------------------------------------------------
# RUN ORAC PREPROCESSOR
#------------------------------------------------------------------------------
#get uid and time information
#uuid_tag=`exec uuidgen -t`
uuid_tag=0
exec_time=`exec date +%Y%m%d%H%M%S`

#start the preprocessing and pass the variables to the binary on the command line
sec=`date +"%s"`
com=()
for j in ${!sensor[*]}; do
   echo "Processing ${label[$j]}"

   folder=$out_folder/'V'$revision/${label[$j]}
   if [ ! -d $folder ]; then mkdir -p $folder; fi
   log_file=$folder/${label[$j]}'_PREPROC_V'$revision'_'`date +"%y%m%d_%H%M"`.log
   echo 'UUID' $uuid_tag 1> $log_file
   echo '' 1>> $log_file
   echo "LD_LIBRARY_PATH=${LD_LIBRARY_PATH}" 1>> $log_file
   echo '' 1>> $log_file
   echo 'Do this:' 1>> $log_file

   # command line arguments change every so often
   if [[ $revision -ge 2208 ]]; then
       arg=( "${sensor[$j]}" "${path_to_l1b[$j]}" "${path_to_geo[$j]}" "${ggam_folder}" "${coeffs_folder}" "${emiss_atlas_folder}" "${ice_folder}" "${albedo_folder}" "${emiss_folder}" "${dellon}" "${dellat}" "${folder}" "${startx[$j]}" "${endx[$j]}" "${starty[$j]}" "${endy[$j]}" "${ncdf_version}" "${cf_convention}" "${processing_inst}" "${l2processor}" "${revision}" "${contact_email}" "${contact_website}" "${file_version}" "${reference}" "${hist}" "${summary}" "${keywords}" "${comment}" "${label[$j]}" "${license}" "${uuid_tag}" "${exec_time}" "${calib_folder}" "${badc_flag}" "${ggas_folder}" "${gpam_folder}" "${chunkproc}" "${daynight}" "${verbose}" "${ncdf_chunk}" "${full_path}" )
   elif [[ $revision -ge 2133 ]]; then
       arg=( "${sensor[$j]}" "${path_to_l1b[$j]}" "${path_to_geo[$j]}" "${ggam_folder}" "${coeffs_folder}" "${emiss_atlas_folder}" "${ice_folder}" "${albedo_folder}" "${emiss_folder}" "${gridflag}" "${dellon}" "${dellat}" "${folder}" "${startx[$j]}" "${endx[$j]}" "${starty[$j]}" "${endy[$j]}" "${ncdf_version}" "${cf_convention}" "${processing_inst}" "${l2processor}" "${revision}" "${contact_email}" "${contact_website}" "${file_version}" "${reference}" "${hist}" "${summary}" "${keywords}" "${comment}" "${label[$j]}" "${license}" "${uuid_tag}" "${exec_time}" "${calib_folder}" "${badc_flag}" "${ggas_folder}" "${gpam_folder}" "${chunkproc}" "${daynight}" "${verbose}" "${ncdf_chunk}" "${full_path}" )
   elif [[ $revision -ge 1958 ]]; then
       arg=( "${sensor[$j]}" "${path_to_l1b[$j]}" "${path_to_geo[$j]}" "${ggam_folder}" "${coeffs_folder}" "${emiss_atlas_folder}" "${ice_folder}" "${albedo_folder}" "${emiss_folder}" "${gridflag}" "${dellon}" "${dellat}" "${folder}" "${startx[$j]}" "${endx[$j]}" "${starty[$j]}" "${endy[$j]}" "${ncdf_version}" "${cf_convention}" "${processing_inst}" "${l2processor}" "${revision}" "${contact_email}" "${contact_website}" "${file_version}" "${reference}" "${hist}" "${summary}" "${keywords}" "${comment}" "${label[$j]}" "${license}" "${uuid_tag}" "${exec_time}" "${calib_folder}" "${badc_flag}" "${ggas_folder}" "${gpam_folder}" "${chunkproc}" "${daynight}" "${verbose}" "${ncdf_chunk}" )
   elif [[ $revision -ge 1654 ]]; then
       arg=( "${sensor[$j]}" "${path_to_l1b[$j]}" "${path_to_geo[$j]}" "${ggam_folder}" "${coeffs_folder}" "${emiss_atlas_folder}" "${ice_folder}" "${albedo_folder}" "${emiss_folder}" "${gridflag}" "${dellon}" "${dellat}" "${folder}" "${startx[$j]}" "${endx[$j]}" "${starty[$j]}" "${endy[$j]}" "${ncdf_version}" "${cf_convention}" "${processing_inst}" "${l2processor}" "${revision}" "${contact_email}" "${contact_website}" "${file_version}" "${reference}" "${hist}" "${summary}" "${keywords}" "${comment}" "${label[$j]}" "${license}" "${uuid_tag}" "${exec_time}" "${calib_folder}" "${badc_flag}" "${ggas_folder}" "${gpam_folder}" "${chunkproc}" "${daynight}" "${verbose}" )
   else
       arg=( "${sensor[$j]}" "${path_to_l1b[$j]}" "${path_to_geo[$j]}" "${ggam_folder}" "${coeffs_folder}" "${emiss_atlas_folder}" "${ice_folder}" "${albedo_folder}" "${emiss_folder}" "${gridflag}" "${dellon}" "${dellat}" "${folder}" 0 "${startx[$j]}" "${endx[$j]}" "${starty[$j]}" "${endy[$j]}" "${ncdf_version}" "${cf_convention}" "${processing_inst}" "${l2processor}" "${revision}" "${contact_email}" "${contact_website}" "${file_version}" "${reference}" "${hist}" "${summary}" "${keywords}" "${comment}" "${label[$j]}" "${license}" "${uuid_tag}" "${exec_time}" "${calib_folder}" "${badc_flag}" "${ggas_folder}" "${gpam_folder}" "${chunkproc}" "${daynight}" )
   fi

  echo $preproc_folder/orac_preproc.x "${arg[@]}" 1>> $log_file
  echo '' 1>> $log_file

#  Non-parallel version:
#  sec=`date +"%s"`
#  $preproc_folder/orac_preproc.x "${arg[@]}" >> $log_file 2>&1
#  if (( $? != 0 )); then
#      echo "${label[$j]}: Error."
#      exit -2
#  fi
#  echo 'Processing took '$((`date +"%s"`-$sec))' s'

  # parallel version
  com="$com$preproc_folder/orac_preproc.x ${arg[@]} >> $log_file 2>&1 && "\
"echo 'Processed ${label[$j]}'"$'\n'
# the second line prints 'Processed' on completition
done 
echo "$com" | xargs -P $n_procs -n 1 -I COMMAND sh -c COMMAND
echo 'Processing took '$((`date +"%s"`-$sec))' s'

#------------------------------------------------------------------------------
# CHECK RESULTS
#------------------------------------------------------------------------------
# call IDL routine to compare this output to the previous version
if (( $do_compare && ("$?" == 0) )); then
    $idl_folder/idl -rt=$tool_folder/compare_orac_out.sav -args $out_folder \
        $revision 'preproc' ${#label[@]} ${label[@]}
fi