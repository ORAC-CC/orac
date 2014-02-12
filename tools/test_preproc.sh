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
#       [-do DAYAATSR|NITAATSR|AATSR|AVHRR|DAYMYD|NITMYD|MYD|MODIS|ALL|NONE]
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
#    AVHRR     An entire AVHRR orbit.
#    DAYMYD    The daytime section of an MODIS-AQUA orbit.
#    NITMYD    The nighttime section of an MODIS-AQUA orbit.
#    MYD|MODIS DAYMYD and NITMYD.
#    ALL       DAYAATS, NITAATSR, AVHRR, DAYMYD, and NITMYD. The default option.
#    NONE      No processing is performed, only a comparison.
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
idl_folder=/usr/local/PACK/idl82/idl82/bin

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

# 1:ecmwf grid, 2:L3 grid, 3: own definition
gridflag=1
# this is the inverse of the actual lat/lon increment
dellon=2.0
dellat=2.0

# set flag to break ATSR files into smaller chunks
chunkproc=0

# set flag to use chunking when writing NCDF files
ncdf_chunk=0

# set flag to print maximal output
verbose=true

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
calib_folder=$data_repos/AATSR_VIS_DRIFT_V02-05.DAT
# path where ECMWF files are located (one argument per extension)
ggam_folder=$data_repos/ecmwf
ggas_folder=$data_repos/ecmwf
gpam_folder=$data_repos/ecmwf
# path where ice/snow files are located
ice_folder=$data_repos/ice_snow
# path where modis emissivity files are located
emiss_folder=$data_repos/emissivity

#------------------------------------------------------------------------------
# MANAGE SETTINGS
#------------------------------------------------------------------------------
# set flag if using badc NCDF files
badc_flag=true

# determine new revision number by asking SVN for current number and adding 1
revstr=`svn info | grep 'Revision'`
revind=$((`expr index "$revstr" '[0123456789]'`-1))
if [[ "$revind" -lt 0 ]]; then
    echo 'Unable to determine version number from SVN.'
    revision=10000
else
    revision=$((${revstr:$revind}+1))
fi

# default settings
short=0
n_procs=5
do_all=1
do_DAYAATSR=0
do_NITAATSR=0
do_AVHRR=0
do_DAYMYD=0
do_NITMYD=0
do_compare=1
rev_set=0
ld_set=1

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
            badc_flag=true
            ;;
        -ggas_folder)
            shift
            ggas_folder="$1"
            badc_flag=true
            ;;
        -gpam_folder)
            shift
            gpam_folder="$1"
            badc_flag=true
            ;;
        -ecmwf_folder)
            shift
            ggam_folder="$1"
            badc_flag=false
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
            rev_set=1
            ;;
        -first)
            do_compare=0
            if (( ! $rev_set )); then let revision-=1; fi
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
                AVHRR)
                    do_AVHRR=1
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

if (( $ld_set )); then
    # load library paths from Makefile lib file
    # 1) Read contents of lib file, whose location is given by $ORAC_LIB
    lib_contents=$(<${preproc_folder}/${ORAC_LIB})
    # 2) Replace round braces with curly braces
    lib_commands=`echo "$lib_contents" | sed -e 's/(/\{/g' -e 's/)/\}/g'`
    # 3) Make a string that does everything before defining the variable $LIBS
    str='LIBS='
    not_libs="${lib_commands%%$str*}"
    # 4) Evaluate those lines
    eval "${lib_commands:0:${#not_libs}}"
    # 5) Now all of those variables are defined here so we can build the path
    LD_LIBRARY_PATH=${EPR_APILIB}:${LD_LIBRARY_PATH}
    LD_LIBRARY_PATH=${GRIBLIB}:${LD_LIBRARY_PATH}
    LD_LIBRARY_PATH=${HDFLIB}:${LD_LIBRARY_PATH}
    LD_LIBRARY_PATH=${HDF5LIB}:${LD_LIBRARY_PATH}
    LD_LIBRARY_PATH=${NCDFLIB}:${LD_LIBRARY_PATH}
    LD_LIBRARY_PATH=${NCDF_FORTRAN_LIB}:${LD_LIBRARY_PATH}
    LD_LIBRARY_PATH=${SZLIB}:${LD_LIBRARY_PATH}
    export LD_LIBRARY_PATH
fi

#------------------------------------------------------------------------------
# DEFINE INPUT DATA INFORMATION
#------------------------------------------------------------------------------
i=0

#---- AATSR (day) ----
if (( ($do_all) || ($do_DAYAATSR) )); then
    sensor[$i]=AATSR
    label[$i]=DAYAATSR
    path_to_l1b[$i]=$in_folder/ATS_TOA_1PRUPA20080620_002337_000065272069_00345_32964_0666.N1
    path_to_geo[$i]=${path_to_l1b[$i]}
    if (( $short == 1 )); then startx[$i]=1; else startx[$i]=0 ; fi
    endx[$i]=512
    starty[$i]=21366
    endy[$i]=21370
    daynight[$i]=1
    let i+=1
fi
#---- AATSR (night) ----
if (( ($do_all) || ($do_NITAATSR) )); then
    sensor[$i]=AATSR
    label[$i]=NITAATSR
    path_to_l1b[$i]=$in_folder/ATS_TOA_1PRUPA20080620_002337_000065272069_00345_32964_0666.N1
    path_to_geo[$i]=${path_to_l1b[0]}
    if (( $short == 1 )); then startx[$i]=1; else startx[$i]=0 ; fi
    endx[$i]=512
    starty[$i]=37450
    endy[$i]=37454
    daynight[$i]=2
    let i+=1
fi

#---- AVHRR ----
if (( ($do_all) || ($do_AVHRR) )); then
    sensor[$i]=AVHRR
    label[$i]=AVHRR
    path_to_l1b[$i]=$in_folder/noaa18_20080620_0050_99999_satproj_00000_13111_avhrr.h5
    path_to_geo[$i]=$in_folder/noaa18_20080620_0050_99999_satproj_00000_13111_sunsatangles.h5
    if (( $short == 1 )); then startx[$i]=1; else startx[$i]=0 ; fi
    endx[$i]=409
    starty[$i]=10150
    endy[$i]=10154
    daynight[$i]=0
    let i+=1
fi

#---- AQUA MODIS from LAADS (day) ----
if (( ($do_all) || ($do_DAYMYD) )); then
    sensor[$i]=MODIS
    label[$i]=DAYMYD
    path_to_l1b[$i]=$in_folder/MYD021KM.A2008172.0405.005.2009317014309.hdf
    # if you don't want to check against LAADSweb full files, use this instead
    #path_to_l1b[$i]=$in_folder/MYD021KM.A2008172.0405.005.2009317014309.bscs_000500531943.hdf
    path_to_geo[$i]=$in_folder/MYD03.A2008172.0405.005.2009316101940.hdf
    if (( $short == 1 )); then startx[$i]=700; else startx[$i]=0 ; fi
    endx[$i]=1299
    starty[$i]=1200
    endy[$i]=1204
    daynight[$i]=0
    let i+=1
fi

#---- AQUA MODIS from DWD (night) ----
if (( ($do_all) || ($do_NITMYD) )); then
    sensor[$i]=MODIS
    label[$i]=NITMYD
    path_to_l1b[$i]=$in_folder/MYD021KM.A2008172.0405.005.2009317014309.bscs_000500531943.hdf
    path_to_geo[$i]=$in_folder/MYD03.A2008172.0405.005.2009316101940.hdf
    if (( $short == 1 )); then startx[$i]=500; else startx[$i]=0 ; fi
    endx[$i]=1099
    starty[$i]=900
    endy[$i]=904
    daynight[$i]=0
    let i+=1
fi

# add distinction for short mode to labels
if (( $short == 1 )); then for j in ${!label[*]}; do
   label[j]=${label[j]}S
done
fi

#------------------------------------------------------------------------------
# RUN ORAC PREPROCESSOR
#------------------------------------------------------------------------------
#get uid and time information
uuid_tag=`exec uuidgen -t`
exec_time=`exec date +%Y%m%d%H%M%S`

#start the preprocessing and pass the variables to the binary on the command line
sec=`date +"%s"`
com=()
for j in ${!sensor[*]}; do
   echo "Processing ${label[$j]}"

   log_file=${out_folder}/${label[$j]}'_V'$revision'_'`date +"%y%m%d_%H%M"`.log
   echo 'UUID' $uuid_tag 1> $log_file
   echo '' 1>> $log_file
   echo "LD_LIBRARY_PATH=${LD_LIBRARY_PATH}" 1>> $log_file
   echo '' 1>> $log_file
   echo 'Do this:' 1>> $log_file

   # command line arguments change every so often
   if [[ "$revision" -ge 1958 ]]; then
       arg=( "${sensor[$j]}" "${path_to_l1b[$j]}" "${path_to_geo[$j]}" "${ggam_folder}" "${coeffs_folder}" "${emiss_atlas_folder}" "${ice_folder}" "${albedo_folder}" "${emiss_folder}" "${gridflag}" "${dellon}" "${dellat}" "${out_folder}" "${startx[$j]}" "${endx[$j]}" "${starty[$j]}" "${endy[$j]}" "${ncdf_version}" "${cf_convention}" "${processing_inst}" "${l2processor}" "${revision}" "${contact_email}" "${contact_website}" "${file_version}" "${reference}" "${hist}" "${summary}" "${keywords}" "${comment}" "${label[$j]}" "${license}" "${uuid_tag}" "${exec_time}" "${calib_folder}" "${badc_flag}" "${ggas_folder}" "${gpam_folder}" "${chunkproc}" "${daynight}" "${verbose}" "${ncdf_chunk}" )
   elif [[ "$revision" -ge 1654 ]]; then
       arg=( "${sensor[$j]}" "${path_to_l1b[$j]}" "${path_to_geo[$j]}" "${ggam_folder}" "${coeffs_folder}" "${emiss_atlas_folder}" "${ice_folder}" "${albedo_folder}" "${emiss_folder}" "${gridflag}" "${dellon}" "${dellat}" "${out_folder}" "${startx[$j]}" "${endx[$j]}" "${starty[$j]}" "${endy[$j]}" "${ncdf_version}" "${cf_convention}" "${processing_inst}" "${l2processor}" "${revision}" "${contact_email}" "${contact_website}" "${file_version}" "${reference}" "${hist}" "${summary}" "${keywords}" "${comment}" "${label[$j]}" "${license}" "${uuid_tag}" "${exec_time}" "${calib_folder}" "${badc_flag}" "${ggas_folder}" "${gpam_folder}" "${chunkproc}" "${daynight}" "${verbose}" )
   else
       arg=( "${sensor[$j]}" "${path_to_l1b[$j]}" "${path_to_geo[$j]}" "${ggam_folder}" "${coeffs_folder}" "${emiss_atlas_folder}" "${ice_folder}" "${albedo_folder}" "${emiss_folder}" "${gridflag}" "${dellon}" "${dellat}" "${out_folder}" 0 "${startx[$j]}" "${endx[$j]}" "${starty[$j]}" "${endy[$j]}" "${ncdf_version}" "${cf_convention}" "${processing_inst}" "${l2processor}" "${revision}" "${contact_email}" "${contact_website}" "${file_version}" "${reference}" "${hist}" "${summary}" "${keywords}" "${comment}" "${label[$j]}" "${license}" "${uuid_tag}" "${exec_time}" "${calib_folder}" "${badc_flag}" "${ggas_folder}" "${gpam_folder}" "${chunkproc}" "${daynight}" )
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
if (( ($do_compare) && ("$?" == 0) )); then
    $idl_folder/idl -rt=$tool_folder/compare_orac_out.sav -args $out_folder $revision 'preproc'
fi