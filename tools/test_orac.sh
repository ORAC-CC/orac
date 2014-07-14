#!/bin/bash
# Name:
#    test_orac.sh
#
# Purpose:
# A shell script to run the ORAC processor on the test files and compare
# the results to a previous version. The directories in the first section
# should be set to represent your local file system.
#
# Calling sequence:
#    ./test_orac.sh [-first] [-short|-long] [-no_compare] [-revision NUMBER]
#       [-do DAYAATSR|NITAATSR|AATSR|DAYAVHRR|NITAVHRR|AVHRR|DAYMYD|NITMYD|MYD|
#            MODIS|DAY|NIT|NIGHT|ALL|NONE] [-v21] [-thresh NUMBER] [-drop]
#       [-WAT|ICE] [-n_procs NUM] [-ld_set] [-orac_lib NAME]
#       [-idl_folder PATH] [-orac_folder PATH] [-preproc_folder PATH] 
#       [-tool_folder PATH] [-in_folder PATH] [-out_folder PATH] 
#       [-albedo_folder PATH] [-coeffs_folder PATH] [-emiss_atlas_folder PATH] 
#       [-calib_folder PATH]  [-ice_folder PATH] [-emiss_folder PATH]
#       [-ggam_folder PATH -ggas_folder PATH -gpam_folder PATH|-e_folder PATH]
#       [-only_compare]
#
# Arguments:
# -do          Select which of the test cases to process. This argument may
#              be used multiple times to select multiple options. They are:
#    DAYAATSR  The daytime section of an AATSR orbit.
#    NITAATSR  The nighttime section of an AATSR orbit.
#    AATSR     DAYAATS and NITAATSR.
#    DAYAVHRR  The daytime section of an AVHRR orbit.
#    NITAVHRR  The nighttime section of an AVHRR orbit.
#    AVHRR     DAYAVHRR and NITAVHRR.
#    DAYMYD    The daytime section of an MODIS-AQUA orbit.
#    NITMYD    The nighttime section of an MODIS-AQUA orbit.
#    MYD|MODIS DAYMYD and NITMYD.
#    DAY       DAYAATSR, DAYAVHRR, DAYMYD.
#    NIT|NIGHT NITAATSR, NITAVHRR, NITMYD.
#    ALL       DAYAATS, NITAATSR, AVHRR, DAYMYD, and NITMYD. The default option.
#    NONE      No processing is performed, only a comparison.
# -drop        Don't use the second and fifth channels. Mostly intended for
#              bug identification rather than useful processing.
# -first       By default, the script determines the revision number from svn
#              and then increments it by 1 as you are comparing altered code
#              to the previous commited revision. Set this argument to not
#              increment the counter (presumably because you are processing
#              unaltered code). Implies -no_compare.
# -ICE         Use the ice-cloud look-up tables. This is exclusive of -WAT.
# -ld_set      Do not set the LD_LIBRARY_PATH variable.
# -long        The opposite of -short and default behaviour.
# -n_procs     The maximum number of simultaneous processes allowed. The default
#              is 5.
# -no_compare  Do not compare the output files to a previous version after
#              processing.
# -only_compare Do not run processor. Only run the regression test.
# -orac_lib    Name of the library file for the ORAC preprocessor, from which
#              LD_LIBRARY_PATH will be generated.
# -revision    Force the code to use the given revision number. Implies
#              -first but not -no_compare.
# -short       Process only a small segment of each of the test files (5 lines).
#              This is significantly faster than the default processing of the
#              full files but may not catch all possible circumstances.
# -thresh      The threshold to be used in the rounding error regression test.
# -v21         Use version 2.1 AATSR data. Default is version 2.0.
# -WAT         Use the water-cloud look-up tables. The default option.
# -idl_folder     The path of your IDL executable.
# -orac_folder    The path containing the ORAC executable.
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
#   The file search string doesn't account for chunked ATSR processing. If you
#   wish to chunk ATSR, an additional argument (e.g. chunk) would be required
#   to specify chunk number.
#
# History :
# 2004/01/24: AP Original version
# 2014/01/28: AP Improved command line arguments.
# 2014/02/04: AP Expanded print statements - now displays # converged and 
#                average cost. Added xargs parallelisation.
# 2014/04/30: AP New folder structure. Minor bug fixes. Added -drop.
# 2014/07/01: AP Changed processing order as MODIS is faster. Removed
#                parallelisation as ORAC already parallel.
# 2014/07/14: AP Added THRESH and ONLY_COMPARE arguments.
#
set -e

#------------------------------------------------------------------------------
# DEFINE LOCAL FOLDERS
#------------------------------------------------------------------------------
# path to ORAC repository
orac_repos=/home/jupiter/eodg2/povey/orac/trunk

# path to ORAC data repository
data_repos=/local/home/povey/povey/data

# path to SAD repository
sad_repos=/local/home/povey/povey/sad_dir

# path to IDL executable
idl_folder=/usr/local/PACK/idl83/idl83/bin

#------------------------------------------------------------------------------
# DETERMINE NECESSARY FOLDERS
#------------------------------------------------------------------------------
# assume the hierachy of the ORAC repository
orac_folder=$orac_repos/src
preproc_folder=$orac_repos/pre_processing/src
tool_folder=$orac_repos/tools
in_folder=$data_repos/testoutput
out_folder=$data_repos/testoutput

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
do_compare=1
skip_proc=0
new=1
revision=0
ver21=0
ld_set=1
drop=0

# perl command used at end of script. specifies which lines of log file
# are printed
perl='print if ($_=~"No. of retrievals converged" or $_=~"Avge cost per conv")'

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
        -orac_folder)
            shift
            orac_folder="$1"
            ;;
        -preproc_folder)
            shift
            preproc_folder="$1"
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
        -only_compare)
            skip_proc=1
            ;;
        -n_procs)
            shift
            n_procs="$1"
            ;;
        -ld_set)
            ld_set=0
            ;;
        -WAT)
            ;;
        -ICE)
            phase="ICE"
            ;;
        -v21)
            ver21=1
            ;;
        -thresh)
            shift
            thresh="$1"
            ;;
        -drop)
            drop=1
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
    lib_contents=$(<${preproc_folder}/${ORAC_LIB})
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
fi

#------------------------------------------------------------------------------
# DEFINE INPUT DATA INFORMATION
#------------------------------------------------------------------------------
i=0

if (( $do_all || $do_DAYMYD )); then
    sensor[$i]=MODIS-AQUA
    label[$i]=DAYMYD
    let i+=1
fi
if (( $do_all || $do_NITMYD )); then
    sensor[$i]=MODIS-AQUA
    label[$i]=NITMYD
    let i+=1
fi
if (( $do_all || $do_DAYAATSR )); then
    sensor[$i]=AATSR
    if (( $ver21 )); then
        label[$i]=DAYAATSRV21
    else
        label[$i]=DAYAATSR
    fi
    let i+=1
fi
if (( $do_all || $do_NITAATSR )); then
    sensor[$i]=AATSR
    if (( $ver21 )); then
        label[$i]=NITAATSRV21
    else
        label[$i]=NITAATSR
    fi
    let i+=1
fi
if (( $do_all || $do_DAYAVHRR || ( (! $short) && $do_NITAVHRR) )); then
    sensor[$i]=AVHRR-NOAA18
    if (( $short )); then
        label[$i]=DAYAVHRR
    else
        label[$i]=AVHRR
    fi
    let i+=1
fi
if (( $short && ($do_all || $do_NITAVHRR) )); then
    sensor[$i]=AVHRR-NOAA18
    label[$i]=NITAVHRR
    let i+=1
fi

# add distinction for short mode to labels
if (( $short )); then for i in ${!label[*]}; do
   label[i]=${label[i]}S
done
fi

# set which channels should be used by the retrieval
if (( $drop )); then
    channels='1 0 1 1 0 1'
else
    channels='1 1 1 1 1 1'
fi

# for only_compare, kill $sensor whilst leaving $label alone
if (( $skip_proc )); then
    unset sensor
fi

#------------------------------------------------------------------------------
# RUN ORAC PROCESSOR
#------------------------------------------------------------------------------
# save driver file to output folder
driver_file_base=$out_folder/test_driver_
sec=`date +"%s"`
com=()
for j in ${!sensor[*]}; do
   echo "Processing ${label[$j]}"

   # locate output folder
   folder=$out_folder/'V'$revision/${label[$j]}
   if [ ! -d $folder ]; then 
       echo 'Preprocessor output folder does not exist for ${label[$j]}.'
       continue
   fi
   if (( $drop )); then
       output_folder=${folder}D
       if [ ! -d $output_folder ]; then mkdir $output_folder; fi
   else
       output_folder=$folder
   fi

   # find root file names
   unset files i
   while IFS= read -r -d $'\0' tmp; do
       files[i++]="$tmp"
   done < <(find $folder -name "${label[$j]}_*ORACV${revision}*alb.nc" \
       -printf "%f\0")
   if (( "${#files}" == 0 )); then
       echo 'No files found. Check revision number.'
       continue
   fi

   # loop over files found
   for alb in ${files[*]}; do
       fileroot=${alb:0:$((${#alb}-7))}

       # write driver file
       driver_file=$driver_file_base${label[$j]}.txt
       driver="'$folder'
'$fileroot'
'$output_folder'
'$sad_repos'
${sensor[$j]}
6
$channels
$phase"
       echo "$driver" 1> $driver_file

       # make header for log file
       log_file=$output_folder/${label[$j]}
       if (( $drop )); then log_file=$log_file'D'; fi
       log_file=$log_file'_ORAC_'$phase'_V'$revision'_'`date +"%y%m%d_%H%M"`.log
       echo "LD_LIBRARY_PATH=${LD_LIBRARY_PATH}" 1>> $log_file
       echo '' 1>> $log_file
       echo '-----DRIVER FILE-----' 1>> $log_file
       echo "$driver" 1>> $log_file
       echo '---------------------' 1>> $log_file
       echo '' 1>> $log_file
       echo '-----Do this-----' 1>> $log_file
       echo "${orac_folder}/orac" $driver_file 1>> $log_file
       echo '-----------------' 1>> $log_file
       echo '' 1>> $log_file
       
       #  Non-parallel version:
       $orac_folder/orac $driver_file >> $log_file 2>&1
       if (( $? != 0 )); then
           echo "${label[$j]}: Error."
           exit
       fi
       echo "Processed ${label[$j]}"
       perl -ne "$perl" $log_file

       # parallel version
#       com="$com$orac_folder/orac $driver_file >> $log_file 2>&1 && "\
#"echo 'Processed ${label[$j]}' && perl -ne '$perl' $log_file"$'\n'
# the second line prints 'Processed' and convergence data on completition
   done
done
# parallelize the commands
#echo "$com" | xargs -0 -P $n_procs -n 1 -I COMMAND sh -c COMMAND
echo 'Processing took '$((`date +"%s"`-$sec))' s'

rm -f $driver_file_base*
   
#------------------------------------------------------------------------------
# CHECK RESULTS
#------------------------------------------------------------------------------
# call IDL routine to compare this output to the previous version
if (( $do_compare && ("$?" == 0) )); then
    $idl_folder/idl -rt=$tool_folder/compare_orac_out.sav -args $out_folder \
        $revision 'main' ${#label[@]} ${label[@]} $thresh
fi