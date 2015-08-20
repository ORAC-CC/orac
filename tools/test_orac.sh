#!/bin/bash
# Name:
#    test_orac.sh
#
# Purpose:
#    A shell script to run the ORAC processor on the test files and compare
#    the results to a previous version.
#
# Calling sequence:
#    ./test_orac.sh [-do DAYAATSR|NITAATSR|AATSR|DAYAVHRR|NITAVHRR|AVHRR|DAYMYD|
#       NITMYD|MYD|MODIS|DAY|NIT|NIGHT|ALL] [-drop] [-first] [-ICE]
#       [-idl_folder PATH] [-ld_set] [-in_folder PATH] [-n_procs NUM]
#       [-no_compare|only_compare] [-out_folder PATH] [-orac_folder PATH]
#       [-orac_lib NAME] [-preproc_folder PATH] [-revision NUMBER]  [-short]
#       [-thresh NUM] [-tool_folder PATH] [-v21]
#
# Arguments:
#   See header.sh for full details of arguments.
#   NOTE: For this script, -in_folder refers to the location of the preprocessed
#   files. If you wish the main processed files to be in yet another folder, set
#   this keyword for this script but not test_preproc.sh.
#
# Known issues:
#   None know.
#
# History:
# 2014/01/24, AP: Original version
# 2014/01/28, AP: Improved command line arguments.
# 2014/02/04, AP: Expanded print statements - now displays # converged and
#    average cost. Added xargs parallelisation.
# 2014/04/30, AP: New folder structure. Minor bug fixes. Added -drop.
# 2014/07/01, AP: Changed processing order as MODIS is faster. Removed
#    parallelisation as ORAC already parallel.
# 2014/07/14, AP: Added THRESH and ONLY_COMPARE arguments.
# 2014/07/28, AP: Moved arguments into header script.
# 2015/01/06, AP: Ammending how -drop and -v21 are managed.
#
set -e

source header.sh


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
        echo "Preprocessor output folder does not exist for ${label[$j]}."
        continue
    fi
    # If dropping channels, put output in separate folder.
    output_folder=$folder$desc
    if [ ! -d $output_folder ]; then mkdir -p $output_folder; fi

    # find root file names
    unset files i
    while IFS= read -r -d $'\0' tmp; do
        files[i++]="$tmp"
    done < <(find $folder \
        -name "${label[$j]}-*_ORAC_*_${file_version}.alb.nc" \
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
${sensor[$j]}${platform[$j]}
6
$channels
$phase
Ctrl%process_cloudy_only=false
Ctrl%NTypes_to_process=10
Ctrl%Types_to_process=0,1,2,3,4,5,6,7,8,9"
        if (( $sabotage )); then
            driver=$driver"
Ctrl%sabotage_inputs=true"
        fi
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
if (( $drop )); then
    for i in ${!label[*]}; do
        label[$i]=${label[$i]}$desc
    done
fi
# call IDL routine to compare this output to the previous version
if (( $do_compare && ("$?" == 0) )); then
    $idl_folder/idl -rt=$tool_folder/compare_orac_out.sav -args $out_folder \
        $revision 'main' ${#label[@]} ${label[@]}$desc $thresh
fi