#!/bin/bash
# Name:
#    postprocess.sh
#
# Purpose:
#    A shell script to run the ORAC postprocessor on the test files.
#
# Calling sequence:
#    ./postprocess.sh [-do DAYAATSR|NITAATSR|AATSR|DAYAVHRR|NITAVHRR|AVHRR|
#       DAYMYD|NITMYD|MYD|MODIS|DAY|NIT|NIGHT|ALL] [-first] [-ld_set]
#       [-n_procs NUM] [-out_folder PATH] [-orac_lib NAME] [-preproc_folder PATH]
#       [-postproc_folder PATH] [-revision NUMBER] [-short]
#
# Arguments:
#    See header.sh for full details of arguments.
#
# Known issues:
#    None known.
#
# History:
# 2014/07/28, AP: Original version
# 2015/01/06, AP: Ammending how -drop and -v21 are managed.
#
set -e

source header.sh

for i in ${!label[*]}; do
    label[$i]=${label[$i]}$desc
done

#------------------------------------------------------------------------------
# RUN ORAC POST-PROCESSOR
#------------------------------------------------------------------------------
driver_file_base=$out_folder/post_driver_
com=()
for j in ${!sensor[*]}; do
    echo "Processing ${label[$j]}"

    # find root file names
    fdr=$out_folder/V$revision/${label[$j]}
    unset files fi
    while IFS= read -r -d $'\0' tmp; do
        # take only the part of the filename before _ORACV (differentiate chunks)
        files[fi++]="${tmp%*_ORAC_*}"
    done < <(find $fdr \
        -name "${label[$j]}-*_ORAC_*_${file_version}WAT.primary.nc" \
        -printf "%f\0")
    if (( "${#files}" == 0 )); then
        echo 'No files found. Check revision number.'
        continue
    fi

    for root in ${files[*]}; do
        # find paths to main processor outputs
        wat_prim=`find $fdr -name $root*WAT.primary.nc`
        ice_prim=`find $fdr -name $root*ICE.primary.nc`
        out_prim=${wat_prim:0:-14}.primary.nc
        wat_sec=`find $fdr -name $root*WAT.secondary.nc`
        ice_sec=`find $fdr -name $root*ICE.secondary.nc`
        out_sec=${wat_sec:0:-16}.secondary.nc

        driver=$driver_file_base${label[$j]}.txt
        log_file=$fdr/${label[$j]}'_POSTPROC_V'$revision'_'`date +"%y%m%d_%H%M"`.log

        # write driver file
        echo "'$wat_prim'" 1>  $driver # Primary files for WAT & ICE phases
        echo "'$ice_prim'" 1>> $driver
        echo "'$wat_sec'"  1>> $driver # Secondary files for WAT & ICE phases
        echo "'$ice_sec'"  1>> $driver
        echo "'$out_prim'" 1>> $driver # Primary/secondary output filenames
        echo "'$out_sec'"  1>> $driver
        echo "false"       1>> $driver # Process one phase only
        echo "use_netcdf_compression=false" 1>> $driver # For testing purposes

        echo "LD_LIBRARY_PATH=${LD_LIBRARY_PATH}" 1> $log_file
        echo '' 1>> $log_file
        echo '-----DRIVER FILE-----' 1>> $log_file
        cat $driver 1>> $log_file
        echo 'Do this:' 1>> $log_file
        echo $postproc_folder/post_process_level2 $driver 1>> $log_file
        echo '' 1>> $log_file

        # run postprocessor
        # Non-parallel version
        $postproc_folder/post_process_level2 $driver >> $log_file
        if (( $? != 0 )); then
            echo "${inst}: Error."
            exit -2
        fi
        echo "Postprocessed $root"

        # Parallel version
#    com="$com$postproc_folder/post_process_level2 $driver >> $log_file 2>&1 "\
#"echo 'Postprocessed $root'"$'\n'
    done
done
#echo "$com" | xargs -P $n_procs -n 1 -I COMMAND sh -c COMMAND

# clean up driver files
rm -f $driver_file_base*

#------------------------------------------------------------------------------
# CHECK RESULTS
#------------------------------------------------------------------------------
# call IDL routine to compare this output to the previous version
if (( $do_compare && ("$?" == 0) )); then
    $idl_folder/idl -rt=$tool_folder/compare_orac_out.sav -quiet \
        -args $out_folder $revision ${#label[@]} ${label[@]} \
              2 primary secondary $thresh
fi
