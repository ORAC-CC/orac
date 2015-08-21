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
    echo "Processing $inst"

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

        driver=$driver_file_base${inst}.txt
        log_file=$fdr/${inst}'_POSTPROC_V'$revision'_'`date +"%y%m%d_%H%M"`.log

        # write driver file
        echo "'$wat_prim'" 1> $driver
        echo "'$ice_prim'" 1>> $driver
        echo "'$wat_sec'" 1>> $driver
        echo "'$ice_sec'" 1>> $driver
        echo "'$out_prim'" 1>> $driver
        echo "'$out_sec'" 1>> $driver
        echo "false" 1>> $driver                  # Process one phase only
        echo "false" 1>> $driver                  # Process cloudy only
        echo "0.1  0.1       " >> $driver         # minre water/ice
        echo "30.  200.      " >> $driver         # maxre water/ice
        echo "0.1   0.1      " >> $driver         # minod water/ice
        echo "999.  999.     " >> $driver         # maxod water/ice
        echo "1000000.       " >> $driver         # maxcost
        echo "1.5            " >> $driver         # costfactor
        echo "1.39            " >> $driver        # cot thre hold land cloud mask
        echo "0.2            " >> $driver         # cot thre hold sea cloud mask
        echo "0.3            " >> $driver         # cot thre hold for cloud mask
        echo "2 3 4 5 6 7  " >> $driver           # AATSR channel ids !!IGNORED!!
        echo "BLANK  " >> $driver                 # instrument
        echo "1                 " >> $driver      #! 1: work also on secondary input file, 0 dont
        echo "0                 " >> $driver      #! 1: do strict checking, with convergence flag, 0 dont
        echo "19.0           " >> $driver         # temp_thres_h
        echo "19.0           " >> $driver         # temp_thres_m
        echo "19.0           " >> $driver         # temp_thres_l
        echo "14.0           " >> $driver         # temp_thres1
        echo "150.0        " >> $driver           # ctt_bound
        echo "275.0        " >> $driver           # ctt_bound_winter
        echo "295.0        " >> $driver           # ctt_bound_summer
        echo "280.0        " >> $driver           # ctt_thres
        echo "907.0        " >> $driver           # ctp_thres
        echo "957.0        " >> $driver           # ctp_thres1
        echo "50.0           " >> $driver         # ctp_bound
        echo "1100.0      " >> $driver            # ctp_bound_up
        echo "100000.0 " >> $driver               # ctp_udivctp
        echo "xxx" >> $driver                     # uuid_tag_primary
        echo "xxx" >> $driver                     # uuid_tag_secondary
        echo "???" >> $driver                     # platform
        echo "20120803:122601" >> $driver         # prodtime
        echo "'L2 final output'" >> $driver       # prod_name
        echo 'L2' >> $driver                      # cprodtype
        echo "$ncdf_version" >> $driver           # cncver
        echo "$cf_convention" >> $driver          # ccon
        echo "$processing_inst" >> $driver        # cinst
        echo "$l2processor" >> $driver            # l2cproc
        echo "$revision" >> $driver               # l2cprocver
        echo "$contact_email" >> $driver          # contact
        echo "$contact_website" >> $driver        # website
        echo "$reference" >> $driver              # reference
        echo "$hist" >> $driver                   # history
        echo "$summary" >> $driver                # summary
        echo "$keywords" >> $driver               # keywords
        echo "$comment" >> $driver                # comment
        echo "${label[$j]}" >> $driver            # project
        echo "$license" >> $driver                # license
        echo "$file_version" >> $driver           # cfile_version
        echo "???" >> $driver                     # csource
        echo " " >> $driver                       # !!IGNORED!!
        echo "'NetCDF Climate Forecast (CF) Metadata Convention version 18'" >> $driver # std_name_voc

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
    $idl_folder/idl -rt=$tool_folder/compare_orac_out.sav \
        -args $out_folder $revision ${#label[@]} ${label[@]} \
              2 primary secondary $thresh
fi
