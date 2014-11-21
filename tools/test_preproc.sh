#!/bin/bash
# Name:
#    test_preproc.sh
#
# Purpose:
#    A shell script to run the ORAC preprocessor on the test files and compare
#    the results to the most recent previous version of the output.
#
# Calling sequence -
#    ./test_preproc.sh [-albedo_folder PATH] [-calib_folder PATH]
#       [-coeffs_folder PATH] [-do DAYAATSR|NITAATSR|AATSR|DAYAVHRR|NITAVHRR|
#       AVHRR|DAYMYD|NITMYD|MYD|MODIS|DAY|NIT|NIGHT|ALL] [-ecmwf 0|1
#       -ecmwf_folder PATH] [-ecmwf 2 -ggam_folder PATH -ggas_folder PATH
#       -gpam_folder PATH] [-emiss_atlas_folder PATH] [-emiss_folder PATH]
#       [-first] [-ice_folder PATH] [-idl_folder PATH] [-ld_set]
#       [-in_folder PATH] [-n_procs NUM] [-no_compare|only_compare]
#       [-out_folder PATH] [-orac_folder PATH] [-orac_lib NAME]
#       [-preproc_folder PATH] [-revision NUMBER]  [-short] [-thresh NUM]
#       [-tool_folder PATH]
#
# Command line arguments -
#    See header.sh for full details of arguments.
#
# Known issues:
#    The "project" field from the file name has been requisitioned to
#    distinguish the different test cases.
#
# History:
# 2013/11/01, AP: Original version
# 2014/01/27, AP: Altered formation of LD_LIBRARY_PATH to use library file.
#    Fixed bug in passing header arguments.
# 2014/02/04, AP: Improved command line arguments.
# 2014/04/30, AP: New folder structure. Minor bug fixes. Correctly assigned
#    NITMYD files. Updated AVHRR. Added -v21.
# 2014/07/01, AP: Changed processing order as MODIS is faster.
# 2014/07/14, AP: Added THRESH and ONLY_COMPARE arguments.
# 2014/07/28, AP: Moved arguments into header script.
# 2014/11/21, GM: Update to account for input path 'modis_brdf_path' with
#    $brdf_folder.
#
set -e

source header.sh

#------------------------------------------------------------------------------
# DEFINE PREPROCESSING DETAILS
#------------------------------------------------------------------------------
# this is the inverse of the actual lat/lon increment
dellon=1.38888889
dellat=1.38888889
#dellon=1.422222
#dellat=1.436783

# set flag to break ATSR files into smaller chunks
chunkproc=0

# set flag to use chunking when writing NCDF files
ncdf_chunk=0

# set ECMWF EMOS library to print debugging output
#export JDCNDBG=1

# set to 1 if the folder names given are in fact paths to specific files
full_path=0


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
    if [[ $revision -ge 2729 ]]; then
        arg=( "${sensor[$j]}" "${path_to_l1b[$j]}" "${path_to_geo[$j]}" "${path_to_usgs}" "${ggam_folder}" "${coeffs_folder}" "${emiss_atlas_folder}" "${ice_folder}" "${albedo_folder}" "${brdf_folder}" "${emiss_folder}" "${dellon}" "${dellat}" "${folder}" "${startx[$j]}" "${endx[$j]}" "${starty[$j]}" "${endy[$j]}" "${ncdf_version}" "${cf_convention}" "${processing_inst}" "${l2processor}" "${revision}" "${contact_email}" "${contact_website}" "${file_version}" "${reference}" "${hist}" "${summary}" "${keywords}" "${comment}" "${label[$j]}" "${license}" "${uuid_tag}" "${exec_time}" "${calib_folder}" "${badc_flag}" "${ggas_folder}" "${gpam_folder}" "${chunkproc}" "${daynight[$j]}" "${verbose}" "${ncdf_chunk}" "${full_path}" "${brdf_flag}" )
    elif [[ $revision -ge 2284 ]]; then
        arg=( "${sensor[$j]}" "${path_to_l1b[$j]}" "${path_to_geo[$j]}" "${ggam_folder}" "${coeffs_folder}" "${emiss_atlas_folder}" "${ice_folder}" "${albedo_folder}" "${emiss_folder}" "${dellon}" "${dellat}" "${folder}" "${startx[$j]}" "${endx[$j]}" "${starty[$j]}" "${endy[$j]}" "${ncdf_version}" "${cf_convention}" "${processing_inst}" "${l2processor}" "${revision}" "${contact_email}" "${contact_website}" "${file_version}" "${reference}" "${hist}" "${summary}" "${keywords}" "${comment}" "${label[$j]}" "${license}" "${uuid_tag}" "${exec_time}" "${calib_folder}" "${badc_flag}" "${ggas_folder}" "${gpam_folder}" "${chunkproc}" "${daynight[$j]}" "${verbose}" "${ncdf_chunk}" "${full_path}" "${brdf_flag}" )
    elif [[ $revision -ge 2208 ]]; then
        arg=( "${sensor[$j]}" "${path_to_l1b[$j]}" "${path_to_geo[$j]}" "${ggam_folder}" "${coeffs_folder}" "${emiss_atlas_folder}" "${ice_folder}" "${albedo_folder}" "${emiss_folder}" "${dellon}" "${dellat}" "${folder}" "${startx[$j]}" "${endx[$j]}" "${starty[$j]}" "${endy[$j]}" "${ncdf_version}" "${cf_convention}" "${processing_inst}" "${l2processor}" "${revision}" "${contact_email}" "${contact_website}" "${file_version}" "${reference}" "${hist}" "${summary}" "${keywords}" "${comment}" "${label[$j]}" "${license}" "${uuid_tag}" "${exec_time}" "${calib_folder}" "${badc_flag}" "${ggas_folder}" "${gpam_folder}" "${chunkproc}" "${daynight[$j]}" "${verbose}" "${ncdf_chunk}" "${full_path}" )
    elif [[ $revision -ge 2133 ]]; then
        arg=( "${sensor[$j]}" "${path_to_l1b[$j]}" "${path_to_geo[$j]}" "${ggam_folder}" "${coeffs_folder}" "${emiss_atlas_folder}" "${ice_folder}" "${albedo_folder}" "${emiss_folder}" "${gridflag}" "${dellon}" "${dellat}" "${folder}" "${startx[$j]}" "${endx[$j]}" "${starty[$j]}" "${endy[$j]}" "${ncdf_version}" "${cf_convention}" "${processing_inst}" "${l2processor}" "${revision}" "${contact_email}" "${contact_website}" "${file_version}" "${reference}" "${hist}" "${summary}" "${keywords}" "${comment}" "${label[$j]}" "${license}" "${uuid_tag}" "${exec_time}" "${calib_folder}" "${badc_flag}" "${ggas_folder}" "${gpam_folder}" "${chunkproc}" "${daynight[$j]}" "${verbose}" "${ncdf_chunk}" "${full_path}" )
    elif [[ $revision -ge 1958 ]]; then
        arg=( "${sensor[$j]}" "${path_to_l1b[$j]}" "${path_to_geo[$j]}" "${ggam_folder}" "${coeffs_folder}" "${emiss_atlas_folder}" "${ice_folder}" "${albedo_folder}" "${emiss_folder}" "${gridflag}" "${dellon}" "${dellat}" "${folder}" "${startx[$j]}" "${endx[$j]}" "${starty[$j]}" "${endy[$j]}" "${ncdf_version}" "${cf_convention}" "${processing_inst}" "${l2processor}" "${revision}" "${contact_email}" "${contact_website}" "${file_version}" "${reference}" "${hist}" "${summary}" "${keywords}" "${comment}" "${label[$j]}" "${license}" "${uuid_tag}" "${exec_time}" "${calib_folder}" "${badc_flag}" "${ggas_folder}" "${gpam_folder}" "${chunkproc}" "${daynight[$j]}" "${verbose}" "${ncdf_chunk}" )
    elif [[ $revision -ge 1654 ]]; then
        arg=( "${sensor[$j]}" "${path_to_l1b[$j]}" "${path_to_geo[$j]}" "${ggam_folder}" "${coeffs_folder}" "${emiss_atlas_folder}" "${ice_folder}" "${albedo_folder}" "${emiss_folder}" "${gridflag}" "${dellon}" "${dellat}" "${folder}" "${startx[$j]}" "${endx[$j]}" "${starty[$j]}" "${endy[$j]}" "${ncdf_version}" "${cf_convention}" "${processing_inst}" "${l2processor}" "${revision}" "${contact_email}" "${contact_website}" "${file_version}" "${reference}" "${hist}" "${summary}" "${keywords}" "${comment}" "${label[$j]}" "${license}" "${uuid_tag}" "${exec_time}" "${calib_folder}" "${badc_flag}" "${ggas_folder}" "${gpam_folder}" "${chunkproc}" "${daynight[$j]}" "${verbose}" )
    else
        arg=( "${sensor[$j]}" "${path_to_l1b[$j]}" "${path_to_geo[$j]}" "${ggam_folder}" "${coeffs_folder}" "${emiss_atlas_folder}" "${ice_folder}" "${albedo_folder}" "${emiss_folder}" "${gridflag}" "${dellon}" "${dellat}" "${folder}" 0 "${startx[$j]}" "${endx[$j]}" "${starty[$j]}" "${endy[$j]}" "${ncdf_version}" "${cf_convention}" "${processing_inst}" "${l2processor}" "${revision}" "${contact_email}" "${contact_website}" "${file_version}" "${reference}" "${hist}" "${summary}" "${keywords}" "${comment}" "${label[$j]}" "${license}" "${uuid_tag}" "${exec_time}" "${calib_folder}" "${badc_flag}" "${ggas_folder}" "${gpam_folder}" "${chunkproc}" "${daynight[$j]}" )
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
        $revision 'preproc' ${#label[@]} ${label[@]} $thresh
fi