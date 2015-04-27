#!/bin/bash
#2012/02/14 Matthias Jerg cleans out prototype code and implements more command line arguments.
#2012/02/14 C.poulsen adapted to RAL
#
#set so the first input is the level1b file
path_to_l1b=$1
YYYY=$2
MM=$3
DD=$4
path_to_output=$5
sensor=$6
path_to_geo=$7
version2=$8
daynight=$9
echo 'output input' $3
echo 'geo input' $7
echo 'sensor input' $6
echo 'daynight' $9


#if [ "$2" -gt "-1" ] ; then 
# path_to_output=/work/ralspace/rsg_group/cloud_ecv/data_out/aatsr/preproc/ 
#fi


#
#extract the data
#

scarf_flag=1

if [ $scarf_flag -eq 1 ] ; then
# this script runs on scarf
    path_to_coeffs=/work/ralspace/rsg_group/cloud_ecv/data_in/coeffs/
    path_to_emiss_atlas=/work/ralspace/rsg_group/cloud_ecv/libraries/rttov11/emis_data
#use this when submitting standalone
#    path_to_ecmwf=/work/ralspace/rsg_group/cloud_ecv/data_in/ecmwf/
#    path_to_ecmwf3=/work/ralspace/rsg_group/cloud_ecv/data_in/ecmwf/
#    path_to_ecmwf2=/work/ralspace/rsg_group/cloud_ecv/data_in/ecmwf/

#only works when submitting tby bsub
# -ggam_folder    The path containing NCDF ECMWF files named ggam.
# -ggas_folder    The path containing NCDF ECMWF files named ggas.
# -gpam_folder    The path containing NCDF ECMWF files named gpam.
    ggam_folder=/badc/ecmwf-era-interim/data/gg/as/$YYYY/$MM/$DD/
#ggas_folder=/badc/ecmwf-era-interim/data/gg/as/$YYYY/$MM/$DD/
#gpam_folder=/badc/ecmwf-era-interim/data/gg/am/$YYYY/$MM/$DD/

    ggas_folder=/work/ralspace/rsg_group/cloud_ecv/data_in/ecmwf/$YYYY/$MM/$DD/
    gpam_folder=/work/ralspace/rsg_group/cloud_ecv/data_in/ecmwf/$YYYY/$MM/$DD/
#    path_to_ecmwf3=/work/ralspace/rsg_group/cloud_ecv/data_in/ecmwf/$YYYY/$MM/$DD/
#    path_to_ecmwf2=/badc/ecmwf-era-interim/data/gg/as/$YYYY/$MM/$DD/
#    path_to_ecmwf=/work/ralspace/rsg_group/cloud_ecv/data_in/ecmwf/$YYYY/$MM/$DD/
    echo ' ecmwf 2 path' $path_to_ecmwf2
    path_to_aatsr_drift_table_file=/work/ralspace/rsg_group/cloud_ecv/data_in/aatsr/calibration//AATSR_VIS_DRIFT_V03-00.DAT
    

    
    path_to_albedo=/work/ralspace/rsg_group/cloud_ecv/data_in/modis/albedo/$YYYY/
    path_to_brdf=/work/ralspace/rsg_group/cloud_ecv/data_in/modis/brdf/$YYYY/
    
    path_to_ice=/work/ralspace/rsg_group/cloud_ecv/data_in/ice_snow/$YYYY/
    
    path_to_emissivity=/work/ralspace/rsg_group/cloud_ecv/data_in/emissivity/
    path_to_dem=/work/ralspace/rsg_group/cloud_ecv/data_in/dem/Aux_file_CM_SAF_AVHRR_GAC_ori_0.05deg.nc
    
    l1bdir=/work/ralspace/rsg_group/cloud_ecv/data_in/modis/lv1b/
    
else
echo 'not running on scarf'    
fi


#
#put a chck in here

#set general processing variables
ncdf_version='4.0'
cf_convention='CF-1.4'
processing_inst='RAL'
l2processor='CC4CL'
l2proc_version='1.0'
contact_email='caroline.poulsen@stfc.ac.uk'
contact_website='www.esa-cloud-cci.info'
file_version=${version2}
reference='http://www.esa-cloud-cci.org/'
history='This_data_was_processed_at_RAL'
summary='cloud_macro_and_microphysical_properties_from_ATSR'
keywords='satellite_cloud_cci_cc4cl_ATSR_Envisat_ERS'
comment='xx'
project='ESACCI'
license='_ESA_CCI_Data_policy:_free_and_open_access'
svn_version='2865'
rttov_version='RTTOV_version_11'
ecmwf_version='ERA-Interim'

#
#if AATSR set path to calibration files
#
# set flag to 1 if using badc files

#badc_flag=true
badc_flag=1
brdf_flag=true
#path where grib or ntcdf files are located
#path_to_ecmwf=~/Data/projects/ecv_clouds/ecmwf/
#badc files

#path to MODIS geolocation and angles etc. file
#put path to l1b MODIS sensor file

#        if [ ! $?path_to_l1b ] ; then
               echo 'path_to l1b' $path_to_l1b

#	else

#	    path_to_l1b=/work/ralspace/rsg_group/cloud_ecv/data_in/aatsr/lv1b/2008/06/20/ATS_TOA_1PRUPA20080620_102712_000065272069_00351_32970_0724.N1
#	fi



#sensor=AATSR

#NB grids must be regular
#1:ecmwf grid, 2:L3 grid, 3: own definition(use dellon/dellat values)
gridflag=1
#this is the inverse of the actual increment:
dellon=1.38888889
dellat=1.38888889
#dellon=2.0
#dellat=2.0

#could be something like the 1.6 ,3.4 mum channel thing.
# "0": use all channels (for MODIS where they are all available)
# "1": use 1.6mum channel and NOT 3.4
# "3": use 3.4 mum channel and NOT 1.6
# "2": pick "1" OR "3" automatically depending on which AVHRR is just processed. (to be implemented)

channelflag=0



#start and end pixel in across track direction, if one of the following four is negative: process whole granule
#set to zero will read the whole file daylight
startx=1
endx=512
#endx=200
#endx=1354
#start and end pixel in along track direction
#starty=12000
#endy=13000 #27755
#for chunking
chunkproc=0 #false # For (A)ATSR only
# set flag to use chunking when writing NCDF files


ncdf_chunk=0
verbose=true
full_path=false

starty=0
endy=0
#need this defined even though not read MODIS reads in 2 files

#path_to_geo=$path_to_geo
#for testing the test scene this is currently only relevant to AATSR
#daynight=1 #day
#daynight=0 #all
#daynight=2 #night=2
# for testing
#starty=7651
#endy=10651

#get uid and time information
uuid_tag=`exec uuidgen -t`
#uuid_tag=``

exec_time=`exec date +%Y%m%d%H%M%S`
#exec_time=``

echo 'UUID' $uuid_tag

#start the preprocessing and pass the variables to the binary on the command line
echo ''
echo 'Do this:'
echo ''
#
# or if using the intel debugger
#
#nb must set a new mysup file each time you run
#
#touch mySup4

# put this in and it causes error
#export LD_LIBRARY_PATH=/work/ralspace/rsg_group/cloud_ecv/libraries/eprlibdir/:/work/ralspace/rsg_group/cloud_ecv/libraries/szlib/
#currently have a problem with chunking and reading the file could be epr library?

#old
#/work/ralspace/rsg_group/cloud_ecv/code/code_v2334/trunk/pre_processing/src/orac_preproc.x $sensor $path_to_l1b $path_to_geo $path_to_dem $path_to_ecmwf $path_to_coeffs $path_to_emiss_atlas $path_to_ice $path_to_albedo $path_to_emissivity  $gridflag  $dellon $dellat $path_to_output  $startx $endx $starty $endy $ncdf_version $cf_convention $processing_inst $l2processor $l2proc_version $contact_email $contact_website $file_version $reference $history $summary $keywords $comment $project $license $uuid_tag $exec_time $path_to_aatsr_drift_table_file $badc_flag  $path_to_ecmwf2  $path_to_ecmwf  $chunkproc  $daynight $verbose  $ncdf_chunk $full_path


/work/ralspace/rsg_group/cloud_ecv/code/code_v2740/trunk/pre_processing/orac_preproc.x $sensor $path_to_l1b $path_to_geo $path_to_dem $ggam_folder $path_to_coeffs $path_to_emiss_atlas $path_to_ice $path_to_albedo $path_to_brdf $path_to_emissivity  $dellon $dellat $path_to_output  $startx $endx $starty $endy $ncdf_version $cf_convention $processing_inst $l2processor  $contact_email $contact_website $file_version $reference $history $summary $keywords $comment $project $license $uuid_tag $exec_time $path_to_aatsr_drift_table_file $badc_flag $ggas_folder $gpam_folder $chunkproc  $daynight $verbose  $ncdf_chunk $full_path $brdf_flag $rttov_version $ecmwf_version $svn_version

#adams
#/work/ralspace/rsg_group/cloud_ecv/code/code_v2334/trunk/pre_processing/src/orac_preproc.x $sensor $path_to_l1b $path_to_geo $ggam_folder $coeffs_folder $emiss_atlas_folder $ice_folder $albedo_folder $emiss_folder $dellon $dellat $folder $startx$endx$starty$endy$ncdf_version $cf_convention $processing_inst $l2processor $revision $contact_email $contact_website $file_version $reference $hist $summary $keywords $comment $label$license $uuid_tag $exec_time $calib_folder $badc_flag $ggas_folder $gpam_folder $chunkproc $daynight   $verbose $ncdf_chunk $full_path $brdf_flag