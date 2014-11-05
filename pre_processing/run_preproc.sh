#!/bin/bash
#2012/02/14 Matthias Jerg cleans out prototype code and implements more command line arguments.


#set general processing variables
ncdf_version='3.6.3'
cf_convention='CF-1.4'
processing_inst='DWD'
l2processor='ORAC'
l2proc_version='1.0'
contact_email='contact.cloudcci@dwd.de'
contact_website='www.esa-cloud-cci.info'
file_version='V1.0'
reference='ATBD'
history='!!!'
summary='!!!'
keywords='!!!'
comment='!!!'
project='ESA_Cloud_cci'
license='!!!'

# Library paths for dynamic linking
LIB_BASE=/home/jupiter/eodg/gthomas/orac/orac_for_cci/fortran_preprocessing/libs
LD_LIBRARY_PATH=${LIB_BASE}/hdf4/lib:${LD_LIBRARY_PATH}
LD_LIBRARY_PATH=${LIB_BASE}/hdf5/lib:${LD_LIBRARY_PATH}
LD_LIBRARY_PATH=${LIB_BASE}/hdfeos/lib:${LD_LIBRARY_PATH}
LD_LIBRARY_PATH=${LIB_BASE}/netcdf/lib:${LD_LIBRARY_PATH}
LD_LIBRARY_PATH=${LIB_BASE}/grib/lib:${LD_LIBRARY_PATH}
LD_LIBRARY_PATH=${LIB_BASE}/jasper/lib:${LD_LIBRARY_PATH}
LD_LIBRARY_PATH=${LIB_BASE}/rttov/lib:${LD_LIBRARY_PATH}
LD_LIBRARY_PATH=${LIB_BASE}/epr_api/lib:${LD_LIBRARY_PATH}
export LD_LIBRARY_PATH

path_to_coeffs=/cmsaf/cmsaf-cld4/esa_cloud_cci/orac_new/trunk/pre_processing/coeffs
path_to_emiss_atlas=/cmsaf/cmsaf-cld4/esa_cloud_cci/rttov10.2/emis_data

#put path to l1b MODIS sensor file
path_to_l1b=/cmsaf/cmsaf-cld4/esa_cloud_cci/orac_new/trunk/pre_processing/modis/testinput/MYD021KM.A2008172.1240.005.2009317020933.bscs_000500531943.hdf
#path to MODIS geolocation and angles etc. file
path_to_geo=/cmsaf/cmsaf-cld4/esa_cloud_cci/orac_new/trunk/pre_processing/modis/testinput/MYD03.A2008172.1240.005.2009316103907.hdf

#put path to l1b AVHRR sensor file
#path_to_l1b=/cmsaf/cmsaf-cld4/esa_cloud_cci/orac_new/trunk/pre_processing/modis/testinput/noaa18_20080620_0935_99999_satproj_00000_12119_avhrr.h5
#path to AVHRR geolocation and angles etc. file
#path_to_geo=/cmsaf/cmsaf-cld4/esa_cloud_cci/orac_new/trunk/pre_processing/modis/testinput/noaa18_20080620_0935_99999_satproj_00000_12119_sunsatangles.h5 

#if AATSR set path to calibration files
path_to_aatsr_drift_table_file=/misc/wantage_static/rsg/Data/aatsr/instrument/calibration/AATSR_VIS_DRIFT_V02-09.DAT 


#set sensor
sensor=MODIS

#1:ecmwf grid, 2:L3 grid, 3: own definition
gridflag=1
#this is the inverse of the actual increment:
dellon=2.0
dellat=2.0


#set path where output is supposed to go
path_to_output=/cmsaf/cmsaf-cld4/esa_cloud_cci/orac_new/trunk/pre_processing/modis/testoutput

#path where grib files are located
path_to_ecmwf=/cmsaf/cmsaf-cld4/esa_cloud_cci/orac_new/trunk/pre_processing/ecmwf

#thses files are required for calculate surface reflectaivity
#path where modis albedo files are located
path_to_albedo=/cmsaf/cmsaf-cld4/esa_cloud_cci/orac_new/trunk/pre_processing/albedo
#path where ice/snow files are located
path_to_ice=/cmsaf/cmsaf-cld4/esa_cloud_cci/orac_new/trunk/pre_processing/ice_snow
#path where modis emissivity files are located
path_to_emissivity=/cmsaf/cmsaf-cld4/esa_cloud_cci/orac_new/trunk/pre_processing/emissivity

# set flag to 1 if using badc files
badc=0

#day/night processing
daynight=1
#break the scene/orbit into managable chunks in the preprocessing
chunkproc=0 # For (A)ATSR only

#start and end pixel in across track direction, if one of the following four is negative: process whole granule
startx=-25
endx=133

#start and end pixel in along track direction
starty=56
endy=311

# binary flag setting if text should be output to stdout during processing
verbose=true

#get uid and time information
uuid_tag=`exec uuidgen -t`
exec_time=`exec date +%Y%m%d%H%M%S`

echo 'UUID' $uuid_tag

#start the preprocessing and pass the variables to the binary on the command line
echo ''
echo 'Do this:'
echo './orac_preproc.x' $sensor $path_to_l1b $path_to_geo $path_to_ecmwf $path_to_coeffs $path_to_emiss_atlas $path_to_ice $path_to_albedo $path_to_emissivity $gridflag $dellon $dellat $path_to_output $startx $endx $starty $endy $ncdf_version $cf_convention $processing_inst $l2processor $l2proc_version $contact_email $contact_website $file_version $reference $history $summary $keywords $comment $project $license $uuid_tag $exec_time $path_to_aatsr_drift_table_file $badc $path_to_ecmwf $path_to_ecmwf $chunkproc $daynight $verbose

#echo './orac_preproc.x' ./orac_preproc.x $sensor $path_to_l1b $path_to_geo $path_to_ecmwf $path_to_coeffs $path_to_emiss_atlas $path_to_ice $path_to_albedo $path_to_emissivity  $gridflag  $dellon $dellat $path_to_output $channelflag $startx $endx $starty $endy $ncdf_version $cf_convention $processing_inst $l2processor $l2proc_version $contact_email $contact_website $file_version $reference $history $summary $keywords $comment $project $license $uuid_tag $exec_time $path_to_aatsr_drift_table_file $badc

echo ''

./orac_preproc.x $sensor $path_to_l1b $path_to_geo $path_to_ecmwf $path_to_coeffs $path_to_emiss_atlas $path_to_ice $path_to_albedo $path_to_emissivity  $gridflag  $dellon $dellat $path_to_output $startx $endx $starty $endy $ncdf_version $cf_convention $processing_inst $l2processor $l2proc_version $contact_email $contact_website $file_version $reference $history $summary $keywords $comment $project $license $uuid_tag $exec_time $path_to_aatsr_drift_table_file $badc $path_to_ecmwf $path_to_ecmwf $chunkproc $daynight $verbose

#./orac_preproc.x $sensor $path_to_l1b $path_to_geo $path_to_ecmwf $path_to_coeffs $path_to_emiss_atlas $gridflag  $dellon $dellat $path_to_output $channelflag $startx $endx $starty $endy $ncdf_version $cf_convention $processing_inst $l2processor $l2proc_version $contact_email $contact_website $file_version $reference $history $summary $keywords $comment $project $license $uuid_tag $exec_time
