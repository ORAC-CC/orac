#!/bin/bash
#2012/02/14 Matthias Jerg cleans out prototype code and implements more command line arguments.
#2012/02/14 C.poulsen adapted to RAL
#
#set so the first input is the level1b file
path_to_l1b=$1


#
#extract the data
#

scarf_flag=0

       if [ $scarf_flag -eq 1 ] ; then
# this script runs on scarf
path_to_coeffs=/work/ralspace/rsg_group/cloud_ecv/data_in/coeffs/
path_to_emiss_atlas=/misc/wantage_static/rsg/Data/fm/rttov/rttov102_intel2/emis_data/
path_to_ecmwf=/work/ralspace/rsg_group/cloud_ecv/data_in/ecmwf/

path_to_aatsr_drift_table_file=/work/ralspace/rsg_group/cloud_ecv/data_in/aatsr/calibration//AATSR_VIS_DRIFT_V02-09.DAT

path_to_output=/work/ralspace/rsg_group/cloud_ecv/data_out/preproc/

path_to_albedo=/work/ralspace/rsg_group/cloud_ecv/data_in/modis/albedo/

path_to_ice=/work/ralspace/rsg_group/cloud_ecv/data_in/ice_snow/

path_to_emissivity=~/Data/projects/ecv_clouds/emissivity/
#/work/ralspace/rsg_group/cloud_ecv/data_in/modis/lv1b/2008/
l1bdir=/work/ralspace/rsg_group/cloud_ecv/data_in/modis/lv1b/

else
path_to_coeffs=/home/cluster/cpoulsen/orac_svn/v14/coeffs
path_to_emiss_atlas=/misc/wantage_static/rsg/Data/fm/rttov/rttov102_intel2/emis_data
path_to_ecmwf=/home/cluster/cpoulsen/Data/ecmwf/era-interim//2008/09/01/

path_to_aatsr_drift_table_file=/home/cluster/cpoulsen/Data/aatsr/instrument/calibration/AATSR_VIS_DRIFT_V02-09.DAT

#set path where output is supposed to go
#path_to_output=~/Data/projects/ecv_clouds/test_output/v10/2008/03/01/ ;orig
path_to_output=~/Data/projects/ecv_clouds/test_output/2008/09/01/

#path where modis albedo files are located
path_to_albedo=~/Data/projects/ecv_clouds/albedo/2008/
#path_to_albedo=/misc/cluster_home/cpoulsen/orac_svn/v11/modis/

#path where ice/snow files are located
path_to_ice=/home/cluster/cpoulsen/Data/projects/ecv_clouds/ice_snow/2008/

#path where modis emissivity files are located
path_to_emissivity=/home/cluster/cpoulsen/Data/projects/ecv_clouds/emissivity/

l1bdir=/home/cluster/cpoulsen/Data/aatsr/lv1b/
fi


#
#put a chck in here

#set general processing variables
ncdf_version='3.6.3'
cf_convention='CF-1.4'
processing_inst='RAL'
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


#
#if AATSR set path to calibration files
#
# set flag to 1 if using badc files
badc_flag='1'
#path where grib or ntcdf files are located
#path_to_ecmwf=~/Data/projects/ecv_clouds/ecmwf/
#badc files

#path to MODIS geolocation and angles etc. file
#put path to l1b MODIS sensor file

        if [ ! $?path_to_l1b ] ; then
               echo $path_to_l1b
	else

#	    path_to_l1b=/home/cluster/cpoulsen/Data/aatsr/lv1b/2008/06/20/ATS_TOA_1PRUPA20080620_102712_000065272069_00351_32970_0724.N1

 path_to_l1b=/home/cluster/cpoulsen/Data/aatsr/lv1b/2008/09/01/ATS_TOA_1PRUPA20080901_002918_000065272071_00388_34009_8789.N1
#	    path_to_l1b=/home/cluster/cpoulsen/Data/aatsr/lv1b/2010/04/16/ATS_TOA_1PRUPA20100416_203037_000065272088_00357_42495_3290.N1
#	    path_to_l1b=/home/cluster/cpoulsen/Data/aatsr/lv1b/2008/03/01/ATS_TOA_1PRUPA20080301_201917_000065272066_00271_31387_6841.N1
#  path_to_l1b=/home/cluster/cpoulsen/Data/aatsr/lv1b/2008/12/20/ATS_TOA_1PRUPA20081220_093525_000065272074_00465_35589_9130.N1
#path_to_l1b=~/Data/aatsr/lv1b/modis_collocations/2008/04/01/ATS_TOA_1PRMAP20080401_034030_000000532067_00203_31820_0001.N1
#path_to_l1b=~/Data/aatsr/lv1b/modis_collocations/2008/04/01/ATS_TOA_1PRMAP20080401_084213_000000482067_00206_31823_0001.N1
#path_to_l1b=~/Data/aatsr/lv1b/modis_collocations/2008/04/01/ATS_TOA_1PRMAP20080401_015959_000000482067_00202_31819_0001.N1
#path_to_l1b=~/Data/aatsr/lv1b/modis_collocations/2008/12/18/ATS_TOA_1PRMAP20081218_184307_000000482074_00441_35565_0001.N1
	fi



 
sensor=AATSR

#NB grids must be regular
#1:ecmwf grid, 2:L3 grid, 3: own definition(use dellon/dellat values)
gridflag=1
#this is the inverse of the actual increment:
dellon=2.0
dellat=2.0

#could be something like the 1.6 ,3.4 mum channel thing.
# "0": use all channels (for MODIS where they are all available)
# "1": use 1.6mum channel and NOT 3.4
# "3": use 3.4 mum channel and NOT 1.6
# "2": pick "1" OR "3" automatically depending on which AVHRR is just processed. (to be implemented)

#channelflag=0



#start and end pixel in across track direction, if one of the following four is negative: process whole granule
#set to zero will read the whole file daylight
startx=1
endx=512
#endx=200
#endx=1354
#start and end pixel in along track direction
#starty=7651
#endy=8000 #27755

path_to_geo=~/Data/projects/ecv_clouds/modis_data_rr/2008/06/20/MYD03.A2008172.0340.005.2009316101559.hdf

#for testing the test scene this is currently only relevant to AATSR
daynight=1
#daynight=0 #all
#daynight=2 #night

chunkproc=1 # this is used for AATSR orbits which are very large and need to be split up set to 0 for modis or other instruments when chunking is not required
#starty=1
#endy=0
# for testing

startx=1
endx=512

starty=0
endy=0

# binary flag setting if text should be output to stdout during processing
verbose=true

#starty=1
#endy=100 #27755
#endy=2030
#get uid and time information
uuid_tag=`exec uuidgen -t`
#uuid_tag=``

exec_time=`exec date +%Y%m%d%H%M%S`
#exec_time=``

echo 'UUID' $uuid_tag

#start the preprocessing and pass the variables to the binary on the command line
echo ''
echo 'Do this:'
echo './orac_preproc' $sensor $path_to_l1b $path_to_geo $path_to_ecmwf $path_to_coeffs $path_to_emiss_atlas $path_to_ice $path_to_albedo $path_to_emissivity $gridflag $dellon $dellat $path_to_output $startx $endx $starty $endy $ncdf_version $cf_convention $processing_inst $l2processor $l2proc_version $contact_email $contact_website $file_version $reference $history $summary $keywords $comment $project $license $uuid_tag $exec_time $path_to_aatsr_drift_table_file $badc_flag $path_to_ecmwf $path_to_ecmwf $chunkproc $daynight $verbose

echo ''
#
# or if using the intel debugger
#
#nb must set a new mysup file each time you run
#
touch mySup4
#/opt/intel/inspector_xe_2011/bin64/inspxe-cl -c mi3 -suppression-file mySup4 -r myRes5  -- orac_preproc $sensor $path_to_l1b $path_to_geo $path_to_ecmwf $path_to_coeffs $path_to_emiss_atlas $path_to_ice $path_to_albedo $path_to_emissivity  $gridflag  $dellon $dellat $path_to_output $channelflag $startx $endx $starty $endy $ncdf_version $cf_convention $processing_inst $l2processor $l2proc_version $contact_email $contact_website $file_version $reference $history $summary $keywords $comment $project $license $uuid_tag $exec_time $path_to_aatsr_drift_table_file $badc_flag

./orac_preproc $sensor $path_to_l1b $path_to_geo $path_to_ecmwf $path_to_coeffs $path_to_emiss_atlas $path_to_ice $path_to_albedo $path_to_emissivity  $gridflag  $dellon $dellat $path_to_output $startx $endx $starty $endy $ncdf_version $cf_convention $processing_inst $l2processor $l2proc_version $contact_email $contact_website $file_version $reference $history $summary $keywords $comment $project $license $uuid_tag $exec_time $path_to_aatsr_drift_table_file $badc_flag  $path_to_ecmwf  $path_to_ecmwf  $chunkproc  $daynight  $verbose

#./orac_preproc AATSR /home/cluster/cpoulsen/Data/aatsr/lv1b/2008/06/20/ATS_TOA_1PRUPA20080620_102712_000065272069_00351_32970_0724.N1 /home/cluster/cpoulsen/Data/projects/ecv_clouds/modis_data_rr/2008/06/20/MYD03.A2008172.1240.005.2009316103907.hdf /home/cluster/cpoulsen/Data/projects/ecv_clouds/ecmwf/ /home/cluster/cpoulsen/orac_svn/v11/coeffs /misc/wantage_static/rsg/Data/fm/rttov/rttov102_intel2/emis_data /home/cluster/cpoulsen/Data/projects/ecv_clouds/ice_snow/ /home/cluster/cpoulsen/Data/projects/ecv_clouds/albedo/2008/ /home/cluster/cpoulsen/Data/projects/ecv_clouds/emissivity/ 3 2.0 2.0 /home/cluster/cpoulsen/Data/projects/ecv_clouds/test_output 0 1 512 7651 7751 3.6.3 CF-1.4 RAL ORAC 1.0 contact.cloudcci@dwd.de www.esa-cloud-cci.info V1.0 ATBD !!! !!! !!! !!! ESA_Cloud_cci !!! 7bdb3dc4-fcc6-11e1-861d-001a921d5ab7 20120912114200 /home/cluster/cpoulsen/Data/aatsr/instrument/calibration/AATSR_VIS_DRIFT_V02-09.DAT 0



#
#to read thses errors go to gui and read in myRes in results option
#

