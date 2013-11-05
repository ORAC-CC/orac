#!/bin/bash
# test_preproc.sh
#
# A shell script to run the ORAC preprocessor on the test files and compare
# the results to a previous version. The directories in the first section
# should be set to represent your local file system.
#
# Command line arguments -
# short = set to only process a few lines from each file
# first = set to indicate that you are processing the current version
#         without alteration and do not need to increase the revision number
#
# History -
# 2013/11/01: AP Original version
#

# deal with command arguments
if [[ "$1" == 'first' || "$2" == 'first' ]]; then first=1; else first=0; fi
if [[ "$1" == 'short' || "$2" == 'short' ]]; then short=1; else short=0; fi

#------------------------------------------------------------------------------
# DEFINE LOCAL FOLDERS
#------------------------------------------------------------------------------
# path to ORAC preprocessor executable
oracfolder=/home/jupiter/eodg2/povey/orac/trunk/pre_processing/src

# path to IDL executable
idlfolder=/usr/local/PACK/idl82/idl82/bin

# path to (local) library files
LIB_BASE=/home/jupiter/eodg/gthomas/orac/orac_for_cci/fortran_preprocessing/libs

# path to ORAC data repository
datafolder=/home/midi/eodg2/data

verbose=true

#------------------------------------------------------------------------------
# DETERMINE NECESSARY FOLDERS
#------------------------------------------------------------------------------
# assume the hierachy of the ORAC repository
infolder=$datafolder/testinput
outfolder=$datafolder/testoutput
# path to albedo files (in folders by year)
path_to_albedo=$datafolder/albedo
# path to channel information
path_to_coeffs=$datafolder/coeffs
# path to emissivity atlas (SHOULD BE ADDED TO REPOSITORY)
path_to_emiss_atlas=$datafolder/emissivity
# path to ATSR drift coefficients
path_to_calib=$datafolder/AATSR_VIS_DRIFT_V02-05.DAT
# path where ECMWF files are located
path_to_ecmwf=$datafolder/ecmwf
# set flag to 1 if using badc NCDF files
badc_flag='1'
# path where ice/snow files are located
path_to_ice=$datafolder/ice_snow
# path where modis emissivity files are located
path_to_emissivity=$datafolder/emissivity

# determine new revision number by asking SVN for current number and adding 1
revstr=`svn info | grep 'Revision'`
revind=$((`expr index "$revstr" '[0123456789]'`-1))
if [[ "$revind" -lt 0 ]]; then
    echo 'Unable to determine version number from SVN.'
    revision=10000
else
    if (( $first == 1 ))
       then revision=${revstr:$revind}
       else revision=$((${revstr:$revind}+1))
    fi
    echo "Revision number ${revision}"
fi

# Library paths for dynamic linking
LD_LIBRARY_PATH=${LIB_BASE}/hdf4/lib:${LD_LIBRARY_PATH}
LD_LIBRARY_PATH=${LIB_BASE}/hdf5/lib:${LD_LIBRARY_PATH}
LD_LIBRARY_PATH=${LIB_BASE}/hdfeos/lib:${LD_LIBRARY_PATH}
LD_LIBRARY_PATH=${LIB_BASE}/netcdf/lib:${LD_LIBRARY_PATH}
LD_LIBRARY_PATH=${LIB_BASE}/grib/lib:${LD_LIBRARY_PATH}
LD_LIBRARY_PATH=${LIB_BASE}/jasper/lib:${LD_LIBRARY_PATH}
LD_LIBRARY_PATH=${LIB_BASE}/rttov/lib:${LD_LIBRARY_PATH}
LD_LIBRARY_PATH=${LIB_BASE}/epr_api/lib:${LD_LIBRARY_PATH}
export LD_LIBRARY_PATH

#------------------------------------------------------------------------------
# DEFINE INPUT DATA INFORMATION
#------------------------------------------------------------------------------

#---- AATSR (day) ----
sensor[0]=AATSR
label[0]=DAYAATSR
path_to_l1b[0]=$infolder/ATS_TOA_1PRUPA20080620_002337_000065272069_00345_32964_0666.N1
path_to_geo[0]=${path_to_l1b[0]}
if (( $short == 1 )); then startx[0]=1; else startx[0]=0 ; fi
endx[0]=512
starty[0]=21366
endy[0]=21370
daynight[0]=1

#---- AATSR (night) ----
sensor[1]=AATSR
label[1]=NITAATSR
path_to_l1b[1]=${path_to_l1b[0]}
path_to_geo[1]=${path_to_l1b[0]}
if (( $short == 1 )); then startx[1]=1; else startx[1]=0 ; fi
endx[1]=512
starty[1]=37450
endy[1]=37454
daynight[1]=2

#---- AVHRR ----
sensor[2]=AVHRR
label[2]=AVHRR
path_to_l1b[2]=$infolder/noaa18_20080620_0050_99999_satproj_00000_13111_avhrr.h5
path_to_geo[2]=$infolder/noaa18_20080620_0050_99999_satproj_00000_13111_sunsatangles.h5
if (( $short == 1 )); then startx[2]=1; else startx[2]=0 ; fi
endx[2]=409
starty[2]=10150
endy[2]=10154
daynight[2]=0

#---- AQUA MODIS from LAADS (day) ----
sensor[3]=MODIS
label[3]=DAYMYD
path_to_l1b[3]=$infolder/MYD021KM.A2008172.0405.005.2009317014309.hdf
# if you don't want to check against LAADSweb full files, use this instead
#path_to_l1b[2]=$infolder/MYD021KM.A2008172.0405.005.2009317014309.bscs_000500531943.hdf
path_to_geo[3]=$infolder/MYD03.A2008172.0405.005.2009316101940.hdf
if (( $short == 1 )); then startx[3]=700; else startx[3]=0 ; fi
endx[3]=1299
starty[3]=1200
endy[3]=1204
daynight[3]=0

#---- AQUA MODIS from DWD (night) ----
sensor[4]=MODIS
label[4]=NITMYD
path_to_l1b[4]=$infolder/MYD021KM.A2008172.0405.005.2009317014309.bscs_000500531943.hdf
path_to_geo[4]=$infolder/MYD03.A2008172.0405.005.2009316101940.hdf
if (( $short == 1 )); then startx[4]=500; else startx[4]=0 ; fi
endx[4]=1099
starty[4]=900
endy[4]=904
daynight[4]=0

# add distinction for short mode to labels
if (( $short == 1 )); then for i in ${!label[*]}; do
   label[i]=${label[i]}S
done
fi

#------------------------------------------------------------------------------
# DEFINE FILE HEADER
#------------------------------------------------------------------------------

ncdf_version='3.6.3'
cf_convention='CF-1.4'
processing_inst='UoOx'
l2processor='ORAC'
contact_email='povey@atm.ox.ac.uk'
contact_website='www.esa-cloud-cci.info'
file_version='V1.0'
reference='ATBD'
history='xxx'
summary='xxx'
keywords='xxx'
comment='xxx'
#project='ESA_Cloud_cci' --- Being misused for file naming
license='xxx'

# 1:ecmwf grid, 2:L3 grid, 3: own definition
gridflag=1
# this is the inverse of the actual lat/lon increment
dellon=2.0
dellat=2.0

# break ATSR files into smaller chunks
chunkproc=0

#------------------------------------------------------------------------------
# RUN ORAC PREPROCESSOR
#------------------------------------------------------------------------------

#get uid and time information
uuid_tag=`exec uuidgen -t`
exec_time=`exec date +%Y%m%d%H%M%S`

#start the preprocessing and pass the variables to the binary on the command line
for lp in ${!sensor[*]}; do
   echo "Processing ${label[$lp]}"

   log_file=${outfolder}/${label[$lp]}_`date +"%y%m%d_%H%M"`.log
   echo 'UUID' $uuid_tag 1> $log_file
   echo '' 1>> $log_file
   echo "LD_LIBRARY_PATH=${LD_LIBRARY_PATH}" 1>> $log_file
   echo '' 1>> $log_file
   echo 'Do this:' 1>> $log_file

   # command line arguments changed at revision 1654
   if [[ "$revision" -ge 1654 ]]; then
       arg="${sensor[$lp]} ${path_to_l1b[$lp]} ${path_to_geo[$lp]} ${path_to_ecmwf} ${path_to_coeffs} ${path_to_emiss_atlas} ${path_to_ice} ${path_to_albedo} ${path_to_emissivity} ${gridflag} ${dellon} ${dellat} ${outfolder} ${startx[$lp]} ${endx[$lp]} ${starty[$lp]} ${endy[$lp]} ${ncdf_version} ${cf_convention} ${processing_inst} ${l2processor} ${revision} ${contact_email} ${contact_website} ${file_version} ${reference} ${history} ${summary} ${keywords} ${comment} ${label[$lp]} ${license} ${uuid_tag} ${exec_time} ${path_to_calib} ${badc_flag} ${path_to_ecmwf} ${path_to_ecmwf} ${chunkproc} ${daynight} ${verbose}"
   else
       arg="${sensor[$lp]} ${path_to_l1b[$lp]} ${path_to_geo[$lp]} ${path_to_ecmwf} ${path_to_coeffs} ${path_to_emiss_atlas} ${path_to_ice} ${path_to_albedo} ${path_to_emissivity} ${gridflag} ${dellon} ${dellat} ${outfolder} 0 ${startx[$lp]} ${endx[$lp]} ${starty[$lp]} ${endy[$lp]} ${ncdf_version} ${cf_convention} ${processing_inst} ${l2processor} ${revision} ${contact_email} ${contact_website} ${file_version} ${reference} ${history} ${summary} ${keywords} ${comment} ${label[$lp]} ${license} ${uuid_tag} ${exec_time} ${path_to_calib} ${badc_flag} ${path_to_ecmwf} ${path_to_ecmwf} ${chunkproc} ${daynight}"
   fi

   echo "${oracfolder}/orac_preproc.x" $arg 1>> $log_file
   echo '' 1>> $log_file

   sec=`date +"%s"`
   $oracfolder/orac_preproc.x $arg >> $log_file 2>&1
   if (( $? != 0 )); then
       echo "${label[$lp]}: Error."
       exit
   fi

   echo 'Processing took '$((`date +"%s"`-$sec))' s'
done

#------------------------------------------------------------------------------
# CHECK RESULTS
#------------------------------------------------------------------------------

# call IDL routine to compare this output to the previous version
if (( (! $first) && ($? == 0) )); then
    $idlfolder/idl -rt=$oracfolder/compare_preproc.sav -args $outfolder $revision
fi