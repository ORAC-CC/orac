#!/bin/bash
# test_orac.sh
#
# A shell script to run the ORAC processor on the test files and compare
# the results to a previous version. The directories in the first section
# should be set to represent your local file system.
#
# Command line arguments -
# short = set to only process a few lines from each file
# first = set to indicate that you are processing the current version
#         without alteration and do not need to increase the revision number
#
# History -
# 2004/01/24: AP Original version
#

#------------------------------------------------------------------------------
# DEFINE LOCAL FOLDERS
#------------------------------------------------------------------------------
# path to ORAC preprocessor executable
oracfolder=/home/jupiter/eodg2/povey/orac/trunk/src

# path to this file
toolfolder=/home/jupiter/eodg2/povey/orac/trunk/tools

# path to IDL executable
idlfolder=/usr/local/PACK/idl82/idl82/bin

# path to ORAC preprocessor output
datafolder=/home/midi/eodg2/data/testoutput

# path to SAD files
sadfolder=/home/blt/orac_sad/sad_dir

#------------------------------------------------------------------------------
# SET NECESSARY PARAMETERS
#------------------------------------------------------------------------------
# deal with command arguments
if [[ "$1" == 'first' || "$2" == 'first' ]]; then first=1; else first=0; fi
if [[ "$1" == 'short' || "$2" == 'short' ]]; then short=1; else short=0; fi

sensor[0]=AATSR
sensor[1]=AATSR
#sensor[2]=AVHRR
#sensor[3]=MODIS
#sensor[4]=MODIS
sensor[2]=MODIS-AQUA
sensor[3]=MODIS-AQUA

label[0]=DAYAATSR
label[1]=NITAATSR
#label[2]=AVHRR
#label[3]=DAYMYD
#label[4]=NITMYD
label[2]=DAYMYD
label[3]=NITMYD

# add distinction for short mode to labels
if (( $short == 1 )); then for i in ${!label[*]}; do
   label[i]=${label[i]}S
done
fi

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

# load library paths from Makefile lib file
# 1) Read contents of lib file, whose location is given by $ORAC_LIB
lib_contents=$(<${oracfolder}/${ORAC_LIB})
# 2) Replace round braces with curly braces
lib_commands=`echo "$lib_contents" | sed -e 's/(/\{/g' -e 's/)/\}/g'`
# 3) Make a string that does everything before defining the variable $LIBS
str='LIBS='
not_libs="${lib_commands%%$str*}"
# 4) Evaluate those lines
eval "${lib_commands:0:${#not_libs}}"
# 5) Now all of those variables are defined here so we can build LD_LIBRARY_PATH
LD_LIBRARY_PATH=${EPR_APILIB}:${LD_LIBRARY_PATH}
LD_LIBRARY_PATH=${GRIBLIB}:${LD_LIBRARY_PATH}
LD_LIBRARY_PATH=${HDFLIB}:${LD_LIBRARY_PATH}
LD_LIBRARY_PATH=${HDF5LIB}:${LD_LIBRARY_PATH}
LD_LIBRARY_PATH=${NCDFLIB}:${LD_LIBRARY_PATH}
LD_LIBRARY_PATH=${NCDF_FORTRAN_LIB}:${LD_LIBRARY_PATH}
LD_LIBRARY_PATH=${SZLIB}:${LD_LIBRARY_PATH}
export LD_LIBRARY_PATH

#------------------------------------------------------------------------------
# RUN ORAC PROCESSOR
#------------------------------------------------------------------------------

# generate a driver file and start the processing
driver_file=$oracfolder/test_driver.txt
for lp in ${!sensor[*]}; do
   echo "Processing ${label[$lp]}"

   # find root file name
   alb=`find $datafolder -name "${label[$lp]}_*ORACV${revision}*alb.nc" -printf "%f\n"`
   fileroot=${alb:0:$((${#alb}-7))}

   # write driver file
   driver="'$datafolder'
'$fileroot'
'$datafolder'
'$sadfolder'
${sensor[$lp]}
6
1 1 1 1 1 1
WAT"
   echo "$driver" 1> $driver_file

   # make header for log file
   log_file=${datafolder}/${label[$lp]}_'ORAC'_`date +"%y%m%d_%H%M"`.log
   echo 'UUID' $uuid_tag 1> $log_file
   echo '' 1>> $log_file
   echo "LD_LIBRARY_PATH=${LD_LIBRARY_PATH}" 1>> $log_file
   echo '' 1>> $log_file
   echo 'DRIVER FILE:' 1>> $log_file
   echo "$driver" 1>> $log_file
   echo '' 1>> $log_file
   echo 'Do this:' 1>> $log_file
   echo "${oracfolder}/orac" $driver_file 1>> $log_file
   echo '' 1>> $log_file

   sec=`date +"%s"`
   $oracfolder/orac $driver_file >> $log_file 2>&1
   if (( $? != 0 )); then
       echo "${label[$lp]}: Error."
       exit
   fi

   rm -f $driver_file

   echo 'Processing took '$((`date +"%s"`-$sec))' s'
done

#------------------------------------------------------------------------------
# CHECK RESULTS
#------------------------------------------------------------------------------

# call IDL routine to compare this output to the previous version
if (( (! $first) && ($? == 0) )); then
    $idlfolder/idl -rt=$toolfolder/compare_orac_out.sav -args $datafolder $revision 'main'
fi