#!/bin/bash
# Name:
#    postprocess.sh
#
# Purpose:
#    A shell script to run the ORAC plotting code on the test files in IDL.
#
# Calling sequence:
#    ./plot_orac.sh [-do DAYAATSR|NITAATSR|AATSR|DAYAVHRR|NITAVHRR|AVHRR|DAYMYD|
#       NITMYD|MYD|MODIS|DAY|NIT|NIGHT|ALL] [-first] [-n_procs NUM]
#       [-revision NUMBER] [-short]
#
# Arguments:
#    See header.sh for full details of arguments.
#
# Known issues:
#    None known.
#
# History:
# 2014/07/28, AP: Original version
#
set -e

source header.sh

#------------------------------------------------------------------------------
# RUN IDL PLOTTING COMMANDS
#------------------------------------------------------------------------------
orig=$PWD
cd $orac_repos/idl
for i in ${!label[*]}; do
#    for flag in '' ',/preproc' ',/comp' ',/preproc,/comp'; do
    for flag in '' ',/preproc'; do
        echo "PLOT_ORAC,\'${label[$i]}\',frame=${frame[$i]},$revision$flag"
    done
done | xargs -P 6 -n 1 -I COMMAND idl -quiet -e COMMAND
cd $orig