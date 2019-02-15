#!/bin/bash

# Variables we need to declare for libemos to work
declare -a VARIABLES=(EMOSLIB_FILES
                      LOCAL_DEFINITION_TEMPLATES
                      ECMWF_LOCAL_TABLE_PATH
                      BUFR_TABLES)
declare -ax EMOSLIB_SET=()

# Use any variables that are already defined as a default and keep their value
for VAR in "${VARIABLES[@]}"; do
    if [ -d "${VAR}" ]; then
        DEFAULT="${VAR}"
    else
        EMOSLIB_SET+=("${VAR}")
    fi
done

# If nothing is set, assume the structure of a conda environment
if [ -z "${DEFAULT}" ]; then
    DEFAULT="${CONDA_PREFIX}/share/libemos/tables"
fi

# Set all remaining variables to the default value
for VAR in "${EMOSLIB_SET[@]}"; do
    if [ "${VAR}" == "EMOSLIB_FILES" ]; then
        export "${VAR}"="${DEFAULT}/interpol"
    else
        export "${VAR}"="${DEFAULT}"
    fi
done
