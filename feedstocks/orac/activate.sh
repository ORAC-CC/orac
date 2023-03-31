#!/bin/bash

# Put the EMOS temporary file somewhere sensible
if [ -z "${PPDIR}" ]; then
    export PPDIR="${CONDA_PREFIX}/share/libemos/tables"
fi

# Ensure multithreaded packages don't multithread BLAS
if [ -n "${OPENBLAS_NUM_THREADS}" ]; then
    export OPENBLAS_NUM_THREADS_SAVE="${OPENBLAS_NUM_THREADS}"
fi
export OPENBLAS_NUM_THREADS=1
