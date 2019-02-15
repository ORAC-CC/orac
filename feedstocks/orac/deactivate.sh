#!/bin/bash

export LD_LIBRARY_PATH="${LD_LIBRARY_PATH_SAVE}"
unset LD_LIBRARY_PATH_SAVE

if [ "${PPDIR}" == "${CONDA_PREFIX}/share/libemos/tables" ]; then
    unset PPDIR
fi

if [ -n "${OPENBLAS_NUM_THREADS_SAVE}" ]; then
    export OPENBLAS_NUM_THREADS="${OPENBLAS_NUM_THREADS_SAVE}"
    unset OPENBLAS_NUM_THREADS_SAVE
else
    unset OPENBLAS_NUM_THREADS
fi
