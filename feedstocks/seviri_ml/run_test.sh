#!/bin/bash

set -ex

test -f ${PREFIX}/lib/libsevann.a

# Do tests
export SEVIRI_ML_BACKEND="TENSORFLOW2"
cd test
seviri-ml_ctest
seviri-ml_ftest
${PYTHON} py_test.py
