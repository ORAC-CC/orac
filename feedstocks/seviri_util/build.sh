#!/bin/bash

set -e

make -j ${CPU_COUNT} all

mv libseviri_util.a ${PREFIX}/lib
mv seviri_util.mod ${PREFIX}/include
