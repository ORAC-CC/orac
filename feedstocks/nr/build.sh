#!/bin/bash

set -e

make -j ${CPU_COUNT}
mv libnr.a ${PREFIX}/lib
