#!/bin/bash

# Remove variables we set
for VAR in "${EMOSLIB_SET[@]}"; do
    unset "${VAR}"
done

# Remove tracking variables
unset EMOSLIB_SET
