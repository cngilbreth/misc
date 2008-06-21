#!/bin/bash
################################################################################
# eetags.sh: tiny script to run etags
################################################################################

# Fortran
files1=`find . -iname '*.f90' -or -iname '*.f' | tr '\n' ' '`

# Python
files2=`find . -iname '*.py' | tr '\n' ' '`

etags $files1 $files2