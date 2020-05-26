#!/bin/bash
################################################################################
# eetags.sh: tiny script to run etags
################################################################################

# Fortran
files1=`find . -iname '*.f90' -or -iname '*.F90' -or -iname '*.f' -or \
    -iname '*.F' | grep -v '\.asciifiles' | tr '\n' ' '`

# Python
files2=`find . -iname '*.py' | grep -v '\.asciifiles' | tr '\n' ' '`

# C, C++
files3=`find . -iname '*.c' -or -iname '*.cpp' -or -iname '*.c++' \
    -or -iname '*.cxx' | grep -v '\.asciifiles' | tr '\n' ' '`

etags $files2 $files3 $files1
