#!/bin/bash
################################################################################
# make-prj.sh
# Script to create a directory structure and makefile for a small programming
# project (only for Fortran 90 at present)
################################################################################

##########################################################################################
# Functions
################################################################################

get_user_verification()
{
    ans=""
    while [ "$ans" != "y" ] && [ "$ans" != "n" ]
    do
	echo -n "$1 (y/n): "
	read ans
	if [ "$ans" != "y" ] && [ "$ans" != "n" ]
	then
	    echo "Please answer y or n."
	fi
    done
} <&1

check_last_cmd()
{
    if [ $? -ne 0 ]; then
	echo "An error occured during the last command. Exiting"
	exit 1
    fi
}

################################################################################
# Top-level logic
################################################################################

target=""
echo -n "Enter the desired name of your target/executable: "
read target


echo -n "Locating source files..."
SRCS=`find ./ -iname *.f90 | grep -v -i 'lapack' | sed -e 's/^\.\///' | tr '\n'\
          ' '`
check_last_cmd
SRCS=`echo $SRCS | sed -e 's/ +$//'`
check_last_cmd
echo -n "Found: $SRCS. "
get_user_verification "Is this correct?"
if [ $ans == "n" ]; then
    echo "Exiting!"
    exit
fi

echo -n "Checking for cg_flib dependence..."
CGFLIB=$(grep -i -c -E 'type_decls|ftest|util_io' $SRCS | grep -c '\:[1-9][0-9]*')
#check_last_cmd
if [ "$CGFLIB" != "0" ]; then
    echo "yes"
    get_user_verification "Include cg_flib from ~/Projects/cg_flib/?"
    if [ $ans == "n" ]; then
	CGFLIB="0"
    fi
else
    echo "no"
fi

echo -n "Creating directory structure..."
mkdir -p src build run doc
check_last_cmd
echo "done"

echo -n "Moving sources to src/..."
mkdir -p src
for file in $SRCS
do
    mv -f $file -t src 2>/dev/null
done
echo "done"

echo -n "Creating Makefile..."
OBJS=`echo $SRCS | tr ' ' '\n' | sed -e 's/.f90$/.o/' | \
    sed -e 's/^src\///' | tr '\n' ' '`
check_last_cmd

>Makefile
cat >> Makefile <<EOF
FC=gfortran
FCOPTS=-Wall -funroll-all-loops -O3 -march=native

EOF

if [ "$CGFLIB" -ge 1 ]; then
    cat >> Makefile <<EOF
################################################################################
# Libraries
################################################################################

CG_FLIB=\$(HOME)/Projects/cg_flib
INCLUDE=-I\$(CG_FLIB)
# NOTE: libs have to be included LAST when linking
LIBS=\$(CG_FLIB)/cg_flib.a

EOF
    check_last_cmd
else
    cat >> Makefile <<EOF
################################################################################
# Libraries
################################################################################

INCLUDE=
# NOTE: libs have to be included LAST when linking using gcc/gfortran
LIBS=

EOF
    check_last_cmd
fi

cat >> Makefile <<EOF
################################################################################
# Targets
################################################################################

vpath %.f90 src
vpath %.o build

all: $target

$target: ${OBJS} \$(LIBS)
	\$(FC) \$(FCOPTS) -o \$@ \$(patsubst %.o,build/%.o,\$+)

clean:
	rm -f *.o *.mod $target build/*.o build/*.mod

################################################################################
# object files and dependencies
################################################################################

# List dependencies here
EOF

for obj in $OBJS
do
    echo "${obj}: " >> Makefile
done

cat >> Makefile <<EOF

%.o: %.f90
	\$(FC) \$(FCOPTS) \$(INCLUDE) -c \$< -o build/\$@

EOF

echo "done"

echo ""
echo "Makefile created. Note, you may have to edit it to include dependencies"
echo "between object files."