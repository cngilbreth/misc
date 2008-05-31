#!/bin/bash
################################################################################
# make-prj.sh
# Script to create a directory structure and makefile for a small programming
# project (only for Fortran 90 at present).
#
# Will create and move your files into the following directory structure:
#
#   ./			Containing target binary
#   ./src		Containing source .f90 and .f files
#   ./build		Containing .o files
#   ./mod   		Containing .mod files
#
#   ./lib               For you to put your external libraries and .mod's in
#          		(contents will be automatically linked to program)
#   ./doc		For documentation
#   ./run               For running the program & saving output
#
################################################################################

################################################################################
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
SRCS=`find ./ -iname *.f90 | grep -v -i -E '\./lib' | sed -e 's/^\.\///'\
         | tr '\n' ' '`
check_last_cmd
SRCS=`echo $SRCS | sed -e 's/ +$//'` # kill trailing space
check_last_cmd
echo -n "Found: $SRCS. "
get_user_verification "Is this correct?"
if [ $ans == "n" ]; then
    echo "Exiting!"
    exit
fi

echo -n "Creating directory structure..."
mkdir -p src build run doc mod
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
         tr '\n' ' '`
check_last_cmd

OBJS1=""
for file in $OBJS
do
    OBJS1="$OBJS1 `basename $file`"
done
OBJS="$OBJS1"

>Makefile
cat >> Makefile <<EOF
FC=gfortran
FCOPTS=-Wall -funroll-all-loops -O3 -march=native -Jmod

################################################################################
# Libraries
################################################################################

INCLUDE=-Ilib
# NOTE: libs have to be included LAST when linking using gcc/gfortran
LIBS=\$(shell /bin/ls lib/*.a 2>/dev/null)

################################################################################
# Targets
################################################################################

vpath %.f90 src
vpath %.o build

all: $target

$target: ${OBJS} \$(LIBS)
	\$(FC) \$(FCOPTS) -o \$@ \$+

clean:
	rm -f *.o *.mod $target build/*.o mod/*.mod

################################################################################
# object files and dependencies
################################################################################

# List dependencies here. (Note, you don't need to include the build/ prefix on
# object files after the ':'s.)
EOF
check_last_cmd

for obj in $OBJS
do
    echo "build/${obj}: " >> Makefile
done

cat >> Makefile <<EOF

build/%.o: %.f90
	\$(FC) \$(FCOPTS) \$(INCLUDE) -c \$< -o \$@
build/%.o: %.f
	\$(FC) \$(FCOPTS) \$(INCLUDE) -c \$< -o \$@

EOF
check_last_cmd

echo "done"

cat <<EOF

Note, one may have to edit the makefile to include dependencies between object
files and dependencies on any external libraries in lib/.
EOF
