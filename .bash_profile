# ~/.bash_profile: executed by bash(1) for login shells.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/login.defs
#umask 022

# include .bashrc if it exists
if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi


################################################################################
# Intel Fortran Compiler stuff

. /usr/local/intel/fc/10.1.008/bin/ifortvars.sh
. /usr/local/intel/idb/10.1.008/bin/idbvars.sh


