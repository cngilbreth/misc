# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples
# Customized by CG

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

ulimit -s unlimited

# If not running interactively, don't do anything
#[ -z "$PS1" ] && return

################################################################################
# Bash history
################################################################################

# don't put duplicate lines in the history. See bash(1) for more options
export HISTCONTROL=ignoredups


# configure the BASH history features.
# Notes: typing ctrl-r will give backward incremental search
#        typing history | grep blah will search the history for blah
#        typing !123   will execute history command 123
#        typing !s<enter> will execute the last command starting with s.
export HISTIGNORE="&:ls:[bf]g:exit:ls -l: ls -la"


# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

################################################################################
# Colors, prompt and xterm
################################################################################

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# set a fancy prompt (non-color, unless we know we "want" color)
# case "$TERM" in
# xterm-color)
#     PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
#     ;;
# *)
#     PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
#     ;;
# esac

# Comment in the above and uncomment this below for a color prompt
PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\W\[\033[00m\]\$ '


# enable color support of ls and also add handy aliases
if [ "$TERM" != "dumb" ]; then
    test ! -z `which dircolors` && eval "`dircolors -b`"
    if [ "`uname`" == "Linux" ]; then
	alias ls='ls --color=auto'
    elif [ "`uname`" == "Darwin" ]; then
	export CLICOLOR=1
	alias ls='ls'
    fi

    #alias dir='ls --color=auto --format=vertical'
    #alias vdir='ls --color=auto --format=long'
fi


################################################################################
# Alias definitions.
################################################################################

# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_alias ]; then
    . ~/.bash_alias
fi

alias mv="mv -i"

# some more ls aliases
#alias ll='ls -l'
#alias la='ls -A'
#alias l='ls -CF'

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).

# CG: The following statement is seems to mess up the script.
# Nothing gets exececuted after this. Commented out.
#if [ -f /etc/bash_completion ]; then
#    source /etc/bash_completion
#fi

alias gopen='gnome-open'  # like 'open' on a mac

alias du0='du -h --max-depth 0 ./'
alias du1='du -h --max-depth 1 ./'
alias du2='du -h --max-depth 2 ./'

alias bdj='ssh cng8@bulldogj.wss.yale.edu'
alias bdh='ssh cng8@bulldogh.wss.yale.edu'
alias cmb='ssh cng8@combustion.eng.yale.edu'
alias frk='ssh gilbreth@franklin.nersc.gov'

alias rmm='gvfs-trash'

alias ec="emacsclient -c -n -a ''"

################################################################################
# PATHS
################################################################################

if [ -d $HOME/local/bin ] ; then
    PATH="$HOME/local/bin:${PATH}"
fi

if [ -d $HOME/bin ] ; then
    PATH="$HOME/bin:${PATH}"
fi

if [ -d $HOME/Projects/mn-tm/tools ] ; then
    PATH="$HOME/Projects/mn-tm/tools:$PATH"
fi


# set PYTHONPATH to include my stuff
if [ -d $HOME/Projects/cg_pylib ] ; then
    PYTHONPATH="$PYTHONPATH:$HOME/Projects/cg_pylib:$HOME/Documents/Research/coldatom/code"
fi


# for Bethe
if [ -d "home/chris/local/TeXmacs-1.0.6-static-gnu-linux" ]; then
    export TEXMACS_PATH=/home/chris/local/TeXmacs-1.0.6-static-gnu-linux
    export PATH=$TEXMACS_PATH/bin:$PATH
fi

test -r /sw/bin/init.sh && . /sw/bin/init.sh

export INTDIR="/home/chris/data/interactions/new_interactions"
export SPSDIR="/home/chris/data/interactions/sps"


################################################################################
# Intel Fortran Compiler stuff -- now in /etc/profile.d/intel-compilers.sh
################################################################################

# IDB_BUILD=10.1.015
# IDB_DIR=/opt/intel/idbe/$IDB_BUILD
# test -r $IDB_DIR/bin/idbvars.sh && . $IDB_DIR/bin/idbvars.sh

# IFORT_BUILD=10.1.015
# IFORT_DIR=/opt/intel/fce/$IFORT_BUILD
# test -r $IFORT_DIR/bin/ifortvars.sh && . $IFORT_DIR/bin/ifortvars.sh

# ICC_BUILD=10.1.015
# ICC_DIR=/opt/intel/cce/$ICC_BUILD
# test -r $ICC_DIR/bin/ifortvars.sh && . $ICC_DIR/bin/ifortvars.sh

#test -r /opt/intel/Compiler/11.0/083/bin/ifortvars.sh && \
#    . /opt/intel/Compiler/11.0/083/bin/ifortvars.sh intel64


################################################################################
# MPICH2 stuff
################################################################################

# I have two MPICH2 installations in Winter: one for gfortran, one for ifort.

#MPI_PATH=/usr/local/mpich2_intel
#LD_LIBRARY_PATH=/usr/local/openmpi_gnu/lib:$LD_LIBRARY_PATH
# LAPACK_PATH=${HOME}/local/lib/lapack_gnu
PATH="${MPI_PATH}/bin:$PATH"

# alias mpif90g=/usr/local/mpich2_gnu/bin/mpif90
# alias mpif90i=/usr/local/mpich2_ifort/bin/mpif90

# PATH="/home/chris/local/toolworks/totalview.8.6.0-3/bin/:$PATH"

# Now using Intel MPI library

export PATH
export PYTHONPATH

#xset b off

################################################################################
# OpenMP
################################################################################

export OMP_NUM_THREADS=2
export OMP_SCHEDULE="guided"
export OMP_STACKSIZE=1G
