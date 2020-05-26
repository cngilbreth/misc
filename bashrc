# ~/.bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
        . /etc/bashrc
fi

export PATH=${HOME}/Bin:${HOME}/Projects/pn-int/src:$PATH
export CDPATH=${CDPATH}:${HOME}/Data:${HOME}/Data/qproj

# TeXmacs
export TEXMACS_PATH=/usr/local/src/TeXmacs-1.99.2-8683M-i386-pc-linux-gnu
export PATH=$TEXMACS_PATH/bin:$PATH

# User specific aliases and functions
export LANG=en_US.UTF-8

# Enable extended bash globs!! Yes!!
shopt -s extglob
#
# Reference:
#
# ?(pattern-list)
#   Matches zero or one occurrence of the given patterns.
# *(pattern-list)
#   Matches zero or more occurrences of the given patterns.
# +(pattern-list)
#   Matches one or more occurrences of the given patterns.
# @(pattern-list)
#   Matches one of the given patterns.
# !(pattern-list)
#   Matches anything except one of the given patterns.
#
# Patterns in a list are separated by | characters.
#
# Examples:
#
#  To remove all the files except ones matching *.jpg:
#    $ rm !(*.jpg)
#  All except *.jpg and *.gif and *.png:
#    $ rm !(*.jpg|*.gif|*.png)
#  To copy all the MP3 songs except one to your device
#    $ cp !(04*).mp3 /mnt

#ulimit -s unlimited
export OMP_NUM_THREADS=4
export OMP_STACKSIZE=4G

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


# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# Alias definitions.
if [ -f ~/.bash_alias ]; then
    . ~/.bash_alias
fi


# PATHS

if [ -d /usr/local/hdf5 ]; then
   PATH="/usr/local/hdf5/bin:$PATH"
fi

if [ -d $HOME/local/bin ] ; then
    PATH="$HOME/local/bin:${PATH}"
fi

if [ -d $HOME/bin ] ; then
    PATH="$HOME/bin:${PATH}"
fi

if [ -d $HOME/Projects/mn-tm/tools ] ; then
    PATH="$HOME/Projects/mn-tm/tools:$PATH"
fi

if [ -d $HOME/Projects/luxft/bin ] ; then
    PATH="$HOME/Projects/luxft/bin:$PATH"
fi


# set PYTHONPATH to include my stuff
if [ -d $HOME/Projects/cg_pylib ] ; then
    PYTHONPATH="$PYTHONPATH:$HOME/Projects/pylib:$HOME/Documents/Research/coldatom/code"
fi

export INTDIR="${HOME}/Data/interactions"
export SPSDIR="${HOME}/Data/interactions/sps"

export PATH

# OpenMP
#export OMP_NUM_THREADS=1
#export KMP_AFFINITY=physical
#export OMP_SCHEDULE="guided"
#export OMP_STACKSIZE=1G


# An enhanced 'cd' - push directories
# onto a stack as you navigate to it.
#
# The current directory is at the top
# of the stack.
#
function stack_cd {
    if [ -n "$1" ]; then
        # use the pushd bash command to push the directory
        # to the top of the stack, and enter that directory
        pushd "$1" > /dev/null
    else
        # the normal cd behavior is to enter $HOME if no
        # arguments are specified
        pushd $HOME > /dev/null
    fi
}
# the cd command is now an alias to the stack_cd function
#
alias cd=stack_cd

# Swap the top two directories on the stack
#
function swap {
    pushd > /dev/null
}
# s is an alias to the swap function
#alias s=swap

# Pop the top (current) directory off the stack
# and move to the next directory
#
function pop_stack {
    popd > /dev/null
}
alias p=pop_stack

# Display the stack of directories and prompt
# the user for an entry.
#
# If the user enters 'p', pop the stack.
# If the user enters a number, move that
# directory to the top of the stack
# If the user enters 'q', don't do anything.
#
function display_stack
{
    dirs -v
    echo -n "#: "
    read dir
    if [[ "$dir" = 'p' ]]; then
        pushd > /dev/null
    elif [ "$dir" != 'q' ] && [ ! -z "$dir" ]; then
        d=$(dirs -l +$dir);
        popd +$dir > /dev/null
        pushd "$d" > /dev/null
    fi
}
alias d=display_stack


alias tunnel="ssh -D 8080 chris@newton.physics.yale.edu"
