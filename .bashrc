# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples
# Customized by CG

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi


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

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}: ${PWD/$HOME/~}\007"'
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ "$TERM" != "dumb" ]; then
    test ! -z `which dircolors` && eval "`dircolors -b`"
    if [ "`uname`" == "Linux" ]; then
	alias ls='ls --color=auto -a'
    elif [ "`uname`" == "Darwin" ]; then
	export CLICOLOR=1
	alias ls='ls -a'
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

#if [ -f ~/.bash_aliases ]; then
#    . ~/.bash_aliases
#fi

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
    PYTHONPATH="$PYTHONPATH:$HOME/Projects/cg_pylib"
fi


# for Bethe
if [ -d "home/chris/local/TeXmacs-1.0.6-static-gnu-linux" ]; then
    export TEXMACS_PATH=/home/chris/local/TeXmacs-1.0.6-static-gnu-linux
    export PATH=$TEXMACS_PATH/bin:$PATH
fi

test -r /sw/bin/init.sh && . /sw/bin/init.sh

################################################################################
# Intel Fortran Compiler stuff
################################################################################

test -r /usr/local/intel/fc/10.1.008/bin/ifortvars.sh && \
    . /usr/local/intel/fc/10.1.008/bin/ifortvars.sh

test -r /usr/local/intel/idb/10.1.008/bin/idbvars.sh && \
    . /usr/local/intel/idb/10.1.008/bin/idbvars.sh

test -r /opt/intel/fc/10.1.014/bin/ifortvars.sh && \
    . /opt/intel/fc/10.1.014/bin/ifortvars.sh

export PATH
export PYTHONPATH

xset b off
