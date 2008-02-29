#!/bin/bash
# rbackup.sh
# Copyright (C) 2007 Chris Gilbreth
#
VER=0.5.1  # Dec 7, 2007

# The RSYNC environment variable should be exported before running
# this script if the rsync path needs to be specified.
RSYNC=${RSYNC:-`which rsync`}

# Terminology: 
# 
# vault: A "vault" is a named collection of source directories or
# files ("sources") to be backed up.  It is the basic unit of
# backup. When backing up, a dated vault directory ("vault snapshot")
# will be created containing the backups of all your sources.
#
# backup directory: The directory containing the vault snapshots.

# QUESTION: How can I avoid calling ssh so many times?



################################################################################
# Process command line options
################################################################################

name=`basename $0`

if [ $# -lt 2 ]
then
    cat <<EOF
$name version $VER
Copyright (C) 2007 Chris Gilbreth

Usage:  $name [ -v <vaultname> ] source1 [source2 source3 ...] user@host:/path/to/backup

Backups will be stored as:
    /path/to/backup/<vaultname>/<vaultname>.<date and time>
on the destination host.

Notes: 
    - If no vaultname is specified, it defaults to the basename of
    the source. A vaultname is required if more than one source is
    present.

    - Be sure you have enabled passwordless ssh before using this
    script. Otherwise, you will have to type your password several
    times.

    - You need to include a full path for the destination directory
    (i.e. it must start with a '/').

    - If you include shell globs in your sources (e.g. *.*), be sure
    to include them in single quotes: '/home/me/*.*'. This is to make
    rsync expand the globs and take care of spaces, etc. correctly.

Examples:
    $name /home/me/Documents me@backup-server.net:/backups
    $name -v laptop Documents 'files/*.doc' me@backup-server.net:/backups

EOF
    exit
fi

while getopts "v:" option
do
    case $option in
	v ) VAULT=$OPTARG
    esac
done

shift $(($OPTIND - 1))

num=$#

SOURCES="$1"
shift
for ((i=2; i < num; i++))
do
    SOURCES="$SOURCES $1"
    shift
done

DEST="$1"
DESTHOST="`echo $DEST | cut -d : -f 1`"
BACKDIR="`echo $DEST | cut -d : -f 2`"

if [ -z "$VAULT" ] && [ $num -gt 2 ]
then
    echo "Error: must specify a vault name using the -v option when more"
    echo "than one source is specified."
    exit
fi

# For only one source, set the vault name to the basename of the
# source.
if [ -z "$VAULT" ] && [ $num -eq 2 ]
then
    VAULT="`basename \"$SOURCES\"`"
fi

################################################################################
# Set up some variables and do the rsync
################################################################################

SSH="ssh -T -C $DESTHOST"

BACKDIR="$BACKDIR/$VAULT"
$SSH mkdir -p "$BACKDIR"

DEST="$DEST/$VAULT/$NEW"
NEW="$VAULT.`date +'%a_%b_%d_%H:%M:%S_%Z_%Y'`" # new snapshot name
#OLD="`$SSH readlink $BACKDIR/latest`"          # last snapshot name
OLD="`$SSH ls -l $BACKDIR/latest | awk -F " -> " '{ print $2 }'`"

if [ ! -z "$OLD" ]
then
    OLD="`basename $OLD`"  # for symlinks which have the path embedded
fi

echo "OLD: $OLD"

DEST="$DESTHOST:$BACKDIR/$NEW"     # new snapshot location
OLDDEST="$DESTHOST:$BACKDIR/$OLD"  # last snapshot location


if [ -z "$OLD" ]
then
    LINKDESTOPT=""
else
    LINKDESTOPT="--link-dest=$BACKDIR/$OLD"
fi

# rsync
$RSYNC -avz --delete -e ssh $LINKDESTOPT $SOURCES "$DEST"

if [ $? -ne 0 ]
then
    echo "Error: rsync failed. Exiting" >&2
    exit 1
fi

# make the "latest" symlink point to the backup we just did
$SSH rm -f "$BACKDIR/latest" 
$SSH ln -s "$BACKDIR/$NEW" "$BACKDIR/latest"


