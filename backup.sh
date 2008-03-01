#!/bin/bash
# backup.sh -- backup directories to remote storage

################################################################################
# Process command line args
################################################################################

REMOVE=0

if [ "$1" == "remove" ]
then
    REMOVE=1
elif [ $# -gt 0 ]
then
    echo "Unknown command line arguments."
    exit 1
fi


################################################################################
# Set up hostname, sources, etc.
# Insert machine-specific config here.
################################################################################

HOST=`hostname`

if [ "$HOST" == "Physics-06-15" ]
then
    SOURCES="/Users/cng8/bin /Users/cng8/Documents /Users/cng8/.Xmodmap"
    VAULT="macbook"
elif [ "$HOST" == "Autumn" ]
then
    SOURCES="/home/posco/Desktop /home/posco/bin /home/posco/Projects \
/home/posco/Documents /home/posco/.TeXmacs /home/posco/.emacs \
/home/posco/.bashrc /home/posco/.bash_profile"
    VAULT="autumn"
else
    echo "No configuration found for this host."
    exit 1
fi

REMOTE_PATH="8156@usw-s008.rsync.net:/data2/home/8156/backups"


################################################################################
# Backup or remove the extras
################################################################################

RBACKUP="`which rbackup.sh`"
if [ -z $RBACKUP ]
then
    echo "Can't find rbackup.sh in PATH. Exiting."
    exit 1
fi

LOG="$HOME/backup_log.txt"

backup()
{
    # Redirect stdout and stderr to file
    exec 3>&2 # save stderr to file descriptor 3
    exec 1>>$LOG # stdout
    exec 2>>$LOG # stderr
    
    echo "************************************************************"
    echo "* Backup started: `date`"
    echo "************************************************************"
    echo ""
    
    echo "Files/directories/globs to backup:"
    echo $SOURCES
    
    # do the backup
    
    echo ""
    echo "Running rbackup.sh..."
    echo "Running rbackup.sh..." >&3
    
    $RBACKUP -v $VAULT $SOURCES $REMOTE_PATH
    
    if [ $? -ne 0 ]
    then
	result="FAILED"
	echo "ERROR: rbackup failed. See $LOG for more info." >&3
    else
	result="finished"
	echo "rbackup finished."
    fi
    
    echo "************************************************************"
    echo "* Backup $result: `date`"
    echo "************************************************************"
    echo ""
}

remove_extras()
{
    rbackup-remove.sh -v $VAULT 6 $REMOTE_PATH
}


if [ $REMOVE -eq 0 ]
then
    backup
elif [ $REMOVE -eq 1 ]
then
    remove_extras
fi


    