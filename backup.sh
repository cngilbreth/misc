#!/bin/bash
# backup.sh -- backup directories to remote storage

#SOURCES="/Users/cng8/bin /Users/cng8/Documents /Users/cng8/.Xmodmap"
#VAULT="macbook"

SOURCES="/home/posco/Desktop /home/posco/bin /home/posco/Projects /home/posco/Documents \
/home/posco/.TeXmacs /home/posco/.emacs /home/posco/.bashrc /home/posco/.bash_profile"
VAULT="autumn"

REMOTE_PATH="8156@usw-s008.rsync.net:/data2/home/8156/backups"

RBACKUP="`which rbackup.sh`"
if [ -z $RBACKUP ]
then
    echo "Can't find rbackup.sh in PATH. Exiting."
    exit 1
fi

LOG="$HOME/backup_log.txt"

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

