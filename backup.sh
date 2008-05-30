#!/bin/bash
# backup.sh -- backup directories to remote storage

################################################################################
# Process command line args
################################################################################

ACTION="backup"

if [ "$1" == "remove" ]
then
    ACTION="remove"
elif [ "$1" == "quota" ]
then
    ACTION="quota"
elif [ "$1" == "ls" ]
then
    ACTION="ls"
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
    SOURCES="/Users/cng8/bin /Users/cng8/Documents /Users/cng8/.Xmodmap \
/Users/cng8/Projects"
    VAULT="macbook"

elif [ "$HOST" == "Autumn" ]
then
    SOURCES="/home/posco/Desktop /home/posco/bin /home/posco/Projects \
/home/posco/Documents /home/posco/.TeXmacs /home/posco/.emacs \
/home/posco/.bashrc /home/posco/.bash_profile"
    VAULT="autumn"

elif [ "$HOST" == "bethe.physics.yale.edu" ]
then
    SOURCES="/home/chris/Documents /home/chris/Archive /home/chris/Projects \
/home/chris/oxbash"
    VAULT="bethe"

else
    echo "No configuration found for this host."
    exit 1
fi

REMOTE_USER="8156"
REMOTE_HOST="usw-s008.rsync.net"
REMOTE_PATH="/data2/home/8156/backups"
REMOTE_DEST="$REMOTE_USER@$REMOTE_HOST:$REMOTE_PATH"
# e.g. me@backup-site.net:/home/mybackups


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
    
    $RBACKUP -v $VAULT $SOURCES $REMOTE_DEST
    
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
    rbackup-remove.sh -v $VAULT 6 $REMOTE_DEST
}

list_backups()
{
    ssh $REMOTE_USER@$REMOTE_HOST ls -l "$REMOTE_PATH/$VAULT/"
}


if [ "$ACTION" == "backup" ]
then
    backup
elif [ "$ACTION" == "remove" ]
then
    remove_extras
elif [ "$ACTION" == "quota" ]
then
    ssh "$REMOTE_USER@$REMOTE_HOST" quota
elif [ "$ACTION" == "ls" ]
then
    list_backups
fi

