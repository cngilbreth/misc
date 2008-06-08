#!/bin/bash
# rbackup-remove.sh
# Copyright (C) 2007 Chris Gilbreth
VER=0.1

################################################################################
# Process command line options
################################################################################

name=`basename $0`

if [ $# -lt 1 ]
then
    cat <<EOF
$name version $VER
Copyright (C) 2007 Chris Gilbreth

Usage:  $name -v <vaultname> <N> user@host:/path/to/backups

Where <N> is the number of most recent backups to keep.

Backups will be removed from
    user@host:/path/to/backups/vaultname/ .

EOF
    exit 1
fi

while getopts "v:" option
do
    case $option in
	v ) VAULT=$OPTARG
    esac
done

shift $(($OPTIND - 1))

backups=$1
when="before"

DEST="$2"
DESTHOST="`echo $DEST | cut -d : -f 1`"
BACKDIR="`echo $DEST | cut -d : -f 2`"

# REPLACE WITH SCRIPT PARAMS
echo ""
echo "Host: $DESTHOST"
echo "Directory: $BACKDIR"
echo "Vault: $VAULT"
echo "Number of backups to keep: $backups"
echo ""


################################################################################
# Create the list of backups to remove
################################################################################

# list them all, then sort by date

ssh $DESTHOST ls $BACKDIR/$VAULT/ | sed -e '/^latest$/d' > /tmp/rblist.txt

# E.g., /tmp/rblist.txt first has:
#
# autumn.Fri_Dec_07_11:37:53_EST_2007
# autumn.Mon_Nov_19_21:22:47_EST_2007
# autumn.Sun_Jan_06_21:19:00_EST_2008
#
# Process to become (ignoring time zones for now):
# year mo d  time     zn  filename
# 2008 01 06 21:19:00 EST autumn.Sun_Jan_06_21:19:00_EST_2008
# 2007 12 07 11:37:53 EST autumn.Fri_Dec_07_11:37:53_EST_2007
# 2007 11 19 21:22:47 EST autumn.Mon_Nov_19_21:22:47_EST_2007
#
# This should be the proper order for sorting.
#
# date '+%Y %m %d %H:%M:%S %Z' gives the current date/time in the same date
# formatting as above.

>/tmp/rblist1.txt
line=""
{
    read line

    while [ ! -z "$line" ]
    do
	bdate=`echo $line | sed -e "s/^$VAULT\\.//" | awk -F _ '{print $6 " " $2 " " $3 " " $4 " " $5}' \
	    |  sed -e "s/Jan/01/" -e "s/Feb/02/" -e "s/Mar/03/" -e "s/Apr/04/" -e "s/May/05/" \
	    -e "s/Jun/06/" -e "s/Jul/07/" -e "s/Aug/08/" -e "s/Sep/09/" -e "s/Oct/10/" -e "s/Nov/11/" -e "s/Dec/12/"`
	echo "$bdate $line" >> /tmp/rblist1.txt
	read line
    done
} < /tmp/rblist.txt


sort -r /tmp/rblist1.txt > /tmp/rblist.txt

# I'm going to ignore the timezones, at least for now.

if [ ! -z "$backups" ]
then
    if [ "$when" == "before" ]
    then
	let backups=backups+1
	# backups to remove:
	tail -n "+${backups}" /tmp/rblist.txt | awk '{print $6}' > /tmp/rblist1.txt
    else
	echo "after functionality not implemented yet, sorry"
	exit 1
    fi
else
    echo "Date functionality not implemented yet. Use -b."
    exit 1
fi


################################################################################
# get_user_verification
################################################################################

get_user_verification()
{
    ans=""
    while [ "$ans" != "y" ] && [ "$ans" != "n" ] && [ "$ans" != "YY" ]
    do
	echo -n "Are you sure you want to continue? (y/n/YY='yes to all'): "
	read ans
	if [ "$ans" != "y" ] && [ "$ans" != "n" ] && [ "$ans" != "YY" ]
	then
	    echo "Please answer y, n, or YY."
	fi
    done
} <&1


################################################################################
# remove the backups
################################################################################

line=""
{
    read line
    while [ ! -z "$line" ]
    do
	echo "About to delete: $BACKDIR/$VAULT/$line"
	
	
	if [ "$ans" != "YY" ]; then
	    ans="n"
	    get_user_verification
	fi
	
	if [ "$ans" == "y" ] || [ "$ans" == "YY" ]
	then
	    echo "Deleting..."
	    # </dev/null required to keep ssh from grabbing input from the file
	    ssh $DESTHOST "rm -rf $BACKDIR/$VAULT/$line" </dev/null
	    if [ $? -ne 0 ]
	    then
		echo "ERROR deleting $BACKDIR/$VAULT/$line !!"
	    fi
	else
	    echo "Skipping..."
	fi
	read line
    done
} < /tmp/rblist1.txt

