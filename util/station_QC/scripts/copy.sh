#!/bin/sh
################################################################################
# This script copies restart files.
# Usage: copy.sh
# Imported variables:
#   CONFIG
#   CDATE
#   CDUMP
#   CSTEP
# Configuration variables:
################################################################################
set -ux
export VERBOSE=YES

################################################################################
# Go configure

set -a;. $CONFIG;set +a
export CKSH=$(echo $CSTEP|cut -c-4)
export CKND=$(echo $CSTEP|cut -c5-)
eval export DATA=$DATATMP
eval export COMCOP=$COMCOP
cd;rm -rf $DATA||exit 1;mkdir -p $DATA||exit 1;cd $DATA||exit 1
export PBEG=${PBEG:-$SHDIR/pbeg}
export PEND=${PEND:-$SHDIR/pend}
export PERR=${PERR:-$SHDIR/perr}
$PBEG

################################################################################
# Set other variables

export CKND=${CKND:-fcst1}
export CSTEP=$CKSH$CKND
export PMKR=${PMKR:-$SHDIR/pmkr}
export PCOP=${PCOP:-$SHDIR/pcop}
export COPYCH=${COPYCH:-YES}
export CHGRESSH=${CHGRESSH:-/nwprod/ush/global_chgres.sh}

################################################################################
# Make Rlist if it's missing

[[ -s $RLIST ]]||$PMKR >$RLIST

################################################################################
# Copy in restart and input files

$PCOP $CDATE/$CDUMP/$CKND/COPI $COMCOP $DATA <$RLIST
rc=$?
if [[ $rc -ne 0 ]];then $PERR;exit 1;fi

################################################################################
# Change resolution if necessary

if [[ $CKND = fcst* ]];then
  if [[ $COPYCH = YES ]];then
    export SIGINP=sighir.$CDUMP.$CDATE
    export SFCINP=sfchir.$CDUMP.$CDATE
    export SIGOUT=siganl.$CDUMP.$CDATE
    export SFCOUT=sfcanl.$CDUMP.$CDATE
    mv $SIGOUT $SIGINP
    mv $SFCOUT $SFCINP
    if [[ $CDATE -lt 2006082300 ]]; then 
    export LANDICE_OPT=1
    fi
    export IVSSFC=200509
    $CHGRESSH
    rc=$?
    if [[ $rc -ne 0 ]];then $PERR;exit 1;fi
  fi
#elif [[ $CKND = prep* ]];then
#  echo COPY only works for $CKND without CHGRES
else
  echo COPY does not work for $CKND yet;$PERR;exit 1
fi

################################################################################
# Make output directories

mkdir -p $COMROT

################################################################################
# Copy out restart and output files

$PCOP $CDATE/$CDUMP/$CKND/COPI $DATA $COMROT <$RLIST
rc=$?

################################################################################
# Exit gracefully

if [[ $rc -ne 0 ]];then $PERR;exit 1;fi
$PEND
