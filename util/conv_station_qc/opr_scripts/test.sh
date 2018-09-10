#!/usr/bin/env bash
set -xuea

### define the script, data directory and excutable
### file for each run

export datadir=/com2/gfs/prod
export savedir=$PWD

#  set time
DATA=/ptmpp1/$USER/cnvdiag
rm -rf $DATA; mkdir -p $DATA
cd $DATA

export PDATE=`date +%Y%m%d%H`
PDATE=$(/nwprod/util/exec/ndate -24 $PDATE)
tdate=`echo $PDATE|cut -c1-8`

for hr in 00 06 12 18; do
cdate=$tdate$hr

PRPI=$datadir/gdas.$tdate/gdas1.t${hr}z.prepbufr
CNVS=$datadir/gdas.$tdate/gdas1.t${hr}z.cnvstat
SIGA=$datadir/gdas.$tdate/gdas1.t${hr}z.sanl
PRPO=$savedir/diagbufr.gdas.$cdate

SIGEVENTSH=echo
HOMEcfs=/climate/save/Jack.Woollen/parafits
EXECcfs=/climate/save/Jack.Woollen/parafits/exec
USHcfs=/climate/save/Jack.Woollen/parafits/ush

PRPI=$($USHcfs/ACprof) ## split ac profiles to match convdiag

$HOMEcfs/ush/cfs_bufr_post.sh $SIGA $CNVS $PRPI $PRPO $cdate

done
