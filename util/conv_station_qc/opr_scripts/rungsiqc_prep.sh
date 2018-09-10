#!/bin/sh
set -xa

 export scripts=$1
 export datadir=$2
 export savedir=$3
 sdate=$4
 edate=$5
SIGEVENTSH=echo
HOMEcfs=/climate/save/Jack.Woollen/parafits
EXECcfs=/climate/save/Jack.Woollen/parafits/exec
USHcfs=/climate/save/Jack.Woollen/parafits/ush
 export NDATE=/nwprod/util/exec/ndate
 export  ACCOUNT=GDAS-T2O
export SUB=/gpfs/td1/emc/global/save/emc.glopara/svn/gfs/trunk/para/bin/sub_wcoss
export LOGDIR=/ptmpp1/$USER/gsiqc3/plot_gsiqc3


 mkdir -p $savedir

 rdate=$sdate

 DATA=/ptmpp1/$USER/gsiqc3/global_prepbufr


 mkdir -p  $DATA
 cd $DATA
 rm -f *

while [ $rdate -le $edate ]; do

cdate=`echo $rdate|cut -c1-8`
hr=`echo $rdate|cut -c9-10`

PRPI=$datadir/gdas.$cdate/gdas.t${hr}z.prepbufr
CNVS=$datadir/gdas.$cdate/gdas.t${hr}z.cnvstat
SIGA=$datadir/gdas.$cdate/gdas.t${hr}z.sanl
PRPO=prepbufr.post.$rdate
PRPI=$($USHcfs/ACprof) ## split ac profiles to match convdiag

$HOMEcfs/ush/cfs_bufr_post.sh $SIGA $CNVS $PRPI $PRPO $rdate


mv prepbufr.post.$rdate $savedir
rdate=`$NDATE 06 $rdate`
done     


exit
