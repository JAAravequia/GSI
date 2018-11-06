#!/bin/sh
set -xa
################################################################################
####  UNIX Script Documentation Block
# Script name: rungsiqc_prep.sh 
# Script description:this script put gsi diagnotic file to prepbufr 
# Author: Xiujuan Su    Org: NP20         Date: 2010-09-12

# Script history log:
# 2018-09-14: X. Su  clean up comment lines and document the script
#
# usage: rungsiqc_prep.sh scripts datadir savedir sdate edate HOMEcfs EXECcfs USHcfs NDATE
#
# scripts: the scripts directory
# datadir: The prepbufr file and GSI diagnostic file directory
# savedir: the output prepbufr.post directory
# HOMEcfs: The directory of program to put GSi diagnostic file to prepbufr file 
# EXECcfs: The executable file to put GSi diagnostic file to prepbufr file
# USHcfs : The script directory used by the program to put GSi diagnostic file 
#          to prepbufr file
# NDATE  : The utility to change date 

 export scripts=$1
 export datadir=$2
 export savedir=$3
 export sdate=$4
 export edate=$5
 export HOMEcfs=$6
 export EXECcfs=$7
 export USHcfs=$8
 export NDATE=$9
# HOMEcfs=/climate/save/Jack.Woollen/parafits
# EXECcfs=/climate/save/Jack.Woollen/parafits/exec
# USHcfs=/climate/save/Jack.Woollen/parafits/ush
# export NDATE=/nwprod/util/exec/ndate
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
