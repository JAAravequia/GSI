#!/bin/bash
set -uxa

################################################################################
####  UNIX Script Documentation Block
# Script name: plot.sh 
# Script description:this script is a driver submit all plot jobs 
# Author: Xiujuan Su    Org: NP20         Date: 2010-09-12

# Script history log:
# 2018-09-12: X. Su  clean up comment lines and document the script
#
# usage:plot.sh scripts gscripts sdate edate tmpdir WSHOME WSUSER WS dtime sfctype sondtype 
#                uvsfctype uvsondtype
#
# sdate: starting time
# edate: end of time
# tmpdir: statisticas data directory
# WSHOME: web site directory for monitor web site
# WSUSER: web site user name
# WS: the machine in which the monitor web site locates
# scripts:  the scripts directory
# gscripts: grad script directory
# dtim: the period of data processed
# sfctype: all surface data types except wind observations
# sondtype: all sounding observation type except wind observations 
# uvsfctype: wind surface data types
# uvsondtype: wind sounding observation types  


scripts=$1
gscripts=$2
sdate=$3
edate=$4
tmpdir=$5
WSHOME=$6
WSUSER=$7
WS=$8
dtime=$9
export sfctype=${10}
export sondtype=${11}
export uvsfctype=${12}
export uvsondtype=${13}



export NDATE=/nwprod/util/exec/ndate
export  ACCOUNT=GDAS-T2O
export SUB="bsub"
export LOGDIR=/ptmpp1/$USER/gsiqc3/logs/plot_gsiqc3
export group=dev

pdate=${sdate}-${edate}

mkdir -p /stmpp1/$USER/gsiqc3/plotjobs_gsiqc
mkdir -p $LOGDIR

#### plot the vertical profile for the sounding data

for datatype in ssondtype suvsondtype
do
if [ "${datatype}" = "ssondtype" ]; then
dstype=$sondtype
elif [ "${datatype}" = "suvsondtype" ]; then
dstype=$uvsondtype
fi
for stype in $dstype
do

/bin/sh $scripts/plot_vert.sh $scripts $gscripts $stype $tmpdir $WSHOME $WSUSER $WS $sdate $edate $dtime

done
done

### plot histogram for all data

for datatype in ssfctype ssondtype suvsfctype suvsondtype
do
if [ "${datatype}" = "ssfctype" ]; then
dstype=$sfctype
elif [ "${datatype}" = "ssondtype" ]; then
dstype=$sondtype
elif [ "${datatype}" = "suvsfctype" ]; then
dstype=$uvsfctype
elif [ "${datatype}" = "suvsondtype" ]; then
dstype=$uvsondtype
fi


for stype in $dstype; do
/bin/sh $scripts/plot_hist.sh $stype $tmpdir $sdate $edate $WSHOME $WSUSER $WS $scripts $gscripts $dtime 
done
done
####  plot horizontal map for all data type

#dtype="ps120 ps180 ps181 ps187 q120 q180 q181 q187 t120 t180 t181 t187 "

for datatype in ssfctype ssondtype suvsfctype suvsondtype
do
if [ "${datatype}" = "ssfctype" ]; then
dstype=$sfctype
itype=0
elif [ "${datatype}" = "ssondtype" ]; then
dstype=$sondtype
itype=1
elif [ "${datatype}" = "suvsfctype" ]; then
dstype=$uvsfctype
itype=0
elif [ "${datatype}" = "suvsondtype" ]; then
dstype=$uvsondtype
itype=1
fi


for stype in $dstype; do
cmdfile=/stmpp1/$USER/gsiqc3/plotjobs_gsiqc/cmdfile_phorz_$stype
rm -f $cmdfile
>$cmdfile
echo "/bin/sh $scripts/plot_horz.sh $stype $itype $tmpdir $sdate $edate $WSHOME $WSUSER $WS $scripts $gscripts $dtime" >> $cmdfile
chmod 755 $cmdfile
$SUB -a poe -e $LOGDIR/plot_horz.log -o $LOGDIR/plot_horz.log -P $ACCOUNT -J qc_horz -q dev -M 100 -R affinity[core] -W 02:00  $cmdfile 
done
done


#dtype=" uv220 uv221 uv223 uv224 uv228 uv229 uv280 uv281 uv287"

for datatype in suvsfctype suvsondtype
do
if [ "${datatype}" = "suvsfctype" ]; then
dstype=$uvsfctype
itype=0
elif [ "${datatype}" = "suvsondtype" ]; then
dstype=$uvsondtype
itype=1
fi

for stype in $dstype; do

cmdfile=/stmpp1/$USER/gsiqc3/plotjobs_gsiqc/cmdfile_phorzwind_$stype
rm -f $cmdfile
>$cmdfile
echo "/bin/sh $scripts/plot_horz.sh $stype $itype $tmpdir $sdate $edate $WSHOME $WSUSER $WS $scripts $gscripts $dtime" >> $cmdfile
chmod 755 $cmdfile
#ntasks=`cat $cmdfile|wc -l `
#nodes=`expr $ntasks + 15`
#nodes=`expr $nodes / 16`
$SUB -a poe -e $LOGDIR/plot_horz.log2 -o $LOGDIR/plot_horz.log2 -P $ACCOUNT -J qc_horz -q dev -M 100 -R affinity[core] -W 02:00  $cmdfile 
done
done
#
### plot  time series of statistics, in this script, the o-b(o-a) time series for all types,
###  and the surface observation time series.  The only wind speed o-b(o-a) time series for
## the sounding types is plotted. 

#dtype="ps120 ps180 ps181 ps187 q120 q180 q181 q187 t120 t180 t181 t187 "
for datatype in ssfctype ssondtype 
do
if [ "${datatype}" = "ssfctype" ]; then
dstype=$sfctype
itype=0
elif [ "${datatype}" = "ssondtype" ]; then
dstype=$sondtype
itype=1
fi

for stype in $dstype; do
cmdfile=/stmpp1/$USER/gsiqc3/plotjobs_gsiqc/cmdfile_ptimesfc_$stype
rm -f $cmdfile
>$cmdfile
echo "/bin/sh $scripts/plot_time.sh $stype $itype $tmpdir $sdate $edate $WSHOME $WSUSER $WS $scripts $gscripts " >> $cmdfile
chmod 755 $cmdfile
$SUB -a poe -e $LOGDIR/plot_time.log2 -o $LOGDIR/plot_time.log2 -P $ACCOUNT -J qc_time -q dev -M 100 -R affinity[core] -W 02:00  $cmdfile
done
done

#dtype=" uv220 uv221 uv223 uv224 uv228 uv229 uv280 uv281 uv287"

for datatype in suvsfctype suvsondtype
do
if [ "${datatype}" = "suvsfctype" ]; then
dstype=$uvsfctype
itype=0
elif [ "${datatype}" = "suvsondtype" ]; then
dstype=$uvsondtype
itype=1
fi

for stype in $dstype; do
cmdfile=/stmpp1/$USER/gsiqc3/plotjobs_gsiqc/cmdfile_ptime_$stype
rm -f $cmdfile
>$cmdfile
echo "/bin/sh $scripts/plot_time.sh $stype $itype $tmpdir $sdate $edate $WSHOME $WSUSER $WS $scripts $gscripts " >> $cmdfile
chmod 755 $cmdfile
$SUB -a poe -e $LOGDIR/plot_time.log -o $LOGDIR/plot_time.log -P $ACCOUNT -J qc_time -q dev -M 100  -R affinity[core] -W 02:00  $cmdfile
done
done
#### just plot wind direction time series for sounding types, surface winds already plotted at
#### plot_time.sh
itype=1
#sdtype="uv220 uv221 uv223 uv224 uv228 uv229"

for stype in $uvsondtype; do
cmdfile=/stmpp1/$USER/gsiqc3/plotjobs_gsiqc/cmdfile_ptimewind_$stype
rm -f $cmdfile
>$cmdfile
echo "/bin/sh $scripts/plot_time_dir.sh $stype $itype $tmpdir $sdate $edate $WSHOME $WSUSER $WS $scripts $gscripts " >> $cmdfile
chmod 755 $cmdfile
$SUB -a poe -e $LOGDIR/plot_time_dir.log -o $LOGDIR/plot_time_dir.log -P $ACCOUNT -J gsiqc_time_dir -q dev -M 100 -R -R affinity[core] -W 02:00  $cmdfile
done
### plot time series for  sounding observation
#ddtype="q120 t120 uv220 uv221 uv223 uv224 uv228 uv229"
for datatype in ssondtype suvsondtype
do
if [ "${datatype}" = "ssondtype" ]; then
dstype=$sondtype
elif [ "${datatype}" = "suvsondtype" ]; then
dstype=$uvsondtype
fi

itype=1
for stype in $dstype; do
cmdfile=/stmpp1/$USER/gsiqc3/plotjobs_gsiqc/cmdfile_ptime_obs_$stype
rm -f $cmdfile
>$cmdfile
echo "/bin/sh $scripts/plot_time_obs.sh $stype $itype $tmpdir $sdate $edate $WSHOME $WSUSER $WS $scripts $gscripts " >> $cmdfile
chmod 755 $cmdfile
$SUB -a poe -e $LOGDIR/plot_time_obs.log -o $LOGDIR/plot_time_obs.log -P $ACCOUNT -J gsiqc_time_obs -q dev -M 200 -R affinity[core] -W 02:00  $cmdfile
done
done
##### plot observation time series
#ddtype2=" uv220 uv221 uv223 uv224 uv228 uv229"
itype=1
for stype in $uvsondtype; do
cmdfile=/stmpp1/$USER/gsiqc3/plotjobs_gsiqc/cmdfile_ptime_obs_dir_$stype
rm -f $cmdfile
>$cmdfile
echo "/bin/sh $scripts/plot_time_obs_dir.sh $stype $itype $tmpdir $sdate $edate $WSHOME $WSUSER $WS $scripts $gscripts" >> $cmdfile
chmod 755 $cmdfile
$SUB -a poe -e $LOGDIR/plot_time_obs.log -o $LOGDIR/plot_time_obs.log -P $ACCOUNT -J qc_time_obs -q dev -M 200 -R affinity[core] -W 02:00  $cmdfile
done
#done
exit


