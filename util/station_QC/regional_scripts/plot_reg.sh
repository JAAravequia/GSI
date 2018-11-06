#!/bin/bash
set -uxa

################################################################################
####  UNIX Script Documentation Block
# Script name: plot_reg.sh
# Script description:this script is a driver submit all regional plot jobs
# Author: Xiujuan Su    Org: NP20         Date: 2010-09-12

# Script history log:
# 2018-09-21: X. Su  clean up comment lines and document the script
#
# usage:plot_reg.sh scripts gscripts sdate edate tmpdir WSHOME WSUSER WS dtime sfctype sondtype
#                uvsfctype uvsondtype alltim
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
# alltm: all regional analysis delay time

 export scripts=$1
 export gscripts=$2
 export sdate=$3
 export edate=$4
 export tmpdir=$5
 export WSHOME=$6
 export WSUSER=$7
 export WS=$8
 export dtime=$9
 export sfctype=${10}
 export sondtype=${11}
 export uvsfctype=${12}
 export uvsondtype=${13}
 export alltm=${14}


 export  ACCOUNT=GDAS-T2O
export SUB="bsub"
export LOGDIR=/ptmpp1/$USER/gsiqc3/logs/plot_gsiqc3_regional
export group=dev

pdate=${sdate}-${edate}

mkdir -p /stmpp1/$USER/gsiqc3/plotjobs_gsiqc_regional
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

/bin/sh $scripts/plot_vert_reg.sh $scripts $gscripts $stype $tmpdir $WSHOME $WSUSER $WS $sdate $edate $dtime "${alltm}"

done
done

### plot histogram for all data

#dtype="ps120 ps180 ps181 ps187 q120 q180 q181 q187 t120 t180 t181 t187 uv220 uv221 uv223 uv224 uv228 uv229 uv280 uv281 uv287"
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
/bin/sh $scripts/plot_hist_reg.sh $stype $tmpdir $sdate $edate $WSHOME $WSUSER $WS $scripts $gscripts  "${alltm}" 
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
cmdfile=/stmpp1/$USER/gsiqc3/plotjobs_gsiqc_regional/cmdfile_phorz_$stype
rm -f $cmdfile
>$cmdfile
echo "/bin/sh $scripts/plot_horz_reg.sh $stype $itype $tmpdir $sdate $edate $WSHOME $WSUSER $WS $scripts $gscripts $dtime '${alltm}'    " >> $cmdfile
chmod 755 $cmdfile
$SUB -a poe -e $LOGDIR/plot_horz_reg.log -o $LOGDIR/plot_horz.log -P $ACCOUNT -J qc_horz -q dev -M 100 -R affinity[core] -W 02:00  $cmdfile 
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

cmdfile=/stmpp1/$USER/gsiqc3/plotjobs_gsiqc_regional/cmdfile_phorzwind_$stype
rm -f $cmdfile
>$cmdfile
echo "/bin/sh $scripts/plot_horz_reg.sh $stype $itype $tmpdir $sdate $edate $WSHOME $WSUSER $WS $scripts $gscripts $dtime '${alltm}' " >> $cmdfile
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
cmdfile=/stmpp1/$USER/gsiqc3/plotjobs_gsiqc_regional/cmdfile_ptimesfc_$stype
rm -f $cmdfile
>$cmdfile
echo "/bin/sh $scripts/plot_time_reg.sh $stype $itype $tmpdir $sdate $edate $WSHOME $WSUSER $WS $scripts $gscripts '${alltm}'    " >> $cmdfile
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
cmdfile=/stmpp1/$USER/gsiqc3/plotjobs_gsiqc_regional/cmdfile_ptime_$stype
rm -f $cmdfile
>$cmdfile
echo "/bin/sh $scripts/plot_time_reg.sh $stype $itype $tmpdir $sdate $edate $WSHOME $WSUSER $WS $scripts $gscripts '${alltm}'    " >> $cmdfile
chmod 755 $cmdfile
$SUB -a poe -e $LOGDIR/plot_time.log -o $LOGDIR/plot_time.log -P $ACCOUNT -J qc_time -q dev -M 100  -R affinity[core] -W 02:00  $cmdfile
done
done
#### just plot wind direction time series for sounding types, surface winds already plotted at
#### plot_time.sh
itype=1
#sdtype="uv220 uv221 uv223 uv224 uv228 uv229"

for stype in $uvsondtype; do
cmdfile=/stmpp1/$USER/gsiqc3/plotjobs_gsiqc_regional/cmdfile_ptimewind_$stype
rm -f $cmdfile
>$cmdfile
echo "/bin/sh $scripts/plot_time_dir_reg.sh $stype $itype $tmpdir $sdate $edate $WSHOME $WSUSER $WS $scripts $gscripts '${alltm}'      " >> $cmdfile
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
cmdfile=/stmpp1/$USER/gsiqc3/plotjobs_gsiqc_regional/cmdfile_ptime_obs_$stype
rm -f $cmdfile
>$cmdfile
echo "/bin/sh $scripts/plot_time_obs_reg.sh $stype $itype $tmpdir $sdate $edate $WSHOME $WSUSER $WS $scripts $gscripts '${alltm}'   " >> $cmdfile
chmod 755 $cmdfile
$SUB -a poe -e $LOGDIR/plot_time_obs.log -o $LOGDIR/plot_time_obs.log -P $ACCOUNT -J gsiqc_time_obs -q dev -M 200 -R affinity[core] -W 02:00  $cmdfile
done
done
##### plot observation time series
#ddtype2=" uv220 uv221 uv223 uv224 uv228 uv229"
itype=1
for stype in $uvsondtype; do
cmdfile=/stmpp1/$USER/gsiqc3/plotjobs_gsiqc_regional/cmdfile_ptime_obs_dir_$stype
rm -f $cmdfile
>$cmdfile
echo "/bin/sh $scripts/plot_time_obs_dir_reg.sh $stype $itype $tmpdir $sdate $edate $WSHOME $WSUSER $WS $scripts $gscripts  '${alltm}'     " >> $cmdfile
chmod 755 $cmdfile
$SUB -a poe -e $LOGDIR/plot_time_obs.log -o $LOGDIR/plot_time_obs.log -P $ACCOUNT -J qc_time_obs -q dev -M 200 -R affinity[core] -W 02:00  $cmdfile
done
exit


