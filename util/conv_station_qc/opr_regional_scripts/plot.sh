###  this script gets all plot scripts together
set -uax
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


#export scripts=/u/Xiujuan.Su/home/gsiqc3/scripts
#export gscripts=/u/Xiujuan.Su/home/gsiqc3/grads
#export sdate=2011010100
#export edate=2011033118
#export tmpdir=/ptmpp1/Xiujuan.Su/bufrstas
#export WSHOME=/export/emc-lw-xsu/wd20xs/gsiqc3
#export WSUSER=wd20xs
#export WS=emc-lw-xsu.ncep.noaa.gov

export NDATE=/nwprod/util/exec/ndate
 export  ACCOUNT=GDAS-T2O
#export SUB=/gpfs/td1/emc/global/save/emc.glopara/svn/gfs/trunk/para/bin/sub_wcoss
export SUB="bsub"
export LOGDIR=/ptmpp1/$USER/gsiqc3/logs/plot_gsiqc3_regional
export group=dev

pdate=${sdate}-${edate}

mkdir -p /stmpp1/$USER/gsiqc3/plotjobs_gsiqc_regional
mkdir -p $LOGDIR

#### plot the vertical profile for the sounding data

#dtype1="q120 t120  uv220 uv221 uv223 uv224 uv228 uv229 "
#for stype in $dtype1
#do
for datatype in ssondtype suvsondtype
do
if [ "${datatype}" = "ssondtype" ]; then
dstype=$sondtype
elif [ "${datatype}" = "suvsondtype" ]; then
dstype=$uvsondtype
fi
for stype in $dstype
do

/bin/sh $scripts/plot_vert.sh $scripts $gscripts $stype $tmpdir $WSHOME $WSUSER $WS $sdate $edate $dtime "${alltm}"

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
/bin/sh $scripts/plot_hist.sh $stype $tmpdir $sdate $edate $WSHOME $WSUSER $WS $scripts $gscripts  "${alltm}" 
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
echo "/bin/sh $scripts/plot_horz.sh $stype $itype $tmpdir $sdate $edate $WSHOME $WSUSER $WS $scripts $gscripts $dtime '${alltm}'    " >> $cmdfile
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

cmdfile=/stmpp1/$USER/gsiqc3/plotjobs_gsiqc_regional/cmdfile_phorzwind_$stype
rm -f $cmdfile
>$cmdfile
echo "/bin/sh $scripts/plot_horz.sh $stype $itype $tmpdir $sdate $edate $WSHOME $WSUSER $WS $scripts $gscripts $dtime '${alltm}' " >> $cmdfile
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
echo "/bin/sh $scripts/plot_time.sh $stype $itype $tmpdir $sdate $edate $WSHOME $WSUSER $WS $scripts $gscripts '${alltm}'    " >> $cmdfile
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
echo "/bin/sh $scripts/plot_time.sh $stype $itype $tmpdir $sdate $edate $WSHOME $WSUSER $WS $scripts $gscripts '${alltm}'    " >> $cmdfile
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
echo "/bin/sh $scripts/plot_time_dir.sh $stype $itype $tmpdir $sdate $edate $WSHOME $WSUSER $WS $scripts $gscripts '${alltm}'      " >> $cmdfile
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
echo "/bin/sh $scripts/plot_time_obs.sh $stype $itype $tmpdir $sdate $edate $WSHOME $WSUSER $WS $scripts $gscripts '${alltm}'   " >> $cmdfile
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
echo "/bin/sh $scripts/plot_time_obs_dir.sh $stype $itype $tmpdir $sdate $edate $WSHOME $WSUSER $WS $scripts $gscripts  '${alltm}'     " >> $cmdfile
chmod 755 $cmdfile
$SUB -a poe -e $LOGDIR/plot_time_obs.log -o $LOGDIR/plot_time_obs.log -P $ACCOUNT -J qc_time_obs -q dev -M 200 -R affinity[core] -W 02:00  $cmdfile
done
exit


