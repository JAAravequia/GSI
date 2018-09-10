###  this script gets all plot scripts together
set -uax

export scripts=/u/Xiujuan.Su/home/gsiqc3/scripts
export gscripts=/u/Xiujuan.Su/home/gsiqc3/grads
export sdate=2011010100
export edate=2011033118
export tmpdir=/ptmp/Xiujuan.Su/bufrstas
export WSHOME=/export/lnx42/emc-lw-xsu/gsiqc3
export WSUSER=emc-lw-xsu
export WS=lnx42.ncep.noaa.gov

export NDATE=/nwprod/util/exec/ndate
 export  ACCOUNT=GDAS-T2O
export SUB=/u/wx20mi/bin/sub
export USER=Xiujuan.Su
export LOGDIR=/ptmp/Xiujuan.Su/plot_gsiqc3

pdate=${sdate}-${edate}

mkdir -p /stmp/Xiujuan.Su/plotjobs_gsiqc
### plot time series for  sounding observation
cmdfile=/stmp/Xiujuan.Su/plotjobs_gsiqc/cmdfile_ptime_obs
rm -f $cmdfile
ddtype="q120 t120 uv220 uv221 uv223 uv224 uv229"
>$cmdfile
itype=1
for stype in $ddtype; do
echo "/bin/sh $scripts/plot_time_obs.sh $stype $itype $tmpdir $sdate $edate $WSHOME $WSUSER $WS $scripts $gscripts " >> $cmdfile
done
ntasks=`cat $cmdfile|wc -l `
((nprocs=(ntasks+1)/2))
$SUB -a $ACCOUNT -j plot_gsiqc_time_obs -u $USER -t 3:00:00 -o $LOGDIR/plot_time_obs.log -p $ntasks/1/N -q dev -g devonprod /usr/bin/poe -cmdfile $cmdfile -pgmmodel mpmd -ilevel 2 -labelio yes -stdoutmode ordered

##### plot observation time series
cmdfile=/stmp/Xiujuan.Su/plotjobs_gsiqc/cmdfile_ptime_obs_dir
rm -f $cmdfile
ddtype2=" uv220 uv221 uv223 uv224 uv229"
>$cmdfile
itype=1
for stype in $ddtype2; do
echo "/bin/sh $scripts/plot_time_obs_dir.sh $stype $itype $tmpdir $sdate $edate $WSHOME $WSUSER $WS $scripts $gscripts" >> $cmdfile
done
ntasks=`cat $cmdfile|wc -l `
   ((nprocs=(ntasks+1)/2))
$SUB -a $ACCOUNT -j plot_time_obs_dir -u $USER -t 3:00:00 -o $LOGDIR/plot_time_obs_dir.log -p $ntasks/1/N -q dev -g devonprod /usr/bin/poe -cmdfile $cmdfile -pgmmodel mpmd -ilevel 2 -labelio yes -stdoutmode ordered


exit


