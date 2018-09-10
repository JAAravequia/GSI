###  this script gets all plot scripts together
set -uax

export scripts=/u/Xiujuan.Su/home/gsiqc3/regional_scripts
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
### plot  time series of statistics, in this script, the o-b(o-a) time series for all types,
###  and the surface observation time series.  The only wind speed o-b(o-a) time series for
## the sounding types is plotted. 
cmdfile=/stmp/Xiujuan.Su/plotjobs_gsiqc/cmdfile_ptime
rm -f $cmdfile
dtype="ps120 ps180 ps181 ps187 q180 t180 "
>$cmdfile
for stype in $dtype; do
if [ "${stype}" = 'q120' -o "${stype}" = 't120' -o "${stype}" = 'uv220' -o \
    "${stype}" = 'uv221' -o "${stype}" = 'uv223' -o "${stype}" = 'uv224' -o "${stype}" = 'uv229' ]; then
 itype=1
else
itype=0
fi
echo "/bin/sh $scripts/plot_time.sh $stype $itype $tmpdir $sdate $edate $WSHOME $WSUSER $WS $scripts $gscripts " >> $cmdfile
done
ntasks=`cat $cmdfile|wc -l `
   ((nprocs=(ntasks+1)/2))
$SUB -a $ACCOUNT -j plot_gsiqc_time -u $USER -t 2:00:00 -o $LOGDIR/plot_time.log -p $ntasks/1/N -q dev -g devonprod /usr/bin/poe -cmdfile $cmdfile -pgmmodel mpmd -ilevel 2 -labelio yes -stdoutmode ordered

#### just plot wind direction time series for sounding types, surface winds already plotted at
#### plot_time.sh
cmdfile=/stmp/Xiujuan.Su/plotjobs_gsiqc/cmdfile_ptimewind
rm -f $cmdfile
sdtype="uv220 uv221 uv223 uv224"
>$cmdfile

itype=1
for stype in $sdtype; do
echo "/bin/sh $scripts/plot_time_dir.sh $stype $itype $tmpdir $sdate $edate $WSHOME $WSUSER $WS $scripts $gscripts " >> $cmdfile
done
ntasks=`cat $cmdfile|wc -l `
   ((nprocs=(ntasks+1)/2))
$SUB -a $ACCOUNT -j plot_gsiqc_time_dir -u $USER -t 3:00:00 -o $LOGDIR/plot_time_dir.log -p $ntasks/1/N -q dev -g devonprod /usr/bin/poe -cmdfile $cmdfile -pgmmodel mpmd -ilevel 2 -labelio yes -stdoutmode ordered

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


