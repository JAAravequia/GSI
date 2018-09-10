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
export SUB=${10}

# See which machine is dev.  Jobs can only run on dev.  If machine
# is prod, exit script
prod=`cat /etc/prod | cut -c1`
hostname > host
machine=`cat host | cut -c1`
echo $prod
echo $machine
if [[ "$machine" = "$prod" ]]; then
group=devonprod
else
group=dev
fi


#export scripts=/u/Xiujuan.Su/home/gsiqc3/scripts
#export gscripts=/u/Xiujuan.Su/home/gsiqc3/grads
#export sdate=2011010100
#export edate=2011033118
#export tmpdir=/ptmp/Xiujuan.Su/bufrstas
#export WSHOME=/export/emc-lw-xsu/wd20xs/gsiqc3
#export WSUSER=wd20xs
#export WS=emc-lw-xsu

export NDATE=/nwprod/util/exec/ndate
 export  ACCOUNT=GDAS-T2O
#export SUB=/gpfs/gd1/emc/global/save/emc.glopara/svn/gfs/trunk/para/bin/sub_wcoss
export USER=Xiujuan.Su
export LOGDIR=/ptmp/Xiujuan.Su/logs/plot_regional_gsiqc3


mkdir -p /stmp/Xiujuan.Su/plotjobs_gsiqc_regional
mkdir -p $LOGDIR

#### plot the vertical profile for the sounding data

dtype1="q120 t120  uv220 uv221 uv223 uv224 uv229 "
for stype in $dtype1
do
/bin/sh $scripts/plot_vert.sh $scripts $gscripts $stype $tmpdir $WSHOME $WSUSER $WS $sdate $edate $dtime

done

### plot histogram for all data
dtype="ps120 ps180 ps181 ps187 ps188 q120 q180 q181 q187 q188 t120 t180 t181 t187 t188 uv220 uv221 uv223 uv224 uv229 uv280 uv281 uv287 uv288"
for stype in $dtype; do
/bin/sh $scripts/plot_hist.sh $stype $tmpdir $sdate $edate $WSHOME $WSUSER $WS $scripts $gscripts 
done

#####  plot horizontal map for all data type
mkdir -p $LOGDIR

cmdfile=/stmp/Xiujuan.Su/plotjobs_gsiqc_regional/cmdfile_phorz
rm -f $cmdfile
dtype="ps120 ps180 ps181 ps187 ps188 q120 q180 tq181 q187 q188 t120 t180 t181 t187 t188 uv220 uv221 uv223 uv224 uv229 uv280 uv281 uv287 uv288"
>$cmdfile
for stype in $dtype; do
if [ "${stype}" = 'q120' -o "${stype}" = 't120' -o "${stype}" = 'uv220' -o \
    "${stype}" = 'uv221' -o "${stype}" = 'uv223' -o "${stype}" = 'uv224'  -o "${stype}" = 'uv229' ]; then
 itype=1
else
itype=0
fi
echo "/bin/sh $scripts/plot_horz.sh $stype $itype $tmpdir $sdate $edate $WSHOME $WSUSER $WS $scripts $gscripts $dtime" >> $cmdfile
done
ntasks=`cat $cmdfile|wc -l `
  ((nprocs=(ntasks+1)/2))
$SUB -a $ACCOUNT -j plot_gsiqc_horz_reg  -u $USER -t 1:00:00 -o $LOGDIR/plot_horz.log -p $ntasks/1/N -q dev -g $group  /usr/bin/poe -cmdfile $cmdfile -pgmmodel mpmd -ilevel 2 -labelio yes -stdoutmode ordered
##
#
### plot  time series of statistics, in this script, the o-b(o-a) time series for all types,
###  and the surface observation time series.  The only wind speed o-b(o-a) time series for
## the sounding types is plotted. 
cmdfile=/stmp/Xiujuan.Su/plotjobs_gsiqc_regional/cmdfile_ptime
rm -f $cmdfile
dtype="ps120 ps180 ps181 ps187 ps188 q120 q180 q181 q187 q188 t120 t180 t181 t187 t188 uv220 uv221 uv223 uv224 uv229 uv280 uv281 uv287 uv288"
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
$SUB -a $ACCOUNT -j plot_gsiqc_time_reg -u $USER -t 2:00:00 -o $LOGDIR/plot_time.log -p $ntasks/1/N -q dev -g $group /usr/bin/poe -cmdfile $cmdfile -pgmmodel mpmd -ilevel 2 -labelio yes -stdoutmode ordered

#### just plot wind direction time series for sounding types, surface winds already plotted at
#### plot_time.sh
cmdfile=/stmp/Xiujuan.Su/plotjobs_gsiqc_regional/cmdfile_ptimewind
rm -f $cmdfile
sdtype="uv220 uv221 uv223 uv224 uv229"
>$cmdfile

itype=1
for stype in $sdtype; do
echo "/bin/sh $scripts/plot_time_dir.sh $stype $itype $tmpdir $sdate $edate $WSHOME $WSUSER $WS $scripts $gscripts " >> $cmdfile
done
ntasks=`cat $cmdfile|wc -l `
   ((nprocs=(ntasks+1)/2))
$SUB -a $ACCOUNT -j plot_gsiqc_time_dir_reg -u $USER -t 3:00:00 -o $LOGDIR/plot_time_dir.log -p $ntasks/1/N -q dev -g $group  /usr/bin/poe -cmdfile $cmdfile -pgmmodel mpmd -ilevel 2 -labelio yes -stdoutmode ordered

### plot time series for  sounding observation
cmdfile=/stmp/Xiujuan.Su/plotjobs_gsiqc_regional/cmdfile_ptime_obs
rm -f $cmdfile
ddtype="q120 t120 uv220 uv221 uv223 uv224 uv229"
>$cmdfile
itype=1
for stype in $ddtype; do
echo "/bin/sh $scripts/plot_time_obs.sh $stype $itype $tmpdir $sdate $edate $WSHOME $WSUSER $WS $scripts $gscripts " >> $cmdfile
done
ntasks=`cat $cmdfile|wc -l `
((nprocs=(ntasks+1)/2))
$SUB -a $ACCOUNT -j plot_gsiqc_time_obs_reg -u $USER -t 3:00:00 -o $LOGDIR/plot_time_obs.log -p $ntasks/1/N -q dev -g $group /usr/bin/poe -cmdfile $cmdfile -pgmmodel mpmd -ilevel 2 -labelio yes -stdoutmode ordered

##### plot observation time series
cmdfile=/stmp/Xiujuan.Su/plotjobs_gsiqc_regional/cmdfile_ptime_obs_dir
rm -f $cmdfile
ddtype2=" uv220 uv221 uv223 uv224 uv229"
>$cmdfile
itype=1
for stype in $ddtype2; do
echo "/bin/sh $scripts/plot_time_obs_dir.sh $stype $itype $tmpdir $sdate $edate $WSHOME $WSUSER $WS $scripts $gscripts" >> $cmdfile
done
ntasks=`cat $cmdfile|wc -l `
   ((nprocs=(ntasks+1)/2))
$SUB -a $ACCOUNT -j plot_time_obs_dir_reg -u $USER -t 3:00:00 -o $LOGDIR/plot_time_obs_dir.log -p $ntasks/1/N -q dev -g $group /usr/bin/poe -cmdfile $cmdfile -pgmmodel mpmd -ilevel 2 -labelio yes -stdoutmode ordered


exit


