###  this script gets all plot scripts together
set -uax

export scripts=/u/Xiujuan.Su/home/gsiqc3/regional_scripts
export gscripts=/u/Xiujuan.Su/home/gsiqc3/grads
export sdate=2011040100
export edate=2011053118
export tmpdir=/ptmp/Xiujuan.Su/bufrstas_regional/06
export WSHOME=/export/lnx42/emc-lw-xsu/gsiqc3
export WSUSER=emc-lw-xsu
export WS=lnx42.ncep.noaa.gov

export NDATE=/nwprod/util/exec/ndate
 export  ACCOUNT=GDAS-T2O
export SUB=/gpfs/gd1/emc/global/save/emc.glopara/svn/gfs/trunk/para/bin/sub_wcoss
export USER=Xiujuan.Su
export LOGDIR=/ptmp/Xiujuan.Su/plot_gsiqc3_reg

pdate='201104-05'

mkdir -p /stmp/Xiujuan.Su/plotjobs_gsiqc_reg

####  plot horizontal map for all data type

cmdfile=/stmp/Xiujuan.Su/plotjobs_gsiqc_reg/cmdfile_phorz
rm -f $cmdfile
dtype="ps120 ps180 ps181 ps187 ps188 q120 q180 q181 q187 q188 t120 t180 t181 t187 t188 uv220 uv221 uv223 uv224 uv229 uv280 uv281 uv287 uv288 "
#dtype="uv220 uv221 uv223 uv224 uv229 uv280"
>$cmdfile
for stype in $dtype; do
if [ "${stype}" = 'q120' -o "${stype}" = 't120' -o "${stype}" = 'uv220' -o \
    "${stype}" = 'uv221' -o "${stype}" = 'uv223' -o "${stype}" = 'uv224'  -o \
    "${stype}" = 'uv229' ]; then
itype=1
else
itype=0
fi
echo "/bin/sh $scripts/plot_horz.sh $stype $itype $tmpdir $sdate $edate $WSHOME $WSUSER $WS $scripts $gscripts $pdate" >> $cmdfile
done
ntasks=`cat $cmdfile|wc -l `
  ((nprocs=(ntasks+1)/2))
$SUB -a $ACCOUNT -j plot_gsiqc_horz_reg -u $USER -t 2:00:00 -o $LOGDIR/plot_horz.log -p $ntasks/1/N -q dev -g devonprod /usr/bin/poe -cmdfile $cmdfile -pgmmodel mpmd -ilevel 2 -labelio yes -stdoutmode ordered

exit
