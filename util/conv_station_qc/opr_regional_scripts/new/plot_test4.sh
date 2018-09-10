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

####  plot vertical map for all data type
dtype1="q120 t120 "
for stype in $dtype1
do
/bin/sh $scripts/plot_vert.sh $scripts $gscripts $stype $tmpdir $WSHOME $WSUSER $WS $sdate $edate
done

exit
