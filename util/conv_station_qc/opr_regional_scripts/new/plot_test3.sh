###  this script gets all plot scripts together
set -uax

export scripts=/u/Xiujuan.Su/home/gsiqc3/regional_scripts
export gscripts=/u/Xiujuan.Su/home/gsiqc3/grads
export sdate=2011040100
export edate=2011053118
export tmpdir=/u/Xiujuan.Su/nbns/bufrstas_regional
export WSHOME=/export/lnx42/emc-lw-xsu/gsiqc3
export WSUSER=emc-lw-xsu
export WS=lnx42.ncep.noaa.gov

export NDATE=/nwprod/util/exec/ndate
 export  ACCOUNT=GDAS-T2O
export SUB=/u/wx20mi/bin/sub
export USER=Xiujuan.Su
export LOGDIR=/ptmp/Xiujuan.Su/plot_gsiqc3

pdate=${sdate}-${edate}


####  plot horizontal map for all data type

dtype="ps120 ps180 ps181 ps187 ps188 q120 q180 q181 q187 q188 t120 t180 t181 t187 t188 uv220 uv221 uv223 uv224 uv229 uv280 uv281 uv287 uv288"
dtype=" uv220 uv221 uv223 uv224 uv229 uv280 uv281 uv287 uv288"
for stype in $dtype; do
/bin/sh $scripts/plot_hist.sh $stype $tmpdir $sdate $edate $WSHOME $WSUSER $WS $scripts $gscripts  
done

exit
