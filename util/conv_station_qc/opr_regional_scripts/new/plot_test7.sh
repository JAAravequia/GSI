###  this script gets all plot scripts together
set -uax
scripts=$1
gscripts=$2
sdate=$3
edate=$4
tmpdir=$5
WSHOME=$6
WSUSER=$7
WS=$8
dtime=$9


export NDATE=/nwprod/util/exec/ndate
 export  ACCOUNT=GDAS-T2O
export SUB=/u/wx20mi/bin/sub
export USER=Xiujuan.Su
export LOGDIR=/ptmp/Xiujuan.Su/plot_regional_gsiqc3


mkdir -p /stmp/Xiujuan.Su/plotjobs_gsiqc_regional


### plot histogram for all data
dtype="ps120 ps180 ps181 ps187 ps188 q120 q180 q181 q187 q188 t120 t180 t181 t187 t188 uv220 uv221 uv223 uv224 uv229 uv280 uv281 uv287 uv288"
for stype in $dtype; do
/bin/sh $scripts/plot_hist.sh $stype $tmpdir $sdate $edate $WSHOME $WSUSER $WS $scripts $gscripts 
done


exit


