### this scripts will transfer web html files for display

set -uxa

htmldir=$1
WSHOME=$2
WSUSER=$3
WS=$4
dtime=$5
mapping=$6


scp ${htmldir}/*html $WSUSER@$WS:${WSHOME}/web
scp ${htmldir}/global/*html $WSUSER@$WS:${WSHOME}/web/global
scp -r ${mapping} $WSUSER@$WS:${WSHOME}/web/global/horz



exit









