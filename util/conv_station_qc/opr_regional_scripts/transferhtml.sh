### this scripts will transfer web html files for display

set -uxa

htmldir=$1
WSHOME=$2
WSUSER=$3
WS=$4
dtime=$5


scp ${htmldir}/*html $WSUSER@$WS:${WSHOME}/web
scp ${htmldir}/regional/*html $WSUSER@$WS:${WSHOME}/web/regional



exit









