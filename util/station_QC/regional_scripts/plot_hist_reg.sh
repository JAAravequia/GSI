#!/bin/bash
set -xa

################################################################################
####  UNIX Script Documentation Block
# Script name: plot_hist_reg.sh
# Script description:this script makes regional histogram plots
# Author: Xiujuan Su    Org: NP20         Date: 2010-09-12
# Abstract: this script makes histogram plots
#
# Script history log:
# 2018-09-21: X. Su  clean up comment lines and document the script
#
# usage: plot_hist_reg.sh dtype datadir sdate edate WSHOME WSUSER WS scripts gscripts ttm 
#
# dtype: data type such as  t120, uv220
# datadir:  the data the plot needed
# sdate: starting time
# edate: end of time
# WSHOME: web site directory for monitor web site
# WSUSER: web site user name
# WS: the machine in which the monitor web site locates
# scripts:  the scripts directory
# gscripts: grad script directory
# ttm: regional analysis delay time 


dtype=$1
datadir=$2
sdate=$3
edate=$4
WSHOME=$5
WSUSER=$6
WS=$7
scripts=$8
gscripts=$9
ttm=${10}


export SUBJOB=/global/save/Fanglin.Yang/VRFY/vsdb/bin/sub_wcoss
export  ACCOUNT=GDAS-T2O
export GROUP=dev
export CUE2FTP=transfer

echo $dtype

dtype=` echo $dtype |cut -c1-1`

### make host plot files directory


dtim=${sdate}-${edate}

for tm in  $ttm 

do

if [ "$ddtype" = 'u' ]; then
  export hist_dir=${WSHOME}/web/regional/hist//${tm}/${dtype}_dir
  export hist_sp=${WSHOME}/web/regional/hist/${tm}/${dtype}_sp
  export WSMAP_dir=${hist_dir}
  export WSMAP_sp=${hist_sp}
else
    export hist=${WSHOME}/web/regional/hist/${tm}/${dtype}
    WSMAP=$hist
fi
  tmpdir=/ptmpp1/$USER/gsiqc3/plothist_reg_${dtype}_${tm}

 mkdir -p $tmpdir

cd $tmpdir

rm -f *

ddtype=` echo $dtype |cut -c1-1` 
cp $datadir/${tm}/${dtype}_stas_hist.ctl ./tmp.ctl 
cp $datadir/${tm}/${dtype}_hist_grads  .
 rm -f tmp.gs
  if [ "$ddtype" = 'u' ]; then
    cp ${gscripts}/plothist_stas_wind.gs ./tmp.gs
  else
    cp ${gscripts}/plothist_stas.gs ./tmp.gs 
  fi
     

 if [ -s ${dtype}_hist_grads ]; then
   sdir=" dset ^${dtype}_hist_grads"
   sed -e "s/dset ^*/$sdir/" tmp.ctl >${dtype}_stas_hist.ctl 


   sed -e "s/DTYPE/$dtype/" -e "s/DTIME/'$dtim'/" tmp.gs >${dtype}_hist.gs

    
   echo 'quit' | grads -bpc "run ${dtype}_hist.gs"

cat << EOF >ftpcard$$.sh
#!/bin/ksh
set -x

  echo "yes" | ssh -l $WSUSER $WS "date"
  if [ "$ddtype" = 'u' ]; then
    ssh -l $WSUSER $WS "mkdir -p ${WSMAP_dir}"
    ssh -l $WSUSER $WS "mkdir -p ${WSMAP_sp}"
    scp *sp*png $WSUSER@$WS:${WSMAP_sp}
    scp *dir*png $WSUSER@$WS:${WSMAP_dir}
  else
    ssh -l $WSUSER $WS "mkdir -p ${WSMAP}"
    scp *png $WSUSER@$WS:$WSMAP
  fi
#    rm -f *png
EOF
chmod 755 $tmpdir/ftpcard$$.sh
$SUBJOB -a $ACCOUNT -q $CUE2FTP -g $GROUP -p 1/1/S -t 1:30:00 -r 64/1 -j ftpcrd -o $tmpdir/ftpcard$$.out $tmpdir/ftpcard$$.sh

 else
    echo " no $dtype data available"
 fi
done

exit

