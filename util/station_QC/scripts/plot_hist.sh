#!/bin/bash
set -xa

################################################################################
####  UNIX Script Documentation Block
# Script name: plot_hist.sh 
# Script description:this script makes histogram plots  
# Author: Xiujuan Su    Org: NP20         Date: 2010-09-12
# Abstract: this script makes histogram plots 
#
# Script history log:
# 2018-09-12: X. Su  clean up comment lines and document the script
#
# usage: plot_hist.sh dtype datadir sdate edate WSHOME WSUSER WS scripts gscripts dtim
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
# dtim: the period of data processed


dtype=$1
datadir=$2
sdate=$3
edate=$4
WSHOME=$5
WSUSER=$6
WS=$7
scripts=$8
gscripts=$9
dtim=${10}

export hist=${WSHOME}/web/global/hist/$dtype
export WSMAP=${hist}


echo $dtype

ddtype=` echo $dtype |cut -c1-1`
if [ "$ddtype" = 'u' ]; then
export hist_dir=${WSHOME}/web/global/hist/${dtype}_dir
export hist_sp=${WSHOME}/web/global/hist/${dtype}_sp
export WSMAP_dir=$hist_dir
export WSMAP_sp=$hist_sp
fi

### make host plot files directory


#dtim=${sdate}-${edate}


  tmpdir=/ptmpp1/$USER/gsiqc3/plothist_$dtype

 mkdir -p $tmpdir

cd $tmpdir

#rm -f *

ddtype=` echo $dtype |cut -c1-1` 
cp $datadir/${dtype}_stas_hist.ctl ./tmp.ctl 
cp $datadir/${dtype}_hist_grads  .
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

   ssh -l Xiujuan.S 
   if [ "$ddtype" = 'u' ]; then
    ssh -l $WSUSER $WS "mkdir -p ${WSMAP_dir}"
    ssh -l $WSUSER $WS "mkdir -p ${WSMAP_sp}"
    scp *dir*png $WSUSER@$WS:${WSMAP_dir}
    scp *sp*png $WSUSER@$WS:${WSMAP_sp}
  else
    ssh -l $WSUSER $WS "mkdir -p ${WSMAP}"
    scp *png $WSUSER@$WS:$WSMAP
  fi
    rm -f *png

 else
    echo " no $dtype data available"
 fi
 cd ..


exit

