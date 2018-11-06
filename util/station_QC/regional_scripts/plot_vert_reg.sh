#!/bin/bash
set -xa

################################################################################
####  UNIX Script Documentation Block
# Script name: plot_vert_reg.sh
# Script description:this script plots regional vertical profile of O-A and O-B statistics
# Author: Xiujuan Su    Org: NP20         Date: 2010-09-12
# Abstract: this script plots vertical profile of O-A and O-B statistics

# Script history log:
# 2018-09-21: X. Su  clean up comment lines and document the script
#
# usage: plot_vert.sh dtype sdate edate datadir WSHOME WSUSER WS scripts gscripts dtim ttm
#
# dtype: data type such as  t120, uv220
# sdate: starting time
# edate: end of time
# datadir: statisticas data directory
# WSHOME: web site directory for monitor web site
# WSUSER: web site user name
# WS: the machine in which the monitor web site locates
# scripts:  the scripts directory
# gscripts: grad script directory
# dtim: the period of data processed
# ttm: regional analysis delay time

scripts=$1
gscripts=$2
dtype=$3
datadir=$4
WSHOME=$5
WSUSER=$6
WS=$7
sdate=$8
edate=$9
dtim=${10}
ttm=${11}


### make host plot files directory

#--- preparation

#for tm in 00 03 06 09 12
for tm in ${ttm}   
do

export vert=${WSHOME}/web/regional/vert/${tm}/$dtype
export WSMAP=${vert}
  tmpdir=/ptmpp1/$USER/gsiqc3/plotvert_reg_$dtype_${tm}

 mkdir -p $tmpdir

cd $tmpdir

#rm -f *

#dtim=${sdate}-${edate}

ddtype=` echo $dtype | cut -c1-1` 
cp $datadir/${tm}/${dtype}_stas.ctl ./tmp.ctl 
cp $datadir/${tm}/${dtype}_stas  .
 rm -f tmp.gs
   if [ "$ddtype" = 'u' ]; then
    cp ${gscripts}/plotvert_stas_spdir.gs ./tmp.gs
    cp ${gscripts}/plotvert_obs_w.gs ./tmp_obs.gs
    sed -e "s/DTYPE/$dtype/"  -e "s/DTIME/'${dtim}'/"  tmp.gs >${dtype}_vert.gs
     sdtype=sp
    sed -e "s/DTYPE/$dtype/" -e "s/DTIME/'${dtim}'/" -e "s/STYPE/$sdtype/" tmp_obs.gs >${dtype}_${sdtype}_vert.gs
    sdtype=dir
    sed -e "s/DTYPE/$dtype/" -e "s/DTIME/'${dtim}'/" -e "s/STYPE/$sdtype/"  tmp_obs.gs >${dtype}_${sdtype}_vert.gs
  else
    cp ${gscripts}/plotvert_stas.gs  ./tmp.gs
    cp ${gscripts}/plotvert_obs.gs  ./tmp_obs.gs
   sed -e "s/DTYPE/$dtype/"  -e "s/DTIME/'${dtim}'/"  tmp.gs >${dtype}_vert.gs
   sed -e "s/DTYPE/$dtype/"  -e "s/DTIME/'${dtim}'/"  tmp_obs.gs >${dtype}_vert_obs.gs
  fi
     

 if [ -s ${dtype}_stas ]; then
   sdir=" dset ^${dtype}_stas"
   sed -e "s/dset ^*/$sdir/" tmp.ctl >${dtype}_stas.ctl
 
   if [ ${ddtype} = 'u' ]; then
     echo 'quit' | grads -blc "run ${dtype}_vert.gs" 
     echo 'quit' | grads -blc "run ${dtype}_sp_vert.gs" 
     echo 'quit' | grads -blc "run ${dtype}_dir_vert.gs" 
  else
   echo 'quit' | grads -blc "run ${dtype}_vert.gs"
   echo 'quit' | grads -blc "run ${dtype}_vert_obs.gs"
fi

  echo "yes" | ssh -l $WSUSER $WS "date"
  ssh -l $WSUSER $WS "mkdir -p ${WSMAP}"

   scp *png $WSUSER@$WS:$WSMAP
    rm -f *png

 else
    echo " no $dtype data available"
 fi

done

exit

