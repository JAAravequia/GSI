#!/bin/bash
set -xa

################################################################################
####  UNIX Script Documentation Block
# Script name: plot_time_obs_dir.sh
# Script description:this script plots time series of wind direction observation statistics
# Author: Xiujuan Su    Org: NP20         Date: 2010-09-12
# Abstract: this script plots time series of wind direction observation statistics

# Script history log:
# 2018-09-12: X. Su  clean up comment lines and document the script
#
# usage: plot_time_obs_dir.sh dtype itype sdate edate datadir WSHOME WSUSER WS scripts gscripts
#
# dtype: data type such as  t120, uv220
# itype: digit number of type
# sdate: starting time
# edate: end of time
# datadir: statisticas data directory
# WSHOME: web site directory for monitor web site
# WSUSER: web site user name
# WS: the machine in which the monitor web site locates
# scripts:  the scripts directory
# gscripts: grad script directory

dtype=$1
itype=$2
datadir=$3
sdate=$4
edate=$5
WSHOME=$6
WSUSER=$7
WS=$8
scripts=$9
gscripts=${10}


export WSMAP=$WSHOME/web/global/time/$dtype
export SUBJOB=/global/save/Fanglin.Yang/VRFY/vsdb/bin/sub_wcoss
export  ACCOUNT=GDAS-T2O
export GROUP=dev
export CUE2FTP=transfer



### make host plot files directory

  tmpdir=/ptmpp1/$USER/gsiqc3/plottime_${dtype}_obs_dir

 mkdir -p $tmpdir

cd $tmpdir
rm -f *

ddtype=` echo $dtype | cut -c1-1` 
cp $datadir/${dtype}_time.ctl  ./tmp.ctl
cp $datadir/${dtype}_time  ./${dtype}_time
cp $datadir/${dtype}_stdout2 ./${dtype}_stdout2
ntime=`tail -1 ${dtype}_stdout2`

 rm -f tmp_obs.gs
 rm -f tmp.gs
if [ $itype = 1 ]; then 
  if [ "$ddtype" = 'u' ]; then
    cp ${gscripts}/plot_time_wind_obs_dir.gs ./tmp_obs.gs
  fi

fi

 if [ -s ${dtype}_time ]; then
   sdir=" dset ^${dtype}_time"

   stdate=`expr $ntime - 123`
  if [ "${stdate}" -le 0 ] ; then
     stdate=1
  fi
#   stdate=1
   sed -e "s/dset ^*/$sdir/"   tmp.ctl >${dtype}_time.ctl

     sed -e "s/DTYPE/$dtype/" -e "s/STDATE/$stdate/"  tmp_obs.gs >${dtype}_time_obs.gs
     
     echo 'quit' |  grads -bpc "run ${dtype}_time_obs.gs"
cat << EOF >ftpcard$$.sh

  echo "yes" | ssh -l $WSUSER $WS "date"
  ssh -l $WSUSER $WS "mkdir -p ${WSMAP}"

   scp ${dtype}*1*png $WSUSER@$WS:$WSMAP
    rm -f ${dtype}*1*png
   scp ${dtype}*2*png $WSUSER@$WS:$WSMAP
    rm -f ${dtype}*2*png
   scp ${dtype}*3*png $WSUSER@$WS:$WSMAP
    rm -f ${dtype}*3*png
   scp ${dtype}*4*png $WSUSER@$WS:$WSMAP
    rm -f ${dtype}*4*png
   scp ${dtype}*5*png $WSUSER@$WS:$WSMAP
    rm -f ${dtype}*5*png
   scp ${dtype}*6*png $WSUSER@$WS:$WSMAP
    rm -f ${dtype}*6*png
   scp ${dtype}*7*png $WSUSER@$WS:$WSMAP
    rm -f ${dtype}*7*png
   scp *png $WSUSER@$WS:$WSMAP
    rm -f *png

EOF

chmod 755 $tmpdir/ftpcard$$.sh
$SUBJOB -a $ACCOUNT -q $CUE2FTP -g $GROUP -p 1/1/S -t 1:30:00 -r 64/1 -j ftpcrd -o $tmpdir/ftpcard$$.out $tmpdir/ftpcard$$.sh

 else
    echo " no $dtype data available"
 fi


exit

