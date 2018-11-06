#!/bin/bash
set -xa

################################################################################
####  UNIX Script Documentation Block
# Script name:  plot_horz.sh
# Script description:the script plots horizontal map of statistics
#
# Author: Xiujuan Su    Org: NP20         Date: 2010-09-12
# Script history log:
# 2018-09-12: X. Su  clean up comment lines and document the script
#
# usage: plot_horz.sh dtype itype datadir sdate edate WSHOME WSUSER WS scripts gscripts dtime 
#
# dtype: data type such as  t120, uv220
# itype: digit number of type
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
itype=$2
datadir=$3
sdate=$4
edate=$5
WSHOME=$6
WSUSER=$7
WS=$8
scripts=$8
gscripts=${10}
dtime=${11}

export SUBJOB=/global/save/Fanglin.Yang/VRFY/vsdb/bin/sub_wcoss
export  ACCOUNT=GDAS-T2O
export GROUP=dev
export CUE2FTP=transfer


ddtype=` echo $dtype |cut -c1-1` 
if [ "$ddtype" = 'u' ]; then
  export horz_dir=${WSHOME}/web/global/horz/${dtype}_dir
  export horz_sp=${WSHOME}/web/global/horz/${dtype}_sp
  export WSMAP_dir=$horz_dir
  export WSMAP_sp=$horz_sp
else
    export horz=${WSHOME}/web/global/horz/${dtype}
    WSMAP=$horz
fi

echo $dtype


### make host plot files directory


  tmpdir=/ptmpp1/$USER/gsiqc3/plothorz_glb_${dtype}

 mkdir -p $tmpdir

cd $tmpdir

rm -f *ipng

cp $datadir/${dtype}_stas_station.ctl ./tmp.ctl 
cp $datadir/${dtype}_stas_station  .
cp /u/$USER/home/grads/gslib/page.gs ./page.gs
cp /u/$USER/home/grads/gslib/rgbset2.gs ./rgbset2.gs
 rm -f tmp.gs
if [ $itype = 0 ]; then
  if [ "$ddtype" = 'u' ]; then
    cp ${gscripts}/plot_horz_spdir_sfc.gs ./tmp.gs
  else
    cp ${gscripts}/plot_sfc_horz.gs  ./tmp.gs 
  fi
elif [ $itype = 1 ]; then
   if [ "$ddtype" = 'u' ]; then
    cp ${gscripts}/plot_horz_spdir.gs ./tmp.gs
  else
    cp ${gscripts}/plot_horz.gs  ./tmp.gs
  fi
fi
     

 if [ -s ${dtype}_stas_station ]; then
   sdir=" dset ^${dtype}_stas_station"
   sed -e "s/dset ^*/$sdir/" tmp.ctl >${dtype}_stas_station.ctl 

   stnmap -i ${dtype}_stas_station.ctl
 
   if [ -s ${dtype}.map ]; then
     sed -e "s/DTYPE/$dtype/" \
     -e "s/RDATE/'$dtime'/"  tmp.gs >${dtype}_horz.gs
     echo 'quit' | grads -blc "run ${dtype}_horz.gs"

  cd $tmpdir 
 
 cat << EOF >ftpcard$$.sh
#!/bin/ksh
set -x

  if [ "$ddtype" = 'u' ]; then
  ssh -l $WSUSER $WS "mkdir -p ${WSMAP_dir}"
  ssh -l $WSUSER $WS "mkdir -p ${WSMAP_sp}"
     scp *dir*region1*png $WSUSER@$WS:${WSMAP_dir}
     scp *dir*region2*png $WSUSER@$WS:${WSMAP_dir}
     scp *dir*region3*png $WSUSER@$WS:${WSMAP_dir}
     scp *dir*region4*png $WSUSER@$WS:${WSMAP_dir}
     scp *dir*region5*png $WSUSER@$WS:${WSMAP_dir}
     scp *dir*region6*png $WSUSER@$WS:${WSMAP_dir}
     scp *dir*region7*png $WSUSER@$WS:${WSMAP_dir}
     scp *dir*region8*png $WSUSER@$WS:${WSMAP_dir}
     scp *dir*region9*png $WSUSER@$WS:${WSMAP_dir}
     scp *sp*region1*png $WSUSER@$WS:${WSMAP_sp}
     scp *sp*region2*png $WSUSER@$WS:${WSMAP_sp}
     scp *sp*region3*png $WSUSER@$WS:${WSMAP_sp}
     scp *sp*region4*png $WSUSER@$WS:${WSMAP_sp}
     scp *sp*region5*png $WSUSER@$WS:${WSMAP_sp}
     scp *sp*region6*png $WSUSER@$WS:${WSMAP_sp}
     scp *sp*region7*png $WSUSER@$WS:${WSMAP_sp}
     scp *sp*region8*png $WSUSER@$WS:${WSMAP_sp}
     scp *sp*region9*png $WSUSER@$WS:${WSMAP_sp}
   else
     ssh -l $WSUSER $WS "mkdir -p ${WSMAP}"
     scp *region1*png $WSUSER@$WS:$WSMAP
     scp *region2*png $WSUSER@$WS:$WSMAP
     scp *region3*png $WSUSER@$WS:$WSMAP
     scp *region4*png $WSUSER@$WS:$WSMAP
     scp *region5*png $WSUSER@$WS:$WSMAP
     scp *region6*png $WSUSER@$WS:$WSMAP
     scp *region7*png $WSUSER@$WS:$WSMAP
     scp *region8*png $WSUSER@$WS:$WSMAP
     scp *region9*png $WSUSER@$WS:$WSMAP
   fi
EOF
#    rm -f *png
chmod 755 $tmpdir/ftpcard$$.sh
$SUBJOB -a $ACCOUNT -q $CUE2FTP -g $GROUP -p 1/1/S -t 1:30:00 -r 64/1 -j ftpcrd -o $tmpdir/ftpcard$$.out $tmpdir/ftpcard$$.sh

  fi

 else
    echo " no $dtype data available"
 fi



exit

