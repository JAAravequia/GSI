### the script plot horrizontal map of statistics

set -xa


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

#dtype=t120
#itype=1
#datadir=/ptmpp1/Xiujuan.Su/bufrstas
#sdate=2011010100
#edate=2011033118
#WSHOME=/export/emc-lw-xsu/wd20xs/gsiqc3
#WSUSER=wd20xs
#WS=emc-lw-xsu.ncep.noaa.gov
#scripts=/u/Xiujuan.Su/home/gsiqc3/scripts
#gscripts=/u/Xiujuan.Su/home/gsiqc3/grads

#dtype=w280
#itype=0
#datadir=/ptmpp1/Xiujuan.Su/prepbufr
#sdate=2010030100
#edate=2010030518

#export scripts=/u/Xiujuan.Su/home/gsiqc3/scripts
#export gscripts=/u/Xiujuan.Su/home/gsiqc3/grads
export time_obs=$WSHOME/web/global/time/$dtype
export WSMAP=${time_obs}
#export WSUSER=wd20xs
#export WS=emc-lw-xsu.ncep.noaa.gov
export SUBJOB=/global/save/Fanglin.Yang/VRFY/vsdb/bin/sub_wcoss
export  ACCOUNT=GDAS-T2O
export GROUP=dev
export CUE2FTP=transfer


### make host plot files directory


  tmpdir=/ptmpp1/$USER/gsiqc3/plottime_${dtype}_obs

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
    cp ${gscripts}/plot_time_wind_obs_sp.gs ./tmp_obs.gs
  else
    cp ${gscripts}/plot_time_obs.gs  ./tmp_obs.gs
  fi

fi

 stdate=`expr $ntime - 123`
  if [ "${stdate}" -le 0 ]; then
     stdate=1
  fi
# stdate=1
 if [ -s ${dtype}_time ]; then
   sdir=" dset ^${dtype}_time"

   sed -e "s/dset ^*/$sdir/"   tmp.ctl >${dtype}_time.ctl

     sed -e "s/DTYPE/$dtype/" -e "s/STDATE/$stdate/"  tmp_obs.gs >${dtype}_time_obs.gs
     
     echo 'quit' |  grads -bpc "run ${dtype}_time_obs.gs"

cat << EOF >ftpcard$$.sh
#!/bin/ksh
set -x

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

