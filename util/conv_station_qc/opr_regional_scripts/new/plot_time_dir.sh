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

#dtype=w280
#itype=0
#datadir=/ptmp/Xiujuan.Su/prepbufr
#sdate=2010030100
#edate=2010030518

#export scripts=/u/Xiujuan.Su/home/gsiqc3/scripts
#export gscripts=/u/Xiujuan.Su/home/gsiqc3/grads
#export time_dir=$WSHOME/web/regional/time/$dtype
#export WSMAP=${time_dir}
#export WSUSER=emc-lw-xsu
#export WS=lnx42.ncep.noaa.gov


### make host plot files directory

#--- preparation
for tm in 00 03 06 09 12
do
export time_dir=$WSHOME/web/regional/time/${tm}/$dtype
export WSMAP=${time_dir}

  tmpdir=/ptmp/Xiujuan.Su/plottime_reg_${dtype}_dir_${tm}

 mkdir -p $tmpdir

cd $tmpdir
rm -f *

ddtype=` echo $dtype | cut -c1-1` 
cp $datadir/${tm}/${dtype}_time.ctl  ./tmp.ctl
cp $datadir/${tm}/${dtype}_time  ./${dtype}_time
cp $datadir/${tm}/${dtype}_stdout2 ./${dtype}_stdout2
ntime=`tail -1 ${dtype}_stdout2`
 rm -f tmp_obs.gs
 rm -f tmp.gs
if [ $itype = 1 ]; then 
  if [ "$ddtype" = 'u' ]; then
    cp ${gscripts}/plot_time_wind_dir.gs ./tmp.gs
  fi
fi

 stdate=`expr $ntime - 123`
 if [ -s ${dtype}_time ]; then
   sdir=" dset ^${dtype}_time"

   sed -e "s/dset ^*/$sdir/"  tmp.ctl >${dtype}_time.ctl

    
     sed -e "s/DTYPE/$dtype/"  -e "s/STDATE/$stdate/" tmp.gs >${dtype}_time.gs

#   if [ -s tmp_obs.gs ]; then 
#     sed -e "s/DTYPE/$dtype/" -e "s/STDATE/$stdate/"  tmp_obs.gs >${dtype}_time_obs.gs
#     echo 'quit' |  grads -bpc "run ${dtype}_time_obs.gs"
#   fi

  echo "yes" | ssh -l $WSUSER $WS "date"
  ssh -l $WSUSER $WS "mkdir -p ${WSMAP}"
     echo 'quit' |  grads -bpc "run ${dtype}_time.gs"

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
    rm -f tmp.gs
    rm -f tmp_obs.gs
 else
    echo " no $dtype data available"
 fi

done

exit

