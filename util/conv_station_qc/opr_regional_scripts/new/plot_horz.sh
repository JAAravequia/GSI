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
scripts=$8
gscripts=${10}
dtime=${11}

#export scripts=/u/Xiujuan.Su/home/gsiqc3/regional_scripts
#export gscripts=/u/Xiujuan.Su/home/gsiqc3/grads
ddtype=` echo $dtype |cut -c1-1`
#if [ "$ddtype" = 'u' ]; then
#  export horz_dir=${WSHOME}/web/regional/horz/${dtype}_dir
#  export horz_sp=${WSHOME}/web/regional/horz/${dtype}_sp
#  export WSMAP_dir=$horz_dir
#  export WSMAP_sp=$horz_sp
#else
#    export horz=${WSHOME}/web/regional/horz/${dtype}
#    WSMAP=$horz
#fi


echo $dtype


### make host plot files directory


  for tm in 00 03 06 09 12
   do
if [ "$ddtype" = 'u' ]; then
  export horz_dir=${WSHOME}/web/regional/horz/${tm}/${dtype}_dir
  export horz_sp=${WSHOME}/web/regional/horz/${tm}/${dtype}_sp
  export WSMAP_dir=$horz_dir
  export WSMAP_sp=$horz_sp
else
    export horz=${WSHOME}/web/regional/horz/${tm}/${dtype}
    WSMAP=$horz
fi
  tmpdir=/ptmp/Xiujuan.Su/plothorz_reg_${dtype}_${tm}

 mkdir -p $tmpdir

cd $tmpdir

#rm -f *

ddtype=` echo $dtype |cut -c1-1` 
cp $datadir/${tm}/${dtype}_stas_station.ctl ./tmp.ctl 
cp $datadir/${tm}/${dtype}_stas_station  .
 rm -f tmp.gs
if [ $itype = 0 ]; then
  if [ "$ddtype" = 'u' ]; then
    cp ${gscripts}/plot_horz_spdir_reg_sfc.gs ./tmp.gs
  else
    cp ${gscripts}/plot_sfc_horz_reg.gs ./tmp.gs 
  fi
elif [ $itype = 1 ]; then
   if [ "$ddtype" = 'u' ]; then
    cp ${gscripts}/plot_horz_spdir_reg.gs ./tmp.gs
  else
    cp ${gscripts}/plot_horz_reg.gs  ./tmp.gs
  fi
fi
     

 if [ -s ${dtype}_stas_station ]; then
   sdir=" dset ^${dtype}_stas_station"
   sed -e "s/dset ^*/$sdir/" tmp.ctl >${dtype}_stas_station.ctl 

   stnmap -i ${dtype}_stas_station.ctl
 
   if [ -s ${dtype}.map ]; then
     sed -e "s/DTYPE/$dtype/" \
     -e "s/RDATE/'$dtime'/" tmp.gs >${dtype}_horz.gs
     
     echo 'quit' | grads -blc "run ${dtype}_horz.gs"
  echo "yes" | ssh -l $WSUSER $WS "date"
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
    rm -f *png
  fi

 else
    echo " no $dtype data available"
 fi

done

exit

