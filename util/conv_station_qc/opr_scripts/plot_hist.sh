### the script plot horrizontal map of statistics
set -ax


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


#dtype=w220
#datadir=/ptmpp1/Xiujuan.Su/prepbufr
#sdate=2010030100
#edate=2010053118
#export scripts=/u/Xiujuan.Su/home/gsiqc3/scripts
#export gscripts=/u/Xiujuan.Su/home/gsiqc3/grads
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

