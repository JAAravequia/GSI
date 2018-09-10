### the script plot horrizontal map of statistics
set -xa


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

#dtype=w221
#itype=1
#datadir=/ptmpp1/Xiujuan.Su/prepbufr
#sdate=2010030100
#edate=2010030118


#export scripts=/u/Xiujuan.Su/home/gsiqc3/scripts
#export gscripts=/u/Xiujuan.Su/home/gsiqc3/grads
export vert=${WSHOME}/web/global/vert/$dtype
export WSMAP=${vert}


### make host plot files directory

#--- preparation


  tmpdir=/ptmpp1/$USER/gsiqc3/plotvert_$dtype

 mkdir -p $tmpdir

cd $tmpdir

#rm -f *

#dtim=${sdate}-${edate}

ddtype=` echo $dtype | cut -c1-1` 
cp $datadir/${dtype}_stas.ctl ./tmp.ctl 
cp $datadir/${dtype}_stas  .
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



exit

