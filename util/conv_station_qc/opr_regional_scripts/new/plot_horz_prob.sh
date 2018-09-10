### the script plot horrizontal map of statistics
set -xa


dtype=$1
itype=$2
datadir=$3
sdate=$4
edate=$5
#dtype=ps120
#itype=0
#datadir=/ptmp/Xiujuan.Su/prepbufr
#sdate=2010030100
#edate=2010053118


export scripts=/u/Xiujuan.Su/home/gsiqc3/scripts
export gscripts=/u/Xiujuan.Su/home/gsiqc3/grads
export WSHOME=/export/lnx42/emc-lw-xsu/gsiqc3/$dtype/horz_sus/${sdate}-${edate}
export WSMAP=$WSHOME/png
export WSUSER=emc-lw-xsu
export WS=lnx42.ncep.noaa.gov

echo $dtype


### make host plot files directory

#--- preparation
  echo "yes" | ssh -l $WSUSER $WS "date"
  ssh -l $WSUSER $WS "mkdir -p ${WSMAP}"



  tmpdir=/ptmp/Xiujuan.Su/plothorz_prob/$dtype

 mkdir -p $tmpdir

cd $tmpdir

rm -f *

ddtype=` echo $dtype |cut -c1-1` 
cp $datadir/${dtype}_stas_station.ctl ./tmp.ctl 
cp $datadir/${dtype}_susplist_stas_station  .
 rm -f tmp.gs
if [ $itype = 0 ]; then
  if [ "$ddtype" = 'w' ]; then
    cp ${gscripts}/plot_horz_spdir.gs ./tmp.gs
  else
    cp ${gscripts}/plot_sfc_horz.gs  ./tmp.gs 
  fi
elif [ $itype = 1 ]; then
   if [ "$ddtype" = 'w' ]; then
    cp ${gscripts}/plot_horz_spdir.gs ./tmp.gs
  else
    cp ${gscripts}/plot_horz.gs  ./tmp.gs
  fi
fi
     

 if [ -s ${dtype}_susplist_stas_station ]; then
   sdir=" dset ^${dtype}_susplist_stas_station"
   sed -e "s/dset ^*/$sdir/" tmp.ctl >${dtype}_stas_station.ctl 

   stnmap -i ${dtype}_stas_station.ctl 

   sed -e "s/DTYPE/$dtype/" tmp.gs >${dtype}_horz.gs
    
   echo 'quit' | grads -blc "run ${dtype}_horz.gs"

   scp *png $WSUSER@$WS:$WSMAP
    rm -f *png

 else
    echo " no $dtype data available"
 fi

exit

