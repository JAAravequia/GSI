#!/bin/sh
set -xa

### define the script, data directory and excutable 
### file for each run

export  sdate=2018070100
export  edate=2018083118
export  ppdate='201807-08'

 export scripts=/u/Xiujuan.Su/home/gsiqc3/opr_scripts
 export gscripts=/u/Xiujuan.Su/home/gsiqc3/grads
 export exec=/u/Xiujuan.Su/home/gsiqc3/exec
 export datadir=/gpfs/hps/nco/ops/com/gfs/prod
 export exefile1=post_cnvdiag
 export exefile2=prepbufr.x
 export exefile3=readdata.x
 export exefile4=stas.x
 export exefile5=select.x
 export exefile6=readwrite_list_glb.x
 export exefile7=make_list_time.x
 export exefile8=make_sdmlist.x
 export savedir=/u/Xiujuan.Su/nbns/postevent/data_global
 export savedir2=/u/Xiujuan.Su/nbns/postevent/data_global
 export stasdir=/ptmpp1/$USER/gsiqc3/bufrstas
 export  selectdir=/ptmpp1/$USER/gsiqc3/select
 export listdir=/ptmpp1/$USER/gsiqc3/make_list_global 
  export fixfile=/nwprod/fix/global_convinfo.txt
  export htmldir=/u/Xiujuan.Su/home/gsiqc3/html
  export mappngdir=/u/Xiujuan.Su/home/gsiqc3/mappng
  export bashdir=/u/Xiujuan.Su/home/gsiqc3/google_maps/google_v3
 export savedir3=/u/Xiujuan.Su/home/gsiqc3/data/$ppdate
 export sdmlistdir=/ptmpp1/$USER/gsiqc3/make_global_sdmlist

## define plot destination
export WSHOME=/export/emc-lw-xsu/wd20xs/gsiqc3/$ppdate
export WSUSER=wd20xs
export WS=emc-lw-xsu.ncep.noaa.gov

 export NDATE=/nwprod/util/exec/ndate
 export  ACCOUNT=GDAS-T2O
export SUB=/global/save/emc.glopara/svn/gfs/trunk.r42957/para/bin/psub

###  this data type category is for script rungsiqc_type.sh 
export sub="ADPUPA PROFLR VADWND ADPSFC SFCSHP"
export dtype1="ps120 q120 t120 uv220 uv221"
export dtype2="ps120 uv221"
export dtype3="uv223"
export dtype4="uv224 uv229"
export dtype5="ps181 ps187 q181 q187 t181 t187 uv281 uv287"
export dtype6="ps180 q180 t180 uv280"
###

##  divide data types into different category
##  surface data types, sonde data types for non wind data and wind data

export sfctype='ps120 ps180 ps181 ps187 q180 q181 q187 t180 t181 t187'
export sondtype='q120 t120'
export uvsfctype='uv280 uv281 uv287'
export uvsondtype='uv220 uv221 uv223 uv224 uv228 uv229'
export datatype='120 180 181 187 121 123 124 128 129'
#export datatype='180 '

### put diagnostic file into prepbufr files
##/bin/sh $scripts/rungsiqc_prep.sh $scripts $exec $datadir $exefile1 $savedir $sdate $edate

### read post prepbufr file and regrop the data into different types, there are some statements about types, need to check 
## There are some data types in rungsiqc_type.sh, have to go in to check whether
###  data types in this script covers all 
##/bin/sh $scripts/rungsiqc_type.sh $scripts $exec $savedir $exefile2 $savedir2 $sdate $edate $fixfile "$sub "$dtype1" "$dtype2" "$dtype3" "$dtype4" "$dtype5" "$dtype6" 

### read different data type to do statistics, in this step, 90 pencitile for station
### statistics isthe base to determine to criteria to put station to rejection or 
###bias correction lst  
/bin/sh $scripts/rungsiqc_stas.sh $scripts $exec $exefile3 $exefile4 $savedir2 $savedir3 $stasdir $sdate $edate $ppdate $fixfile "${sfctype}" "${sondtype}" "${uvsfctype}" "${uvsondtype}" 

###  get final selection for the station with statistics from last step  
/bin/sh $scripts/rungsiqc_selet.sh $scripts $exec $exefile5 $stasdir $savedir3 $sdate $edate $ppdate "$sfctype" "$sondtype" "$uvsfctype" "$uvsondtype"

##  rewrite the list output format
/bin/sh $scripts/runreadwrite_list.sh $ppdate $scripts $savedir3 $exec ${exefile6} "${sfctype}" "${sondtype}" "${uvsfctype}" "${uvsondtype}"  
#### plot all figures
/bin/sh $scripts/plot.sh $scripts $gscripts $sdate $edate $stasdir $WSHOME $WSUSER $WS $ppdate "${sfctype}" "${sondtype}" "${uvsfctype}" "${uvsondtype}" 

## make the list for google map plotting 
/bin/sh $scripts/runmake_time_list.sh $scripts $exec ${exefile7} $stasdir $savedir3 $listdir "${sfctype}" "${sondtype}" "${uvsfctype}" "${uvsondtype}"

##  make files for google map 
/bin/sh  $scripts/process_global_maps.bash $ppdate $listdir $WSHOME $WSUSER $WS "${sfctype}" "${sondtype}" "${uvsfctype}" "${uvsondtype}" $bashdir 

##  transfer html files
/bin/sh $scripts/transferhtml.sh $htmldir $WSHOME $WSUSER $WS $ppdate $mappngdir   

## make the list format as SDM list format 

/bin/sh $scripts/runmake_sdm_list.sh $scripts $exec ${exefile8} ${savedir3} ${sdmlistdir} "${datatype}" 

exit

