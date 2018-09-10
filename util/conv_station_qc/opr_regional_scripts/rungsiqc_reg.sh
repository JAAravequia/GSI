#!/bin/sh
set -xa

### define the script, data directory and excutable 
### file for each run
 
export sdate=2018070100
export edate=2018083118
export ppdate='201807-08'
 export scripts=/u/Xiujuan.Su/home/gsiqc3/opr_regional_scripts
 export gscripts=/u/Xiujuan.Su/home/gsiqc3/grads
 export exec=/u/Xiujuan.Su/home/gsiqc3/exec
 export datadir=/com/nam/prod
 export exefile1=post_cnvdiag
 export exefile2=prepbufr.x
 export exefile3=readdata.x
 export exefile4=stas.x
 export exefile5=select_reg.x
 export exefile6=compare_list.x
 export exefile7=compare_list_sp.x
 export exefile8=compare_list_qlyrej.x
 export exefile9=compare_list_reg_glb.x
 export exefile10=compare_list_reg_glb_sp.x
 export exefile11=readwrite_list_reg.x
 export exefile12=readwrite_biasrej.x
 export exefile13=make_list_time_reg.x
 export exefile14=make_sdmlist.x
 export savedir=/ptmpp1/$USER/gsiqc3/post_regional
 export savedir2=/u/Xiujuan.Su/nbns/postevent/data_regional
 export stasdir=/ptmpp1/$USER/gsiqc3/bufrstas_reg
 export selectdir=/ptmpp1/$USER/gsiqc3/select_reg
 export savedir3=/u/Xiujuan.Su/home/gsiqc3/regional_data/${ppdate}
 export global_data=/u/Xiujuan.Su/home/gsiqc3/data/$ppdate
# export fixfile=/u/Xiujuan.Su/home/gsiqc3/fix/nam_regional_convinfo.txt
 export fixfile=/nwprod/fix/nam_regional_convinfo.txt
 export stasdir_glb=/ptmpp1/$USER/gsiqc3/bufrstas
 export listdir=/ptmpp1/$USER/gsiqc3/make_list_region
 export htmldir=/u/Xiujuan.Su/home/gsiqc3/html
 export bashdir=/u/Xiujuan.Su/home/gsiqc3/google_maps/google_v3
  export sdmlistdir=/ptmpp1/$USER/gsiqc3/make_reg_sdmlist
 export datatype='120 180 181 187 188 121 123 124 128 129'

mkdir -p $savedir
mkdir -p $savedir2
mkdir -p $stasdir
mkdir -p $savedir3

## define plot destination
export WSHOME=/export/emc-lw-xsu/wd20xs/gsiqc3/$ppdate
export WSUSER=wd20xs
export WS=emc-lw-xsu.ncep.noaa.gov

 export NDATE=/nwprod/util/exec/ndate
 export  ACCOUNT=GDAS-T2O
export SUB=/gpfs/gd1/emc/global/save/emc.glopara/svn/gfs/trunk/para/bin/sub_wcoss

###  the types and subtypes to be processed, it has to go scripts to change the type or sab types
sub="ADPUPA PROFLR VADWND ADPSFC SFCSHP MSONET"
dtype1="ps120 q120 t120 uv220 uv221"
dtype2="ps120 uv221"
dtype3="uv223 uv229"
dtype4="uv224"
dtype5="ps181 ps187 q181 q187 t181 t187 uv281 uv287"
dtype6="ps180 q180 t180 uv280"
dtype7="ps188 q188 t188 uv288"


export sfctype='ps120 ps180 ps181 ps187 ps188 q180 q181 q187 q188 t180 t181 t187 t188'
export sondtype='q120 t120'
export uvsfctype='uv280 uv281 uv287 uv288'
#export uvsfctype='uv288'
export uvsondtype='uv220 uv221 uv223 uv224 uv228 uv229' 

export tm='00 03 06' 

###

### put diagnostic file into prepbufr files
##/bin/sh $scripts/rungsiqc_prep_reg.sh $scripts $exec $datadir $exefile1 $savedir $sdate $edate "${tm}" 

### read post prepbufr file and regrop the data into different types
##/bin/sh $scripts/rungsiqc_type_reg.sh $scripts $exec $savedir $exefile2 $savedir2 $sdate $edate $fixfile "$sub" "$dtype1" "$dtype2" "$dtype3" "$dtype4" "$dtype5" "$dtype6" "$dtype7" ${exefile3} $datadir "${tm}"

### read different data type to do statistics, in this step, 90 pencitile for station
### statistics isthe base to determine to criteria to put station to rejection or 
###bias correction lst  
/bin/sh $scripts/rungsiqc_stas_reg.sh $scripts $exec $exefile3 $exefile4 ${savedir2} ${savedir3} $stasdir $sdate $edate $ppdate $fixfile "${sfctype}" "${sondtype}" "${uvsfctype}" "${uvsondtype}" "${tm}"

###  get final selection for the station with statistics from last step  
/bin/sh $scripts/rungsiqc_selet_reg.sh $scripts $exec $exefile5 $stasdir $savedir3 $sdate $edate $ppdate $selectdir "${sfctype}" "${sondtype}" "${uvsfctype}" "${uvsondtype}" "${tm}" 

### compare the rejection and bias correction lists from different time delay analysis 

/bin/sh $scripts/runcompare_list.sh $ppdate $scripts $selectdir $stasdir $exec $exefile6 $exefile7 $savedir3 "${sfctype}" "${sondtype}" "${uvsfctype}" "${uvsondtype}" "${tm}" 

### compare the rejection lists by quality control procedure from diferent time delay analysis

/bin/sh $scripts/runcompare_list_qlyrej.sh $ppdate $scripts $selectdir $stasdir $exec $exefile8 $savedir3 "${sfctype}" "${sondtype}" "${uvsfctype}" "${uvsondtype}" "${tm}" 


/bin/sh $scripts/runreadwrite_list.sh $ppdate $scripts $savedir3 $exec ${exefile11} "${sfctype}" "${sondtype}" "${uvsfctype}" "${uvsondtype}"

/bin/sh $scripts/plot.sh $scripts $gscripts $sdate $edate $stasdir $WSHOME $WSUSER $WS $ppdate "${sfctype}" "${sondtype}" "${uvsfctype}" "${uvsondtype}" "${tm}"

## make the list the google map

/bin/sh $scripts/runmake_time_list.sh $scripts $exec ${exefile13} $stasdir $savedir3 $listdir ${global_data} "${sfctype}" "${sondtype}" "${uvsfctype}" "${uvsondtype}" "${tm}"    

/bin/sh  $scripts/process_regional_maps.sh $ppdate ${listdir} ${WSHOME} ${WSUSER} $WS "${sfctype}" "${sondtype}" "${uvsfctype}" "${uvsondtype}" $bashdir "${tm}" 

##  transfer html files
/bin/sh $scripts/transferhtml.sh $htmldir $WSHOME $WSUSER $WS $ppdate

## make the list format as SDM list format

/bin/sh $scripts/runmake_sdm_list.sh $scripts $exec ${exefile14} ${savedir3} ${sdmlistdir} "${datatype}"

exit

