#!/bin/sh
set -xa

### define the script, data directory and excutable 
### file for each run
 
 ppdate='201308-10'
 export scripts=/u/Xiujuan.Su/home/gsiqc3/regional_scripts
 export gscripts=/u/Xiujuan.Su/home/gsiqc3/grads
 export exec=/u/Xiujuan.Su/home/gsiqc3/exec
 export datadir=/u/Xiujuan.Su/nbns/cnvdat_regional
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
 export exefile11=readwrite_list.x
 export exefile12=readwrite_biasrej.x
 export exefile13=make_list_time_reg.x
 export savedir=/u/Xiujuan.Su/nbns/postevent/post_regional
 export savedir2=/u/Xiujuan.Su/nbns/postevent/data_regional
 export stasdir=/ptmp/Xiujuan.Su/bufrstas_reg
 export selectdir=/ptmp/Xiujuan.Su/select_reg
 export savedir3=/u/Xiujuan.Su/home/gsiqc3/regional_data/${ppdate}
 export fixfile=/u/Xiujuan.Su/home/gsiqc3/fix/nam_regional_convinfo.txt
 export stasdir_glb=/ptmp/Xiujuan.Su/bufrstas
 export listdir=/ptmp/Xiujuan.Su/make_time_reg
 sdate=2012080100
# sdate=2011040100
  edate=2012102518
#  edate=2011053118

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
export USER=Xiujuan.Su

###  the types and subtypes to be processed, it has to go scripts to change the type or sab types
sub="ADPUPA PROFLR VADWND ADPSFC SFCSHP MSONET"
dtype="ps120 ps180 ps181 ps187 ps188 q120 q180 q181 q187 q188 t120 t180 t181 t187 t188 uv220 uv221 uv223 uv224 uv280 uv281 uv287 uv288 "
dtype1="ps120 q120 t120 uv220 uv221"
dtype2="ps120 uv221"
dtype3="uv223 uv229"
dtype4="uv224"
dtype5="ps181 ps187 q181 q187 t181 t187 uv281 uv287"
dtype6="ps180 q180 t180 uv280"
dtype7="ps188 q188 t188 uv288"

###

### put diagnostic file into prepbufr files
#/bin/sh $scripts/rungsiqc_prep_reg.sh $scripts $exec $datadir $exefile1 $savedir $sdate $edate $SUB

### read post prepbufr file and regrop the data into different types
#/bin/sh $scripts/rungsiqc_type_reg.sh $scripts $exec $savedir $exefile2 $savedir2 $sdate $edate $fixfile $SUB 

### read different data type to do statistics, in this step, 90 pencitile for station
### statistics isthe base to determine to criteria to put station to rejection or 
###bias correction lst  
#/bin/sh $scripts/rungsiqc_stas_reg.sh $scripts $exec $exefile3 $exefile4 ${savedir2} ${savedir3} $stasdir $sdate $edate $ppdate $fixfile $SUB

###  get final selection for the station with statistics from last step  
#/bin/sh $scripts/rungsiqc_selet_reg.sh $scripts $exec $exefile5 $stasdir $savedir3 $sdate $edate $ppdate $selectdir $SUB

### compare the rejection and bias correction lists from different time delay analysis 

#/bin/sh $scripts/runcompare_list.sh $ppdate $scripts $selectdir $stasdir $exec $exefile6 $exefile7 $savedir3 $SUB

### compare the rejection lists by quality control procedure from diferent time delay analysis

#/bin/sh $scripts/runcompare_list_qlyrej.sh $ppdate $scripts $selectdir $stasdir $exec $exefile8 $savedir3 

### compare the lists from regional with global

#/bin/sh $scripts/runcompare_reglb.sh $ppdate $scripts $savedir3 $stasdir_glb $exec $exefile9 ${exefile10} 

#  rewrite the list output format

#/bin/sh $scripts/runreadwrite_list.sh $ppdate $scripts $savedir3 $exec ${exefile11} 

### check the list of bias correction and rejection lists for possible overlap

#/bin/sh $scripts/runreadwrite_biasrej.sh $ppdate $scripts $savedir3 $exec ${exefile12} 
### plot all figures

#/bin/sh $scripts/plot.sh $scripts $gscripts $sdate $edate $stasdir $WSHOME $WSUSER $WS $ppdate $SUB 

## make the list the google map

#/bin/sh $scripts/runmake_time_list.sh $scripts $exec ${exefile13} $stasdir $savedir3 $listdir    

#/bin/sh  $scripts/process_regional_maps.sh $listdir $WSHOME $WSUSER $WS 


exit

