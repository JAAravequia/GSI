#!/bin/sh
set -xa

### define the script, data directory and excutable 
### file for each run
 export scripts=/u/Xiujuan.Su/home/gsiqc3/regional_scripts
 export gscripts=/u/Xiujuan.Su/home/gsiqc3/grads
 export exec=/u/Xiujuan.Su/home/gsiqc3/exec
 export datadir=/u/Xiujuan.Su/nbns/prd11q1f
 export exefile1=post_cnvdiag
 export exefile2=prepbufr.x
 export exefile3=readdata.x
 export exefile4=stas.x
 export exefile5=select.x
 export savedir=/u/Xiujuan.Su/nbns/postevent/post_regional
 export savedir2=/u/Xiujuan.Su/nbns/postevent/data_regional
 export stasdir=/ptmp/Xiujuan.Su/bufrstas_regional
 export fixfile=/u/Xiujuan.Su/home/gsiqc3/fix/nam_regional_convinfo.txt
 sdate=2011040100
 edate=2011043018
#  edate=2011053118
 pdate=201104-05
 export savedir3=/u/Xiujuan.Su/home/gsiqc3/regional_data/$pdate

mkdir -p $savedir
mkdir -p $savedir2
mkdir -p $stasdir
mkdir -p $savedir3

## define plot destination
export WSHOME=/export/lnx42/emc-lw-xsu/gsiqc3_regional
export WSUSER=emc-lw-xsu
export WS=lnx42.ncep.noaa.gov

 export NDATE=/nwprod/util/exec/ndate
 export  ACCOUNT=GDAS-T2O
export SUB=/u/wx20mi/bin/sub
export USER=Xiujuan.Su

###  the types and subtypes to be processed, it has to go scripts to change the type or sab types
sub="ADPUPA PROFLR VADWND ADPSFC SFCSHP "
dtype1="ps120 q120 t120 uv220 uv221"
dtype3="uv223"
dtype4="uv224 uv229"
dtype5="ps181 ps187"
dtype6="ps180 q180 t180 uv280"
###

### put diagnostic file into prepbufr files
#/bin/sh $scripts/rungsiqc_prep_regional.sh $scripts $exec $datadir $exefile1 $savedir $sdate $edate

### read post prepbufr file and regrop the data into different types
#/bin/sh $scripts/rungsiqc_type_regional.sh $scripts $exec $savedir $exefile2 $savedir2 $sdate $edate $fixfile 

### read different data type to do statistics, in this step, 90 pencitile for station
### statistics isthe base to determine to criteria to put station to rejection or 
###bias correction lst  
/bin/sh $scripts/rungsiqc_stas_regional.sh $scripts $exec $exefile3 $exefile4 $savedir2 $savedir3 $stasdir $sdate $edate $pdate $fixfile

###  get final selection for the station with statistics from last step  
#/bin/sh $scripts/rungsiqc_selet_regional.sh $scripts $exec $exefile5 $stasdir $savedir3 $sdate $edate $pdate

### plot all figures

#/bin/sh $scripts/plot_regional.sh $scripts $gscripts $sdate $edate $stasdir $WSHOME $WSUSER $WS 

exit

