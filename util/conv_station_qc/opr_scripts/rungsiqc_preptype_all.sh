#!/bin/sh
set -xa

### define the script, data directory and excutable 
### file for each run

export scripts=/u/Xiujuan.Su/home/gsiqc3/opr_scripts
 export gscripts=/u/Xiujuan.Su/home/gsiqc3/grads
 export exec=/u/Xiujuan.Su/home/gsiqc3/exec
 export datadir=/com2/gfs/prod
 export exefile1=post_cnvdiag
 export exefile2=prepbufr.x
 export savedir=/u/Xiujuan.Su/nbns/postevent/data_global
 export fixfile=/nwprod/fix/global_convinfo.txt
 ## define plot destination
export WSHOME=/export/emc-lw-xsu/wd20xs/gsiqc3/$ppdate
export WSUSER=wd20xs
export WS=emc-lw-xsu.ncep.noaa.gov
export NCP=/bin/cp
export NDATE=/nwprod/util/exec/ndate


#  set time
tmpdir=/ptmpp1/$USER/gsiqc3/ptime_glb

mkdir -p $tmpdir

cd $tmpdir


#TANKDIR=/u/Xiujuan.Su/nbns/stats/convweb/copr
#cp $TANKDIR/cycle/prodate ./prodate
#export PDATE=`cat 'prodate'`
#
#export PDATE=`date +%Y%m%d%H`
#PDATE=2017020500
#PDATE=$1
PDATE=$(/nwprod/util/exec/ndate -24 $PDATE)
tdate=`echo $PDATE|cut -c1-8`

sdate=2017042400
#edate=2016120118
edate=2017060418



## define plot destination

 export NDATE=/nwprod/util/exec/ndate
 export  ACCOUNT=GDAS-T2O
export SUB=/gpfs/td1/emc/global/save/emc.glopara/svn/gfs/trunk/para/bin/sub_wcoss
mkdir -p $savedir
mkdir -p $savedir2

###  The types and subtypes to be processed, it has to go scripts to change
###  the type or sub types
sub="ADPUPA PROFLR VADWND ADPSFC SFCSHP"
dtype1="ps120 q120 t120 uv220 uv221"
dtype2="ps120 uv221"
dtype3="uv223"
dtype4="uv224 uv229"
dtype5="ps181 ps187 q181 q187 t181 t187 uv281 uv287"
dtype6="ps180 q180 t180 uv280"
###

### put diagnostic file into prepbufr files
#/bin/sh $scripts/rungsiqc_prep.sh $scripts $datadir $savedir $sdate $edate

### read post prepbufr file and regrop the data into different types

/bin/sh $scripts/rungsiqc_type.sh $scripts $exec $savedir $exefile2 $savedir $sdate $edate $fixfile "$sub" "$dtype1" "$dtype2" "$dtype3" "$dtype4" "$dtype5" "$dtype6" 

exit
