#!/bin/sh
set -xa

### define the script, data directory and excutable 
### file for each run

 
#tmpdate=`date '+%Y%m%d'`
#tdate=${tmpdate}00
#sdate=$(/nwprod/util/exec/ndate -24 $tdate)
#edate=$(/nwprod/util/exec/ndate -6 $tdate)
#export sdate=2012062900
#export edate=2012063018
# ppdate=20120205
 export scripts=/u/Xiujuan.Su/home/gsiqc3/regional_scripts
 export gscripts=/u/Xiujuan.Su/home/gsiqc3/grads
 export exec=/u/Xiujuan.Su/home/gsiqc3/exec
 export datadir=/com/nam/prod
 export exefile1=post_cnvdiag
 export exefile2=prepbufr.x
 export savedir=/ptmp/Xiujuan.Su/post_regional
 export savedir2=/u/Xiujuan.Su/nbns/postevent/data_regional
 export stasdir=/u/Xiujuan.Su/nbns/bufrstas_regional_${ppdate}
 export selectdir=/u/Xiujuan.Su/nbns/select_reg_${ppdate}
 export savedir3=/u/Xiujuan.Su/home/gsiqc3/regional_data/${ppdate}
 export fixfile=/nwprod/fix/nam_regional_convinfo.txt
 export stasdir_glb=/u/Xiujuan.Su/nbns/bufrstas
# sdate=2012052200
# sdate=2011040100
#  edate=2012062418
#  edate=2011053118
# pdate='201202-05'

tmpdir=/ptmp/Xiujuan.Su/ptime

mkdir -p $tmpdir

cd $tmpdir
#TANKDIR=/u/Xiujuan.Su/nbns/stats/convweb/copr
#cp $TANKDIR/cycle/prodate ./prodate
#export PDATE=`cat 'prodate'`
#export PDATE=`date +%Y%m%d%H`
export PDATE=2014012100

PDATE=$(/nwprod/util/exec/ndate -24 $PDATE)
tdate=`echo $PDATE|cut -c1-8`

sdate=${tdate}00
edate=${tdate}18

#sdate=2012070400
#edate=2012070518

mkdir -p $savedir
mkdir -p $savedir2
mkdir -p $stasdir
mkdir -p $savedir3

## define plot destination
export WSHOME=/export/lnx42/emc-lw-xsu/gsiqc3/$ppdate
export WSUSER=emc-lw-xsu
export WS=lnx42.ncep.noaa.gov

 export NDATE=/nwprod/util/exec/ndate
 export  ACCOUNT=GDAS-T2O
export SUB=/u/wx20mi/bin/sub
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
/bin/sh $scripts/rungsiqc_prep_reg.sh $scripts $exec $datadir ${exefile1} $savedir $sdate $edate

### read post prepbufr file and regrop the data into different types
/bin/sh $scripts/rungsiqc_type_reg.sh $scripts $exec $savedir ${exefile2} $savedir2 $sdate $edate $fixfile 


exit

