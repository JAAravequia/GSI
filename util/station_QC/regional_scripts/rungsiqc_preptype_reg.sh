#!/bin/sh
set -xa
################################################################################
####  UNIX Script Documentation Block
# Script name: rungsiqc_preptype_reg.sh
# Script description:this script is a driver to run rungisqc_prep_reg.sh and 
# rungsi_type_reg.sh
# Author: Xiujuan Su    Org: NP20         Date: 2010-09-12

# Script history log:
# 2018-09-25: X. Su  clean up comment lines and document the script
#
# usage: rungsiqc_preptype_reg.sh rungsiqc_prep_reg.sh rungsiqc_type_reg.sh
#
# rungsiqc_prep_reg.sh: this script put gsi diagnotic file to prepbufr
# rungsiqc_type_reg.sh: the script put exch cycle information into types and stations
#
# scripts: the scripts directory
# exec   : the excutable file directory
# datadir: The prepbufr file and GSI diagnostic file directory
# exefile: the excutable file to put GSI diagnostic file to prepbufr file 
# exefile2: the excutable file to read prepbufr file and put observation information into 
#            each station of the type.
# exefile3: read GSI diagnostic file and put information to each station of type, this is for profile
#           winds, which is not in prepbufr file
# savedir:  the directory where prepbufr post and data files with station and type information saved   
# fixfile: convinfo file directory and file name


### define the script, data directory and excutable 
### file for each run

 
 export scripts=/u/Xiujuan.Su/home/gsiqc3/regional_scripts
 export exec=/u/Xiujuan.Su/home/gsiqc3/exec
 export datadir=/com2/nam/prod
 export exefile1=post_cnvdiag
 export exefile2=prepbufr.x
 export exefile3=cnvstat.x
 export savedir=/u/Xiujuan.Su/nbns/postevent/data_regional
 export fixfile=/nwprod/fix/nam_regional_convinfo.txt


tmpdir=/ptmpp1/$USER/gsiqc3/ptime

mkdir -p $tmpdir

cd $tmpdir
export PDATE=`date +%Y%m%d%H`

#export PDATE=$1

PDATE=$(/nwprod/util/exec/ndate -24 $PDATE)
tdate=`echo $PDATE|cut -c1-8`

sdate=${tdate}00
edate=${tdate}18


mkdir -p $savedir
mkdir -p $savedir2
mkdir -p $stasdir

## define plot destination
export WSHOME=/export/lnx42/emc-lw-xsu/gsiqc3/$ppdate
export WSUSER=emc-lw-xsu
export WS=lnx42.ncep.noaa.gov

 export NDATE=/nwprod/util/exec/ndate
export SUB=/u/wx20mi/bin/sub

###  the types and subtypes to be processed, it has to go scripts to change the type or sab types
tm='00 03 06'
sub="ADPUPA PROFLR ADPSFC SFCSHP MSONET"
dtype="ps120 ps180 ps181 ps187 ps188 q120 q180 q181 q187 q188 t120 t180 t181 t187 t188 uv220 uv221 uv223 uv224 uv280 uv281 uv287 uv288 "
dtype1="ps120 q120 t120 uv220 uv221"
dtype2="ps120 uv221"
dtype3="uv223 uv229"
dtype4="uv224"
dtype5="ps181 ps187 q181 q187  t181 t187 uv281 uv287 "
dtype6="ps180 q180 t180 uv280"
dtype7="ps188 q188 t188 uv288"

###

### put diagnostic file into prepbufr files
/bin/sh $scripts/rungsiqc_prep_reg.sh $scripts $exec $datadir ${exefile1} $savedir $sdate $edate "$tm"

### read post prepbufr file and regrop the data into different types
/bin/sh $scripts/rungsiqc_type_reg.sh $scripts $exec $savedir ${exefile2} $savedir $sdate $edate $fixfile "$sub" "$dtype1" "$dtype2" "$dtype3" "$dtype4" "$dtype5" "$dtype6" "$dtype7" ${exefile3} $datadir "$tm" 


exit

