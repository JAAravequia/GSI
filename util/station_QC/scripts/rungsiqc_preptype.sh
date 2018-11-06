#!/bin/sh
set -xa
################################################################################
####  UNIX Script Documentation Block
# Script name: rungsiqc_preptype.sh
# Script description:this script is a driver to run rungisqc_prep.sh and                     
# rungsi_type.sh.
# Author: Xiujuan Su    Org: NP20         Date: 2010-09-12

# Script history log:
# 2018-09-14: X. Su  clean up comment lines and document the script
#
# usage: rungsiqc_preptype.sh rungsiqc_prep.sh rungsiqc_type.sh
#
# rungsiqc_prep.sh: this script put gsi diagnotic file to prepbufr
# rungsiqc_type.sh: the script put exch cycle information into types and stations 
#
# scripts: the scripts directory
# datadir: The prepbufr file and GSI diagnostic file directory
# exec   : the excutable file directory 
# exefile: the excutable file to data into each station of each type
# savedir:the output prepbufr file with gsi daignostic file information,          
#         and files with each type and station information directory
# fixfile: the directory for convinfo file
# HOMEcfs: The directory of program to put GSi diagnostic file to prepbufr file
# EXECcfs: The executable file to put GSi diagnostic file to prepbufr file
# USHcfs : The script directory used by the program to put GSi diagnostic file
#          to prepbufr file
# NDATE  : The utility to change date




### define the script, data directory and excutable 
### file for each run

 export scripts=/u/Xiujuan.Su/home/gsiqc3/scripts
 export gscripts=/u/Xiujuan.Su/home/gsiqc3/grads
 export exec=/u/Xiujuan.Su/home/gsiqc3/exec
 export datadir=/gpfs/hps/nco/ops/com/gfs/prod
 export exefile=prepbufr.x
 export savedir=/u/Xiujuan.Su/nbns/postevent/data_global
 export fixfile=/nwprod/fix/global_convinfo.txt
 export HOMEcfs=/u/Xiujuan.Su/home/parafits
 export EXECcfs=/u/Xiujuan.Su/home/parafits/exec
 export USHcfs=/u/Xiujuan.Su/home/parafits/ush
 export NDATE=/nwprod/util/exec/ndate
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



#  set time
tmpdir=/ptmpp1/$USER/gsiqc3/ptime_glb

mkdir -p $tmpdir

cd $tmpdir


PDATE=$(/nwprod/util/exec/ndate -24 $PDATE)
tdate=`echo $PDATE|cut -c1-8`

sdate=${tdate}00
edate=${tdate}18



## define plot destination

mkdir -p $savedir

### put diagnostic file into prepbufr files
/bin/sh $scripts/rungsiqc_prep.sh $scripts $datadir $savedir $sdate $edate $HOMEcfs $EXECcfs $USHcfs $NDATE

### read post prepbufr file and regrop the data into different types
/bin/sh $scripts/rungsiqc_type.sh $scripts $exec $savedir $exefile $savedir $sdate $edate $fixfile "$sub" "$dtype1" "$dtype2" "$dtype3" "$dtype4" "$dtype5" "$dtype6" 

exit
