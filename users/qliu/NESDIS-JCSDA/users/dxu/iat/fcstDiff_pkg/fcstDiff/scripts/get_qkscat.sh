#!/bin/sh
set -x
###############################################################################
## 1. Set date 
## 2. analysis cycle (6hr gdas vs 2.5hr gfs)
## 3. Save directory
###############################################################################
adate=${1:-2008082500}
mod=${2:-gdas}
savedir=${3:-/ptmp/wx23dc/qkswnd}
edate=2008093018
ndate=${ndate_dir}/ndate

if [ ! -d $savedir ]; then mkdir $savedir || exit 8 ; fi ;
tag=qkswnd
end=tm00.bufr_d

cd $savedir

while [ $adate -le $edate ]; do

YYYY=`echo $adate | cut -c1-4`
MM=`echo $adate | cut -c5-6`
DD=`echo $adate | cut -c7-8`
CYC=`echo $adate | cut -c9-10`

/u/wx20mi/bin/hpsstar get /hpssprod/runhistory/rh${YYYY}/${YYYY}${MM}/${YYYY}${MM}${DD}/com_gfs_prod_${mod}.${YYYY}${MM}${DD}${CYC}.tar ./${mod}1.t${CYC}z.${tag}.${end} 

mv ${mod}1.t${CYC}z.${tag}.${end}  ${tag}.${adate}.bufr
adate=`$ndate +06 $adate`
done
