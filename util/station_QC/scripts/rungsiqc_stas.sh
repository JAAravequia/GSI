#!/bin/sh
set -xa
################################################################################
####  UNIX Script Documentation Block
# Script name: rungsiqc_stas.sh
# Script description:this script is get statistics of O-A, O-B and observation for each station  
# Author: Xiujuan Su    Org: NP20         Date: 2010-09-12

# Script history log:
# 2018-09-19: X. Su  clean up comment lines and document the script
#
# usage: rungsiqc_stas.sh scripts exec exefile2 datadir savedir tmpdir sdate edate pdate fixfile 
#                    sfctype sondtype uvsfctype uvsondtype 
#
# scripts:  the scripts directory
# exec   : the excutable file directory
# exefile: read data from each cycle
# exefile2: calculate statistics of observations , O-A and O-B for eachstation
# datadir : the data directory where post prepbufr and files for station and type information
# savedir : the directory where final bias and rejection station lists locate
# tmpdir : working directory
# sdate: starting time
# edate: end of time 
# fixfile: the convinfo file where locates
# sfctype: surface data types except wind observation
# sondtype: sounding type except wind observations
# uvsfctype: wind surface wind types
# uvsondtype: wind sounding types  

 export scripts=$1
 export exec=$2
 export exefile=$3
 export exefile2=$4
 export datadir=$5
 export savedir=$6
 export tmpdir=$7
 export sdate=$8
 export edate=$9
 export pdate=${10}
 export fixfile=${11}
 export sfctype=${12}
 export sondtype=${13}
 export uvsfctype=${14}
 export uvsondtype=${15}



 export NDATE=/nwprod/util/exec/ndate
 export  ACCOUNT=GDAS-T2O
export SUB=/gpfs/td1/emc/global/save/emc.glopara/svn/gfs/trunk/para/bin/sub_wcoss
export LOGDIR=/ptmpp1/Xiujuan.Su/plot_gsiqc3_glb



 mkdir -p $savedir

 rdate=$sdate

tmpdata=/ptmpp1/$USER/gsiqc3/data_global
mkdir -p $tmpdata

cd $tmpdata

while [ $rdate -le $edate ]; do
cp $datadir/alltype.${rdate} ./.

tar -xvf alltype.$rdate
rm -f alltype.$rdate

rdate=`$NDATE 06 $rdate`

done
 


#day=`echo $sdate | cut -c7-8`
#hr=`echo $sdate | cut -c9-10`
#shr=06hr
#case $mon in
#   01) month=jan;;
#   02) month=feb;;
#   03) month=mar;;
#   04) month=apr;;
#   05) month=may;;
#   06) month=jun;;
#   07) month=jul;;
#   08) month=aug;;
#   09) month=sep;;
#   10) month=oct;;
#   11) month=nov;;
#   12) month=dec;;
#    *) echo "month error $mo"
#       exit 1;;
#esac

 mkdir -p  $tmpdir
 cd $tmpdir
# rm -f *


 cp $exec/$exefile ./$exefile
 cp $exec/$exefile2 ./$exefile2
 cp $fixfile  ./convinfo


## to read data into one file for each type


for datatype in ssfctype ssondtype suvsfctype suvsondtype  
do
if [ "${datatype}" = "ssfctype" ]; then
dstype=$sfctype
itype=0
elif [ "${datatype}" = "ssondtype" ]; then
dstype=$sondtype
itype=1
elif [ "${datatype}" = "suvsfctype" ]; then
dstype=$uvsfctype
itype=0
elif [ "${datatype}" = "suvsondtype" ]; then
dstype=$uvsondtype
itype=1
fi

for stype in $dstype

do

rdate=$sdate
ntime=1
rm -f fname
while [ $rdate -le $edate ]; do
fname1=${tmpdata}/${stype}.$rdate
if [ -s $fname1 ]; then
yr=`echo $rdate | cut -c1-4`
mon=`echo $rdate | cut -c5-6`
day=`echo $rdate | cut -c7-8`
hr=`echo $rdate | cut -c9-10`
shr=06hr
case $mon in
   01) month=jan;;
   02) month=feb;;
   03) month=mar;;
   04) month=apr;;
   05) month=may;;
   06) month=jun;;
   07) month=jul;;
   08) month=aug;;
   09) month=sep;;
   10) month=oct;;
   11) month=nov;;
   12) month=dec;;
    *) echo "month error $mo"
       exit 1;;
esac
break

fi
rdate=`$NDATE 06 $rdate`

done

rdate=$sdate
while [ $rdate -le $edate ]; do

 fname1=${tmpdata}/${stype}.$rdate

 if [ -s $fname1 ]; then
ntime=` expr $ntime + 1`
echo $fname1 >>fname
fi
rdate=`$NDATE 06 $rdate`
done
cat <<EOF >input
      &input
      dtype='$stype',itype=$itype
/
EOF

ntime=` expr $ntime - 1`

./$exefile <input>stdout 2>&1

 mv stdout ${stype}_stdout_${pdate}
 rm -f input

cat <<EOF >input
      &input
      dtype='$stype',itype=$itype,yr='$yr',mon='$month',day='$day',hr='$hr',shr='$shr'
/
EOF



if [ -s ${stype}_out ]; then
./$exefile2 <input>stdout2 2>&1
else
echo "cannot find file ${stype}_out"
fi

mv stdout2 ${stype}_stdout2

echo $ntime >>${stype}_stdout2

done

done

cat ps*_hist_90position >>ps_hist_90position
cat q*_hist_90position >>q_hist_90position
cat t*_hist_90position >>t_hist_90position
cat uv*_hist_90position >>uv_hist_90position

mv ps_hist_90position q_hist_90position t_hist_90position uv_hist_90position $savedir

exit
