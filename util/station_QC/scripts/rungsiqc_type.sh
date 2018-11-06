#!/bin/sh
set -xa
################################################################################
####  UNIX Script Documentation Block
# Script name: rungsiqc_type.sh
# Script description:this script puts information to each station of the type, the script run daily 
# Author: Xiujuan Su    Org: NP20         Date: 2010-09-12

# Script history log:
# 2018-09-19: X. Su  clean up comment lines and document the script
#
# usage: rungsiqc_type.sh scripts exec datadir exefile savedir sdate edate fixfile sub dtype1 
#                         dtype2 dtype3 dtype4 dtype5 dtype6
#
# scripts:  the scripts directory
# exec   : the excutable file directory
# datadir: the post prepbufr file locates
# savedir: the directory of output which contain data type and stations
# sdate: starting time 
# edate: end of time
# fixfile: the convinfo file where locates
#    sub: The characters of all data type in prepbufr file such as ADPUPA as rawinsonde type
#  dtype1: all rawinsonde data types
#  dtype2: ps120 including rawinsonde types (represented by ps120) and pibal wind (uv221)
#  dtype3: NOAA profile wind type
#  dtype4: VAD wind type(224) and profile winds (Europe) (uv229)
#  dtype5: synoptic surface observation types
#  dtype6: marine surface observation types
 



 export scripts=$1
 export exec=$2
 export datadir=$3
 export exefile=$4
 export savedir=$5
 export sdate=$6
 export edate=$7
 export fixfile=$8
 export sub=$9
 export dtype1=${10}
 export dtype2=${11}
 export dtype3=${12}
 export dtype4=${13}
 export dtype5=${14}
 export dtype6=${15}


 export NDATE=/nwprod/util/exec/ndate
 export  ACCOUNT=GDAS-T2O
export SUB=/gpfs/td1/emc/global/save/emc.glopara/svn/gfs/trunk/para/bin/sub_wcoss
export USER=Xiujuan.Su


# sdate=2011010100
# edate=2011010100


mkdir -p $savedir

tmpdir=/ptmpp1/$USER/gsiqc3/gsi_type
 tmpsave=/ptmpp1/$USER/gsiqc3/gsi_typeall


 mkdir -p  $tmpdir
 mkdir -p $tmpsave
 rm -f $tmpsave/*
 cd $tmpdir
 rm -f *
rdate=$sdate

 cp $exec/$exefile ./$exefile
 cp $fixfile ./convinfo
# cp /u/Xiujuan.Su/home/gsiqc3/fix/global_convinfo.txt.gpsro.new.no401  ./convinfo

while [ $rdate -le $edate ]; do

 rm -f prepbufr.post
cp $datadir/prepbufr.post.$rdate ./prepbufr.post


for subtype in ADPUPA PROFLR VADWND ADPSFC SFCSHP
#for subtype in SFCSHP 

do
if [ "${subtype}" = "ADPUPA" ]; then
stype=$dtype2
elif [ "${subtype}" = "PROFLR" ]; then
stype=$dtype3
elif [ "${subtype}" = "VADWND" ]; then
stype=$dtype4
elif [ "${subtype}" = "ADPSFC" ]; then
stype=$dtype5
elif [ "${subtype}" = "SFCSHP" ]; then
stype=$dtype6
fi
#
for dtype in ${stype}
do
if [ "${dtype}" = "q120" -o "${dtype}" = "t120" -o "${dtype}" = "uv220" -o \
    "${dtype}" = "uv221" -o "${dtype}" = "uv223" -o "${dtype}" = "uv224" ]; then
 itype=1
else
itype=0
fi   

cctype=` echo $dtype | cut -c1-1`
if [ "${cctype}" = "p" -o "${cctype}" = "u" ]; then
 rtype=` echo $dtype | cut -c3-5`
else
rtype=` echo $dtype | cut -c2-4`
fi


cat <<EOF >input
      &input
      sub='$subtype',dtype='$dtype',itype=$itype,rtype=$rtype,
/
EOF


./$exefile<input >stdout2 2>&1

if [ "${subtype}" = "ADPUPA" -a "${dtype}" = "ps120" ]; then 
mv ps120 $tmpsave/ps120.$rdate
mv q120 $tmpsave/q120.$rdate
mv t120 $tmpsave/t120.$rdate
mv uv220 $tmpsave/uv220.$rdate
else
#mv $dtype $savedir/$dtype.$rdate
mv $dtype $tmpsave/$dtype.$rdate
fi
mv stdout2 stdout2_${dtype}.$rdate

done
done

cd $tmpsave
tar -cvf alltype.$rdate *$rdate*
mv alltype.$rdate $savedir
rm -f *
cd $tmpdir

rdate=`$NDATE 06 $rdate`

done     



exit
