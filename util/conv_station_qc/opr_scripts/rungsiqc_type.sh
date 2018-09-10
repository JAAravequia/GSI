#!/bin/sh
set -xa


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

# export scripts=/u/Xiujuan.Su/home/gsiqc3/scripts
# export exec=/u/Xiujuan.Su/home/gsiqc3/sorc
# export datadir=/u/Xiujuan.Su/nbns/postevent/post
# export exefile=prepbufr.x
# export savedir=/u/Xiujuan.Su/nbns/postevent/data_tmp 

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

#sub="ADPUPA PROFLR VADWND ADPSFC SFCSHP"
#dtype="ps120 ps180 ps181 ps187 q120 q180 t120 t180 w220 w221 w223 w224 w280" 
#dtype1="ps120 q120 t120 uv220 uv221"
#dtype2="ps120 uv221" 
#dtype3="uv223 uv229"
#dtype4="uv224"
#dtype5="ps181 ps187 q181 q187 t181 t187 uv281 uv287"
#dtype6="ps180 q180 t180 uv280"
#dtype6="uv280"
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
