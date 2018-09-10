#!/bin/sh
set -uxa


 export scripts=$1
 export exec=$2
 export datadir=$3
 export exefile=$4
 export savedir=$5
 export sdate=$6
 export edate=$7
export fixfile=$8
#export SUB=$9
# export scripts=/u/Xiujuan.Su/home/regional_gsiqc3/scripts
# export exec=/u/Xiujuan.Su/home/gsiqc3/sorc
# export datadir=/u/Xiujuan.Su/nbns/postevent/post
# export exefile=prepbufr.x
# export savedir=/u/Xiujuan.Su/nbns/postevent/data_tmp 

 export NDATE=/nwprod/util/exec/ndate
 export  ACCOUNT=GDAS-T2O
#export SUB=/u/wx20mi/bin/sub
export USER=Xiujuan.Su


# sdate=2011010100
# edate=2011010100

 pdate=201101-03

mkdir -p $savedir

 tmpdir=/ptmp/Xiujuan.Su/gsi_type_reg


 mkdir -p  $tmpdir
 cd $tmpdir
 rm -f *
rdate=$sdate

sub="ADPUPA PROFLR VADWND ADPSFC SFCSHP MSONET"
dtype="ps120 ps180 ps181 ps187 q120 q180 t120 t180 w220 w221 w223 w224 w280" 
dtype1="ps120 q120 t120 uv220 uv221"
dtype2="ps120 uv221" 
dtype3="uv223 uv229"
dtype4="uv224"
dtype5="ps181 ps187 q181 q187 t181 t187 uv281 uv287"
dtype6="ps180 q180 t180 uv280"
dtype7="ps188 q188 t188 uv288"

 cp $exec/$exefile ./$exefile
 cp $fixfile  ./convinfo

while [ $rdate -le $edate ]; do
for ttm in 00 03 06 09 12 
#for ttm in 06 
do
 rm -f prepbufr.post
cp $datadir/prepbufr.post.${rdate}.tm${ttm} ./prepbufr.post

#for subtype in ADPUPA PROFLR VADWND ADPSFC SFCSHP MSONET
for subtype in $sub

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
elif [ "${subtype}" = "MSONET" ]; then
stype=$dtype7
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

if [ "${dtype}" = "ps120" -o "${dtype}" = "ps180" -o "${dtype}" = "ps181" -o \
    "${dtype}" = "ps187" -o  "${dtype}" = "uv220" -o "${dtype}" = "uv221" \
    -o "${dtype}" = "uv223" -o "${dtype}" = "uv224" -o "${dtype}" = "uv229" -o \
     "${dtype}" = "uv280"  -o "${dtype}" = "ps188" -o "${dtype}" = "uv288" -o \
      "${dtype}" = "uv281"  -o "${dtype}" = "uv287"  ]; then
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
mv ps120 $savedir/ps120.$rdate.tm${ttm}
mv q120 $savedir/q120.$rdate.tm${ttm}
mv t120 $savedir/t120.$rdate.tm${ttm}
mv uv220 $savedir/uv220.$rdate.tm${ttm}
else
mv $dtype $savedir2/$dtype.$rdate.tm${ttm}
fi
mv stdout2 stdout2_${dtype}.$rdate.tm${ttm}
done  ## done with type

done ##  for sub
done     ##  for ttm

rdate=`$NDATE 06 $rdate`


done     


exit
