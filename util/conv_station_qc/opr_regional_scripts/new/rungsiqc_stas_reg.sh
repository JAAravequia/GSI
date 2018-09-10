#!/bin/sh
set -xa

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
 export SUB=${12}


# export scripts=/u/Xiujuan.Su/home/gsiqc3/scripts
# export exec=/u/Xiujuan.Su/home/gsiqc3/exec
# export exefile=readdata.x
# export exefile2=stas.x
# export datadir=/u/Xiujuan.Su/nbns/postevent/data_tmp
# export savedir=/u/Xiujuan.Su/home/gsiqc3/data/201101-03
 export NDATE=/nwprod/util/exec/ndate
 export  ACCOUNT=GDAS-T2O
#export SUB=/u/wx20mi/bin/sub
export USER=Xiujuan.Su
export LOGDIR=/ptmp/Xiujuan.Su/plot_gsiqc3


# sdate=2011010100
# edate=2011010218

# pdate=201101-03

 mkdir -p $savedir

 rdate=$sdate

# tmpdir=/ptmp/Xiujuan.Su/bufrstas

yr=`echo $sdate | cut -c1-4`
mon=`echo $sdate | cut -c5-6`
day=`echo $sdate | cut -c7-8`
hr=`echo $sdate | cut -c9-10`
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

 mkdir -p  $tmpdir
 cd $tmpdir
# rm -f *


## to read data into one file for each type

dtype="ps120 ps180 ps181 ps187 ps188  q120 q180 q181 q187 q188 t120 t180 t181 t187 t188 uv220 uv221 uv223 uv224 uv229 uv280 uv281 uv287 uv288" 
#dtype="uv288" 
#for ttm in  00 06 09 12 
for ttm in 00 03 06 09 12  
do

 mkdir -p $tmpdir/$ttm
 cd $tmpdir/$ttm

 cp $exec/$exefile ./$exefile
 cp $exec/$exefile2 ./$exefile2
 cp $fixfile  ./convinfo

for stype in $dtype
do
if [ "${stype}" = 'q120' -o "${stype}" = 't120' -o "${stype}" = 'uv220' -o \
    "${stype}" = 'uv221' -o "${stype}" = 'uv223' -o "${stype}" = 'uv224' \
    -o "${stype}" = 'uv229' ]; then
 itype=1
else
itype=0
fi


rdate=$sdate
ntime=1
rm -f fname
while [ $rdate -le $edate ]; do

 fname1=${datadir}/${stype}.$rdate.tm${ttm}
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
 cp $exec/$exefile ./$exefile

./$exefile <input>stdout 2>&1

#cp  ${stype}_out $savedir2/${sdate}-${edate}/${stype}_out 
 mv stdout ${stype}_stdout

#cp $savedir2/${sdate}-${edate}/${stype}_out ./${stype}_out
if [ -s ${stype}_out ]; then
rm -f input
cat <<EOF >input
      &input
      dtype='$stype',itype=$itype,yr='$yr',mon='$month',day='$day',hr='$hr',shr='$shr'
/
EOF
./$exefile2 <input>stdout2 2>&1
else
echo "cannot find file ${stype}_out"
fi

mv stdout2 ${stype}_stdout2

echo $ntime >>${stype}_stdout2

done    ## type


done   ### ttm
exit
