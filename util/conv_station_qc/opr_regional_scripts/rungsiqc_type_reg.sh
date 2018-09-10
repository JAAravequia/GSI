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
export sub=$9
 export dtype1=${10}
 export dtype2=${11}
 export dtype3=${12}
 export dtype4=${13}
 export dtype5=${14}
 export dtype6=${15}
 export dtype7=${16}
 export exefile2=${17} 
 export datadir2=${18}
 export alltm=${19}

export nreal_t=19
export nreal_pw=19
export nreal_ps=19
export nreal_q=20
export nreal_uv=23

#export SUB=$9
# export scripts=/u/Xiujuan.Su/home/regional_gsiqc3/scripts
# export exec=/u/Xiujuan.Su/home/gsiqc3/sorc
# export datadir=/u/Xiujuan.Su/nbns/postevent/post
# export exefile=prepbufr.x
# export savedir=/u/Xiujuan.Su/nbns/postevent/data_tmp 

 export NDATE=/nwprod/util/exec/ndate


# sdate=2011010100
# edate=2011010100


mkdir -p $savedir

 tmpdir=/ptmpp1/$USER/gsiqc3/gsi_type_reg
 tmpsave=/ptmpp1/$USER/gsiqc3/gsi_type_regall


mkdir -p $tmpsave
rm -f $tmpsave/*
 mkdir -p  $tmpdir
 cd $tmpdir
 rm -f *
rdate=$sdate

#sub="ADPUPA PROFLR VADWND ADPSFC SFCSHP MSONET"
#dtype="ps120 ps180 ps181 ps187 q120 q180 t120 t180 w220 w221 w223 w224 w280" 
#dtype1="ps120 q120 t120 uv220 uv221"
#dtype2="ps120 uv221" 
#dtype3="uv223 uv229"
#dtype4="uv224"
#dtype5="ps181 ps187 q181 q187 t181 t187 uv281 uv287"
#dtype6="ps180 q180 t180 uv280"
#dtype7="ps188 q188 t188 uv288"

 cp $exec/$exefile ./$exefile
 cp $exec/$exefile2 ./$exefile2
 cp $fixfile  ./convinfo
 

while [ $rdate -le $edate ]; do
for ttm in $alltm  
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
    "${dtype}" = "uv221" -o "${dtype}" = "uv223" -o "${dtype}" = "uv224" -o \
    "${dtype}" = "uv228" -o "${dtype}" = "uv229" ]; then
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


rm -f input

cat <<EOF >input
      &input
      sub='$subtype',dtype='$dtype',itype=$itype,rtype=$rtype,
/
EOF


./$exefile<input >stdout2 2>&1

if [ "${subtype}" = "ADPUPA" -a "${dtype}" = "ps120" ]; then 
mv ps120 $tmpsave/ps120.$rdate.tm${ttm}
mv q120 $tmpsave/q120.$rdate.tm${ttm}
mv t120 $tmpsave/t120.$rdate.tm${ttm}
mv uv220 $tmpsave/uv220.$rdate.tm${ttm}
else
mv $dtype $tmpsave/$dtype.$rdate.tm${ttm}
fi
mv stdout2 stdout2_${dtype}.$rdate.tm${ttm}
done  ## done with type

done ##  for sub

##For new VAD winds

cdate=`echo $rdate | cut -c1-8`
hour=`echo $rdate | cut -c9-10`

cp ${datadir2}/nam.$cdate/nam.t${hour}z.cnvstat.tm${ttm} ./cnvstat.t${hour}z.tm${ttm}
adate=${cdate}$hour
 ddate=`/nwprod/util/exec/ndate -$ttm $adate`
echo $ddate
 rm -f diag_conv_anl.$ddate
 rm -f diag_conv_ges.$ddate
 rm -f *Z
 tar -xvf cnvstat.t${hour}z.tm${ttm}
gunzip -v *gz
 rm -f conv_anl
 rm -f conv_ges
 mv diag_conv_anl.$ddate conv_anl
 mv diag_conv_ges.$ddate conv_ges

 dtype=uv224_00 

mtype=`echo ${dtype} | cut -f1 -d_`
subtype=`echo ${dtype} | cut -f2 -d_`
ttype=`echo ${dtype} | cut -c1-1`

if [ "${ttype}" = 'p' ]; then
type=ps
elif [ "${ttype}" = 'u' ]; then
type=uv
else
type=$ttype
fi
if [ "${type}" = 'uv' -o "${type}" = 'ps' ];then
itype=`echo ${mtype} | cut -c3-5`
else
itype=`echo ${mtype} | cut -c2-4`
fi

eval nreal=\${nreal_${type}}

rm -f input

cat <<EOF >input
      &input
       file1='conv_ges',file2='conv_anl',intype='${type}',stype='${mtype}',
       itype=${itype},subtype='${subtype}',isubtype=${subtype},nreal=${nreal},sfctype=1,
/
EOF

./${exefile2} <input>stdout 2>&1

mv $mtype $tmpsave/${mtype}.$rdate.tm${ttm}
mv stdout ${dtype}.stdout.${rdate}.${ttm}




cd $tmpsave
tar -cvf alltype.$rdate.tm${ttm} *$rdate.tm${ttm}*
mv alltype.$rdate.tm${ttm} $savedir
#rm -f *
cd $tmpdir

done     ##  for ttm

rdate=`$NDATE 06 $rdate`


done     


exit
