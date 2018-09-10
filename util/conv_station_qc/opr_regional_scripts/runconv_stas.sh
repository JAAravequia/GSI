#!/bin/sh

set -ax
date

#scripts=/u/Xiujuan.Su/home/gsiqc3/opr_regional_scripts
#export datadir=/com/nam/prod
#export execdir=/u/Xiujuan.Su/home/gsiqc3/exec
#exefile=cnvstat.x
#export savedir=/u/Xiujuan.Su/nbns/postevent/post_regional_tmp
#export fixfile=/u/Xiujuan.Su/home/gsiqc3/fix/nam_regional_convinfo.txt
#sdate=2015060800
#edate=2015060800
export scripts=$1
export datadir=$2
export execdir=$3
export exefile=$4
export savedir=$5
export fixfile=$6
sdate=$7
edate=$8

export NDATE=/nwprod/util/exec/ndate
export nreal_t=19
export nreal_pw=19
export nreal_ps=19
export nreal_q=20
export nreal_uv=23

tmpdir=/ptmpp1/$LOGNAME/gsiqc3/cnvstat
tmpsave=/ptmpp1/$LOGNAME/gsiqc3/gsi_type_regall
mkdir -p $tmpdir 

stype='uv224_00'

cd $tmpdir 

cp ${execdir}/${exefile} ./
cp $fixfile ./convinfo

rdate=$sdate
while [ $rdate -le $edate ]; do

cdate=`echo $rdate | cut -c1-8`
for hour in 00 06 12 18
  do
 for ttm in 00 03 06 
  do

cp ${datadir}/nam.$cdate/nam.t${hour}z.cnvstat.tm${ttm} ./cnvstat.t${hour}z.tm${ttm}
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

for dtype in ${stype}
do

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

./${exefile} <input>stdout 2>&1

mv $mtype $tmpsave/${mtype}.$rdate.tm${ttm}
mv stdout ${dtype}.stdout.${rdate}.${ttm}


done   ### done with dtype

cd $tmpsave
tar -cvf alltype.$rdate.tm${ttm} *$rdate.tm${ttm}*
mv alltype.$rdate.tm${ttm} $savedir
#rm -f *
cd $tmpdir


done   ### done with ttm 

#rm -f stdout

done   ## done with hour

rdate=`$NDATE 06 $rdate`

done     ## done with date
