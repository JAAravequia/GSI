#!/bin/sh
set -xa

 export scripts=$1
 export exec=$2
 export exefile=$3
 export datadir=$4
 export savedir=$5
 export sdate=$6
 export edate=$7
 export pdate=$8
 export sfctype=${9}
 export sondtype=${10}
 export uvsfctype=${11}
 export uvsondtype=${12}

# export scripts=/u/Xiujuan.Su/home/gsiqc3/scripts
# export exec=/u/Xiujuan.Su/home/gsiqc3/exec
# export exefile=selet.x
# export datadir=/ptmpp1/Xiujuan.Su/bufrstas
# export savedir=/u/Xiujuan.Su/home/gsiqc3/data/$pdate
 export NDATE=/nwprod/util/exec/ndate
 export  ACCOUNT=GDAS-T2O
export SUB=/gpfs/td1/emc/global/save/emc.glopara/svn/gfs/trunk/para/bin/sub_wcoss


# sdate=2011010100
# edate=2011010218

# pdate=201101-03

 mkdir -p $savedir

 rdate=$sdate

 tmpdir=/ptmpp1/$USER/gsiqc3/select


 mkdir -p  $tmpdir
 cd $tmpdir
 rm -f *

 cp $exec/$exefile ./$exefile


## to read data into one file for each type

#dtype="ps120 ps180 ps181 ps187 q120 q180 q181 q187 t120 t180 t181 t187 uv220 uv221 uv223 uv224 uv228 uv229 uv280 uv281 uv287 " 
#dtype="uv280" 

#for stype in $dtype
#do
#if [ "${stype}" = 'q120' -o "${stype}" = 't120' -o "${stype}" = 'uv220' -o \
#    "${stype}" = 'uv221' -o "${stype}" = 'uv223' -o "${stype}" = 'uv224' \
#     -o "${stype}" = 'uv228' -o "${stype}" = 'uv229' ]; then
# itype=1
#else
#itype=0
#fi
#
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

cat <<EOF >input
      &input
      dtype='$stype',itype=$itype,
/
EOF

 cp $datadir/${stype}_stas ./${stype}_stas
 cp $datadir/${stype}_station ./${stype}_station 

./$exefile <input>stdout 2>&1

 mv stdout ${stype}_stdout_${pdate}

ddtype=`echo ${stype} | cut -c1-1` 
 if [ "${ddtype}" = 'u' ] ; then 
if [ -s ${stype}_qlyrej_list ]; then
cat ${stype}_qlyrej_list >> ${stype}_rej_dir_list 
fi
cp ${stype}_rej_dir_list $savedir/
cp ${stype}_rej_sp_list $savedir/
else
if [ -s ${stype}_qlyrej_list ]; then
cat ${stype}_qlyrej_list >> ${stype}_rej_list 
fi
cp ${stype}_rej_list $savedir/ 
cp ${stype}_bias_list $savedir
cp ${stype}_discnt_list $savedir

fi
done

done

exit
