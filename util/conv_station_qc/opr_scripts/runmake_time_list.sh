#!/bin/sh
set -xa

export scripts=$1
export exec=$2
export exefile=$3
export datadir=$4
export datadir2=$5
export tmpdir=$6
export sfctype=${7}
export sondtype=${8}
export uvsfctype=${9}
export uvsondtype=${10}


# export scripts=/u/Xiujuan.Su/home/gsiqc3/regional_scripts 
# export exec=/u/Xiujuan.Su/home/gsiqc3/exec
# export exefile=make_list_time.x 
## export datadir=/u/Xiujuan.Su/nbns/bufrstas_regional
# export datadir=/u/Xiujuan.Su/nbns/bufrstas_regional_20110405
# export datadir2=/u/Xiujuan.Su/nbns/select_reg_20110405
# export savedir=/u/Xiujuan.Su/home/gsiqc3/regional/data/201104-05
 export NDATE=/nwprod/util/exec/ndate
 export  ACCOUNT=GDAS-T2O
export SUB=/gpfs/td1/emc/global/save/emc.glopara/svn/gfs/trunk/para/bin/sub_wcoss
export LOGDIR=/ptmpp1/$USER/gsiqc3/plot_gsiqc3


# sdate=2011010100
# edate=2011010218

# pdate=201101-03

# mkdir -p $savedir

 rdate=$sdate

# tmpdir=/ptmpp1/Xiujuan.Su/make_time


## to read data into one file for each type

#dtype1="ps120 ps180 ps181 ps187 q120 q180 q181 q187 t120 t180 t181 t187" 
#dtype2="uv220 uv221 uv223 uv224 uv228 uv229 uv280 uv281 uv287 " 

#dtype1="t120 "
#dtype2="uv220 "
 mkdir -p $tmpdir
 cd $tmpdir
 rm -f *

 cp $exec/$exefile ./$exefile

for datatype in ssfctype ssondtype 
do
if [ "${datatype}" = "ssfctype" ]; then
dstype=$sfctype
itype=0
elif [ "${datatype}" = "ssondtype" ]; then
dstype=$sondtype
itype=1
fi


for stype in $dstype
do
if [ -s $datadir2/${stype}_bias_list ]; then
  cp $datadir2/${stype}_bias_list_final ./${stype}_bias_list 
fi
if [ -s $datadir2/${stype}_rej_list ]; then
cp $datadir2/${stype}_rej_list_final ./${stype}_rej_list
fi


cp $datadir/${stype}_station ./${stype}_station


cat <<EOF >input
      &input
      dtype='$stype',itype=$itype
/
EOF



./$exefile <input>stdout 2>&1

 mv stdout ${stype}_stdout
 
rm -f ${stype}_other_list
 rm -f ${stype}_bias_list
 rm -f ${stype}_rej_list
 rm -f ${stype}_station


done    ## type
done

for datatype in suvsfctype suvsondtype
do
if [ "${datatype}" = "suvsfctype" ]; then
dstype=$uvsfctype
itype=0
elif [ "${datatype}" = "suvsondtype" ]; then
dstype=$uvsondtype
itype=1
fi


for stype in $dstype
do
if [ -s $datadir2/${stype}_bias_list ]; then
  cp $datadir2/${stype}_bias_list_final ./${stype}_bias_list
fi
if [ -s $datadir2/${stype}_rej_dir_list_final ]; then
cp $datadir2/${stype}_rej_dir_list_final ./${stype}_rej_list
fi
cp $datadir/${stype}_station ./${stype}_station


cat <<EOF >input
      &input
      dtype='$stype',itype=$itype
/
EOF


 cp $exec/$exefile ./$exefile

./$exefile <input>stdout 2>&1

 mv stdout ${stype}_stdout

 rm -f ${stype}_other_list
 rm -f ${stype}_bias_list
 rm -f ${stype}_rej_list
 rm -f ${stype}_qlyrej_list
 rm -f ${stype}_station


done    ## type
done




exit
