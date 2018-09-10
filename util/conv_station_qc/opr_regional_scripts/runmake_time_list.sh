#!/bin/sh
set -xa

export scripts=$1
export exec=$2
export exefile=$3
export datadir=$4
export datadir2=$5
export tmpdir=$6
export global_data=$7
export sfctype=${8}
export sondtype=${9}
export uvsfctype=${10}
export uvsondtype=${11}
export alltm=${12}

# export scripts=/u/Xiujuan.Su/home/gsiqc3/regional_scripts 
# export exec=/u/Xiujuan.Su/home/gsiqc3/exec
# export exefile=make_list_time.x 
## export datadir=/u/Xiujuan.Su/nbns/bufrstas_regional
# export datadir=/u/Xiujuan.Su/nbns/bufrstas_regional_20110405
# export datadir2=/u/Xiujuan.Su/nbns/select_reg_20110405
# export savedir=/u/Xiujuan.Su/home/gsiqc3/regional/data/201104-05
 export NDATE=/nwprod/util/exec/ndate
export SUB=/u/wx20mi/bin/sub
export LOGDIR=/ptmpp1/$USER/gsiqc3/plot_gsiqc3


# sdate=2011010100
# edate=2011010218

# pdate=201101-03

mkdir -p $savedir

 rdate=$sdate

# tmpdir=/ptmpp1/$USER/gsiqc3/make_time_reg


## to read data into one file for each type

#dtype1="ps120 ps180 ps181 ps187 ps188 q120 q180 q181 q187 q188 t120 t180 t181 t187 t188" 
#dtype2="uv220 uv221 uv223 uv224 uv229 uv280 uv281 uv287 uv288" 

#dtype1="q120 "
#dtype2="uv223 "
 mkdir -p $tmpdir
 cd $tmpdir
# rm -f *

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
if [ -s ${global_data}/${stype}_rej_list ]; then
cp ${global_data}/${stype}_rej_list_final ./${stype}_rej_list
 elif [ -s $datadir2/${stype}_rej_list ]; then
cp $datadir2/${stype}_rej_list_final ./${stype}_rej_list
fi


for tm in  ${alltm} 
do
cp $datadir/${tm}/${stype}_station ./${stype}_station_${tm}



cat <<EOF >input
      &input
      dtype='$stype',itype=$itype,ttm='$tm',
/
EOF

./$exefile <input>stdout 2>&1

 mv stdout ${stype}_stdout_${tm}

# rm -f ${stype}_other_list_${tm}
# rm -f ${stype}_bias_list_${tm}
# rm -f ${stype}_rej_list_${tm}
# rm -f ${stype}_qlyrej_list_${tm}
# rm -f ${stype}_station_${tm}

done

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
if [ -s ${global_data}/${stype}_rej_dir_list_final ]; then
cp ${global_data}/${stype}_rej_dir_list_final ./${stype}_rej_list
elif [ -s $datadir2/${stype}_rej_dir_list_final ]; then
cp $datadir2/${stype}_rej_dir_list_final ./${stype}_rej_list
fi
for tm in ${alltm} 
do
cp $datadir/${tm}/${stype}_station ./${stype}_station_${tm}



cat <<EOF >input
      &input
      dtype='$stype',itype=$itype,ttm='$tm', 
/
EOF



./$exefile <input>stdout 2>&1

 mv stdout ${stype}_stdout_${tm}

# rm -f ${stype}_other_list_${tm}
# rm -f ${stype}_bias_list_${tm}
# rm -f ${stype}_rej_list_${tm}
# rm -f ${stype}_qlyrej_list_${tm}
# rm -f ${stype}_station_${tm}

done

done    ## type

done




exit
