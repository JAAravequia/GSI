#!/bin/sh
set -xa

export scripts=$1
export exec=$2
export exefile=$3
export datadir=$4
export datadir2=$5
export tmpdir=$6

# export scripts=/u/Xiujuan.Su/home/gsiqc3/regional_scripts 
# export exec=/u/Xiujuan.Su/home/gsiqc3/exec
# export exefile=make_list_time.x 
## export datadir=/u/Xiujuan.Su/nbns/bufrstas_regional
# export datadir=/u/Xiujuan.Su/nbns/bufrstas_regional_20110405
# export datadir2=/u/Xiujuan.Su/nbns/select_reg_20110405
# export savedir=/u/Xiujuan.Su/home/gsiqc3/regional/data/201104-05
 export NDATE=/nwprod/util/exec/ndate
 export  ACCOUNT=GDAS-T2O
export SUB=/u/wx20mi/bin/sub
export USER=Xiujuan.Su
export LOGDIR=/ptmp/Xiujuan.Su/plot_gsiqc3


# sdate=2011010100
# edate=2011010218

# pdate=201101-03

 mkdir -p $savedir

 rdate=$sdate

# tmpdir=/ptmp/Xiujuan.Su/make_time_reg


## to read data into one file for each type

dtype1="ps120 ps180 ps181 ps187 ps188 q120 q180 q181 q187 q188 t120 t180 t181 t187 t188" 
dtype2="uv220 uv221 uv223 uv224 uv229 uv280 uv281 uv287 uv288" 

#dtype1="q120 "
#dtype2="uv223 "
 mkdir -p $tmpdir
 cd $tmpdir
 rm -f *

 cp $exec/$exefile ./$exefile

for stype in $dtype1
do
if [ -s $datadir2/${stype}_bias_list ]; then
  cp $datadir2/${stype}_bias_list_final ./${stype}_bias_list 
fi
if [ -s $datadir2/${stype}_rej_list ]; then
cp $datadir2/${stype}_rej_list_final ./${stype}_rej_list
fi


for tm in 00 03 06 09 12
do
cp $datadir/${tm}/${stype}_station ./${stype}_station_${tm}

if [ "${stype}" = 'q120' -o "${stype}" = 't120' -o "${stype}" = 'uv220' -o \
    "${stype}" = 'uv221' -o "${stype}" = 'uv223' -o "${stype}" = 'uv224' \
    -o "${stype}" = 'uv229' ]; then
 itype=1
else
itype=0
fi


cat <<EOF >input
      &input
      dtype='$stype',tm='${tm}',itype=$itype
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

for stype in $dtype2
do
if [ -s $datadir2/${stype}_bias_list ]; then
  cp $datadir2/${stype}_bias_list_final ./${stype}_bias_list
fi
if [ -s $datadir2/${stype}_rej_dir_list_final ]; then
cp $datadir2/${stype}_rej_dir_list_final ./${stype}_rej_list
fi
for tm in 00 03 06 09 12
do
cp $datadir/${tm}/${stype}_station ./${stype}_station_${tm}

if [ "${stype}" = 'q120' -o "${stype}" = 't120' -o "${stype}" = 'uv220' -o \
    "${stype}" = 'uv221' -o "${stype}" = 'uv223' -o "${stype}" = 'uv224' \
    -o "${stype}" = 'uv229' ]; then
 itype=1
else
itype=0
fi


cat <<EOF >input
      &input
      dtype='$stype',tm='${tm}',itype=$itype
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





exit
