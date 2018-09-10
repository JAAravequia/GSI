#!/bin/sh
set -xa

 export scripts=$1
 export exec=$2
 export exefile=$3
 export datadir1=$4
 export savedir=$5
 export sdate=$6
 export edate=$7
 export pdate=$8
 export selectdir=$9 
 export SUB=${10} 


# export scripts=/u/Xiujuan.Su/home/gsiqc3/scripts
# export exec=/u/Xiujuan.Su/home/gsiqc3/exec
# export exefile=selet_reg.x
# export datadir=/ptmp/Xiujuan.Su/bufrstas
# export savedir=/u/Xiujuan.Su/home/gsiqc3/data/$pdate
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

# for ttm in 03 
 for ttm in 00 03 06 09 12
do


 datadir=${datadir1}/$ttm
 tmpdir=$selectdir/$ttm

 mkdir -p  $tmpdir
 cd $tmpdir
 rm -f *

 cp $exec/$exefile ./$exefile


## to read data into one file for each type

dtype="ps120 ps180 ps181 ps187 ps188 q120 q180 q181 q187 q188 t120 t180 t181 t187 t188  uv220 uv221 uv223 uv224 uv229 uv280 uv281 uv287 uv288" 
#dtype="ps188" 

for stype in $dtype
do
if [ "${stype}" = 'q120' -o "${stype}" = 't120' -o "${stype}" = 'uv220' -o \
    "${stype}" = 'uv221' -o "${stype}" = 'uv223' -o "${stype}" = 'uv224' \
    -o "${stype}" = 'uv229' ]; then
 itype=1
else
itype=0
fi

cat <<EOF >input
      &input
      dtype='$stype',itype=$itype,
/
EOF

 cp $datadir/${stype}_stas ./${stype}_stas
 cp $datadir/${stype}_station ./${stype}_station 
 cp $exec/$exefile ./$exefile

./$exefile <input>stdout 2>&1

 mv stdout stdout_${stype}_${pdate}

#ddtype=`echo ${stype} | cut -c1-1` 
#mv ${stype}_rej_dir_list rejlist_${stype}_${pdate}
#cat ${stype}_qlyrej_list >> rejlist_${stype}_${pdate}
#mv rejlist_${stype}_${pdate} $savedir/
#mv ${stype}_other_list $savedir3/otherlist_${stype}_${pdate}
#mv ${stype}_rej_sp_list $savedir3/
#else
#mv ${stype}_rej_list rejlist_${stype}_${pdate}
#cat ${stype}_qlyrej_list >> rejlist_${stype}_${pdate}
#mv rejlist_${stype}_${pdate} $savedir3/ 
#mv ${stype}_other_list $savedir3/otherlist_${stype}_${pdate}
#mv ${stype}_bias_list $savedir3/biaslist_${stype}_${pdate}
#mv ${stype}_discnt_list $savedir3/discntlist_${stype}_${pdate}

#fi
done

done
exit
