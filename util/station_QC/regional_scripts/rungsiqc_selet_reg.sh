#!/bin/sh
set -xa
################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:       rungsiqc_selet_reg.sh
# Script description:  Get bias and rejection station lists base on O-A and O-B
#                      statistics
#
# Author:        Xiujuan Su       Org: NP20         Date: 2010-09-15
#
# Script history log:
# 2018-09-20  X. Su  Removed unnecessary lines and documented the script
#
# usage: rungsiqc_selet_reg.sh scripts exec exefile datadir savedir sdate edate pdate
#                          sfctype sondtype uvsfctype uvsondtype
#
# scripts:  the scripts directory
# exec   : the excutable file directory
# exefile: read data from each cycle
# datadir :the directory where O-A and O-B statistic files locate, it is rungsiqc_stas.sh
#           working directory
# savedir: the bias and rejection station list directory
# sdate: starting time
# edate: end of time
# pdate: the period of data processed
# sfctype: all surface data types except wind observations
# sondtype: all sounding observation type except wind observations
# uvsfctype: wind surface data types
# uvsondtype: wind sounding observation types
# alltm: regional analysis delay time

 export scripts=$1
 export exec=$2
 export exefile=$3
 export datadir1=$4
 export savedir=$5
 export sdate=$6
 export edate=$7
 export pdate=$8
 export selectdir=$9 
 export sfctype=${10}
 export sondtype=${11}
 export uvsfctype=${12}
 export uvsondtype=${13}
 export alltm=${14}

# export scripts=/u/Xiujuan.Su/home/gsiqc3/scripts
# export exec=/u/Xiujuan.Su/home/gsiqc3/exec
# export exefile=selet_reg.x
# export datadir=/ptmpp1/Xiujuan.Su/bufrstas
# export savedir=/u/Xiujuan.Su/home/gsiqc3/data/$pdate
 export NDATE=/nwprod/util/exec/ndate
 export  ACCOUNT=GDAS-T2O
#export SUB=/u/wx20mi/bin/sub
export LOGDIR=/ptmpp1/$USER/gsiqc3/plot_gsiqc3


# sdate=2011010100
# edate=2011010218

# pdate=201101-03

 mkdir -p $savedir

 rdate=$sdate

# for ttm in 03 
# for ttm in 00 03 06 09 12
 for ttm in ${alltm} 
do


 datadir=${datadir1}/$ttm
 tmpdir=$selectdir/$ttm

 mkdir -p  $tmpdir
 cd $tmpdir
 rm -f *

 cp $exec/$exefile ./$exefile


## to read data into one file for each type

#dtype="ps120 ps180 ps181 ps187 ps188 q120 q180 q181 q187 q188 t120 t180 t181 t187 t188  uv220 uv221 uv223 uv224 uv229 uv280 uv281 uv287 uv288" 
#dtype="ps188" 

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
done
exit
