#!/bin/sh
set -xa
################################################################################
####  UNIX Script Documentation Block
# Script name:runcompare_list_qlyrej_reg.sh
# Script description:this script compares rejection station lists because of 
                      the station observation rejected by quality control from 
#                    different regional analysis delay time and gets final bias
#                    end rejection lists 
# Author: Xiujuan Su    Org: NP20         Date: 2010-09-12
#
# Script history log:
# 2018-10-25: X. Su  clean up comment lines and document the script
#
# usage runcompare_list_qlyrej.sh dtime scripts datadir datadir2 exec execfile execfile1 savedir
#                          sfctype sondtype uvsfctype uvsondtype alltm
#
# dtim: the period of data processed
# scripts:  the scripts directory
# datadir: the directory holding original station bias and rejection lists(select_reg)
# datadir2: the directory has all station observation statistics (bufrstas_reg)
# exec:     the excutable file directory
# execfile: the program to compare the lists from different regional analysis
# execfile1: the program to compare the wind speed lists from different regional analysis
# savedir: the directory for final bias and rejection lists
# sfctype: all surface data types except wind observations
# sondtype: all sounding observation type except wind observations
# uvsfctype: wind surface data types
# uvsondtype: wind sounding observation types
# alltm: regional analysis delay times


### define the script, data directory and excutable
### file for each run
export dtime=$1
export scripts=$2
export datadir=$3
export datadir2=$4
export exec=$5
export execfile=$6
export savedir=$7
export sfctype=${8}
export sondtype=${9}
 export uvsfctype=${10}
 export uvsondtype=${11}
 export alltm=${12}

 export tmpdir=/ptmpp1/$USER/gsiqc3/compare_list_time_qlyrej_reg

 mkdir -p $savedir

mkdir -p $tmpdir

cd $tmpdir

rm -f *

cp $exec/$execfile ./$execfile
cp $exec/$execfile1 ./$execfile1
   

#   for dtype in $stype 
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

for dtype in $dstype

      do


#     if [ -s ${datadir}/00/${dtype}_qlyrej_list ]; then
#     export    fexist1=.true.
#        echo ${fexist1}
#        cp ${datadir}/00/${dtype}_qlyrej_list ./${dtype}_qlyrej_list_00
#     else
#      export  fexist1=.false.
#     fi
     if [ -s ${datadir}/00/${dtype}_qlyrej_list ]; then
        export fexist2=.true.
        cp ${datadir}/00/${dtype}_qlyrej_list ./${dtype}_qlyrej_list_00
     else
        export fexist2=.false.
     fi

     if [ -s ${datadir}/03/${dtype}_qlyrej_list ]; then
        export fexist3=.true.
        cp ${datadir}/03/${dtype}_qlyrej_list ./${dtype}_qlyrej_list_03
     else
       export fexist3=.false.
     fi
     if [ -s ${datadir}/06/${dtype}_qlyrej_list ]; then
        export fexist4=.true.
        cp ${datadir}/06/${dtype}_qlyrej_list ./${dtype}_qlyrej_list_06
     else
        export fexist4=.false.
     fi

    rm -f input

cat << EOF > input
 &input
  fileexist(1)=${fexist2},fileexist(2)=${fexist3},fileexist(3)=${fexist4},
  tm(1)='00',tm(2)='03',tm(3)='06',dtype='${dtype}',itype=${itype}
/
EOF

  ./$execfile <input >stdout 2>&1 
  mv stdout ${dtype}_stdout
  mv  ${dtype}_qlyrej_list $savedir
done
done
  

