#!/bin/sh
set -xa

################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:      runreadwrite_list.sh 
# Script description:  rewrite regional station bias and rejection lists in simple format 
#
# Author:        Xiujuan Su       Org: NP20         Date: 2010-09-15
#
# Script history log:
# 2018-11-05  X. Su  Removed unnecessary lines and documented the script
#
# usage runmake_time_list_reg.sh dtime scripts datadir xec exefile sfctype sondtype
#                            uvsfctype uvsondtype
# 
# dtime: the period of data processed
# scripts: Script directory
# datadir: final list save directory
# exec: excutable directory
# exefile: executable file
# sfctype: all surface data types except wind observations
# sondtype: all sounding observation type except wind observations
# uvsfctype: wind surface data types
# uvsondtype: wind sounding observation types


export dtime=$1
export scripts=$2
export datadir=$3
export exec=$4
export execfile=$5
export sfctype=${6}
export sondtype=${7}
export uvsfctype=${8}
export uvsondtype=${9}


export tmpdir=/ptmpp1/$USER/gsiqc3/readwrite_list_reg

mkdir -p $tmpdir

cd $tmpdir

#rm -f *

cp $exec/$execfile ./$execfile
   
#stype="ps120 ps180 ps181 ps187 ps188 q120 q180 q181 q187 q188 t120 t180 t181 t187 t188"
#sstype="uv220 uv221 uv223 uv224 uv228 uv229 uv280 uv281 uv287 uv288 "

for datatype in ssfctype ssondtype
do
if [ "${datatype}" = "ssfctype" ]; then
stype=$sfctype
itype=0
elif [ "${datatype}" = "ssondtype" ]; then
stype=$sondtype
itype=1
fi



   for dtype in $stype 
      do


   for filetype in bias rej
    do
    cp $datadir/${dtype}_${filetype}_list ./${dtype}_${filetype}_list
    rm -f input

cat << EOF > input
 &input
  dtype='${dtype}',filetype='${filetype}',itype=${itype}
/
EOF

  ./$execfile <input >stdout 2>&1 
  mv stdout ${dtype}_${filetype}_stdout
  mv ${dtype}_${filetype}_list_final $datadir/${dtype}_${filetype}_list_final

done
done
done

for datatype in suvsfctype suvsondtype
do
if [ "${datatype}" = "suvsfctype" ]; then
sstype=$uvsfctype
itype=0
elif [ "${datatype}" = "suvsondtype" ]; then
sstype=$uvsondtype
itype=1
fi

for dtype in $sstype
do


    for ftype in dir sp  
     do
     cp $datadir/${dtype}_rej_${ftype}_list ./${dtype}_rej_${ftype}_list
    rm -f input

cat << EOF > input
 &input
  dtype='${dtype}',filetype='rej',itype=${itype}
/
EOF


  ./$execfile <input >stdout 2>&1

  mv stdout ${dtype}_rej_${ftype}_stdout
  mv ${dtype}_rej_${ftype}_list_final $datadir/${dtype}_rej_${ftype}_list_final
done
done
done  
##  for the quality control rejection list

#ssstype="ps120 ps180 ps181 ps187 ps188  q120 q180 q181 q187 q188 t120 t180 t181 t187 t188 uv220 uv221 uv223 uv224 uv229 uv280 uv281 uv287 uv288"
#ssstype="t120 "

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

cp $datadir/${dtype}_qlyrej_list ./${dtype}_qlyrej_list

filetype=qlyrej

cat << EOF > input
 &input
  dtype='${dtype}',filetype='${filetype}',itype=${itype}
/
EOF

./$execfile <input >stdout 2>&1

mv stdout ${dtype}_${filetype}_stdout
mv ${dtype}_${filetype}_list_final $datadir/${dtype}_${filetype}_list_final

if [ -s ${dtype}_${filetype}_list_final ]; then
cp $datadir/${dtype}_rej_list_final ./${dtype}_rej_list_final

 if [ -s ${dtype}_${filetype}_list_final ]; then
cat ${dtype}_${filetype}_list_final >> ${dtype}_rej_list_final 
mv ${dtype}_rej_list_final $datadir/${dtype}_rej_list_final
fi
fi

done
done

exit
