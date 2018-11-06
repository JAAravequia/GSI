#!/bin/sh
set -xa
################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:      runreadwrite_list.sh 
# Script description:  rewrite station bias and rejection lists in simple format 
#
# Author:        Xiujuan Su       Org: NP20         Date: 2010-09-15
#
# Script history log:
# 2018-09-18  X. Su  Removed unnecessary lines and documented the script
#
# usage runmake_time_list.sh dtime scripts datadir xec exefile sfctype sondtype
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


export tmpdir=/ptmpp1/$USER/gsiqc3/readwrite_list_glb

mkdir -p $tmpdir

cd $tmpdir

rm -f *

cp $exec/$execfile ./$execfile
   

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

#if [  "${dtype}" = 'uv220' -o \
#    "${dtype}" = 'uv221' -o "${dtype}" = 'uv223' -o "${dtype}" = 'uv224' \
#    -o "${dtype}" = 'uv228' -o "${dtype}" = 'uv229' ]; then
# itype=1
#else
#itype=0
#fi


    for ftype in dir sp  
     do
     cp $datadir/${dtype}_rej_${ftype}_list ./${dtype}_rej_${ftype}_list
    rm -f input
 done

cat << EOF > input
 &input
  dtype='${dtype}',filetype='rej',itype=${itype}
/
EOF


  ./$execfile <input >stdout 2>&1

  mv stdout ${dtype}_rej_stdout
  mv ${dtype}_rej_*final $datadir
done
done
  

exit
