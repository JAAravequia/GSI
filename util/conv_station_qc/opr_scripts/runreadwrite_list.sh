#!/bin/sh
set -xa

### define the script, data directory and excutable
### file for each run
# dtime=20120103
# export scripts=/u/Xiujuan.Su/home/gsiqc3/regional_scripts
# export datadir=/u/Xiujuan.Su/home/gsiqc3/regional_data/${dtime}
# export exec=/u/Xiujuan.Su/home/gsiqc3/exec
# export execfile=readwrite_list.x 

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
   
#stype="ps120 ps180 ps181 ps187 q120 q180 q181 q187 t120 t180 t181 t187"
#sstype="uv220 uv221 uv223 uv224 uv228 uv229 uv280 uv281 uv287"


#   for dtype in $stype 
#      do
#if [ "${dtype}" = 'q120' -o "${dtype}" = 't120' ]; then
# itype=1
#else
#itype=0
#fi

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
