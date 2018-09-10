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

export tmpdir=/ptmp/Xiujuan.Su/readwrite_list

mkdir -p $tmpdir

cd $tmpdir

rm -f *

cp $exec/$execfile ./$execfile
   
stype="ps188 q181 q187 q188 t181 t187 t188"
sstype="uv281 uv287 uv288 "


   for dtype in $stype 
      do

itype=0

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

for dtype in $sstype
do

itype=0

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
  
##  for the quality control rejection list

ssstype="ps120 ps180 ps181 ps187 ps188  q120 q180 q181 q187 q188 t120 t180 t181 t187 t188 uv220 uv221 uv223 uv224 uv229 uv280 uv281 uv287 uv288"
#ssstype="t120 "

for dtype in $ssstype
do
if [ "${dtype}" = 'q120' -o "${dtype}" = 't120' -o "${dtype}" = 'uv220' -o \
 "${dtype}" = 'uv221' -o "${dtype}" = 'uv223' -o "${dtype}" = 'uv224' \
    -o "${dtype}" = 'uv229' ]; then
 itype=1
else
itype=0
fi

cp $datadir/${dtype}_qlyrej_list ./${dtype}_qlyrej_list

filetype=qlyrej

cat << EOF > input
 &input
  dtype='${dtype}',filetype='${filetype}',itype=${itype}
/
EOF

./$execfile <input >stdout 2>&1

mv stdout ${dtype}_${filetype}_stdout
#mv ${dtype}_${filetype}_list_final $datadir/${dtype}_${filetype}_list_final

if [ -s ${dtype}_${filetype}_list_final ]; then
cp $datadir/${dtype}_rej_list_final ./${dtype}_rej_list_final

cat ${dtype}_${filetype}_list_final >> ${dtype}_rej_list_final 
#mv ${dtype}_rej_list_final $datadir/${dtype}_rej_list_final
fi

done


exit
