#!/bin/sh
set -xa

### define the script, data directory and excutable
### file for each run
# dtime=20120103
# export scripts=/u/Xiujuan.Su/home/gsiqc3/regional_scripts
# export datadir=/u/Xiujuan.Su/home/gsiqc3/regional_data/${dtime}
# export datadir2=/u/Xiujuan.Su/nbns/bufrstas 
# export exec=/u/Xiujuan.Su/home/gsiqc3/exec
# export execfile=compare_list_reg_glb.x 
# export execfile1=compare_list_reg_glb_sp.x

dtime=$1
scripts=$2
datadir=$3
datadir2=$4
exec=$5
execfile=$6
execfile1=$7

export tmpdir=/ptmpp1/$USER/gsiqc3/compare_list_reglb

mkdir -p $savedir
mkdir -p $tmpdir

cd $tmpdir

rm -f *

cp $exec/$execfile ./$execfile
cp $exec/$execfile1 ./$execfile1
   
#stype="ps120 ps180 ps181 ps187 ps188 q120 q180 q181 q187 q188 t120 t180 t181 t187 t188 uv220 uv221 uv223 uv224 uv229 uv280 uv281 uv287 uv288"
#stype="ps120 ps180 ps181 ps187 ps188 q181 q187 q188 t180 t181 t187 t188"
stype="ps120 ps180 ps181 ps187 q180 q181 q187 t180 q120 t120 t181 t187 "
# stype="t120 q120 "
#sstype="uv220 uv221 uv223 uv224 uv280 "
sstype="uv220 uv221 uv223 uv224" 


   for dtype in $stype 
      do
    cp ${datadir2}/${dtype}_station ./${dtype}_station 
    cp ${datadir2}/${dtype}_stas ./${dtype}_stas 

if [ "${dtype}" = 'q120' -o "${dtype}" = 't120' ]; then
 itype=1
else
itype=0
fi

   for filetype in bias rej
    do
    cp $datadir/${dtype}_${filetype}_list ./${dtype}_${filetype}_list
    rm -f input

cat << EOF > input
 &input
  dtype='${dtype}',filetype='${filetype}',itype=${itype}
/
EOF

  $execfile <input >stdout 2>&1 
  mv stdout ${dtype}_${filetype}_stdout
  mv ${dtype}_${filetype}_list_regglb $datadir
  mv ${dtype}_${filetype}_list_regglb_final $datadir/${dtype}_${filetype}_list_final

done
done

for dtype in $sstype
do
    cp ${datadir2}/${dtype}_station ./${dtype}_station
    cp ${datadir2}/${dtype}_stas ./${dtype}_stas

if [ "${dtype}" = 'uv220' -o  "${dtype}" = 'uv229' -o \
    "${dtype}" = 'uv221' -o "${dtype}" = 'uv223' -o "${dtype}" = 'uv224' ]; then 
 itype=1
else
itype=0
fi

    for ftype in dir sp  
     do
     cp $datadir/${dtype}_rej_${ftype}_list ./${dtype}_rej_${ftype}_list
    rm -f input

cat << EOF > input
 &input
  dtype='${dtype}',filetype='rej',itype=${itype}
/
EOF


 if [ "${ftype}" = 'sp' ]; then
   $execfile1<input >stdout 2>&1
 else
  $execfile <input >stdout 2>&1
fi
  mv stdout ${dtype}_rej_${ftype}_stdout
  mv ${dtype}_rej_${ftype}_list_regglb $datadir/${dtype}_rej_${ftype}_list_regglb
  mv ${dtype}_rej_${ftype}_list_regglb_final $datadir/${dtype}_rej_${ftype}_list_final
done
done
  

