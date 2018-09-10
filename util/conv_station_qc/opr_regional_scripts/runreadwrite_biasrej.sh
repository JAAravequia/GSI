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

export tmpdir=/ptmpp1/$USER/gsiqc3/readwrite_biasrej_reg

mkdir -p $tmpdir

cd $tmpdir

rm -f *

cp $exec/$execfile ./$execfile
   
#stype="ps120 ps180 ps181 ps187 ps188 q180 q181 q187 q188 t180 t181 t187 t188"

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

#    mv  $datadir/${dtype}_rej_list_final_tmp $datadir/${dtype}_rej_list_final
    cp $datadir/${dtype}_bias_list_final ./${dtype}_bias_list_final
    cp $datadir/${dtype}_rej_list_final ./${dtype}_rej_list_final
    rm -f input

cat << EOF > input
 &input
  dtype='${dtype}',itype=${itype}
/
EOF

  ./$execfile <input >stdout 2>&1 
  mv stdout ${dtype}_${filetype}_stdout
  mv $datadir/${dtype}_rej_list_final $datadir/${dtype}_rej_list_final_tmp
  mv $datadir/${dtype}_bias_list_final $datadir/${dtype}_bias_list_final_tmp
  mv ${dtype}_rej_list_final_final $datadir/${dtype}_rej_list_final
  mv ${dtype}_bias_list_final_final $datadir/${dtype}_bias_list_final

done
done
  

exit
