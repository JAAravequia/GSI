#!/bin/sh
set -xa

### define the script, data directory and excutable
### file for each run
 export scripts=/u/Xiujuan.Su/home/gsiqc3/regional_scripts
 export datadir=/u/Xiujuan.Su/nbns/select_reg
 export datadir2=/u/Xiujuan.Su/nbns/bufrstas_regional 
 export exec=/u/Xiujuan.Su/home/gsiqc3/exec
 export execfile=compare_list.x

export tmpdir=/ptmp/Xiujuan.Su/compare_list_time

mkdir -p $tmpdir

cd $tmpdir

rm -f *

cp $exec/$execfile ./$execfile
   
#stype="ps120 ps180 ps181 ps187 ps188 q120 q180 q181 q187 q188 t120 t180 t181 t187 t188 uv220 uv221 uv223 uv224 uv229 uv280 uv281 uv287 uv288"
#stype="ps120 ps180 ps181 ps187 ps188 q181 q187 q188 t180 t181 t187 t188"
stype="ps188 "
sstype="uv220 uv221 uv223 uv224 uv229 uv280 uv281 uv287 uv288"


   for dtype in $sstype 
      do
    for ftype in dir sp 
     do
     if [ -s ${datadir}/00/${dtype}_rej_${ftype}_list ]; then
     export    fexist1=.true.
        echo ${fexist1}
        cp ${datadir}/00/${dtype}_rej_${ftype}_list ./${dtype}_rej_${ftype}_list_00
     else
      export  fexist1=.false.
     fi
     if [ -s ${datadir}/03/${dtype}_rej_${ftype}_list ]; then
        export fexist2=.true.
        cp ${datadir}/03/${dtype}_rej_${ftype}_list ./${dtype}_rej_${ftype}_list_03
     else
       export fexist2=.false.
     fi
     if [ -s ${datadir}/06/${dtype}_rej_${ftype}_list ]; then
        export fexist3=.true.
        cp ${datadir}/06/${dtype}_rej_${ftype}_list ./${dtype}_rej_${ftype}_list_06
     else
        export fexist3=.false.
     fi
     if [ -s ${datadir}/09/${dtype}_rej_${ftype}_list ]; then
          export fexist4=.true.
        cp ${datadir}/09/${dtype}_rej_${ftype}_list ./${dtype}_rej_${ftype}_list_09
     else
        export fexist4=.false.
     fi
     if [ -s ${datadir}/12/${dtype}_rej_${ftype}_list ]; then
        export fexist5=.true.
        cp ${datadir}/12/${dtype}_rej_${ftype}_list ./${dtype}_rej_${ftype}_list_12
     else
        export fexist5=.false.
     fi

    rm -f input

cat << EOF > input
 &input
  fileexist(1)=${fexist1},fileexist(2)=${fexist2},fileexist(3)=${fexist3},
  fileexist(4)=${fexist4},fileexist(5)=${fexist5},dtype='${dtype}',
  filetype='rej'
/
EOF

  $execfile <input >stdout 2>&1 
  mv stdout ${dtype}_rej_${ftype}_stdout

done
done



done
  

