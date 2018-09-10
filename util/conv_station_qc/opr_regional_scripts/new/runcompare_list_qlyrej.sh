#!/bin/sh
set -xa

### define the script, data directory and excutable
### file for each run
dtime=$1
scripts=$2
datadir=$3
datadir2=$4
exec=$5
execfile=$6
savedir=$7

# dtime=20120103
# export scripts=/u/Xiujuan.Su/home/gsiqc3/regional_scripts
# export datadir=/u/Xiujuan.Su/nbns/select_reg_20120103
# export datadir2=/u/Xiujuan.Su/nbns/bufrstas_regional 
# export exec=/u/Xiujuan.Su/home/gsiqc3/exec
# export execfile=compare_list_qlyrej.x
# export savedir=/u/Xiujuan.Su/home/gsiqc3/regional_data/$dtime
 export tmpdir=/ptmp/Xiujuan.Su/compare_list_time_qlyrej

 mkdir -p $savedir

mkdir -p $tmpdir

cd $tmpdir

rm -f *

cp $exec/$execfile ./$execfile
cp $exec/$execfile1 ./$execfile1
   
stype="ps120 ps180 ps181 ps187 ps188 q120 q180 q181 q187 q188 t120 t180 t181 t187 t188 uv220 uv221 uv223 uv224 uv229 uv280 uv281 uv287 uv288"
#stype="ps120 ps180 ps181 ps187 ps188 q181 q187 q188 t180 t181 t187 t188"
#stype="t120 "
#sstype="uv220 uv221 uv223 uv224 uv229 uv280 uv281 uv287 uv288"
#stype="ps188" 


   for dtype in $stype 
      do

if [ "${dtype}" = 'q120' -o "${dtype}" = 't120' -o "${dtype}" = 'uv220' -o  "${dtype}" = 'uv229' -o \
    "${dtype}" = 'uv221' -o "${dtype}" = 'uv223' -o "${dtype}" = 'uv224' ]; then 
 itype=1
else
itype=0
fi

     if [ -s ${datadir}/00/${dtype}_qlyrej_list ]; then
     export    fexist1=.true.
        echo ${fexist1}
        cp ${datadir}/00/${dtype}_qlyrej_list ./${dtype}_qlyrej_list_00
     else
      export  fexist1=.false.
     fi
     if [ -s ${datadir}/03/${dtype}_qlyrej_list ]; then
        export fexist2=.true.
        cp ${datadir}/03/${dtype}_qlyrej_list ./${dtype}_qlyrej_list_03
     else
       export fexist2=.false.
     fi
     if [ -s ${datadir}/06/${dtype}_qlyrej_list ]; then
        export fexist3=.true.
        cp ${datadir}/06/${dtype}_qlyrej_list ./${dtype}_qlyrej_list_06
     else
        export fexist3=.false.
     fi
     if [ -s ${datadir}/09/${dtype}_qlyrej_list ]; then
          export fexist4=.true.
        cp ${datadir}/09/${dtype}_qlyrej_list ./${dtype}_qlyrej_list_09
     else
        export fexist4=.false.
     fi
     if [ -s ${datadir}/12/${dtype}_qlyrej_list ]; then
        export fexist5=.true.
        cp ${datadir}/12/${dtype}_qlyrej_list ./${dtype}_qlyrej_list_12
     else
        export fexist5=.false.
     fi

    rm -f input

cat << EOF > input
 &input
  fileexist(1)=${fexist1},fileexist(2)=${fexist2},fileexist(3)=${fexist3},
  fileexist(4)=${fexist4},fileexist(5)=${fexist5},dtype='${dtype}',
  itype=${itype}
/
EOF

  ./$execfile <input >stdout 2>&1 
  mv stdout ${dtype}_stdout
  mv  ${dtype}_qlyrej_list $savedir
done

  

