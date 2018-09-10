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
export sfctype=${8}
 export sondtype=${9}
 export uvsfctype=${10}
 export uvsondtype=${11}
 export alltm=${12}

# dtime=20120103
# export scripts=/u/Xiujuan.Su/home/gsiqc3/regional_scripts
# export datadir=/u/Xiujuan.Su/nbns/select_reg_20120103
# export datadir2=/u/Xiujuan.Su/nbns/bufrstas_regional 
# export exec=/u/Xiujuan.Su/home/gsiqc3/exec
# export execfile=compare_list_qlyrej.x
# export savedir=/u/Xiujuan.Su/home/gsiqc3/regional_data/$dtime
 export tmpdir=/ptmpp1/$USER/gsiqc3/compare_list_time_qlyrej_reg

 mkdir -p $savedir

mkdir -p $tmpdir

cd $tmpdir

rm -f *

cp $exec/$execfile ./$execfile
cp $exec/$execfile1 ./$execfile1
   
#stype="ps120 ps180 ps181 ps187 ps188 q120 q180 q181 q187 q188 t120 t180 t181 t187 t188 uv220 uv221 uv223 uv224 uv229 uv280 uv281 uv287 uv288"
#stype="ps120 ps180 ps181 ps187 ps188 q181 q187 q188 t180 t181 t187 t188"
#stype="t120 "
#sstype="uv220 uv221 uv223 uv224 uv229 uv280 uv281 uv287 uv288"
#stype="ps188" 


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
  

