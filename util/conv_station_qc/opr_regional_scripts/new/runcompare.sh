#!/bin/sh
set -xa

 export scripts=/u/Xiujuan.Su/home/gsiqc3/regional_scripts 
 export exec=/u/Xiujuan.Su/home/gsiqc3/sorc_reg
 export exefile=compare.x 
 export datadir=/ptmp/Xiujuan.Su/bufrstas_regional
 export savedir=/u/Xiujuan.Su/home/gsiqc3/data/201101-03
 export NDATE=/nwprod/util/exec/ndate
 export  ACCOUNT=GDAS-T2O
export SUB=/u/wx20mi/bin/sub
export USER=Xiujuan.Su
export LOGDIR=/ptmp/Xiujuan.Su/plot_gsiqc3


# sdate=2011010100
# edate=2011010218

# pdate=201101-03

 mkdir -p $savedir

 rdate=$sdate

 tmpdir=/ptmp/Xiujuan.Su/reg_compare


## to read data into one file for each type

#dtype="ps120 ps180 ps181 ps187 ps188  q120 q180 q181 q187 q188 t120 t180 t181 t187 t188 uv220 uv221 uv223 uv224 uv229 uv280 uv281 uv287 uv288" 

#dtype="ps120 ps180 ps181 ps187 ps188 q180 q181 q187 q188 t180 t181 t187 t188 uv280 uv281 uv287 uv288"
dtype="uv220"
 mkdir -p $tmpdir/$ttm
 cd $tmpdir


 cp $exec/$exefile ./$exefile

for stype in $dtype
do

cp $datadir/00/${stype}_station ./${stype}_station_00
cp $datadir/00/${stype}_stas ./${stype}_stas_00
cp $datadir/03/${stype}_station ./${stype}_station_03
cp $datadir/03/${stype}_stas ./${stype}_stas_03
cp $datadir/06/${stype}_station ./${stype}_station_06
cp $datadir/06/${stype}_stas ./${stype}_stas_06
cp $datadir/09/${stype}_station ./${stype}_station_09
cp $datadir/09/${stype}_stas ./${stype}_stas_09
cp $datadir/12/${stype}_station ./${stype}_station_12
cp $datadir/12/${stype}_stas ./${stype}_stas_12

if [ "${stype}" = 'q120' -o "${stype}" = 't120' -o "${stype}" = 'uv220' -o \
    "${stype}" = 'uv221' -o "${stype}" = 'uv223' -o "${stype}" = 'uv224' \
    -o "${stype}" = 'uv229' ]; then
 itype=1
else
itype=0
fi


cat <<EOF >input
      &input
      dtype='$stype',itype=$itype
/
EOF


 cp $exec/$exefile ./$exefile

./$exefile <input>stdout 2>&1

 mv stdout ${stype}_stdout

 rm -f ${stype}_station_00
 rm -f ${stype}_station_03
 rm -f ${stype}_station_06
 rm -f ${stype}_station_09
 rm -f ${stype}_station_12
 rm -f ${stype}_stas_00
 rm -f ${stype}_stas_03
 rm -f ${stype}_stas_06
 rm -f ${stype}_stas_09
 rm -f ${stype}_stas_12


done    ## type


exit
