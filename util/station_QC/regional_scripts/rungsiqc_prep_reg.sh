#!/bin/sh
set -xa

################################################################################
####  UNIX Script Documentation Block
# Script name: rungsiqc_prep_reg.sh
# Script description:this script put regional gsi diagnotic file to prepbufr
# Author: Xiujuan Su    Org: NP20         Date: 2010-09-12

# Script history log:
# 2018-09-14: X. Su  clean up comment lines and document the script
#
# usage: rungsiqc_prep_reg.sh scripts exec datadir iexecfile savedir sdate edate 
#
# scripts: the scripts directory
# exec   : the excutable file directory
# datadir: The prepbufr file and GSI diagnostic file directory
# exefile   : the excutable file
# savedir: the output prepbufr.post directory
# SUB: the script to submit jobs
# sdate: starting time
# edate: end of time
# alltm: all regional analysis delay time
 

 export scripts=$1
 export exec=$2
 export datadir=$3
 export exefile=$4
 export savedir=$5
 export SUB=$6
 export sdate=$6
 export edate=$7
 export alltm=$8

 LOGDIR=/ptmpp1/$USER/gsiqc3/plot_gsiqc3



 mkdir -p $savedir

 rdate=$sdate

 tmpdir=/ptmpp1/$USER/gsiqc3/regional_prepbufr


 mkdir -p  $tmpdir
 cd $tmpdir
 rm -f *

while [ $rdate -le $edate ]; do

 cdate=`echo $rdate | cut -c1-8`

 for hour in 00 06 12 18
  do
 for ttm in $alltm 
  do
 rm -f cnvstat.t${hour}z.tm${ttm}
 rm -f prepbufr.t${hour}z.tm${ttm}

#cp ${datadir}/ndas.$cdate/ndas.t${hour}z.cnvstat.tm${ttm} ./cnvstat.t${hour}z.tm${ttm}
#cp ${datadir}/ndas.$cdate/ndas.t${hour}z.prepbufr.tm${ttm} ./prepbufr.t${hour}z.tm${ttm}
cp ${datadir}/nam.$cdate/nam.t${hour}z.cnvstat.tm${ttm} ./cnvstat.t${hour}z.tm${ttm}
cp ${datadir}/nam.$cdate/nam.t${hour}z.prepbufr.tm${ttm} ./prepbufr.t${hour}z.tm${ttm}
 if [ -s prepbufr.t${hour}z.tm${ttm} -a -s cnvstat.t${hour}z.tm${ttm} ];then

 adate=${cdate}$hour
 ddate=`/nwprod/util/exec/ndate -$ttm $adate`
 echo $ddate
 rm -f diag_conv_anl.$ddate
 rm -f diag_conv_ges.$ddate
 rm -f *Z
 tar -xvf cnvstat.t${hour}z.tm${ttm}
# uncompress *Z
  gunzip -v *gz
 rm -f diag_conv_anl
 rm -f diag_conv_ges
 mv diag_conv_anl.$ddate diag_conv_anl
 mv diag_conv_ges.$ddate diag_conv_ges

 rm -f preppost_*
 rm -f fort*

 ln -sf prepbufr.t${hour}z.tm${ttm} fort.20

cp $exec/$exefile ./$exefile 
 ./$exefile >stdout 2>&1
$scripts/combfr preppost_* prepbufr.post

mv prepbufr.post $savedir/prepbufr.post.${adate}.tm${ttm}
mv stdout stdout.${adate}.tm${ttm}
rm -f cnvstat.t${hour}z.tm${ttm}
rm -f prepbufr.t${hour}z.tm${ttm}
fi
done
done


#for hour in 00 06 12 18
#  do
# for ttm in 00 
#  do
# rm -f cnvstat.t${hour}z.tm${ttm}
# rm -f prepbufr.t${hour}z.tm${ttm}
#
# cp ${datadir}/nam.${cdate}/nam.t${hour}z.prepbufr.tm${ttm} ./prepbufr.t${hour}z.tm${ttm}
# cp $datadir/nam.${cdate}/nam.t${hour}z.cnvstat.tm${ttm}  ./cnvstat.t${hour}z.tm${ttm}
#
# if [ -s prepbufr.t${hour}z.tm${ttm} -a -s cnvstat.t${hour}z.tm${ttm} ];then
#
# adate=${cdate}$hour
# ddate=`/nwprod/util/exec/ndate -$ttm $adate`
# echo $ddate
# rm -f diag_conv_anl.$ddate
# rm -f diag_conv_ges.$ddate
# rm -f *Z
# tar -xvf cnvstat.t${hour}z.tm${ttm}
# uncompress *Z
# rm -f diag_conv_anl
# rm -f diag_conv_ges
# mv diag_conv_anl.$ddate diag_conv_anl
# mv diag_conv_ges.$ddate diag_conv_ges
#
# rm -f preppost_*
# rm -f fort*
#
# ln -sf prepbufr.t${hour}z.tm${ttm} fort.20
#
#cp $exec/$exefile ./$exefile 
#./$exefile >stdout 2>&1
#$scripts/combfr preppost_* prepbufr.post
#mv prepbufr.post $savedir/prepbufr.post.${adate}.tm${ttm}
#mv stdout stdout.${adate}.tm${ttm}
#rm -f cnvstat.t${hour}z.tm${ttm}
#rm -f prepbufr.t${hour}z.tm${ttm}
#fi
#done
#done



rdate=`$NDATE 24 $rdate`
done

ddate=`$NDATE -72 $rdate`

rm -f $savedir/prepbufr.post.${ddate}* 
exit

