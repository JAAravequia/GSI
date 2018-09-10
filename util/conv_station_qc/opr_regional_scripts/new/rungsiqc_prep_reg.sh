#!/bin/sh
set -xa

 export scripts=$1
 export exec=$2
 export datadir=$3
 export exefile=$4
 export savedir=$5
 export SUB=$6
 sdate=$6
 edate=$7
# export scripts=/u/Xiujuan.Su/home/gsiqc3/scripts
# export exec=/u/Xiujuan.Su/home/gsiqc3/exec
# export datadir=/u/Xiujuan.Su/nbns/prd11q1f_cnv
# export exefile=post_cnvdiag
# export savedir=/u/Xiujuan.Su/nbns/postevent/post
 export NDATE=/nwprod/util/exec/ndate
 export  ACCOUNT=GDAS-T2O
#export SUB=/u/wx20mi/bin/sub
export USER=Xiujuan.Su
export LOGDIR=/ptmp/Xiujuan.Su/plot_gsiqc3


# sdate=2011010100
# edate=2011033118
# edate=2011010100


 mkdir -p $savedir

 rdate=$sdate

 tmpdir=/ptmp/Xiujuan.Su/regional_prepbufr


 mkdir -p  $tmpdir
 cd $tmpdir
 rm -f *

while [ $rdate -le $edate ]; do

 cdate=`echo $rdate | cut -c1-8`

 for hour in 00 06 12 18
  do
 for ttm in 03 06 09 12
  do
 rm -f cnvstat.t${hour}z.tm${ttm}
 rm -f prepbufr.t${hour}z.tm${ttm}

# cp ${datadir}/${cdate}/ndas.t${hour}z.prepbufr.tm${ttm} ./prepbufr.t${hour}z.tm${ttm}
# cp $datadir/${cdate}/ndas.t${hour}z.cnvstat.tm${ttm}  ./cnvstat.t${hour}z.tm${ttm}
cp ${datadir}/ndas.$cdate/ndas.t${hour}z.cnvstat.tm${ttm} ./cnvstat.t${hour}z.tm${ttm}
cp ${datadir}/ndas.$cdate/ndas.t${hour}z.prepbufr.tm${ttm} ./prepbufr.t${hour}z.tm${ttm}
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


for hour in 00 06 12 18
  do
 for ttm in 00 
  do
 rm -f cnvstat.t${hour}z.tm${ttm}
 rm -f prepbufr.t${hour}z.tm${ttm}

 cp ${datadir}/nam.${cdate}/nam.t${hour}z.prepbufr.tm${ttm} ./prepbufr.t${hour}z.tm${ttm}
 cp $datadir/nam.${cdate}/nam.t${hour}z.cnvstat.tm${ttm}  ./cnvstat.t${hour}z.tm${ttm}

 if [ -s prepbufr.t${hour}z.tm${ttm} -a -s cnvstat.t${hour}z.tm${ttm} ];then

 adate=${cdate}$hour
 ddate=`/nwprod/util/exec/ndate -$ttm $adate`
 echo $ddate
 rm -f diag_conv_anl.$ddate
 rm -f diag_conv_ges.$ddate
 rm -f *Z
 tar -xvf cnvstat.t${hour}z.tm${ttm}
 uncompress *Z
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



rdate=`$NDATE 24 $rdate`
done

ddate=`$NDATE -72 $rdate`

rm -f $savedir/prepbufr.post.${ddate}* 
exit

