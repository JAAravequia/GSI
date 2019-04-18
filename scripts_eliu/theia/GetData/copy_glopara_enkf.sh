#!/bin/sh -xvf


expid=fv3fy18retro2
expid=prfv3rt1
expid=fv3test

bdate=2017082206
edate=2017082212
cdate=${bdate}

base_srcdir=/scratch4/NCEPDEV/stmp4/Emily.Liu/ROTDIR/$expid
base_outdir=/scratch4/NCEPDEV/stmp3/Emily.Liu/Data_Input/$expid

while [[ ${cdate} -le ${edate} ]]; do

   y4=`echo $cdate | cut -c1-4`
   m2=`echo $cdate | cut -c5-6`
   d2=`echo $cdate | cut -c7-8`
   h2=`echo $cdate | cut -c9-10`

   yyyymmdd=${y4}${m2}${d2}
   hh=${h2}

   srcdir=${base_srcdir}/enkf.gdas.${yyyymmdd}/${hh}
   outdir=${base_outdir}/enkf.gdas.${yyyymmdd}/${hh}

   mkdir -p ${outdir}
   cd ${srcdir}

   nens=80
   imem=1
   while [[ $imem -le $nens ]]; do
       member="mem"`printf %03i $imem`
       cp -r $member ${outdir} &
       (( imem = $imem + 1 ))
    done
    cp gdas.t${hh}z.sfcf006.ensmean.nemsio ${outdir} &

    cdate=`ndate +6 ${cdate}`

done

