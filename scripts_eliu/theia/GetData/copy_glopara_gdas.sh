#!/bin/sh -xvf

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

   srcdir=${base_srcdir}/gdas.${yyyymmdd}/${hh}
   outdir=${base_outdir}/gdas.${yyyymmdd}/${hh}

   mkdir -p ${outdir} 

   cd ${srcdir}

#  flist="03 04 05 06 07 08 09"
   flist="03 06 09"
   prefix=gdas.t${hh}z

   cp ${prefix}.atminc.nc ${outdir} &

   for fh in $flist; do
      sigfile=${prefix}.atmf0${fh}.nemsio
      sfcfile=${prefix}.sfcf0${fh}.nemsio
      cp $sigfile ${outdir} & 
      cp $sfcfile ${outdir} & 
   done
   cp ${prefix}.atmges.nemsio ${outdir} &
   cp ${prefix}.atmgm3.nemsio ${outdir} &
   cp ${prefix}.atmgp3.nemsio ${outdir} &

   cp ${prefix}.abias     ${outdir} & 
   cp ${prefix}.abias_pc  ${outdir} &
   cp ${prefix}.abias_air ${outdir} &

   cdate=`ndate +6 ${cdate}`

done

