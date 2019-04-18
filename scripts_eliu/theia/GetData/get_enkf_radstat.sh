#!/bin/sh -xvf

expid=prfv3rt1

bdate=2018082412
edate=2018082412
cdate=${bdate}

inst_sat=amsua_metop-a


hpss_base_dir=/NCEPDEV/emc-global/5year/emc.glopara/WCOSS_C/Q2FY19/$expid
data_dir=/scratch4/NCEPDEV/stmp4/Emily.Liu/EnKF/$expid
mkdir -p ${data_dir}
cd ${data_dir}

while [[ ${cdate} -le ${edate} ]]; do

   y4=`echo $cdate | cut -c1-4`
   m2=`echo $cdate | cut -c5-6`
   d2=`echo $cdate | cut -c7-8`
   h2=`echo $cdate | cut -c9-10`

   yyyymmdd=${y4}${m2}${d2}
   hh=${h2}
   yyyymmddhh=${yyyymmdd}${hh}

   hpss_dir=${hpss_base_dir}/${yyyymmdd}${hh}

   igrp=1 
   imem=1
   nens=10
   while [[ $imem -le $nens ]]; do
      member="mem"`printf %03i $imem`
      group="grp"`printf %02i $igrp`
      hpsstar get ${hpss_dir}/enkf.gdas_${group}.tar ./enkf.gdas.${yyyymmdd}/${hh}/${member}/gdas.t${hh}z.radstat
      mv ./enkf.gdas.${yyyymmdd}/${hh}/${member}/gdas.t${hh}z.radstat ${data_dir}/gdas.${yyyymmddhh}.${member}.radstat 
      tar -xf gdas.${yyyymmddhh}.${member}.radstat diag_${inst_sat}_ges.${yyyymmddhh}_${member}
      rm -f gdas.${yyyymmddhh}.${member}.radstat
      (( imem = $imem + 1 ))
   done
   rm -rf ./enkf.gdas.${yyyymmdd}

   igrp=2 
   imem=11
   nens=20
   while [[ $imem -le $nens ]]; do
      member="mem"`printf %03i $imem`
      group="grp"`printf %02i $igrp`
      hpsstar get ${hpss_dir}/enkf.gdas_${group}.tar ./enkf.gdas.${yyyymmdd}/${hh}/${member}/gdas.t${hh}z.radstat
      mv ./enkf.gdas.${yyyymmdd}/${hh}/${member}/gdas.t${hh}z.radstat ${data_dir}/gdas.${yyyymmddhh}.${member}.radstat 
      tar -xf gdas.${yyyymmddhh}.${member}.radstat diag_${inst_sat}_ges.${yyyymmddhh}_${member}
      rm -f gdas.${yyyymmddhh}.${member}.radstat
      (( imem = $imem + 1 ))
   done
   rm -rf ./enkf.gdas.${yyyymmdd}

   igrp=3 
   imem=21
   nens=30
   while [[ $imem -le $nens ]]; do
      member="mem"`printf %03i $imem`
      group="grp"`printf %02i $igrp`
      hpsstar get ${hpss_dir}/enkf.gdas_${group}.tar ./enkf.gdas.${yyyymmdd}/${hh}/${member}/gdas.t${hh}z.radstat
      mv ./enkf.gdas.${yyyymmdd}/${hh}/${member}/gdas.t${hh}z.radstat ${data_dir}/gdas.${yyyymmddhh}.${member}.radstat 
      tar -xf gdas.${yyyymmddhh}.${member}.radstat diag_${inst_sat}_ges.${yyyymmddhh}_${member}
      rm -f gdas.${yyyymmddhh}.${member}.radstat
      (( imem = $imem + 1 ))
   done
   rm -rf ./enkf.gdas.${yyyymmdd}

   igrp=4 
   imem=31
   nens=40
   while [[ $imem -le $nens ]]; do
      member="mem"`printf %03i $imem`
      group="grp"`printf %02i $igrp`
      hpsstar get ${hpss_dir}/enkf.gdas_${group}.tar ./enkf.gdas.${yyyymmdd}/${hh}/${member}/gdas.t${hh}z.radstat
      mv ./enkf.gdas.${yyyymmdd}/${hh}/${member}/gdas.t${hh}z.radstat ${data_dir}/gdas.${yyyymmddhh}.${member}.radstat 
      tar -xf gdas.${yyyymmddhh}.${member}.radstat diag_${inst_sat}_ges.${yyyymmddhh}_${member}
      rm -f gdas.${yyyymmddhh}.${member}.radstat
      (( imem = $imem + 1 ))
   done
   rm -rf ./enkf.gdas.${yyyymmdd}

   igrp=5 
   imem=41
   nens=50
   while [[ $imem -le $nens ]]; do
      member="mem"`printf %03i $imem`
      group="grp"`printf %02i $igrp`
      hpsstar get ${hpss_dir}/enkf.gdas_${group}.tar ./enkf.gdas.${yyyymmdd}/${hh}/${member}/gdas.t${hh}z.radstat
      mv ./enkf.gdas.${yyyymmdd}/${hh}/${member}/gdas.t${hh}z.radstat ${data_dir}/gdas.${yyyymmddhh}.${member}.radstat 
      tar -xf gdas.${yyyymmddhh}.${member}.radstat diag_${inst_sat}_ges.${yyyymmddhh}_${member}
      rm -f gdas.${yyyymmddhh}.${member}.radstat
      (( imem = $imem + 1 ))
   done
   rm -rf ./enkf.gdas.${yyyymmdd}

   igrp=6 
   imem=51
   nens=60
   while [[ $imem -le $nens ]]; do
      member="mem"`printf %03i $imem`
      group="grp"`printf %02i $igrp`
      hpsstar get ${hpss_dir}/enkf.gdas_${group}.tar ./enkf.gdas.${yyyymmdd}/${hh}/${member}/gdas.t${hh}z.radstat
      mv ./enkf.gdas.${yyyymmdd}/${hh}/${member}/gdas.t${hh}z.radstat ${data_dir}/gdas.${yyyymmddhh}.${member}.radstat 
      tar -xf gdas.${yyyymmddhh}.${member}.radstat diag_${inst_sat}_ges.${yyyymmddhh}_${member}
      rm -f gdas.${yyyymmddhh}.${member}.radstat
      (( imem = $imem + 1 ))
   done
   rm -rf ./enkf.gdas.${yyyymmdd}

   igrp=7 
   imem=61
   nens=70
   while [[ $imem -le $nens ]]; do
      member="mem"`printf %03i $imem`
      group="grp"`printf %02i $igrp`
      hpsstar get ${hpss_dir}/enkf.gdas_${group}.tar ./enkf.gdas.${yyyymmdd}/${hh}/${member}/gdas.t${hh}z.radstat
      mv ./enkf.gdas.${yyyymmdd}/${hh}/${member}/gdas.t${hh}z.radstat ${data_dir}/gdas.${yyyymmddhh}.${member}.radstat 
      tar -xf gdas.${yyyymmddhh}.${member}.radstat diag_${inst_sat}_ges.${yyyymmddhh}_${member}
      rm -f gdas.${yyyymmddhh}.${member}.radstat
      (( imem = $imem + 1 ))
   done
   rm -rf ./enkf.gdas.${yyyymmdd}

   igrp=8 
   imem=71
   nens=80
   while [[ $imem -le $nens ]]; do
      member="mem"`printf %03i $imem`
      group="grp"`printf %02i $igrp`
      hpsstar get ${hpss_dir}/enkf.gdas_${group}.tar ./enkf.gdas.${yyyymmdd}/${hh}/${member}/gdas.t${hh}z.radstat
      mv ./enkf.gdas.${yyyymmdd}/${hh}/${member}/gdas.t${hh}z.radstat ${data_dir}/gdas.${yyyymmddhh}.${member}.radstat 
      tar -xf gdas.${yyyymmddhh}.${member}.radstat diag_${inst_sat}_ges.${yyyymmddhh}_${member}
      rm -f gdas.${yyyymmddhh}.${member}.radstat
      (( imem = $imem + 1 ))
   done
   rm -rf ./enkf.gdas.${yyyymmdd}

   cdate=`ndate +6 ${cdate}`

done

