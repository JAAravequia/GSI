#8!/bin/sh
set -xa
################################################################################
####  UNIX Script Documentation Block
# Script name: rungsiqc.sh
# Script description:this script is a driver to run station quality control package 
# Author: Xiujuan Su    Org: NP20         Date: 2010-09-12

# Script history log:
# 2018-09-14: X. Su  clean up comment lines and document the script
#
#  usage: rungsiqc.sh rungsiqc_stas.sh rungsiqc_selet.sh runreadwrite_list.sh plot.sh 
#                     runmake_time_list.sh process_global_maps.bash transferhtml.sh 
#                     runmake_sdm_list.sh
#
#  rungsiqc_stas.sh :   the script to get statistics of O-A and O-B all stations of 
#                       each data type
#  rungsiqc_selet.sh:   get bias and rejection station list based on O-A and O-B statistic
#                       of each station
#  runreadwrite_list.sh:rewrite bias and rejection station lists in simple format  
#  plot.sh             :the driver to plot all figures for monitor web site
#  runmake_time_list.sh:rewrite the lists for google map dispaly programs
#  process_global_maps.bash:make files for google display 
#  transferhtml.sh:    transfer html file to the machine where web site locates
#  runmake_sdm_list.sh:rewrite the lists in sdm rejection list format  
#     
# sdate: starting time
# edate: end of time
# ppdate: the period of data processed
# scripts:  the scripts directory
# gscripts: grad script directory
# exec   : the excutable file directory 
# datadir: The prepbufr file and GSI diagnostic file directory
# HOMEcfs: The directory of program to put GSi diagnostic file to prepbufr file
# EXECcfs: The executable file to put GSi diagnostic file to prepbufr file
# USHcfs : The script directory used by the program to put GSi diagnostic file
#          to prepbufr file
# NDATE  : The utility to change date
# prepbufr.x: the excutable file reads prepbufr file and put data to each station of each type array
# readdata.x: read array with each data type and each station information
# stas.x: calculate all statistics for each station
# select.x: create the bias and rejection station lists 
# readwrite_list_glb.x: rewrite bias and rejection station lists in simple format 
# make_list_time.x : rewrite th elist for goolge map files
# make_sdmlist.x: rewrite the lists in sdm rejection list format 
# savedir:the output prepbufr file with gsi daignostic file information, 
#         and files with each type and station information directory
# stasdir: the directory where all station statistic files locate
# selectdir: the directory where bias and rejection station list selection performed
# listdir  : the directory to make the list for google map dispaly files
# fixfile: the convinfo file where locates
# htmldir: the directory for html files for web
# mappngdir: horizontal region map 
# bashdir: the directory to produce google map display files
# savedir2: the bias and rejection station list save directory 
# sdmlistdir: the directory to produce list in sdm rejection list format
# sdmsave : the save directory for lists in sdm format



### define the script, data directory and excutable 
### file for each run

 export  sdate=2018081000
 export  edate=2018101018
 export  ppdate='20180810-1010'

 export qchome=/u/Xiujuan.Su/home/opr_gsiqc3
 export scripts=$qchome/scripts
 export gscripts=$qchome/grads
 export exec=$qchome/exec
 export datadir=/gpfs/hps/nco/ops/com/gfs/prod
 export HOMEcfs=/u/Xiujuan.Su/home/parafits
 export EXECcfs=/u/Xiujuan.Su/home/parafits/exec
 export USHcfs=/u/Xiujuan.Su/home/parafits/ush
 export NDATE=/nwprod/util/exec/ndate
 export exefile2=prepbufr.x
 export exefile3=readdata.x
 export exefile4=stas.x
 export exefile5=select.x
 export exefile6=readwrite_list_glb.x
 export exefile7=make_list_time.x
 export exefile8=make_sdmlist.x
 export savedir=/u/Xiujuan.Su/nbns/postevent/data_global
 export stasdir=/ptmpp1/$USER/gsiqc3/bufrstas
 export selectdir=/ptmpp1/$USER/gsiqc3/select
 export listdir=/ptmpp1/$USER/gsiqc3/make_list_global 
 export fixfile=/nwprod/fix/global_convinfo.txt
 export htmldir=$qchome/html
 export mappngdir=$qchome/mappng
 export bashdir=$qchome/google_maps/google_v3
 export savedir2=$qchome/data/$ppdate
 export sdmlistdir=/ptmpp1/$USER/gsiqc3/make_global_sdmlist
 export sdmsave=$qchome/data/sdmlist/$ppdate

## define plot destination
#export WSHOME=/home/people/emc/www/htdocs/gmb/gdas/station
#export WSUSER=wd20xs
#export WS=emcrzdm.ncep.noaa.gov
export WSHOME=/export/emc-lw-xsu/wd20xs/gsiqc3/$ppdate
export WSUSER=wd20xs
export WS=emc-lw-xsu.ncep.noaa.gov

 export NDATE=/nwprod/util/exec/ndate
 export  ACCOUNT=GDAS-T2O
export SUB=/global/save/emc.glopara/svn/gfs/trunk.r42957/para/bin/psub

###  this data type category is for script rungsiqc_type.sh 
export sub="ADPUPA PROFLR VADWND ADPSFC SFCSHP"
export dtype1="ps120 q120 t120 uv220 uv221"
export dtype2="ps120 uv221"
export dtype3="uv223"
export dtype4="uv224 uv229"
export dtype5="ps181 ps187 q181 q187 t181 t187 uv281 uv287"
export dtype6="ps180 q180 t180 uv280"
###

##  divide data types into different category
##  surface data types, sonde data types for non wind data and wind data

export sfctype='ps120 ps180 ps181 ps187 q180 q181 q187 t180 t181 t187'
export sondtype='q120 t120'
export uvsfctype='uv280 uv281 uv287'
export uvsondtype='uv220 uv221 uv223 uv224 uv228 uv229'
export datatype='120 180 181 187 121 123 124 128 129'
#export datatype='180 '

### put diagnostic file into prepbufr files
## /bin/sh $scripts/rungsiqc_prep.sh $scripts $datadir $savedir $sdate $edate $HOMEcfs $EXECcfs $USHcfs $NDATE

### read post prepbufr file and regrop the data into different types, there are some statements about types, need to check 
## There are some data types in rungsiqc_type.sh, have to go in to check whether
###  data types in this script covers all 
#/bin/sh $scripts/rungsiqc_type.sh $scripts $exec $savedir $exefile $savedir $sdate $edate $fixfile "$sub" "$dtype1" "$dtype2" "$dtype3" "$dtype4" "$dtype5" "$dtype6"

### read different data type to do statistics, in this step, 90 pencitile for station
### statistics isthe base to determine to criteria to put station to rejection or 
###bias correction lst  
#/bin/sh $scripts/rungsiqc_stas.sh $scripts $exec $exefile3 $exefile4 $savedir $savedir2 $stasdir $sdate $edate $ppdate $fixfile "${sfctype}" "${sondtype}" "${uvsfctype}" "${uvsondtype}" 

###  get final selection for the station with statistics from last step  
#/bin/sh $scripts/rungsiqc_selet.sh $scripts $exec $exefile5 $stasdir $savedir2 $sdate $edate $ppdate "$sfctype" "$sondtype" "$uvsfctype" "$uvsondtype"

##  rewrite the list output format
/bin/sh $scripts/runreadwrite_list.sh $ppdate $scripts $savedir2 $exec ${exefile6} "${sfctype}" "${sondtype}" "${uvsfctype}" "${uvsondtype}"  
#### plot all figures
#/bin/sh $scripts/plot.sh $scripts $gscripts $sdate $edate $stasdir $WSHOME $WSUSER $WS $ppdate "${sfctype}" "${sondtype}" "${uvsfctype}" "${uvsondtype}" 

## make the list for google map plotting 
/bin/sh $scripts/runmake_time_list.sh $scripts $exec ${exefile7} $stasdir $savedir2 $listdir "${sfctype}" "${sondtype}" "${uvsfctype}" "${uvsondtype}"

##  make files for google map 
/bin/sh  $scripts/process_global_maps.bash $ppdate $listdir $WSHOME $WSUSER $WS "${sfctype}" "${sondtype}" "${uvsfctype}" "${uvsondtype}" $bashdir 

##  transfer html files
/bin/sh $scripts/transferhtml.sh $htmldir $WSHOME $WSUSER $WS $ppdate $mappngdir   

## make the list format as SDM list format 

/bin/sh $scripts/runmake_sdm_list.sh $scripts $exec ${exefile8} ${savedir2} ${sdmlistdir} "${datatype}" $sdmsave 

exit

