#!/bin/sh
set -xa
################################################################################
####  UNIX Script Documentation Block
# Script name: rungsiqc_reg.sh
# Script description:this script is a driver to run regional station quality control package 
# Author: Xiujuan Su    Org: NP20         Date: 2010-09-12

# Script history log:
# 2018-10-30: X. Su  clean up comment lines and document the script
#
#  usage: rungsiqc_reg.sh rungsiqc_stas_reg.sh rungsiqc_selet_reg.sh runcompare_list_qlyrej_reg.sh
                      runcompare_list_reg.sh runreadwrite_list_reg.sh plot_reg.sh 
#                     runmake_time_list_reg.sh process_global_maps_reg.bash transferhtml_reg.sh 
#                     runmake_sdm_list_reg.sh
#
#  rungsiqc_stas_reg.sh :   the script to get statistics of O-A and O-B all stations of 
#                            each data type
#  rungsiqc_selet_reg.sh:   get bias and rejection station list based on O-A and O-B statistic
#                            of each station
#  runcompare_list_reg.sh   :this script compares rejection and bias station lists from 
#                        different regional analysis delay time and gets final bias
#                         end rejection lists
#  
#  
#  runreadwrite_list_reg.sh:rewrite bias and rejection station lists in simple format  
#  plot_reg.sh             :the driver to plot all figures for monitor web site
#  runmake_time_list_reg.sh:rewrite the lists for google map dispaly programs
#  process_global_maps_reg.bash:make files for google display 
#  transferhtml_reg.sh:    transfer html file to the machine where web site locates
#  runmake_sdm_list_reg.sh:rewrite the lists in sdm rejection list format  
#     
# sdate: starting time
# edate: end of time
# ppdate: the period of data processed
# scripts:  the scripts directory
# gscripts: grad script directory
# exec   : the excutable file directory 
# datadir: The prepbufr file and GSI diagnostic file directory
# exefile1(post_cnvdiag): the excutable file to read GSI diagnostic file and put information to prepbufr file
# exefile2(prepbufr.x): the excutable file reads prepbufr file and put data to each station of each type array
# exefile3(readdata.x): read array with each data type and each station information
# exefile4(stas.x):calculate all statistics for each station
# exefile5(select_reg.x):create the bias and rejection station lists
# exefile6(compare_list.x):  compares rejection and bias station lists from different 
#                            regional analysis delay time and gets final bias
#                            and rejection lists
# exefile7(compare_list_sp.x) : compares rejection and bias station wind speed lists from different
#                               regional analysis delay time and gets final bias
#                               and rejection lists
# exefile8(compare_list_qlyrej.x) : compares rejection and bias station listsrejected by quslity control 
#                                    from different regional analysis delay time and gets final bias
#                                   and rejection lists
# exefile9(readwrite_list_reg.x):  rewrite bias and rejection station lists in simple format 
# exefile10(make_list_time_reg.x): rewrite th elist for goolge map files
# exefile11(make_sdmlist.x): rewrite the lists in sdm rejection list format
# savedir: temporay directry to read gsi diagnostic file and put into prepbufr file
# savedir2:the output prepbufr file with gsi daignostic file information, 
#         and files with each type and station information directory 
# stasdir: the directory where all station statistic files locate
# selectdir: the directory where bias and rejection station list selection performed
# savedir3: the bias and rejection station list save directory
# global_data: global bias and rejection station list save directory
# fixfile: convinfo file directory
# stasdir_glb: the directory where all station statistic files for global locate
# listdir: the directory to make the list for google map dispaly files
# htmldir: the directory for html files for web
# bashdir: the directory to produce google map display files
# htmldir: the directory for html files for web
# sdmlistdir: the directory to produce list in sdm rejection list format
# sdmsave : the save directory for lists in sdm format
# sdmsave_glb: the save directory for listsf global  in sdm format

   



### define the script, data directory and excutable 
### file for each run
 
 export  sdate=2018081000
 export  edate=2018101018
 export  ppdate='20180810-1010'

 export qchome=/u/Xiujuan.Su/home/opr_gsiqc3
 export scripts=$qchome/regional_scripts
 export gscripts=$qchome/grads
 export exec=$qchome/exec
 export datadir=/com/nam/prod
 export exefile1=post_cnvdiag
 export exefile2=prepbufr.x
 export exefile3=readdata.x
 export exefile4=stas.x
 export exefile5=select_reg.x
 export exefile6=compare_list.x
 export exefile7=compare_list_sp.x
 export exefile8=compare_list_qlyrej.x
 export exefile9=readwrite_list_reg.x
 export exefile10=make_list_time_reg.x
 export exefile11=make_sdmlist.x
 export savedir=/ptmpp1/$USER/gsiqc3/post_regional
 export savedir2=/u/Xiujuan.Su/nbns/postevent/data_regional
 export stasdir=/ptmpp1/$USER/gsiqc3/bufrstas_reg
 export selectdir=/ptmpp1/$USER/gsiqc3/select_reg
 export savedir3=$qchome/regional_data/${ppdate}
 export global_data=$qchome/data/$ppdate
 export fixfile=/nwprod/fix/nam_regional_convinfo.txt
 export stasdir_glb=/ptmpp1/$USER/gsiqc3/bufrstas
 export listdir=/ptmpp1/$USER/gsiqc3/make_list_region
 export htmldir=$qchome/html
 export bashdir=$qchome/google_maps/google_v3
 export sdmlistdir=/ptmpp1/$USER/gsiqc3/make_reg_sdmlist
 export sdmsave=$qchome/regional_data/sdmlist/$ppdate
 export sdmsave_glb=$qchome/data/sdmlist/$ppdate
 export datatype='120 180 181 187 188 121 123 124 128 129'

mkdir -p $savedir
mkdir -p $savedir2
mkdir -p $stasdir
mkdir -p $savedir3

## define plot destination
#export WSHOME=/home/people/emc/www/htdocs/gmb/gdas/station
#export WSUSER=wd20xs
#export WS=emcrzdm.ncep.noaa.gov
export WSHOME=/export/emc-lw-xsu/wd20xs/gsiqc3/$ppdate
export WSUSER=wd20xs
export WS=emc-lw-xsu.ncep.noaa.gov
 export NDATE=/nwprod/util/exec/ndate
 export  ACCOUNT=GDAS-T2O
export SUB=/gpfs/gd1/emc/global/save/emc.glopara/svn/gfs/trunk/para/bin/sub_wcoss

###  the types and subtypes to be processed, it has to go scripts to change the type or sab types
sub="ADPUPA PROFLR VADWND ADPSFC SFCSHP MSONET"
dtype1="ps120 q120 t120 uv220 uv221"
dtype2="ps120 uv221"
dtype3="uv223 uv229"
dtype4="uv224"
dtype5="ps181 ps187 q181 q187 t181 t187 uv281 uv287"
dtype6="ps180 q180 t180 uv280"
dtype7="ps188 q188 t188 uv288"


export sfctype='ps120 ps180 ps181 ps187 ps188 q180 q181 q187 q188 t180 t181 t187 t188'
export sondtype='q120 t120'
export uvsfctype='uv280 uv281 uv287 uv288'
#export uvsfctype='uv288'
export uvsondtype='uv220 uv221 uv223 uv224 uv228 uv229' 

export tm='00 03 06' 

###

### put diagnostic file into prepbufr files
##/bin/sh $scripts/rungsiqc_prep_reg.sh $scripts $exec $datadir $exefile1 $savedir $sdate $edate "${tm}" 

### read post prepbufr file and regrop the data into different types
##/bin/sh $scripts/rungsiqc_type_reg.sh $scripts $exec $savedir $exefile2 $savedir2 $sdate $edate $fixfile "$sub" "$dtype1" "$dtype2" "$dtype3" "$dtype4" "$dtype5" "$dtype6" "$dtype7" ${exefile3} $datadir "${tm}"

### read different data type to do statistics, in this step, 90 pencitile for station
### statistics isthe base to determine to criteria to put station to rejection or 
###bias correction lst  
/bin/sh $scripts/rungsiqc_stas_reg.sh $scripts $exec $exefile3 $exefile4 ${savedir2} ${savedir3} $stasdir $sdate $edate $ppdate $fixfile "${sfctype}" "${sondtype}" "${uvsfctype}" "${uvsondtype}" "${tm}"

###  get final selection for the station with statistics from last step  
/bin/sh $scripts/rungsiqc_selet_reg.sh $scripts $exec $exefile5 $stasdir $savedir3 $sdate $edate $ppdate $selectdir "${sfctype}" "${sondtype}" "${uvsfctype}" "${uvsondtype}" "${tm}" 

### compare the rejection and bias correction lists from different time delay analysis 

/bin/sh $scripts/runcompare_list_reg.sh $ppdate $scripts $selectdir $stasdir $exec $exefile6 $exefile7 $savedir3 "${sfctype}" "${sondtype}" "${uvsfctype}" "${uvsondtype}" "${tm}" 

### compare the rejection lists by quality control procedure from diferent time delay analysis

/bin/sh $scripts/runcompare_list_qlyrej_reg.sh $ppdate $scripts $selectdir $stasdir $exec $exefile8 $savedir3 "${sfctype}" "${sondtype}" "${uvsfctype}" "${uvsondtype}" "${tm}" 


/bin/sh $scripts/runreadwrite_list_reg.sh $ppdate $scripts $savedir3 $exec ${exefile9} "${sfctype}" "${sondtype}" "${uvsfctype}" "${uvsondtype}"

/bin/sh $scripts/plot_reg.sh $scripts $gscripts $sdate $edate $stasdir $WSHOME $WSUSER $WS $ppdate "${sfctype}" "${sondtype}" "${uvsfctype}" "${uvsondtype}" "${tm}"

## make the list the google map

/bin/sh $scripts/runmake_time_list_reg.sh $scripts $exec ${exefile10} $stasdir $savedir3 $listdir ${global_data} "${sfctype}" "${sondtype}" "${uvsfctype}" "${uvsondtype}" "${tm}"    

#/bin/sh  $scripts/process_regional_maps_reg.sh $ppdate ${listdir} ${WSHOME} ${WSUSER} $WS "${sfctype}" "${sondtype}" "${uvsfctype}" "${uvsondtype}" $bashdir "${tm}" 

##  transfer html files
/bin/sh $scripts/transferhtml_reg.sh $htmldir $WSHOME $WSUSER $WS $ppdate

## make the list format as SDM list format

/bin/sh $scripts/runmake_sdm_list_reg.sh $scripts $exec ${exefile11} ${savedir3} ${sdmlistdir} "${datatype}" $sdmsave $sdmsave_glb

exit

