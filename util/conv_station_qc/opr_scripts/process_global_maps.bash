#!/bin/bash
set -xa

export ppdate=$1
export CTL_DIR=$2
export WSHOME=$3
export WSUSER=$4
export WS=$5
export sfctype=${6}
export sondtype=${7}
export uvsfctype=${8}
export uvsondtype=${9}
export bashdir=${10}
#export ppdate=201403-05
#export bashdir=/u/Xiujuan.Su/home/gsiqc3/google_maps/google_v3
#export CTL_DIR=/ptmpp1/Xiujuan.Su/make_list_global
#export WSHOME=/export/emc-lw-xsu/wd20xs/gsiqc3/$ppdate
#export WSUSER=wd20xs
#export WS=emc-lw-xsu.ncep.noaa.gov

# web directory
export googlemap_time=$WSHOME/web/global/time/googlemap_htmls
export googlemap_vert=$WSHOME/web/global/vert/googlemap_htmls
##working directory
CWD=/ptmpp1/$USER/gsiqc3/googlemap_global
mkdir -p ${CWD}



##execs for creating php files (Time)
CREATE_MARKERS_MULTILEVEL=$bashdir/global/Time/tools/create_markers_multilevel
CREATE_MARKERS_MULTILEVEL_WIND=$bashdir/global/Time/tools/create_markers_multilevel_wind
CREATE_MARKERS_SURFACE=$bashdir/global/Time/tools/create_markers_surface

##execs for creating php files (Vertical)
CREATE_MARKERS_VERT_SOUNDINGS=$bashdir/global/Vertical/tools/create_markers_vert_soundings
CREATE_MARKERS_VERT_WIND=$bashdir/global/Vertical/tools/create_markers_vert_wind

### create html files
export CREATE_HTML=$bashdir/global/Time/tools/create_multilevel_html.bash
export CREATE_HTML_WIND=$bashdir/global/Time/tools/create_multilevel_html_wind.bash
export CREATE_HTML_SFC=$bashdir/global/Time/tools/create_surface_html.bash
export CREATE_HTML_SOND=$bashdir/global/Vertical/tools/create_vertical_html.bash

cd $CWD

##make php files for Time, including surface, multilevel, and wind

##make php files for multilevel
for datatype in ssfctype ssondtype suvsfctype suvsondtype
do
if [ "${datatype}" = "ssfctype" ]; then
dstype=$sfctype
elif [ "${datatype}" = "ssondtype" ]; then
dstype=$sondtype
elif [ "${datatype}" = "suvsfctype" ]; then
dstype=$uvsfctype
elif [ "${datatype}" = "suvsondtype" ]; then
dstype=$uvsondtype
fi

for type in $dstype
do
rm -f ${type}_stationlist
cp $CTL_DIR/${type}_stationlist ${type}_stationlist
done
done

for type in $sondtype 
do
file=${type}_stationlist  
 if [ -s ${type}_stationlist ];then
$CREATE_MARKERS_MULTILEVEL $file
 for level in 5mb 30mb 125mb 225mb 325mb 425mb 525mb 625mb 725mb 825mb 925mb
      do
      mv ${type}_${level}.php ${type}_${level}.js
 done
   sleep 2
fi
done
echo 'finish php files for multilevel CREATE_MARKERS_MULTILEVEL' 
sleep 10

#make php files for surface
for datatype in ssfctype suvsfctype 
do
if [ "${datatype}" = "ssfctype" ]; then
dstype=$sfctype
elif [ "${datatype}" = "suvsfctype" ]; then
dstype=$uvsfctype
fi

#for type in ps120 ps180 ps181 ps187 q180 q181 q187 t180 t181 t187 uv280 uv281 uv287
for type in $dstype
do
   file=${type}_stationlist
 if [ -s ${type}_stationlist ];then
   $CREATE_MARKERS_SURFACE $file
   mv ${type}.php ${type}.js
   sleep 2
 fi
done
done

echo 'finish php files for CREATE_MARKERS_SURFACE' 
sleep 10

#make php files for wind
#for type in uv220 uv221 uv223 uv224 uv228 uv229
for type in $uvsondtype 
do
   file=${type}_stationlist
 if [ -s ${type}_stationlist ];then
   $CREATE_MARKERS_MULTILEVEL_WIND $file
   for level in 5mb 30mb 125mb 225mb 325mb 425mb 525mb 625mb 725mb 825mb 925mb
      do
      mv ${type}_${level}.php ${type}_${level}.js
   done
   sleep 2
 fi
done

echo 'finish php files for CREATE_MARKERS_MULTILEVEL_WIND' 
##make php files for Vertical, including soundings and wind

##make php files for soundings
for type in $sondtype 
do
   file=${type}_stationlist
 if [ -s ${type}_stationlist ];then
   $CREATE_MARKERS_VERT_SOUNDINGS $file
   mv ${type}.php ${type}.js       
   sleep 2
 fi
done

echo 'finish php files for CREATE_MARKERS_VERT_SOUNDINGS' 
sleep 10

##make php files for wind
#for type in uv220 uv221 uv223 uv224 uv228 uv229 
for type in $uvsondtype 
do
   file=${type}_stationlist
 if [ -s ${type}_stationlist ];then
   $CREATE_MARKERS_VERT_WIND $file
   mv ${type}.php ${type}.js       
   sleep 2
 fi
done

echo 'finish php files for CREATE_MARKERS_VERT_WIND' 
###Now create the html files
for type in $sondtype 
do
  for level in 5 30 125 225 325 425 525 625 725 825 925
  do
    $CREATE_HTML $type $level
  done

done

sleep 2
for type in $uvsondtype 
do
  for level in 5 30 125 225 325 425 525 625 725 825 925
  do
    $CREATE_HTML_WIND $type $level
  done

done
sleep 2

for datatype in ssfctype suvsfctype
do
if [ "${datatype}" = "ssfctype" ]; then
dstype=$sfctype
elif [ "${datatype}" = "suvsfctype" ]; then
dstype=$uvsfctype
fi


for type in $dstype 
do
  $CREATE_HTML_SFC $type
done
done

sleep 2

for datatype in ssondtype suvsondtype
do
if [ "${datatype}" = "ssondtype" ]; then
dstype=$sondtype
elif [ "${datatype}" = "suvsondtype" ]; then
dstype=$uvsondtype
fi


for type in $dstype 
do
 $CREATE_HTML_SOND $type
done
done


sleep 2


echo 'finish html files for TIME'  

###Now move the files to CWD
ssh -l $WSUSER $WS "mkdir -p ${googlemap_time}"
ssh -l $WSUSER $WS "mkdir -p ${googlemap_vert}"
ssh -l $WSUSER $WS "rm -f ${googlemap_vert}/*"
ssh -l $WSUSER $WS "rm -f ${googlemap_time}/*"

for datatype in ssfctype suvsfctype 
do
if [ "${datatype}" = "ssfctype" ]; then
dstype=$sfctype
elif [ "${datatype}" = "suvsfctype" ]; then
dstype=$uvsfctype
fi

for stype in $dstype 
do
scp ${stype}.html $WSUSER@$WS:${googlemap_time}
scp ${stype}.js $WSUSER@$WS:${googlemap_time}

done
done


for datatype in ssondtype  suvsondtype
do
if [ "${datatype}" = "ssondtype" ]; then
dstype=$sondtype
elif [ "${datatype}" = "suvsondtype" ]; then
dstype=$uvsondtype
fi

for type in $dstype 
do
scp ${type}_*.html $WSUSER@$WS:${googlemap_time}
scp ${type}_*.js $WSUSER@$WS:${googlemap_time}

scp ${type}.html  $WSUSER@$WS:${googlemap_vert}
scp ${type}.js   $WSUSER@$WS:${googlemap_vert}
done
done
#
scp -r /u/Xiujuan.Su/home/gsiqc3/css $WSUSER@$WS:${googlemap_vert}
scp -r /u/Xiujuan.Su/home/gsiqc3/markers $WSUSER@$WS:${googlemap_vert}
scp -r /u/Xiujuan.Su/home/gsiqc3/css $WSUSER@$WS:${googlemap_time}
scp -r /u/Xiujuan.Su/home/gsiqc3/markers $WSUSER@$WS:${googlemap_time}

exit
