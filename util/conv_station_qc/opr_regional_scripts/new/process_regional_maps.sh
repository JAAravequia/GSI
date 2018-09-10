#!/bin/bash
set -xa
CTL_DIR=$1
WSHOME=$2
WSUSER=$3
WS=$4

##execs for creating php files (Time)
#CREATE_MARKERS_MULTILEVEL=/gpfs/t3/global/save/wx23yl/su_google_maps/google_v3/global/Time/tools/create_markers_multilevel
#CREATE_MARKERS_MULTILEVEL_WIND=/gpfs/t3/global/save/wx23yl/su_google_maps/google_v3/global/Time/tools/create_markers_multilevel_wind
#CREATE_MARKERS_SURFACE=/gpfs/t3/global/save/wx23yl/su_google_maps/google_v3/global/Time/tools/create_markers_surface
CREATE_MARKERS_SURFACE=/u/Xiujuan.Su/home/gsiqc3/google_map/code_surface/create_markers_surface
CREATE_MARKERS_MULTILEVEL=/u/Xiujuan.Su/home/gsiqc3/google_map/code_multilevel/create_markers_multilevel
CREATE_MARKERS_MULTILEVEL_WIND=/u/Xiujuan.Su/home/gsiqc3/google_map/code_multilevel_wind/create_markers_multilevel_wind

##execs for creating php files (Vertical)
#CREATE_MARKERS_VERT_SOUNDINGS=/gpfs/t3/global/save/wx23yl/su_google_maps/google_v3/global/Vertical/tools/create_markers_vert_soundings
#CREATE_MARKERS_VERT_WIND=/gpfs/t3/global/save/wx23yl/su_google_maps/google_v3/global/Vertical/tools/create_markers_vert_wind

CREATE_MARKERS_VERT_SOUNDINGS=/u/Xiujuan.Su/home/gsiqc3/google_map/code_vert_soundings/create_markers_vert_soundings
CREATE_MARKERS_VERT_WIND=/u/Xiujuan.Su/home/gsiqc3/google_map/code_vert_wind/create_markers_vert_wind
### create the html files

CREATE_HTML=/gpfs/t3/global/save/wx23yl/su_google_maps/google_v3/global/Time/tools/create_surface_html.bash
CREATE_HTML_wind=/gpfs/t3/global/save/wx23yl/su_google_maps/google_v3/global/Time/tools/create_multilevel_html_wind.bash
CREATE_HTML_vert=/gpfs/t3/global/save/wx23yl/su_google_maps/google_v3/global/Vertical/tools/create_vertical_html.bash
CREATE_HTML_vert2=/gpfs/t3/global/save/wx23yl/su_google_maps/google_v3/global/Time/tools/create_multilevel_html.bash

for tm in 00 03 06 09 12
do

export googlemap_time=$WSHOME/web/regional/time/${tm}/googlemap_htmls
export googlemap_vert=$WSHOME/web/regional/vert/${tm}/googlemap_htmls

##working directory 
CWD=/ptmp/Xiujuan.Su/googlemap_regional_${tm}
mkdir -p ${CWD}

##directory with station list 
#CTL_DIR=/ptmp/Xiujuan.Su/make_time



cd $CWD
rm -f *

 cp $CTL_DIR/*list_${tm} ./

 for type in ps120 ps180 ps181 ps187 ps188 q120 q180 q181 q187 q188 t120 t180 t180 t181 t187 uv220 uv221 uv223 uv224 uv280 uv281 uv287 uv288
do
mv ${type}_stationlist_${tm} ${type}_stationlist

done

##make php files for Time, including surface, multilevel, and wind

##make php files for multilevel
for type in q120 t120
  do
     file=${type}_stationlist
     $CREATE_MARKERS_MULTILEVEL $file  >stdout 2>&1
     mv stdout ${type}_stdout_time
     sleep 2
  done

  sleep 10

  echo ' mske q120 and t120 php files'

 #make php files for surface
  for type in ps120 ps180 ps181 ps187 ps188 q180 t180 q181 q187 q188 t180 t181 t187 uv280 uv281 uv287 uv288 
  do
     file=${type}_stationlist
     $CREATE_MARKERS_SURFACE $file >stdout 2>&1
     mv stdout ${type}_stdout_time
     sleep 2
  done

  sleep 10

  echo ' mske surface php files'
#make php files for wind
  for type in uv220 uv221 uv223 uv224 
  do
     file=${type}_stationlist
     $CREATE_MARKERS_MULTILEVEL_WIND $file  >stdout 2>&1
     mv stdout ${type}_stdout_time
     sleep 2
  done

  echo ' mske wind php files'
##make php files for Vertical, including soundings and wind

##make php files for soundings
  for type in q120 t120
  do
     file=${type}_stationlist
     $CREATE_MARKERS_VERT_SOUNDINGS $file >stdout 2>&1
     mv stdout ${type}_stdout
     sleep 2
  done

  sleep 10

##make php files for wind
  for file in uv220 uv221 uv223 uv224  
  do
     file=${type}_stationlist
     $CREATE_MARKERS_VERT_WIND $file  >stdout 2>&1
      mv stdout ${type}_stdout
     sleep 2
  done

##Now create the html files

  for type in ps120 ps180 ps181 ps187 ps188 q180 q181 q187 q188 t180 t181 t187 t188 uv280 uv281 uv287 uv288
   do
    $CREATE_HTML $type
    done

# creat the html for the time series
   for type in uv220 uv221 uv223 uv224 
      do
      for level in 5 30 125 225 325 425 525 625 725 825 925
    do
      $CREATE_HTML_wind $type $level
    done
  done

for type in q120 t120 uv220 uv221 uv223 uv224
do
 $CREATE_HTML_vert $type
done

for type in q120 t120
do
  for level in 5 30 125 225 325 425 525 625 725 825 925
  do
    $CREATE_HTML_vert2 $type $level
  done
done


  ###Now move the files to CWD/$tmmark
#  mv *.php ${CWD}/${tmmark}/.
#  mv *.html ${CWD}/${tmmark}/.

ssh -l $WSUSER $WS "mkdir -p ${googlemap_time}"
ssh -l $WSUSER $WS "mkdir -p ${googlemap_vert}"


for sfctype in ps120 ps180 ps181 ps187 ps188 q180 q181 q187 q188 t180 t181 t187 t188 uv280 uv281 uv287 uv288
do 
scp ${sfctype}.html $WSUSER@$WS:${googlemap_time}  
scp ${sfctype}.php $WSUSER@$WS:${googlemap_time} 

done

for type in q120 t120 uv220 uv221 uv223 uv224  
do
scp ${type}_*.html $WSUSER@$WS:${googlemap_time} 
scp ${type}_*.php $WSUSER@$WS:${googlemap_time} 

scp ${type}.html  $WSUSER@$WS:${googlemap_vert}
scp ${type}.php   $WSUSER@$WS:${googlemap_vert}
done

scp -r /u/Xiujuan.Su/home/gsiqc3/css $WSUSER@$WS:${googlemap_vert} 
scp -r /u/Xiujuan.Su/home/gsiqc3/markers $WSUSER@$WS:${googlemap_vert} 
scp -r /u/Xiujuan.Su/home/gsiqc3/css $WSUSER@$WS:${googlemap_time}
scp -r /u/Xiujuan.Su/home/gsiqc3/markers $WSUSER@$WS:${googlemap_time}


done                  ##  done with tm


exit
