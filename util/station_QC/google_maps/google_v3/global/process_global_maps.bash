#!/bin/bash

##working directory 
CWD=/meso/save/Yangrong.Ling/su_google_maps/google_v3/global/htmls

##directory with the control files
CTL_DIR=/meso/save/Yangrong.Ling/su_google_maps/google_v3/global/ctl_files_4cats

##execs for creating php files (Time)
CREATE_MARKERS_MULTILEVEL=/meso/save/Yangrong.Ling/su_google_maps/google_v3/global/Time/tools/create_markers_multilevel
CREATE_MARKERS_MULTILEVEL_WIND=/meso/save/Yangrong.Ling/su_google_maps/google_v3/global/Time/tools/create_markers_multilevel_wind
CREATE_MARKERS_SURFACE=/meso/save/Yangrong.Ling/su_google_maps/google_v3/global/Time/tools/create_markers_surface

##execs for creating php files (Vertical)
CREATE_MARKERS_VERT_SOUNDINGS=/meso/save/Yangrong.Ling/su_google_maps/google_v3/global/Vertical/tools/create_markers_vert_soundings
CREATE_MARKERS_VERT_WIND=/meso/save/Yangrong.Ling/su_google_maps/google_v3/global/Vertical/tools/create_markers_vert_wind

cd $CTL_DIR

##make php files for Time, including surface, multilevel, and wind

##make php files for multilevel
for file in q120_stationlist t120_stationlist
do
   $CREATE_MARKERS_MULTILEVEL $file
   sleep 2
done

sleep 10

#make php files for surface uv280, uv281 uv287 are surface winds
for file in ps120_stationlist ps180_stationlist ps181_stationlist ps187_stationlist \
            q180_stationlist q181_stationlist q187_stationlist t180_stationlist \
            t181_stationlist t187_stationlist uv280_stationlist uv281_stationlist uv287_stationlist
do
   $CREATE_MARKERS_SURFACE $file
   sleep 2
done

sleep 10

#make php files for wind
for file in uv220_stationlist uv221_stationlist uv223_stationlist uv224_stationlist \
            uv229_stationlist
do
   $CREATE_MARKERS_MULTILEVEL_WIND $file
   sleep 2
done

##make php files for Vertical, including soundings and wind

##make php files for soundings
for file in q120_stationlist t120_stationlist
do
   $CREATE_MARKERS_VERT_SOUNDINGS $file
   sleep 2
done

sleep 10

##make php files for wind
for file in uv220_stationlist uv221_stationlist uv223_stationlist uv224_stationlist \
            uv229_stationlist
do
   $CREATE_MARKERS_VERT_WIND $file
   sleep 2
done

###Now create the html files
sh /meso/save/Yangrong.Ling/su_google_maps/google_v3/global/Time/tools/run_create_multilevel_html.bash
sleep 2
sh /meso/save/Yangrong.Ling/su_google_maps/google_v3/global/Time/tools/run_create_multilevel_wind_html.bash
sleep 2
sh /meso/save/Yangrong.Ling/su_google_maps/google_v3/global/Time/tools/run_create_surface_html.bash
sleep 2
sh /meso/save/Yangrong.Ling/su_google_maps/google_v3/global/Vertical/tools/run_create_vertical_html.bash
sleep 2

###Now move the files to CWD
mv *.php $CWD
mv *.html $CWD
