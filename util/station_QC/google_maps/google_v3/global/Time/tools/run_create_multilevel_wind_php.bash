#!/bin/bash

CREATE_MARKERS=/meso/save/Yangrong.Ling/su_google_maps/google_v3/global/Time/tools/create_markers_multilevel_wind
##CREATE_MARKERS=/gpfs/t3/global/save/wx23yl/su_google_maps/google_v3/global/Time/tools/create_markers_multilevel_wind

for file in *_ed.txt 
do
  $CREATE_MARKERS $file
  sleep 2
done
