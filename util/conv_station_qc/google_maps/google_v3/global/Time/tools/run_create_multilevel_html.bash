#!/bin/bash

CREATE_HTML=/meso/save/Yangrong.Ling/su_google_maps/google_v3/global/Time/tools/create_multilevel_html.bash
##CREATE_HTML=/gpfs/t3/global/save/wx23yl/su_google_maps/google_v3/global/Time/tools/create_multilevel_html.bash

for type in q120 t120
do
  for level in 5 30 125 225 325 425 525 625 725 825 925
  do
    $CREATE_HTML $type $level
  done 

done
