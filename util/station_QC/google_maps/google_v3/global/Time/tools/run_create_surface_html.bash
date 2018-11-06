#!/bin/bash

CREATE_HTML=/meso/save/Yangrong.Ling/su_google_maps/google_v3/global/Time/tools/create_surface_html.bash
##CREATE_HTML=/gpfs/t3/global/save/wx23yl/su_google_maps/google_v3/global/Time/tools/create_surface_html.bash
#uv280, uv281, and uv287 are surface winds
for type in ps120 ps180 ps181 ps187 q180 q181 q187 t180 t181 t187 uv280 uv281 uv287
do
  $CREATE_HTML $type
done
