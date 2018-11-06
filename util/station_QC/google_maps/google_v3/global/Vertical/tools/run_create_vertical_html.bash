#!/bin/bash

CREATE_HTML=/meso/save/Yangrong.Ling/su_google_maps/google_v3/global/Vertical/tools/create_vertical_html.bash

for type in q120 t120 uv220 uv221 uv223 uv224 uv229
do
 $CREATE_HTML $type
done
