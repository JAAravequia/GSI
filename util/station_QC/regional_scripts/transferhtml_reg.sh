### this scripts will transfer web html files for display

set -uxa

################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:     transferhtml_reg.sh 
# Script description: transfer web html files for regional web display 
#
# Author:        Xiujuan Su       Org: NP20         Date: 2010-09-15
#
# Script history log:
# 2018-11-05  X. Su  Removed unnecessary lines and documented the script
#
# usage transferhtml_reg.sh htmldir WSHOME WSUSER WS dtime mapping
#
# htmldir: the directory has all html files for web site.
# WSHOME: the web site directory for monitor web site
# WSUSER: web site user name
# WS: the machine in which the monitor web site locates
# dtime: the period of data processed
# mappng: horizontal region map

htmldir=$1
WSHOME=$2
WSUSER=$3
WS=$4
dtime=$5


scp ${htmldir}/*html $WSUSER@$WS:${WSHOME}/web
scp ${htmldir}/regional/*html $WSUSER@$WS:${WSHOME}/web/regional



exit









