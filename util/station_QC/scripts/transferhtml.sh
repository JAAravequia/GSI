#!/bin/sh
set -xa
################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:     transferhtml.sh 
# Script description: transfer web html files for web display 
#
# Author:        Xiujuan Su       Org: NP20         Date: 2010-09-15
#
# Script history log:
# 2018-09-20  X. Su  Removed unnecessary lines and documented the script
#
# usage transferhtml.sh htmldir WSHOME WSUSER WS dtime mapping
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
mapping=$6


scp ${htmldir}/*html $WSUSER@$WS:${WSHOME}/web
scp ${htmldir}/global/*html $WSUSER@$WS:${WSHOME}/web/global
scp -r ${mapping} $WSUSER@$WS:${WSHOME}/web/global/horz



exit









