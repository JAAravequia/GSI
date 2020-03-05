#!/bin/sh

set -ax
package=ProdGSI/util/Radiance_Monitor

scripts=/gpfs/dell2/emc/modeling/noscrub/Edward.Safford/${package}/image_gen/ush

export DO_DATA_RPT=1
export DO_DIAG_RPT=1
export NDATE=/gpfs/dell1/nco/ops/nwprod/prod_util.v1.1.2/exec/ips/ndate
export DO_ARCHIVE=0
export JOB_QUEUE=dev_shared
export NUM_CYCLES=360
export MAIL_CC="russ.treadon@noaa.gov, john.derber@noaa.gov, andrew.collard@noaa.gov"
#export MAIL_CC="edward.c.safford@gmail.com"

export REGIONAL_RR=0
export CYCLE_INTERVAL=6
export TANK_USE_RUN=1
export RUN_TRANSFER=1

export RADMON_SUFFIX=GFS
export RUN=gdas

data_map=${scripts}/data_map.xml

tankdir=/u/Edward.Safford/nbns/stats/${RADMON_SUFFIX}

imgdate=`${scripts}/query_data_map.pl ${data_map} ${RADMON_SUFFIX}_${RUN} imgdate`

idate=`$NDATE +${CYCLE_INTERVAL} $imgdate`

prodate=`${scripts}/nu_find_cycle.pl --dir ${tankdir} --cyc 1 --run ${RUN}`
echo "imgdate, prodate = $imgdate, $prodate"

logs=/gpfs/dell2/ptmp/Edward.Safford/logs/${RADMON_SUFFIX}/${RUN}/radmon

if [[ $idate -le $prodate ]]; then

   echo " firing CkPlt_glbl.sh with ${RADMON_SUFFIX} $idate"
   ${scripts}/CkPlt_glbl.sh ${RADMON_SUFFIX} $idate 1>${logs}/CkPlt_${RADMON_SUFFIX}_${RUN}.log 2>${logs}/CkPlt_${RADMON_SUFFIX}_${RUN}.err

   rc=`${scripts}/update_data_map.pl ${data_map} ${RADMON_SUFFIX}_${RUN} imgdate ${idate}`

fi


exit
