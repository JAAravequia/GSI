#!/bin/sh
set -ax

source_dir=/gpfs/dell2/emc/modeling/noscrub/emc.glopara/monitor/radmon/stats/v16rt2

export ACCOUNT=dev
export USE_ANL=1
export DO_DIAG_RPT=1
export DO_DATA_RPT=1
export JOB_QUEUE=dev_shared
MY_MACHINE=wcoss_d

shell=sh
source /usrx/local/prod/lmod/lmod/init/${shell}

export MODULEPATH=/usrx/local/prod/lmod/lmod/modulefiles/Core:/usrx/local/prod/modulefiles/core_third:/usrx/local/prod/modulefiles/defs:/gpfs/dell1/nco/ops/nwprod/modulefiles/core_prod:/usrx/local/dev/modulefiles
module load ips/18.0.1.163
module load metplus/2.1
module load lsf/10.1


package=ProdGSI/util/Radiance_Monitor
#package=RadMon
export TANK_USE_RUN=1

scripts=/gpfs/dell2/emc/modeling/noscrub/Edward.Safford/${package}/data_extract/ush


#--------------------------------------------------------------------
export RADMON_SUFFIX=v16rt2
export RUN=gdas
export RAD_AREA=glb

TANKverf=/u/Edward.Safford/nbns/stats
NDATE=/gpfs/dell1/nco/ops/nwprod/prod_util.v1.1.2/exec/ips/ndate

ldate=`${scripts}/nu_find_cycle.pl --run $RUN --cyc 1 --dir ${TANKverf}/${RADMON_SUFFIX}`
echo "last cycle processed is $ldate"

ldate_len=`echo ${#ldate}`
if [[ ${ldate_len} -ne 10 ]]; then
   exit 1
fi
START_DATE=`${NDATE} +06 $ldate`
#START_DATE=2020032800

day=`echo $START_DATE | cut -c1-8` 
cyc=`echo $START_DATE | cut -c 9-10`

export RADSTAT_DIR=/gpfs/dell3/ptmp/emc.glopara/ROTDIRS/v16rt2/${RUN}.${day}/${cyc}
export DATDIR=${source_dir}/gdas.${day}

logs=/gpfs/dell2/ptmp/Edward.Safford/logs/${RADMON_SUFFIX}/${RUN}/radmon
logfile=${logs}/cp.${day}.${cyc}.log
if [[ -e ${logfile} ]]; then
   rm -f ${logfile}
fi

PROJECT=GDAS-T2O
SUB=/gpfs/lsf/10.1/linux3.10-glibc2.17-x86_64/bin/bsub
copy_job="${scripts}/Copy_glbl.sh ${RADMON_SUFFIX} ${START_DATE}"
jobname="${RADMON_SUFFIX}_${RUN}.CP"

echo ldate, START_DATE = $ldate, $START_DATE

if [[ $MY_MACHINE = "wcoss_d" ]]; then
   $SUB -q $JOB_QUEUE -P $PROJECT -o ${logfile} \
        -M 100 -R affinity[core] -W 0:20 -cwd ${PWD} -J ${jobname} ${copy_job}

fi


exit
