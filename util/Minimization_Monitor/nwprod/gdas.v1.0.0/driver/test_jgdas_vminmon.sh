#!/bin/ksh

#BSUB -o gdas_vminmon.o%J
#BSUB -e gdas_vminmon.o%J
#BSUB -J gdas_vminmon
#BSUB -q dev_shared
#BSUB -n 1
#BSUB -R affinity[core]
#BSUB -M 80
#BSUB -W 00:05
#BSUB -a poe
#BSUB -P GFS-T2O

set -x

export PDATE=${PDATE:-2018011112}

#############################################################
# Specify whether the run is production or development
#############################################################
export PDY=`echo $PDATE | cut -c1-8`
export cyc=`echo $PDATE | cut -c9-10`
export job=gdas_vminmon.${cyc}
export pid=${pid:-$$}
export jobid=${job}.${pid}
export envir=para
id=`hostname | cut -c1`
export DATAROOT=${DATAROOT:-/gpfs/${id}d2/emc/da/noscrub/Edward.Safford/test_data}
export COMROOT=${COMROOT:-/ptmpp1/$LOGNAME/com}


#############################################################
# Specify versions
#############################################################
export gdas_ver=v14.1.0
export global_shared_ver=v14.1.0
export gdas_minmon_ver=v1.0.0
export minmon_shared_ver=v1.0.1


#############################################################
# Load modules
#############################################################
. /usrx/local/Modules/3.2.9/init/ksh
module use /nwprod2/modulefiles
module load prod_util
#module load util_shared

module list


#############################################################
# WCOSS environment settings
#############################################################
export POE=YES


#############################################################
# Set user specific variables
#############################################################
export MINMON_SUFFIX=${MINMON_SUFFIX:-testminmon_gdas}
export NWTEST=${NWTEST:-/da/noscrub/${LOGNAME}/ProdGSI/util/Minimization_Monitor/nwprod}
export HOMEgdas=${NWTEST}/gdas.${gdas_minmon_ver}
export JOBGLOBAL=${HOMEgdas}/jobs
export HOMEminmon=${NWTEST}/minmon_shared.${minmon_shared_ver}
export COM_IN=${COM_IN:-${DATAROOT}}
export M_TANKverf=${COMROOT}/${MINMON_SUFFIX}


#############################################################
# Execute job
#############################################################
$JOBGLOBAL/JGDAS_VMINMON

exit

