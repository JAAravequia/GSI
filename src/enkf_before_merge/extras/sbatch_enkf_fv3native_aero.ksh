#!/bin/ksh
#
# -- Request cores
#SBATCH -n 10
#
# -- Specify a maximum wallclock of 4 hours
#SBATCH -t 00:10:00
#
# -- Specify under which account a job should run
#SBATCH -q debug
#
# -- Set the name of the job, or moab will default to STDIN
#SBATCH -A chem-var

#SBATCH -J enkf_aero
#SBATCH -D /scratch3/BMC/fim/MAPP_2018/jedi/enkf/scripts
#SBATCH -o /scratch3/BMC/fim/MAPP_2018/jedi/enkf/qslogs/%x.o%j
#SBATCH -e /scratch3/BMC/fim/MAPP_2018/jedi/enkf/qslogs/%x.e%j

#sbatch --export=ALL,ident=2018041418 sbatch_enkf_fv3native_aero.ksh

ident=2018041418
nanals=10
nlevs=64
cycle_frequency=6


MAINDIR=/scratch3/BMC/fim/MAPP_2018/jedi/enkf

. ${MAINDIR}/scripts/enkf_modules.sh

set -x

analdate=$ident

export enkf_threads=1
export cores_enkf=$nanals
export mpitaskspernode=1
export OMP_NUM_THREADS=$enkf_threads
export OMP_STACKSIZE=256M
export nprocs=$PBS_NP

ndate=~/bin/ndate

analdatep=`$ndate +$cycle_frequency $analdate`

yyyymmdd=`echo $analdatep | cut -c1-8`
hh=`echo $analdatep | cut -c9-10`

workdir=${MAINDIR}/tmpdir/enkf_run

/bin/rm -rf $workdir
mkdir -p $workdir

. ./enkf_nml.sh

/bin/cp enkf_fv3.x $workdir

cd $workdir

cat <<EOF > enkf.nml
&nam_enkf
datestring="$datestring",
datapath="$datapath",
obspath="$obspath",
fgfileprefixes="$fgfileprefixes",
anlfileprefixes="$anlfileprefixes",
analpertwtnh=$analpertwtnh,
analpertwtsh=$analpertwtsh,
analpertwttr=$analpertwttr,
lupd_satbiasc=$lupd_satbiasc,
zhuberleft=$zhuberleft,
zhuberright=$zhuberright,
huber=$huber,
varqc=$varqc,
covinflatemax=$covinflatemax,
covinflatemin=$covinflatemin,
pseudo_rh=$pseudo_rh,
corrlengthnh=$corrlengthnh,
corrlengthsh=$corrlengthsh,
corrlengthtr=$corrlengthtr,
obtimelnh=$obtimelnh,
obtimelsh=$obtimelsh,
obtimeltr=$obtimeltr,
iassim_order=$iassim_order,
lnsigcutoffnh=$lnsigcutoffnh,
lnsigcutoffsh=$lnsigcutoffsh,
lnsigcutofftr=$lnsigcutofftr,
lnsigcutoffsatnh=$lnsigcutoffsatnh,
lnsigcutoffsatsh=$lnsigcutoffsatsh,
lnsigcutoffsattr=$lnsigcutoffsattr,
lnsigcutoffpsnh=$lnsigcutoffpsnh,
lnsigcutoffpssh=$lnsigcutoffpssh,
lnsigcutoffpstr=$lnsigcutoffpstr,
simple_partition=$simple_partition,
nlons=$nlons,
nlats=$nlats,
smoothparm=$smoothparm,
readin_localization=$readin_localization,
saterrfact=$saterrfact,
numiter=$numiter,
sprd_tol=$sprd_tol,
paoverpb_thresh=$paoverpb_thresh,
letkf_flag=$letkf_flag,
use_qsatensmean=$use_qsatensmean,
npefiles=$npefiles,
lobsdiag_forenkf=$lobsdiag_forenkf,
netcdf_diag=$netcdf_diag,
reducedgrid=$reducedgrid,
nlevs=$nlevs,
nanals=$nanals,
deterministic=$deterministic,
write_spread_diag=.true.,
sortinc=$sortinc,
univaroz=$univaroz,
univartracers=$univartracers,
massbal_adjust=$massbal_adjust,
nhr_anal=$nhr_anal,
nhr_state=$nhr_state,
use_gfs_nemsio=$use_gfs_nemsio,
adp_anglebc=$adp_anglebc,
angord=$angord,
newpc4pred=$newpc4pred,
use_edges=$use_edges,
emiss_bc=$emiss_bc,
biasvar=$biasvar,
write_spread_diag=$write_spread_diag
fv3_native=$fv3_native
/
&END
&nam_wrf
/
&END
&nam_fv3
fv3fixpath="$fv3fixpath",
nx_res=$nx_res,
ny_res=$nx_res,
ntiles=6,
l_pres_add_saved=$l_pres_add_saved
/
&END
&satobs_enkf
/
&END
&ozobs_enkf
/
&END
&aodobs_enkf
sattypes_aod(1)=${sattypes_aod}
/
&END
EOF

cat enkf.nml

LOG_DIR=${MAINDIR}/logs

if [[ ! -r $LOG_DIR ]]
then
    mkdir -p $LOG_DIR
fi

/bin/cp ${MAINDIR}/fix/aeroinfo_aod.txt ./aeroinfo 
/bin/cp ${MAINDIR}/fix/anavinfo_fv3_gocart_enkf ./anavinfo

nanal=1
while [[ $nanal -le $nanals ]]
do
    charnanal="mem"`printf %03i $nanal`
    itile=1
    while [[ $itile -le 6 ]]
    do
	/bin/cp ${MAINDIR}/${analdate}/${charnanal}/${yyyymmdd}.${hh}0000.fv_tracer.res.tile${itile}.nc ${MAINDIR}/${analdate}/${charnanal}/${yyyymmdd}.${hh}0000.anal.fv_tracer.res.tile${itile}.nc
	((itile=itile+1))
    done
    ((nanal=nanal+1))
done

echo "srun -n $nanals enkf_fv3.x > ${LOG_DIR}/enkf.out 2>&1"
srun -n $nanals enkf_fv3.x > ${LOG_DIR}/enkf.out 2>&1

exit

OUTDIR=${FV3_RUNS}/${sim}/${analdate}

grep 'all done' ${LOG_DATE_DIR}/enkf.out

if [[ $? -eq 0 ]]
then
    EXEC=${MY_EXECDIR}/calc_increment_nemsio.x
    nanal=1
    while [[ $nanal -le $nanals ]]
    do
	charnanal="mem"`printf %03i $nanal`
	outdir=${OUTDIR}/${charnanal}/OUTPUT_ENKF
	if [[ ! -r $outdir ]] then
	    mkdir -p $outdir
	fi

	ln -sf sfg_${analdate}_fhr06_${charnanal} file_fcst 
	ln -sf sanl_${analdate}_${charnanal} file_anal
	
	file_increment=increment_${analdate}_${charnanal}.nc
	/bin/cp ${FV3_FIXDIR}/C${RES}/fv3_increment_zero.nc $file_increment
	${EXEC} file_fcst file_anal $file_increment

	if [[ ! -r sanl_${analdate}_$charnanal || ! -r $file_increment ]]
	then
	    echo "EnKF analysis and/or increment missing - Stopping"
	    exit 1
	fi

	/bin/mv sanl_${analdate}_$charnanal $file_increment $outdir

#	ncpdq -O -a '-lev' $file_increment ${file_increment}_lev_reversed
#	/bin/mv sanl_${analdate}_$charnanal $file_increment ${file_increment}_lev_reversed $outdir

	((nanal=nanal+1))
    done
else
    echo 'ENKF failed'
    exit 1
fi

cd $SCRIPTDIR_DRIVER

nanal=1
while [[ $nanal -le $nanals ]]
do
    echo "qsub -v ident=$ident,member=$nanal qsub_fv3_gfs_c192_warm_da_ensemble.sh"
    qsub -v ident=$ident,member=$nanal qsub_fv3_gfs_c192_warm_da_ensemble.sh
    ((nanal=nanal+1))
done


cd $SCRIPTDIR_UTIL
echo "qsub -v ident=$analdatem qsub_calc_aod_ensemble.sh"
#qsub -v ident=$analdatem qsub_calc_aod_ensemble.sh

exit 0

echo "$analdate starting ens mean analysis computation `date`"
csh ${enkfscripts}/compute_ensmean_enkf.csh >&!  ${current_logdir}/compute_ensmeans_enkf.out
echo "$analdate done computing ensemble mean analyses `date`"

# check output files again.
nanal=1
set filemissing='no'
while ($nanal <= $nanals)
   set charnanal="mem"`printf %03i $nanal`
   if ($IAU == ".true.") then
      set analfile="${datapath2}/sanl_${analdate}_${charfhr}_${charnanal}"
   else
      set analfile="${datapath2}/sanl_${analdate}_${charnanal}"
   endif
   if ( ! -s $analfile) set filemissing='yes'
   @ nanal = $nanal + 1
end
if ( $satbiasc == ".true." &&  ! -s $ABIAS) set filemissing='yes'

if ($filemissing == 'yes') then
    echo "there are output files missing!"
    exit 1
else
    echo "all output files seem OK `date`"
endif

exit 0
