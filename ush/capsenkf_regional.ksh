#!/bin/ksh
# #SBATCH -N 100         #CONUS requirement
# #SBATCH -n 700         #CONUS requirement
#SBATCH -N 4
#SBATCH -n 160
#SBATCH -o gsienkf.log
#SBATCH -e gsienkf.log
#SBATCH -t 02:00:00
#SBATCH -J capsenkf
#SBATCH -p skx-dev
# #SBATCH -p skx-normal
#SBATCH -A TG-ATM160014 

#####################################################
# machine set up (users should change this part)
#####################################################
#
set OMP_NUM_THREADS = 2
set -x

#
# GSIPROC = processor number used for GSI analysis
#------------------------------------------------
  GSIPROC=60
  ARCH='LINUX_STAMPEDE2'

# Supported configurations:
            # IBM_LSF,
            # LINUX, LINUX_LSF, LINUX_PBS,
            # DARWIN_PGI
#
#####################################################
# case set up (users should change this part)
#####################################################
#
# ANAL_TIME= analysis time  (YYYYMMDDHH)
# WORK_ROOT= working directory, where GSI runs
# PREPBURF = path of PreBUFR conventional obs
# OBS_ROOT = path of observations files
# FIX_ROOT = path of fix files
# ENKF_EXE  = path and name of the EnKF executable 
  ANAL_TIME=201904302000  #used by comenkf_namelist.sh
  N_CYC=5
  JOB_DIR=/scratch/01540/ctong/BOKFV3
     #normally you put run scripts here and submit jobs form here, require a copy of enkf_wrf.x at this directory
  RUN_NAME=sarfv3_EnKF
  OBS_ROOT=/scratch/01540/ctong/GSIobs/BOK
  BK_ROOT=${JOB_DIR}/cycle${N_CYC}/GENBE
  GSI_ROOT=/home1/01540/ctong/ProdGSI
  diag_ROOT=${JOB_DIR}/cycle${N_CYC}/sarfv3_GSI
  ENKF_EXE=/home1/01540/ctong/build/bin/enkf_wrf.x
  WORK_ROOT=${JOB_DIR}/cycle${N_CYC}/${RUN_NAME}
  FIX_ROOT=${GSI_ROOT}/fix
  ENKF_NAMELIST=${GSI_ROOT}/ush/capsenkf_namelist.sh

# ensemble parameters
#
  NMEM_ENKF=40
  BK_dynv_mem=${BK_ROOT}/dynvars
  BK_trac_mem=${BK_ROOT}/tracer
  NLONS=450
  NLATS=450
  NLEVS=63
  NVARS=11
  IF_ARW=.false.
  IF_NMM=.false.
  IF_FV3=.true.
  list="conv"
#  list="conv amsua_n18 mhs_n18 hirs4_n19"
#
#####################################################
# Users should NOT change script after this point
#####################################################
#

case $ARCH in
   'IBM_LSF')
      ###### IBM LSF (Load Sharing Facility)
      RUN_COMMAND="mpirun.lsf " ;;

   'LINUX')
      if [ $GSIPROC = 1 ]; then
         #### Linux workstation - single processor
         RUN_COMMAND=""
      else
         ###### Linux workstation -  mpi run
        RUN_COMMAND="mpirun -np ${GSIPROC} "
      fi ;;

   'LINUX_STAMPEDE2')
      if [ $GSIPROC = 1 ]; then
         #### Linux workstation - single processor
         RUN_COMMAND=""
      else
         ###### Linux workstation -  mpi run
        # RUN_COMMAND="mpirun -np ${GSIPROC} "
        RUN_COMMAND="ibrun "
      fi ;;

   'LINUX_LSF')
      ###### LINUX LSF (Load Sharing Facility)
      RUN_COMMAND="mpirun.lsf " ;;

   'LINUX_PBS')
      #### Linux cluster PBS (Portable Batch System)
      RUN_COMMAND="mpirun -np ${GSIPROC} " ;;

   'DARWIN_PGI')
      ### Mac - mpi run
      if [ $GSIPROC = 1 ]; then
         #### Mac workstation - single processor
         RUN_COMMAND=""
      else
         ###### Mac workstation -  mpi run
         RUN_COMMAND="mpirun -np ${GSIPROC} -machinefile ~/mach "
      fi ;;

   * )
     print "error: $ARCH is not a supported platform configuration."
     exit 1 ;;
esac

# Given the analysis date, compute the date from which the
# first guess comes.  Extract cycle and set prefix and suffix
# for guess and observation data files
# gdate=`$ndate -06 $adate`
#gdate=$ANAL_TIME
#YYYYMMDD=`echo $adate | cut -c1-8`
#HH=`echo $adate | cut -c9-10`

# Fixed files
# CONVINFO=${FIX_ROOT}/global_convinfo.txt
# SATINFO=${FIX_ROOT}/global_satinfo.txt
# SCANINFO=${FIX_ROOT}/global_scaninfo.txt
# OZINFO=${FIX_ROOT}/global_ozinfo.txt
ANAVINFO=${diag_ROOT}/anavinfo
CONVINFO=${diag_ROOT}/convinfo
SATINFO=${diag_ROOT}/satinfo
SCANINFO=${diag_ROOT}/scaninfo
OZINFO=${diag_ROOT}/ozinfo
# LOCINFO=${FIX_ROOT}/global_hybens_locinfo.l64.txt

# Set up workdir
rm -rf $WORK_ROOT
mkdir -p $WORK_ROOT
cd $WORK_ROOT

cp $ENKF_EXE enkf.x

#cp $ANAVINFO        ./anavinfo
#cp $CONVINFO        ./convinfo
#cp $SATINFO         ./satinfo
#cp $SCANINFO        ./scaninfo
#cp $OZINFO          ./ozinfo
#cp $LOCINFO         ./hybens_locinfo

cp $diag_ROOT/satbias_in ./satbias_in
cp $diag_ROOT/satbias_pc ./satbias_pc

# link/copy required grid spec files if running sar-FV3
if [ "$IF_FV3" = .true. ]; then
  cp ${diag_ROOT}/fv3_akbk .
  ln -s ${diag_ROOT}/fv3_grid_spec   fv3_grid_spec
  ln -s ${diag_ROOT}/coupler.res     coupler.res
fi 

# get mean
ln -s ${BK_dynv_mem}.mean ./firstguess.dynv.ensmean
ln -s ${BK_trac_mem}.mean ./firstguess.trac.ensmean
for type in $list; do
   ln -s $diag_ROOT/diag_${type}_ges.ensmean diag_${type}_ges.ensmean
done
                                                            
# get each member
imem=1
while [[ $imem -le $NMEM_ENKF ]]; do
   member=`printf %03i $imem`
   cp ${diag_ROOT}/anavinfo ./anavinfo
   cp ${diag_ROOT}/convinfo ./convinfo
   ln -s ${BK_dynv_mem}.e${member} ./firstguess.dynv.mem${member}
   ln -s ${BK_trac_mem}.e${member} ./firstguess.trac.mem${member}
   for type in $list; do
      ln -s $diag_ROOT/diag_${type}_ges.mem${member} diag_${type}_ges.mem${member}
   done
   (( imem = $imem + 1 ))
done

# Build the GSI namelist on-the-fly
. $ENKF_NAMELIST

# make analysis files
cp firstguess.dynv.ensmean analysis.dynv.ensmean
cp firstguess.trac.ensmean analysis.trac.ensmean
# get each member
imem=1
while [[ $imem -le $NMEM_ENKF ]]; do
   member="mem"`printf %03i $imem`
   cp firstguess.dynv.${member} analysis.dynv.${member}
   cp firstguess.trac.${member} analysis.trac.${member}
   (( imem = $imem + 1 ))
done

#
###################################################
#  run  EnKF
###################################################
echo ' Run EnKF'

${RUN_COMMAND} ./enkf.x < enkf.nml > stdout 2>&1

##################################################################
#  run time error check
##################################################################
error=$?

if [ ${error} -ne 0 ]; then
  echo "ERROR: ${ENKF_EXE} crashed  Exit status=${error}"
  exit ${error}
fi

exit
