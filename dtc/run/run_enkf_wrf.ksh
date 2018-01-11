#!/bin/ksh
#####################################################
# machine set up (users should change this part)
#####################################################
#

set -x

#
# GSIPROC = processor number used for GSI analysis
#------------------------------------------------
  GSIPROC=32
  ARCH='LINUX_LSF'

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
# BK_FILE  = path and name of background file
# OBS_ROOT = path of observations files
# FIX_ROOT = path of fix files
# GSI_EXE  = path and name of the gsi executable 

  ANAL_TIME=2012102506
  WORK_ROOT=enkf/regional/enkf_arw
  diag_ROOT=enkf/regional/gsidiag_arw
  BK_ROOT=enkf/enkfdata/arw/bk
  BK_FILE=${BK_ROOT}/wrfarw.ensmean
  GSI_ROOT=/enkf/code/comGSIv3.4_EnKFv1.0
  FIX_ROOT=${GSI_ROOT}/fix
  ENKF_EXE=${GSI_ROOT}/src/main/enkf/wrf_enkf
  CRTM_ROOT=CRTM_REL-2.2.3
  ENKF_NAMELIST=${GSI_ROOT}/run/enkf_wrf_namelist.sh

# ensemble parameters
#
  NMEM_ENKF=20
  BK_FILE_mem=${BK_ROOT}/wrfarw
  NLONS=111
  NLATS=111
  NLEVS=56
  IF_ARW=.true.
  IF_NMM=.false.
  list="conv amsua_n18 hirs4_n19"
#  list="conv amsua_n18 mhs_n19 hirs4_n19"
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
        RUN_COMMAND="mpirun -np ${GSIPROC} -machinefile ~/mach "
      fi ;;

   'LINUX_LSF')
      ###### LINUX LSF (Load Sharing Facility)
      RUN_COMMAND="mpirun.lsf " ;;

   'LINUX_PBS')
      #### Linux cluster PBS (Portable Batch System)
#      RUN_COMMAND="mpirun -np ${GSIPROC} " ;;
      RUN_COMMAND="mpiexec_mpt -n ${GSIPROC} " ;;

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
gdate=$ANAL_TIME
YYYYMMDD=`echo $adate | cut -c1-8`
HH=`echo $adate | cut -c9-10`

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

cp $ENKF_EXE        ./enkf.x


cp $ANAVINFO        ./anavinfo
cp $CONVINFO        ./convinfo
cp $SATINFO         ./satinfo
cp $SCANINFO        ./scaninfo
cp $OZINFO          ./ozinfo
# cp $LOCINFO         ./hybens_locinfo

cp $diag_ROOT/satbias_in ./satbias_in
cp $diag_ROOT/satbias_pc ./satbias_pc

# get mean
ln -s ${BK_FILE_mem}.ensmean ./firstguess.ensmean
for type in $list; do
   ln -s $diag_ROOT/diag_${type}_ges.ensmean .
done

# get each member
imem=1
while [[ $imem -le $NMEM_ENKF ]]; do
   member="mem"`printf %03i $imem`
   ln -s ${BK_FILE_mem}.${member} ./firstguess.${member}
   for type in $list; do
      ln -s $diag_ROOT/diag_${type}_ges.${member} .
   done
   (( imem = $imem + 1 ))
done

# Build the GSI namelist on-the-fly
. $ENKF_NAMELIST

# make analysis files
cp firstguess.ensmean analysis.ensmean
# get each member
imem=1
while [[ $imem -le $NMEM_ENKF ]]; do
   member="mem"`printf %03i $imem`
   cp firstguess.${member} analysis.${member}
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
