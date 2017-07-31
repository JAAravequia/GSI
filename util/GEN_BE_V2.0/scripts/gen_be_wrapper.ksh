#! /bin/ksh
#-----------------------------------------------------------------------
# Script gen_be_wrapper.ksh
#
# Purpose: Calculates background error statistics for WRF-Var.
#-----------------------------------------------------------------------

##PBS -N gen_be
##PBS -l walltime=00:30:00
##PBS -l nodes=100:ppn=4
##PBS -q debug
##PBS -A hybrid
##PBS -j oe


#[1] Define job by overriding default environment variables:
# this steps need to be run successively by setting up each variable to true

export RUN_GEN_BE_STAGE0=true  # Run stage 0 (create perturbation files).
export RUN_GEN_BE_STAGE1=false   # Run stage 1 (Remove mean, split variables).
export RUN_GEN_BE_STAGE2=false  # Run stage 2 (Regression coefficients).
export RUN_GEN_BE_STAGE3=false  # Run stage 3 (Vertical Covariances).
export RUN_GEN_BE_STAGE4=false  # Run stage 4 (Horizontal Covariances).
export RUN_GEN_BE_DIAGS=false   # Generate the be.nc file


export GEN_BE_DIR="/scratch4/NCEPDEV/da/save/Catherine.Thomas/gsi/EXP-genbe-gfs/util/GEN_BE_V2.0"  # code directory
export START_DATE=2010050300                           # the first perturbation valid date
export END_DATE=2010050306                             # the last perturbation valid date
export NUM_LEVELS=64                                   # number levels - 1
export FC_DIR="/scratch4/NCEPDEV/da/noscrub/Catherine.Thomas/bkerror/sig_fcst/nc"  # forecast directory"
export RUN_DIR="/scratch4/NCEPDEV/da/save/Catherine.Thomas/gsi/EXP-genbe-gfs/util/GEN_BE_V2.0/scripts"            # scripts directory
export WORK_DIR="/scratch4/NCEPDEV/stmp4/Catherine.Thomas/gen_be2" # working directory
export DOMAIN=01
export INTERVAL=06
export STRIDE=1
export FCST_RANGE=48
export FCST_RANGE1=48
export FCST_RANGE2=24

export GLOBAL=true
export GAUSSIAN_LATS=true
export MODEL="GFS"
export BE_METHOD="NMC"
export NE="1"

#[2] Run gen_be:
cd "$GEN_BE_DIR/scripts"
rm -rf $WORK_DIR
./gen_be.ksh
