#!/bin/csh -x

#PBS -N nmcstats
#PBS -l walltime=00:10:00
#PBS -l nodes=100:ppn=4
#PBS -q debug
#PBS -A da-cpu
#PBS -j oe

# Set NMC statistics utility directory
setenv GSIDIR         /scratch4/NCEPDEV/da/save/$USER/gsi/EXP-NMC-NEMS
#setenv GSIDIR         /scratch4/NCEPDEV/da/save/$USER/gsi/EXP-NMC-NEMS
#setenv GSIDIR         /scratch4/NCEPDEV/global/save/$USER/gsi/branches/EXP-betaprofile
setenv CALCSTATS_EXEC $GSIDIR/util/NMC_Bkerror/sorc/calcstats.exe

# Set Input Resolution and path to lagged pairs database
setenv JCAPIN 574
setenv NEMS yes
if ( $NEMS == "yes") then
  setenv PERTURBDIR /scratch4/NCEPDEV/da/noscrub/Catherine.Thomas/bkerror/sig_fcst/nems
else
  setenv PERTURBDIR /scratch4/NCEPDEV/da/noscrub/Catherine.Thomas/bkerror/sig_fcst/sig
  #setenv PERTURBDIR /scratch4/NCEPDEV/da/noscrub/Rahul.Mahajan/BKERROR
  #setenv PERTURBDIR /scratch4/NCEPDEV/global/noscrub/glopara/enkf/data254_specps
endif

# Set Output Resolution
setenv JCAP 574
setenv NLAT 578
setenv NLON 1152
#setenv JCAP 254
#setenv NLAT 258
#setenv NLON 512
setenv LEVS 64

# Number of cases to calculate statistics from
setenv MAXCASES 8

# Create a temporary working directory
setenv TMPDIR "/scratch4/NCEPDEV/stmp4/$user/tmp/nmcstats_T${JCAP}_n$MAXCASES"
if ( -d $TMPDIR ) rm -rf $TMPDIR
mkdir -p $TMPDIR
cd $TMPDIR

# Namelist
rm -f stats.parm
if ($NEMS == "yes") then
    setenv nems ".true."
else
    setenv nems ".false."
endif
cat << EOF > stats.parm
&namstat
    jcap=$JCAP,
    jcapin=$JCAPIN,
    jcapsmooth=$JCAP,
    nsig=$LEVS,
    nlat=$NLAT,
    nlon=$NLON,
    maxcases=$MAXCASES,
    hybrid=.true.,
    smoothdeg=0.5,
    biasrm=.true.,
    vertavg=.true.,
    nems=$nems
/
EOF

# Link perturbation database
rm -f infiles
touch infiles
if ( $NEMS == "yes") then
  ls $PERTURBDIR/gfnf24.gfs.* >> infiles
  ls $PERTURBDIR/gfnf48.gfs.* >> infiles
else
  ls $PERTURBDIR/sigf24.gfs.* >> infiles
  ls $PERTURBDIR/sigf48.gfs.* >> infiles
endif
ln -sf infiles fort.10

# Copy the executable and link necessary files
cp -f $CALCSTATS_EXEC                                   ./calcstats.exe
cp -f $GSIDIR/util/NMC_Bkerror/fix/sst2dvar_stat0.5.ufs ./berror_sst

setenv MPI_BUFS_PER_PROC 1024
setenv MPI_BUFS_PER_HOST 1024
setenv MPI_GROUP_MAX     1024
setenv OMP_NUM_THREADS   1
setenv APRUN             "mpirun -np $PBS_NP"

$APRUN calcstats.exe |& tee calcstats.out
if ( $status ) exit $status

rm -f fort.[0-9]*

exit 0
