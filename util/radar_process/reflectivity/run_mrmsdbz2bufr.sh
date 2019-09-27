#!/bin/bash
#####################################################
# machine set up (users should change this part)
#####################################################
#SBATCH -J mrmsdbz2bufr             # Job Name
#SBATCH -o mrmsdbz2bufr.o%j
#SBATCH -e mrmsdbz2bufr.o%j
########################################################################
###    used on schooner                                              ###
# #SBATCH -p debug                  # queue : normal, debug, debug_5min, etc.          
# #SBATCH -N 2                      # number of nodes
# #SBATCH -n 40                     # number of mpi tasks (proc, core)
# #SBATCH --tasks-per-node=20
########################################################################
###    used on stampede2                                             ###
#SBATCH -p skx-dev              # queue : (skx-)normal, development, skx-dev etc.
#SBATCH -N 1                        # number of nodes
#SBATCH -n 40                       # number of mpi tasks (proc, core)
#SBATCH --tasks-per-node=40
########################################################################
#SBATCH -t 00:30:00                 # Run time
#SBATCH -A TG-ATM160014 

#run the executable
################################################################################
####  UNIX Script Documentation Block
#                      .                                             .

set -x
# np=$PBS_NP
  np=40

  ATIME=201904302000
# ANAL_TIME=2016052619
  ANAL_TIME=`echo ${ATIME} | cut -c1-10`
  INIT_TIME=${ATIME}
  EN=00
  YYYYMMDD=`echo ${INIT_TIME} | cut -c1-8`
  HH=`echo ${INIT_TIME} | cut -c9-10`
  MN=`echo ${INIT_TIME} | cut -c11-12`
  START_TIME=${ANAL_TIME}
  modlopt=2

  machname="`/bin/uname -n`"
  HOST="`/bin/uname -n`"
  case "$machname" in
      *schooner* | *oscer* )
          ARCH='LINUX_Schooner_SLURM'
          export myhome=/home/${USER}
          export MYHOME=/home/${USER}
          export myscratch=/scratch/${USER}
          export MYSCRATCH=/scratch/${USER}
          export mywork=$myscratch
          export MYWORK=$myscratch
          HWT_KeyWrd="HWT2016"
          ;;
      *stampede* | *tacc* )
          ARCH='LINUX_Stampede_SLURM'
          export myhome=/home1/01540/${USER}
          export MYHOME=/home1/01540/${USER}
          export myscratch=/scratch/01540/${USER}
          export MYSCRATCH=/scratch/01540/${USER}
          export mywork=/work/01540/${USER}
          export MYWORK=/work/01540/${USER}
          HWT_KeyWrd="HWT2019"
          ;;
      *)
          if [ -d $HOME ] ; then
              export myhome=$HOME
              export MYHOME=$HOME
          else
              echo "variables myhome is not set for $machname"
              exit 1
          fi
          if [ -d $SCRATCH ] ; then
              export myscratch=$SCRATCH
              export MYSCRATCH=$SCRATCH
              export mywork=$myscratch
              export MYWORK=$myscratch
          else
              echo "variables myscratch is not set for $machname"
              exit 1
          fi
          HWT_KeyWrd="HWT2018"
          echo "please set variable ARCH for the mpi command on local machine $machname"
          exit 1
          ;;
  esac

  module list

  KeyName="Small3"

  if [ -z ${HWT_KeyWrd} ] ; then
      echo "variable HWT_KeyWrd is not defined. Check with it. Job abort!"
      exit 1
  fi
  if [ -z ${ARCH} ] ; then
      echo "variable ARCH is not defined. Check with it. Job abort!"
      echo "define ARCH and the mpi command to run parallel job on $HOST !"
      exit 1
  fi
  test_root=${myscratch}/${HWT_KeyWrd}/testrun/GSI_radar_${KeyName}
# test_root=${mywork}/${HWT_KeyWrd}/testrun/GSI_radar_${KeyName}

  rundir=${test_root}/dbz2bufr_mrmsg2_${ATIME}
  gsihome=${myhome}/build
  BUFRTABLE=${myhome}/ProdGSI/util/radar_process/reflectivity
  arpshome=${myhome}/ARPS/arps5.4_radarbufr

  testdatadir=${myscratch}/${HWT_KeyWrd}/testdata
# testdatadir=${mywork}/${HWT_KeyWrd}/testdata

  radardatdir=${testdatadir}/radar
  nexradl2dir=${radardatdir}/nexrad_l2
  mrmsgribdir=${radardatdir}/mrms/grib2
  arpsdatadir=${testdatadir}/arpsdata_for_88d2arps
  HWTSYS=${MYWORK}/${HWT_KeyWrd}
  WRFPARAMSDIR=${HWTSYS}/WRF_PARAMS

  SRC_RDRPROC=${gsihome}/bin

# DATADIR=${MYSCRATCH}/Data
  DATADIR=$testdatadir

  if [ $modlopt == 1 ]; then
    GEO_FILE=${myscratch}/geo_em.d01.nc_NEWSE20170518
  else
    GEO_FILE=/scratch/01540/ctong/BOKFV3/001/fv3_C768_201904301800/grid_spec.nc 
  fi
  RDR_INFO=${WRFPARAMSDIR}/radarinfo.dat

  workdir=${rundir}
  exefile=process_CAPS_mosaic.x
  export MOSAICTILENUM=1

  if [ ! -d ${workdir} ] ; then
      mkdir -p ${workdir}
  else
      rm -rf ${workdir}/*
  fi
  cd ${workdir}
  rm -f *

# NSSL MOSAIC Data
  COM_MOSAIC=${DATADIR}/radar
  COM_MOSAIC_GRIB2=${COM_MOSAIC}/mrms/grib2

### Process Mosaic
numtiles=${MOSAICTILENUM}

# command for parallel run
# make sure ARCH is defined
if [ ! "$ARCH" ] ; then
    echo " ARCH is not defined. Please define it in job script and re-submit the job script."
    exit 1
fi

case $ARCH in
   'IBM_LSF')
      ###### IBM LSF (Load Sharing Facility)
      RUN_COMMAND="mpirun.lsf " ;;

   'LINUX')
      if [ $np = 1 ]; then
         #### Linux workstation - single processor
         RUN_COMMAND=""
      else
         ###### Linux workstation -  mpi run
        RUN_COMMAND="mpirun -np ${GSIPROC} -machinefile ~/mach "
      fi ;;

   'LINUX_Schooner_SLURM')
      if [ $np = 1 ]; then
         #### Linux workstation - single processor
         RUN_COMMAND=""
      else
         ###### Linux workstation -  mpi run
         RUN_COMMAND="mpirun  "
      fi ;;

   'LINUX_Stampede_SLURM')
      if [ $np = 1 ]; then
         #### Linux workstation - single processor
         RUN_COMMAND=""
      else
         ###### Linux workstation -  mpi run
         RUN_COMMAND="ibrun  "
      fi ;;

   'LINUX_LSF')
      ###### LINUX LSF (Load Sharing Facility)
      RUN_COMMAND="mpirun.lsf " ;;

   'LINUX_PBS')
      #### Linux cluster PBS (Portable Batch System)
      RUN_COMMAND="mpirun -np ${GSIPROC} " ;;

   'DARWIN_PGI')
      ### Mac - mpi run
      if [ $np = 1 ]; then
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

# Directory for the Radar Mosaic input files

echo $START_TIME >STARTTIME

#====================================================================#
# Compute date & time components for the analysis time
ymd=`echo ${START_TIME} | cut -c1-8`
ymdh=`echo ${START_TIME} | cut -c1-10`
hh=`echo ${START_TIME} | cut -c9-10`
YYYYJJJHH00=`date +"%Y%j%H00" -d "${ymd} ${hh}"`
YYYYMMDDHH=`date +"%Y%m%d%H" -d "${ymd} ${hh}"`
YYYY=`date +"%Y" -d "${ymd} ${hh}"`
MM=`date +"%m" -d "${ymd} ${hh}"`
DD=`date +"%d" -d "${ymd} ${hh}"`
HH=`date +"%H" -d "${ymd} ${hh}"`
mm=`date +"%M" -d "${ymd} ${hh}"`
#====================================================================#

cp ${BUFRTABLE}/prepobs_prep.bufrtable   ./prepobs_prep.bufrtable
cp ${GEO_FILE}                           ./geo_em.d01.nc
cp ${RDR_INFO}                           ./radarinfo.dat

# find NSSL grib2 mosaic files
numgrib2_00=`ls ${COM_MOSAIC_GRIB2}/*_MergedReflectivityQC_*_${YYYY}${MM}${DD}-${HH}${EN}??.grib2 | wc -l`
numgrib2_01=`ls ${COM_MOSAIC_GRIB2}/*_MergedReflectivityQC_*_${YYYY}${MM}${DD}-${HH}01??.grib2 | wc -l`
numgrib2_02=`ls ${COM_MOSAIC_GRIB2}/*_MergedReflectivityQC_*_${YYYY}${MM}${DD}-${HH}02??.grib2 | wc -l`
if [ ${numgrib2_00} -eq 33 ]; then
   cp ${COM_MOSAIC_GRIB2}/*_MergedReflectivityQC_*_${YYYY}${MM}${DD}-${HH}${EN}??.grib2 .
   ls *_MergedReflectivityQC_*_${YYYY}${MM}${DD}-${HH}????.grib2 > filelist_mrms
else
   if [ ${numgrib2_01} -eq 33 ]; then
      cp ${COM_MOSAIC_GRIB2}/*_MergedReflectivityQC_*_${YYYY}${MM}${DD}-${HH}01??.grib2 .
      ls *_MergedReflectivityQC_*_${YYYY}${MM}${DD}-${HH}????.grib2 > filelist_mrms
   else
      if [ ${numgrib2_02} -eq 33 ]; then
         cp ${COM_MOSAIC_GRIB2}/*_MergedReflectivityQC_*_${YYYY}${MM}${DD}-${HH}02??.grib2 .
         ls *_MergedReflectivityQC_*_${YYYY}${MM}${DD}-${HH}????.grib2 > filelist_mrms
      else
         echo " No NSSL gribs data available, use NCEP 8 tiles binary"
         if [ -s filelist_mrms ]; then
            rm -f filelist_mrms
         fi
      fi
   fi
fi

if [ -s filelist_mrms ]; then
   numgrib2=`more filelist_mrms | wc -l`
   echo "iSSL grib2 file level number = $numgrib2"
else
   numgrib2=0
fi

# Link to the radar data
if [ $numgrib2 -eq 36 ]; then 
   gzip -d *.gz
   numtiles=1
   rm -f filelist_mrms
   ls *_MergedReflectivityQC_*_${YYYY}${MM}${DD}-${HH}????.grib2 > filelist_mrms
else
   if [ -s ${COM_MOSAIC}/nssl/mrms_binary/tile1/mrefl/MREF3D33L.${YYYY}${MM}${DD}.${HH}${mm}00.gz ] && \
      [ -s ${COM_MOSAIC}/nssl/mrms_binary/tile2/mrefl/MREF3D33L.${YYYY}${MM}${DD}.${HH}${mm}00.gz ] && \
      [ -s ${COM_MOSAIC}/nssl/mrms_binary/tile3/mrefl/MREF3D33L.${YYYY}${MM}${DD}.${HH}${mm}00.gz ] && \
      [ -s ${COM_MOSAIC}/nssl/mrms_binary/tile4/mrefl/MREF3D33L.${YYYY}${MM}${DD}.${HH}${mm}00.gz ]; then
      numtiles=4
      cp ${COM_MOSAIC}/nssl/mrms_binary/tile1/mrefl/MREF3D33L.${YYYY}${MM}${DD}.${HH}${mm}00.gz ./mosaic_t1.gz
      cp ${COM_MOSAIC}/nssl/mrms_binary/tile2/mrefl/MREF3D33L.${YYYY}${MM}${DD}.${HH}${mm}00.gz ./mosaic_t2.gz
      cp ${COM_MOSAIC}/nssl/mrms_binary/tile3/mrefl/MREF3D33L.${YYYY}${MM}${DD}.${HH}${mm}00.gz ./mosaic_t3.gz
      cp ${COM_MOSAIC}/nssl/mrms_binary/tile4/mrefl/MREF3D33L.${YYYY}${MM}${DD}.${HH}${mm}00.gz ./mosaic_t4.gz
      gzip -d *.gz
   else
      numtiles=81
      export MOSAICdir=${COM_MOSAIC}/nssl/mosaic3d_nc
      ln -s ${MOSAICdir}/tile1/${ymd}_${hh}00.mosaic ./mosaic_t1
      ln -s ${MOSAICdir}/tile2/${ymd}_${hh}00.mosaic ./mosaic_t2
      ln -s ${MOSAICdir}/tile3/${ymd}_${hh}00.mosaic ./mosaic_t3
      ln -s ${MOSAICdir}/tile4/${ymd}_${hh}00.mosaic ./mosaic_t4
      ln -s ${MOSAICdir}/tile5/${ymd}_${hh}00.mosaic ./mosaic_t5
      ln -s ${MOSAICdir}/tile6/${ymd}_${hh}00.mosaic ./mosaic_t6
      ln -s ${MOSAICdir}/tile7/${ymd}_${hh}00.mosaic ./mosaic_t7
      ln -s ${MOSAICdir}/tile8/${ymd}_${hh}00.mosaic ./mosaic_t8
   fi
fi

echo ${ymdh} > ./mosaic_cycle_date

cat << EOF > mosaic.namelist
 &setup
  tversion=${numtiles},
  analysis_time = ${YYYYMMDDHH},
  dataPath = './',
  l_latlon = .TRUE.,
  l_psot = .FALSE.,
  iskip = 1, 
  jskip = 1,
  lev_keep = 250,
  modlopt = ${modlopt},
 /
 &oneob
   l_latlon_psot = .FALSE.,
   olat = 100,
   olon = 150,
   olvl = 15,
   odbz = 75.0,
 /

EOF

#startmsg

  cp -p ${SRC_RDRPROC}/${exefile} ./
# mpiexec -np $np $pgm > process_mosaic.out
${RUN_COMMAND}  ${exefile} > process_mosaic.out 2>&1

 msg="JOB $job FOR RAP_PREP HAS COMPLETED NORMALLY"


# cp -p ${SRC_RDRPROC}/get_mosaic.exe ./
# ./get_mosaic.exe

# cp -p ${SRC_RDRPROC}/plot_mine/plt_crefl_obs.ncl ./plt_crefl_obs.ncl
#sed -i 's/OBS_DATE_TIME/'${START_TIME}'/g'       ./plt_crefl_obs.ncl
# sed -e "s@OBS_DATE_TIME@${START_TIME}@g" -i      ./plt_crefl_obs.ncl
 ncl plt_crefl_obs.ncl

exit 0

