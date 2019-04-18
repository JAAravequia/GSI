#!/bin/sh --login

#BSUB -L /bin/sh
#BSUB -P GFS-T2O
#BSUB -o gsi1534.%J.log
#BSUB -e gsi1534.%J.log
#BSUB -J gsi1534
##BSUB -q dev 
#BSUB -q debug 
#BSUB -M 3072
#BSUB -extsched 'CRAYLINUX[]'
##BSUB -W 02:00
#BSUB -W 00:30
##BSUB -cwd /gpfs/hps3/emc/da/noscrub/Emily.Liu/GSI/ProdGSI-clddet/scripts_eliu/cray

set -x

#>>orig
export NODES=60
ntasks=360
ptile=6
#<<orig
#>>emily
#emily: need to increase the nodes handle more hydrometers
#export NODES=80
#ntasks=480
#ptile=6
#<<emily
threads=4

ulimit -s unlimited
ulimit -a

. $MODULESHOME/init/sh
module load PrgEnv-intel
module load cray-mpich
module load prod_util
module load prod_envir
module load cfp-intel-sandybridge
#module load crtm-intel/2.2.6

module list


export KMP_AFFINITY=disabled
export OMP_STACKSIZE=2G
export OMP_NUM_THREADS=$threads
export FORT_BUFFERED=true



# Set experiment name and analysis date
adate=2018052512
adate=2018052506

exp=clddet-cris.$adate


# Set YES to use background from EMC paralel
# NO = take from /com/gfs/para
use_emc_para=YES


# Set YES to use ensemble, NO=standard 3dvar
DOHYBVAR=YES
DOHYBVAR=NO

# Set YES to run 4D-EnsVar.  NO=3D-EnsVar or 3DVAR
DO4DENSVAR=YES
DO4DENSVAR=NO

# Set YES to use smoothed enkf forecasts
SMOOTH_ENKF=YES

# Set new radiance bias correction flag
export UNCOMPRESS=gunzip

# Generate diagnostic files
USE_RADSTAT=NO
GENDIAG=YES
DIAG_SUFFIX=""
CDATE=$adate
DIAG_COMPRESS=YES
COMPRESS=gzip
DIAG_TARBALL=YES
USE_CFP=NO


# Select data dump (gdas creates diagnostic files, gfs has no diagnostic files)
dumpobs=gdas


# Size of ensemble
ENS_NUM_ANAL=80
##ENS_NUM_ANAL=8


# Set path/file for gsi executable
##gsiexec=/gpfs/hps3/emc/global/noscrub/emc.glopara/git/fv3gfs/test/exec/global_gsi
gsiexec=/gpfs/hps3/emc/da/noscrub/Emily.Liu/GSI/ProdGSI-clddet/exec/global_gsi.x


# Set the JCAP resolution which you want.
# All resolutions use LEVS=64
export JCAP=766
export JCAP_B=1534
export LEVS=64

# Set runtime and save directories
PTMP=/gpfs/hps2/ptmp
DATA=$PTMP/$LOGNAME/tmp${JCAP}_sigmap/${exp}
SAVDIR=$PTMP/$LOGNAME/out${JCAP}_sigmap/${exp}
NOSCRUB=/gpfs/hps3/emc/da/noscrub


# Specify GSI fixed field
##fixgsi=/gpfs/hps3/emc/global/noscrub/emc.glopara/git/fv3gfs/test/fix/fix_gsi
#fixgsi=/gpfs/hps3/emc/global/noscrub/emc.glopara/git/gsi/master/fix
fixgsi=/gpfs/hps3/emc/da/noscrub/Emily.Liu/GSI/ProdGSI-clddet/fix
fixcrtm=$CRTM_FIX   # CRTM_FIX defined by crtm module
fixcrtm=/gpfs/hps3/emc/da/noscrub/Emily.Liu/RTM/CRTM/REL-2.3.1.beta/fix_crtm   # CRTM_FIX defined by crtm module

# Set variables used in script
#   CLEAN up $DATA when finished (YES=remove, NO=leave alone)
#   ndate is a date manipulation utility
#   ncp is cp replacement, currently keep as /bin/cp

CLEAN=NO
NDATE=${NDATE:-/nwprod/util/exec/ndate}
export wc=${wc:-/usr/bin/wc}
ncpc=/bin/cp
ncpl="ln -fs"

# Given the requested resolution, set dependent resolution parameters
if [[ "$JCAP" = "1534" ]]; then
   export LONA=3072
   export LATA=1536
   export DELTIM=120
elif [[ "$JCAP" = "1148" ]]; then
   export LONA=2304
   export LATA=1152
   export DELTIM=120
elif [[ "$JCAP" = "878" ]]; then
   export LONA=1760
   export LATA=880
   export DELTIM=120
elif [[ "$JCAP" = "766" ]]; then
   export LONA=1536
   export LATA=768
## export DELTIM=450
   export DELTIM=${DELTIM:-$((3600/($JCAP/20)))}   
elif [[ "$JCAP" = "574" ]]; then
   export LONA=1152
   export LATA=576
   export DELTIM=450
elif [[ "$JCAP" = "382" ]]; then
   export LONA=768
   export LATA=384
   export DELTIM=180
elif [[ "$JCAP" = "254" ]]; then
   export LONA=512
   export LATA=256
   export DELTIM=450
elif [[ "$JCAP" = "126" ]]; then
   export LONA=384
   export LATA=190
   export DELTIM=600
elif [[ "$JCAP" = "62" ]]; then
   export LONA=192
   export LATA=94
   export DELTIM=1200
else
   echo "INVALID JCAP = $JCAP"
   exit
fi
export NLON=$LONA
export NLAT=$((${LATA}+2))

# Given the analysis date, compute the date from which the
# first guess comes.  Extract cycle and set prefix and suffix
# for guess and observation data files
PDYa=`echo $adate | cut -c1-8`
cyca=`echo $adate | cut -c9-10`
gdate=`$NDATE -06 $adate`
PDYg=`echo $gdate | cut -c1-8`
cycg=`echo $gdate | cut -c9-10`

prefix_obs=${dumpobs}.t${cyca}z
prefix_ges=gdas.t${cycg}z
prefix_ens=gdas.t${cycg}z
suffix=tm00.bufr_d

dumpges=gdas
dumpanl=$dumpges

if [[ "$use_emc_para" = "YES" ]]; then
   DMPROOT=`echo $NWROOTp1 | cut -d"/" -f1-3`
   datobs=$DMPROOT/emc/globaldump/$adate/${dumpobs}
#  COMROOT=/gpfs/hps2/ptmp/Emily.Liu/Data_Input
   COMROOT=${NOSCRUB}/$LOGNAME/Data_Input/fv3test
   datges=$COMROOT/$dumpges.$PDYg/$cycg
   datanl=$COMROOT/$dumpanl.$PDYa/$cyca
else
   datobs=$COMROOThps/gfs/para/$dumpobs.$PDYa
   COMROOT=$COMROOThps
   datges=$COMROOT/$dumpges.$PDYg
   datanl=$COMROOT/$dumpanl.$PDYa   
fi
datens=$COMROOT/enkf.gdas.$PDYg/$cycg  


# Set up $DATA
rm -rf $DATA
mkdir -p $DATA
cd $DATA
rm -rf core*

# Make gsi namelist

# CO2 namelist and file decisions
ICO2=${ICO2:-0}
if [ $ICO2 -gt 0 ] ; then
#  Copy co2 files to $DATA
   co2dir=${CO2DIR:-$fixgsi}
   yyyy=$(echo ${CDATE:-$adate}|cut -c1-4)
   rm ./global_co2_data.txt
   co2=$co2dir/global_co2.gcmscl_$yyyy.txt
   if [ -s $co2 ] ; then
      $ncpc $co2 ./global_co2_data.txt
   fi
   if [ ! -s ./global_co2_data.txt ] ; then
      echo "\./global_co2_data.txt" not created
##    exit 1
   fi
fi

SETUP=""
GRIDOPTS=""
BKGVERR=""
ANBKGERR=""
JCOPTS=""
STRONGOPTS=""
OBSQC=""
OBSINPUT=""
SUPERRAD=""
LAGDATA=""
HYBRIDENSEMBLE=""
RR_CLDSURF=""
CHEM=""
SINGLEOB=""
NSST=""


# T254 quadratic grid dimensions are nlon=768, nlat=384
# T254 linear grid dimensions are nlon=512, nlat=256+2
# HYBRIDENSEMBLE namelist below set up for linear T254 grid
if [[ "$DOHYBVAR" = "NO" ]]; then
   STRONGOPTS="tlnmc_option=1,"
fi
if [[ "$DOHYBVAR" = "YES" ]]; then
ensemble_dir='./ensemble_data/'
export ensemble_path=${ensemble_dir:-./}
HYBRIDENSEMBLE="l_hyb_ens=.true.,n_ens=$ENS_NUM_ANAL,beta_s0=0.125,readin_beta=.false.,s_ens_h=800.,s_ens_v=-0.8,generate_ens=.false.,uv_hyb_ens=.true.,jcap_ens=766,nlat_ens=770,nlon_ens=1536,aniso_a_en=.false.,jcap_ens_test=766,readin_localization=.true.,oz_univ_static=.false.,ensemble_path='${ensemble_path}',ens_fast_read=.true.,write_ens_sprd=.true.,"
fi

# Turn off generation of diagnostic files for GFS run
SETUPGFS=""
if [[ "$dumpobs" = "gfs" ]]; then
  SETUPGFS="diag_rad=.false.,diag_pcp=.false.,diag_conv=.false.,diag_ozone=.false.,write_diag(3)=.false.,"
fi

SETUP_NSST=""
NSST=""

SETUP_NSST="tzr_qc=1,sfcnst_comb=.true.,"
NSST="nst_gsi=3,nstinfo=4,zsea1=0,zsea2=0,fac_dtl=1,fac_tsl=1,"

SETUP_4DVAR=""
JCOPTS_4DVAR=""
STRONGOPTS_4DVAR=""
if [[ "$DO4DENSVAR" = "YES" ]]; then
## SETUP_4DVAR="l4densvar=.true.,ens_nstarthr=3,nhr_obsbin=1,nhr_assimilation=6,lwrite4danl=.true."   # 1 hourly, 7 analysis
   SETUP_4DVAR="l4densvar=.true.,ens_nstarthr=3,nhr_obsbin=1,nhr_assimilation=6,lwrite4danl=.false.,"  # 1 hourly, 1 analysis
   JCOPTS_4DVAR="ljc4tlevs=.true.,"
   STRONGOPTS_4DVAR="tlnmc_option=3,"
##   if [[ "$dumpobs" = "gfs" ]]; then
##    SETUP_4DVAR="l4densvar=.true.,ens4d_nstarthr=3,nhr_obsbin=1,nhr_assimilation=6,"
##   fi
fi

##TEST
##SETUPGFS="factqmin=0.,factqmax=0.,"
##STRONGOPTS_4DVAR="tlnmc_option=0,"
####OBSQC="aircraft_t_bc=.false.,biaspredt=1000.0,upd_aircraft=.false.,"
##JCOPTS_4DVAR="ljcpdry=.false.,bamp_jcpdry=0.0,ljc4tlevs=.false.,"
##TEST

SETUP="$SETUP_4DVAR $SETUP_NSST $SETUPGFS"
JCOPTS="$JCOPTS_4DVAR"
STRONGOPTS="$STRONGOPTS_4DVAR"
#STRONGOPTS="$STRONGOPTS"  #emily

export verbose=${verbose:-".false."}
export imp_physics=11
export imp_physics=${imp_physics:-99}
export lupp=${lupp:-".true."}
export lrun_subdirs=${lrun_subdirs:-".true."}
export use_readin_anl_sfcmask=".true."       #orig
#export use_readin_anl_sfcmask=${use_readin_anl_sfcmask:-".false."}
export use_gfs_nemsio=".true."
export use_gfs_nemsio=${use_gfs_nemsio:-".false."}
crtm_coeffs=./crtm_coeffs/
export crtm_coeffs=${crtm_coeffs:-./}

# MLS ozone version 2.0 before 2013010306.   Version 3.0 starting 2013010306.
# Overwrite MLS entry in OBSINPUT for dates before 2013010306.
# NOTE:  scripting below DOES NOT WORK for GSI
##if [ $CDATE -lt 2013010306 ] ; then
##  export OBS_INPUT="mlsbufr        mls20       aura        mls20_aura          0.0     0     0"
##elif
##  export OBS_INPUT="mlsbufr        mls30       aura        mls30_aura          0.0     0     0"
##fi

rm -f gsiparm.anl

cat <<EOF > gsiparm.anl
 &SETUP
   miter=1,niter(1)=1,niter(2)=100,
   niter_no_qc(1)=25,niter_no_qc(2)=0,
   write_diag(1)=.true.,write_diag(2)=.false.,write_diag(3)=.true.,
   qoption=2,
   gencode=82,factqmin=5.0,factqmax=0.005,deltim=$DELTIM,
   iguess=-1,
   oneobtest=.false.,retrieval=.false.,l_foto=.false.,
   use_pbl=.false.,use_compress=.true.,nsig_ext=12,gpstop=50.,
   use_gfs_nemsio=${use_gfs_nemsio},lrun_subdirs=${lrun_subdirs},use_readin_anl_sfcmask=${use_readin_anl_sfcmask},
   crtm_coeffs_path='${crtm_coeffs}',
   newpc4pred=.true.,adp_anglebc=.true.,angord=4,passive_bc=.true.,use_edges=.false.,
   diag_precon=.true.,step_start=1.e-3,emiss_bc=.true.,thin4d=.true.,cwoption=3,
   verbose=${verbose},imp_physics=$imp_physics,lupp=$lupp,
   print_diag_pcg=.false.,
   luse_obsdiag=.false.,
   $SETUP
 /
 &GRIDOPTS
   JCAP_B=$JCAP_B,JCAP=$JCAP,NLAT=$NLAT,NLON=$NLON,nsig=$LEVS,
   regional=.false.,nlayers(63)=3,nlayers(64)=6,
   $GRIDOPTS
 /
 &BKGERR
   vs=0.7,
   hzscl=1.7,0.8,0.5,
   hswgt=0.45,0.3,0.25,
   bw=0.0,norsp=4,
   bkgv_flowdep=.true.,bkgv_rewgtfct=1.5,
   bkgv_write=.false.,
   cwcoveqqcov=.false.,
   $BKGERR   
 /
 &ANBKGERR
   anisotropic=.false.,
   $ANBKGERR
 /
 &JCOPTS
   ljcdfi=.false.,alphajc=0.0,ljcpdry=.true.,bamp_jcpdry=5.0e7,
   $JCOPTS   
 /
 &STRONGOPTS
   tlnmc_option=2,nstrong=1,nvmodes_keep=8,period_max=6.,period_width=1.5,
   baldiag_full=.false.,baldiag_inc=.false.,
   $STRONGOPTS   
 /
 &OBSQC
   dfact=0.75,dfact1=3.0,noiqc=.true.,oberrflg=.false.,c_varqc=0.04,
   use_poq7=.true.,qc_noirjaco3_pole=.true.,vqc=.true.,
   aircraft_t_bc=.true.,biaspredt=1000.0,upd_aircraft=.true.,cleanup_tail=.true.,
   $OBSQC
 /
 &OBS_INPUT
   dmesh(1)=145.0,dmesh(2)=150.0,dmesh(3)=100.0,time_window_max=3.0,
   $OBSINPUT
 /
OBS_INPUT::
!  dfile          dtype       dplat       dsis                dval    dthin dsfcalc
    crisfsbufr     cris-fsr    npp         cris-fsr_npp        0.0     1     0
::
  &SUPEROB_RADAR
   $SUPERRAD
 /
  &LAG_DATA
   $LAGDATA
 /
  &HYBRID_ENSEMBLE
   $HYBRIDENSEMBLE
 /
  &RAPIDREFRESH_CLDSURF
   dfi_radar_latent_heat_time_period=30.0,
   $RR_CLDSURF
 /
  &CHEM
   $CHEM
 /
  &NST
   $NSST
 /
 &SINGLEOB_TEST
   maginnov=0.1,magoberr=0.1,oneob_type='t',
   oblat=45.,oblon=180.,obpres=1000.,obdattim=${adate},
   obhourset=0.,
   $SINGLEOB
 /
EOF


# Set fixed files
#   berror   = forecast model background error statistics
#   specoef  = CRTM spectral coefficients
#   trncoef  = CRTM transmittance coefficients
#   emiscoef = CRTM coefficients for IR sea surface emissivity model
#   aerocoef = CRTM coefficients for aerosol effects
#   cldcoef  = CRTM coefficients for cloud effects
#   satinfo  = text file with information about assimilation of brightness temperatures
#   satangl  = angle dependent bias correction file (fixed in time)
#   pcpinfo  = text file with information about assimilation of prepcipitation rates
#   ozinfo   = text file with information about assimilation of ozone data
#   errtable = text file with obs error for conventional data (optional)
#   convinfo = text file with information about assimilation of conventional data
#   bufrtable= text file ONLY needed for single obs test (oneobstest=.true.)
#   bftab_sst= bufr table for sst ONLY needed for sst retrieval (retrieval=.true.)
#   aeroinfo = text file with information about assimilation of aerosol data

anavinfo=$fixgsi/global_anavinfo.l${LEVS}.txt
berror=$fixgsi/Big_Endian/global_berror.l${LEVS}y${NLAT}.f77
locinfo=$fixgsi/global_hybens_info.l${LEVS}.txt
satinfo=$fixgsi/global_satinfo.txt
scaninfo=$fixgsi/global_scaninfo.txt
satangl=$fixgsi/global_satangbias.txt
pcpinfo=$fixgsi/global_pcpinfo.txt
ozinfo=$fixgsi/global_ozinfo.txt
convinfo=$fixgsi/global_convinfo.txt
insituinfo=$fixgsi/global_insituinfo.txt
errtable=$fixgsi/prepobs_errtable.global
aeroinfo=$fixgsi/global_aeroinfo.txt
atmsbeaminfo=$fixgsi/atms_beamwidth.txt
cloudyinfo=$fixgsi/cloudy_radiance_info.txt

#>>emily
#   Set CONVINFO
    if [[ "$CDATE" -ge "2018022818" ]]; then
        export convinfo=$fixgsi/fv3_historical/global_convinfo.txt.2018022818
    elif [[ "$CDATE" -ge "2018010512" ]]; then
        export convinfo=$fixgsi/fv3_historical/global_convinfo.txt.2018010512
    elif [[ "$CDATE" -ge "2017071912" ]]; then
        export convinfo=$fixgsi/fv3_historical/global_convinfo.txt.2017071912
    elif [[ "$CDATE" -ge "2016031512" ]]; then
        export convinfo=$fixgsi/fv3_historical/global_convinfo.txt.2016031512
    elif [[ "$CDATE" -ge "2014041400" ]]; then
        export convinfo=$fixgsi/fv3_historical/global_convinfo.txt.2014041400
    else
        echo "WARNING: No CONVINFO for $CDATE"
    fi

#   Set SATINFO
    if [[ "$CDATE" -ge "2018021212" ]]; then
        export satinfo=$fixgsi/fv3_historical/global_satinfo.txt.2018021212
    elif [[ "$CDATE" -ge "2017103118" ]]; then
        export satinfo=$fixgsi/fv3_historical/global_satinfo.txt.2017103118
    elif [[ "$CDATE" -ge "2017031612" ]]; then
        export satinfo=$fixgsi/fv3_historical/global_satinfo.txt.2017031612
    elif [[ "$CDATE" -ge "2017030812" ]]; then
        export satinfo=$fixgsi/fv3_historical/global_satinfo.txt.2017030812
    elif [[ "$CDATE" -ge "2016110812" ]]; then
        export satinfo=$fixgsi/fv3_historical/global_satinfo.txt.2016110812
    elif [[ "$CDATE" -ge "2016090912" ]]; then
        export satinfo=$fixgsi/fv3_historical/global_satinfo.txt.2016090912
    elif [[ "$CDATE" -ge "2016020312" ]]; then
        export satinfo=$fixgsi/fv3_historical/global_satinfo.txt.2016020312
    elif [[ "$CDATE" -ge "2016011912" ]]; then
        export satinfo=$fixgsi/fv3_historical/global_satinfo.txt.2016011912
    elif [[ "$CDATE" -ge "2015111012" ]]; then
        export satinfo=$fixgsi/fv3_historical/global_satinfo.txt.2015111012
    elif [[ "$CDATE" -ge "2015100118" ]]; then
        export satinfo=$fixgsi/fv3_historical/global_satinfo.txt.2015100118
    elif [[ "$CDATE" -ge "2015070218" ]]; then
        export satinfo=$fixgsi/fv3_historical/global_satinfo.txt.2015070218
    elif [[ "$CDATE" -ge "2015011412" ]]; then
        export satinfo=$fixgsi/fv3_historical/global_satinfo.txt.2015011412
    else
        echo "WARNING: No SATINFO for $CDATE"
    fi

    export satinfo=$fixgsi/global_satinfo.txt  #emily

#<<emily

emiscoef_IRwater=$fixcrtm/Nalli.IRwater.EmisCoeff.bin
emiscoef_IRice=$fixcrtm/NPOESS.IRice.EmisCoeff.bin
emiscoef_IRland=$fixcrtm/NPOESS.IRland.EmisCoeff.bin
emiscoef_IRsnow=$fixcrtm/NPOESS.IRsnow.EmisCoeff.bin
emiscoef_VISice=$fixcrtm/NPOESS.VISice.EmisCoeff.bin
emiscoef_VISland=$fixcrtm/NPOESS.VISland.EmisCoeff.bin
emiscoef_VISsnow=$fixcrtm/NPOESS.VISsnow.EmisCoeff.bin
emiscoef_VISwater=$fixcrtm/NPOESS.VISwater.EmisCoeff.bin
emiscoef_MWwater=$fixcrtm/FASTEM6.MWwater.EmisCoeff.bin
aercoef=$fixcrtm/AerosolCoeff.bin
cldcoef=$fixcrtm/CloudCoeff.bin
cldcoef=/gpfs/hps3/emc/da/noscrub/Emily.Liu/RTM/CRTM/REL-2.3.1.beta/CloudCoeff_Test/MC6_Coeff/MC6_Coeff_ELiu/BE/CloudCoeff_c6_IR_MW_T230_BE.bin

# Only need this file for single obs test
bufrtable=$fixgsi/prepobs_prep.bufrtable

# Only need this file for sst retrieval
bftab_sst=$fixgsi/bufrtab.012

# Copy executable and fixed files to $DATA
$ncpc $gsiexec ./gsi.x

$ncpc $anavinfo ./anavinfo
$ncpc $berror   ./berror_stats
$ncpc $locinfo  ./hybens_info
$ncpc $satinfo  ./satinfo
$ncpc $scaninfo ./scaninfo
$ncpc $pcpinfo  ./pcpinfo
$ncpc $ozinfo   ./ozinfo
$ncpc $convinfo ./convinfo
$ncpc $insituinfo ./insituinfo
$ncpc $errtable ./errtable
$ncpc $aeroinfo ./aeroinfo
$ncpc $atmsbeaminfo ./atms_beamwidth.txt
$ncpc $cloudyinfo   ./cloudy_radiance_info.txt

$ncpc $bufrtable ./prepobs_prep.bufrtable
$ncpc $bftab_sst ./bftab_sstphr

# Copy CRTM coefficient files based on entries in satinfo file
mkdir -p ${crtm_coeffs}
for file in `awk '{if($1!~"!"){print $1}}' satinfo | sort | uniq` ;do
   $ncpc $fixcrtm/${file}.SpcCoeff.bin ${crtm_coeffs}
   $ncpc $fixcrtm/${file}.TauCoeff.bin ${crtm_coeffs}
done
$ncpc $emiscoef_IRwater  ${crtm_coeffs}Nalli.IRwater.EmisCoeff.bin
$ncpc $emiscoef_IRice    ${crtm_coeffs}NPOESS.IRice.EmisCoeff.bin
$ncpc $emiscoef_IRsnow   ${crtm_coeffs}NPOESS.IRsnow.EmisCoeff.bin
$ncpc $emiscoef_IRland   ${crtm_coeffs}NPOESS.IRland.EmisCoeff.bin
$ncpc $emiscoef_VISice   ${crtm_coeffs}NPOESS.VISice.EmisCoeff.bin
$ncpc $emiscoef_VISland  ${crtm_coeffs}NPOESS.VISland.EmisCoeff.bin
$ncpc $emiscoef_VISsnow  ${crtm_coeffs}NPOESS.VISsnow.EmisCoeff.bin
$ncpc $emiscoef_VISwater ${crtm_coeffs}NPOESS.VISwater.EmisCoeff.bin
$ncpc $emiscoef_MWwater  ${crtm_coeffs}FASTEM6.MWwater.EmisCoeff.bin
$ncpc $aercoef           ${crtm_coeffs}AerosolCoeff.bin
$ncpc $cldcoef           ${crtm_coeffs}CloudCoeff.bin

# Copy observational data to $DATA
#$ncpl $datanl/${prefix_obs}.prepbufr                ./prepbufr
#$ncpl $datanl/${prefix_obs}.prepbufr.acft_profiles  ./prepbufr_profl
#$ncpl $datanl/${prefix_obs}.nsstbufr                ./nsstbufr
$ncpl $datobs/${prefix_obs}.prepbufr                ./prepbufr
$ncpl $datobs/${prefix_obs}.prepbufr.acft_profiles  ./prepbufr_profl
$ncpl $datobs/${prefix_obs}.nsstbufr                ./nsstbufr

##datnew=/gpfs/hps3/ptmp/emc.glopara/ROTDIRS/prfv3test2/$dumpanl.$PDYa/$cyca
##$ncpl $datnew/${prefix_obs}.prepbufr                ./prepbufr
##$ncpl $datnew/${prefix_obs}.prepbufr.acft_profiles  ./prepbufr_profl
##$ncpl $datnew/${prefix_obs}.nsstbufr                ./nsstbufr

$ncpl $datobs/${prefix_obs}.gpsro.${suffix}         ./gpsrobufr
$ncpl $datobs/${prefix_obs}.satwnd.${suffix}        ./satwndbufr
$ncpl $datobs/${prefix_obs}.spssmi.${suffix}        ./ssmirrbufr
$ncpl $datobs/${prefix_obs}.sptrmm.${suffix}        ./tmirrbufr
$ncpl $datobs/${prefix_obs}.osbuv8.${suffix}        ./sbuvbufr
$ncpl $datobs/${prefix_obs}.goesfv.${suffix}        ./gsnd1bufr
$ncpl $datobs/${prefix_obs}.1bamua.${suffix}        ./amsuabufr
$ncpl $datobs/${prefix_obs}.1bamub.${suffix}        ./amsubbufr
$ncpl $datobs/${prefix_obs}.1bhrs2.${suffix}        ./hirs2bufr
$ncpl $datobs/${prefix_obs}.1bhrs3.${suffix}        ./hirs3bufr
$ncpl $datobs/${prefix_obs}.1bhrs4.${suffix}        ./hirs4bufr
$ncpl $datobs/${prefix_obs}.1bmhs.${suffix}         ./mhsbufr
$ncpl $datobs/${prefix_obs}.1bmsu.${suffix}         ./msubufr
$ncpl $datobs/${prefix_obs}.airsev.${suffix}        ./airsbufr
$ncpl $datobs/${prefix_obs}.sevcsr.${suffix}        ./seviribufr
$ncpl $datobs/${prefix_obs}.mtiasi.${suffix}        ./iasibufr
$ncpl $datobs/${prefix_obs}.ssmit.${suffix}         ./ssmitbufr
## $ncpl $datobs/${prefix_obs}.amsre.${suffix}      ./amsrebufr
$ncpl $datobs/${prefix_obs}.ssmisu.${suffix}        ./ssmisbufr
$ncpl $datobs/${prefix_obs}.gome.${suffix}          ./gomebufr
$ncpl $datobs/${prefix_obs}.omi.${suffix}           ./omibufr
$ncpl $datobs/${prefix_obs}.mls.${suffix}           ./mlsbufr
$ncpl $datobs/${prefix_obs}.eshrs3.${suffix}        ./hirs3bufrears
$ncpl $datobs/${prefix_obs}.esamua.${suffix}        ./amsuabufrears
$ncpl $datobs/${prefix_obs}.esamub.${suffix}        ./amsubbufrears
$ncpl $datobs/${prefix_obs}.atms.${suffix}          ./atmsbufr
$ncpl $datobs/${prefix_obs}.cris.${suffix}          ./crisbufr
$ncpl $datobs/${prefix_obs}.crisf4.${suffix}        ./crisfsbufr
$ncpl $datobs/${prefix_obs}.syndata.tcvitals.tm00   ./tcvitl
$ncpl $datobs/${prefix_obs}.avcsam.${suffix}        ./avhambufr
$ncpl $datobs/${prefix_obs}.avcspm.${suffix}        ./avhpmbufr
$ncpl $datobs/${prefix_obs}.saphir.${suffix}        ./saphirbufr
$ncpl $datobs/${prefix_obs}.gmi1cr.${suffix}        ./gmibufr
## $ncpl $datobs/${prefix_obs}.amsr2.tm00.bufr_d    ./amsr2bufr
$ncpl $datobs/${prefix_obs}.esiasi.${suffix}        ./iasibufrears
$ncpl $datobs/${prefix_obs}.hrs3db.${suffix}        ./hirs3bufr_db
$ncpl $datobs/${prefix_obs}.amuadb.${suffix}        ./amsuabufr_db
$ncpl $datobs/${prefix_obs}.amubdb.${suffix}        ./amsubbufr_db
$ncpl $datobs/${prefix_obs}.iasidb.${suffix}        ./iasibufr_db
$ncpl $datobs/${prefix_obs}.crisdb.${suffix}        ./crisbufr_db
$ncpl $datobs/${prefix_obs}.atmsdb.${suffix}        ./atmsbufr_db
$ncpl $datobs/${prefix_obs}.escris.${suffix}        ./crisbufrears
$ncpl $datobs/${prefix_obs}.esatms.${suffix}        ./atmsbufrears


# Copy bias correction, atmospheric and surface files
$ncpl $datges/${prefix_ges}.abias                   ./satbias_in
$ncpl $datges/${prefix_ges}.abias_pc                ./satbias_pc
$ncpl $datges/${prefix_ges}.abias_air               ./aircftbias_in

$ncpl $datges/${prefix_ges}.sfcf003.nemsio          ./sfcf03
$ncpl $datges/${prefix_ges}.sfcf006.nemsio          ./sfcf06
$ncpl $datges/${prefix_ges}.sfcf009.nemsio          ./sfcf09

$ncpl $datges/${prefix_ges}.atmf003.nemsio          ./sigf03
$ncpl $datges/${prefix_ges}.atmf006.nemsio          ./sigf06
$ncpl $datges/${prefix_ges}.atmf009.nemsio          ./sigf09

if [[ "$DO4DENSVAR" = "YES" ]]; then
   $ncpl $datges/${prefix_ges}.sfcf004.nemsio       ./sfcf04
   $ncpl $datges/${prefix_ges}.sfcf005.nemsio       ./sfcf05
   $ncpl $datges/${prefix_ges}.sfcf007.nemsio       ./sfcf07
   $ncpl $datges/${prefix_ges}.sfcf008.nemsio       ./sfcf08
   $ncpl $datges/${prefix_ges}.atmf004.nemsio       ./sigf04
   $ncpl $datges/${prefix_ges}.atmf005.nemsio       ./sigf05
   $ncpl $datges/${prefix_ges}.atmf007.nemsio       ./sigf07
   $ncpl $datges/${prefix_ges}.atmf008.nemsio       ./sigf08
fi


if [[ "$DOHYBVAR" = "YES" ]]; then
  enkf_suffix=""
  if [[ "$SMOOTH_ENKF" = "YES" ]]; then  
     enkf_suffix="s"
  fi
  flist="06"
  if [[ "$DO4DENSVAR" = "YES" ]]; then
     flist="03 04 05 06 07 08 09"
  fi
  mkdir -p $ensemble_path
  for fh in $flist; do
    sigens=${prefix_ens}.atmf0${fh}${enkf_suffix}.nemsio
    imem=1
    while [[ $imem -le $ENS_NUM_ANAL ]]; do
       member="mem"`printf %03i $imem`
       if [[ "$use_emc_para" = "YES" ]]; then
          $ncpl $datens/$member/$sigens ${ensemble_path}sigf${fh}_ens_${member}
       else
          $ncpl $datens/$sigens ${ensemble_path}sigf${fh}_ens_${member}
       fi
       (( imem = $imem + 1 ))
    done
  done
fi

if [[ "${use_readin_anl_sfcmask}" = ".true." ]]; then
   $ncpl $datens/${prefix_ens}.sfcf006.ensmean.nemsio ./sfcf06_anlgrid
fi



# If requested, copy and de-tar guess radstat file
if [[ $USE_RADSTAT = YES ]]; then
    $ncpl $datges/${prefix_ges}.radstat ./radstat.gdas
   ##radstat=/gpfs/hps3/ptmp/emc.glopara/out766_sigmap/test.2018012312.no_radstat.zero_abias.gfdlmp.crisf4/radstat.gdas.2018012312
    $ncpl $radstat ./radstat.gdas
fi

# If requested, copy and de-tar guess radstat file
if [[ $USE_CFP = YES ]]; then
   rm $DATA/unzip.sh
   rm $DATA/mp_unzip.sh
   set +x
cat <<\EOFunzip > unzip.sh
#!/bin/ksh
{ echo
 set -aux
 diag_file=$1
 fname=`echo $diag_file | cut -d'.' -f1`
 date=`echo $diag_file | cut -d'.' -f2`
 $UNCOMPRESS $diag_file
 fnameges=$(echo $fname|sed 's/_ges//g')
 mv $fname.$date $fnameges
}
EOFunzip
   set -x
   chmod 755 $DATA/unzip.sh
fi

listdiag=`tar xvf radstat.gdas | cut -d' ' -f2 | grep _ges`
for type in $listdiag; do
   diag_file=`echo $type | cut -d',' -f1`
   if [[ $USE_CFP = YES ]] ; then
      echo "$DATA/unzip.sh $diag_file" | tee -a $DATA/mp_unzip.sh
   else
      fname=`echo $diag_file | cut -d'.' -f1`
      date=`echo $diag_file | cut -d'.' -f2`
      $UNCOMPRESS $diag_file
      fnameges=$(echo $fname|sed 's/_ges//g')
      mv $fname.$date $fnameges
   fi
done

if [[ $USE_CFP = YES ]] ; then
   chmod 755 $DATA/mp_unzip.sh
   ncmd=`cat $DATA/mp_unzip.sh | $wc -l`
   npe_node_a=24
   if [ $ncmd -lt $npe_node_a ]; then
      npe_node_a = $ncmd
   fi
   if [ $ncmd -gt 0 ]; then
      export APRUNCFP='aprun -q -j1 -n$ncmd -N$npe_node_a -d1 cfp'
      export APRUNCFP_UNZIP=$(eval echo $APRUNCFP)
      $APRUNCFP_UNZIP $DATA/mp_unzip.sh
   fi
fi



# Run gsi under Parallel Operating Environment (poe) on NCEP IBM

printenv > stdout.env
printenv

which aprun

aprun -j1 -n $ntasks -N $ptile -d $threads -cc depth $DATA/gsi.x < gsiparm.anl 1> stdout 2> stderr
rc=$?

##cat fort.2* > gdas1.t${cyca}z.gsistat

if [[ "$GENDIAG" = "NO" ]] ; then
  exit
fi

# Save output
mkdir -p $SAVDIR
cat stdout fort.2* > $SAVDIR/stdout.anl.$adate
cat fort.2*        > $SAVDIR/${prefix_obs}.gsistat
cat fort.2*        > $SAVDIR/gsistat.$dumpobs.$adate
$ncpc siganl          $SAVDIR/gfnanl.$dumpobs.$adate
$ncpc satbias_out     $SAVDIR/biascr.$dumpobs.$adate
$ncpc satbias_pc.out  $SAVDIR/biascr_pc.$dumpobs.$adate
$ncpc satbias_out.int $SAVDIR/biascr.int.$dumpobs.$adate

##$ncpc sfcanl.gsi      $SAVDIR/sfcanl_gsi.$dumpobs.$adate
##$ncpc sfcf06          $SAVDIR/sfcf06.$dumpges.$gdate
##$ncpc sigf06          $SAVDIR/sigf06.$dumpges.$gdate


CNVSTAT=$SAVDIR/cnvstat.gdas.$adate
PCPSTAT=$SAVDIR/pcpstat.gdas.$adate
OZNSTAT=$SAVDIR/oznstat.gdas.$adate
RADSTAT=$SAVDIR/radstat.gdas.$adate

rm -f $CNVSTAT
rm -f $PCPSTAT
rm -f $OZNSTAT
rm -f $RADSTAT


cd $DATA    # we should already be in $DATA, but extra cd to be sure.
rm -rf diag_*


# Set up lists and variables for various types of diagnostic files.
ntype=3

diagtype[0]="conv"
diagtype[1]="pcp_ssmi_dmsp pcp_tmi_trmm"
diagtype[2]="sbuv2_n16 sbuv2_n17 sbuv2_n18 sbuv2_n19 gome_metop-a gome_metop-b omi_aura mls30_aura"
diagtype[3]="hirs2_n14 msu_n14 sndr_g08 sndr_g11 sndr_g12 sndr_g13 sndr_g08_prep sndr_g11_prep sndr_g12_prep sndr_g13_prep sndrd1_g11 sndrd2_g11 sndrd3_g11 sndrd4_g11 sndrd1_g12 sndrd2_g12 sndrd3_g12 sndrd4_g12 sndrd1_g13 sndrd2_g13 sndrd3_g13 sndrd4_g13 sndrd1_g14 sndrd2_g14 sndrd3_g14 sndrd4_g14 sndrd1_g15 sndrd2_g15 sndrd3_g15 sndrd4_g15 hirs3_n15 hirs3_n16 hirs3_n17 amsua_n15 amsua_n16 amsua_n17 amsub_n15 amsub_n16 amsub_n17 hsb_aqua airs_aqua amsua_aqua imgr_g08 imgr_g11 imgr_g12 imgr_g14 imgr_g15 ssmi_f13 ssmi_f15 hirs4_n18 hirs4_metop-a amsua_n18 amsua_metop-a mhs_n18 mhs_metop-a amsre_low_aqua amsre_mid_aqua amsre_hig_aqua ssmis_f16 ssmis_f17 ssmis_f18 ssmis_f19 ssmis_f20 iasi_metop-a hirs4_n19 amsua_n19 mhs_n19 seviri_m08 seviri_m09 seviri_m10 cris_npp cris-fsr_npp cris-fsr_n20 atms_npp atms_n20 hirs4_metop-b amsua_metop-b mhs_metop-b iasi_metop-b avhrr_n18 avhrr_metop-a amsr2_gcom-w1 gmi_gpm saphir_meghat ahi_himawari8"

diaglist[0]=listcnv
diaglist[1]=listpcp
diaglist[2]=listozn
diaglist[3]=listrad

diagfile[0]=$CNVSTAT
diagfile[1]=$PCPSTAT
diagfile[2]=$OZNSTAT
diagfile[3]=$RADSTAT

numfile[0]=0
numfile[1]=0
numfile[2]=0
numfile[3]=0


# Set diagnostic file prefix based on lrun_subdirs variable
if [ $lrun_subdirs = ".true." ]; then
   prefix=" dir.*/"
else
   prefix="pe*"
fi


# Collect diagnostic files as a function of loop and type.
loops="01 03"
for loop in $loops; do
   date
   case $loop in
      01) string=ges;;
      03) string=anl;;
       *) string=$loop;;
   esac
   n=-1
   while [ $((n+=1)) -le $ntype ] ;do
      for type in `echo ${diagtype[n]}`; do
         date
         count=`ls ${prefix}${type}_${loop}* | $wc -l`
         if [ $count -gt 0 ]; then
            cat ${prefix}${type}_${loop}* > diag_${type}_${string}.${CDATE}${DIAG_SUFFIX}
            echo "diag_${type}_${string}.${CDATE}*" >> ${diaglist[n]}
            numfile[n]=`expr ${numfile[n]} + 1`
         fi
      done
   done
done
date

cd $DATA    # we should already be in $DATA, but extra cd to be sure.

# If requested, compress diagnostic files
if [[ $DIAG_COMPRESS = YES ]]; then
   for file in `ls diag_*${CDATE}${DIAG_SUFFIX}`; do
      date
      $COMPRESS $file
   done
fi
date

# If requested, create diagnostic file tarballs
if [[ $DIAG_TARBALL = YES ]]; then
   n=-1
   while [ $((n+=1)) -le $ntype ] ;do
      date
      TAROPTS="-uvf"
      if [ ! -s ${diagfile[n]} ]; then
         TAROPTS="-cvf"
      fi
      if [ ${numfile[n]} -gt 0 ]; then
         tar $TAROPTS ${diagfile[n]} `cat ${diaglist[n]}`
      fi
   done

#  Restrict CNVSTAT 
   chmod 750 $CNVSTAT
   chgrp rstprod $CNVSTAT
fi
date


# If requested, clean up $DATA
if [[ "$CLEAN" = "YES" ]];then
   if [[ $rc -eq 0 ]];then
      rm -rf $DATA
      cd $DATA
      cd ../
      rmdir $DATA
   fi
fi
date

exit
