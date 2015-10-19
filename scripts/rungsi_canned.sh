#!/bin/ksh
#=======================================================
## Below are LSF (WCOSS queueing system) commands
#BSUB -a poe
#BSUB -e gsi_global.o%J
#BSUB -o gsi_global.o%J
#BSUB -J gsi_global
#BSUB -network type=sn_all:mode=US
#BSUB -q dev
#BSUB -n 32
#BSUB -R span[ptile=8]
#BSUB -R affinity[core(2):distribute=balance]
#BSUB -x
#BSUB -W 00:25
#BSUB -P GFS-T2O
#=======================================================

set -x

arch="`uname -s | awk '{print $1}'`"        
echo "Time starting the job is `date` "
# Set default top-level directory
MACHINE=WCOSS

#=================================================================================================
#  Most commom parameters to edit:
#=================================================================================================

# Set experiment name and analysis date
adate=2015101200
expnm=globalprod    
exp=globalprod.$adate
expid=${expnm}.$adate.wcoss

# Set path/file for gsi executable
version="r62854"
#version="r63277"
gsiexec=/global/save/$USER/gsi_branches/fov_util_goes/src/global_gsi.${version}

# Specify GSI fixed field
fixgsi=/global/save/$USER/gsi_branches/fov_util_goes/fix

# Set the JCAP resolution which you want.
# All resolutions use LEVS=64
export JCAP=62
export LEVS=64
export JCAP_B=$JCAP
export lrun_subdirs=.true.


# Set data, runtime and save directories
if [ $MACHINE = WCOSS ]; then
   datdir=/ptmpp1/$USER/data_sigmap/${exp}
   tmpdir=/ptmpp1/$USER/tmp${JCAP}_sigmap.${version}/${expid}  
   savdir=/ptmpp1/$USER/out${JCAP}.${version}/sigmap/${expid}  
   fixcrtm=/da/save/Michael.Lueken/CRTM_REL-2.2.3/crtm_v2.2.3/fix
   endianness=Big_Endian
   COMPRESS=gzip 
   UNCOMPRESS=gunzip
   DIAG_COMPRESS=YES 
   DIAG_SUFFIX="" 
   DIAG_TARBALL=YES 
else
  echo "Unsupported machine $MACHINE (not sure how you got to here)"
  exit 1
fi

# Other Executables and scripts
if [ $MACHINE = WCOSS ]; then
   export SIGHDR=/nwprod/exec/global_sighdr
   export CHGRESSH=/nwprod/ush/global_chgres.sh
   export ndate=/nwprod/util/exec/ndate
   export ncp=/bin/cp
   export wc=/usr/bin/wc
else
  echo "Unsupported machine $MACHINE (not sure how you got to here)"
  exit 1
fi

#=================================================================================================

# Refractive Index or Bending Angle for GPS?
export gps_dtype="gps_ref"


if [[  $MACHINE = WCOSS  ]]; then
  export MP_EAGER_LIMIT=65536 
  export MP_COREFILE_FORMAT=lite
  export MP_MPILIB=mpich2
  export MP_LABELIO=yes
  export MP_USE_BULK_XFER=yes
  export MP_SHARED_MEMORY=yes
  export MPICH_ALLTOALL_THROTTLE=0
  export MP_COLLECTIVE_OFFLOAD=yes
  export KMP_STACKSIZE=2048m
fi

# Variables for debugging (don't always need)
##export XLFRTEOPTS="buffering=disable_all"
##export MP_COREFILE_FORMAT=lite

# Set variables used in script
#   CLEAN up $tmpdir when finished (YES=remove, NO=leave alone)
#   ndate is a date manipulation utility
#   ncp is cp replacement, currently keep as /bin/cp

CLEAN=NO

# Given the requested resolution, set dependent resolution parameters
if [[ "$JCAP" = "878" ]]; then
   export LONA=1760
   export LATA=880
   export DELTIM=400
   export resol=1
elif [[ "$JCAP" = "574" ]]; then
   export LONA=1152
   export LATA=576
   export DELTIM=120
   export resol=1
elif [[ "$JCAP" = "382" ]]; then
   export LONA=768
   export LATA=384
   export DELTIM=180
   export resol=1
elif [[ "$JCAP" = "254" ]]; then
   export LONA=512
   export LATA=256
   export DELTIM=300
   export resol=1
elif [[ "$JCAP" = "190" ]]; then
   export LONA=576
   export LATA=288
   export DELTIM=600
   export resol=1
elif [[ "$JCAP" = "126" ]]; then
   export LONA=384
   export LATA=190
   export DELTIM=600
   export resol=1
elif [[ "$JCAP" = "62" ]]; then
   export LONA=192
   export LATA=94
   export DELTIM=1200
   export resol=2     # emily: test this
   export resol=1
else
   echo "INVALID JCAP = $JCAP"
   exit
fi
export NLAT_A=$((${LATA}+2))
export NLON_A=$LONA


# Given the analysis date, compute the date from which the
# first guess comes.  Extract cycle and set prefix and suffix
# for guess and observation data files
gdate=`$ndate -06 $adate`

dumpobs=gdas
dumpges=gdas


# Set up $tmpdir
 
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir
rm -rf core*

cp /global/noscrub/George.Gayno/goes_fov/globalprod.2015101200.wcoss/*  .

# Make gsi namelist

# CO2 namelist and file decisions
ICO2=${ICO2:-0}
if [ $ICO2 -gt 0 ] ; then
        # Copy co2 files to $tmpdir
        co2dir=${CO2DIR:-$fixgsi}
        yyyy=$(echo ${CDATE:-$adate}|cut -c1-4)
        rm ./global_co2_data.txt
        co2=$co2dir/global_co2.gcmscl_$yyyy.txt
        while [ ! -s $co2 ] ; do
                ((yyyy-=1))
                co2=$co2dir/global_co2.gcmscl_$yyyy.txt
        done
        if [ -s $co2 ] ; then
                $ncp $co2 ./global_co2_data.txt
        fi
        if [ ! -s ./global_co2_data.txt ] ; then
                echo "\./global_co2_data.txt" not created
                exit 1
   fi
fi
#CH4 file decision
ICH4=${ICH4:-0}
if [ $ICH4 -gt 0 ] ; then
#        # Copy ch4 files to $tmpdir
        ch4dir=${CH4DIR:-$fixgsi}
        yyyy=$(echo ${CDATE:-$adate}|cut -c1-4)
        rm ./ch4globaldata.txt
        ch4=$ch4dir/global_ch4_esrlctm_$yyyy.txt
        while [ ! -s $ch4 ] ; do
                ((yyyy-=1))
                ch4=$ch4dir/global_ch4_esrlctm_$yyyy.txt
        done
        if [ -s $ch4 ] ; then
                $ncp $ch4 ./ch4globaldata.txt
        fi
        if [ ! -s ./ch4globaldata.txt ] ; then
                echo "\./ch4globaldata.txt" not created
                exit 1
   fi
fi
IN2O=${IN2O:-0}
if [ $IN2O -gt 0 ] ; then
#        # Copy n2o files to $tmpdir
        n2odir=${N2ODIR:-$fixgsi}
        yyyy=$(echo ${CDATE:-$adate}|cut -c1-4)
        rm ./n2oglobaldata.txt
        n2o=$n2odir/global_n2o_esrlctm_$yyyy.txt
        while [ ! -s $n2o ] ; do
                ((yyyy-=1))
                n2o=$n2odir/global_n2o_esrlctm_$yyyy.txt
        done
        if [ -s $n2o ] ; then
                $ncp $n2o ./n2oglobaldata.txt
        fi
        if [ ! -s ./n2oglobaldata.txt ] ; then
                echo "\./n2oglobaldata.txt" not created
                exit 1
   fi
fi
ICO=${ICO:-0}
if [ $ICO -gt 0 ] ; then
#        # Copy CO files to $tmpdir
        codir=${CODIR:-$fixgsi}
        yyyy=$(echo ${CDATE:-$adate}|cut -c1-4)
        rm ./coglobaldata.txt
        co=$codir/global_co_esrlctm_$yyyy.txt
        while [ ! -s $co ] ; do
                ((yyyy-=1))
                co=$codir/global_co_esrlctm_$yyyy.txt
        done
        if [ -s $co ] ; then
                $ncp $co ./coglobaldata.txt
        fi
        if [ ! -s ./coglobaldata.txt ] ; then
                echo "\./coglobaldata.txt" not created
                exit 1
   fi
fi

GRIDOPTS=""
BKGVERR=""
ANBKGERR=""
JCOPTS=""
STRONGOPTS=""
OBSQC=""
OBSINPUT=""
SUPERRAD=""
LAGDATA=""
HYBRID_ENSEMBLE=""
RAPIDREFRESH_CLDSURF=""
CHEM=""
SINGLEOB=""


cat << EOF > gsiparm.anl
 &SETUP
   miter=2,niter(1)=10,niter(2)=10,
   niter_no_qc(1)=50,niter_no_qc(2)=0,
   write_diag(1)=.true.,write_diag(2)=.false.,write_diag(3)=.true.,
   qoption=2,
   gencode=82,factqmin=5.0,factqmax=5.0,deltim=$DELTIM,
   iguess=-1,
   oneobtest=.false.,retrieval=.false.,l_foto=.false.,
   use_pbl=.false.,use_compress=.true.,nsig_ext=12,gpstop=50.,
   use_gfs_nemsio=.false.,lrun_subdirs=${lrun_subdirs},
   newpc4pred=.true.,adp_anglebc=.true.,angord=4,
   passive_bc=.true.,use_edges=.false.,diag_precon=.true.,
   step_start=1.e-3,emiss_bc=.true.,
   $SETUP
 /
 &GRIDOPTS
   JCAP_B=$JCAP_B,JCAP=$JCAP,NLAT=$NLAT_A,NLON=$NLON_A,nsig=$LEVS,
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
   $BKGVERR
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
   baldiag_full=.true.,baldiag_inc=.true.,
   $STRONGOPTS
 /
 &OBSQC
   dfact=0.75,dfact1=3.0,noiqc=.true.,oberrflg=.false.,c_varqc=0.02,
   use_poq7=.true.,qc_noirjaco3_pole=.true.,
   $OBSQC
 /
 &OBS_INPUT
   dmesh(1)=145.0,dmesh(2)=150.0,time_window_max=3.0,
   $OBSINPUT
 /
OBS_INPUT::
!  dfile          dtype       dplat       dsis                 dval    dthin  dsfcalc
   prepbufr       ps          null        ps                   0.0     0      0
   prepbufr       t           null        t                    0.0     0      0
   prepbufr       q           null        q                    0.0     0      0
   prepbufr       pw          null        pw                   0.0     0      0
   satwndbufr     uv          null        uv                   0.0     0      0
   prepbufr       uv          null        uv                   0.0     0      0
   prepbufr       spd         null        spd                  0.0     0      0
   prepbufr       dw          null        dw                   0.0     0      0
   radarbufr      rw          null        rw                   0.0     0      0
   prepbufr       sst         null        sst                  0.0     0      0
   gpsrobufr      $gps_dtype  null        gps                  0.0     0      0
   ssmirrbufr     pcp_ssmi    dmsp        pcp_ssmi             0.0    -1      0
   tmirrbufr      pcp_tmi     trmm        pcp_tmi              0.0    -1      0
   sbuvbufr       sbuv2       n16         sbuv8_n16            0.0     0      0
   sbuvbufr       sbuv2       n17         sbuv8_n17            0.0     0      0
   sbuvbufr       sbuv2       n18         sbuv8_n18            0.0     0      0
   hirs3bufr      hirs3       n17         hirs3_n17            0.0     1      1
   hirs4bufr      hirs4       metop-a     hirs4_metop-a        0.0     1      1
   gimgrbufr      goes_img    g11         imgr_g11             0.0     1      0
   gimgrbufr      goes_img    g12         imgr_g12             0.0     1      0
   airsbufr       airs        aqua        airs281SUBSET_aqua   0.0     1      1
   amsuabufr      amsua       n15         amsua_n15            0.0     1      1
   amsuabufr      amsua       n18         amsua_n18            0.0     1      1
   amsuabufr      amsua       metop-a     amsua_metop-a        0.0     1      1
   airsbufr       amsua       aqua        amsua_aqua           0.0     1      1
   amsubbufr      amsub       n17         amsub_n17            0.0     1      1
   mhsbufr        mhs         n18         mhs_n18              0.0     1      1
   mhsbufr        mhs         metop-a     mhs_metop-a          0.0     1      1
   ssmitbufr      ssmi        f14         ssmi_f14             0.0     1      0
   ssmitbufr      ssmi        f15         ssmi_f15             0.0     1      0
   amsrebufr      amsre_low   aqua        amsre_aqua           0.0     1      0
   amsrebufr      amsre_mid   aqua        amsre_aqua           0.0     1      0
   amsrebufr      amsre_hig   aqua        amsre_aqua           0.0     1      0
   ssmisbufr      ssmis       f16         ssmis_f16            0.0     1      0
   gsnd1bufr      sndrd1      g12         sndrD1_g12           0.0     1      0
   gsnd1bufr      sndrd2      g12         sndrD2_g12           0.0     1      0
   gsnd1bufr      sndrd3      g12         sndrD3_g12           0.0     1      0
   gsnd1bufr      sndrd4      g12         sndrD4_g12           0.0     1      0
   gsnd1bufr      sndrd1      g15         sndrD1_g15           0.0     1      0
   gsnd1bufr      sndrd2      g15         sndrD2_g15           0.0     1      0
   gsnd1bufr      sndrd3      g15         sndrD3_g15           0.0     1      0
   gsnd1bufr      sndrd4      g15         sndrD4_g15           0.0     1      0
   gsnd1bufr      sndrd1      g13         sndrD1_g13           0.0     1      0
   gsnd1bufr      sndrd2      g13         sndrD2_g13           0.0     1      0
   gsnd1bufr      sndrd3      g13         sndrD3_g13           0.0     1      0
   gsnd1bufr      sndrd4      g13         sndrD4_g13           0.0     1      0
   iasibufr       iasi        metop-a     iasi616_metop-a      0.0     1      1
   gomebufr       gome        metop-a     gome_metop-a         0.0     2      0
   omibufr        omi         aura        omi_aura             0.0     2      0
   sbuvbufr       sbuv2       n19         sbuv8_n19            0.0     0      0
   hirs4bufr      hirs4       n19         hirs4_n19            0.0     1      1
   amsuabufr      amsua       n19         amsua_n19            0.0     1      1
   mhsbufr        mhs         n19         mhs_n19              0.0     1      1
   tcvitl         tcp         null        tcp                  0.0     0      0
   seviribufr     seviri      m08         seviri_m08           0.0     1      0
   seviribufr     seviri      m09         seviri_m09           0.0     1      0
   seviribufr     seviri      m10         seviri_m10           0.0     1      0
   hirs4bufr      hirs4       metop-b     hirs4_metop-b        0.0     1      0
   amsuabufr      amsua       metop-b     amsua_metop-b        0.0     1      0
   mhsbufr        mhs         metop-b     mhs_metop-b          0.0     1      0
   iasibufr       iasi        metop-b     iasi616_metop-b      0.0     1      0
   gomebufr       gome        metop-b     gome_metop-b         0.0     2      0
   atmsbufr       atms        npp         atms_npp             0.0     1      0
   crisbufr       cris        npp         cris_npp             0.0     1      0
::
 &SUPEROB_RADAR
   $SUPERRAD
 /
&LAG_DATA
   $LAGDATA
 /
 &HYBRID_ENSEMBLE
   $HYBRID_ENSEMBLE
 /
 &RAPIDREFRESH_CLDSURF
   dfi_radar_latent_heat_time_period=30.0,
   $RAPIDREFRESH_CLDSURF
 /
 &CHEM
   $CHEM
 /
 &SINGLEOB_TEST
   maginnov=0.1,magoberr=0.1,oneob_type='t',
   oblat=45.,oblon=180.,obpres=1000.,obdattim=${adate},
   obhourset=0.,
   $SINGLEOB
 /
EOF

# Copy executable and fixed files to $tmpdir
$ncp $gsiexec ./gsi.x

# Run gsi under Parallel Operating Environment (poe) on NCEP IBM

mpirun.lsf $tmpdir/gsi.x < gsiparm.anl > stdout
rc=$?

# Save output
mkdir -p $savdir

cat stdout fort.2* > $savdir/gsistat.$dumpobs.$adate
$ncp siganl          $savdir/siganl.$dumpobs.$adate
$ncp sfcanl.gsi      $savdir/sfcanl_gsi.$dumpobs.$adate
$ncp satbias_out     $savdir/biascr.$dumpobs.$adate
$ncp sfcf06          $savdir/sfcf06.$dumpges.$gdate
$ncp sigf06          $savdir/sigf06.$dumpges.$gdate

CNVSTAT=$savdir/cnvstat.$dumpobs.$adate
PCPSTAT=$savdir/pcpstat.$dumpobs.$adate
OZNSTAT=$savdir/oznstat.$dumpobs.$adate
RADSTAT=$savdir/radstat.$dumpobs.$adate

# Loop over first and last outer loops to generate innovation
# diagnostic files for indicated observation types (groups)
#
# NOTE:  Since we set miter=2 in GSI namelist SETUP, outer
#        loop 03 will contain innovations with respect to 
#        the analysis.  Creation of o-a innovation files
#        is triggered by write_diag(3)=.true.  The setting
#        write_diag(1)=.true. turns on creation of o-g
#        innovation files.
#

cd $tmpdir    # we should already be in $DATA, but extra cd to be sure. 

# Set up lists and variables for various types of diagnostic files. 
ntype=3 

diagtype[0]="conv" 
diagtype[1]="pcp_ssmi_dmsp pcp_tmi_trmm" 
diagtype[2]="sbuv2_n16 sbuv2_n17 sbuv2_n18 sbuv2_n19 gome_metop-a omi_aura mls_aura" 
diagtype[3]="hirs2_n14 msu_n14 sndr_g08 sndr_g11 sndr_g12 sndr_g13 sndr_g08_prep sndr_g11_prep sndr_g12_prep sndr_g13_prep sndrd1_g15 sndrd2_g15 sndrd3_g15 sndrd4_g15 sndrd1_g12 sndrd2_g12 sndrd3_g12 sndrd4_g12 sndrd1_g13 sndrd2_g13 sndrd3_g13 sndrd4_g13 sndrd1_g14 sndrd2_g14 sndrd3_g14 sndrd4_g14 sndrd1_g15 sndrd2_g15 sndrd3_g15 sndrd4_g15 hirs3_n15 hirs3_n16 hirs3_n17 amsua_n15 amsua_n16 amsua_n17 amsub_n15 amsub_n16 amsub_n17 hsb_aqua airs_aqua amsua_aqua imgr_g08 imgr_g11 imgr_g12 imgr_g14 imgr_g15 ssmi_f13 ssmi_f14 ssmi_f15 hirs4_n18 hirs4_metop-a amsua_n18 amsua_metop-a mhs_n18 mhs_metop-a amsre_low_aqua amsre_mid_aqua amsre_hig_aqua ssmis_las_f16 ssmis_uas_f16 ssmis_img_f16 ssmis_env_f16 ssmis_las_f17 ssmis_uas_f17 ssmis_img_f17 ssmis_env_f17 ssmis_las_f18 ssmis_uas_f18 ssmis_img_f18 ssmis_env_f18 ssmis_las_f19 ssmis_uas_f19 ssmis_img_f19 ssmis_env_f19 ssmis_las_f20 ssmis_uas_f20 ssmis_img_f20 ssmis_env_f20 iasi_metop-a hirs4_n19 amsua_n19 mhs_n19 seviri_m08 seviri_m09 seviri_m10 cris_npp atms_npp hirs4_metop-b amsua_metop-b mhs_metop-b iasi_metop-b gome_metop-b" 

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
   case $loop in 
      01) string=ges;; 
      03) string=anl;; 
       *) string=$loop;; 
   esac 
   n=-1 
   while [ $((n+=1)) -le $ntype ] ;do 
      for type in `echo ${diagtype[n]}`; do 
         count=`ls ${prefix}${type}_${loop}* | $wc -l` 
         if [ $count -gt 0 ]; then 
            cat ${prefix}${type}_${loop}* > diag_${type}_${string}.${adate}${DIAG_SUFFIX} 
            echo "diag_${type}_${string}.${adate}*" >> ${diaglist[n]} 
            numfile[n]=`expr ${numfile[n]} + 1` 
         fi 
      done 
   done 
done 

cd $tmpdir    # we should already be in $DATA, but extra cd to be sure. 

# If requested, compress diagnostic files 
if [[ $DIAG_COMPRESS = YES ]]; then 
   for file in `ls diag_*${adate}${DIAG_SUFFIX}`; do 
      $COMPRESS $file 
   done 
fi 

# If requested, create diagnostic file tarballs 
if [[ $DIAG_TARBALL = YES ]]; then 
   n=-1 
   while [ $((n+=1)) -le $ntype ] ;do 
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

# If requested, clean up $tmpdir
if [[ "$CLEAN" = "YES" ]];then
   if [[ $rc -eq 0 ]];then
      rm -rf $tmpdir
      cd $tmpdir
      cd ../
      rmdir $tmpdir
   fi
fi


# End of script
exit
