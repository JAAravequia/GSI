#!/bin/sh

#--------------------------------------------------------------------
#  Copy_glbl.sh
#
#    This script searches for new radmon output from the global GDAS
#    and copies those filess to the user's $TANKDIR directory under 
#    the specified suffix argument. 
#
#    The bad_penalty, low count, and missing diag reports are 
#    reevaluated using local copies of the base file and satype
#    files in the $TANKdir/$suffix/info directory. 
#    
#    Note that processing occurs within TANKdir, not in stmp space.
#
#    The unified error report is journaled to warning.${PDY}${CYC}
#    which is moved to the $TANKdir.
#
#--------------------------------------------------------------------

function usage {
  echo "Usage:  Copy_glbl.sh suffix date"
  echo "            File name for Copy_glbl.sh may be full or relative path"
  echo "            Suffix is the indentifier for this data source, and should"
  echo "             correspond to an entry in the ../../parm/data_map file."
  echo "            DATE is 10 digit yyyymmddhh string."
}




set -ax
echo start Copy_glbl.sh
exit_value=0

nargs=$#
if [[ $nargs -ne 2 ]]; then
   usage
   exit 1
fi

this_file=`basename $0`
this_dir=`dirname $0`

export RADMON_SUFFIX=$1
export DATE=$2
RUN=${RUN:-gdas}

export RAD_AREA=glb

echo RADMON_SUFFIX = $RADMON_SUFFIX
echo DATE   = $DATE


#--------------------------------------------------------------------
# Set environment variables
#--------------------------------------------------------------------

top_parm=${this_dir}/../../parm
export RADMON_VERSION=${RADMON_VERSION:-${top_parm}/radmon.ver}
if [[ -s ${RADMON_VERSION} ]]; then
   . ${RADMON_VERSION}
else
   echo "Unable to source ${RADMON_VERSION} (radmon version) file"
   exit 2
fi

export RADMON_CONFIG=${RADMON_CONFIG:-${top_parm}/RadMon_config}
if [[ -s ${RADMON_CONFIG} ]]; then
   . ${RADMON_CONFIG}
else
   echo "Unable to source ${RADMON_CONFIG} (radmon config) file"
   exit 2
fi

if [[ -s ${RADMON_USER_SETTINGS} ]]; then
   . ${RADMON_USER_SETTINGS}
else
   echo "Unable to source ${RADMON_USER_SETTINGS} (radmon user settings) file"
   exit 3
fi

export USHradmon=${USHradmon:-$HOMEradmon/ush}


#---------------------------------------------------------------
# Create any missing directories.
#---------------------------------------------------------------
mkdir -p $TANKverf
mkdir -p $LOGdir

day=`echo $DATE|cut -c1-8`
cycle=`echo $DATE|cut -c9-10`
echo day  = $day

PDATE=${DATE}
echo PDATE = $PDATE

prev=`$NDATE -06 $PDATE`
prev_day=`echo $prev|cut -c1-8`
prev_cyc=`echo $prev|cut -c9-10`
next=`$NDATE +06 $PDATE`
next_day=`echo $next|cut -c1-8`
next_cyc=`echo $next|cut -c9-10`

echo prev_day, prev_cyc = $prev_day, $prev_cyc
echo next_day, next_cyc = $next_day, $next_cyc

#----------------------------------------------
#  Default location is GFS output.  Override 
#  to copy parallel data. 
#
DATA=${DATA:-/gpfs/dell1/nco/ops/com/gfs/prod}


#----------------------------------------------
#  GFS output doesn't use this but most 
#  parallels do.  
USE_HR=${USE_HR:-0}

if [[ $USE_HR -eq 1 ]]; then
   DATDIR=${DATDIR:-${DATA}/${RUN}.${day}/${cycle}/radmon}
else
   DATDIR=${DATDIR:-${DATA}/${RUN}.${day}/radmon}
fi


if [[ $TANK_USE_RUN -eq 1 ]]; then
   test_dir=${TANKverf}/${RUN}.${day}/${cycle}/${MONITOR}
else
   test_dir=${TANKverf}/radmon.${day}
fi


satype_file=${TANKverf}/info/${RUN}_radmon_satype.txt

if [[ ! -s ${satype_file} ]]; then
   satype_file=${FIXgdas}/gdas_radmon_satype.txt
fi

if [[ ! -s ${satype_file} ]]; then
   echo "WARNING:  unable to locate ${satype_file}"
fi


nfile_src=`ls -l ${DATDIR}/*${PDATE}*ieee_d* | egrep -c '^-'`

if [[ $nfile_src -gt 0 ]]; then
   if [[ ! -d ${test_dir} ]]; then
      mkdir -p ${test_dir}
   fi
   cd ${test_dir}

   type_list="angle bcoef bcor time"

   for type in ${type_list}; do 

      file_list=`ls ${DATDIR}/${type}.*${PDATE}*ieee_d* `


      for file in ${file_list}; do
         bfile=`basename ${file}`
         echo "testing ${file}"
 
         if [[ ! -e ${test_dir}/${bfile} ]]; then
            echo "copying file" 
            ${NCP} ${file} ${test_dir}/${bfile}
         fi
      done
   done

   $NCP ${DATDIR}/*.ctl* ${test_dir}/.

   echo "PWD = $PWD"
   echo "test_dir = ${test_dir}"



   if [[ $DO_DATA_RPT -eq 1 ]]; then


      #-------------------------------------------------
      #  run validate.sh
      #
      $NCP ${DE_EXEC}/radmon_validate_tm.x ${test_dir}/.
      $NCP $DE_SCRIPTS/validate.sh    ${test_dir}/.
      echo "firing validate.sh"

      ./validate.sh ${PDATE}

      valid_tar=stdout.validate.tar

      if [[ $cycle -eq "00" ]]; then
         tar -cvf ${valid_tar} stdout.validate.*.00 
      else
         tar -rvf ${valid_tar} stdout.validate.*.${cycle}
      fi

      rm -f stdout.validate.*.${cycle}
  
      ls -la ./${valid_tar} 
   fi

else
   exit_value=5
fi

warn_msg="warning.${PDATE}"
warn_msg2="warning2.${PDATE}"

if [[ $exit_value == 0 ]]; then

   #--------------------------------------------------------------------
   #  Tar up the stdout.validation files 
   #--------------------------------------------------------------------
   if [[ $DO_DATA_RPT -eq 1 ]]; then


      #--------------------------------------------------------------------
      #  Remove extra spaces in new bad_pen and low_count files
      #--------------------------------------------------------------------
      bad_pen=bad_pen.${PDATE}
      gawk '{$1=$1}1' $bad_pen > tmp.bad_pen
      mv -f tmp.bad_pen $bad_pen

      low_count=low_count.${PDATE}
      gawk '{$1=$1}1' $low_count > tmp.low_count
      mv -f tmp.low_count $low_count
         
 
      #--------------------------------------------------------------------
      #  Diag report processing
      #
      #  New algorithm:
      #
      #     1.  locate satype and radstat files, specify output file
      #     2.  run new radmon_diag_ck.sh script
      #     3.  build diag report from output file
      #     4.  move output file to target tankdir
      #--------------------------------------------------------------------
      radstat_dir=${RADSTAT_DIR:-${DATA}/${RUN}.${day}/${cycle}}
      radstat=${radstat:-${radstat_dir}/${RUN}.t${cycle}z.radstat}
      diag_out="bad_diag.${PDATE}"

      if [[ -e ${satype_file} || -e ${radstat} ]]; then
         echo "satype  = $satype_file"
         echo "radstat = $radstat"

	 ${DE_SCRIPTS}/radmon_diag_ck.sh --rad ${radstat} --sat ${satype_file} --out ${diag_out}
         if [[ -e ${diag_out} ]]; then
            $NCP ./${diag_out} ${TANKverf}/${RUN}.${day}/${cyc}/radmon/.
         fi
      fi


      #--------------------------------------------------------------------
      #  Create a new penalty error report using the new bad_pen file
      #--------------------------------------------------------------------
      $NCP $DE_SCRIPTS/radmon_err_rpt.sh      ${test_dir}/.

      if [[ $TANK_USE_RUN -eq 1 ]]; then
         prev_bad_pen=${TANKverf}/${RUN}.${prev_day}/${prev_cyc}/${MONITOR}/bad_pen.${prev}
         prev_low_count=${TANKverf}/${RUN}.${prev_day}/${prev_cyc}/${MONITOR}/low_count.${prev}
      else
         prev_bad_pen=${TANKverf}/radmon.${prev_day}/bad_pen.${prev}
         prev_bad_pen=${TANKverf}/radmon.${prev_day}/low_count.${prev}
      fi

      bad_pen=bad_pen.${PDATE}
      diag_rpt="diag.txt"
      bad_pen_rpt="pen.${PDATE}.txt"
      err_rpt="err.${PDATE}.txt"
      low_obs_rpt="obs.${PDATE}.txt"

      ./radmon_err_rpt.sh $prev_bad_pen $bad_pen pen ${prev} ${PDATE} $diag_out $bad_pen_rpt

      ./radmon_err_rpt.sh $prev_low_count $low_count cnt ${prev} ${PDATE} $diag_out $low_obs_rpt



      #--------------------------
      #  Build the $warn_msg file
      #
      if [[ -s $bad_pen_rpt || -s $low_obs_rpt || -s ${diag_out} ]]; then

         if [[ -s $bad_pen_rpt ]]; then
            args="${args} --pen ${bad_pen_rpt}"
         fi

         if [[ -s $low_obs_rpt ]]; then
            args="${args} --obs ${low_obs_rpt}"
         fi

         if [[ -s $diag_out ]]; then
            args="${args} --diag ${diag_out}"
         fi

         ${DE_SCRIPTS}/radmon_mk_warning.sh ${args} --out ${warn_msg}
         ${NCP} ${warn_msg} ${DATDIR}/.

      fi

      $COMPRESS *.ctl

   fi

   #--------------------------------------------------------------------
   # Remove processing scripts/executables and exit.
   #--------------------------------------------------------------------
   rm -f radmon_validate_tm.x
   rm -f validate.sh 
   rm -f radmon_err_rpt.sh  

   nfile_dest=`ls -l ${test_dir}/*${PDATE}*ieee_d* | egrep -c '^-'`

   if [[ exit_value -eq 0 && $nfile_src -ne $nfile_dest ]]; then
      exit_value=6 
   fi

fi


echo end Copy_glbl.sh
exit ${exit_value}

