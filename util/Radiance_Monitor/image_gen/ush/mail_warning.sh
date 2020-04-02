#!/bin/bash
set -ax

#
# mail_warning.sh
#
# Find the warning.${PDATE} file and mail it to the .
# distribution list.
#
# This may only be called from CkPlt_glbl.sh.
#

#function usage {
#  echo "Usage:  mail_warning.sh pdate "
#  echo "            day is in 8 digit format [YYYYMMDD]"
#  echo "            cycle is in 2 digit format [CC]"
#  echo "            logfile is the relative or absolute path to the log file"
#}


#nargs=$#
#if [[ $nargs -ne 3 ]]; then
#   usage
#   exit 1
#fi

echo MAIL_TO = $MAIL_TO
echo MAIL_CC = $MAIL_CC


warning_file=${TANKverf}/${RUN}.${PDY}/${CYC}/${MONITOR}/warning.${PDATE}

if [[ -e ${warning_file} ]]; then
   echo "Have ${warning_file}"

   if [[ $MAIL_CC == "" ]]; then
      /bin/mail -s RadMon_error_report ${MAIL_TO}< ${warning_file}
   else
      /bin/mail -s RadMon_error_report -c "${MAIL_CC}" ${MAIL_TO}< ${warning_file}
   fi
fi

exit
