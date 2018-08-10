#!/bin/sh
set -xa

#-----------------------------------------------------
#  
#  diag2grad_gps_case.sh
#
#-----------------------------------------------------

echo "--> diag2grad_gps_case.sh"

   echo "CMON_SUFFIX   = $CMON_SUFFIX"
   echo "TANKDIR_cmon  = $TANKDIR_cmon"
   echo "type          = $type"
   echo "PDATE         = $PDATE"
   echo "EXECcmon      = $EXECcmon"
   echo "cycle         = $cycle"
   echo "nreal         = $nreal"
   echo "mtype         = $mtype (type = $type)"
   echo "subtype       = $subtype"
   echo "hint          = $hint"
   echo "workdir       = $workdir"

   ctype=`echo ${mtype} | cut -c4-6`
   nreal2=`expr $nreal - 2`
   if [[ $VERBOSE = "YES" ]]; then
      echo "ctype, nreal2 = $ctype, $nreal2"
   fi

   #t130_card=alllev
   #t131_card=alllev
   #t132_card=alllev
   #t133_card=alllev

   card=alllev

   if [[ -e ./diag2grads ]]; then 
      rm -f ./diag2grads
   fi

#
#  start with all cases using grads_gpslev and see whate that gets us
#
#   if [ "$mtype" = 't130' -o "$mtype" = 't131' -o "$mtype" = 't132' -o "$mtype" = 't133' -o "$mtype" = 't134' -o "$mtype" = 't135' ]; then

      cp $EXECcmon/grads_gpslev.x ./diag2grads

      cat <<EOF >input
         &input
         intype='gps',stype='${mtype}',itype=$ctype,nreal=$nreal,
         iscater=1,igrads=1,levcard='$card',intv=$hint,subtype='${subtype}',isubtype=${subtype},
         /
EOF

#
#  I think mandlev is specific to t
#     
#   elif [ "$mtype" = 't120' ]; then
#      cp $EXECcmon/grads_mandlev.x ./diag2grads
#
#      cat <<EOF >input
#         &input
#         intype='  t',stype='${mtype}',itype=$ctype,nreal=$nreal,
#         iscater=1,igrads=1,subtype='${subtype}',isubtype=${subtype},
#         /
#EOF
#
##
##  probably don't need the sfc case?
##
#   elif [ "$mtype" = 't180' -o "$mtype" = 't181' -o "$mtype" = 't182' -o "$mtype" = 't183'  -o "$mtype" = 't187' ]; then
#      cp $EXECcmon/grads_sfc.x ./diag2grads
#      cat <<EOF >input
#         &input
#         intype='  t',stype='${mtype}',itype=$ctype,nreal=$nreal,
#         iscater=1,igrads=1,subtype='${subtype}',isubtype=${subtype},
#         /
#EOF
#   fi


./diag2grads <input>stdout 2>&1 


rm -f *tmp
mv stdout stdout_diag2grads_${mtype}_${subtype}.${cycle}

dest_dir="${TANKDIR_cmon}/horz_hist/${cycle}"

for file in gps*grads; do
   mv ${file} ${dest_dir}/${file}.${PDATE}
done
for file in gps*scater; do
   mv ${file} ${dest_dir}/${file}.${PDATE}
done


echo "<-- diag2grad_gps_case.sh"
exit
