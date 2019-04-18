#!/bin/sh -xvf

bdate=2018010506
edate=2018010506
cdate=${bdate}

while [[ ${cdate} -le ${edate} ]]; do

   ./Get_Initial_Files_Ops ${cdate}  
   cdate=`ndate +6 ${cdate}`

done

