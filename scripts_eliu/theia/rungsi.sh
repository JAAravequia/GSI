#!/bin/sh -xvf

bdate=2018052506
edate=2018052506

ndate=/scratch4/NCEPDEV/da/save/Michael.Lueken/nwprod/util/exec/ndate

cdate=${bdate}

while [[ ${cdate} -le ${edate} ]]; do

 # qsub -v adate=${cdate} rungsi_master.sh   
 # qsub -v adate=${cdate} rungsi_projir.sh   
 # qsub -v adate=${cdate} rungsi_projir2master.sh   

   qsub -v adate=${cdate} rungsi_clddet_iasi.sh   
   qsub -v adate=${cdate} rungsi_clddet_ecmwf_iasi.sh   

 # qsub -v adate=${cdate} rungsi_clddet_cris.sh   
 # qsub -v adate=${cdate} rungsi_clddet_ecmwf_cris.sh   

 # qsub -v adate=${cdate} rungsi_clddet_airs.sh   
 # qsub -v adate=${cdate} rungsi_clddet_ecmwf_airs.sh   

   cdate=`$ndate +6 ${cdate}`

done

