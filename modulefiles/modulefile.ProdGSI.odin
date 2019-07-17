#%Module######################################################################
##                                                       Russ.Treadon@noaa.gov
##                                                           NOAA/NWS/NCEP/EMC
## GDAS_ENKF v6.2.3
##_____________________________________________________
#set ver v6.2.3

set COMP ifort
set COMP_MP ftn
set COMP_MPI ftn

set C_COMP icc
set C_COMP_MP cc

# Known conflicts

# Loading Intel Compiler Suite
#module load intel/15.6.233

# Loading impi
#module load impi/5.1.2.150
#module load mvapich2/2.1rc1

# Loading netcdf and hdf5
#module load hdf5/1.8.14
#module load netcdf/4.3.0
# Loading nceplibs modules
module use -a /oldscratch/ywang/external/modulefiles
module load bufr/v11.2.0
module load nemsio/v2.2.2
module load sfcio/v1.0.0
module load sigio/v2.0.1
module load sp/v2.0.2
module load w3nco/v2.0.6
module load w3emc/v2.3.0
module load bacio/v2.0.2
module load crtm/v2.2.5
#echo "done loading modules"

# Loading production utilities (ndate)
#module use /scratch4/NCEPDEV/nems/noscrub/emc.nemspara/soft/modulefiles
module load prod_util

#module use -a /contrib/da/modulefiles
#module load cmake

setenv CRAYOS_VERSION 6.0.UP04
#setenv NETCDF /opt/cray/pe/netcdf-hdf5parallel/4.4.1.1.3/INTEL/16.0
setenv NETCDF4 /opt/cray/pe/netcdf-hdf5parallel/4.4.1.1.3/INTEL/16.0
setenv LAPACK_PATH /opt/intel/compilers_and_libraries_2016.4.258/linux/mkl/lib/intel64
setenv BLAS_DIR /opt/intel/compilers_and_libraries_2016.4.258/linux/mkl/lib/intel64
setenv BLAS_LIBRARIES /opt/intel/compilers_and_libraries_2016.4.258/linux/mkl/lib/intel64/libmkl_blas95_lp64.a 
setenv LAPACK_LIBRARIES /opt/intel/compilers_and_libraries_2016.4.258/linux/mkl/lib/intel64/libmkl_lapack95_lp64.a
