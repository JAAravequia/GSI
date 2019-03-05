#!/bin/csh

#source ~/.tcshrc

setenv CRAYOS_VERSION "6.0.4144"
setenv LAPACK_PATH /opt/intel/compilers_and_libraries_2016.4.258/linux/mkl/lib/intel64
setenv BLAS_DIR /opt/intel/compilers_and_libraries_2016.4.258/linux/mkl/lib/intel64
#setenv NETCDF /opt/cray/pe/netcdf-hdf5parallel/4.4.1.1.3/INTEL/16.0
setenv NETCDF4 ${NETCDF}

mkdir build
cd build

cmake -DBLAS_LIBRARIES=${BLAS_DIR}/libmkl_blas95_lp64.a -DLAPACK_LIBRARIES=${LAPACK_PATH}/libmkl_lapack95_lp64.a -DBUILD_WRF=ON ../

      #VERBOSE=1 ../
     # -DCMAKE_BUILD_TYPE=Debug


#echo "$cmdstr"
# $cmdstr

#make -j8 #VERBOSE=1
