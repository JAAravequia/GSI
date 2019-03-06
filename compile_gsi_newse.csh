#!/bin/csh

#source ~/.tcshrc

setenv CRAYOS_VERSION "6.0.4144"
setenv LAPACK_PATH /opt/intel/compilers_and_libraries_2016.4.258/linux/mkl/lib/intel64
setenv BLAS_DIR /opt/intel/compilers_and_libraries_2016.4.258/linux/mkl/lib/intel64
#setenv NETCDF /opt/cray/pe/netcdf-hdf5parallel/4.4.1.1.3/INTEL/16.0
setenv NETCDF4 ${NETCDF}

if (! -d build ) mkdir -p build
cd build

#if ( -r CMakeCache.txt ) rm CMakeCache.txt
cmake -DBLAS_LIBRARIES=${BLAS_DIR}/libmkl_blas95_lp64.a -DLAPACK_LIBRARIES=${LAPACK_PATH}/libmkl_lapack95_lp64.a -DBUILD_WRF=ON -DBUILD_UTIL_COM=ON ../

if ($1 =~ "util") then
  make -j8 read_diag_conv.x
  make -j8 read_diag_rad.x
  make -j8 innov_mean_conv.x
  make -j8 innov_mean_radiance.x
  make -j8 histo_adj_radiance.x
  #make -j8 #VERBOSE=1
else
  #cmake -DBLAS_LIBRARIES=${BLAS_DIR}/libmkl_blas95_lp64.a -DLAPACK_LIBRARIES=${LAPACK_PATH}/libmkl_lapack95_lp64.a -DBUILD_WRF=ON ../
      #VERBOSE=1 ../
      # -DCMAKE_BUILD_TYPE=Debug
  make -j8 #VERBOSE=1
endif
