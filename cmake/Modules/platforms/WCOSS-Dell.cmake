function (setWCOSS_D)
  message("Setting paths for ")
  set(HOST_FLAG "-xHOST" CACHE INTERNAL "Host Flag")
  set(MKL_FLAG "-mkl"  CACHE INTERNAL "MKL Flag")
  set(GSI_Platform_FLAGS "-DPOUND_FOR_STRINGIFY -fp-model strict -assume byterecl -convert big_endian -implicitnone -D_REAL8_ ${OMPFLAG} ${MPI_Fortran_COMPILE_FLAGS} -O3" CACHE INTERNAL "GSI Fortran Flags")
  set(GSI_LDFLAGS "${OMPFLAG} ${MKL_FLAG}" CACHE INTERNAL "")
  set(ENKF_Platform_FLAGS "-O3 -fp-model strict -convert big_endian -assume byterecl -implicitnone  -DGFS -D_REAL8_ ${MPI3FLAG} ${OMPFLAG} " CACHE INTERNAL "ENKF Fortran Flags")

  set(HDF5_USE_STATIC_LIBRARIES "ON" CACHE INTERNAL "" )
  if( NOT DEFINED ENV{COREPATH} )
    set(COREPATH "/gpfs/dell1/nco/ops/nwprod/lib" PARENT_SCOPE )
  else()
    set(COREPATH $ENV{COREPATH} PARENT_SCOPE )
  endif()
  if( NOT DEFINED ENV{CRTM_INC} )
    set(CRTM_BASE "/gpfs/dell1/nco/ops/nwprod/lib/crtm" PARENT_SCOPE )
  endif()
  if( NOT DEFINED ENV{NETCDF_VER} )
    set(NETCDF_VER "3.6.3" PARENT_SCOPE)
  endif()
  if( NOT DEFINED ENV{BACIO_VER} )
    set(BACIO_VER "2.0.2" PARENT_SCOPE)
  endif()
  if( NOT DEFINED ENV{BUFR_VER} )
    set(BUFR_VER "11.2.0" PARENT_SCOPE)
  endif()
  if( NOT DEFINED ENV{CRTM_VER} )
    set(CRTM_VER "2.2.5" PARENT_SCOPE)
  endif()
  if( NOT DEFINED ENV{NEMSIO_VER} )
    set(NEMSIO_VER "2.2.3" PARENT_SCOPE)
  endif()
  if( NOT DEFINED ENV{SFCIO_VER} )
    set(SFCIO_VER "1.0.0" PARENT_SCOPE)
  endif()
  if( NOT DEFINED ENV{SIGIO_VER} )
    set(SIGIO_VER "2.0.1" PARENT_SCOPE)
  endif()
  if( NOT DEFINED ENV{SP_VER} )
    set(SP_VER "2.0.2" PARENT_SCOPE)
  endif()
  if( NOT DEFINED ENV{W3EMC_VER} )
    set(W3EMC_VER "2.3.0" PARENT_SCOPE)
  endif()
  if( NOT DEFINED ENV{W3NCO_VER} )
    set(W3NCO_VER "2.0.6" PARENT_SCOPE)
  endif()
endfunction()
