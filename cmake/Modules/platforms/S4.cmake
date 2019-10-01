macro (setS4)
  message("Setting paths for S4")
  option(FIND_HDF5 "Try to Find HDF5 libraries" OFF)
  option(FIND_HDF5_HL "Try to Find HDF5 libraries" OFF)
  set(HOST_FLAG "-xHOST" CACHE INTERNAL "Host Flag")
  set(MKL_FLAG "-mkl"  CACHE INTERNAL "MKL Flag")
  set(GSI_Intel_Platform_FLAGS "-DPOUND_FOR_STRINGIFY -O3 -fp-model source -assume byterecl -convert big_endian -g -traceback -D_REAL8_ ${OpenMP_Fortran_FLAGS} ${MPI_Fortran_COMPILE_FLAGS}" CACHE INTERNAL "GSI Fortran Flags")
  set(ENKF_Platform_FLAGS "-O3 ${HOST_FLAG} -warn all -implicitnone -traceback -fp-model strict -convert big_endian -DGFS -D_REAL8_ ${MPI3FLAG} ${OpenMP_Fortran_FLAGS}" CACHE INTERNAL "ENKF Fortran Flags")
  set(HDF5_USE_STATIC_LIBRARIES "OFF")
  if( NOT DEFINED ENV{COREPATH} )
    set(COREPATH "/usr/local/jcsda/nwprod_gdas_2014/lib"  )
  else()
    set(COREPATH $ENV{COREPATH}  )
  endif()
  if( NOT DEFINED ENV{CRTM_INC} )
    set(CRTM_BASE "/usr/local/jcsda/NESDIS-JCSDA/tools_R2O/nwprod_2016q1/GFS_LIBs/CRTM_REL-2.2.3/crtm_v2.2.3"  )
  endif()
  if( NOT DEFINED ENV{WRFPATH} )
    set(WRFPATH "/usr/local/jcsda/nwprod_gdas_2014/sorc/nam_nmm_real_fcst.fd"  )
  else()
    set(WRFPATH $ENV{WRFPATH}  )
  endif()
  if( NOT DEFINED ENV{NETCDF_VER} )
    set(NETCDF_VER "4.3.3" )
  endif()
  if( NOT DEFINED ENV{BACIO_VER} )
    set(BACIO_VER "2.0.1" )
  endif()
  if( NOT DEFINED ENV{BUFR_VER} )
    set(BUFR_VER "10.2.5" )
  endif()
  if( NOT DEFINED ENV{CRTM_VER} )
    set(CRTM_VER "2.2.3" )
  endif()
  if( NOT DEFINED ENV{NEMSIO_VER} )
    set(NEMSIO_VER "2.2.1" )
  endif()
  if( NOT DEFINED ENV{SFCIO_VER} )
    set(SFCIO_VER "1.0.0" )
  endif()
  if( NOT DEFINED ENV{SIGIO_VER} )
    set(SIGIO_VER "2.0.1" )
#    set(SIGIO_VER "2.0.1_beta" )
#    set(ENV{SIGIO_LIB4} "/usr/local/jcsda/nwprod_gdas_2014/lib/libsigio_v2.0.1_beta.a")
#    set(ENV{SIGIO_INC4} "/usr/local/jcsda/nwprod_gdas_2014/lib/incmod/sigio_v2.0.1_beta" )
  endif()
  if( NOT DEFINED ENV{SP_VER} )
    set(SP_VER "2.0.2" )
  endif()
  if( NOT DEFINED ENV{W3EMC_VER} )
    set(W3EMC_VER "2.0.5" )
  endif()
  if( NOT DEFINED ENV{W3NCO_VER} )
    set(W3NCO_VER "2.0.6" )
  endif()
endmacro()
