function (setJet)
  message("Setting paths for Jet")
  set(HOST_FLAG "-axSSE4.2,AVX,CORE-AVX2" CACHE INTERNAL "Host Flag")
  set(MKL_FLAG "-mkl"  CACHE INTERNAL "MKL Flag")
  set(GSI_Platform_FLAGS "${HOST_FLAG} -DPOUND_FOR_STRINGIFY -O3 -fp-model source -assume byterecl -convert big_endian -g -traceback -D_REAL8_ ${OMPFLAG} ${MPI_Fortran_COMPILE_FLAGS}" CACHE INTERNAL "GSI Fortran Flags")
  set(ENKF_Platform_FLAGS "-O3 ${HOST_FLAG} -warn all -implicitnone -traceback -fp-model strict -convert big_endian -DGFS -D_REAL8_ ${MPI3FLAG} ${OMPFLAG}" CACHE INTERNAL "ENKF Fortran Flags")
# set(BUILD_CORELIBS "OFF" )
endfunction()

