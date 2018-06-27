function (setIntel)
  string(REPLACE "." ";" COMPILER_VERSION_LIST ${CMAKE_C_COMPILER_VERSION})
  list(GET COMPILER_VERSION_LIST 0 MAJOR_VERSION)
  list(GET COMPILER_VERSION_LIST 1 MINOR_VERSION)
  list(GET COMPILER_VERSION_LIST 2 PATCH_VERSION)
  set(COMPILER_VERSION "${MAJOR_VERSION}.${MINOR_VERSION}.${PATCH_VERSION}" CACHE INTERNAL "Compiler Version") 
  message("Compiler version is ${MAJOR_VERSION}.${MINOR_VERSION}.${PATCH_VERSION}") 
  message("Compiler version is ${COMPILER_VERSION}")
  if(${MAJOR_VERSION} GREATER 15 )
    set( OMPFLAG "-qopenmp" CACHE INTERNAL "OpenMP flag")
  else()
    set( OMPFLAG "-openmp" CACHE INTERNAL "OpenMP flag")
  endif() 
  STRING(COMPARE EQUAL ${CMAKE_BUILD_TYPE} "RELEASE" BUILD_RELEASE)
  STRING(COMPARE EQUAL ${CMAKE_BUILD_TYPE} "PRODUCTION" BUILD_PRODUCTION)
  set( MKL_FLAG "-mkl" )
  if( (BUILD_RELEASE) OR (BUILD_PRODUCTION) )
    if(( HOST-Tide ) OR ( HOST-Gyre ))
      set(GSI_Fortran_FLAGS "-DPOUND_FOR_STRINGIFY -traceback -O3 -fp-model source -convert big_endian -assume byterecl -implicitnone -D_REAL8_ ${OMPFLAG} ${MPI_Fortran_COMPILE_FLAGS}" PARENT_SCOPE)
      set(ENKF_Fortran_FLAGS "-O3 -fp-model source -convert big_endian -assume byterecl -implicitnone  -DGFS -D_REAL8_ ${OMPFLAG}" PARENT_SCOPE)
      set(UTIL_Fortran_FLAGS "-O3 -fp-model source -convert big_endian -assume byterecl -implicitnone  -DWRF -D_REAL8_ ${OMPFLAG}" PARENT_SCOPE)
      set (BACIO_Fortran_FLAGS "-O3 -free -assume nocc_omp " PARENT_SCOPE )
      set (BUFR_Fortran_FLAGS "-O2 -r8 -fp-model strict -traceback -xSSE2 -O3 -axCORE-AVX2 ${OMPFLAG} " PARENT_SCOPE )
      set (BUFR_C_FLAGS "-g -traceback -DUNDERSCORE -O3 -axCORE-AVX2 -DDYNAMIC_ALLOCATION -DNFILES=32 -DMAXCD=250 -DMAXNC=600 -DMXNAF=3" PARENT_SCOPE )
    elseif( HOST-Luna OR HOST-Surge )
      set( MKL_FLAG "" )
      set(GSI_Fortran_FLAGS "-DPOUND_FOR_STRINGIFY -fp-model source -assume byterecl -convert big_endian -implicitnone -D_REAL8_ ${OMPFLAG} ${MPI_Fortran_COMPILE_FLAGS}" PARENT_SCOPE)
      set(GSI_LDFLAGS "-liomp5" PARENT_SCOPE)
 
      set(ENKF_Fortran_FLAGS "-O3 -fp-model source -convert big_endian -assume byterecl -implicitnone  -DGFS -D_REAL8_ ${OMPFLAG} " PARENT_SCOPE)
      set(UTIL_Fortran_FLAGS "-O3 -fp-model source -convert big_endian -assume byterecl -implicitnone  -DWRF -D_REAL8_ ${OMPFLAG} " PARENT_SCOPE)
      set (BACIO_Fortran_FLAGS "-O3 -free -assume nocc_omp " PARENT_SCOPE )
      set (BUFR_Fortran_FLAGS " -c -g -traceback -O3 -axCORE-AVX2 -r8 " PARENT_SCOPE )
      set (BUFR_C_FLAGS "-DSTATIC_ALLOCATION -DUNDERSCORE -DNFILES=32 -DMAXCD=250 -DMAXNC=600 -DMXNAF=3" PARENT_SCOPE )
    elseif ( HOST-Venus OR HOST-Mars )
      set(GSI_Fortran_FLAGS "-DPOUND_FOR_STRINGIFY -traceback -fp-model strict -assume byterecl -convert big_endian -implicitnone -D_REAL8_ ${OMPFLAG} ${MPI_Fortran_COMPILE_FLAGS} -O3" PARENT_SCOPE)
      set(GSI_LDFLAGS "${OMPFLAG} ${MKL_FLAG}" PARENT_SCOPE)
 
      set(ENKF_Fortran_FLAGS "-O3 -fp-model source -convert big_endian -assume byterecl -implicitnone  -DGFS -D_REAL8_ ${OMPFLAG} " PARENT_SCOPE)
      set(UTIL_Fortran_FLAGS "-O3 -fp-model source -convert big_endian -assume byterecl -implicitnone  -DWRF -D_REAL8_ ${OMPFLAG} " PARENT_SCOPE)
      set (BACIO_Fortran_FLAGS "-O3 -free -assume nocc_omp " PARENT_SCOPE )
      set (BUFR_Fortran_FLAGS " -c -g -traceback -O3 -axCORE-AVX2 -r8 " PARENT_SCOPE )
      set (BUFR_C_FLAGS "-DSTATIC_ALLOCATION -DUNDERSCORE -DNFILES=32 -DMAXCD=250 -DMAXNC=600 -DMXNAF=3" PARENT_SCOPE )
    else()
      set(GSI_Fortran_FLAGS "-DPOUND_FOR_STRINGIFY -O3 -fp-model source -assume byterecl -convert big_endian -g -traceback -D_REAL8_ ${OMPFLAG} ${MPI_Fortran_COMPILE_FLAGS}" PARENT_SCOPE)
      set(ENKF_Fortran_FLAGS "-O3 -xHOST -warn all -implicitnone -traceback -fp-model strict -convert big_endian -DGFS -D_REAL8_ ${OMPFLAG}" PARENT_SCOPE)
      set(UTIL_Fortran_FLAGS "-O3 -xHOST -warn all -implicitnone -traceback -fp-model strict -convert big_endian -DWRF -D_REAL8_ ${OMPFLAG}" PARENT_SCOPE)
      set (BACIO_Fortran_FLAGS "-O3 -free -assume nocc_omp " PARENT_SCOPE )
      set (BUFR_Fortran_FLAGS "-O2 -r8 -fp-model strict -traceback -xSSE2 -O3 -axCORE-AVX2 ${OMPFLAG} " PARENT_SCOPE )
      set (BUFR_C_FLAGS "-g -traceback -DUNDERSCORE -O3 -axCORE-AVX2 -DDYNAMIC_ALLOCATION -DNFILES=32 -DMAXCD=250 -DMAXNC=600 -DMXNAF=3" PARENT_SCOPE )
    endif() 

    #Common release/production flags
    set(WRFLIB_Fortran_FLAGS "-DPOUND_FOR_STRINGIFY -O3 -fp-model source -assume byterecl -convert big_endian -g -traceback -D_REAL8_ ${MPI_Fortran_COMPILE_FLAGS}" PARENT_SCOPE)
    set(WRFLIB_C_FLAGS "-I. -DFortranByte=char -DFortranInt=int -DFortranLlong='long long'  -O3  -Dfunder" PARENT_SCOPE )
    set(GSI_CFLAGS "-I. -DFortranByte=char -DFortranInt=int -DFortranLlong='long long'  -O3  -Dfunder" PARENT_SCOPE )
    set (BUFR_Fortran_PP_FLAGS " -P -traditional-cpp -C  " PARENT_SCOPE )
    set (CRTM_Fortran_FLAGS " -O3 -convert big_endian -free -assume byterecl -fp-model source -traceback " PARENT_SCOPE )
    set (NEMSIO_Fortran_FLAGS " -O2 -convert big_endian -free -assume byterecl -xSSE2 -fp-model strict -traceback  -g ${MKL_FLAG} ${OMPFLAG} " PARENT_SCOPE )
    set (SFCIO_Fortran_FLAGS "  -O2 -convert big_endian -free -assume byterecl  -xSSE2  -fp-model strict -traceback  -g ${MKL_FLAG} ${OMPFLAG} " PARENT_SCOPE )
    set (SIGIO_Fortran_FLAGS "  -O2 -convert big_endian -free -assume byterecl  -xSSE2  -fp-model strict -traceback  -g ${MKL_FLAG} ${OMPFLAG} " PARENT_SCOPE )
    set (SP_Fortran_FLAGS " -O2 -ip -fp-model strict -assume byterecl -convert big_endian -fpp -i${intsize} -r8 -convert big_endian -assume byterecl -DLINUX  ${OMPFLAG} " PARENT_SCOPE )
    set (SP_F77_FLAGS " -DLINUX -O2 -ip -fp-model strict -assume byterecl -convert big_endian -fpp -i${intsize} -r8 -convert big_endian -assume byterecl -DLINUX ${OMPFLAG} " PARENT_SCOPE )
    set (W3EMC_Fortran_FLAGS " -O3 -auto -assume nocc_omp -i${intsize} -r8 -convert big_endian -assume byterecl -fp-model strict ${OMPFLAG} " PARENT_SCOPE )
    set (W3NCO_Fortran_FLAGS " -O3 -auto -assume nocc_omp -i${intsize} -r8 -convert big_endian -assume byterecl -fp-model strict ${OMPFLAG} " PARENT_SCOPE )
    set (W3NCO_C_FLAGS "-O0 -DUNDERSCORE -DLINUX -D__linux__ " PARENT_SCOPE )
    set (NDATE_Fortran_FLAGS "-fp-model source -ftz -assume byterecl -convert big_endian -heap-arrays  -DCOMMCODE -DLINUX -DUPPLITTLEENDIAN -O3 -Wl,-noinhibit-exec" PARENT_SCOPE )
    set( NCDIAG_Fortran_FLAGS "-free -assume byterecl -convert big_endian" PARENT_SCOPE )
  else( ) #DEBUG flags
    message("Building DEBUG version of GSI")
    set( debug_suffix "_DBG" PARENT_SCOPE )
    set(EXTRA_LINKER_FLAGS ${OMPFLAG} CACHE INTERNAL "linker flag")
    if(( HOST-Tide ) OR ( HOST-Gyre ))
      set(GSI_Fortran_FLAGS "-DPOUND_FOR_STRINGIFY -O0 -fp-model source -convert big_endian -assume byterecl -implicitnone -mcmodel medium -shared-intel -g -traceback -debug -ftrapuv -check all,noarg_temp_created -fp-stack-check -fstack-protector -warn all,nointerfaces -convert big_endian -implicitnone -D_REAL8_ ${OMPFLAG} ${MPI_Fortran_COMPILE_FLAGS}" PARENT_SCOPE)
      set(ENKF_Fortran_FLAGS "-g -O0 -fp-model source -convert big_endian -assume byterecl -implicitnone -warn all -traceback -debug all -check all,noarg_temp_created -implicitnone  -DGFS -D_REAL8_ ${OMPFLAG}" PARENT_SCOPE)
      set(UTIL_Fortran_FLAGS "-g -O0 -fp-model source -convert big_endian -assume byterecl -implicitnone -warn all -traceback -debug all -check all,noarg_temp_created -implicitnone  -DWRF -D_REAL8_ ${OMPFLAG}" PARENT_SCOPE)
      set (BACIO_Fortran_FLAGS "-g -free -assume nocc_omp " PARENT_SCOPE )
    else()
      set(GSI_Fortran_FLAGS "-DPOUND_FOR_STRINGIFY -O0 -fp-model strict -convert big_endian -assume byterecl -implicitnone -g -traceback -debug -ftrapuv -check all,noarg_temp_created  -fp-stack-check  -fstack-protector -warn all,nointerfaces -D_REAL8_ ${OMPFLAG} ${MPI_Fortran_COMPILE_FLAGS}" PARENT_SCOPE)
      set(ENKF_Fortran_FLAGS "-O0 -xHOST -warn all -implicitnone -traceback -g -debug full -fp-model strict -convert big_endian -D_REAL8_ ${OMPFLAG}" PARENT_SCOPE)
      set(UTIL_Fortran_FLAGS "-O0 -xHOST -warn all -implicitnone -traceback -g -debug full -fp-model strict -convert big_endian -D_REAL8_ ${OMPFLAG}" PARENT_SCOPE)
      set (BACIO_Fortran_FLAGS "-g -free -assume nocc_omp " PARENT_SCOPE )
    endif() 
    #Common debug flags
    set(GSI_CFLAGS "-I. -DFortranByte=char -DFortranInt=int -DFortranLlong='long long'  -g  -Dfunder" PARENT_SCOPE )
    set (BUFR_Fortran_PP_FLAGS " -P -traditional-cpp -C  " PARENT_SCOPE )
    set (BUFR_Fortran_FLAGS "-g -r8 -fp-model strict -traceback -xSSE2 -axCORE-AVX2 ${OMPFLAG} " PARENT_SCOPE )
    set (BUFR_C_FLAGS "-g -traceback -DUNDERSCORE  -axCORE-AVX2 -DDYNAMIC_ALLOCATION -DNFILES=32 -DMAXCD=250 -DMAXNC=600 -DMXNAF=3" PARENT_SCOPE )
    set (CRTM_Fortran_FLAGS " -convert big_endian -free -assume byterecl  -xSSE2 -fp-model strict -traceback -g ${OMPFLAG} " PARENT_SCOPE )
#   set(CRTM_Fortran_FLAGS "-O0 -fp-model strict -free -convert big_endian -assume byterecl -implicitnone -mcmodel medium -shared-intel -g -traceback -debug -ftrapuv -check all,noarg_temp_created -fp-stack-check -fstack-protector -warn all,nointerfaces ${OMPFLAG} ${MPI_Fortran_COMPILE_FLAGS}" PARENT_SCOPE)
    set (SFCIO_Fortran_FLAGS "  -convert big_endian -free -assume byterecl  -xSSE2  -fp-model strict -traceback  -g ${MKL_FLAG} ${OMPFLAG} " PARENT_SCOPE )
    set (SIGIO_Fortran_FLAGS "  -convert big_endian -free -assume byterecl  -xSSE2  -fp-model strict -traceback  -g ${MKL_FLAG} ${OMPFLAG} " PARENT_SCOPE )
    set (SP_Fortran_FLAGS " -g -ip -fp-model strict -assume byterecl -fpp -i${intsize} -r8 -convert big_endian  -DLINUX  ${OMPFLAG} " PARENT_SCOPE )
    set (SP_F77_FLAGS " -g -ip -fp-model strict -assume byterecl -convert big_endian -fpp -i${intsize} -r8 -DLINUX ${OMPFLAG} " PARENT_SCOPE )
    set (W3EMC_Fortran_FLAGS " -g -auto -assume nocc_omp -i${intsize} -r8 -convert big_endian -assume byterecl -fp-model strict ${OMPFLAG} " PARENT_SCOPE )
    set (NEMSIO_Fortran_FLAGS " -convert big_endian -free -assume byterecl -xSSE2 -fp-model strict -traceback  -g ${MKL_FLAG} ${OMPFLAG} " PARENT_SCOPE )
    set (W3NCO_Fortran_FLAGS " -g -auto -assume nocc_omp -i${intsize} -r8 -convert big_endian -assume byterecl -fp-model strict ${OMPFLAG} " PARENT_SCOPE )
    set (W3NCO_C_FLAGS "-O0 -g -DUNDERSCORE -DLINUX -D__linux__ " PARENT_SCOPE )
    set (NDATE_Fortran_FLAGS "-fp-model source -ftz -assume byterecl -convert big_endian -heap-arrays  -DCOMMCODE -DLINUX -DUPPLITTLEENDIAN -O3 -Wl,-noinhibit-exec" PARENT_SCOPE)
    set( NCDIAG_Fortran_FLAGS "-free -assume byterecl -convert big_endian" PARENT_SCOPE )
    set(WRFLIB_Fortran_FLAGS "-DPOUND_FOR_STRINGIFY -O1 -g -fp-model source -assume byterecl -convert big_endian -g -traceback -D_REAL8_ ${MPI_Fortran_COMPILE_FLAGS}" PARENT_SCOPE)
    set(WRFLIB_C_FLAGS "-I. -DFortranByte=char -DFortranInt=int -DFortranLlong='long long'  -g  -Dfunder" PARENT_SCOPE )
  endif()
endfunction()

function (setGNU)
  message("Setting GNU Compiler Flags")
  set(GSI_Fortran_FLAGS " -O3 -fconvert=big-endian -ffast-math -fno-second-underscore -frecord-marker=4 -funroll-loops -ggdb -static -Wall -fno-range-check -D_REAL8_ -fopenmp -ffree-line-length-0" PARENT_SCOPE)
  set(EXTRA_LINKER_FLAGS "-lgomp" PARENT_SCOPE)
  set(GSI_CFLAGS "-I. -DFortranByte=char -DFortranInt=int -DFortranLlong='long long'  -g  -Dfunder" PARENT_SCOPE )
  set(ENKF_Fortran_FLAGS " -O3 -fconvert=big-endian -ffree-line-length-0 -ffast-math -fno-second-underscore -frecord-marker=4 -funroll-loops -ggdb -static -Wall -fno-range-check -DGFS -D_REAL8_ -fopenmp" PARENT_SCOPE)
  set(UTIL_Fortran_FLAGS " -O3 -fconvert=big-endian -ffree-line-length-0 -ffast-math -fno-second-underscore -frecord-marker=4 -funroll-loops -ggdb -static -Wall -fno-range-check -DWRF -D_REAL8_ -fopenmp" PARENT_SCOPE)
  set(BUFR_Fortran_FLAGS " -O3 -fconvert=big-endian -ffast-math -fno-second-underscore -frecord-marker=4 -funroll-loops -ggdb -static -Wall -fno-range-check  -D_REAL8_ -fopenmp" PARENT_SCOPE)
  set(BUFR_Fortran_PP_FLAGS " -P " PARENT_SCOPE)
  set(BUFR_C_FLAGS " -O3 -g -DUNDERSCORE  -DDYNAMIC_ALLOCATION -DNFILES=32 -DMAXCD=250 -DMAXNC=600 -DMXNAF=3" PARENT_SCOPE )
  set(BACIO_Fortran_FLAGS " -O3 -fconvert=big-endian -ffree-form -ffast-math -fno-second-underscore -frecord-marker=4 -funroll-loops -ggdb -static -Wall -fno-range-check  -D_REAL8_ -fopenmp" PARENT_SCOPE)
  set(CRTM_Fortran_FLAGS " -g -std=f2003 -fdollar-ok -O3 -fconvert=big-endian -ffree-form -fno-second-underscore -frecord-marker=4 -funroll-loops -static -Wall " PARENT_SCOPE)
  set(NEMSIO_Fortran_FLAGS " -O3 -fconvert=big-endian -ffast-math -fno-second-underscore -frecord-marker=4 -funroll-loops -ggdb -static -Wall -fno-range-check  -D_REAL8_ -fopenmp" PARENT_SCOPE)
  set(SIGIO_Fortran_FLAGS " -O3 -fconvert=big-endian -ffree-form -ffast-math -fno-second-underscore -frecord-marker=4 -funroll-loops -ggdb -static -Wall -fno-range-check  -D_REAL8_ -fopenmp" PARENT_SCOPE)
  set(SFCIO_Fortran_FLAGS " -O3 -ffree-form -fconvert=big-endian -ffast-math -fno-second-underscore -frecord-marker=4 -funroll-loops -ggdb -static -Wall -fno-range-check  -D_REAL8_ -fopenmp" PARENT_SCOPE)
  set(SP_Fortran_FLAGS " -O3 -fconvert=big-endian -ffast-math -fno-second-underscore -frecord-marker=4 -funroll-loops -ggdb -static -Wall -fno-range-check  -D_REAL8_ -fopenmp -DLINUX" PARENT_SCOPE)
  set(SP_F77_FLAGS " -O3 -fconvert=big-endian -ffast-math -fno-second-underscore -frecord-marker=4 -funroll-loops -ggdb -static -Wall -fno-range-check  -D_REAL8_ -fopenmp -DLINUX" PARENT_SCOPE)
  set(W3EMC_Fortran_FLAGS " -O3 -fconvert=big-endian -ffast-math -fno-second-underscore -frecord-marker=4 -funroll-loops -ggdb -static -Wall -fno-range-check  -D_REAL8_ -fopenmp" PARENT_SCOPE)
  set(W3NCO_Fortran_FLAGS " -O3 -fconvert=big-endian -ffixed-form -ffast-math -fno-second-underscore -frecord-marker=4 -funroll-loops -ggdb -static -Wall -fno-range-check  -D_REAL8_ " PARENT_SCOPE)
  set(W3NCO_C_FLAGS " -O3 -fconvert=big-endian -ffast-math -fno-second-underscore -frecord-marker=4 -funroll-loops -ggdb -static -Wall -fno-range-check  -D_REAL8_ -fopenmp" PARENT_SCOPE)
  set(WRFLIB_Fortran_FLAGS " -O3 -fconvert=big-endian -ffast-math -fno-second-underscore -frecord-marker=4 -funroll-loops -ggdb -static -Wall -fno-range-check -D_REAL8_ -fopenmp -ffree-line-length-0" PARENT_SCOPE)
  set( NCDIAG_Fortran_FLAGS "-ffree-line-length-none" PARENT_SCOPE )
  set( NDATE_Fortran_FLAGS "-fconvert=big-endian -DCOMMCODE -DLINUX -DUPPLITTLEENDIAN -O3 -Wl,-noinhibit-exec" PARENT_SCOPE)
endfunction()

function (setPGI)
  message("Setting PGI Compiler Flags")
  set(CMAKE_Fortran_FLAGS_RELEASE "")  
  set(Fortran_FLAGS "" PARENT_SCOPE)
  set(GSI_Fortran_FLAGS "-Minform=inform -O1 -byteswapio  -D_REAL8_ -mp -Mfree" PARENT_SCOPE)
  set(GSI_CFLAGS "-I. -DFortranByte=char -DFortranInt=int -DFortranLlong='long long'  -g  -Dfunder" PARENT_SCOPE )
  set(ENKF_Fortran_FLAGS " -O3 -byteswapio -fast -DGFS -D_REAL8_ -mp" PARENT_SCOPE)
  set(UTIL_Fortran_FLAGS " -O3 -byteswapio -fast -DWRF -D_REAL8_ -mp" PARENT_SCOPE)

  set(BUFR_Fortran_FLAGS "-O1 -byteswapio  -D_REAL8_ -mp" PARENT_SCOPE)
  set(BUFR_Fortran_PP_FLAGS " -P " PARENT_SCOPE)
  set(BUFR_C_FLAGS " -g -DUNDERSCORE  -DDYNAMIC_ALLOCATION -DNFILES=32 -DMAXCD=250 -DMAXNC=600 -DMXNAF=3" PARENT_SCOPE )

  set(BACIO_C_INCLUDES " -I/usr/include/malloc" PARENT_SCOPE)
  set(BACIO_Fortran_FLAGS " -O3 -byteswapio -fast -D_REAL8_ -mp -Mfree" PARENT_SCOPE)
  set(CRTM_Fortran_FLAGS " -O1 -byteswapio -module ../../include -Mfree " PARENT_SCOPE)
  set(NEMSIO_Fortran_FLAGS " -O1 -byteswapio -D_REAL8_ -mp" PARENT_SCOPE)
  set(SIGIO_Fortran_FLAGS " -O3 -Mfree -byteswapio -fast -D_REAL8_ -mp" PARENT_SCOPE)
  set(SFCIO_Fortran_FLAGS " -O3 -byteswapio  -Mfree -fast -D_REAL8_ -mp" PARENT_SCOPE)
  set(SP_Fortran_FLAGS " -O1 -byteswapio  -D_REAL8_ -mp" PARENT_SCOPE)
  set(SP_F77_FLAGS "-DLINUX -O1 -byteswapio  -D_REAL8_ -mp" PARENT_SCOPE)
  set(W3EMC_Fortran_FLAGS " -O1 -byteswapio  -D_REAL8_ " PARENT_SCOPE)
  set(W3NCO_Fortran_FLAGS " -O1 -byteswapio  -D_REAL8_ " PARENT_SCOPE)
  set(W3NCO_C_FLAGS " -O1 -D_REAL8_ -mp" PARENT_SCOPE)
  set(WRFLIB_Fortran_FLAGS "-Minform=inform -O1 -byteswapio  -D_REAL8_ -mp -Mfree" PARENT_SCOPE)
endfunction()
