function (setGNU)
  set(COMPILER_TYPE "gnu" CACHE INTERNAL "Compiler brand")
  message("Setting GNU Compiler Flags")
  if( (BUILD_RELEASE) OR (BUILD_PRODUCTION) )
    set(GSI_Fortran_FLAGS " -O3 -fconvert=big-endian -ffast-math -fno-second-underscore -frecord-marker=4 -funroll-loops -ggdb -static -Wall -fno-range-check -D_REAL8_ ${GSDCLOUDOPT} -fopenmp -ffree-line-length-0" CACHE INTERNAL "")
    set(EXTRA_LINKER_FLAGS "-lgomp -lnetcdf -lnetcdff" CACHE INTERNAL "")
    set(GSI_CFLAGS "-I. -DFortranByte=char -DFortranInt=int -DFortranLlong='long long'  -g  -Dfunder" CACHE INTERNAL "" )
    set(ENKF_Fortran_FLAGS " -O3 -fconvert=big-endian -ffree-line-length-0 -ffast-math -fno-second-underscore -frecord-marker=4 -funroll-loops -ggdb -static -Wall -fno-range-check -DGFS -D_REAL8_ -fopenmp" CACHE INTERNAL "")
    set(UTIL_Fortran_FLAGS " -O3 -fconvert=big-endian -ffree-line-length-0 -ffast-math -fno-second-underscore -frecord-marker=4 -funroll-loops -ggdb -static -Wall -fno-range-check -DWRF -D_REAL8_ -fopenmp" CACHE INTERNAL "")
    set(UTIL_COM_Fortran_FLAGS " -O3 -fconvert=big-endian -ffree-line-length-0 -ffast-math -fno-second-underscore -frecord-marker=4 -funroll-loops -ggdb -static -Wall -fno-range-check" CACHE INTERNAL "")
    set(BUFR_Fortran_FLAGS " -O3 -fconvert=big-endian -ffast-math -fno-second-underscore -frecord-marker=4 -funroll-loops -ggdb -static -Wall -fno-range-check -fdefault-real-8 -D_REAL8_ -fopenmp" CACHE INTERNAL "")
    set(BUFR_Fortran_PP_FLAGS " -P " CACHE INTERNAL "")
    set(BUFR_C_FLAGS " -O3 -g -DUNDERSCORE  -DDYNAMIC_ALLOCATION -DNFILES=32 -DMAXCD=250 -DMAXNC=600 -DMXNAF=3" CACHE INTERNAL "" )
    set(BACIO_Fortran_FLAGS " -O3 -fconvert=big-endian -ffree-form -ffast-math -fno-second-underscore -frecord-marker=4 -funroll-loops -ggdb -static -Wall -fno-range-check  -D_REAL8_ -fopenmp" CACHE INTERNAL "")
    set(CRTM_Fortran_FLAGS " -g -std=f2003 -fdollar-ok -O3 -fconvert=big-endian -ffree-form -fno-second-underscore -frecord-marker=4 -funroll-loops -static -Wall " CACHE INTERNAL "")
    set(NEMSIO_Fortran_FLAGS " -O3 -fconvert=big-endian -ffast-math -fno-second-underscore -frecord-marker=4 -funroll-loops -ggdb -static -Wall -fno-range-check  -D_REAL8_ -fopenmp" CACHE INTERNAL "")
    set(SIGIO_Fortran_FLAGS " -O3 -fconvert=big-endian -ffree-form -ffast-math -fno-second-underscore -frecord-marker=4 -funroll-loops -ggdb -static -Wall -fno-range-check  -D_REAL8_ -fopenmp" CACHE INTERNAL "")
    set(SFCIO_Fortran_FLAGS " -O3 -ffree-form -fconvert=big-endian -ffast-math -fno-second-underscore -frecord-marker=4 -funroll-loops -ggdb -static -Wall -fno-range-check  -D_REAL8_ -fopenmp" CACHE INTERNAL "")
    set(SP_Fortran_FLAGS " -O3 -fconvert=big-endian -ffast-math -fno-second-underscore -frecord-marker=4 -funroll-loops -ggdb -static -Wall -fno-range-check -fdefault-real-8 -D_REAL8_ -fopenmp -DLINUX" CACHE INTERNAL "")
    set(SP_Fortran_4_FLAGS " -O3 -fconvert=big-endian -ffast-math -fno-second-underscore -frecord-marker=4 -funroll-loops -ggdb -static -Wall -fno-range-check -fopenmp -DLINUX" CACHE INTERNAL "")
    set(SP_F77_FLAGS " -O3 -fconvert=big-endian -ffast-math -fno-second-underscore -frecord-marker=4 -funroll-loops -ggdb -static -Wall -fno-range-check -fdefault-real-8 -D_REAL8_ -fopenmp -DLINUX" CACHE INTERNAL "")
    set(SP_F77_4_FLAGS " -O3 -fconvert=big-endian -ffast-math -fno-second-underscore -frecord-marker=4 -funroll-loops -ggdb -static -Wall -fno-range-check -fopenmp -DLINUX" CACHE INTERNAL "")
    set(W3EMC_Fortran_FLAGS " -O3 -fconvert=big-endian -ffast-math -fno-second-underscore -frecord-marker=4 -funroll-loops -ggdb -static -Wall -fno-range-check -fdefault-real-8 -D_REAL8_ -fopenmp" CACHE INTERNAL "")
    set(W3EMC_4_Fortran_FLAGS " -O3 -fconvert=big-endian -ffixed-form -ffast-math -fno-second-underscore -frecord-marker=4 -funroll-loops -ggdb -static -Wall -fno-range-check  " CACHE INTERNAL "")
    set(W3NCO_Fortran_FLAGS " -O3 -fconvert=big-endian -ffixed-form -ffast-math -fno-second-underscore -frecord-marker=4 -funroll-loops -ggdb -static -Wall -fno-range-check -fdefault-real-8 -D_REAL8_ " CACHE INTERNAL "")
    set(W3NCO_4_Fortran_FLAGS " -O3 -fconvert=big-endian -ffixed-form -ffast-math -fno-second-underscore -frecord-marker=4 -funroll-loops -ggdb -static -Wall -fno-range-check  " CACHE INTERNAL "")
    set(W3NCO_C_FLAGS " -O3 -fconvert=big-endian -ffast-math -fno-second-underscore -frecord-marker=4 -funroll-loops -ggdb -static -Wall -fno-range-check  -D_REAL8_ -fopenmp" CACHE INTERNAL "")
    set(WRFLIB_Fortran_FLAGS " -O3 -fconvert=big-endian -ffast-math -fno-second-underscore -frecord-marker=4 -funroll-loops -ggdb -static -Wall -fno-range-check -D_REAL8_ -fopenmp -ffree-line-length-0" CACHE INTERNAL "")
    set(NCDIAG_Fortran_FLAGS "-ffree-line-length-none" CACHE INTERNAL "")
    set(NDATE_Fortran_FLAGS "-fconvert=big-endian -DCOMMCODE -DLINUX -DUPPLITTLEENDIAN -O3 -Wl,-noinhibit-exec" CACHE INTERNAL "")
    set(GSDCLOUD_Fortran_FLAGS "-O3 -fconvert=big-endian" CACHE INTERNAL "")
  else( ) #DEBUG
    set(GSI_Fortran_FLAGS " -g -fbacktrace -fconvert=big-endian -ffast-math -fno-second-underscore -frecord-marker=4 -funroll-loops -g -ggdb -static -Wall -fno-range-check -D_REAL8_ ${GSDCLOUDOPT} -fopenmp -ffree-line-length-0" CACHE INTERNAL "")
    set(EXTRA_LINKER_FLAGS "-lgomp -lnetcdf -lnetcdff" CACHE INTERNAL "")
    set(GSI_CFLAGS "-I. -DFortranByte=char -DFortranInt=int -DFortranLlong='long long'  -g -fbacktrace  -Dfunder" CACHE INTERNAL "" )
    set(ENKF_Fortran_FLAGS " -g -fbacktrace -fconvert=big-endian -ffree-line-length-0 -ffast-math -fno-second-underscore -frecord-marker=4 -funroll-loops -g -ggdb -static -Wall -fno-range-check -DGFS -D_REAL8_ -fopenmp" CACHE INTERNAL "")
    set(UTIL_Fortran_FLAGS " -g -fbacktrace -fconvert=big-endian -ffree-line-length-0 -ffast-math -fno-second-underscore -frecord-marker=4 -funroll-loops -g -ggdb -static -Wall -fno-range-check -DWRF -D_REAL8_ -fopenmp" CACHE INTERNAL "")
    set(UTIL_COM_Fortran_FLAGS " -g -fbacktrace -fconvert=big-endian -ffree-line-length-0 -ffast-math -fno-second-underscore -frecord-marker=4 -funroll-loops -g -ggdb -static -Wall -fno-range-check" CACHE INTERNAL "")
    set(BUFR_Fortran_FLAGS " -g -fbacktrace -fconvert=big-endian -ffast-math -fno-second-underscore -frecord-marker=4 -funroll-loops -g -ggdb -static -Wall -fno-range-check -fdefault-real-8 -D_REAL8_ -fopenmp" CACHE INTERNAL "")
    set(BUFR_Fortran_PP_FLAGS " -P " CACHE INTERNAL "")
    set(BUFR_C_FLAGS " -g -fbacktrace -g -fbacktrace -DUNDERSCORE  -DDYNAMIC_ALLOCATION -DNFILES=32 -DMAXCD=250 -DMAXNC=600 -DMXNAF=3" CACHE INTERNAL "" )
    set(BACIO_Fortran_FLAGS " -g -fbacktrace -fconvert=big-endian -ffree-form -ffast-math -fno-second-underscore -frecord-marker=4 -funroll-loops -g -ggdb -static -Wall -fno-range-check  -D_REAL8_ -fopenmp" CACHE INTERNAL "")
    set(CRTM_Fortran_FLAGS " -g -fbacktrace -std=f2003 -fdollar-ok -g -fbacktrace -fconvert=big-endian -ffree-form -fno-second-underscore -frecord-marker=4 -funroll-loops -static -Wall " CACHE INTERNAL "")
    set(NEMSIO_Fortran_FLAGS " -g -fbacktrace -fconvert=big-endian -ffast-math -fno-second-underscore -frecord-marker=4 -funroll-loops -g -ggdb -static -Wall -fno-range-check  -D_REAL8_ -fopenmp" CACHE INTERNAL "")
    set(SIGIO_Fortran_FLAGS " -g -fbacktrace -fconvert=big-endian -ffree-form -ffast-math -fno-second-underscore -frecord-marker=4 -funroll-loops -g -ggdb -static -Wall -fno-range-check  -D_REAL8_ -fopenmp" CACHE INTERNAL "")
    set(SFCIO_Fortran_FLAGS " -g -fbacktrace -ffree-form -fconvert=big-endian -ffast-math -fno-second-underscore -frecord-marker=4 -funroll-loops -g -ggdb -static -Wall -fno-range-check  -D_REAL8_ -fopenmp" CACHE INTERNAL "")
    set(SP_Fortran_FLAGS " -g -fbacktrace -fconvert=big-endian -ffast-math -fno-second-underscore -frecord-marker=4 -funroll-loops -g -ggdb -static -Wall -fno-range-check -fdefault-real-8 -D_REAL8_ -fopenmp -DLINUX" CACHE INTERNAL "")
    set(SP_Fortran_4_FLAGS " -g -fbacktrace -fconvert=big-endian -ffast-math -fno-second-underscore -frecord-marker=4 -funroll-loops -g -ggdb -static -Wall -fno-range-check -fopenmp -DLINUX" CACHE INTERNAL "")
    set(SP_F77_FLAGS " -g -fbacktrace -fconvert=big-endian -ffast-math -fno-second-underscore -frecord-marker=4 -funroll-loops -g -ggdb -static -Wall -fno-range-check -fdefault-real-8 -D_REAL8_ -fopenmp -DLINUX" CACHE INTERNAL "")
    set(SP_F77_4_FLAGS " -g -fbacktrace -fconvert=big-endian -ffast-math -fno-second-underscore -frecord-marker=4 -funroll-loops -g -ggdb -static -Wall -fno-range-check -fopenmp -DLINUX" CACHE INTERNAL "")
    set(W3EMC_Fortran_FLAGS " -g -fbacktrace -fconvert=big-endian -ffast-math -fno-second-underscore -frecord-marker=4 -funroll-loops -g -ggdb -static -Wall -fno-range-check -fdefault-real-8 -D_REAL8_ -fopenmp" CACHE INTERNAL "")
    set(W3EMC_4_Fortran_FLAGS " -g -fbacktrace -fconvert=big-endian -ffixed-form -ffast-math -fno-second-underscore -frecord-marker=4 -funroll-loops -g -ggdb -static -Wall -fno-range-check  " CACHE INTERNAL "")
    set(W3NCO_Fortran_FLAGS " -g -fbacktrace -fconvert=big-endian -ffixed-form -ffast-math -fno-second-underscore -frecord-marker=4 -funroll-loops -g -ggdb -static -Wall -fno-range-check -fdefault-real-8 -D_REAL8_ " CACHE INTERNAL "")
    set(W3NCO_4_Fortran_FLAGS " -g -fbacktrace -fconvert=big-endian -ffixed-form -ffast-math -fno-second-underscore -frecord-marker=4 -funroll-loops -g -ggdb -static -Wall -fno-range-check  " CACHE INTERNAL "")
    set(W3NCO_C_FLAGS " -g -fbacktrace -fconvert=big-endian -ffast-math -fno-second-underscore -frecord-marker=4 -funroll-loops -g -ggdb -static -Wall -fno-range-check  -D_REAL8_ -fopenmp" CACHE INTERNAL "")
    set(WRFLIB_Fortran_FLAGS " -g -fbacktrace -fconvert=big-endian -ffast-math -fno-second-underscore -frecord-marker=4 -funroll-loops -g -ggdb -static -Wall -fno-range-check -D_REAL8_ -fopenmp -ffree-line-length-0" CACHE INTERNAL "")
    set(NCDIAG_Fortran_FLAGS "-ffree-line-length-none" CACHE INTERNAL "")
    set(NDATE_Fortran_FLAGS "-fconvert=big-endian -DCOMMCODE -DLINUX -DUPPLITTLEENDIAN -g -fbacktrace -Wl,-noinhibit-exec" CACHE INTERNAL "")
    set(GSDCLOUD_Fortran_FLAGS "-O3 -fconvert=big-endian" CACHE INTERNAL "")
  endif( (BUILD_RELEASE) OR (BUILD_PRODUCTION) )
endfunction()
  
