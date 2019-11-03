cmake_minimum_required(VERSION 2.6)
  include_directories( ${NETCDF_INCLUDES}  ${GRIB2_INCLUDE_PATH}  ${MPI_Fortran_INCLUDE_PATH} )

# Add stuff for NSSL mosaic executable
  file(GLOB LOCAL_NSSL_SRC ${CMAKE_CURRENT_SOURCE_DIR}/*.f90)
  list(REMOVE_ITEM LOCAL_NSSL_SRC ${CMAKE_CURRENT_SOURCE_DIR}/process_CAPS_mosaic.f90)
  list(REMOVE_ITEM LOCAL_NSSL_SRC ${CMAKE_CURRENT_SOURCE_DIR}/write_CAPSbufr_ref.f90)
  list(REMOVE_ITEM LOCAL_NSSL_SRC ${CMAKE_CURRENT_SOURCE_DIR}/netCDFsub_geo.f90)
  list(REMOVE_ITEM LOCAL_NSSL_SRC ${CMAKE_CURRENT_SOURCE_DIR}/netCDFsub.f90)
  list(REMOVE_ITEM LOCAL_NSSL_SRC ${CMAKE_CURRENT_SOURCE_DIR}/read_grib2.f90)
  list(REMOVE_ITEM LOCAL_NSSL_SRC ${CMAKE_CURRENT_SOURCE_DIR}/read_grib2_mod.f90)
  list(REMOVE_ITEM LOCAL_NSSL_SRC ${CMAKE_CURRENT_SOURCE_DIR}/read_ncep_binary.f90)
  list(REMOVE_ITEM LOCAL_NSSL_SRC ${CMAKE_CURRENT_SOURCE_DIR}/read_NSSL_mosaic.f90)
  set_source_files_properties( ${LOCAL_NSSL_SRC} PROPERTIES COMPILE_FLAGS ${UTIL_COM_Fortran_FLAGS} )

  add_executable(process_NSSL_mosaic.x ${LOCAL_NSSL_SRC} )
  set_target_properties( process_NSSL_mosaic.x PROPERTIES COMPILE_FLAGS ${UTIL_COM_Fortran_FLAGS} )
  target_link_libraries( process_NSSL_mosaic.x ${GSILIB} ${BUFR_LIBRARY} ${NETCDF_LIBRARIES} ${MPI_Fortran_LIBRARIES} )

# Add stuff for CAPS mosaic executable
  file(GLOB LOCAL_CAPS_SRC ${CMAKE_CURRENT_SOURCE_DIR}/*.f90)
  list(REMOVE_ITEM LOCAL_CAPS_SRC ${CMAKE_CURRENT_SOURCE_DIR}/process_NSSL_mosaic.f90)
  list(REMOVE_ITEM LOCAL_CAPS_SRC ${CMAKE_CURRENT_SOURCE_DIR}/write_bufr_ref.f90)
  list(REMOVE_ITEM LOCAL_CAPS_SRC ${CMAKE_CURRENT_SOURCE_DIR}/read_NSSL_mosaic.f90)

  set(UTIL_COM_Fortran_FLAGS_OMP "${UTIL_COM_Fortran_FLAGS} -qopenmp")
  add_executable(process_CAPS_mosaic.x ${LOCAL_CAPS_SRC} )
  set_target_properties( process_CAPS_mosaic.x PROPERTIES LINK_FLAGS ${UTIL_COM_Fortran_FLAGS_OMP} )
  target_link_libraries( process_CAPS_mosaic.x ${GSILIB} ${GRIB2_LIBRARY} ${JASPER_LIB} ${PNG_LIB} ${Z_LIB} ${W3EMC_LIBRARY} ${BUFR_LIBRARY} ${BACIO_LIBRARY} ${SP_LIBRARY} ${NETCDF_LIBRARIES} ${MPI_Fortran_LIBRARIES} )
