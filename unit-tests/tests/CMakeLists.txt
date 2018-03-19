cmake_minimum_required(VERSION 2.8 FATAL_ERROR)

if(CMAKE_Fortran_COMPILER_ID MATCHES GNU)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -g -fprofile-arcs -ftest-coverage")
endif()

#set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -openmp")
set(CMAKE_Fortran_FLAGS ${GSI_Fortran_FLAGS} )
enable_testing()
set(PFUNIT_DIR ${PROJECT_BINARY_DIR} ) 

include_directories( 
      ${PROJECT_SOURCE_DIR}
      ${PFUNIT_DIR}/mod
)
set(UNIT_TEST_LIST "test_read_wrf_nmm;test_adjoint;test_setupps")
# set(UNIT_TEST_LIST "test_read_wrf_nmm;test_read_wrf_mass;test_adjoint")
#set(UNIT_TEST_LIST "test_read_wrf_nmm")
#foreach(_sample ${UNIT_TEST_LIST})
#  message("HEY!!! tests are ${_sample}")
#endforeach()
#foreach(_test test_read_wrf_nmm test_read_wrf_mass test_adjoint)
foreach(_test ${UNIT_TEST_LIST})
    message("HEY!!! tests are ${_sample}")
    file(MAKE_DIRECTORY ${PROJECT_BINARY_DIR}/${_test}/generated)
    file(WRITE ${PROJECT_BINARY_DIR}/generated/${_test}/testSuites.inc "")
    message("working on test ${_test}")
    set(test_dependency ${PROJECT_SOURCE_DIR}/unit-tests/tests/${_test}.pf)
    add_custom_command( OUTPUT ${PROJECT_BINARY_DIR}/generated/${_test}/${_test}.F90
        COMMAND python ${PFUNIT_SRC}/bin/pFUnitParser.py 
          ${PROJECT_SOURCE_DIR}/unit-tests/tests/${_test}.pf ${PROJECT_BINARY_DIR}/generated/${_test}/${_test}.F90
        DEPENDS ${test_dependency})
    file(APPEND ${PROJECT_BINARY_DIR}/generated/${_test}/testSuites.inc "ADD_TEST_SUITE(${_test}_mod_suite)")
    set_source_files_properties(${PFUNIT_SRC}/include/driver.F90 PROPERTIES GENERATED 1)
  add_executable(
    ${_test}.x
    ${PFUNIT_SRC}/include/driver.F90
    ${PROJECT_BINARY_DIR}/generated/${_test}/${_test}.F90)
  set_target_properties(${_test}.x PROPERTIES Fortran_MODULE_DIRECTORY "${PROJECT_BINARY_DIR}/include/wrf")

  add_dependencies(${_test}.x ${GSISHAREDLIB} ${GSILIB} ${GSISHAREDLIB})
  message("HEYYY corelibs are ${CORE_LIBRARIES}|${WRF_LIBRARIES}")
  target_link_libraries(
    ${_test}.x
    ${PFUNIT_DIR}/lib/libpfunit.a 
    ${GSILIB} ${GSISHAREDLIB} ${GSILIB} ${GSISHAREDLIB} 
         ${GSILIB} ${FRAMEPACK} ${FRAMEMODULE}
         ${NETCDF_LIBRARIES_F90} ${NETCDF_LIBRARIES} ${HDF5_Fortran_HL_LIBRARIES} ${MPI_Fortran_LIBRARIES}   
         ${LAPACK_LIBRARIES} -L./ ${EXTRA_LINKER_FLAGS} ${HDF5_LIBRARIES} ${CURL_LIBRARIES} ${CORE_LIBRARIES} 
         ${IOINT_LIB} ${WRFNETCDF_LIB} )
  target_include_directories( ${_test}.x PUBLIC ${PROJECT_BINARY_DIR}/generated/${_test})
  add_test(NAME ${_test} WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/unit-tests/data COMMAND 
       ${PROJECT_BINARY_DIR}/bin/${_test}.x )
endforeach()
#set_tests_properties( test_read_wrf_nmm test_read_wrf_mass test_adjoint PROPERTIES LABELS UNIT )
set_tests_properties( ${UNIT_TEST_LIST} PROPERTIES LABELS UNIT )
add_custom_target(test_unit ${CMAKE_CTEST_COMMAND} -L UNIT)

