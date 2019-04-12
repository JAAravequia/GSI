# This module looks for environment variables detailing where W3NCO lib is
# If variables are not set, W3NCO will be built from external source 
include(ExternalProject)
if( (NOT BUILD_W3NCO ) AND (DEFINED ENV{W3NCO_LIBd}) )
    set(W3NCO_LIBRARY $ENV{W3NCO_LIBd} )
    set(W3NCO_4_LIBRARY $ENV{W3NCO_LIB4} )
    if( CORE_LIBRARIES )
      list( APPEND CORE_LIBRARIES ${W3NCO_LIBRARY} )
    else()
      set( CORE_LIBRARIES ${W3NCO_LIBRARY} )
    endif()
else()
  set(CMAKE_INSTALL_PREFIX ${PROJECT_BINARY_DIR})
  ExternalProject_Add(NCEPLIBS-w3nco 
    PREFIX ${PROJECT_BINARY_DIR}/NCEPLIBS-w3nco
    CMAKE_ARGS
      -DCMAKE_INSTALL_PREFIX=${CMAKE_INSTALL_PREFIX}
      -DCMAKE_CXX_COMPILER=${CMAKE_CXX_COMPILER}
      -DCMAKE_C_COMPILER=${CMAKE_C_COMPILER}
      -DCMAKE_Fortran_COMPILER=${CMAKE_Fortran_COMPILER}
    SOURCE_DIR ${PROJECT_SOURCE_DIR}/libsrc/w3nco 
    INSTALL_DIR ${CMAKE_INSTALL_PREFIX}
    BUILD_COMMAND make
    INSTALL_COMMAND make install
  )
  execute_process(COMMAND grep "set(VERSION" CMakeLists.txt WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}/libsrc/w3nco OUTPUT_VARIABLE LIBVERSION)
  string(REPLACE "set(VERSION " "" LIBVERSION ${LIBVERSION})
  string(REPLACE ")" "" LIBVERSION ${LIBVERSION})
  string(REPLACE "\n" "" LIBVERSION ${LIBVERSION})
  message("w3nco version is ${LIBVERSION}")
  set( W3NCO_LIBRARY ${PROJECT_BINARY_DIR}/lib/libw3nco_${LIBVERSION}_d.a )
  if( CORE_BUILT )
      list( APPEND CORE_BUILT ${W3NCO_LIBRARY} )
      list( APPEND EXT_BUILT NCEPLIBS-w3nco )
  else()
      set( CORE_BUILT ${W3NCO_LIBRARY} )
      set( EXT_BUILT NCEPLIBS-w3nco )
  endif()
endif( )

set( W3NCO_LIBRARY_PATH ${W3NCO_LIBRARY} CACHE STRING "W3NCO Library Location" )
set( W3NCO_4_LIBRARY_PATH ${W3NCO_LIBRARY} CACHE STRING "W3NCO_4 Library Location" )

