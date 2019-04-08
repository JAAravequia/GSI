# This module looks for environment variables detailing where BUFR lib is
# If variables are not set, BUFR will be built from external source 
include(ExternalProject)
if((NOT BUILD_BUFR ) AND (DEFINED ENV{BUFR_LIBd}))
    set(BUFR_LIBRARY $ENV{BUFR_LIBd} )
    if( CORE_LIBRARIES )
      list( APPEND CORE_LIBRARIES ${BUFR_LIBRARY} )
    else()
      set( CORE_LIBRARIES ${BUFR_LIBRARY} )
    endif()
else()
  set(CMAKE_INSTALL_PREFIX ${PROJECT_BINARY_DIR})
  ExternalProject_Add(NCEPLIBS-bufr 
    CMAKE_ARGS
      -DCMAKE_INSTALL_PREFIX=${CMAKE_INSTALL_PREFIX}
      -DCMAKE_CXX_COMPILER=${CMAKE_CXX_COMPILER}
      -DCMAKE_C_COMPILER=${CMAKE_C_COMPILER}
      -DCMAKE_Fortran_COMPILER=${CMAKE_Fortran_COMPILER}
      -DGSIBUILD=ON
    SOURCE_DIR ${PROJECT_SOURCE_DIR}/libsrc/bufr 
    INSTALL_DIR ${CMAKE_INSTALL_PREFIX}
    BUILD_COMMAND make
    INSTALL_COMMAND make install
  )
  execute_process(COMMAND grep "set(VERSION" CMakeLists.txt WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}/libsrc/bufr OUTPUT_VARIABLE LIBVERSION)
  string(REPLACE "set(VERSION " "" LIBVERSION ${LIBVERSION})
  string(REPLACE ")" "" LIBVERSION ${LIBVERSION})
  string(REPLACE "\n" "" LIBVERSION ${LIBVERSION})
  message("bufr version is ${LIBVERSION}")
  set( BUFR_LIBRARY ${PROJECT_BINARY_DIR}/lib/libbufr_${LIBVERSION}_d.a )
  if( CORE_BUILT )
      list( APPEND CORE_BUILT ${BUFR_LIBRARY} )
      list( APPEND EXT_BUILT NCEPLIBS-bufr )
  else()
      set( CORE_BUILT ${BUFR_LIBRARY} )
      set( EXT_BUILT NCEPLIBS-bufr )
  endif()
endif( )

set( BUFR_LIBRARY_PATH ${BUFR_LIBRARY} CACHE STRING "BUFR Library Location" )

