# This module looks for environment variables detailing where BACIO lib is
# If variables are not set, BACIO will be built from external source 
include(ExternalProject)
if(NOT BUILD_BACIO )
  if(DEFINED ENV{BACIO_LIB4})
    set(BACIO_LIBRARY $ENV{BACIO_LIB4} )
    if( CORE_LIBRARIES )
      list( APPEND CORE_LIBRARIES ${BACIO_LIBRARY} )
    else()
      set( CORE_LIBRARIES ${BACIO_LIBRARY} )
    endif()
  endif()
else()
  set(CMAKE_INSTALL_PREFIX ${PROJECT_BINARY_DIR})
  ExternalProject_Add(NCEPLIBS-bacio 
    CMAKE_ARGS
      -DCMAKE_INSTALL_PREFIX=${CMAKE_INSTALL_PREFIX}
    SOURCE_DIR ${PROJECT_SOURCE_DIR}/libsrc/bacio 
    INSTALL_DIR ${CMAKE_INSTALL_PREFIX}
    BUILD_COMMAND make
    INSTALL_COMMAND make install
  )
  execute_process(COMMAND grep "set(VERSION" CMakeLists.txt WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}/libsrc/bacio OUTPUT_VARIABLE LIBVERSION)
  string(REPLACE "set(VERSION " "" LIBVERSION ${LIBVERSION})
  string(REPLACE ")" "" LIBVERSION ${LIBVERSION})
  string(REPLACE "\n" "" LIBVERSION ${LIBVERSION})
  message("bacio version is ${LIBVERSION}")
  set( BACIO_LIBRARY ${PROJECT_BINARY_DIR}/lib/libbacio_${LIBVERSION}_4.a )
  if( CORE_BUILT )
      list( APPEND CORE_BUILT ${BACIO_LIBRARY} )
      list( APPEND EXT_BUILT NCEPLIBS-bacio )
  else()
      set( CORE_BUILT ${BACIO_LIBRARY} )
      set( EXT_BUILT NCEPLIBS-bacio )
  endif()
endif( )

set( BACIO_LIBRARY_PATH ${BACIO_LIBRARY} CACHE STRING "BACIO Library Location" )

