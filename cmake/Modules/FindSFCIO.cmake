# This module looks for environment variables detailing where SFCIO lib is
# If variables are not set, SFCIO will be built from external source 
include(ExternalProject)
if((NOT BUILD_SFCIO ) AND (DEFINED ENV{SFCIO_LIB4}))
    set(SFCIO_LIBRARY $ENV{SFCIO_LIB4} )
    set(SFCIOINC $ENV{SFCIO_INC4} )
    if( CORE_LIBRARIES )
      list( APPEND CORE_LIBRARIES ${SFCIO_LIBRARY} )
      list( APPEND CORE_INCS ${SFCIOINC} )
    else()
      set( CORE_LIBRARIES ${SFCIO_LIBRARY} )
      set( CORE_INCS ${SFCIOINC} )
    endif()
else()
  set(CMAKE_INSTALL_PREFIX ${PROJECT_BINARY_DIR})
  ExternalProject_Add(NCEPLIBS-sfcio 
    PREFIX ${PROJECT_BINARY_DIR}/NCEPLIBS-sfcio
    CMAKE_ARGS
      -DCMAKE_INSTALL_PREFIX=${CMAKE_INSTALL_PREFIX}
      -DCMAKE_CXX_COMPILER=${CMAKE_CXX_COMPILER}
      -DCMAKE_C_COMPILER=${CMAKE_C_COMPILER}
      -DCMAKE_Fortran_COMPILER=${CMAKE_Fortran_COMPILER}
    SOURCE_DIR ${PROJECT_SOURCE_DIR}/libsrc/sfcio 
    INSTALL_DIR ${CMAKE_INSTALL_PREFIX}
    BUILD_COMMAND make
    INSTALL_COMMAND make install
  )
  execute_process(COMMAND grep "set(VERSION" CMakeLists.txt WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}/libsrc/sfcio OUTPUT_VARIABLE LIBVERSION)
  string(REPLACE "set(VERSION " "" LIBVERSION ${LIBVERSION})
  string(REPLACE ")" "" LIBVERSION ${LIBVERSION})
  string(REPLACE "\n" "" LIBVERSION ${LIBVERSION})
  message("sfcio version is ${LIBVERSION}")
  set( SFCIO_LIBRARY ${PROJECT_BINARY_DIR}/lib/libsfcio_${LIBVERSION}.a )
  if( CORE_BUILT )
      list( APPEND CORE_BUILT ${SFCIO_LIBRARY} )
      list( APPEND EXT_BUILT NCEPLIBS-sfcio )
  else()
      set( CORE_BUILT ${SFCIO_LIBRARY} )
      set( EXT_BUILT NCEPLIBS-sfcio )
  endif()
endif( )

set( SFCIO_LIBRARY_PATH ${SFCIO_LIBRARY} CACHE STRING "SFCIO Library Location" )
set( SFCIO_INCLUDE_PATH ${SFCIO_LIBRARY} CACHE STRING "SFCIO Include Location" )

