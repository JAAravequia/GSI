# This module defines
#  CORE_INCS
#    List of include file paths for all required modules for GSI
#  CORE_LIBRARIES
#    Full list of libraries required to link GSI executable
include(findHelpers)
if(DEFINED ENV{NEMSIO_VER})
  set(NEMSIO_VER $ENV{NEMSIO_VER})
  STRING(REGEX REPLACE "v" "" NEMSIO_VER ${NEMSIO_VER})
endif()

set( NO_DEFAULT_PATH )
if(NOT BUILD_NEMSIO )
  if(DEFINED ENV{NEMSIO_LIB} )
    set(NEMSIO_LIB $ENV{NEMSIO_LIB} )
    set(NEMSIO_INC $ENV{NEMSIO_INC} )
    message("NEMSIO library ${NEMSIO_LIB} set via Environment variable")
  else()
    findInc( nemsio NEMSIO_VER NEMSIO_INC )
    find_library( NEMSIO_LIB 
    NAMES libnemsio_v${NEMSIO_VER}.a libnemsio.a libNEMSIO.a 
    HINTS 
      $ENV{COREPATH}/lib 
      /usr/local/jcsda/nwprod_gdas_2014/lib	
      ${COREPATH}/nemsio/v${NEMSIO_VER}
      ${COREPATH}/nemsio/v${NEMSIO_VER}/intel
      ${COREPATH}/nemsio/v${NEMSIO_VER}//ips/${COMPILER_VERSION}/impi/${COMPILER_VERSION}
      ${COREPATH}/nemsio/v${NEMSIO_VER}//ips/${COMPILER_VERSION}/smpi/${COMPILER_VERSION}
    PATH_SUFFIXES
        lib
     ${NO_DEFAULT_PATH})
    set( nemsio "nemsio_v${NEMSIO_VER}")
    message("Found NEMSIO library ${NEMSIO_LIB}")
  endif()
endif()
if( NOT NEMSIO_LIB ) # didn't find the library, so build it from source
    message("Could not find NEMSIO library, so building from libsrc")
    if( NOT DEFINED ENV{NEMSIO_SRC} )
        findSrc( "nemsio" NEMSIO_VER NEMSIO_DIR )
        set(NEMSIO_INC  "${CMAKE_BINARY_DIR}/include")
    else()
      set( NEMSIO_DIR "$ENV{NEMSIO_SRC}/libsrc" CACHE STRING "NEMSIO Source Location")
      set(NEMSIO_INC  "${CORENEMSIO}/nemsio/${NEMSIO_VER}/incmod/nemsio_v${NEMSIO_VER}")
    endif()
    set( libsuffix "_v${NEMSIO_VER}${debug_suffix}" )
    set( NEMSIO_LIB "${LIBRARY_OUTPUT_PATH}/libnemsio${libsuffix}.a" CACHE STRING "NEMSIO Library" )
    set( nemsio "nemsio${libsuffix}")
    set( BUILD_NEMSIO "ON" CACHE INTERNAL "Build the NEMSIO library")
    add_subdirectory(${CMAKE_SOURCE_DIR}/libsrc/nemsio)
    set( NEMSIO_LIB ${nemsio} )
    if( CORE_BUILT )
      list( APPEND CORE_BUILT ${NEMSIO_LIB} )
    else()
      set( CORE_BUILT ${NEMSIO_LIB} )
    endif()
else( NOT NEMSIO_LIB )
  if( CORE_LIBRARIES )
    list( APPEND CORE_LIBRARIES ${NEMSIO_LIB} )
  else()
    set( CORE_LIBRARIES ${NEMSIO_LIB} )
  endif()
endif( NOT NEMSIO_LIB )

if( CORE_INCS )
  list( APPEND CORE_INCS ${NEMSIO_INC} )
else()
  set( CORE_INCS ${INCLUDE_OUTPUT_PATH} ${NEMSIO_INC} )
endif()

set( NEMSIO_LIB_PATH ${NEMSIO_LIB} CACHE STRING "NEMSIO Library Location" )
set( NEMSIO_INCLUDE_PATH ${NEMSIO_INC} CACHE STRING "NEMSIO Include Location" )

