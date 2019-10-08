# This module defines
#  CORE_INCS
#    List of include file paths for all required modules for GSI
#  CORE_LIBRARIES
#    Full list of libraries required to link GSI executable
include(findHelpers)
if(DEFINED ENV{SFCIO_VER})
  set(SFCIO_VER $ENV{SFCIO_VER})
  STRING(REGEX REPLACE "v" "" SFCIO_VER ${SFCIO_VER})
endif()

set( NO_DEFAULT_PATH )
if(NOT BUILD_SFCIO )
  if(DEFINED ENV{SFCIO_LIB4} )
    set(SFCIO_LIB $ENV{SFCIO_LIB4} )
    set(SFCIO_INC $ENV{SFCIO_INC4} )
    message("SFCIO library ${SFCIO_LIB} set via Environment variable")
  else()
    findInc( sfcio SFCIO_VER SFCIO_INC )
    find_library( SFCIO_LIB 
    NAMES libsfcio.a libsfcio_4.a libsfcio_i4r4.a libsfcio_v${SFCIO_VER}_4.a
    HINTS 
      $ENV{COREPATH}/lib 
      /usr/local/jcsda/nwprod_gdas_2014/lib	
      ${COREPATH}/sfcio/v${SFCIO_VER}
      ${COREPATH}/sfcio/v${SFCIO_VER}/intel
      ${COREPATH}/sfcio/v${SFCIO_VER}/ips/${COMPILER_VERSION}
    PATH_SUFFIXES
        lib
     ${NO_DEFAULT_PATH})
    set( sfcio "sfcio_v${SFCIO_VER}")
    message("Found SFCIO library ${SFCIO_LIB}")
  endif()
endif()
if( NOT SFCIO_LIB ) # didn't find the library, so build it from source
    message("Could not find SFCIO library, so building from libsrc")
    if( NOT DEFINED ENV{SFCIO_SRC} )
        findSrc( "sfcio" SFCIO_VER SFCIO_DIR )
        set(SFCIO_INC  "${CMAKE_BINARY_DIR}/include")
    else()
      set( SFCIO_DIR "$ENV{SFCIO_SRC}/libsrc" CACHE STRING "SFCIO Source Location")
      set(SFCIO_INC  "${CORESFCIO}/sfcio/${SFCIO_VER}/incmod/sfcio_v${SFCIO_VER}")
    endif()
    set( libsuffix "_v${SFCIO_VER}${debug_suffix}" )
    set( SFCIO_LIB "${LIBRARY_OUTPUT_PATH}/libsfcio${libsuffix}.a" CACHE STRING "SFCIO Library" )
    set( sfcio "sfcio${libsuffix}")
    set( BUILD_SFCIO "ON" CACHE INTERNAL "Build the SFCIO library")
    add_subdirectory(${CMAKE_SOURCE_DIR}/libsrc/sfcio)
    set( SFCIO_LIB ${sfcio} )
    if( CORE_BUILT )
      list( APPEND CORE_BUILT ${SFCIO_LIB} )
    else()
      set( CORE_BUILT ${SFCIO_LIB} )
    endif()
else( NOT SFCIO_LIB )
  if( CORE_LIBRARIES )
    list( APPEND CORE_LIBRARIES ${SFCIO_LIB} )
  else()
    set( CORE_LIBRARIES ${SFCIO_LIB} )
  endif()
endif( NOT SFCIO_LIB )

if( CORE_INCS )
  list( APPEND CORE_INCS ${SFCIO_INC} )
else()
  set( CORE_INCS ${INCLUDE_OUTPUT_PATH} ${SFCIO_INC} )
endif()

set( SFCIO_LIB_PATH ${SFCIO_LIB} CACHE STRING "SFCIO Library Location" )
set( SFCIO_INCLUDE_PATH ${SFCIO_INC} CACHE STRING "SFCIO Include Location" )

