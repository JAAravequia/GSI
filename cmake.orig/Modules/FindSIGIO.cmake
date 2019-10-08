# This module defines
#  CORE_INCS
#    List of include file paths for all required modules for GSI
#  CORE_LIBRARIES
#    Full list of libraries required to link GSI executable
include(findHelpers)
if(DEFINED ENV{SIGIO_VER})
  set(SIGIO_VER $ENV{SIGIO_VER})
  STRING(REGEX REPLACE "v" "" SIGIO_VER ${SIGIO_VER})
endif()

set( NO_DEFAULT_PATH )
if(NOT BUILD_SIGIO )
  if(DEFINED ENV{SIGIO_LIB4} )
    set(SIGIO_LIB $ENV{SIGIO_LIB4} )
    set(SIGIO_INC $ENV{SIGIO_INC4} )
    message("SIGIO library ${SIGIO_LIB} set via Environment variable")
  else()
    findInc( sigio SIGIO_VER SIGIO_INC )
    find_library( SIGIO_LIB 
    NAMES libsigio.a libsigio_4.a libsigio_i4r4.a libsigio_v${SIGIO_VER}_4.a
    HINTS 
     $ENV{COREPATH}/lib 
     /usr/local/jcsda/nwprod_gdas_2014/lib	
     ${COREPATH}/sigio/v${SIGIO_VER}
     ${COREPATH}/sigio/v${SIGIO_VER}/intel
     ${COREPATH}/sigio/v${SIGIO_VER}/ips/${COMPILER_VERSION}
    PATH_SUFFIXES
        lib
     ${NO_DEFAULT_PATH})
    set( sigio "sigio_v${SIGIO_VER}")
    message("Found SIGIO library ${SIGIO_LIB}")
  endif()
endif()
if( NOT SIGIO_LIB ) # didn't find the library, so build it from source
    message("Could not find SIGIO library, so building from libsrc")
    if( NOT DEFINED ENV{SIGIO_SRC} )
        findSrc( "sigio" SIGIO_VER SIGIO_DIR )
        set(SIGIO_INC  "${CMAKE_BINARY_DIR}/include")
    else()
      set( SIGIO_DIR "$ENV{SIGIO_SRC}/libsrc" CACHE STRING "SIGIO Source Location")
      set(SIGIO_INC  "${CORESIGIO}/sigio/${SIGIO_VER}/incmod/sigio_v${SIGIO_VER}")
    endif()
    set( libsuffix "_v${SIGIO_VER}${debug_suffix}" )
    set( SIGIO_LIB "${LIBRARY_OUTPUT_PATH}/libsigio${libsuffix}.a" CACHE STRING "SIGIO Library" )
    set( sigio "sigio${libsuffix}")
    set( BUILD_SIGIO "ON" CACHE INTERNAL "Build the SIGIO library")
    add_subdirectory(${CMAKE_SOURCE_DIR}/libsrc/sigio)
    set( SIGIO_LIB ${sigio} )
    if( CORE_BUILT )
      list( APPEND CORE_BUILT ${SIGIO_LIB} )
    else()
      set( CORE_BUILT ${SIGIO_LIB} )
    endif()
else( NOT SIGIO_LIB )
  if( CORE_LIBRARIES )
    list( APPEND CORE_LIBRARIES ${SIGIO_LIB} )
  else()
    set( CORE_LIBRARIES ${SIGIO_LIB} )
  endif()
endif( NOT SIGIO_LIB )

if( CORE_INCS )
  list( APPEND CORE_INCS ${SIGIO_INC} )
else()
  set( CORE_INCS ${INCLUDE_OUTPUT_PATH} ${SIGIO_INC} )
endif()


set( SIGIO_LIB_PATH ${SIGIO_LIB} CACHE STRING "SIGIO Library Location" )
set( SIGIO_INCLUDE_PATH ${SIGIO_INC} CACHE STRING "SIGIO Include Location" )

