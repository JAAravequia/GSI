# This module defines
#  CORE_INCS
#    List of include file paths for all required modules for GSI
#  CORE_LIBRARIES
#    Full list of libraries required to link GSI executable
include(findHelpers)
if(DEFINED ENV{CRTM_VER})
  set(CRTM_VER $ENV{CRTM_VER})
  STRING(REGEX REPLACE "v" "" CRTM_VER ${CRTM_VER})
endif()

set( NO_DEFAULT_PATH )
if(NOT BUILD_CRTM )
  if(DEFINED ENV{CRTM_LIB} )
    set(CRTM_LIB $ENV{CRTM_LIB} )
    set(CRTM_INC $ENV{CRTM_INC} )
    message("CRTM library ${CRTM_LIB} set via Environment variable")
  else()
    findInc( crtm CRTM_VER CRTM_INC )
    find_library( CRTM_LIB 
    NAMES libcrtm_v${CRTM_VER}.a libcrtm.a libCRTM.a 
    HINTS 
      /usr/local/jcsda/nwprod_gdas_2014/lib	
      ${CRTM_BASE}
      ${CRTM_BASE}/lib
      ${CRTM_BASE}/${CRTM_VER}
      ${CRTM_BASE}/${CRTM_VER}/lib
      ${CRTM_BASE}/v${CRTM_VER}/intel
      ${CRTM_BASE}/v${CRTM_VER}/ips/${COMPILER_VERSION}
      ${COREPATH}/v${CRTM_VER}/ips/${COMPILER_VERSION}
      ${COREPATH}
      ${COREPATH}/lib
      $ENV{COREPATH} 
      $ENV{COREPATH}/lib 
      $ENV{COREPATH}/include 
      ${CORECRTM}/crtm/${CRTM_VER}
      /nwprod2/lib/crtm/v${CRTM_VER}
    PATH_SUFFIXES
        lib
     ${NO_DEFAULT_PATH})
    set( crtm "crtm_v${CRTM_VER}")
    message("Found CRTM library ${CRTM_LIB}")
  endif()
endif()
if( NOT CRTM_LIB ) # didn't find the library, so build it from source
    message("Could not find CRTM library, so building from libsrc")
    if( NOT DEFINED ENV{CRTM_SRC} )
        findSrc( "crtm" CRTM_VER CRTM_DIR )
        set(CRTM_INC  "${CMAKE_BINARY_DIR}/include")
    else()
      set( CRTM_DIR "$ENV{CRTM_SRC}/libsrc" CACHE STRING "CRTM Source Location")
      set(CRTM_INC  "${CORECRTM}/crtm/${CRTM_VER}/incmod/crtm_v${CRTM_VER}")
    endif()
    set( libsuffix "_v${CRTM_VER}${debug_suffix}" )
    set( CRTM_LIB "${LIBRARY_OUTPUT_PATH}/libcrtm${libsuffix}.a" CACHE STRING "CRTM Library" )
    set( crtm "crtm${libsuffix}")
    set( BUILD_CRTM "ON" CACHE INTERNAL "Build the CRTM library")
    add_subdirectory(${CMAKE_SOURCE_DIR}/libsrc/crtm)
    set( CRTM_LIB ${crtm} )
    if( CORE_BUILT )
      list( APPEND CORE_BUILT ${CRTM_LIB} )
    else()
      set( CORE_BUILT ${CRTM_LIB} )
    endif()
else( NOT CRTM_LIB )
  if( CORE_LIBRARIES )
    list( APPEND CORE_LIBRARIES ${CRTM_LIB} )
  else()
    set( CORE_LIBRARIES ${CRTM_LIB} )
  endif()
endif( NOT CRTM_LIB )

if( CORE_INCS )
  list( APPEND CORE_INCS ${CRTM_INC} )
else()
  set( CORE_INCS ${INCLUDE_OUTPUT_PATH} ${CRTM_INC} )
endif()

set( CRTM_LIB_PATH ${CRTM_LIB} CACHE STRING "CRTM Library Location" )
set( CRTM_INCLUDE_PATH ${CRTM_INC} CACHE STRING "CRTM Include Location" )

