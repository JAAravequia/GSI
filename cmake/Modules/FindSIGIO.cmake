# This module defines
#  CORE_INCS
#    List of include file paths for all required modules for GSI
#  CORE_LIBRARIES
#    Full list of libraries required to link GSI executable
include(findHelpers)
if(DEFINED ENV{SIGIO_VER})
  set(SIGIO_VER $ENV{SIGIO_VER})
  STRING(REGEX REPLACE "v" "" SIGIO_VER ${SIGIO_VER})
else()
  set(SIGIO_VER "")
endif()

set( NO_DEFAULT_PATH )
if(NOT BUILD_SIGIO )
  if(DEFINED ENV{SIGIO_LIB4} )
    set(SIGIO_LIBRARY $ENV{SIGIO_LIB4} )
    set(SIGIOINC $ENV{SIGIO_INC4} )
    message("SIGIO library ${SIGIO_LIBRARY} set via Environment variable")
  endif()
endif()
if( NOT SIGIO_LIBRARY ) # didn't find the library, so build it from source
    message("Could not find SIGIO library, so building from libsrc")
    if( NOT DEFINED ENV{SIGIO_SRC} )
        findSrc( "sigio" SIGIO_VER SIGIO_DIR )
        set(SIGIOINC  "${CMAKE_BINARY_DIR}/include")
    else()
      set( SIGIO_DIR "$ENV{SIGIO_SRC}/libsrc" CACHE STRING "SIGIO Source Location")
      set(SIGIOINC  "${CORESIGIO}/sigio/${SIGIO_VER}/incmod/sigio_v${SIGIO_VER}")
    endif()
    set( libsuffix "_v${SIGIO_VER}${debug_suffix}" )
    set( SIGIO_LIBRARY "${LIBRARY_OUTPUT_PATH}/libsigio${libsuffix}.a" CACHE STRING "SIGIO Library" )
    set( sigio "sigio${libsuffix}")
    set( BUILD_SIGIO "ON" CACHE INTERNAL "Build the SIGIO library")
    add_subdirectory(${CMAKE_SOURCE_DIR}/libsrc/sigio)
    set( SIGIO_LIBRARY ${sigio} )
    if( CORE_BUILT )
      list( APPEND CORE_BUILT ${SIGIO_LIBRARY} )
    else()
      set( CORE_BUILT ${SIGIO_LIBRARY} )
    endif()
else( NOT SIGIO_LIBRARY )
  if( CORE_LIBRARIES )
    list( APPEND CORE_LIBRARIES ${SIGIO_LIBRARY} )
  else()
    set( CORE_LIBRARIES ${SIGIO_LIBRARY} )
  endif()
endif( NOT SIGIO_LIBRARY )

if( CORE_INCS )
  list( APPEND CORE_INCS ${SIGIOINC} )
else()
  set( CORE_INCS ${INCLUDE_OUTPUT_PATH} ${SIGIOINC} )
endif()


set( SIGIO_LIBRARY_PATH ${SIGIO_LIBRARY} CACHE STRING "SIGIO Library Location" )
set( SIGIO_INCLUDE_PATH ${SIGIOINC} CACHE STRING "SIGIO Include Location" )

