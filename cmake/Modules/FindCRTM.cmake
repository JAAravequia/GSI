# This module defines
#  CORE_INCS
#    List of include file paths for all required modules for GSI
#  CORE_LIBRARIES
#    Full list of libraries required to link GSI executable
if(NOT BUILD_CRTM )
  if(DEFINED ENV{CRTM_LIB} )
    set(CRTM_LIBRARY $ENV{CRTM_LIB} )
    set(CRTMINC $ENV{CRTM_INC} )
    message("CRTM library ${CRTM_LIBRARY} set via Environment variable")
  endif()
else()
    set( BUILD_CRTM "ON" CACHE INTERNAL "Build the CRTM library")
    add_subdirectory(${CMAKE_SOURCE_DIR}/libsrc/crtm)
    set( CRTM_LIBRARY ${PROJECT_BINARY_DIR}/lib/libCRTM.a )
    if( CORE_BUILT )
      list( APPEND CORE_BUILT ${CRTM_LIBRARY} )
      list( APPEND EXT_BUILT ${CRTM_LIBRARY} )
    else()
      set( CORE_BUILT ${CRTM_LIBRARY} )
      set( EXT_BUILT ${CRTM_LIBRARY} )
    endif()
endif()
if( CORE_LIBRARIES )
   list( APPEND CORE_LIBRARIES ${CRTM_LIBRARY} )
else()
   set( CORE_LIBRARIES ${CRTM_LIBRARY} )
endif()

if( CORE_INCS )
  list( APPEND CORE_INCS ${CRTMINC} )
else()
  set( CORE_INCS ${INCLUDE_OUTPUT_PATH} ${CRTMINC} )
endif()

set( CRTM_LIBRARY_PATH ${CRTM_LIBRARY} CACHE STRING "CRTM Library Location" )
set( CRTM_INCLUDE_PATH ${CRTMINC} CACHE STRING "CRTM Include Location" )

