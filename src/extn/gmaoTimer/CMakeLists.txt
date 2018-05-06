#!/usr/bin/env cmake
#$$$  CMakeLists documentation block
#                .      .    .                                       .
#   module:	 a cmake-module to build Fortran gmaoTimer library
#   prgmmr:	 j guo <jguo@nasa.gov>
#      org:	 NASA/GSFC, Global Modeling and Assimilation Office, 610.3
#     date:	 2018-05-06
#
# abstract: cmake wrapper of gmaoTimer, an extension of GSI abstractTimer.
#
# program history log:
#   2018-05-06  j guo   - added this document block
#
#   input argument list: as expected pre-defined cmake variables
#
#	message("gmaoTimer.cmake: CMAKE_CURRENT_BINARY_DIR := ${CMAKE_CURRENT_BINARY_DIR}")
#	message("gmaoTimer.cmake:       PROJECT_BINARY_DIR := ${PROJECT_BINARY_DIR}")
#	message("gmaoTimer.cmake:             GSISHAREDLIB := ${GSISHAREDLIB}")
#	message("gmaoTimer.cmake:        GSI_Fortran_FLAGS := ${GSI_Fortran_FLAGS}")
#	message("gmaoTimer.cmake:               GSI_CFLAGS := ${GSI_CFLAGS}")
#
#   output argument list: as a target product and a Fortram module directory.
#
#	           add_library() := gmaoTimer
#	Fortran_MODULE_DIRECTORY := ${PROJECT_BINARY_DIR}/include/gmaoTimer
#
# attributes:
#	cmake_minimum_required(VERSION 2.8.12)
#
#$$$  end CMakeLists documentation block

enable_language (C Fortran)

# (1) define my target and its source contents
add_library( gmaoTimer STATIC 
	m_gmaoTimer.F90
	m_mergedList.F90
	m_sysclocks.F90
	m_zeit.F90
	get_zeits.c
)

# Given the portability of the source code in this cmake-module, there is likely
# no need to specify COMPILE_FLAGS properties.  But if one does need to have them,
# just comment out if()/endif() statements below.
if ( DO_NEED_GSI_COMPILE_FLAGS )
    # Source properties: Fortran
    set_source_files_properties(
	m_gmaoTimer.F90
	m_mergedList.F90
	m_sysclocks.F90
	m_zeit.F90
   	PROPERTIES COMPILE_FLAGS ${GSI_Fortran_FLAGS}
    )
    # Source properties: C
    set_source_files_properties(
	get_zeits.c
	PROPERTIES COMPILE_FLAGS ${GSI_CFLAGS}
    )
endif()

# (2) Some dependency details to <library::prerequisites>:
#	gmaoTimer: ${GSISHAREDLIB}::m_abstracttimer.mod
#	gmaoTimer: ${GSISHAREDLIB}::mpeu_util.mod
#	gmaoTimer: ${GSISHAREDLIB}::mpeu_mpif.mod
add_dependencies( gmaoTimer
	${GSISHAREDLIB}
)

# (3) Imports of prerequisites, for -I configuration flags
target_include_directories( gmaoTimer
	PUBLIC ${CMAKE_CURRENT_BINARY_DIR}
	PUBLIC ${PROJECT_BINARY_DIR}/include
)
# (4) My export of .mod module interfaces
set_target_properties(gmaoTimer PROPERTIES
	Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/include/gmaoTimer
)

#.
