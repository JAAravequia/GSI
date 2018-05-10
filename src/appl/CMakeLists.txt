#!/usr/bin/env cmake
#$$$  CMakeLists documentation block
#                .      .    .                                       .
#   module:	 a cmake-module to build a gsimain excutable 
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
#	message("src/appl.cmake:   CMAKE_CURRENT_BINARY_DIR := ${CMAKE_CURRENT_BINARY_DIR}")
#	message("src/appl.cmake:         PROJECT_BINARY_DIR := ${PROJECT_BINARY_DIR}")
#	message("src/appl.cmake:                    GSIEXEC := ${GSIEXEC}")
#	message("src/appl.cmake:               GSISHAREDLIB := ${GSISHAREDLIB}")
#	message("src/appl.cmake:                     GSILIB := ${GSILIB}")
#	message("src/appl.cmake:          GSI_Fortran_FLAGS := ${GSI_Fortran_FLAGS}")
#	message("src/appl.cmake: GSIEXEC_EXTERNAL_LIBRARIES := ${GSIEXEC_EXTERNAL_LIBRARIES}")
#
#   output argument list: as a target product and a Fortram module directory.
#
#	add_executable( timered_${GSIEXEC} )
#
# attributes:
#	cmake_minimum_required(VERSION 2.8.12)
#
#$$$  end CMakeLists documentation block

enable_language (Fortran)

set(gsimain "timered_${GSIEXEC}")
add_executable(${gsimain}
	gsimain.f90
)

    set_source_files_properties( gsimain.f90
	PROPERTIES COMPILE_FLAGS "${GSI_Fortran_FLAGS}"
    )

# Explicit dependencies for compilation, i.e. gmaoTimer, ${GSILIB} and ${GSISHAREDLIB}
# direct prerequisits of ${gsimain} compilation.
add_dependencies(${gsimain}
	gmaoTimer				# resolving gmaoTimer
	${GSILIB}				# resolving gsimod, and whatnots
	${GSISHAREDLIB}				# resolving gsisub, and whatnots
)

# Imports of my prerequisite .mod files:
target_include_directories(${gsimain}
	PUBLIC ${CMAKE_CURRENT_BINARY_DIR}
	PUBLIC ${PROJECT_BINARY_DIR}/include
	PUBLIC ${PROJECT_BINARY_DIR}/include/gmaoTimer
)

# Note below the need for multiple passes over ${GSISHAREDLIB} and ${GSILIB},
# because of interleaving dependency between ${GSISHAREDLIB} and ${GSILIB} because
# they are defined such by src/CMakeLists.txt.

target_link_libraries(${gsimain}		# including gsimain.o
	gmaoTimer				# resolving gmaoTimer
	${GSILIB}				# resolving ${GSILIB}::gsimod, and whatnots
	${GSISHAREDLIB}				# resolving ${GSISHAREDLIB}::gsisub, and whatnots
	${GSILIB}				# resolving ${GSILIB}::gesinfo, and whatnots
	${GSISHAREDLIB}				# resolving all remaining unresolved ${GSISHAREDLIB} symbols
	${GSIEXEC_EXTERNAL_LIBRARIES} 		# finally all external symbols
)
#.
