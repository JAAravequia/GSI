project (gmaoTimer)
enable_language (C Fortran)

set(	gmaoTimer_srcs
	m_gmaoTimer.F90
	m_mergedList.F90
	m_sysclocks.F90
	m_zeit.F90
	get_zeits.c
)

add_library(gmaoTimer ${gmaoTimer_srcs})
add_dependencies(gmaoTimer ${GSILIB} )

# Where my prerequisite .mod files are
include_directories(${CMAKE_CURRENT_BINARY_DIR} "${PROJECT_BINARY_DIR}/include")

# Where my product .mod files will be
set(Fortran_MODULE_DIRECTORY "${PROJECT_BINARY_DIR}/include")

