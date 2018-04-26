project (gmaoTimer)

enable_language (C Fortran)

set(	gmaoTimer_srcs
	m_gmaoTimer.F90
	m_mergedList.F90
	m_sysclocks.F90
	m_zeit.F90
	get_zeits.c
)

add_library(gmaoTimer $(gmaoTimer_srcs))

