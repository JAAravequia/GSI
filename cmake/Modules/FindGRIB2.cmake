# - Find the GRIB2 modules
set( NO_DEFAULT_PATH )
find_library( G2_LIB4  
    NAMES libg2_4.a
    HINTS
       /scratch2/BMC/wrfruc/gge/codes/WPS/WPS.tanya/ungrib/src/ngl/g2
    ${NO_DEFAULT_PATH})

find_library( G2TMPL_LIB
    NAMES libg2tmpl.a
    HINTS
       /scratch2/BMC/wrfruc/gge/codes/UPP/GSD_UPP_17oct2019/comupp/lib
    ${NO_DEFAULT_PATH})

find_path( GRIB2INC
    NAMES grib_mod.mod
    HINTS
       /scratch2/BMC/wrfruc/gge/codes/UPP/GSD_UPP_17oct2019/comupp/include
    ${NO_DEFAULT_PATH})

set( GRIB2_LIBRARY ${G2_LIB4} ${G2TMPL_LIB} CACHE STRING "GRIB2 Library Location" )
set( GRIB2_INCLUDE_PATH ${GRIB2INC} CACHE STRING "GRIB2 Include Location" )

# - Find the JASPER modules
set( NO_DEFAULT_PATH )
find_library( JASPER_LIB
    NAMES libjasper.so
    HINTS
       /usr/lib64
    ${NO_DEFAULT_PATH})

# - Find the PNG modules
set( NO_DEFAULT_PATH )
find_library( PNG_LIB
    NAMES libpng.so
    HINTS
       /usr/lib64
    ${NO_DEFAULT_PATH})

# - Find the Z modules
set( NO_DEFAULT_PATH )
find_library( Z_LIB
    NAMES libz.so
    HINTS
       /usr/lib64
    ${NO_DEFAULT_PATH})
