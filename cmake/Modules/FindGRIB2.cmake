# - Find the GRIB2 modules
set( NO_DEFAULT_PATH )
find_library( G2_LIB4  
    NAMES libg2_4.a
    HINTS
       $ENV{GRIB2PATH}/g2/v2.5.0/lib 
    ${NO_DEFAULT_PATH})

find_library( G2TMPL_LIB
    NAMES libg2tmpl.a
    HINTS
       $ENV{GRIB2PATH}/g2tmpl/v1.3.0/lib 
    ${NO_DEFAULT_PATH})

find_path( GRIB2INC
    NAMES grib_mod.mod
    HINTS
       $ENV{GRIB2PATH}/g2/v2.5.0/incmod/g2_4 
    ${NO_DEFAULT_PATH})

set( GRIB2_LIBRARY ${G2_LIB4} ${G2TMPL_LIB} CACHE STRING "GRIB2 Library Location" )
set( GRIB2_INCLUDE_PATH ${GRIB2INC} CACHE STRING "GRIB2 Include Location" )

# - Find the JASPER modules
set( NO_DEFAULT_PATH )
find_library( JASPER_LIB
    NAMES libjasper.a
    HINTS
       $ENV{GRIB2PATH}/jasper/v1.900.1/lib
    ${NO_DEFAULT_PATH})

# - Find the PNG modules
set( NO_DEFAULT_PATH )
find_library( PNG_LIB
    NAMES libpng.a
    HINTS
       $ENV{GRIB2PATH}/libpng/v1.6.34/lib
    ${NO_DEFAULT_PATH})

# - Find the Z modules
set( NO_DEFAULT_PATH )
find_library( Z_LIB
    NAMES libz.a
    HINTS
       $ENV{GRIB2PATH}/zlib/v1.2.6/lib
    ${NO_DEFAULT_PATH})
