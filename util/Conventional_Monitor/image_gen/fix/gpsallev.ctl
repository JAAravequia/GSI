DTYPE  station 
options big_endian sequential
STNMAP tupair.map
UNDEF  -999.0
TITLE  Station Data Sample
TDEF   1 linear 00z27may2006 12hr 
*ZDEF pressure 915.,840.,690.,590.,490.,390.,290.,240.,190.,90. 
VARS  20
lat      1  0   lat in degrees
lon      1  0   lon in degrees
bend     1  0   incremental bending angle (x100 %)
press    1  0   pressure at obs location
ohgt     1  0   impact height(meters)
dtime    1  0   relative time to analysis hours
zsges    1  0   model terrain (m)
iqc      1  0   input prepbufr qc or event mark
iuse     1  0   read_prepbufr data usage flag
muse     1  0   setup data usage flag
rwgt     1  0   nonlear qc relative weight 
err      1  0   original inverse gps obs error (rad**-1)
rerr     1  0   original + represent error inverse gps obs error (rad**-1) 
ferr     1  0   final inverse observation
igps     1  0   bending angle observation (radians) 
trefges  1  0   temp at obs location (Kelvin) 
hob      1  0   model vertical grid (interface) 
fish     1  0   uses gps_ref (one = uses)
qrefges  1  0   spedific humidity at obs !location (kg/kg) 
spread   1  0   spread (filled in by EnKF)
ENDVARS
