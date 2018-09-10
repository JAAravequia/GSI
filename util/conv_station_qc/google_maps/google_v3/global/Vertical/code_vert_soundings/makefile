create_markers_vert_soundings   : create_markers_vert_soundings.o stringmod.o precmod.o 
	ifort -o create_markers_vert_soundings create_markers_vert_soundings.o stringmod.o precmod.o -L /nwprod/lib -lbufr_4_64

precmod.o    : precmod.f90
	ifort -c precmod.f90

stringmod.o    : stringmod.f90
	ifort -c stringmod.f90

create_markers_vert_soundings.o  : create_markers_vert_soundings.f90 stringmod.f90 precmod.f90
	ifort -c create_markers_vert_soundings.f90 -L /nwprod/lib -lbufr_4_64
