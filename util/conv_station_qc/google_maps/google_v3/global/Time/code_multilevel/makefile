create_markers_multilevel   : create_markers_multilevel.o stringmod.o precmod.o 
	ifort -o create_markers_multilevel create_markers_multilevel.o stringmod.o precmod.o -L /nwprod/lib -lbufr_4_64

precmod.o    : precmod.f90
	ifort -c precmod.f90

stringmod.o    : stringmod.f90
	ifort -c stringmod.f90

create_markers_multilevel.o  : create_markers_multilevel.f90 stringmod.f90 precmod.f90
	ifort -c create_markers_multilevel.f90 -L /nwprod/lib -lbufr_4_64
